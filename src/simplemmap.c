#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>


/**
 ** Memory Mapped Vectors
 **/

/*
 * MMAP Object State
 */

/* State is hald in a LISTSXP of length 3, and includes
   
       file
       size and length in a REALSXP
       type, ptrOK, wrtOK in an INTSXP

   These are used by the methods, and also represent the serialized
   state object.
 */

static SEXP make_mmap_state(SEXP file, size_t size, int type,
			    Rboolean ptrOK, Rboolean wrtOK)
{
    SEXP sizes = PROTECT(allocVector(REALSXP, 2));
    double *dsizes = REAL(sizes);
    dsizes[0] = size;
    switch(type) {
    case INTSXP: dsizes[1] = size / sizeof(int); break;
    case REALSXP: dsizes[1] = size / sizeof(double); break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP info = PROTECT(allocVector(INTSXP, 3));
    INTEGER(info)[0] = type;
    INTEGER(info)[1] = ptrOK;
    INTEGER(info)[2] = wrtOK;

    SEXP state = list3(file, sizes, info);

    UNPROTECT(2);
    return state;
}
			    
#define MMAP_STATE_FILE(x) CAR(x)
#define MMAP_STATE_SIZE(x) ((size_t) REAL_ELT(CADR(x), 0))
#define MMAP_STATE_LENGTH(x) ((size_t) REAL_ELT(CADR(x), 1))
#define MMAP_STATE_TYPE(x) INTEGER(CADDR(x))[0]
#define MMAP_STATE_PTROK(x) INTEGER(CADDR(x))[1]
#define MMAP_STATE_WRTOK(x) INTEGER(CADDR(x))[2]


/*
 * MMAP Classes and Objects
 */

static R_altrep_class_t mmap_integer_class;
static R_altrep_class_t mmap_real_class;

/* MMAP objects are ALTREP objects with data fields

       data1: an external pointer to the mmaped address
       data2: the MMAP object's state

   The state is also stored in the Protected field of the external
   pointer for use by the finalizer.
*/

static void register_mmap_eptr(SEXP eptr);
static SEXP make_mmap(void *p, SEXP file, size_t size, int type,
		      Rboolean ptrOK, Rboolean wrtOK)
{
    SEXP state = PROTECT(make_mmap_state(file, size, type, ptrOK, wrtOK));
    SEXP eptr = PROTECT(R_MakeExternalPtr(p, R_NilValue, state));
    register_mmap_eptr(eptr);

    R_altrep_class_t class;
    switch(type) {
    case INTSXP:
	class = mmap_integer_class;
	break;
    case REALSXP:
	class = mmap_real_class;
	break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP ans = R_new_altrep(class, eptr, state);
    if (ptrOK && ! wrtOK)
	MARK_NOT_MUTABLE(ans);

    UNPROTECT(2); /* state, eptr */
    return ans;
}

#define MMAP_EPTR(x) R_altrep_data1(x)
#define MMAP_STATE(x) R_altrep_data2(x)
#define MMAP_LENGTH(x) MMAP_STATE_LENGTH(MMAP_STATE(x))
#define MMAP_PTROK(x) MMAP_STATE_PTROK(MMAP_STATE(x))
#define MMAP_WRTOK(x) MMAP_STATE_WRTOK(MMAP_STATE(x))

#define MMAP_EPTR_STATE(x) R_ExternalPtrProtected(x)

static R_INLINE void *MMAP_ADDR(SEXP x)
{
    SEXP eptr = MMAP_EPTR(x);
    void *addr = R_ExternalPtrAddr(eptr);

    if (addr == NULL)
	error("object has been unmapped");
    return addr;
}

/* We need to maintain a list of weak references to the external
   pointers of memory-mapped objects so a request to unload the shared
   library can finalize them before unloading; otherwise, attempting
   to run a finalizer after unloading would result in an illegal
   instruction. */

static SEXP mmap_list = NULL;

#define MAXCOUNT 10

static void mmap_finalize(SEXP eptr);
static void register_mmap_eptr(SEXP eptr)
{
    /* clean out the weak list every MAXCOUNT calls*/
    static int cleancount = MAXCOUNT;
    if (--cleancount <= 0) {
	cleancount = MAXCOUNT;
	for (SEXP last = mmap_list, next = CDR(mmap_list);
	     next != R_NilValue;
	     next = CDR(next))
	    if (R_WeakRefKey(CAR(next)) == R_NilValue)
		SETCDR(last, CDR(next));
	    else
		last = next;
    }

    /* add a weak reference with a finalizer to the list */
    SETCDR(mmap_list, 
	   CONS(R_MakeWeakRefC(eptr, R_NilValue, mmap_finalize, TRUE),
		CDR(mmap_list)));

    /* store the weak reference in the external pointer for do_munmap_file */
    R_SetExternalPtrTag(eptr, CAR(CDR(mmap_list)));
}

static void finalize_mmap_objects()
{
    /* finalize any remaining mmap objects before unloading */
    for (SEXP next = CDR(mmap_list); next != R_NilValue; next = CDR(next))
	R_RunWeakRefFinalizer(CAR(next));
    R_ReleaseObject(mmap_list);
}


/*
 * ALTREP Methods
 */

static SEXP mmap_serialized_state(SEXP x)
{
    /**** For now, if ptrOK is true then serialize as a regular typed
	  vector. If ptrOK is false, then serialize information to
	  allow the mmap to be reconstructed. The original file name
	  is serialized; it will be expanded again when unserializing,
	  in a context where the result may be different. */
    if (MMAP_PTROK(x))
	return NULL;
    else
	return MMAP_STATE(x);
}

static SEXP mmap_file(SEXP, int, Rboolean, Rboolean);

static SEXP mmap_unserialize(SEXP class, SEXP state, SEXP attr)
{
    SEXP file = MMAP_STATE_FILE(state);
    int type = MMAP_STATE_TYPE(state);
    Rboolean ptrOK = MMAP_STATE_PTROK(state);
    Rboolean wrtOK = MMAP_STATE_WRTOK(state);

    /**** For now, this will throw an error on failure. Eventualy
	  this needs to have a mechanism to locate a file that isn't
	  found or to return something reasonable, e.g. numeric(0), on
	  failure. A failure result should probably ignore the
	  attributes. */
    SEXP val = mmap_file(file, type, ptrOK, wrtOK);
    SET_ATTRIB(val, attr);
    return val;
}

Rboolean mmap_inspect(SEXP x, int pre, int deep, int pvec,
		      void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean ptrOK = MMAP_PTROK(x);
    Rboolean wrtOK = MMAP_WRTOK(x);
    Rprintf(" mmaped %s", type2char(TYPEOF(x)));
    Rprintf(" [ptr=%d,wrt=%d]\n", ptrOK, wrtOK);
    return TRUE;
}


/*
 * ALTVEC Methods
 */

static R_xlen_t mmap_xlength(SEXP x)
{
    return MMAP_LENGTH(x);
}

static void *mmap_dataptr(SEXP x)
{
    /**** get addr first to get error for unmapped? */
    if (MMAP_PTROK(x))
	return MMAP_ADDR(x);
    else
	error("cannot access data pointer for this mmaped vector");
}

static void *mmap_dataptr_or_null(SEXP x)
{
    return MMAP_PTROK(x) ? MMAP_ADDR(x) : NULL;
}


/*
 * ALTINTEGER Methods
 */

static int mmap_integer_elt(SEXP x, R_xlen_t i)
{
    int *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_integer_get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    int *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(int));
    return ncopy;
}


/*
 * ALTREAL Methods
 */

static double mmap_real_elt(SEXP x, R_xlen_t i)
{
    double *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_real_get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    double *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(double));
    return ncopy;
}


/*
 * Class Objects and Method Tables
 */

static void InitMmapIntegerClass(DllInfo *info)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("mmap_integer", "simplemmap", info);
    mmap_integer_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_unserialize_method(cls, mmap_unserialize);
    R_set_altrep_serialized_state_method(cls, mmap_serialized_state);
    R_set_altrep_inspect_method(cls, mmap_inspect);

    /* override ALTVEC methods */
    R_set_altvec_length_method(cls, mmap_xlength);
    R_set_altvec_dataptr_method(cls, mmap_dataptr);
    R_set_altvec_dataptr_or_null_method(cls, mmap_dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_elt_method(cls, mmap_integer_elt);
    R_set_altinteger_get_region_method(cls, mmap_integer_get_region);
}

static void InitMmapRealClass(DllInfo *info)
{
    R_altrep_class_t cls =
	R_make_altreal_class("mmap_real", "simplemmap", info);
    mmap_real_class = cls;

    /* override ALTREP methods */
    R_set_altrep_unserialize_method(cls, mmap_unserialize);
    R_set_altrep_serialized_state_method(cls, mmap_serialized_state);
    R_set_altrep_inspect_method(cls, mmap_inspect);

    /* override ALTVEC methods */
    R_set_altvec_length_method(cls, mmap_xlength);
    R_set_altvec_dataptr_method(cls, mmap_dataptr);
    R_set_altvec_dataptr_or_null_method(cls, mmap_dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_elt_method(cls, mmap_real_elt);
    R_set_altreal_get_region_method(cls, mmap_real_get_region);
}


/*
 * Constructor
 */

#ifdef Win32
# error "I'm sure this needs adjusting for Windows, so punt for now."
#else
/* derived from the example in
  https://www.safaribooksonline.com/library/view/linux-system-programming/0596009585/ch04s03.html */

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

#define DEBUG_PRINT(x) REprintf(x);
//#define DEBUG_PRINT(x) do { } while (0)

static void mmap_finalize(SEXP eptr)
{
    DEBUG_PRINT("finalizing ... ");
    void *p = R_ExternalPtrAddr(eptr);
    size_t size = MMAP_STATE_SIZE(MMAP_EPTR_STATE(eptr));
    R_SetExternalPtrAddr(eptr, NULL);

    if (p != NULL) {
	munmap(p, size); /* don't check for errors */
	R_SetExternalPtrAddr(eptr, NULL);
    }
    DEBUG_PRINT("done\n");
}

static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK)
{
    const char *efn = R_ExpandFileName(translateChar(STRING_ELT(file, 0)));
    struct stat sb;

    /* Target not link */
    if (stat(efn, &sb) != 0)
	error("stat: %s", strerror(errno));

    if (! S_ISREG(sb.st_mode))
	error("%s is not a regular file", efn);

    int oflags = wrtOK ? O_RDWR : O_RDONLY;
    int fd = open(efn, oflags);
    if (fd == -1)
	error("open: %s", strerror(errno));

    int pflags = wrtOK ? PROT_READ | PROT_WRITE : PROT_READ;
    void *p = mmap(0, sb.st_size, pflags, MAP_SHARED, fd, 0);
    close(fd); /* don't care if this fails */
    if (p == MAP_FAILED)
	error("mmap: %s", strerror(errno));

    return make_mmap(p, file, sb.st_size, type, ptrOK, wrtOK);
}
#endif

static Rboolean asLogicalNA(SEXP x, Rboolean dflt)
{
    Rboolean val = asLogical(x);
    return val == NA_LOGICAL ? dflt : val;
}

SEXP do_mmap_file(SEXP args)
{
    args = CDR(args);
    SEXP file = CAR(args);
    SEXP stype = CADR(args);
    SEXP sptrOK = CADDR(args);
    SEXP swrtOK = CADDDR(args);

    int type = 0;
    if (stype != R_NilValue) {
	const char *typestr = CHAR(asChar(stype));
	if (strcmp(typestr, "double") == 0)
	    type = REALSXP;
	else if (strcmp(typestr, "integer") == 0 ||
		 strcmp(typestr, "int") == 0)
	    type = INTSXP;
	else
	    error("type '%s' is not supported", typestr);
    }    

    Rboolean ptrOK = sptrOK == R_NilValue ? FALSE : asLogicalNA(sptrOK, FALSE);
    Rboolean wrtOK = swrtOK == R_NilValue ? FALSE : asLogicalNA(swrtOK, FALSE);

    if (TYPEOF(file) != STRSXP || LENGTH(file) != 1 || file == NA_STRING)
	error("invalud 'file' argument");

    return mmap_file(file, type, ptrOK, wrtOK);
}

SEXP do_munmap_file(SEXP args)
{
    args = CDR(args);
    SEXP x = CAR(args);

    /**** would be useful to have R_mmap_class virtual class as parent here */
    if (! (R_altrep_inherits(x, mmap_integer_class) ||
	   R_altrep_inherits(x, mmap_real_class)))
	error("not a memory-mapped object");

    /* using the finalizer is a cheat to avoid yet another #ifdef Windows */
    SEXP eptr = MMAP_EPTR(x);
    errno = 0;
    R_RunWeakRefFinalizer(R_ExternalPtrTag(eptr));
    if (errno)
	error("munmap: %s", strerror(errno));
    return R_NilValue;
}    
	
static const R_ExternalMethodDef ExtEntries[] = {
    {"mmap_file", (DL_FUNC) &do_mmap_file, -1},
    {"munmap_file", (DL_FUNC) &do_munmap_file, -1},
    {NULL, NULL, 0}
};


/*
 * Shared Library Initialization and Finalization
 */

void R_init_simplemmap(DllInfo *info)
{
    mmap_list = CONS(R_NilValue, R_NilValue);
    R_PreserveObject(mmap_list);
    
    InitMmapIntegerClass(info);
    InitMmapRealClass(info);

    R_registerRoutines(info, NULL, NULL, NULL, ExtEntries);
}

void R_unload_simplemmap(DllInfo *info)
{
    finalize_mmap_objects();
}
