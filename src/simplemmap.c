#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>

#define MMAP_PKG simplemmap
#define PKG_STR(NAME) (#NAME)
#define PKG_INIT(NAME) R_init_##NAME
#define PKG_UNLOAD(NAME) R_unload_##NAME

/**
 ** Memory Mapped Vectors
 **/

/*
 * Methods
 */

typedef struct {
    R_xlen_t len;
    Rboolean ptrOK;
    Rboolean wrtOK;
} mmap_info_t;

#define MMAP_EPTR(x) CAR(x)
#define MMAP_INFO(x) CDR(x)

static R_INLINE void *MMAP_ADDR(SEXP x)
{
    SEXP eptr = MMAP_EPTR(x);
    void *addr = R_ExternalPtrAddr(eptr);

    if (addr == NULL)
	error("object has been unmapped");
    return addr;
}

static SEXP mmap_serialized_state(SEXP x)
{
    SEXP info = MMAP_INFO(x);
    mmap_info_t *pi = DATAPTR(info);

    /**** For now, if ptrOK is true then serializa as a regular typed
	  vector. I ptrOK is false, then serialize information to
	  allow the mmap to be reconstructed. The original file name
	  is serialized; it will be expanded again when unserializing,
	  in a context where the result may be different. */
    if (pi->ptrOK)
	return NULL;
    else
	return R_ExternalPtrProtected(MMAP_EPTR(x));
}

static SEXP mmap_file(SEXP, int, Rboolean, Rboolean);

static SEXP mmap_unserialize(SEXP class, SEXP state, SEXP attr)
{
    SEXP file = CAR(state);
    SEXP dinfo = CADDR(state);
    int type = INTEGER(dinfo)[0];
    Rboolean ptrOK = INTEGER(dinfo)[1];
    Rboolean wrtOK = INTEGER(dinfo)[2];

    /**** For now, this will throuw an error on failure. Eventualy
	  this needs to have a mechanism to locate a file that isn't
	  found or to return something reasonable, e.g. numeric(0), on
	  failure. A faulure result should probably ignore the
	  attributes. */
    SEXP val = mmap_file(file, type, ptrOK, wrtOK);
    SET_ATTRIB(val, attr);
    return val;
}

Rboolean mmap_inspect(SEXP x, int pre, int deep, int pvec,
		      void (*inspect_subtree)(SEXP, int, int, int))
{
    SEXP info = MMAP_INFO(x);
    mmap_info_t *pi = DATAPTR(info);
    Rprintf(" mmaped %s", type2char(TYPEOF(x)));
    Rprintf(" [ptr=%d,wrt=%d]\n", pi->ptrOK, pi->wrtOK);
    return TRUE;
}

static R_xlen_t mmap_xlength(SEXP x)
{
    SEXP info = MMAP_INFO(x);
    mmap_info_t *pi = DATAPTR(info);
    return pi->len;
}

static void *mmap_dataptr(SEXP x)
{
    SEXP info = MMAP_INFO(x);
    mmap_info_t *pi = DATAPTR(info);
    if (pi->ptrOK)
	return MMAP_ADDR(x);
    else
	error("cannot access data pointer for this mmaped vector");
}

static void *mmap_dataptr_or_null(SEXP x)
{
    SEXP info = MMAP_INFO(x);
    mmap_info_t *pi = DATAPTR(info);
    return pi->ptrOK ? MMAP_ADDR(x) : NULL;
}

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

static R_altrep_class_t R_mmap_integer_class;
static R_altrep_class_t R_mmap_real_class;

static void InitMmapIntegerClass(DllInfo *info)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("mmap_integer", "simplemmap", info);
    R_mmap_integer_class = cls;
 
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
    R_mmap_real_class = cls;

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
static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK)
{
    /* I'm sure tis needs adjusting for Windows, so punt for now. */
    error("not supported on Windows yet");
}
#else
/* derived from the example in
  https://www.safaribooksonline.com/library/view/linux-system-programming/0596009585/ch04s03.html */

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

static void mmap_finalize(SEXP eptr)
{
    REprintf("finalizing ... ");
    void *p = R_ExternalPtrAddr(eptr);
    size_t len = REAL_ELT(CADR(R_ExternalPtrProtected(eptr)), 0);
    R_SetExternalPtrAddr(eptr, NULL);

    if (p != NULL) {
	munmap(p, len); /* don't check for errors */
	R_SetExternalPtrAddr(eptr, NULL);
    }
    REprintf("done\n");
}

static SEXP mmap_list = NULL;

#define MAXCOUNT 10

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
    
}

static void finalize_mmap_objects()
{
    /* finalize any remaining mmap objects before unloading */
    for (SEXP next = CDR(mmap_list); next != R_NilValue; next = CDR(next))
	R_RunWeakRefFinalizer(CAR(next));
    R_ReleaseObject(mmap_list);
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

    SEXP data = PROTECT(CONS(file, R_NilValue));
    SETCDR(data, CONS(ScalarReal(sb.st_size), R_NilValue));
    SEXP dinfo = allocVector(INTSXP, 3);
    INTEGER(dinfo)[0] = type;
    INTEGER(dinfo)[1] = ptrOK;
    INTEGER(dinfo)[2] = wrtOK;
    SETCDR(CDR(data), CONS(dinfo, R_NilValue));
    SEXP eptr = PROTECT(R_MakeExternalPtr(p, install("mmap"), data));
    SEXP info = PROTECT(allocVector(RAWSXP, sizeof(mmap_info_t)));
    mmap_info_t *pi = DATAPTR(info);
    pi->ptrOK = ptrOK;
    pi->wrtOK = wrtOK;

    register_mmap_eptr(eptr);

    R_altrep_class_t class;
    switch(type) {
    case INTSXP:
	pi->len = sb.st_size / sizeof(int);
	class = R_mmap_integer_class;
	break;
    case REALSXP:
	pi->len = sb.st_size / sizeof(double);
	class = R_mmap_real_class;
	break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP ans = R_new_altrep(class, eptr, info);
    if (ptrOK && ! wrtOK)
	MARK_NOT_MUTABLE(ans);

    UNPROTECT(3); /* data, eptr, info */
    return ans;
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

    int type = INTSXP;
    if (stype != R_NilValue) {
	const char *typestr = CHAR(asChar(stype));
	if (strcmp(typestr, "double") == 0)
	    type = REALSXP;
	else if (strcmp(typestr, "integer") != 0)
	    error("type '%s' is not supported", typestr);
    }    

    Rboolean ptrOK = sptrOK == R_NilValue ? FALSE : asLogicalNA(sptrOK, FALSE);
    Rboolean wrtOK = swrtOK == R_NilValue ? FALSE : asLogicalNA(swrtOK, FALSE);

    if (TYPEOF(file) != STRSXP || LENGTH(file) != 1 || file == NA_STRING)
	error("invalud 'file' argument");

    return mmap_file(file, type, ptrOK, wrtOK);
}

static const R_ExternalMethodDef ExtEntries[] = {
    {"mmap_file", (DL_FUNC) &do_mmap_file, -1},
    {NULL, NULL, 0}
};

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
