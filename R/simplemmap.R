mmap <- function(filename, type, ptrOK = FALSE, wrtOK = FALSE) {
    .External(C_mmap_file, filename, type, ptrOK, wrtOK)
}

