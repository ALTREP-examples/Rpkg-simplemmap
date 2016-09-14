# Copyright 2016 R Core Team
# Distributed under GPL 2 or 3

mmap <- function(filename, type = c("double", "integer", "int"),
                 ptrOK = TRUE, wrtOK = FALSE) {
    type = match.arg(type)
    .External(C_mmap_file, filename, type, ptrOK, wrtOK)
}

munmap <- function(x)
    invisible(.External(C_munmap_file, x))

