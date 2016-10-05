% Copyright 2016 R Core Team
% Distributed under GPL 2 or 3

\name{mmap}
\alias{mmap}
\alias{munmap}
\title{Simple Memory-Mapped Vectors}
\description{
  Create vectors with data in a memory-mapped file.
}
\usage{
mmap(filename, type = c("double", "integer", "int"), ptrOK = TRUE,
     wrtOK = FALSE, serOK = TRUE)
munmap(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{character; name of the file to be memory-mapped.}
  \item{type}{character; the binary data type of the elements in the file.}
  \item{ptrOK}{logical; whether the data pointer should be accessible.}
  \item{wrtOK}{logical; whether modifying the file is allowed.}
  \item{serOK}{logical; whether the object should be serialized as an
               mmaped file.}
  \item{x}{a memory-mapped object}
}
\details{

  \code{mmap} creates vector with data in a specified memory-mapped
  file. The \code{type} argument specifies the binary element type in
  the file; for now only \code{C} \code{double} and \code{int} are
  supported.

  If \code{ptrOK} is true, then \code{C} code requesting the data
  pointer will succeed, which means all C code written to work with R
  vectors should work, but may cause large allocations for result
  objects if the file mapped file is large. If \code{ptrOK} is false,
  then \code{C} code written to use the data pointer will fail; only
  \code{C} code written to access individual elements or blocks will
  succeed. Code for basic summary methods and subsettin has been
  rewritten to access data only by elements or blocks.

  The file is opened read-only if \code{wrtOK} is false. In this
  case the vector is marked as not mutable and will be duplicated before
  assignments are committed. Attempts by \code{C} code to write to the data
  will produce a segmentation fault. If \code{wrtOK} is true, then the
  file is opened for reading and writing, and an assignment that would
  modify a standard vector in place will modify the file.

  A vector created with \code{serOK} false is serialized as an ordinary
  vector, which will create a very large serialization if the mapped
  file is large. A vector created with \code{ptrOK} true is serialized
  by recording the file name and data type as specified to \code{mmap}
  in the serialization, and unserializing will attempt to recreate the
  memory mapping. If this fails, \code{unserialize} will issue a warning
  and return a zero-length vector.
}
\value{
  A vector of the specified type with data in a memory-mapped file.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
  Luke Tierney
}

\examples{
tmp <- tempfile()
x <- runif(1000)
writeBin(x, tmp)
y <- mmap(tmp)
head(x)
head(y)
munmap(y)
unlink(tmp)
}
\keyword{file}
