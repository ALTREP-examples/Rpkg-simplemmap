\name{mmap}
\alias{mmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple Memory-Mapped Vectors}
\description{
  Create R vectors with data in a memory-mapped file.
}
\usage{
mmap(filename, type = c("double", "integer", "int"), ptrOK = FALSE, wrtOK = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{character; name of the file to be memory-mapped.}
  \item{type}{character; the binary data type of the elements in the file.}
  \item{ptrOK}{logical; whether the data pointer should be accessible.}
  \item{wrtOK}{logical; whether modifying the file is allowed.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  An R vector of the specified type with data in a memory-mapped file.
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
unlink(tmp)
}
\keyword{file}
