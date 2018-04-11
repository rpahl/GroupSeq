pkg.env <- new.env(parent=emptyenv())
pkg.env$taskWindow <- NULL
pkg.env$scipen.old <- options(scipen=10)[[1]]


groupseq <- function(mode="g")
{
    mode <- match.arg(mode)
    switch(mode,
           "g" = guiMode(),
           "c" = consoleMode())
}

.onAttach <- function(libname, pkgname)
{
    groupseq()
}

quitGroupSeq <- function() {
    if (!is.null(pkg.env$taskWindow)) {
        tcltk::tkdestroy(pkg.env$taskWindow)
        pkg.env$taskWindow <- NULL
    }
    options(scipen=pkg.env$scipen.old)
    invisible()
}

.onUnload <- function(libpath)
{
    quitGroupSeq()
}

.onDetach <- function(libpath)
{
    quitGroupSeq()
}

