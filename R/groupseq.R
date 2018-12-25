#' @title Group sequential design probabilities - with graphical user interface.
#'
#' @description Computes probabilities related to group sequential designs for
#'  normally distributed test statistics. Enables to derive critical
#'  boundaries, power, drift, and confidence intervals of such designs.
#'  Supports the alpha spending approach by Lan-DeMets as well as the
#'  conditional rejection probability principle by Müller and Schäfer.
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @docType package
#' @name groupseq.pkg
NULL


pkg.env <- new.env(parent=emptyenv())
pkg.env$taskWindow <- NULL
pkg.env$scipen.old <- options(scipen=10)[[1]]


#' @title Start GroupSeq
#' @description Starts the graphical user interface, optionally the legacy
#'  version of GroupSeq prior to version 2.0.
#' @param legacy (logical) if \code{TRUE} starts legacy GroupSeq version < 2.0
#' @export
groupseq <- function(legacy=FALSE)
{
    mode <- match.arg(mode)
    switch(mode,
           "g" = guiMode(),
           stop("Invalid mode '", mode, "'"))
}



#' @keywords internal
.onAttach <- function(libname, pkgname)
{
    #groupseq()
}

#' @keywords internal
quitGroupSeq <- function() {
    if (!is.null(pkg.env$taskWindow)) {
        tcltk::tkdestroy(pkg.env$taskWindow)
        pkg.env$taskWindow <- NULL
    }
    options(scipen=pkg.env$scipen.old)
    invisible()
}

#' @keywords internal
.onUnload <- function(libpath)
{
    quitGroupSeq()
}

#' @keywords internal
.onDetach <- function(libpath)
{
    quitGroupSeq()
}

