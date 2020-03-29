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


.groupseqEnv <- new.env(parent = emptyenv())
groupseqEnv <- function() .groupseqEnv


#' @title Start GroupSeq
#' @description Starts the graphical user interface, optionally the legacy
#'  version of GroupSeq prior to version 2.
#' @param legacy `logical` if `TRUE`, starts legacy GroupSeq version < 2.
#' @export
start_gui <- function(legacy = FALSE)
{
    if (legacy) {
        guiMode()
    } else {
        pkg.env$root <- tcltk::tktoplevel()
        gui(pkg.env$root)
    }
    invisible()
}


#' @keywords internal
.onAttach <- function(libname, pkgname)
{
    doStart <- getOption("AutostartGroupSeq", default = FALSE)
    if (interactive() && doStart) {
        start_gui()
    } else {
        invisible()
    }
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
.onUnload <- function(libpath) quitGroupSeq()

#' @keywords internal
.onDetach <- function(libpath) quitGroupSeq()

