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

# Environment used by legacy version < 2
pkg.env <- new.env(parent = emptyenv())
pkg.env$taskWindow <- NULL
pkg.env$scipen.old <- options(scipen=10)[[1]]

# Environments used since version 2
.env <- container::dict()
get.env <- function() .env

.par <- container::dict()
get.par <- function() .par

isNew <- function() !get.par()$has("load.par")
register_if_new <- function(key, value) {
    if (isNew()) get.par()$add(key, value)
}


#' @title Start GroupSeq
#' @description Starts the graphical user interface, optionally the legacy
#'  version of GroupSeq prior to version 2.
#' @param legacy `logical` if `TRUE`, starts legacy GroupSeq version < 2.
#' @param loadGUI `logical` if `TRUE`, loads parameters from earlier session
#' @export
start_gui <- function(legacy = FALSE, loadGUI = FALSE)
{
    if (legacy) {
        guiMode()
    } else {
        .env$add("root", tcltk::tktoplevel())
        if (loadGUI) get.par()$set("load.par", TRUE, add = TRUE)
        gui(get.env()$get("root"))
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
        tkdestroy(pkg.env$taskWindow)
        pkg.env$taskWindow <- NULL
    }
    options(scipen=pkg.env$scipen.old)
    invisible()
}


#' @keywords internal
.onUnload <- function(libpath) {
    quitGroupSeq()
}

#' @keywords internal
.onDetach <- function(libpath) {
    quitGroupSeq()
}

