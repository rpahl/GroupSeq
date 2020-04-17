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

# Environments used since version 2
.env <- new.env()
get.env <- function() .env
get.root <- function() get("root", envir = get.env())

get.par <- function() get("par", envir = get.env())
get.par.last <- function() get("par.last", envir = get.env())

isNew <- function() !get.par()$has("load.par")
register_if_new <- function(key, value) {
    if (isNew()) get.par()$add(key, value)
}


#' @title Start GroupSeq
#' @description Starts the graphical user interface, optionally the legacy
#'  version of GroupSeq prior to version 2.
#' @param legacy `logical` if `TRUE`, starts legacy GroupSeq version < 2.
#' @export
start_gui <- function(legacy = FALSE)
{
    assign("legacy", legacy, envir = get.env())
    if (legacy) {
        pkg.env$taskWindow <- NULL
        pkg.env$scipen.old <- options(scipen=10)[[1]]
        guiMode()
    } else {
        assign("par", container::dict(), envir = get.env())
        assign("par.last", container::dict(), envir = get.env())

        assign("root", tcltk::tktoplevel(), envir = get.env())
        gui(get.root())
    }
    invisible()
}


#' @keywords internal
.onLoad <- function(libname, pkgname)
{
    doStart <- getOption("AutostartGroupSeq", default = FALSE)
    if (interactive() && doStart) {
        start_gui()
    } else {
        invisible()
    }
}


#' @keywords internal
onQuit <- function() {
    isLegacy <- get0("legacy", envir = get.env(), ifnotfound = TRUE)
    if (isLegacy) {
        if (!is.null(pkg.env$taskWindow)) {
            tkdestroy(pkg.env$taskWindow)
            pkg.env$taskWindow <- NULL
            options(scipen=pkg.env$scipen.old)
        }
    } else {
        tkdestroy(get.root())
        remove(list = ls(get.env()), envir = get.env())
    }
    invisible()
}


#' @keywords internal
.onUnload <- function(libpath) {
    onQuit()
}

#' @keywords internal
.onDetach <- function(libpath) {
    onQuit()
}

