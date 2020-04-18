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
.env <- container::dict()

init_env <- function(legacy = FALSE)
{
    if (legacy) {
        pkg.env$taskWindow <- NULL
        pkg.env$scipen.old <- options(scipen=10)[[1]]

    } else {
        .env$add("root", tcltk::tktoplevel())
        .env$add("par", container::dict())
        .env$add("par.last", container::dict())
        .env$add("name", "")
    }
}


#' @keywords internal
get.par <- function() .env$get("par")

#' @keywords internal
get.par.last <- function() .env$get("par.last")


#' @keywords internal
add.par <- function(key, value) {
    get.par$add(key, value)
    get.par.last$add(key, value)
}


#' @keywords internal
has_changed_parameters <- function()
{
    param_list <- as.list(get.par())
    current <- lapply(as.list(param_list), tclvalue)
    last <- as.list(get.par.last())
    if (length(last) != length(current)) stop("length mismatch")
    last <- last[names(current)]
    hasChanges <- !identical(current, last)
    hasChanges
}


#' @keywords internal
isNew <- function() nchar(.env$get("name")) == 0


#' @keywords internal
update_title <- function()
{
    name <- if (isNew()) "[New]" else .env$get("name")
    plus <- if (has_changed_parameters()) " + " else ""
    title <- paste0(name, plus, " - GroupSeq")
    tkwm.title(.env$get("root"), title)
}


#' @keywords internal
update_changed_parameters <- function()
{
    param_list <- lapply(as.list(.env$get("par")), FUN = tclvalue)
    .env$set("par.last", container::dict(param_list))
    update_title()
}


#' @title Start GroupSeq
#' @description Starts the graphical user interface, optionally the legacy
#'  version of GroupSeq prior to version 2.
#' @param legacy `logical` if `TRUE`, starts legacy GroupSeq version < 2.
#' @export
start_gui <- function(legacy = FALSE)
{
    init_env(legacy)
    if (legacy) {
        guiMode()
    } else {
        gui(.env$get("root"))
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
    isLegacy <- .env$empty()
    if (isLegacy) {
        if (!is.null(pkg.env$taskWindow)) {
            tkdestroy(pkg.env$taskWindow)
            pkg.env$taskWindow <- NULL
            options(scipen=pkg.env$scipen.old)
        }
    } else {
        tkdestroy(.env$get("root"))
        .env$clear()
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

