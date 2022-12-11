# Environment used by legacy version < 2
pkg.env <- new.env(parent = emptyenv())

# Environments used since version 2
.env <- NULL

init_env <- function(legacy = FALSE)
{
    if (legacy) {
        pkg.env$taskWindow <- NULL
        pkg.env$scipen.old <- options(scipen = 10)[[1]]
        pkg.env
    } else {
        .env[["par"]] <- new.env()
        .env[["par.last"]] <- new.env()
        .env[["name"]] <- ""
        .env[["root"]] <- tcltk::tktoplevel()
        .env
    }
}


get.par <- function() .env[["par"]]


get.par.last <- function() .env[["par.last"]]


add.par <- function(key, value) {
    get.par$add(key, value)
    get.par.last$add(key, value)
}


has_changed_parameters <- function()
{
    param_list <- as.list(get.par())
    current <- lapply(as.list(param_list), tcltk::tclvalue)
    last <- as.list(get.par.last())
    if (length(last) != length(current)) stop("length mismatch")
    last <- last[names(current)]
    hasChanges <- !identical(current, last)
    hasChanges
}


isNew <- function() nchar(.env[["name"]]) == 0


update_title <- function()
{
    name <- if (isNew()) "[New]" else .env[["name"]]
    plus <- if (has_changed_parameters()) " + " else ""
    title <- paste0(name, plus, " - GroupSeq")
    tcltk::tkwm.title(.env[["root"]], title)
}


update_changed_parameters <- function()
{
    param_list <- lapply(
        as.list(.env[["par"]]),
        FUN = tcltk::tclvalue
    )

    .env[["par.last"]] <- list2env(param_list)
    update_title()
}


#' @title Start GroupSeq
#' @description Starts the graphical user interface.
#' @return No return value, called for side effects.
#' @export
#' @examples
#' start_gui()
start_gui <- function()
{
    legacy = TRUE
    init_env(legacy)

    if (legacy) {
        guiMode()
    } else {
        gui(.env[["root"]])
    }
    invisible()
}


.onLoad <- function(libname, pkgname)
{
    .env <<- new.env()

    doStart <- getOption("AutostartGroupSeq", default = TRUE)
    if (interactive() && doStart) {
        start_gui()
    } else {
        invisible()
    }
}


onQuit <- function() {
    #isLegacy <- .env$is_empty()
    isLegacy <- TRUE
    if (isLegacy) {
        if (!is.null(pkg.env$taskWindow)) {
            tcltk::tkdestroy(pkg.env$taskWindow)
            pkg.env$taskWindow <- NULL
            options(scipen = pkg.env$scipen.old)
        }
    } else {
        tcltk::tkdestroy(.env[["root"]])
        #.env$clear()
    }
    invisible()
}


.onUnload <- function(libpath) {
    onQuit()
}

.onDetach <- function(libpath) {
    onQuit()
}

