# Environment used by legacy version < 2
pkg.env <- new.env(parent = emptyenv())

# Environments used since version 2
.env <- NULL

init_env <- function(legacy = FALSE)
{
    if (legacy) {
        pkg.env$taskWindow <- NULL
        pkg.env$scipen.old <- options(scipen=10)[[1]]
        pkg.env
    } else {
        .env$add("par", container::dict())
        .env$add("par.last", container::dict())
        .env$add("name", "")
        .env$add("root", tcltk::tktoplevel())
        .env
    }
}


get.par <- function() .env$at2("par")


get.par.last <- function() .env$at2("par.last")


add.par <- function(key, value) {
    get.par$add(key, value)
    get.par.last$add(key, value)
}


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


isNew <- function() nchar(.env$at2("name")) == 0


update_title <- function()
{
    name <- if (isNew()) "[New]" else .env$at2("name")
    plus <- if (has_changed_parameters()) " + " else ""
    title <- paste0(name, plus, " - GroupSeq")
    tkwm.title(.env$at2("root"), title)
}


update_changed_parameters <- function()
{
    param_list <- lapply(as.list(.env$at2("par")), FUN = tclvalue)
    .env$replace_at("par.last", container::as.dict(param_list))
    update_title()
}


#' @title Start GroupSeq
#' @description Starts the graphical user interface.
#' @export
start_gui <- function()
{
    legacy = TRUE
    init_env(legacy)

    if (legacy) {
        guiMode()
    } else {
        gui(.env$at2("root"))
    }
    invisible()
}


.onLoad <- function(libname, pkgname)
{
    .env <<- container::dict()

    doStart <- getOption("AutostartGroupSeq", default = TRUE)
    if (interactive() && doStart) {
        start_gui()
    } else {
        invisible()
    }
}


onQuit <- function() {
    isLegacy <- .env$is_empty()
    if (isLegacy) {
        if (!is.null(pkg.env$taskWindow)) {
            tkdestroy(pkg.env$taskWindow)
            pkg.env$taskWindow <- NULL
            options(scipen = pkg.env$scipen.old)
        }
    } else {
        tkdestroy(.env$at2("root"))
        .env$clear()
    }
    invisible()
}


.onUnload <- function(libpath) {
    onQuit()
}

.onDetach <- function(libpath) {
    onQuit()
}

