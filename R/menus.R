#' @keywords internal
create_file_menu <- function(parent, root)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    onNew <- function() {
        tkdestroy(root)
        get.par()$clear()
        start_gui()
    }
    onLoad <- function() {
        fn <- tclvalue(tkgetOpenFile(filetypes = "{{Config Files} {.rds}}",
                                     parent = get.root()))
        if (nchar(fn) > 0) {
            param_list <- readRDS(fn)
            update_tcl_parameters_from_list(get.par(), param_list)
        }
        invisible()
    }
    onSave <- function() {
        fn <- tclvalue(tkgetSaveFile(filetypes = "{{Config Files} {.rds}}",
                                     parent = get.root()))
        if (nchar(fn) > 0) {
            param_list <- parameters_to_list(get.par())
            saveRDS(param_list, file = fn)
        }
        invisible()
    }
    onSaveAs <- function() {
        fn <- tclvalue(tkgetSaveFile(filetypes = "{{Config Files} {.rds}}",
                                     parent = get.root()))
        if (nchar(fn) > 0) {
            param_list <- parameters_to_list(get.par())
            saveRDS(param_list, file = fn)
        }
        invisible()
    }
    tkadd(menu, "command", label = "New...", command = onNew)
    tkadd(menu, "command", label = "Load config...", command = onLoad)
    tkadd(menu, "command", label = "Save config...", command = onSave)
    tkadd(menu, "command", label = "Save config as...", command = onSaveAs)
    tkadd(menu, "separator")
    tkadd(menu, "command", label = "Quit", command = onQuit)
    menu
}


#' @keywords internal
create_design_menu <- function(parent)
{
    menu <- tk2menu(parent, tearoff = FALSE)
    on1 <- function() message("One Sample...")
    on2 <- function() message("Two Samples...")
    onIRL <- function() message("Release limits...")
    stdMenu <- tk2menu(menu, tearoff = FALSE)

    tkadd(stdMenu, "command", label = "One Sample...", command = on1)
    tkadd(stdMenu, "command", label = "Two Samples...", command = on2)
    tkadd(menu, "cascade", label = "Standard", menu = stdMenu)
    tkadd(menu, "command", label = "Release limits...", command = onIRL)
    menu
}


#' @keywords internal
create_help_menu <- function(parent)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    onAbout <- function() message("about")
    tkadd(menu, "command", label = "About", command = onAbout)
    menu
}


#' @keywords internal
on_change_nlook <- function(x)
{
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
    #.par$set("nlook", default = tclVar("1"))
}

#' @keywords internal
create_number_of_looks_combobox <- function(parent, nMax = 10)
{
    choices <- as.character(seq_len(nMax))
    if (!.par$has("nlook")) .par$add("nlook", tclVar("1"))
    cb.var <- .par$get("nlook")
    signal_number_of_looks <- function() on_change_nlook(tclvalue(cb.var))

    cb <- ttkcombobox(parent, value = choices, textvariable = cb.var,
                      state = "readonly", width = 2)
    tkbind(cb, "<<ComboboxSelected>>", signal_number_of_looks)
    invisible(cb)
}


