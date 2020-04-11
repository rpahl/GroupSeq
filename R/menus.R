#' @keywords internal
create_file_menu <- function(parent, root)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    # TODO: load, save, save as ...
    onLoad <- function() message("load")
    onSave <- function() message("save")
    onSaveAs <- function() message("save as")
    onQuit <- function() tkdestroy(root)
    tkadd(menu, "command", label = "Open config file...", command = onLoad)
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
on_change_number_of_looks <- function(x)
{
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
}

#' @keywords internal
create_number_of_looks_combobox <- function(parent, nMax = 10)
{
    choices <- as.character(seq_len(nMax))
    combovar <- tclVar("1")
    signal_number_of_looks <- function() on_change_number_of_looks(tclvalue(combovar))

    cb <- ttkcombobox(parent, value = choices, textvariable = combovar,
                      state = "readonly", width = 2)
    tkbind(cb, "<<ComboboxSelected>>", signal_number_of_looks)
    invisible(cb)
}


