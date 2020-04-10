#' @keywords internal
create_file_menu <- function(parent, root)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    # TODO: load, save, save as ...
    onLoad <- function() print("load")
    onSave <- function() print("save")
    onSaveAs <- function() print("save as")
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
    on1 <- function() print("One Sample...")
    on2 <- function() print("Two Samples...")
    onIRL <- function() print("Release limits...")
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

    onAbout <- function() print("about")
    tkadd(menu, "command", label = "About", command = onAbout)
    menu
}

