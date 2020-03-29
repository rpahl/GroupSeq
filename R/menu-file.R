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
