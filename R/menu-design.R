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
