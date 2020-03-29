#' @keywords internal
create_help_menu <- function(parent)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    onAbout <- function() print("about")
    tkadd(menu, "command", label = "About", command = onAbout)
    menu
}
