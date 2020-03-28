#' @keywords internal
add_custom_icons <- function()
{
    path <- system.file("icons", package = "GroupSeq")
    iconFiles <- list.files(path, full.names = TRUE, pattern = "*.png")
    iconNames <- tools::file_path_sans_ext(basename(iconFiles))
    addStockIcons(iconNames, iconFiles)
}

#' @title Create graphical user interface.
#' @description This function builds the main GUI that appears when [GroupSeq]
#' is started.
#' @return Invoked for its side effects.
#' @import gWidgets gWidgetstcltk tcltk
#' @keywords internal
gui <- function()
{
    version <- utils::packageVersion(utils::packageName())
    #options("guiToolkit" = "tcltk")
    options("guiToolkit"="RGtk2")
    add_custom_icons()
    win <- gwindow(title = paste0("GroupSeq (version ", version, ")"),
                  visible = TRUE)

    items.file <- list(quit = gaction("Quit", icon = "quit",
                                      handler = function(...) dispose(win)))
    items.calc <- list(calc = gaction("Calculate", icon = "matrix"))
    items.help <- list(about = gaction("About", icon = "info-icon"))
    menubar <- list("File" = items.file,
                    "Calculate" = items.calc,
                    "Help" = items.help)
    gmenu(menubar, container = win)

    grp_main <- ggroup(horizontal = TRUE, container = win)

    gf.input <- gframe("Input", container = grp_main, expand = T)
	g.input <- ggroup(container = gf.input)

    menu = gcombobox(c("Probabilities given drift",
                       "Drift given power",
                       "Confidence interval"),
                     container = g.input);

    gf.out <- gframe("Output", container = grp_main, expand = T)
    g.out <- ggroup(container = gf.out)
    addSpace(g.out, val = 30, horizontal = FALSE)
    invisible()
}

