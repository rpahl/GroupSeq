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
#'  is started.
#' @return Invoked for its side effects.
#' @import tcltk
#' @keywords internal
gui <- function(root)
{
    # Init
    version <- utils::packageVersion(utils::packageName())
    #win <- gwindow(title = , visible = TRUE)
    tkwm.title(root, paste0("GroupSeq (version ", version, ")"))

    # Menu
    menu.file <- tkmenu(root)

    items.file <- list(quit = "Quit", icon = "quit")
    items.help <- list("About" = list(icon = "info-icon", handler = show_about))
    menubar <- list("File" = items.file, "Help" = items.help)

    # Toolbar
    #a1 <- gaction(label = "   One\nSample", icon = "normal-dens30px", handler = function(h, ...) print("one"))
    #a2 <- gaction(label = "   Two\nSamples", icon = "normal-dens-bw", handler = function(h, ...) print("two"))
    #tbl <- list(One = a1, Two = a2)
    #tb <- gtoolbar(tbl, container = win)

    # Notebook
    #nb <- gnotebook(expand = TRUE, container = gf.input)
    #glabel(text = "Design Parameters", container = nb, label = "Design Parameters")
    #glabel(text = "Boundaries", container = nb, label = "Boundaries")
    #svalue(nb) <- 1


    # Input
    if (F) {

        g.input <- ggroup(container = gf.input)

        menu = gcombobox(c("Probabilities given drift",
                           "Drift given power",
                           "Confidence interval"),
                         container = g.input);

        gf.out <- gframe("Output", container = grp_main, expand = T)
        g.out <- ggroup(container = gf.out)
        addSpace(g.out, val = 30, horizontal = FALSE)
    }
    invisible(root)
}

show_about <- function(...)
{
    msg <- paste0("Computes probabilities related to group sequential ",
                  "designs for normally distributed test statistics. ",
                  "Enables to derive critical boundaries, power, drift, ",
                  "and confidence intervals of such designs. Supports the ",
                  "alpha spending approach by Lan-DeMets as well as the ",
                  "conditional rejection probability principle by Müller ",
                  "and Schäfer.")
    gmessage(title = "About", message = msg, icon = "info")
}

