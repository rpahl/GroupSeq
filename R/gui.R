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
#' @import tcltk tcltk2
#' @keywords internal
gui <- function(root)
{
    # Init
    version <- utils::packageVersion(utils::packageName())
    tkwm.title(root, paste0("GroupSeq ", version))

    # Menu
    menu <- tkmenu(root)
    tkconfigure(root, menu = menu)
    tkadd(menu, "cascade", label = "File", menu = create_file_menu(menu, root))
    tkadd(menu, "cascade", label = "Design", menu = create_design_menu(menu))
    tkadd(menu, "cascade", label = "Help", menu = create_help_menu(menu))

    # Main part
    fr.main <- tkframe(root, relief = "flat", borderwidth = 1)

    # Number of looks
    fr.looks <- tkframe(fr.main, padx = 10)
    looks.lab <- .tklabel(fr.looks, text = "Number of looks ", justify = "left")
    tkgrid(looks.lab, create_number_of_looks_combobox(parent = fr.looks),
           padx = 2, pady = 5)

    # Notebook
    nb <- tk2notebook(parent = fr.main, tabs = c("Test parameters", "Boundaries"),
                      padding = 5)

    ## Test parameters
    tab.test <- tk2notetab(nb, "Test parameters")
    #tkadd(nb, tab.test, text = "Test parameters", padding = 5)
    fr.lab <- tkframe(tab.test, padx = 10)
    add_label <- function(x) {
        tkgrid(.tklabel(fr.lab, text = x, justify = "left"), sticky = "w")
    }
    labs <- c("Test Type", "Type I Error", "Power", "Sample Size (n)")
    sapply(labs, add_label)
    tkgrid(fr.lab, sticky = "w")

    ## Boundaries
    #tab.bounds <- tkframe(root)
    tab.bounds <- tk2notetab(nb, "Boundaries")
    #tkadd(nb, tab.bounds, text = "Boundaries", padding = 5)

    tkgrid(fr.looks, sticky = "nw")
    tkgrid(nb, sticky = "w", pady = 2, padx = 3)
    tkgrid(fr.main)
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

