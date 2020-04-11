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
    tkwm.title(root, paste0("GroupSeq (version ", version, ")"))

    # Menu
    menu <- tk2menu(root)
    tkconfigure(root, menu = menu)
    tkadd(menu, "cascade", label = "File", menu = create_file_menu(menu, root))
    tkadd(menu, "cascade", label = "Design", menu = create_design_menu(menu))
    tkadd(menu, "cascade", label = "Help", menu = create_help_menu(menu))

    # Main part
    fr.main <- tkframe(root, relief = "flat", borderwidth = 1)

    # Number of looks
    fr.looks <- tkframe(fr.main, borderwidth = 5)
    looks.lab <- tklabel(fr.looks, anchor = "nw", justify = "left",
                         text = "Number of interim looks ")
    tkgrid(looks.lab, create_number_of_looks_combobox(parent = fr.looks))

    # Notebook
    nb <- ttknotebook(parent = fr.main)

    ## Design parameters
    tab.design <- tkframe(root, borderwidth = 3)
    tkadd(nb, tab.design, text = "Design parameters", padding = 3)
    fr.lab <- tkframe(tab.design)
    add_label <- function(x) {
        tkgrid(tklabel(fr.lab, text = x, justify = "left"), sticky = "w")
    }
    labs <- c("Test Type", "Type I Error", "Power", "Sample Size (n)",
              "Allocation Ratio")
    sapply(labs, add_label)
    tkgrid(fr.lab, sticky = "w")

    ## Boundaries
    tab.bounds <- tkframe(root)
    tkadd(nb, tab.bounds, text = "Boundaries", padding = 3)


    tkgrid(fr.looks)
    tkgrid(nb, sticky = "nw")
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

