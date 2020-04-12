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
gui <- function(root, tabs = c("Test parameters", "Boundaries"))
{
    # Init
    version <- utils::packageVersion(utils::packageName())
    tkwm.title(root, paste0("GroupSeq ", version))
    if (.Platform$OS.type == "windows") {
        iconfile <- system.file("icons", "gs-logo.ico", package = "GroupSeq")
        tkwm.iconbitmap(root, iconfile)
    }

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
    nlook.lab <- .tklabel(fr.looks, text = "Number of looks ", justify = "left")
    nlook.selector <- create_combobox(parent = fr.looks,
                                      param.name = "nlook",
                                      choices = as.character(1:10),
                                      state = "readonly")
    tkgrid(nlook.lab, nlook.selector, padx = 2, pady = 5)

    # Notebook
    nb <- tk2notebook(parent = fr.main, tabs = tabs, padding = 5, width = 500, height = 200)
    pack_tabs(nb, tabs)

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

