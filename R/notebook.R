#' @keywords internal
`pack Test parameters tab` <- function(nb)
{
    tab <- tk2notetab(nb, "Test parameters")
    gridder <- function(x) tkgrid(x, sticky = "w", pady = 3)

    # Labels
    fr.lab <- tkframe(tab)
    add_label <- function(x) {
        gridder(.tklabel(fr.lab, text = x, justify = "left"))
    }
    labs <- c("Test Type", "Type 1 Error", "Power", "Sample Size (n)")
    sapply(labs, add_label)

    # Fields
    fr.field <- tkframe(tab, pady = 3)

    ## Test type
    testtype.selector <- create_combobox(parent = fr.field,
                                         param.name = "test.side",
                                         choices = c("1-Sided", "2-Sided"),
                                         state = "readonly")
    txt.width = 8
    gridder(testtype.selector)
    gridder(create_numeric_entry(fr.field, "alpha", "0.05", width = txt.width,
                                 min = 0, max = 1))
    gridder(create_numeric_entry(fr.field, "power", "0.8", width = txt.width,
                                 min = 0, max = 1))
    gridder(create_numeric_entry(fr.field, "sample.size", width = txt.width,
                                 min = 0))
    tkgrid(fr.lab, fr.field, sticky = "w", padx = 5, pady = 5)

    invisible(tab)
}


#' @keywords internal
`pack Boundaries tab` <- function(nb)
{
    tab <- tk2notetab(nb, "Boundaries")
    invisible(tab)
}


#' @keywords internal
pack_tabs <- function(nb, tabs)
{
    for (tab in tabs) {
        packfunc <- paste("pack", tab, "tab")
        do.call(packfunc, args = list(nb = nb))
    }

}

