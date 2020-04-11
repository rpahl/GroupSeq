#' @keywords internal
`pack Test parameters tab` <- function(nb)
{
    tab <- tk2notetab(nb, "Test parameters")

    # Labels
    fr.lab <- tkframe(tab)
    add_label <- function(x) {
        tkgrid(.tklabel(fr.lab, text = x, justify = "left"), sticky = "w")
    }
    labs <- c("Test Type", "Type I Error", "Power", "Sample Size (n)")
    sapply(labs, add_label)

    # Fields
    fr.field <- tkframe(tab)

    ## Test type
    testtype.selector <- create_combo_box(parent = fr.field,
                                          param.name = "test.side",
                                          choices = c("1-Sided", "2-Sided"),
                                          state = "readonly")
    tkgrid(testtype.selector)

    tkgrid(fr.lab, fr.field, sticky = "w")
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

