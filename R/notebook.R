#' @keywords internal
create_number_of_looks_combobox <- function(parent, nMax = 10)
{
    on_change_nlook <- function(x) {
        # TODO: implement
        message("selecting ", x, " looks")
        #as.integer(tclvalue(tcl(cb, "get")))
    }

    choices <- as.character(seq_len(nMax))
    if (!.par$has("nlook")) .par$add("nlook", tclVar(choices[1]))
    cb.var <- .par$get("nlook")
    signal_number_of_looks <- function() on_change_nlook(tclvalue(cb.var))

    cb <- ttkcombobox(parent, value = choices, textvariable = cb.var,
                      state = "readonly", width = 2)
    tkbind(cb, "<<ComboboxSelected>>", signal_number_of_looks)
    invisible(cb)
}


#' @keywords internal
`pack Test parameters tab` <- function(nb)
{
    on_change_test_type <- function(x) {
        # TODO: implement
        message("selecting ", x, " test")
        #as.integer(tclvalue(tcl(cb, "get")))
    }

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
    choices <- .par$peek("test.type.choices", default = c("1-Sided", "2-Sided"))
    if (!.par$has("test.side")) .par$add("test.side", tclVar(choices[1]))
    cb.var <- .par$get("test.side")
    signal_test_type <- function() on_change_test_type(tclvalue(cb.var))
    cb <- ttkcombobox(fr.field, value = choices, textvariable = cb.var,
                      state = "readonly", width = max(nchar(choices)) + 2)
    tkbind(cb, "<<ComboboxSelected>>", signal_test_type)
    tkgrid(cb)

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

