#' @keywords internal
on_change_nlook <- function(x)
{
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
    #.par$set("nlook", default = tclVar("1"))
}

#' @keywords internal
create_number_of_looks_combobox <- function(parent, nMax = 10)
{
    choices <- as.character(seq_len(nMax))
    if (!.par$has("nlook")) .par$add("nlook", tclVar("1"))
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
    tab <- tk2notetab(nb, "Test parameters")
    frame <- tkframe(tab)
    add_label <- function(x) {
        tkgrid(.tklabel(frame, text = x, justify = "left"), sticky = "w")
    }
    labs <- c("Test Type", "Type I Error", "Power", "Sample Size (n)")
    sapply(labs, add_label)
    tkgrid(frame, sticky = "w")
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

