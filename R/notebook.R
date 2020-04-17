#' @keywords internal
`pack Test parameters tab` <- function(nb)
{
    tab <- tk2notetab(nb, "Test parameters")
    fr.tab <- tk2frame(tab)
    grid <- function(...) tkgrid(..., sticky = "w", pady = 3, padx = 2)
    add_label <- function(x, fr) grid(.tklabel(fr, text = x, justify = "left"))

    # Labels
    fr.lab <- tk2frame(fr.tab)
    labs <- c("Test Type", "Type 1 Error (\u3b1)",
              "Power (1 - \u3b2)", "Sample Size (n)")
    sapply(labs, add_label, fr = fr.lab)

    # Fields
    fr.field <- tkframe(fr.tab)
    testtype.selector <- create_combobox(parent = fr.field,
                                         param.name = "test.side",
                                         choices = c("1-Sided", "2-Sided"),
                                         state = "readonly")
    grid(testtype.selector, columnspan = 2)
    create_entry <- function(...) create_numeric_entry(fr.field, ...)
    grid(create_entry("alpha", "0.05", width = 8, min = 0, max = 1))
    e.power <- create_entry("power", "0.8", width = 8, min = 0, max = 1)
    e.sample.size <- create_entry("sample.size", value = "0", width = 8,
                                  min = 0, state = "disabled")
    onRadioSelect <- function(x) {
        if (x == "1") {
            config(e.power, state = "active", tip = "")
            config(e.sample.size, state = "disabled",
                   tip = "The sample size is going to be computed")
        } else {
            config(e.power, state = "disabled",
                   tip = "The power is going to be computed")
            config(e.sample.size, state = "active", tip = "")
        }
    }
    create_rb <- function(...) {
        create_radiobutton(fr.field, "test.param.radiobutton",
                           onSelect = onRadioSelect, ...)
    }
    get.par()$add("test.param.radiobutton", tclVar("1"))
    rb.power <- create_rb(value = 1)
    rb.sample.size <- create_rb(value = 2)
    grid(e.power, rb.power)
    grid(e.sample.size, rb.sample.size)

    # Distribution
    fr.distparams <- tk2labelframe(fr.tab, text = "Distribution")
    fr.lab2 <- tk2frame(fr.distparams)
    sapply(c("Drift", "Std dev (\u3c3)"), add_label, fr = fr.lab2)
    fr.field2 <- tk2frame(fr.distparams)
    tip.drift <- paste0("Drift is the expected test statistic\n",
                  "for the full sample, that is, when time = 1.")
    grid(create_numeric_entry(fr.field2, "drift", "0", width = 7, tip = tip.drift))
    grid(create_numeric_entry(fr.field2, "sd", "1", width = 7, min = 0, cmp.min = ">"))
    tkgrid(fr.lab2, fr.field2, padx = 5)

    # Put it all together
    tkgrid(fr.lab, fr.field, fr.distparams, sticky = "nw", padx = 5, pady = 5)
    tkgrid(fr.tab, pady = 10)
    invisible(tab)
}


#' @keywords internal
`pack Boundaries tab` <- function(nb)
{
    tab <- tk2notetab(nb, "Boundaries")
    fr.tab <- tk2frame(tab)
    grid <- function(...) tkgrid(..., sticky = "w", pady = 3, padx = 2)
    add_label <- function(x, fr) grid(.tklabel(fr, text = x, justify = "left"))

    # Methods
    fr.eff.fut <- tk2frame(fr.tab)
    fr.eff <- tk2labelframe(fr.eff.fut, text = "Efficacy")
    add_label("Boundary method", fr.eff)
    fr.eff.spend <- tk2frame(fr.eff)

    fr.fut <- tk2labelframe(fr.eff.fut, text = "Futility")
    add_label("Boundary method", fr.fut)
    fr.fut.lab <- tk2frame(fr.fut)
    tkgrid(fr.eff, fr.fut, padx = 5, pady = 5, sticky = "nw")


    # Input table
    fr.table <- tk2frame(fr.tab)
    data <- as.data.frame(matrix(1:40, ncol = 4))
    table <- create_table(fr.table)
    fill_table(table, data)
    tkgrid(table)

    # Put it all together
    tkgrid(fr.eff.fut, padx = 10, pady = 5, sticky = "nw")
    tkgrid(fr.table, padx = 10, pady = 5, sticky = "nw")

    tkgrid(fr.tab, sticky = "nw")
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


