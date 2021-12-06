#' @keywords internal
on_change_nlook <- function(x) {
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
}

#' @title [tcltk2::tk2combobox()] wrapper
#' @description Creates a [tcltk2::tk2combobox()] and adds its textvariable to the
#' global parameter dictionary.
#' @keywords internal
create_combobox <- function(parent, param.name, width, choices = NULL,
                            onSelect = function(x) message("selected ", x),
                            register = get.par(),
                            ...)
{
    param.name.choices <- paste0(param.name, ".choices")
    choices <- register$peek_at2(param.name.choices,
                                 default = as.character(choices))
    if (missing(width)) width  <- max(nchar(choices)) + 1

    get.par()$add(param.name, tclVar(choices[1]))
    cb.var <- register$at2(param.name)

    cb <- tk2combobox(parent, value = choices, textvariable = cb.var,
                      width = width, ...)

    signal_selected <- function() onSelect(tclvalue(cb.var))
    tkbind(cb, "<<ComboboxSelected>>", signal_selected)
    invisible(cb)
}


#' @title [tcltk2::tk2entry()] wrapper
#' @description Creates a [tcltk2::tk2entry()] with optional range checks and
#' adds its textvariable to the global parameter dictionary.
#' @details As long as the range is violated, the value in the entry field is
#' colored red.
#' @keywords internal
create_numeric_entry <- function(parent, param.name,
                                 value = "",
                                 justify = "right",
                                 min = -Inf, max = Inf,
                                 cmp.min = `>=`, cmp.max = `<=`,
                                 register = get.par(),
                                 ...)
{
    get.par()$add(param.name, tclVar(value))
    e.var <- register$at2(param.name)

    validatecommand <- function() {
        val <- tclvalue(e.var)

        # Make entry appear in red if it is outside of min/max range
        num <- suppressWarnings(as.numeric(val))
        if (isTRUE(cmp.min(num, min)) && isTRUE(cmp.max(num,max)) ||
            nchar(val) == 0) {
            tkconfigure(e, foreground = "black")
            tclVar(TRUE)
        } else {
            tkconfigure(e, foreground = "red")
            tclVar(FALSE)
        }
    }

    e <- tk2entry(parent, textvariable = e.var,
                  justify = justify, validate = "focus",
                  validatecommand = validatecommand,
                  ...)
    invisible(e)
}


#' @title [tcltk2::tk2radiobutton()] wrapper
#' @description Creates a [tcltk2::tk2radiobutton()] with optional range checks and
#' adds its textvariable to the global parameter dictionary.
#' @keywords internal
create_radiobutton <- function(parent, param.name, value,
                               onSelect = function(x) message("button ", x),
                               register = get.par(),
                               ...)
{
    rb.var <- register$at2(param.name)
    signal_selected <- function() onSelect(tclvalue(rb.var))
    rb <- tk2radiobutton(parent, variable = rb.var, value = value,
                         command = signal_selected, ...)
    invisible(rb)
}


#' @title `tklabel` wrapper with some changed default parameters
#' @param bg 'character' background color.
#' @param justify `character` alignment of the text.
#' @return [tcltk::tklabel()] object
#' @keywords internal
.tklabel <- function(..., bg = "grey95", justify = c("left", "center", "right"))
{
    arg.just <- match.arg(justify)
    tcltk::tklabel(..., bg = bg, justify = arg.just)
}

