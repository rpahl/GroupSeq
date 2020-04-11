#' @title combobox factory
#' @keywords internal
create_combo_box <- function(parent, param.name, choices = NULL,
                             onSelect = function(x) message("selected ", x),
                             ...)
{
    param.name.choices <- paste0(param.name, ".choices")
    choices <- .par$peek(param.name.choices,
                         default = as.character(choices))

    if (!.par$has(param.name)) .par$add(param.name, tclVar(choices[1]))
    cb.var <- .par$get(param.name)

    width  <- max(nchar(choices)) + 1
    cb <- ttkcombobox(parent, value = choices, textvariable = cb.var,
                      width = width, ...)

    signal_selected <- function() onSelect(tclvalue(cb.var))
    tkbind(cb, "<<ComboboxSelected>>", signal_selected)
    invisible(cb)
}

#' @keywords internal
on_change_nlook <- function(x) {
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
}


