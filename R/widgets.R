#' @keywords internal
on_change_nlook <- function(x) {
    # TODO: implement
    message("selecting ", x, " looks")
    #as.integer(tclvalue(tcl(cb, "get")))
}

#' @title combobox factory
#' @keywords internal
create_combobox <- function(parent, param.name, choices = NULL,
                             onSelect = function(x) message("selected ", x),
                             ...)
{
    param.name.choices <- paste0(param.name, ".choices")
    choices <- .par$peek(param.name.choices,
                         default = as.character(choices))

    .par$add(param.name, tclVar(choices[1]))
    cb.var <- .par$get(param.name)

    width  <- max(nchar(choices)) + 1
    cb <- ttkcombobox(parent, value = choices, textvariable = cb.var,
                      width = width, ...)

    signal_selected <- function() onSelect(tclvalue(cb.var))
    tkbind(cb, "<<ComboboxSelected>>", signal_selected)
    invisible(cb)
}


#' @title entry factory
#' @keywords internal
create_numeric_entry <- function(parent, param.name,
                                 value = "",
                                 justify = "right",
                                 min = -Inf, max = Inf,
                                 ...)
{
    .par$add(param.name, tclVar(value))
    e.var <- .par$get(param.name)

    validatecommand <- function() {
        val <- tclvalue(e.var)
        .par$set(".last.entry", val, add = TRUE)

        num <- suppressWarnings(as.numeric(val))
        if (isTRUE(num >= min) && isTRUE(num <= max) || nchar(val) == 0) {
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
