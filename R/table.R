#' @keywords internal
fill_table <- function(tab, data)
{
    # Arg checks
    stopifnot(inherits(tab, "tkwin"),
              is.data.frame(data))

    get_tclVar_from_dataframe <- function(r, c)
    {
        isHeader <- r == "0"
        val <- if(isHeader) {
            names(data)[as.integer(c)]
        } else {
            data[as.integer(r), as.integer(c)]
        }
        tclVar(as.character(val))
    }

    tkconfigure(tab,
                command = get_tclVar_from_dataframe,
                rows = nrow(data) + 1,
                cols = ncol(data),
                titlerows = 1)
    invisible(tab)
}


#' @keywords internal
get_cell_value <- function(tab, i, j)
{
    tclvalue(tcl(tab, "get", paste0(i, ", ", j)))
}


#' @keywords internal
set_cell_value <- function(tab, i, j, value)
{
    tcl(tab, "set", paste0(i, ", ", j), as.character(value))
    invisible(tab)
}


#' @keywords internal
create_table <- function(parent, dims)
{
    # Arg checks
    tclRequire("Tktable")
    if (missing(dims)) dims <- c(1L, 1L)
    stopifnot(is.integer(dims),
              length(dims) == 2)

    as.tclVar <- function(x) tclVar(x)

    tab <- tkwidget(parent, "table",
                    rows = dims[1] + 1,
                    cols = dims[2],
                    colorigin = 1,
                    titlerows = 1,
                    selecttype = "cell",
                    selectmode = "extended",
                    multiline = FALSE,
                    flashmode = TRUE,
                    invertselected = TRUE,
    #                yscrollcommand = function(...) tkset(scroll.y, ...),
    #                xscrollcommand = function(...) tkset(scroll.x, ...),
                    command = as.tclVar
    )
    #scroll.x <- ttkscrollbar(parent, orient = "horizontal",
    #                         command = function(...) tkxview(tab, ...))
    #scroll.y <- ttkscrollbar(parent, orient = "vertical",
    #                         command = function(...) tkyview(tab, ...))
    #tkpack(scroll.x, fill = "x", expand = FALSE, side = "bottom")
    #tkpack(scroll.y, fill = "y", expand = FALSE, side = "right")
    invisible(tab)
}


