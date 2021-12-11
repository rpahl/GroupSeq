
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
    tcl(tab, "clear", "cache")
    invisible(tab)
}


get_cell_value2 <- function(tab, i, j)
{
    tclvalue(tcl(tab, "get", paste0(i, ", ", j)))
}


set_cell_value2 <- function(tab, r, c, value)
{
    as.tclVar <- function(x) tclVar(x)
    # @x,y, or <row>,<col>
    #tcl(tab, "set", paste0("@", i, ", ", j), as.tclVar(value))
    tkconfigure(tab,
                command = as.tclVar,
                rows = r,
                cols = c)
    tcl(tab, "clear", "cache")
    invisible(tab)
}


create_table2 <- function(parent, dims, name = "myTable")
{
    # Arg checks
    tclRequire("Tktable")
    if (missing(dims)) dims <- c(1L, 1L)
    stopifnot(is.integer(dims),
              length(dims) == 2)

    as.tclVar <- function(x) tclVar(x)

    tab <- tkwidget(parent, type = "table",
                    variable = name,
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


create_table <- function(parent, dims, name = "myTable", ...)
{
    tk2table(parent,
                    variable = name,
                    rows = dims[1] + 1,
                    cols = dims[2],
                    colorigin = 1,
                    titlerows = 1,
                    selecttype = "cell",
                    selectmode = "extended",
                    multiline = FALSE,
                    flashmode = TRUE,
                    background = "white",
                    invertselected = TRUE,
                    ...
    )
}


set_cell_value <- function(tab, i, j, value)
{
    tcl(tab, "set", paste0(i, ",", j), as.character(value))
}


get_cell_value <- function(tab, i, j)
{
    tclvalue(tcl(tab, "get", paste0(i, ", ", j)))
}


configure_cell <- function(tab, i, j, ...) {
    # Note: configuration of state of entire table shadows state configuration
    # of individual cells.
    str.tag <- paste0(i, j)
    str.pos <- paste0(i, ",", j)
    tcl(tab, "tag", "celltag", str.tag, str.pos)
    tcl(tab, "tag", "configure", str.tag, ...)
    invisible(tab)
}

