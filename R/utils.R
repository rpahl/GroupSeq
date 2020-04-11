#' @title Verify function arguments
#' @description This function enables shortened arg checking.
#' @param x object to verify
#' @param type `character` expected class or type.
#' @param lenOp `character` operator used for length comparison.
#' @param len `integer` expected length of argument.
#' @return returns invisibly `TRUE` if verification succeeds
verify_arg <- function(x,
                       type = "character",
                       lenOp = c("==", ">", "<", ">=", "<=", "!="),
                       len = 1L
                       )
{
    # Arg checks
    stopifnot(is.character(type),
              length(len) == 1,
              is.integer(len),
              is.character(lenOp))
    op <- match.arg(lenOp)
    xstr <- deparse(substitute(x))

    # Verify
    if (!inherits(x, type)) stop(xstr, " is not of type ", type)
    if (!do.call(op, args = list(length(x), len))) {
        stop("length(", xstr, ") ", op, " ", len, " is not true")
    }

    invisible(TRUE)
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

