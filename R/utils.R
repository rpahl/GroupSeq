#' @title Verify function arguments
#' @description This function enables shortened arg checking.
#' @param x object to verify
#' @param type `character` expected class or type.
#' @param lenOp `character` operator used for length comparison.
#' @param len `integer` expected length of argument.
#' @return returns invisibly `TRUE` if verification succeeds
#' @keywords internal
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



#' @keywords internal
update_tcl_parameters_from_list <- function(dict, param_list)
{
    stopifnot(is.list(param_list))
    for (key in names(param_list)) {
        elem <- param_list[[key]]
        if (dict$has(key)) {
            tcl.var <- dict$get(key)
            tclvalue(tcl.var) <- elem
        } else {
            dict$add(key, tclVar(elem))
        }
    }
}


