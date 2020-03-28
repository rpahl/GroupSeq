#' @title Verify function arguments
#' @description Enables shortened arg checking
#' @param x object to verify
#' @param type `character` expected class or type
#' @param len `integer` expected length of argument
#' @param throw `logical` if TRUE, an error is signaled if verification fails,
#' otherwise just `FALSE` is returned.
#' @return returns invisibly `TRUE` if verification succeeds
verify_arg <- function(x, type = "character", len = 1L, throw = TRUE)
{
    if (!is.character(type)) stop("'type' must be character")
    if (!is.integer(len) || length(len) != 1) {
        stop("'len' must be an integer of length 1")
    }
    if (!is.logical(throw) || length(throw) != 1) {
        stop("'throw' must be a logical of length 1")
    }

    if (length(x) != len || !inherits(x, type)) {
        if (throw) {
            stop("'", toString(x), "' is not a ", type, " of length ", len)
        } else {
            return(invisible(FALSE))
        }
    }
    invisible(TRUE)
}

