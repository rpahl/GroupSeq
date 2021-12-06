
update_tcl_parameters_from_list <- function(dict, param_list)
{
    stopifnot(is.list(param_list))

    for (key in names(param_list)) {

        elem <- param_list[[key]]

        if (dict$has_name(key)) {
            tcl.var <- dict$at2(key)
            tclvalue(tcl.var) <- elem
        } else {
            dict$add(key, tclVar(elem))
        }
    }

    invisible(dict)
}


