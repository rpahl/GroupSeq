#' @title Create graphical user interface.
#' @description This function builds the main GUI that appears when [GroupSeq]
#' is started.
#' @return Invoked for its side effects.
#' @import gWidgets gWidgetstcltk tcltk
#' @keywords internal
gui <- function()
{
    version <- utils::packageVersion(utils::packageName())
    #options("guiToolkit" = "tcltk")
    win <- gwindow(title = paste0("GroupSeq (version ", version, ")"),
                  visible = TRUE)

    grp_main <- ggroup(horizontal = TRUE, container = win)

    gf.input <- gframe("Input", container = grp_main, expand = T)
	g.input <- ggroup(container = gf.input)

    menu = gcombobox(c("Probabilities given drift",
                       "Drift given power",
                       "Confidence interval"),
                     container = g.input);

    gf.out <- gframe("Output", container = grp_main, expand = T)
    g.out <- ggroup(container = gf.out)
    addSpace(g.out, val = 30, horizontal = FALSE)
    invisible()
}

