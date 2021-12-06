
create_file_menu <- function(parent, root)
{
    stop("here")
    menu <- tk2menu(parent, tearoff = FALSE)

    onNew <- function() {
        hasName <- nchar(.env$at2("name")) > 0
        if (.env$at2("hasChanges") && 0)

        tkdestroy(root)
        get.par()$clear()
        start_gui()
    }
    onLoad <- function() {
        fn <- tclvalue(tkgetOpenFile(filetypes = "{{Config Files} {.rds}}",
                                     parent = root))
        if (nchar(fn) > 0) {
            param_list <- readRDS(fn)
            update_tcl_parameters_from_list(get.par(), param_list)
            .env$replace_at("name", fn)
            update_changed_parameters()
        }
        invisible()
    }
    onSave <- function() {
        fn <- .env$at2("name")

        if (nchar(fn) > 0) {
            param_list <- as.list(get.par())
            values <- lapply(param_list, tclvalue)
            saveRDS(values, file = fn)
            .env$replace_at("name", fn)
            update_changed_parameters()
        } else {
            onSaveAs()
        }
        invisible()
    }
    onSaveAs <- function() {
        fn <- tclvalue(tkgetSaveFile(filetypes = "{{Config Files} {.rds}}",
                                     parent = root))
        .env$replace_at("name", fn)
        onSave()
        invisible()
    }
    tkadd(menu, "command", label = "New...", command = onNew)
    tkadd(menu, "command", label = "Load config...", command = onLoad)
    tkadd(menu, "command", label = "Save config...", command = onSave)
    tkadd(menu, "command", label = "Save config as...", command = onSaveAs)
    tkadd(menu, "separator")
    tkadd(menu, "command", label = "Quit", command = onQuit)
    menu
}


create_design_menu <- function(parent)
{
    menu <- tk2menu(parent, tearoff = FALSE)
    on1 <- function() message("One Sample...")
    on2 <- function() message("Two Samples...")
    onIRL <- function() message("Release limits...")
    stdMenu <- tk2menu(menu, tearoff = FALSE)

    tkadd(stdMenu, "command", label = "One Sample...", command = on1)
    tkadd(stdMenu, "command", label = "Two Samples...", command = on2)
    tkadd(menu, "cascade", label = "Standard", menu = stdMenu)
    tkadd(menu, "command", label = "Release limits...", command = onIRL)
    menu
}


create_help_menu <- function(parent)
{
    menu <- tk2menu(parent, tearoff = FALSE)

    onAbout <- function() message("about")
    tkadd(menu, "command", label = "About", command = onAbout)
    menu
}

