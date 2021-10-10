register_s3_method <- function(pkg, generic, class, fun = NULL) {
   
    stopifnot(is.character(pkg), length(pkg) == 1)
    stopifnot(is.character(generic), length(generic) == 1)
    stopifnot(is.character(class), length(class) == 1)
    
    if (is.null(fun)) {
        fun <- get(paste0(generic, ".", class), envir = parent.frame())
    } else {
        stopifnot(is.function(fun))
    }
    
    if (pkg %in% loadedNamespaces()) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
    
    # Always register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
    )
}

## Empty default objects to avoid repeated calls
.onLoad <- function(libname, pkgname) {
    
    ns <- asNamespace(pkgname)
    delayedAssign("EmptyColInfo", InstantiatedColumnInfo(),
                  assign.env = ns)
    delayedAssign("EmptyElTable", ElementaryTable(),
                  assign.env = ns)
    delayedAssign("EmptyRootSplit", RootSplit(),
                  assign.env = ns) ## is this used?
    delayedAssign("EmptyAllSplit", AllSplit(),
                  assign.env = ns) ## is this used?
    namespaceExport(ns, c("EmptyColInfo", "EmptyElTable"))
    
    # TODO: need object class name here
    register_s3_method("knitr", "knit_print", "VTableTree")
    
    invisible()
}

#' Empty table, column, split objects
#' @name EmptyColInfo
#' @aliases EmptyElTable EmptyRootSplit EmptyAllSplit
#' @description Empty objects of various types to compare against efficiently.
NULL
