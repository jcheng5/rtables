#' Knit print the table
#'
#' This facilitates printing of the table within a knitr code chunk.
#'
#' @param x An object of class `VTableTree`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.VTableTree <- function(x, ...) {

  latex_packages <- 
    lapply(
      c("amsmath", "booktabs", "caption", "longtable"),
      rmarkdown::latex_dependency
    )
  
    # Default to HTML output
    x <- as_latex(x) %>%
      knitr::asis_output(meta = latex_packages)

    # Use `knit_print()` to print in a code chunk
    knitr::knit_print(x, ...)
}
