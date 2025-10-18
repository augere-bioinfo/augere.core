#' Deparse object to a string
#'
#' Deparse an object to a single string by concatenation, possibly with indentation for multi-line output.
#'
#' @param x R object to be deparsed.
#' @param indent Integer scalar specifying the indenting for multi-line deparsed output.
#' If \code{NULL}, the deparsed output will be put on a single line with no indenting.
#'
#' @return String containing the deparsed value of \code{x}.
#'
#' @details
#' This function is based on \code{\link{deparse}} but with special accommodations when the deparsed output spans multiple lines.
#' If \code{indent=NULL}, the multiple output lines are directly \code{paste}d together with no separator. 
#' If \code{indent} is an integer, a newline is added between lines, indented by the specified number of spaces. 
#'
#' @author Aaron Lun
#'
#' @examples
#' x <- sample(100)
#' deparse(x)
#' deparseToString(x)
#' deparseToString(x, indent=4)
#'
#' @export
deparseToString <- function(x, indent = NULL) {
    if (is.null(indent)) {
        col <- ""
    } else {
        col <- paste0("\n", strrep(" ", indent))
    }
    paste(deparse(x), collapse=col)
}

