#' Write Rmarkdown file
#'
#' Write Rmarkdown-formatted strings to file.
#'
#' @param contents A character vector of a (nested) list of such vectors.
#' @param file String containing a file path or a connection object to an opened file.
#' @param append Logical scalar indicating whether to append \code{contents} to \code{file}.
#' @param clean.empty Logical scalar indicating whether extraneous empty lines should be removed.
#'
#' @return \code{contents} is written to \code{file} and \code{NULL} is invisibly returned.
#'
#' @details
#' Each element of each character vector in \code{contents} is placed on a new line in the output.
#' Empty strings are respected and manifest as empty lines.
#' If \code{clean.empty=TRUE}, repeated empty lines are removed.
#'
#' @author Aaron Lun
#'
#' @examples
#' contents <- list(
#'     c("# TITLE", ""),
#'     list(
#'         c("```{r}", "a <- 1", "```"),
#'         c("", "super foo bar")
#'     ),
#'     c("", "## section")
#' )
#' tmp <- tempfile(fileext=".Rmd")
#' writeRmd(contents, tmp)
#' cat(readLines(tmp), sep="\n") # having a look
#'
#' @export
#' @importFrom utils head tail
writeRmd <- function(contents, file, append = FALSE, clean.empty = TRUE) {
    contents <- unlist(contents, use.names=FALSE)

    if (clean.empty) {
        runs <- rle(contents)
        runs$lengths[runs$values==""] <- 1L
        contents <- inverse.rle(runs)
    }

    write(contents, file=file, append=append, ncolumns=1)
    invisible(NULL)
}
