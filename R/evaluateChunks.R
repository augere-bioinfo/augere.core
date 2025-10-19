#' Evaluate R chunks
#'
#' Evaluate R chunks, typically derived from an Rmarkdown report.
#'
#' @param chunks List of character vectors, typically from \code{\link{extractChunks}}.
#' Each vector corresponds to a chunk of R code.
#' @param env Environment in which to evaluate \code{chunks}.
#'
#' @details
#' Each character vector may have some \pkg{knitr}-inspired attributes:
#' \itemize{
#' \item \code{eval} is a string that, upon evaluation within \code{env}, specifies if the current chunk should be evaluated.
#' \item \code{ref.label} is a string that, upon evaluation within \code{env}, contains the name of another character vector in \code{chunks}.
#' The chunk in the named character vector will be evaluated instead of the current chunk.
#' \item \code{error} is a string that, upon evaluation within \code{env}, specifies if errors should be ignored for this chunk.
#' }
#'
#' @return All \code{chunks} are evaluated within \code{env}.
#' @author Aaron Lun
#'
#' @examples
#' chunks <- list("a <- 1", "a <- a + 2", "", "a <- NULL")
#' names(chunks) <- c("", "foo1", "", "foo3")
#' attr(chunks[[3]], "ref.label") <- "paste0('foo', 1)"
#' attr(chunks[[4]], "eval") <- "a < 0"
#'
#' env <- new.env()
#' evaluateChunks(chunks, env)
#' env$a
#'
#' @export
evaluateChunks <- function(chunks, env) {
    cnames <- names(chunks)
    cnames <- cnames[cnames != ""]
    if (anyDuplicated(cnames)) {
        stop("names of R chunks should be unique within an Rmarkdown template")
    }

    for (current in chunks) {
        eval.condition <- attr(current, "eval")
        if (!is.null(eval.condition)) {
            if (!eval(parse(text=eval.condition), envir=env)) {
                next
            }
        }

        # Possibly re-use an existing chunk.
        chunk.label <- attr(current, "ref.label")
        if (!is.null(chunk.label)) {
            current <- chunks[[eval(parse(text=chunk.label), envir=env)]]
        }

        error.condition <- attr(current, "error")
        if (!is.null(error.condition) && eval(parse(text=error.condition), envir=env)) {
            try(eval(parse(text=current), envir=env))
        } else {
            eval(parse(text=current), envir=env)
        }
    }

    invisible(NULL)
}
