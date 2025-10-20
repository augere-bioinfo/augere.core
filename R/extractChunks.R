#' Extract R chunks
#'
#' Extract R chunks from an Rmarkdown document, along with their names and \pkg{knitr} properties.
#'
#' @param contents A character vector or list (of lists) of character vectors containing an Rmarkdown document,
#' typically obtained via \code{\link{parseRmdTemplate}}.
#' Each string should correspond to a separate line.
#'
#' @return List of character vectors, each of which contains the R code for a single chunk.
#' Each vector is named according to the name of the chunk.
#' \pkg{knitr} properties are stored in the attributes for each vector.
#'
#' @author Aaron Lun
#' @examples
#' contents <- "
#' ```{r alpha}
#' alpha <- 1
#' bravo <- 2
#' ```
#' 
#' Some random text here
#'
#' ```{r, echo=FALSE, eval=FALSE}
#' charlie <- 3
#' delta <- 4
#' ```
#' 
#' - point 1 with some `r inline <- TRUE`.
#' - point 2.
#' 
#'   ```{r i-am-indented, error=TRUE}
#'   echo <- 5
#'   foxtrot <- 6
#'   ```
#'
#' even more text here with `r 'some'` `r 'more'` `r 'inlining'`.
#' "
#'
#' extractChunks(strsplit(contents, "\n"))
#'
#' @export
extractChunks <- function(contents) {
    all.lines <- unlist(contents)

    start.line <- NULL
    end.pattern <- NULL
    all.chunks <- list()
    chunk.params <- list()
    chunk.name <- NULL

    for (i in seq_along(all.lines)) {
        line <- all.lines[i]

        if (grepl("^\\s*```{r ?", line, perl=TRUE)) {
            if (!is.null(start.line)) {
                stop("unclosed chunk starting at line ", start.line)
            }
            start.line <- i

            # Recovering the same indenting for the closing backticks.
            end.pattern <- sub("```{r.*", "```", line, perl=TRUE)
            end.pattern <- paste0("^", end.pattern, "\\s*$")

            chunk.header <- sub(".*```{r(.*)}.*", "\\1", line, perl=TRUE)
            chunk.params <- eval(parse(text=paste0("alist(", chunk.header, ")")))
            chunk.params <- lapply(chunk.params, deparseToString)

            chunk.name <- NULL
            if (length(chunk.params)) {
                raw.chunk.name <- trimws(sub(",.*", "", chunk.header))
                if (raw.chunk.name != "") {
                    chunk.name <- raw.chunk.name
                }
                chunk.params <- chunk.params[-1]
            }

        } else if (is.null(start.line)) {
            inline.starts <- as.integer(gregexpr("`r ", line, fixed=TRUE)[[1]])
            if (length(inline.starts) == 1L && inline.starts == -1L) {
                next
            }

            delim <- gregexpr("`", line, fixed=TRUE)[[1]]
            inline.ends <- setdiff(delim, inline.starts)
            i <- findInterval(inline.starts, inline.ends) + 1L
            all.chunks <- append(all.chunks, as.list(substring(line, inline.starts + 3L, inline.ends[i] - 1L)))

        } else if (grepl(end.pattern, line, perl=TRUE)) {
            current.lines <- all.lines[start.line + seq_len(i - start.line - 1)]
            attributes(current.lines) <- chunk.params
            to.add <- list(current.lines)
            names(to.add) <- chunk.name
            all.chunks <- append(all.chunks, to.add)
            start.line <- NULL
        }
    }

    if (!is.null(start.line)) {
        stop("unclosed chunk starting at line ", start.line)
    }
    all.chunks 
}
