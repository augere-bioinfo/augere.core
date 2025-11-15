#' Substitute placeholders
#'
#' Find all placeholders in an Rmarkdown template and replace them with actual text. 
#'
#' @param contents String, character vector or list containing an Rmarkdown report template.
#' This is typically the output of \code{\link{parseRmdTemplate}}.
#' @param replacements Named character vector of replacement text for placeholders.
#' The name is the placeholder and the value is the replacement text.
#' This may also be a named list of strings.
#' @param error Boolean indicating whether to throw an error if a placeholder does not have a replacement in \code{replacements}.
#' If \code{FALSE}, no error is thrown.
#'
#' @return An object of the same type as \code{contents}, but with all of the placeholders replaced.
#'
#' @details
#' A placeholder takes the form \code{<\%= LABEL \%>}, where \code{LABEL} can be any name without \code{\%} or newlines.
#' Pipeline functions are expected to replace these placeholders with actual text, usually based on user-supplied parameters.
#' 
#' @author Aaron Lun
#' @examples
#' replacePlaceholders(
#'     "Hi my name is <%= NAME %> and I enjoy <%= FOOD %>.",
#'     replacements=c(NAME="Aaron", FOOD="pork")
#' )
#' 
#' @export
replacePlaceholders <- function(contents, replacements, error = TRUE) {
    if (is.list(replacements)) {
        replacements <- unlist(replacements)
        if (is.null(replacements)) {
            replacements <- character(0)
        }
    }
    .replace_contents(contents, replacements, error=error)
}

.replace_contents <- function(contents, replacements, error) {
    if (!is.character(contents)) {
        for (i in seq_along(contents)) {
            contents[[i]] <- .replace_contents(contents[[i]], replacements, error=error)
        }
        return(contents)
    }

    matches <- gregexpr("<%=[^%]+%>", contents)

    for (i in seq_along(matches)) {
        starts <- matches[[i]]
        if (length(starts) == 1L && starts[1] == -1L) {
            next 
        }
        ends <- starts + attr(starts, "match.length") - 1L

        curline <- contents[i]
        instances.0 <- substring(curline, starts + 3L, ends - 2L)
        instances <- trimws(instances.0) 

        substitutes <- replacements[instances]
        if (anyNA(substitutes)) {
            no.sub <- is.na(substitutes)
            if (error) {
                lost <- unique(instances[no.sub])
                stop("no available replacements for ", paste(sprintf("'%s'", lost), collapse=", "))
            } else {
                substitutes[no.sub] <- sprintf("<%%=%s%%>", instances.0[no.sub])
            }
        }

        rest.starts <- c(1L, ends + 1L)
        rest.ends <- c(starts - 1L, nchar(curline))
        rest <- substring(curline, rest.starts, rest.ends)

        combined <- c(rest[1], rbind(substitutes, rest[-1]))
        contents[i] <- paste0(combined, collapse="")
    }

    contents
}
