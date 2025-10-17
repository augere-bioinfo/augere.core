#' Parse Rmarkdown template 
#'
#' Parse an Rmarkdown report template to extract blocks of text or code.
#'
#' @param contents String or character vector containing an Rmarkdown report template.
#' 
#' @return List of character vectors or nested lists, see Details.
#'
#' @author Aaron Lun
#'
#' @details
#' An Rmarkdown report template may contain any number of paired \code{:BEGIN} and \code{:END} tags.
#' These tags enclose blocks of text that may be dynamically removed or duplicated by the pipeline functions.
#' 
#' Tag pairs should follow these rules:
#' \itemize{
#' \item Each tag should start on its own line.
#' \item The \code{:BEGIN} tag should be followed by a name, e.g., \code{:BEGIN my-name-here}.
#' \item The corresponding \code{:END} tag does not need to be followed by a name, but if it does, the name should be the same as that of the matching \code{:BEGIN} tag.
#' \item Tag pairs may be nested within another tag pair.
#' \item Names should be unique for any given level of nesting.
#' For nested tag pairs, there should be no other tag pair with the same name within the parent tag pair.
#' For unnested tag pairs, there should be no other unnested tag pair with the same name. 
#' }
#'
#' In the output of this function, each entry of the top-level list may be either:
#' \itemize{
#' \item A list, corresponding to a tag pair.
#' This list contains all text in the Rmarkdown template enclosed by the tag pair,
#' which may be character vectors or further lists corresponding to nested tag pairs.
#' If the tag pair is named, the same name will be used for this list entry. 
#' \item A character vector, to store newline-separated text within or between tag pairs.
#' } 
#' Calling \code{unlist} on the output of this function will return a newline-separated version of \code{contents} without the tags.
#'
#' @examples
#' template <- "
#' Ochite iku sunadokei bakari miteru yo
#' Sakasama ni sureba hora mata hajimaru yo
#' Kizanda dake susumu jikan ni
#' Itsuka boku mo haireru kana
#'
#' ```{r}
#' nagisa <- 'furukawa' 
#' ```
#'
#' :BEGIN clannad
#'
#' Kimi dake ga sugisatta saka no tochuu wa
#' Atataka na hidamari ga ikutsu mo dekiteta
#' Boku hitori ga koko de yasashii
#' Atatakasa wo omoikaeshiteru
#'
#' :BEGIN after-story
#' 
#' ```{r}
#' tomoya <- 'Okazaki'
#' ```
#' 
#' :END
#'
#' Kimi dake wo kimi dake wo
#' Suki de ita yo
#' Kaze de me ga nijinde
#' Tooku naru yo
#'
#' :BEGIN toki-wa-kizamu-uta
#'
#' Itsumademo oboeteru
#' Kono machi ga kawatte mo
#' Dore dake no kanashimi to deau koto ni natte mo
#' Misete yaru hontou wa tsuyokatta doki no koto
#' Saa iku yo arukidasu saka no michi wo
#'
#' ```{r}
#' ushio <- TRUE
#' ```
#'
#' :END
#'
#' :END"
#'
#' parseRmdTemplate(template)
#' @export
parseRmdTemplate <- function(contents) {
    lines <- strsplit(contents, "\n")
    for (i in seq_along(lines)) {
        if (length(lines[[i]]) == 0L) {
            lines[[i]] <- ""
        }
    }
    .recursive_parse_rmd_template(unlist(lines), 1L, is.top=TRUE)[[1]]
}

.recursive_parse_rmd_template <- function(lines, position, is.top=FALSE) {
    current <- list()
    original <- previous <- position 
    endname <- NULL
    num.lines <- length(lines)

    while (position <= num.lines) {
        curline <- lines[position]

        if (grepl("^\\s*:BEGIN", curline)) {
            if (position > previous) {
                current <- c(current, list(lines[previous:(position - 1L)]))
            }

            curname <- ""
            if (grepl("^\\s*:BEGIN\\s", curline)) {
                curname <- sub("^\\s*:BEGIN\\s+", "", curline)
                curname <- sub("\\s.*", "", curname)
            }
            if (curname=="") {
                stop("unnamed BEGIN at line ", position)
            } else if (curname %in% names(current)) {
                stop("duplicate name for BEGIN at line ", position)
            }

            out <- .recursive_parse_rmd_template(lines, position + 1L)
            if (!is.null(out[[3]]) && curname != out[[3]]) {
                stop("block at line ", position, " has different BEGIN/END names")
            }

            x <- out[1]
            names(x) <- curname
            current <- c(current, x)

            position <- out[[2]] + 1L
            previous <- position

        } else if (grepl("^\\s*:END", curline)) {
            if (position > previous) {
                current <- c(current, list(lines[previous:(position - 1L)]))
            }

            endname <- ""
            if (grepl("^\\s*:END\\s", curline)) {
                endname <- sub(".*:END\\s+", "", curline)
                endname <- sub("\\s.*", "", endname)
            }
            if (endname == "") {
                endname <- NULL
            }
            break

        } else {
            position <- position + 1L
        }
    }

    if (is.top) {
        if (position != length(lines) + 1L) {
            stop("unpaired END at line ", position)
        }
        if (position > previous) {
            # Remaining text at the end of the template. 
            current <- c(current, list(lines[previous:(position - 1)]))
        }
    } else {
        if (position == length(lines) + 1L) {
            stop("unpaired BEGIN at line ", original - 1L)
        }
    }

    list(current, position, endname)
}
