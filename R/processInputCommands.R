#' Process commands for a pipeline input
#'
#' Convert an input object into commands that can be stored in an Rmarkdown report for a reproducible analysis.
#'
#' @param x An arbitrary R object.
#' Alternatively, a list with the \code{augere.input} class, created by \code{createInput}.
#' @param name String containing the name of the object in the Rmarkdown report.
#' @param object An arbitrary R object.
#' Alternatively \code{NULL}, in which case the object will be generated from \code{commands}.
#' @param commands Character vector of R commands to be used to generate \code{object}.
#' If missing, this is obtained by deparsing the expression supplied as \code{object}.
#' Alternatively \code{NULL}, if no commands are available.
#'
#' @return
#' \code{processInputCommands} will return a string containing R commands that can be inserted into an Rmarkdown code chunk.
#' If \code{x} is an \code{augere.input} with non-\code{NULL} commands, those commands will be used directly.
#' Otherwise, a \code{stop} command will be generated that instructs the user to manually insert commands to generate \code{x}.
#'
#' \code{wrapInput} will return an \code{augere.input} list containing the \code{object} and \code{commands} fields.
#'
#' \code{resetInputCache} will set the input cache to an empty list.
#' It returns a function that restores the input cache to the state prior to the function's invocation.
#'
#' @details
#' Pipeline developers should call \code{processInputCommands} to define the generation of input objects in the Rmarkdown report.
#' Each call to \code{processInputCommands} will store the input R object in an internal cache.
#' Upon compiling the report with \code{\link{compileReport}}, the cached object will be used instead of rerunning the code returned by \code{processInputCommands}.
#' This saves time and avoids errors for inputs that have no associated commands.
#' (The exception is if \code{object=NULL}, in which case the commands must be run to generate the object.)
#'
#' At the start of a pipeline function, it is good practice to call \code{resetInputCache}.
#' This clears any existing cached inputs from calls to other pipeline functions,
#' e.g., in the pathological case where one pipeline function is called within another function.
#' The function returned by \code{resetInputCache} should then be called once the pipeline function completes, e.g., in an \code{\link{on.exit}} block.
#'
#' @seealso
#' \code{\link{compileReport}}, which uses the input cache. 
#'
#' \code{\link{replacePlaceholders}}, to insert the input commands into the Rmarkdown template.
#'
#' @author Aaron Lun
#'
#' @examples
#' fun <- resetInputCache()
#'
#' df <- data.frame(foo=1:5, bar=LETTERS[2:6])
#' cat(processInputCommands(df, "my_df"))
#'
#' wrapped <- wrapInput(df, c("y <- data.frame(foo = 1:5)", "y$bar <- LETTERS[2:6]", "y"))
#' cat(processInputCommands(wrapped, "my_df"))
#'
#' fun()
#' @export
processInputCommands <- function(x, name) {
    if (is.list(x) && inherits(x, "augere.input")) {
        object <- x$object
        commands <- x$commands
    } else {
        object <- x
        commands <- NULL
    }

    if (is.null(commands)) {
        commands <- paste0("stop(\"insert commands to generate '", name, "' here\")")
    }
    commands <- sprintf("    %s", commands)

    if (!is.null(object)) {
        n <- .add_to_input_cache(object)
        preamble <-  paste(name, .input_tag, paste0("(", n, ")"))
    } else {
        preamble <- paste(name, "<- local({")
    }

    paste(c(preamble, commands, "})"), collapse="\n")
}

.input_tag <- "<- local({ # augere.core input"

input.cache <- new.env()
input.cache$objects <- list()

.get_input_cache <- function(x) {
    prev <- input.cache$objects
    if (missing(x)) {
        prev
    } else {
        input.cache$objects <- x
        invisible(prev)
    }
}

.add_to_input_cache <- function(object) {
    N <- length(input.cache$objects)
    input.cache$objects[[N + 1L]] <- object
    N + 1L
}

.pop_input_cache <- function(i) {
    obj <- input.cache$objects[[i]]
    if (is.null(obj)) {
        stop("entry", i, "was already extracted from the input cache")
    }
    input.cache$objects[i] <- list(NULL)
    obj
}

#' @export
#' @rdname processInputCommands
resetInputCache <- function() {
    old <- .get_input_cache(list())
    function() {
        .get_input_cache(old)
    }
}

#' @export
#' @aliases augere.input
#' @rdname processInputCommands
wrapInput <- function(object, commands) {
    if (missing(commands)) {
        commands <- deparse(substitute(object))
    }
    output <- list(object=object, commands=commands)
    class(output) <- "augere.input"
    output
}
