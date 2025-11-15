#' Save a DE result
#'
#' Save result objects to disk using the \pkg{alabaster.base} framework.
#'
#' @param x Result object to be saved.
#' @param path String containing the path to the directory in which \code{x} is to be saved.
#' @param metadata Named list of metadata to save alongside \code{x}.
#' If \code{NULL}, no metadata is saved.
#' @param meta.name String containing the name of the metadata file within \code{path}.
#' Only used if \code{metadata} is not \code{NULL}.
#' @param requires String containing the name of the \pkg{alabaster.*} package containing the relevant methods to save \code{x}.
#' This may be necessary if the class of \code{x} is not known to \code{\link[alabaster.base]{saveObject}}.
#'
#' @return
#' For \code{saveResult}, \code{x} and \code{metadata} (if supplied) are saved to disk at \code{path}.
#' \code{NULL} is invisibly returned.
#' 
#' @author Aaron Lun
#'
#' @examples
#' df <- S4Vectors::DataFrame(A = 1:5, B=2:6)
#' meta <- list(
#'     title = "This is a very important result",
#'     author = "Myself",
#'     date = "2022-02-22"
#' )
#'
#' tmp <- tempfile()
#' saveResult(df, tmp, metadata = meta)
#' list.files(tmp, recursive=TRUE)
#'
#' readResult(tmp)
#'
#' @seealso
#' \code{\link[alabaster.base]{saveObject}} and \code{\link[alabaster.base]{readObject}}, for the underlying functionality.
#' 
#' @export
saveResult <- function(x, path, metadata, meta.name = "_metadata.json", requires = NULL) {
    if (!is.null(requires)) {
        loadNamespace(requires)
    }

    alabaster.base::saveObject(x, path)

    if (!is.null(metadata)) {
        metadata <- metadata[!duplicated(names(metadata))]
        write(jsonlite::toJSON(metadata, pretty=4, digits=8, auto_unbox=TRUE), file=file.path(path, meta.name))
    }
}

#' @export
#' @rdname saveResult
readResult <- function(path, meta.name = "_metadata.json") {
    output <- list()
    output$x <- alabaster.base::readObject(path)

    if (!is.null(meta.name)) {
        meta.path <- file.path(path, meta.name)
        if (file.exists(meta.path)) {
            output$metadata <- jsonlite::fromJSON(meta.path, simplifyVector=FALSE)
        }
    }

    output
}
