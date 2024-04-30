#' Remove whitespace from character vectors
#'
#' Removes any trailing or preceding whitepsace using [trimws()], replaces all newlines and double spaces with single spaces using [gsub()]
#' @param x a vector to clean
#' @export

remove_whitespace <- function(x) {
    # Set formals
    formals(gsub)$perl <- TRUE
    x <- x |>
        trimws() |>
        gsub("\\n+", " ", x = _) |>
        gsub("\\s+", " ", x = _)

    # Reset formals
    formals(gsub)$perl <- FALSE
}
