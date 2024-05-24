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
}



#' Add backticks to strings
#'
#' Wrapper for [encodeString()] and (optionally) [noquote()]
#' @param x a vector to add backticks to
#' @param remove_outer a boolean indicating whether outer quotes should be removed using [noquote()]) after backticks have been added; default is TRUE
#' @examples
#' x <- c("these are", "some strings")
#'
#' add_backticks(x)
#'
#' add_backticks(x, remove_outer = FALSE)
#' @export
#'
add_backticks <- function(x, remove_outer = TRUE) {
    checkmate::qassert(x, "s+") # string of length 1+, NAs ok

    x <- encodeString(x, quote = "`")

    if (remove_outer == TRUE) {
        x <- noquote(x)
    }
    return(x)
}

#' @rdname add_backticks
#' @export
backticks_add <- add_backticks



#' Remove backticks from strings
#'
#' Simple wrapper for [gsub()]
#' @param x a vector to remove backticks from
#' @param add_quotes a boolean indicating whether backticks should be replaced with double quotes (default) or simply removed
#' @examples
#' x <- c("these are", "some strings")
#'
#' x <- add_backticks(x)
#'
#' remove_backticks(x, add_quotes = FALSE)
#' @export
#'
#'
remove_backticks <- function(x, add_quotes = TRUE) {
    checkmate::qassert(x, "s+") # string of length 1+, NAs ok
    checkmate::qassert(add_quotes, "B")

    if (add_quotes == FALSE) {
        x <- gsub(x = x, pattern = "`", replacement = "")
    } else {
        x <- gsub(x = x, pattern = "`", replacement = '"')
    }
    return(x)
}

#' @rdname remove_backticks
#' @export
backticks_remove <- remove_backticks
