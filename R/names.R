#' Check whether all, some, or no elements of an object have names
#' @param x an object to check
#' @examples
#' namecheck(c("a" = 0, 1))
#'
#' namecheck(list("a" = 0, "b" = 1))
#'
#' namecheck(c(0, 1))
#' @export

namecheck <- function(x) {
    noname <- allNames(x) == ""

    res <- data.table::fcase(
        all(noname == TRUE), "none_named",
        any(noname == FALSE) && any(noname == TRUE), "some_named",
        all(noname == FALSE), "all_named",
        default = "ERROR"
    )
    return(res)
}
