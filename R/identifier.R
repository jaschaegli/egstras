#' Create response or respondent IDs
#'
#' @param x an R object provided by a native pipe ( |> )
#' @param ... arguments
#' @examples
#' df <- data.frame(runif(50), runif(50))
#' make_ids()
#'
#' @export

make_ids <- function(df, prefix = NULL, num_digits = NULL, randomise = TRUE) {
    checkmate::qassert(df, "D")
    checkmate::qassert(num_digits, c("x", "0"))
    checkmate::qassert(prefix, c("s", "0"))
    checkmate::qassert(randomise, "b")

    nums <- seq_len(nrow(df))

    if (randomise == TRUE) {
        nums <- sample(x = nums, replace = FALSE)
    }


    if (is.null(num_digits)) {
        num_digits <- nchar(max(nums))
    } else {
        if (num_digits < nchar(max(nums))) {
            warning("num_digits is lower than the width of the largest numeric identiifer. Ignoring argument")
            num_digits <- nchar(max(nums))
        }
    }

    numids <- formatC(
        x = nums, width = num_digits, format = "d", flag = "0"
    )

    ids <- paste0(prefix, numids)

    return(ids)
}
