# #' @keywords internal

# id_helper_numeric <- function() {
#     checkmate::qassert(df, "d") # Data frame
#     checkmate::qassert(num_digits, c("x", "0")) # Integerish, NA or NULL
#     checkmate::qassert(randomise, "B") # Boolean

#     nums <- seq_len(nrow(df))

#     if (randomise == TRUE) {
#         nums <- sample(x = nums, replace = FALSE)
#     }

#     if (is.null(num_digits)) {
#         num_digits <- nchar(max(nums))
#     } else {
#         if (num_digits < nchar(max(nums))) {
#             warning("num_digits is lower than the width of the largest numeric identiifer. Ignoring argument")
#             num_digits <- nchar(max(nums))
#         }
#     }

#     numids <- formatC(
#         x = nums, width = num_digits, format = "d", flag = "0"
#     )

#     ids <- paste0(prefix, numids)

#     return(ids)
# }

#' Create response or respondent IDs
#'
#' @param df A data frame
#' @param prefix String to prefix the IDs with
#' @param num_digits Number of numeric digits to construct
#' @param randomise Whether to randomise the order of the input object when constructing IDs
#' @examples
#' df <- data.frame(stats::runif(50), stats::runif(50))
#' make_ids(df)
#'
#' @export

make_ids <- function(df, prefix = NULL, num_digits = NULL, randomise = TRUE) {
  checkmate::qassert(df, "d") # Data frame
  checkmate::qassert(num_digits, c("x", "0")) # Integerish, NA or NULL
  checkmate::qassert(prefix, c("s1", "0")) # String of length 1 or NULL
  checkmate::qassert(randomise, "B") # Boolean

  nums <- seq_len(nrow(df))

  if (randomise == TRUE) {
    nums <- sample(x = nums, replace = FALSE)
  }

  if (is.null(num_digits)) {
    num_digits <- nchar(max(nums))
  } else {
    if (num_digits < nchar(max(nums))) {
      warning(
        "num_digits is lower than the width of the largest numeric identiifer. Ignoring argument"
      )
      num_digits <- nchar(max(nums))
    }
  }

  numids <- formatC(
    x = nums,
    width = num_digits,
    format = "d",
    flag = "0"
  )

  ids <- paste0(prefix, numids)

  return(ids)
}
