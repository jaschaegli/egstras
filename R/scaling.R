#' Normalise vectors to specified mean (defaults to 1)
#'
#' Shifts center while leaving scale intact; unlike scale(),
#' this function returns a numeric vector and not an array, and leaves
#' attributes intact
#' @param vector a vector to transform
#' @param mean the desired mean of the transformed vector
#' @examples
#' vec <- c(1, 50, 30)
#' summary(vec)
#'
#' rescaled <- center_vector(vec, mean = 10)
#' summary(rescaled)
#' @export

center_vector <- function(vector, mean = 1) {
    checkmate::assert_numeric(vector)
    attrs <- attributes(vector)
    out <- scale(vector, center = TRUE, scale = FALSE) + mean |> as.numeric()
    attributes(out) <- attrs
    return(out)
}
