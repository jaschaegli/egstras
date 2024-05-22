#' Shortcut wrapper for pipebind::bind with . as default parameter
#'
#' Allows for unnamed positional argument use similar to magrittr
#' @param x an R object provided by a native pipe ( |> )
#' @param ... arguments
#' @examples
#' c <- c("some", "text")
#' c |> egstras::pb(grep("some", .))
#'
#' @export


pb <- function(x, ...) {
    pipebind::bind(.pipeValue = x, .pipeBind = ., ...)
}
