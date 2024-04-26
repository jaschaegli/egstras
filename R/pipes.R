#' Shortcut wrapper for pipebind::bind with . as default parameter
#'
#' Allows for unnamed positional argument use similar to magrittr
#' @param x an R object provided by a native pipe ( |> )
#' @param ... arguments
#' @examples
#' c <- c("some", "text")
#' c |> pb(grep("some", .))
#'
#' @importFrom pipebind bind
#' @export


pb <- function(x, ...) {
    pipebind::bind(.pipeValue = x, .pipeBind = ., ...)
}
