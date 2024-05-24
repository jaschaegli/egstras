#' "Flip" integer vectors
#'
#' A utility intended for reversal of numeric response scales
#' @param vec A numeric input vector that is either an integer, or coercible to one.
#'
#' @export


flip_integer <- function(vec) {
    checkmate::qassert(vec, "x")

    class_old <- class(vec)

    minv <- as.integer(min(vec, na.rm = TRUE))
    maxv <- as.integer(max(vec, na.rm = TRUE))


    clab <- getcl(vec)
    vlab <- getvl(vec)

    x <- vec

    conditions_raw <- glue::glue("x == {minv:maxv}; {maxv:minv}")

    conditions_parsed <- lapply(conditions_raw, rlang::parse_exprs) |> unlist()

    call_parsed <- rlang::expr(
        data.table::fcase(
            !!!conditions_parsed,
            default = NA
        )
    )

    flipped <- eval(call_parsed)

    if (!is.null(vlab) || !is.null(clab)) {
        attr(flipped, "labels") <-
            setNames(
                unname(vlab),
                nm = rev(names(vlab))
            )
    }

    if (!is.null(clab)) {
        flipped <- setcl(flipped, clab)
    }


    class(flipped) <- class_old
    return(flipped)
}
