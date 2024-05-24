#' "Flip" integer vectors
#'
#' A utility intended for reversal of numeric response scales
#' @param vec A numeric input vector that is either an integer, or coercible to one.
#'
#' @export


flip_integer <- function(vec) {
    checkmate::qassert(vec, "x")

    class_old <- class(vec)

    clab <- egstras::getcl(vec)
    vlab <- egstras::getvl(vec)


    vn <- deparse(substitute(vec))

    conditions_raw <- glue::glue("{vn} == {min(vec):max(vec)}; {max(vec):min(vec)}")

    conditions_parsed <- lapply(conditions_raw, rlang::parse_exprs) |> unlist()

    call_parsed <- rlang::expr(
        data.table::fcase(
            !!!conditions_parsed,
            default = NA
        )
    )

    flipped <- eval(call_parsed)

    if (!is.null(vlab) || !is.null(clab)) {
        flipped <- egstras::setvl(
            flipped,
            labs =
                setNames(
                    unname(vlab),
                    nm = rev(names(vlab))
                ),
            fill_partial = FALSE
        )
    }

    if (!is.null(clab)) {
        flipped <- egstras::setcl(flipped, clab)
    }


    class(flipped) <- class_old
    return(flipped)
}
