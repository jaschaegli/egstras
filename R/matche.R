#' Check values in one vector against values in another
#'
#' @param x a vector to compare
#' @param y a vector to compare x with
#' @param return the desired output format; can be "logical", "factor", "labelled", "numeric"
#' @param nahandle what to do with missing values in x; can be "keep" (the return vector will be NA for this element), or "false" (treated like a non-match)
#' @param levels labels for the output vector; defaults to "No" and "Yes"; will be ignored unless return is "factor", or "labelled"
#' @param label an optional "label" attribute for the output vector
#' @export


make_matchcheck <- function(
    x, y,
    output = c("logical", "factor", "labelled", "numeric"),
    nahandle = c("keep", "false"),
    levels = c("No", "Yes"),
    label = NULL) {
    output <- match.arg(output, several.ok = FALSE)
    nahandle <- match.arg(nahandle)
    checkmate::qassert(levels, "s2")
    checkmate::qassert(label, c("s1", 0))


    checkvec <- data.table::fcase(
        x == y, TRUE,
        x != y, FALSE,
        is.na(x), NA
    )

    if (nahandle == "false") {
        checkvec[is.na(checkvec)] <- FALSE
    }

    if (output == "numeric") {
        checkvec <- as.numeric(checkvec)
    }

    if (output == "factor") {
        checkvec <- as.factor(checkvec)
        levels(checkvec) <- levels
    }

    if (output == "labelled") {
        checkvec <- as.numeric(checkvec)
        lab <- setNames(sort(unique(na.omit(checkvec))), nm = levels)
        checkvec <- setvl(checkvec, lab)
    }


    if (!is.null(label)) {
        attr(checkvec, "label") <- label
    }

    return(checkvec)
}
