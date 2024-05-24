#' Get value labels
#'
#' Returns value labels as written by e.g. SPSS
#' @param x a labelled vector
#' @param textonly a boolean indicating whether to return just the label, or also the corresponding number
#' @export

getvl <- function(x, textonly = FALSE) {
    qassert(x, "v") # Atomic vector
    out <- attr(x, "labels")
    if (textonly == TRUE) {
        out <- names(out)
    }
    return(out)
}

#' @rdname getvl
#' @export
vl_get <- getvl



#' Set value labels for matching args
#'
#' @keywords internal
setvl_helper_add_partial <- function(x, labs) {
    if (!all(labs %in% unique(stats::na.omit(x)))) {
        warning("some labels are not present in x, and will be ignored")
    }
    labs_missing <- x[which(!x %in% labs)]
    nums_as_labs <- stats::setNames(labs_missing, nm = as.character(labs_missing))
    labs <- sort(c(nums_as_labs, labs))
    attr(x, "labels") <- sort(labs)

    return(x)
}



#' Set value labels with unnamed args
#'
#' @keywords internal
setvl_helper_add_unnamed <- function(x, labs) {
    labs <- stats::setNames(
        sort(unique(as.numeric(stats::na.omit(x)))),
        nm = labs
    )
    attr(x, "labels") <- labs
    return(x)
}



#' Set value labels
#'
#' sets the "labels" attribute
#' @param x a vector to be labelled
#' @param labs a vector containing labels; either an unnamed character vector
#' equal in length to the number of unique elements of x, or a named vector
#' @param fill_partial A boolean indicating whether to force-construct "missing" (unspecified) labels by recycling numerical values.
#' Only applicable when labs is a named vector with fewer elements than there are unique values in x.
#' @export
setvl <- function(x, labs, fill_partial = TRUE) {
    checkmate::qassert(x, "x") # Integerish, NAs ok
    checkmate::assertAtomicVector(labs, unique = TRUE, any.missing = FALSE)

    if (namecheck(labs) == "some_named") {
        stop("Labels may not mix named and unnamed elements")
    }

    ulen <- length(unique(stats::na.omit(x)))
    labsnamed <- checkmate::testAtomicVector(labs, names = "named")

    if (
        ((!length(labs) == ulen) && !labsnamed) ||
            ((length(labs) == ulen) && !is.character(labs) && !labsnamed)
    ) {
        stop("Labels must either be a character vector of length unique(x), or a named vector")
    }

    if (!length(labs) == ulen) {
        if (!any(labs %in% x)) {
            stop("Labels are not equal in length to x and contain no variables to match on")
        } else if (fill_partial == FALSE) {
            attr(x, "labels") <- labs
        } else {
            x <- setvl_helper_add_partial(x, labs)
        }
    } else {
        if (!checkmate::testAtomicVector(labs, names = "named")) {
            warning("Labels are not a named vector; assigning based on order")
            x <- setvl_helper_add_unnamed(x, labs)
        } else {
            attr(x, "labels") <- labs
        }
    }
    return(x)
}

#' @rdname setvl
#' @export
vl_set <- setvl




#' Get variable label
#'
#' Retrieves variable (column) labels
#' @param x a labelled vector
#' @export

getcl <- function(x) {
    checkmate::qassert(x, "v") # Atomic vector, NAs ok
    out <- attr(x, "label", exact = TRUE)
    return(out)
}

#' @rdname getcl
#' @export
cl_get <- getcl



#' Set variable label
#'
#' Sets variable (column) labels
#' @param x a vector to be labelled
#' @param lab a label to be set; must be a character vector of length 1
#' @export
setcl <- function(x, lab) {
    checkmate::qassert(lab, "S1") # String of length 1, no NAs
    attr(x, "label") <- lab
    return(x)
}

#' @rdname setcl
#' @export
cl_set <- setcl
