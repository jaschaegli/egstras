#' Get value labels
#'
#' Returns value labels as written by e.g. SPSS
#' @param x a labelled vector
#' @param textonly a boolean indicating whether to return just the label, or also the corresponding number
#' @export

getvl <- function(x, textonly = FALSE) {
    out <- attr(x, "labels")
    if (textonly == TRUE) {
        out <- names(out)
    }
    return(out)
}



#' Get value labels
#'
#' Returns value labels as written by e.g. SPSS
#' @param x a vector to be labelled
#' @param labs a vector with labels; either an unnamed character vector, or a named numeric vector;
#' length must be equal to the number of unique elements in x
#' @export
setvl <- function(x, labs) {
    inputcomplaint <- "labs must either be a named numeric vector in the form
    c('Example' = 1, 'Test' = 2), or an unnamed character vector"
    numericcomplaint <- "x is not numeric; numbers in the label may not
    correspond to values of x"


    if (!is.vector(labs)) {
        stop(inputcomplaint)
    }

    if (!length(labs) == length(unique(x))) {
        stop("You must provide as many labels as there are unique values in x")
    }

    if (is.null(names(labs))) {
        if (!is.character(labs)) {
            stop(inputcomplaint)
        }

        if (is.numeric(x)) {
            labs <- setNames(
                order(unique(as.numeric(x))),
                nm = labs
            )
        } else {
            warning(numericcomplaint)
            labs <- setNames(
                c(seq_along(labs)),
                nm = labs
            )
        }
        attr(x, "labels") <- labs
    }

    if (!is.null(names(labs))) {
        if (!is.numeric(labs)) {
            stop(inputcomplaint)
        }

        if (is.numeric(x) && !all(labs) %in% x) {
            warning("Label numbers do not fully match numeric values of x")
        }

        if (!is.numeric(x)) {
            warning(numericcomplaint)
        }

        attr(x, "labels") <- sort(labs)
    }
    return(x)
}



#' Get variable label
#'
#' Retrieves variable (column) labels
#' @param x a labelled vector
#' @export

getcl <- function(x) {
    out <- attr(x, "label")
    return(out)
}



#' Set variable label
#'
#' Sets variable (column) labels
#' @param x a vector to be labelled
#' @param lab a label to be set; must be a character vector of length 1
#' @export
setcl <- function(x, lab) {
    if (!length(lab) == 1 || !is.character(lab)) {
        stop("lab must be a character vector of length 1")
    }
    attr(x, "label") <- lab
    return(x)
}
