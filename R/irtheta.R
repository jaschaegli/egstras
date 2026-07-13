#' Estimate Knowledge Ability Using a 2PL IRT Model
#'
#' Fits a unidimensional two-parameter logistic (2PL) item response theory
#' model to a set of dichotomously scored knowledge items and extracts
#' empirical Bayes ability estimates (thetas) for all respondents.
#'
#' Items must be coded as dichotomous variables (e.g. 0 = incorrect,
#' 1 = correct). User-defined missing values from SPSS-labelled variables are
#' automatically converted to `NA` prior to model fitting.
#'
#' @param data A data frame.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Dichotomous item
#'   variables to include in the model.
#' @param theta_name Name of the theta variable to create.
#' @param method Estimation method passed to [mirt::mirt()]. Default is
#'   `"EM"`.
#' @param fscores_method Method passed to [mirt::fscores()]. Default is
#'   `"EAP"`.
#'
#' @return
#' A copy of `data` with an additional numeric variable containing respondent
#' theta estimates.
#'
#' @details
#' A one-factor 2PL model is fitted using [mirt::mirt()] with
#' `itemtype = "2PL"`.
#'
#' Any SPSS-labelled user-missing values are converted to `NA` using
#' [haven::zap_missing()] before fitting.
#'
#' Respondent abilities are estimated using [mirt::fscores()]. By default,
#' expected-a-posteriori (EAP) estimates are returned.
#'
#' Respondents with all items missing receive `NA` theta estimates.
#'
#' @examples
#' \dontrun{
#'
#' dat <- fit_irt_theta(
#'   data = dat,
#'   cols = dplyr::ends_with("_res")
#' )
#'
#' dat <- fit_irt_theta(
#'   data = dat,
#'   cols = dplyr::starts_with("kn"),
#'   theta_name = "knowledge_theta"
#' )
#'
#' }
#'
#' @seealso
#' [mirt::mirt()],
#' [mirt::fscores()]
#'
#' @export

fit_irt_theta <- function(
    data,
    cols,
    theta_name = "theta",
    method = "EM",
    fscores_method = "EAP"
) {
    item_names <- names(
        dplyr::select(data, {{ cols }})
    )

    items <- data |>
        dplyr::select(dplyr::all_of(item_names)) |>
        haven::zap_missing()

    items <- dplyr::mutate(
        items,
        dplyr::across(
            dplyr::everything(),
            as.numeric
        )
    )

    complete_rows <- rowSums(!is.na(items)) > 0

    theta <- rep(NA_real_, nrow(data))

    model <- mirt::mirt(
        data = items[complete_rows, , drop = FALSE],
        model = 1,
        itemtype = "2PL",
        method = method,
        verbose = FALSE
    )

    theta[complete_rows] <- as.numeric(
        mirt::fscores(
            model,
            method = fscores_method,
            full.scores = TRUE
        )[, 1]
    )

    data[[theta_name]] <- theta

    data
}
