#' Estimate Knowledge Ability Using a 2PL IRT Model
#'
#' Fits a unidimensional two-parameter logistic (2PL) item response theory
#' model to a set of dichotomously scored knowledge items and extracts
#' empirical Bayes ability estimates (thetas) for all respondents.
#'
#' Items must be coded as dichotomous variables (e.g. 0 = incorrect,
#' 1 = correct). Non-dichotomous input variables can be transformed using
#' the "coerce_" arguments, which will recode values for the model computation
#' but return the original columns in its original state.
#'
#' User-defined missing values from SPSS-labelled variables are
#' automatically converted to `NA` prior to model fitting.
#'
#'
#'
#'
#' @param data A data frame.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Dichotomous item
#'   variables to include in the model.
#' @param theta_name Name of the theta variable to create.
#' @param method Estimation method passed to [mirt::mirt()]. Default is
#'   `"EM"`.
#' @param fscores_method Method passed to [mirt::fscores()]. Default is
#'   `"EAP"`.
#' @param return_model Logical indicating whether the fitted IRT model should
#'   be returned alongside the data. Defaults to `FALSE`.
#' @param coerce_correct Vector holding additional values that should be treated as
#' 1 (i.e. correct responses)
#' @param coerce_incorrect Vector holding additional values that should be treated as
#' 0 (i.e. incorrect responses)
#' @param coerce_na Vector holding additional values that should be treated as
#' missings
#' @return
#' If `return_model = FALSE` (default), a data frame with an additional theta
#' variable.
#'
#' If `return_model = TRUE`, a list with elements:
#' \describe{
#'   \item{data}{The input data with appended theta estimates.}
#'   \item{model}{The fitted \code{mirt} 2PL model object.}
#' }
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
  fscores_method = "EAP",
  return_model = FALSE,
  coerce_correct = NULL,
  coerce_incorrect = NULL,
  coerce_na = NULL
) {
  data_cop <- data

  item_names <- names(
    dplyr::select(data, {{ cols }})
  )

  if (!is.null(coerce_correct)) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(item_names),
          .fns = \(x) {
            dplyr::replace_when(x, x %in% coerce_correct ~ 1)
          }
        )
      )
  }

  if (!is.null(coerce_incorrect)) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(item_names),
          .fns = \(x) {
            dplyr::replace_when(x, x %in% coerce_incorrect ~ 0)
          }
        )
      )
  }

  if (!is.null(coerce_na)) {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(item_names),
          .fns = \(x) {
            dplyr::replace_when(x, x %in% coerce_na ~ NA_real_)
          }
        )
      )
  }

  items <- data |>
    dplyr::select(tidyselect::all_of(item_names)) |>
    haven::zap_missing()

  items <- dplyr::mutate(
    items,
    dplyr::across(
      dplyr::everything(),
      as.numeric
    )
  )

  theta <- rep(NA_real_, nrow(data))

  model <- mirt::mirt(
    data = items,
    model = 1,
    itemtype = "2PL",
    method = method,
    verbose = FALSE
  )

  theta <- as.numeric(
    mirt::fscores(
      model,
      method = fscores_method,
      full.scores = TRUE
    )[, 1]
  )

  data_cop[[theta_name]] <- theta

  if (return_model) {
    return(
      list(
        data = data_cop,
        model = model
      )
    )
  }

  data_cop
}
