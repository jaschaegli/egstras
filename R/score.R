#' Score Single-Choice Knowledge Questions
#'
#' Compares responses to a predefined answer key and creates scored result
#' variables for each selected question. Result variables are added immediately
#' to the right of their source variables and returned as
#' [haven::labelled()] or [haven::labelled_spss()] vectors.
#'
#' Responses matching the correct answer receive `correct_value`; all other
#' valid responses receive `incorrect_value`. System missings (`NA`) are
#' preserved. User-defined missing values specified via `no_answer` are
#' retained in the output and optionally registered as SPSS user missings.
#'
#' @param data A data frame.
#' @param key Named vector containing the correct answer for each question.
#'   Names must correspond to variable names in `data`.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more question
#'   variables to score.
#' @param suffix Character scalar appended to each question name to create the
#'   corresponding result variable name.
#' @param no_answer Optional numeric vector of user-defined missing codes
#'   (e.g. `c(-99, -98)`). These values are retained in the scored variables.
#'   If `NULL`, only system missings (`NA`) are treated as missing.
#' @param correct_value Numeric value assigned to correct responses.
#' @param incorrect_value Numeric value assigned to incorrect responses.
#' @param result_labels Named numeric vector of value labels for the scored
#'   variables. Values should include labels for all possible output values,
#'   including any codes specified in `no_answer`.
#' @param column_label Function used to generate variable labels for scored
#'   variables. The function must accept a single character string (the source
#'   variable name) and return a character scalar.
#'
#' @return
#' A data frame with one additional scored variable per selected question.
#'
#' If `no_answer = NULL`, scored variables are returned as
#' [haven::labelled()] vectors.
#'
#' If `no_answer` is specified, scored variables are returned as
#' [haven::labelled_spss()] vectors with the supplied codes registered as
#' SPSS user-defined missings.
#'
#' @details
#' The answer key is matched by variable name. Variables selected via `cols`
#' that are not present in `key` are ignored.
#'
#' User-defined missing codes are propagated to the output unchanged. For
#' example, if a respondent answered `-98`, the scored variable will also
#' contain `-98` rather than being recoded to a generic missing value.
#'
#' Result variables are inserted immediately after their corresponding source
#' variables.
#'
#' @examples
#' key <- c(
#'   kn1 = 1,
#'   kn2 = 3
#' )
#'
#' dat <- tibble::tibble(
#'   kn1 = c(1, 2, -99),
#'   kn2 = c(3, 4, -98)
#' )
#'
#' score_items(
#'   data = dat,
#'   key = key,
#'   cols = dplyr::starts_with("kn")
#' )
#'
#' score_items(
#'   data = dat,
#'   key = key,
#'   cols = dplyr::starts_with("kn"),
#'   no_answer = c(-99, -98),
#'   result_labels = c(
#'     Incorrect = 0,
#'     Correct = 1,
#'     "No answer" = -99,
#'     "Don't know" = -98
#'   )
#' )
#'
#' score_items(
#'   data = dat,
#'   key = key,
#'   cols = dplyr::starts_with("kn"),
#'   column_label = function(x) {
#'     paste("Correctly answered:", x)
#'   }
#' )
#'
#' @seealso
#' [haven::labelled()],
#' [haven::labelled_spss()],
#' [dplyr::across()],
#' [data.table::fcase()]
#'
#' @export

score_items <- function(
  data,
  key,
  cols = tidyselect::everything(),
  suffix = "_res",
  no_answer = NULL,
  correct_value = 1,
  incorrect_value = 0,
  result_labels = NULL,
  column_label = \(x) {
    paste0("Answer to ", x, " correct?")
  }
) {
  question_cols <- names(
    dplyr::select(data, {{ cols }})
  )

  question_cols <- intersect(question_cols, names(key))

  for (q in question_cols) {
    res_col <- paste0(q, suffix)

    values <- if (is.null(no_answer)) {
      data.table::fcase(
        is.na(data[[q]])      , NA_real_      ,
        data[[q]] == key[[q]] , correct_value ,
        default = incorrect_value
      )
    } else {
      data.table::fcase(
        data[[q]] %in% no_answer , as.numeric(data[[q]]) ,
        is.na(data[[q]])         , NA_real_              ,
        data[[q]] == key[[q]]    , correct_value         ,
        default = incorrect_value
      )
    }

    data[[res_col]] <- if (is.null(no_answer)) {
      haven::labelled(
        values,
        labels = result_labels,
        label = column_label(q)
      )
    } else {
      haven::labelled_spss(
        values,
        labels = result_labels,
        na_values = unname(no_answer),
        label = column_label(q)
      )
    }

    data <- dplyr::relocate(
      data,
      dplyr::all_of(res_col),
      .after = dplyr::all_of(q)
    )
  }

  data
}
