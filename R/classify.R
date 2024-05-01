#' Return factors based on regular expression matches on an input vector
#'
#' @return A list with two components: "recoded" contains a vector of factors, "replacements" contains a data.frame of distinct replacement operations.
#' @param x A vector to classify
#' @param matchdf A data frame containing at minimum a column with regular expressions and a
#' column with associated factor levels. If the regex contains escape sequences (e.g. "\\n"),
#' the expression must either be wrapped in backticks instead of double quotes (see [add_backticks()]),
#' or escaped again (e.g. "\\\\n") before being fed to the function
#' @param col.regex The name of the column in matchdf holding regular expressions
#' @param col.factor The name of the column in matchdf which holds the factor levels
#' @param fallback The factor level to assign when no match is found; defaults to "Other/Unsortable"
#' @param use_perl Whether to treat the strings in col.regex as PCRE-compatible regular expressions (default: TRUE)
#' @param ignore_case Whether to ignore capitalisation when parsing the regex (default: TRUE)
#' @seealso [data.table::fcase()], which this function uses under the hood
#' @export


classify <- function(x, matchdf, col.regex, col.factor, fallback = "Other/Unsortable", use_perl = TRUE, ignore_case = TRUE) {
    checkmate::qassert(x, "s+") # string of length 1+, NAs ok
    checkmate::qassert(matchdf, "d") # data frame, NAs ok
    checkmate::qassert(use_perl, "B1") # boolean, NA not ok
    checkmate::qassert(ignore_case, "B1") # boolean, NA not ok

    col.factor <- rlang::ensym(col.factor)
    col.regex <- rlang::ensym(col.regex)

    check.columns <- as.character(unlist(c(col.factor, col.regex)))

    checkmate::assertSubset(check.columns, colnames(matchdf))


    formals(grepl)$perl <- use_perl
    formals(grepl)$ignore.case <- ignore_case


    # Sanitise input: Cut double spaces, newlines, excess whitespace

    x <- remove_whitespace(x)


    rgx <- matchdf[[col.regex]] |>
        remove_backticks(x = _, add_quotes = FALSE)
    fct <- matchdf[[col.factor]]


    ### Unleash the metaprogramming

    # Glue all fcase arguments. The comma is substituted with a semicolon because
    # these are technically two expressions: Arg and Return
    greps <- glue::glue('grepl(pattern = "{rgx}", x = x) == TRUE; "{fct}"')

    # rlang::parse expr returns a list of lists, so we have to unlist once before injecting
    ag <- lapply(greps, rlang::parse_exprs) |> unlist()


    # Frame expression with injected grep arguments
    expr <- rlang::expr(
        data.table::fcase(
            !!!ag,
            is.na(x) | x == "", "SETMETONA", # fcase can't mix returns, so we use a dummy NA
            default = fallback
        )
    )

    # Run
    y <- eval(expr)

    y[y == "SETMETONA"] <- NA

    replaced <-
        cbind(
            "Input" = x[!x == y],
            "Replacement" = y[!x == y]
        ) |>
        data.frame()

    distinctrep <- na.omit(replaced[!(duplicated(replaced)), ])

    cli::cli_div(theme = list(span.instruct = list(color = "#f5da0f")))
    cli::cli_text(
        "Made {nrow(replaced)} replacements, of which
        {nrow(distinctrep)}
        were distinct.\n
        {.instruct Please ensure their accuracy before proceeding!}"
    )
    cli::cli_end()
    print(distinctrep)

    y <- factor(y)

    outlist <- list("recoded" = y, "replacements" = distinctrep)

    invisible(outlist)
}
