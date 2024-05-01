#' Return factors based on regular expression matches on an input vector
#'
#' Outputs a list with two components: "recoded" contains a vector of factors, "replacements" contains a data.frame of distinct replacement operations
#' @param x a vector to classify
#' @param matchdf a data frame containing at minimum a column with regular expressions and a column with associated factor levels
#' @param col.regex name of the column in matchdf which holds the regular expressions
#' @param col.factor name of the column in matchdf which holds the factor levels
#' @param fallback The factor level to assign when no match is found; defaults to "Other/Unsortable"
#' @export


classify <- function(x, matchdf, col.regex, col.factor, fallback = "Other/Unsortable") {
    checkmate::qassert(x, "s+") # string of length 1+, NAs ok
    checkmate::qassert(matchdf, "d") # data frame, NAs ok

    col.factor <- rlang::ensym(col.factor)
    col.regex <- rlang::ensym(col.regex)

    check.columns <- as.character(unlist(c(col.factor, col.regex)))

    checkmate::assertSubset(check.columns, colnames(matchdf))


    formals(grepl)$perl <- TRUE
    formals(grepl)$ignore.case <- TRUE


    # Sanitise input: Cut double spaces, newlines, excess whitespace

    x <- x |>
        trimws() |>
        gsub("\\n+", " ", x = _) |>
        gsub("\\s+", " ", x = _)


    rgx <- matchdf[[col.regex]]

    fct <- matchdf[[col.factor]]


    ### Unleash the metaprogramming

    # Glue all fcase arguments. The comma is substituted with a semicolon because
    # these are technically two expressions: Arg and Return
    greps <- glue::glue('grepl(pattern = "{rgx}", x = x) == TRUE; "{fct}"')

    # rlang::parse expr returns a list of lists, so we have to unlist once before injecting
    ag <- lapply(greps, rlang::parse_exprs) |> unlist()


    # Frame expression with injected grep arguments
    expr <- rlang::expr(data.table::fcase(
        !!!ag,
        is.na(x) | x == "", "SETMETONA", # fcase can't mix returns, so we use a dummy NA
        default = fallback
    ))

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

    formals(grepl)$ignore.case <- FALSE
    formals(grepl)$perl <- FALSE

    invisible(outlist)
}
