#' Get list of .R files in either git tree or working directory
#'
#' @keywords internal

expl_filepick <- function(filesfrom, rgx_filesubset) {
    expl_rlwd <- function() {
        out <- list.files(path = getwd(), recursive = TRUE)

        return(out)
    }


    if (filesfrom == "git") {
        gs <- system("git status", intern = TRUE) |> paste(collapse = " ")

        if (grepl(x = gs, pattern = "^fatal")) {
            cli::cli_inform("Not inside a git repository, falling back to all files in wd")

            rawlist <- expl_rlwd()
        } else {
            cur_branch <- gsub(
                x = gs,
                pattern = "On branch | Your branch .*",
                replacement = "",
                perl = TRUE
            )

            git_arg <- paste0("git ls-tree -r ", cur_branch, " --name-only")

            rawlist <- system(git_arg, intern = TRUE)
        }
    }

    if (filesfrom == "wd") {
        rawlist <- expl_rlwd
    }

    filelist <- rawlist |>
        grep(x = _, pattern = rgx_filesubset, value = TRUE, perl = TRUE, ignore.case = TRUE) |>
        paste0(x = "./", y = _)

    return(filelist)
}


#' Make all function calls in the current directory explicit
#'
#' Loops over all files in a project and adds namespace prefixes to all function calls by searching for dependencies inside the working directory with [renv::dependencies()] and feeding them to [pedant::add_double_colons()]
#' @param filesfrom A string that is either "git", or "wd", indicating whether to explicitise all files on the currently-active git branch (the default, if available), or all files in the working directory
#' @param rgx_filesubset A case_insensitive PCRE expression used for subsetting the list of target files that explicitiser is run on.  The default expression, "\\.R$", limits the operations to files with a ".R" or ".r" file extension.
#' @param ... Additional arguments passed to [pedant::add_double_colons()]
#' @export

explicitise <- function(filesfrom = c("git", "wd"), rgx_filesubset = "\\.R$", ...) {
    filesfrom <- match.arg(filesfrom)

    file <- expl_filepick(filesfrom, rgx_filesubset)

    deps <- renv::dependencies(path = getwd(), quiet = TRUE)[, "Package"] |>
        unique() |>
        unlist() |>
        setNames(nm = c()) |>
        sort()

    toexplicit <- function(file, dependencies) {
        text <- readLines(file)
        expanded <- lapply(text, \(x) {
            pedant::add_double_colons(x, use_packages = dependencies, ...)
        }) |> unlist()

        writeLines(expanded, file)
    }

    lapply(file, \(x) {
        toexplicit(x)
    })
}
