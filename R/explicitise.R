#' Run git and capture its output
#'
#' Returns `NULL` (instead of erroring) when git is not installed or the command
#' exits with a non-zero status, e.g. outside a repository.
#'
#' @param args Character vector of arguments passed to `git`.
#' @keywords internal
#' @noRd

expl_git <- function(args) {
    if (!nzchar(Sys.which("git"))) {
        return(NULL)
    }

    # core.quotePath=false keeps non-ASCII paths unquoted
    out <- suppressWarnings(
        system2("git", c("-c", "core.quotePath=false", args), stdout = TRUE, stderr = FALSE)
    )

    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
        return(NULL)
    }

    out
}


#' Is the working directory inside a git work tree?
#'
#' @keywords internal
#' @noRd

expl_in_git <- function() {
    identical(expl_git(c("rev-parse", "--is-inside-work-tree")), "true")
}


#' Get list of target files from either the git index or the working directory
#'
#' All returned paths are relative to the working directory, and only existing
#' files are returned.
#'
#' @inheritParams explicitise
#' @keywords internal
#' @noRd

expl_filepick <- function(filesfrom, rgx_filesubset, rgx_exclude) {
    if (filesfrom == "git" && !expl_in_git()) {
        cli::cli_inform("Not inside a git work tree, falling back to all files in the working directory.")
        filesfrom <- "wd"
    }

    # `git ls-files` lists tracked files below the working directory, with paths
    # relative to it (unlike `git ls-tree`, which is relative to the repo root)
    rawlist <- switch(filesfrom,
        git = expl_git("ls-files"),
        wd = list.files(path = ".", recursive = TRUE)
    )

    filelist <- grep(x = rawlist, pattern = rgx_filesubset, value = TRUE, perl = TRUE, ignore.case = TRUE)

    if (!is.na(rgx_exclude)) {
        filelist <- grep(x = filelist, pattern = rgx_exclude, value = TRUE, perl = TRUE, invert = TRUE)
    }

    # tracked files can be missing from the work tree (deleted but not committed)
    filelist[file.exists(filelist) & !dir.exists(filelist)]
}


#' Find target files whose current content git could not restore
#'
#' These are files with unstaged modifications, untracked files, and (outside a
#' git work tree) all files.
#'
#' @param files Character vector of paths relative to the working directory.
#' @keywords internal
#' @noRd

expl_unrestorable <- function(files) {
    if (!expl_in_git()) {
        return(files)
    }

    risky <- c(
        expl_git(c("ls-files", "--modified")),
        expl_git(c("ls-files", "--others"))
    )

    intersect(files, risky)
}


#' Read a whole file as a single UTF-8 string, preserving line endings
#'
#' @keywords internal
#' @noRd

expl_read <- function(path) {
    size <- file.size(path)

    if (is.na(size) || size == 0) {
        return("")
    }

    text <- readChar(path, nchars = size, useBytes = TRUE)
    Encoding(text) <- "UTF-8"

    text
}


#' Write a single string to a file byte-for-byte
#'
#' @keywords internal
#' @noRd

expl_write <- function(text, path) {
    con <- file(path, open = "wb")
    on.exit(close(con))

    writeChar(text, con, eos = NULL, useBytes = TRUE)
}


#' Build a lookup table from exported function name to package
#'
#' When several packages export the same name, the first one in `use_packages`
#' wins.
#'
#' @param use_packages Character vector of installed packages, in order of
#'   priority.
#' @return A named character vector: names are exports, values are packages.
#' @keywords internal
#' @noRd

expl_exports <- function(use_packages) {
    exports <- lapply(use_packages, getNamespaceExports)
    lookup <- rep(use_packages, lengths(exports))
    names(lookup) <- unlist(exports)

    lookup[!duplicated(names(lookup))]
}


#' Parse code and return its parse data
#'
#' @param text A single string of R code.
#' @return The full parse data (see [utils::getParseData()]) ordered by
#'   position, an empty data frame for code without tokens, or `NULL` if the
#'   code does not parse.
#' @keywords internal
#' @noRd

expl_parse <- function(text) {
    # the parser rejects "\r"; dropping it before line ends does not shift any
    # columns, since it is always the last character of its line
    code <- gsub("\r\n", "\n", text, fixed = TRUE)

    exprs <- tryCatch(
        parse(text = code, keep.source = TRUE, encoding = "UTF-8"),
        error = function(e) NULL
    )

    if (is.null(exprs)) {
        return(NULL)
    }

    pd <- utils::getParseData(exprs, includeText = TRUE)

    if (is.null(pd)) {
        pd <- data.frame(
            line1 = integer(), col1 = integer(), line2 = integer(), col2 = integer(),
            id = integer(), parent = integer(), token = character(),
            terminal = logical(), text = character()
        )
    }

    pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
}


#' Names of functions defined at assignment in parsed code
#'
#' Finds `name <- function`, `name = function`, `name <<- function` and the
#' lambda equivalents (`\(x)`). Assignments to fields (`obj$name <- function`)
#' are not definitions of callable names and are ignored.
#'
#' @param pd Parse data as returned by `expl_parse()`.
#' @keywords internal
#' @noRd

expl_defined_functions <- function(pd) {
    tk <- pd[pd$terminal, ]
    n <- nrow(tk)

    if (n < 3) {
        return(character(0))
    }

    is_fun <- tk$token == "FUNCTION" | tk$text == "\\"
    is_assign <- tk$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN") & tk$text != "->"
    prev_tok <- c(NA, tk$token[-n])

    i <- seq_len(n - 2)
    hit <- tk$token[i] %in% c("SYMBOL", "STR_CONST") &
        !(prev_tok[i] %in% c("'$'", "'@'")) &
        is_assign[i + 1] &
        is_fun[i + 2]

    gsub("^[`'\"]|[`'\"]$", "", tk$text[i][hit])
}


#' Locate function calls that should get a namespace prefix
#'
#' @param pd Parse data as returned by `expl_parse()`.
#' @param lookup Lookup table as returned by `expl_exports()`.
#' @param skip Character vector of names never to prefix (functions defined in
#'   the project and `ignore_functions`).
#' @return A list with `calls`, a data frame of `line`, `col` and `pkg` for each
#'   call to prefix, and `unresolved`, the called names without a known package.
#' @keywords internal
#' @noRd

expl_find_calls <- function(pd, lookup, skip) {
    tk <- pd[pd$terminal, ]
    n <- nrow(tk)

    if (n == 0) {
        return(list(calls = data.frame(line = integer(), col = integer(), pkg = character()), unresolved = character()))
    }

    prev_tok <- c(NA, tk$token[-n])

    is_call <- tk$token == "SYMBOL_FUNCTION_CALL" &
        !(prev_tok %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"))
    calls <- tk[is_call, ]
    calls$name <- gsub("^`|`$", "", calls$text)

    # calls to a function's own arguments, e.g. `function(f) f(x)`, refer to the
    # argument and must not be prefixed
    formals <- tk[tk$token == "SYMBOL_FORMALS", ]
    fun_spans <- pd[match(formals$parent, pd$id), c("line1", "col1", "line2", "col2")]
    after <- \(l, c, L, C) l > L | (l == L & c >= C)
    is_arg_call <- vapply(seq_len(nrow(calls)), \(i) {
        hits <- formals$text == calls$name[i]
        any(
            after(calls$line1[i], calls$col1[i], fun_spans$line1[hits], fun_spans$col1[hits]) &
                after(fun_spans$line2[hits], fun_spans$col2[hits], calls$line1[i], calls$col1[i])
        )
    }, logical(1))
    calls <- calls[!is_arg_call & !(calls$name %in% skip), ]

    # replacement calls (`f(x) <- value`) use the export `f<-`; the call is the
    # grandparent of the function name token, and is directly followed by `<-`
    call_ids <- pd$parent[match(calls$parent, pd$id)]
    call_rows <- match(call_ids, pd$id)
    next_row <- vapply(seq_len(nrow(calls)), \(i) {
        later <- after(tk$line1, tk$col1, pd$line2[call_rows[i]], pd$col2[call_rows[i]] + 1)
        if (any(later)) which(later)[1] else NA_integer_
    }, integer(1))
    is_replacement <- tk$token[next_row] %in% c("LEFT_ASSIGN", "EQ_ASSIGN") &
        !(tk$text[next_row] %in% "->")
    lookup_name <- ifelse(is_replacement, paste0(calls$name, "<-"), calls$name)

    pkg <- unname(lookup[lookup_name])
    unresolved <- unique(calls$name[is.na(pkg)])
    keep <- !is.na(pkg) & pkg != "base"

    list(
        calls = data.frame(line = calls$line1[keep], col = calls$col1[keep], pkg = pkg[keep]),
        unresolved = unresolved
    )
}


#' Convert a parser column to a character index
#'
#' The parser counts columns with tabs expanded to the next multiple of 8.
#'
#' @param line A single line of code.
#' @param col Integer vector of parser columns on that line.
#' @keywords internal
#' @noRd

expl_col_to_index <- function(line, col) {
    if (!grepl("\t", line, fixed = TRUE)) {
        return(col)
    }

    chars <- strsplit(line, "", fixed = TRUE)[[1]]
    start_col <- integer(length(chars))
    cur <- 0L

    for (i in seq_along(chars)) {
        start_col[i] <- cur + 1L
        cur <- if (chars[i] == "\t") (cur %/% 8L + 1L) * 8L else cur + 1L
    }

    match(col, start_col)
}


#' Insert namespace prefixes into code at the given positions
#'
#' @param text A single string of R code.
#' @param calls Data frame of `line`, `col` and `pkg`, as returned in
#'   `expl_find_calls()$calls`.
#' @keywords internal
#' @noRd

expl_insert_prefixes <- function(text, calls) {
    if (nrow(calls) == 0) {
        return(text)
    }

    # splitting on "\n" only keeps any "\r" at the end of its line
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    trailing_newline <- endsWith(text, "\n")

    for (l in unique(calls$line)) {
        on_line <- calls[calls$line == l, ]
        idx <- expl_col_to_index(lines[l], on_line$col)
        ord <- order(idx, decreasing = TRUE) # right to left keeps indices valid

        for (j in ord) {
            lines[l] <- paste0(
                substr(lines[l], 1, idx[j] - 1),
                on_line$pkg[j], "::",
                substr(lines[l], idx[j], nchar(lines[l]))
            )
        }
    }

    out <- paste(lines, collapse = "\n")
    if (trailing_newline) paste0(out, "\n") else out
}


#' Make all function calls in the current directory explicit
#'
#' Loops over all target files in a project and adds namespace prefixes
#' (`pkg::`) to all calls of functions exported by the packages in
#' `use_packages`. By default, these are the dependencies found in the target
#' files by [renv::dependencies()].
#'
#' Files are rewritten **in place**. To keep this undoable, `explicitise()`
#' refuses to touch files whose current content git could not restore (files
#' with unstaged changes, untracked files, or any file outside a git work tree)
#' unless `force = TRUE`. Use `dry_run = TRUE` to see which files would change.
#'
#' Line endings, trailing newlines and encoding (assumed UTF-8) are preserved;
#' files whose content would not change are not written. Files that do not
#' parse are skipped with a warning.
#'
#' @section What is prefixed:
#' Function calls are found with R's own parser ([utils::getParseData()]), so
#' comments and string literals are never changed. Left alone are:
#'
#' * calls that already have a prefix (`pkg::f()`, `pkg:::f()`) and method
#'   calls (`obj$f()`, `obj@f()`);
#' * calls of functions defined anywhere in the target files
#'   (`f <- function(...)`, `f = \(...)`), and calls of a function's own
#'   arguments inside that function (`function(f) f(x)`);
#' * functions from `base`, and functions named in `ignore_functions`;
#' * infix operators (`%>%`, `%in%`) and functions called by name, e.g.
#'   `do.call("f", ...)` or `lapply(x, f)`.
#'
#' Replacement calls are prefixed using the replacement function's export, so
#' `var_label(x) <- "a"` becomes `labelled::var_label(x) <- "a"` only if
#' `labelled` exports `` `var_label<-` ``.
#'
#' When several packages export a function of the same name, the first package
#' in `use_packages` wins. With the default, that is the order in which
#' dependencies first appear in the target files, followed by the default
#' attached packages (`getOption("defaultPackages")`) and `base`.
#'
#' @param filesfrom A string that is either `"git"` or `"wd"`. `"git"` (the
#'   default) targets files tracked by git below the working directory, falling
#'   back to `"wd"` outside a git work tree. `"wd"` targets all files in the
#'   working directory.
#' @param rgx_filesubset A case-insensitive PCRE expression used for subsetting
#'   the list of target files. The default, `"\\.R$"`, limits the operation to
#'   files with a `.R` or `.r` extension.
#' @param rgx_exclude A case-sensitive PCRE expression; matching files are
#'   excluded from the target files. The default excludes the `renv/` and
#'   `packrat/` directories. Use `NA` to exclude nothing.
#' @param use_packages Character vector of installed packages whose exports are
#'   prefixed, in order of priority. `NULL` (the default) uses the installed
#'   dependencies found by [renv::dependencies()] in the target files, followed
#'   by the default attached packages and `base`.
#' @param ignore_functions Character vector of function names never to prefix.
#' @param dry_run If `TRUE`, report the files that would change without writing
#'   anything.
#' @param force If `TRUE`, also rewrite files whose current content git could
#'   not restore.
#'
#' @return Invisibly, a character vector of the paths of the files that were
#'   changed (or would be changed, if `dry_run = TRUE`).
#' @export

explicitise <- function(filesfrom = c("git", "wd"),
                        rgx_filesubset = "\\.R$",
                        rgx_exclude = "(^|/)(renv|packrat)/",
                        use_packages = NULL,
                        ignore_functions = character(),
                        dry_run = FALSE,
                        force = FALSE) {
    filesfrom <- match.arg(filesfrom)
    checkmate::qassert(rgx_filesubset, "S1") # single non-missing string
    checkmate::qassert(rgx_exclude, c("s1", "b1", "0")) # single string, NA or NULL
    checkmate::qassert(use_packages, c("S+", "0")) # non-missing strings or NULL
    checkmate::qassert(ignore_functions, "S") # non-missing strings, possibly none
    checkmate::qassert(dry_run, "B1") # single flag
    checkmate::qassert(force, "B1") # single flag

    if (is.null(rgx_exclude)) {
        rgx_exclude <- NA_character_
    }

    files <- expl_filepick(filesfrom, rgx_filesubset, rgx_exclude)

    if (length(files) == 0) {
        cli::cli_inform("No files match {.val {rgx_filesubset}}, nothing to do.")
        return(invisible(character(0)))
    }

    if (is.null(use_packages)) {
        deps <- unique(renv::dependencies(path = files, quiet = TRUE)[["Package"]])

        installed <- vapply(deps, \(pkg) nzchar(system.file(package = pkg)), logical(1))
        if (any(!installed)) {
            cli::cli_warn("Skipping dependencies that are not installed: {.pkg {deps[!installed]}}")
        }

        use_packages <- unique(c(deps[installed], getOption("defaultPackages"), "base"))
    }

    if (!force && !dry_run) {
        unrestorable <- expl_unrestorable(files)

        if (length(unrestorable) > 0) {
            cli::cli_abort(c(
                "Refusing to rewrite {length(unrestorable)} file{?s} whose current content git could not restore:",
                stats::setNames(unrestorable, rep("*", length(unrestorable))),
                "i" = "Commit or stash your changes first, or use {.code force = TRUE}."
            ))
        }
    }

    texts <- lapply(files, expl_read)
    parsed <- lapply(texts, expl_parse)

    unparsable <- vapply(parsed, is.null, logical(1))
    if (any(unparsable)) {
        cli::cli_warn("Skipping files that do not parse: {.file {files[unparsable]}}")
    }

    lookup <- expl_exports(use_packages)
    defined <- unique(unlist(lapply(parsed[!unparsable], expl_defined_functions)))
    skip <- c(defined, ignore_functions)

    changed <- logical(length(files))
    unresolved <- character(0)

    for (i in which(!unparsable)) {
        found <- expl_find_calls(parsed[[i]], lookup, skip)
        unresolved <- c(unresolved, found$unresolved)

        if (nrow(found$calls) == 0) {
            next
        }

        changed[i] <- TRUE

        if (!dry_run) {
            expl_write(expl_insert_prefixes(texts[[i]], found$calls), files[i])
        }
    }

    unresolved <- sort(unique(unresolved))
    if (length(unresolved) > 0) {
        cli::cli_warn("Couldn't find packages exporting {length(unresolved)} function{?s}: {.fn {unresolved}}")
    }

    changed <- files[changed]

    if (length(changed) == 0) {
        cli::cli_inform("No files needed changes.")
    } else if (dry_run) {
        cli::cli_inform("Would change {length(changed)} file{?s}: {.file {changed}}")
    } else {
        cli::cli_inform("Changed {length(changed)} file{?s}: {.file {changed}}")
    }

    invisible(changed)
}
