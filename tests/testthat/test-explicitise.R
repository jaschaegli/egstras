# Create a temporary project directory, make it the working directory for the
# calling test, and write `files` (a named list of file contents) into it
local_project <- function(files = list(), env = parent.frame()) {
  dir <- tempfile("explicitise-")
  dir.create(dir)
  old <- setwd(dir)
  cleanup <- substitute(
    {
      setwd(old)
      unlink(dir, recursive = TRUE)
    },
    list(old = old, dir = dir)
  )
  do.call(base::on.exit, list(cleanup, add = TRUE), envir = env)

  for (path in names(files)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeChar(files[[path]], path, eos = NULL, useBytes = TRUE)
  }

  invisible(dir)
}

read_raw <- function(path) {
  readChar(path, file.size(path), useBytes = TRUE)
}

# Explicitise a single snippet outside git, with a fixed package order
expl_snippet <- function(
  code,
  use_packages = c("dplyr", "stats", "base"),
  ...
) {
  local_project(list(a.R = code))
  suppressMessages(
    explicitise(
      filesfrom = "wd",
      use_packages = use_packages,
      force = TRUE,
      ...
    )
  )
  read_raw("a.R")
}

test_that("plain calls are prefixed, base calls are not", {
  expect_equal(
    expl_snippet("x <- mutate(df, y = sd(z), w = paste(v))\n"),
    "x <- dplyr::mutate(df, y = stats::sd(z), w = paste(v))\n"
  )
})

test_that("comments and strings are untouched", {
  expect_equal(
    expl_snippet("y <- \"filter(x)\" # mutate(df)\n"),
    "y <- \"filter(x)\" # mutate(df)\n"
  )
})

test_that("prefixed calls and method calls are untouched", {
  code <- "dplyr::filter(x); stats:::sd(x); obj$mutate(); obj@select()\n"
  expect_equal(expl_snippet(code), code)
})

test_that("functions defined in the project are not prefixed", {
  local_project(list(
    a.R = "filter <- function(x) x\nselect = \\(x) x\n",
    "sub/b.R" = "filter(1); select(2); mutate(3)\n"
  ))
  suppressMessages(explicitise(
    "wd",
    use_packages = c("dplyr", "base"),
    force = TRUE
  ))

  expect_equal(read_raw("sub/b.R"), "filter(1); select(2); dplyr::mutate(3)\n")
})

test_that("non-function assignments do not shadow package functions", {
  expect_equal(
    expl_snippet("filter <- 1\nfilter(x)\n"),
    "filter <- 1\ndplyr::filter(x)\n"
  )
})

test_that("calls of a function's own arguments are only skipped inside it", {
  expect_equal(
    expl_snippet("g <- function(filter) filter(x)\nfilter(y)\n"),
    "g <- function(filter) filter(x)\ndplyr::filter(y)\n"
  )
})

test_that("ignore_functions are not prefixed", {
  expect_equal(
    expl_snippet("filter(x); mutate(y)\n", ignore_functions = "filter"),
    "filter(x); dplyr::mutate(y)\n"
  )
})

test_that("backticked names keep their backticks", {
  expect_equal(expl_snippet("`mutate`(df)\n"), "dplyr::`mutate`(df)\n")
})

test_that("replacement calls use the replacement function's export", {
  expect_equal(
    expl_snippet(
      "names2(x) <- y\nnames2(x)\n",
      use_packages = c("rlang", "base")
    ),
    "rlang::names2(x) <- y\nrlang::names2(x)\n"
  )
})

test_that("the first package in use_packages wins", {
  expect_equal(
    expl_snippet("filter(x)\n", c("dplyr", "stats")),
    "dplyr::filter(x)\n"
  )
  expect_equal(
    expl_snippet("filter(x)\n", c("stats", "dplyr")),
    "stats::filter(x)\n"
  )
})

test_that("tabs and non-ASCII characters before a call are handled", {
  expect_equal(
    expl_snippet("\tä <- \"ü\"; mutate(x)\n\t\tfilter(y)\n"),
    "\tä <- \"ü\"; dplyr::mutate(x)\n\t\tdplyr::filter(y)\n"
  )
})

test_that("multi-line calls are handled", {
  expect_equal(
    expl_snippet("x <- filter(df,\n  a == sd(\n    b))\n"),
    "x <- dplyr::filter(df,\n  a == stats::sd(\n    b))\n"
  )
})

test_that("line endings and trailing newlines are preserved", {
  expect_equal(
    expl_snippet("mutate(x)\r\nsd(y)\r\n"),
    "dplyr::mutate(x)\r\nstats::sd(y)\r\n"
  )
  expect_equal(expl_snippet("mutate(x)"), "dplyr::mutate(x)")
  expect_equal(expl_snippet("mutate(x)\n\n\n"), "dplyr::mutate(x)\n\n\n")
})

test_that("files that do not parse are skipped with a warning", {
  local_project(list(a.R = "mutate(x\n", b.R = "mutate(x)\n"))

  expect_warning(
    out <- suppressMessages(explicitise(
      "wd",
      use_packages = "dplyr",
      force = TRUE
    )),
    "do not parse"
  )
  expect_equal(out, "b.R")
  expect_equal(read_raw("a.R"), "mutate(x\n")
})

test_that("unresolved functions are reported once", {
  local_project(list(a.R = "nope(1)\nnope(2)\n", b.R = "nope(3)\n"))

  expect_warning(
    suppressMessages(explicitise("wd", use_packages = "base", force = TRUE)),
    "1 function"
  )
})

test_that("dry_run writes nothing", {
  local_project(list(a.R = "mutate(x)\n"))

  out <- suppressMessages(explicitise(
    "wd",
    use_packages = "dplyr",
    dry_run = TRUE
  ))

  expect_equal(out, "a.R")
  expect_equal(read_raw("a.R"), "mutate(x)\n")
})

test_that("files outside git are only rewritten with force", {
  local_project(list(a.R = "mutate(x)\n"))

  expect_error(
    suppressMessages(explicitise(use_packages = "dplyr")),
    "Refusing"
  )
  expect_equal(read_raw("a.R"), "mutate(x)\n")
})

test_that("in git, committed files are rewritten and dirty or untracked ones refused", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")

  local_project(list(
    a.R = "mutate(x)\n",
    "renv/activate.R" = "mutate(x)\n"
  ))
  git <- \(...) {
    system2(
      "git",
      c("-c", "user.name=t", "-c", "user.email=t@t", ...),
      stdout = FALSE,
      stderr = FALSE
    )
  }
  git("init", "-q")
  git("add", "-A")
  git("commit", "-q", "-m", "init")

  out <- suppressMessages(explicitise(use_packages = "dplyr"))
  expect_equal(out, "a.R")
  expect_equal(read_raw("a.R"), "dplyr::mutate(x)\n")
  expect_equal(read_raw("renv/activate.R"), "mutate(x)\n")

  # a.R now has unstaged changes, so a second run must refuse
  expect_error(
    suppressMessages(explicitise(use_packages = "dplyr")),
    "Refusing"
  )
})

test_that("no matching files is not an error", {
  local_project(list(a.txt = "mutate(x)\n"))

  expect_equal(
    suppressMessages(explicitise("wd", use_packages = "dplyr", force = TRUE)),
    character(0)
  )
})
