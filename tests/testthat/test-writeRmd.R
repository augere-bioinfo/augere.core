# library(testthat); library(augere.core); source("test-writeRmd.R")

contents <- list(
    c("# TITLE", ""),
    list(
        c("```{r}", "a <- 1", "```"),
        c("", "", "super foo bar")
    ),
    c("", "## section")
)

test_that("writeRmd works as expected", {
    tmp <- tempfile(fileext=".Rmd")
    writeRmd(contents, tmp, clean.empty=FALSE)
    expect_identical(readLines(tmp), unlist(contents))

    writeRmd(contents, tmp, clean.empty=TRUE)
    expected <- unlist(contents)
    expected <- expected[-6]
    expect_identical(readLines(tmp), expected)

    writeRmd("FOO", tmp)
    writeRmd("BAR", tmp, append=TRUE)
    expect_identical(readLines(tmp), c("FOO", "BAR"))
})
