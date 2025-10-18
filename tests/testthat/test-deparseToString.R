# library(testthat); library(augere.core); source("test-deparseToString.R")

test_that("deparseToString works as expected", {
    text <- "foo bar"
    expect_identical(deparseToString(text), deparse(text))

    vec <- sample(100)
    expect_true(length(deparse(vec)) > 1)
    deparsed <- deparseToString(vec)
    expect_identical(length(deparsed), 1L)

    roundtrip <- eval(parse(text=deparsed))
    expect_identical(vec, roundtrip)
})
