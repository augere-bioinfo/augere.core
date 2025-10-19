# library(testthat); library(augere.core); source("test-evaluateChunks.R")

test_that("evaluateChunks works as expected", {
    chunks <- list("a <- 1", "a <- a + 2", "a <- NULL")
    env <- new.env()
    evaluateChunks(chunks, env)
    expect_null(env$a)
})

test_that("evaluateChunks respects eval", {
    chunks <- list(c("a <- 1", "a <- a + 2"), "a <- NULL")
    env <- new.env()
    attr(chunks[[2]], "eval") <- "a < 0"
    evaluateChunks(chunks, env)
    expect_identical(env$a, 3)
})

test_that("evaluateChunks respects ref.label", {
    chunks <- list("a <- 1", "a <- a + 2", "")
    names(chunks) <- c("", "foo1", "")
    env <- new.env()
    attr(chunks[[3]], "ref.label") <- "paste0('foo', 1)"
    evaluateChunks(chunks, env)
    expect_identical(env$a, 5)
})

test_that("evaluateChunks respects error", {
    chunks <- list("a <- 1", "a <- foobar + 2", c("a <- a + 5", "a <- a + 5")) 
    env <- new.env()
    expect_error(evaluateChunks(chunks, env), "foobar")
    attr(chunks[[2]], "error") <- "a > 0"
    expect_error(evaluateChunks(chunks, env), NA) 
    expect_identical(env$a, 11)
})
