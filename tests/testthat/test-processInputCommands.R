# library(testthat); library(augere.core); source("test-processInputCommands.R")

test_that("wrapInput works as expected", {
    x <- data.frame(A=1,B=2)
    out <- wrapInput(data.frame(A=1,B=2), "foo")
    expect_s3_class(out, "augere.input")
    expect_identical(out$commands, "foo")
    expect_identical(out$object, x)

    out <- wrapInput(NULL, "foo")
    expect_s3_class(out, "augere.input")
    expect_identical(out$commands, "foo")
    expect_null(out$object)

    out <- wrapInput(x, NULL)
    expect_s3_class(out, "augere.input")
    expect_identical(out$object, x)
    expect_null(out$commands)

    out <- wrapInput(data.frame(x=runif(20)))
    expect_s3_class(out, "augere.input")
    expect_true(is.data.frame(out$object))
    expect_match(out$commands, "data.frame")
    expect_match(out$commands, "runif")
})

test_that("processInputCommands works as expected", {
    original.length <- length(augere.core:::.get_input_cache())
    fun <- resetInputCache()
    expect_identical(length(augere.core:::.get_input_cache()), 0L)

    df <- data.frame(A=1:5, whee=LETTERS[2:6])
    cmds <- processInputCommands(df, "my_df")
    env <- new.env()
    expect_error(eval(parse(text=cmds), envir=env), "insert commands")
    expect_identical(augere.core:::.get_input_cache()[[1]], df)

    wrapped <- wrapInput(NULL, c("y <- data.frame(B = 10:6)", "y$stuff <- LETTERS[5:9]", "y"))
    cmds <- processInputCommands(wrapped, "my_df")
    env <- new.env()
    eval(parse(text=cmds), envir=env)
    expected <- data.frame(B=10:6, stuff=LETTERS[5:9])
    expect_identical(env$my_df, expected)
    expect_identical(length(augere.core:::.get_input_cache()), 1L)

    df2 <- data.frame(C=runif(10), bar=letters[10:19])
    wrapped <- wrapInput(df2, c("something")) # commands don't get run
    cmds <- processInputCommands(wrapped, "my_df")
    expect_identical(augere.core:::.get_input_cache()[[2]], df2)

    expect_identical(augere.core:::.pop_input_cache(1), df)
    expect_identical(augere.core:::.pop_input_cache(2), df2)
    expect_identical(length(augere.core:::.get_input_cache()), 2L)
    expect_true(all(vapply(augere.core:::.get_input_cache(), is.null, TRUE)))

    fun()
    expect_identical(length(augere.core:::.get_input_cache()), original.length)
})
