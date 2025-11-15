# library(testthat); library(augere.core); source("test-saveResult.R")

library(S4Vectors)
df <- DataFrame(A = 1:5, B=2:6)

test_that("saveResult works correctly with metadata", {
    meta <- list(
        title = "This is a very important result",
        author = "Myself",
        date = "2022-02-22"
    )

    tmp <- tempfile()
    saveResult(df, tmp, metadata = meta)
    roundtrip <- readResult(tmp)
    expect_identical(roundtrip$x, df)
    expect_identical(roundtrip$metadata, meta)

    # Duplicate metadata fields are correctly removed.
    tmp <- tempfile()
    saveResult(df, tmp, metadata = c(meta, list(title="POCKET SAND")))
    roundtrip <- readResult(tmp)
    expect_identical(roundtrip$metadata, meta)
})

test_that("saveResult works correctly without metadata", {
    tmp <- tempfile()
    saveResult(df, tmp, metadata = NULL)

    roundtrip <- readResult(tmp)
    expect_identical(roundtrip$x, df)
    expect_null(roundtrip$metadata)

    roundtrip <- readResult(tmp, meta.name = NULL)
    expect_identical(roundtrip$x, df)
    expect_null(roundtrip$metadata)
})
