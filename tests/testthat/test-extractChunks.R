# library(testthat); library(augere.core); source("test-extractChunks.R")

test_that("extractChunks works with unnamed chunks", {
    payload <- "
```{r}
a <- 1
b <- 2
```

yancha na yaki-dango yasashii an-dango

```{r, eval=FALSE}
c <- 3
d <- 4
```

sukoshi yumemigachi na tsukimi-dango

    ```{r  ,  echo = a > 2  ,  error=b * c != d }
    e <- 5
    f <- 6
    ```
"

    processed <- extractChunks(unlist(strsplit(payload, "\n")))
    expect_identical(length(processed), 3L)
    expect_null(names(processed))

    expect_identical(processed[[1]], c("a <- 1", "b <- 2"))

    expected2 <- c("c <- 3", "d <- 4")
    attr(expected2, "eval") <- "FALSE"
    expect_identical(processed[[2]], expected2)

    expected3 <- c("    e <- 5", "    f <- 6")
    attr(expected3, "echo") <- "a > 2"
    attr(expected3, "error") <- "b * c != d"
    expect_identical(processed[[3]], expected3)
})

test_that("extractChunks works with named chunks", {
    payload <- "
```{r foo}
a <- 1
b <- 2
```

yancha na yaki-dango yasashii an-dango

```{r bar, eval=(b==2)}
c <- 3
d <- 4
```

sukoshi yumemigachi na tsukimi-dango

```{r   foo-bar,  echo=(a!=b),  error=(d*2>1) }
e <- 5
f <- 6
```"

    processed <- extractChunks(unlist(strsplit(payload, "\n")))
    expect_identical(length(processed), 3L)
    expect_identical(names(processed), c("foo", "bar", "foo-bar")) # make sure it works with a non-syntatically valid name.

    expect_identical(processed[[1]], c("a <- 1", "b <- 2"))

    expected2 <- c("c <- 3", "d <- 4")
    attr(expected2, "eval") <- "(b == 2)"
    expect_identical(processed[[2]], expected2)

    expected3 <- c("e <- 5", "f <- 6")
    attr(expected3, "echo") <- "(a != b)"
    attr(expected3, "error") <- "(d * 2 > 1)"
    expect_identical(processed[[3]], expected3)

    # Also fine with partial or repeated names.
    payload2 <- paste0(payload, "
osumashi goma-dango yotsugo kushi dango

```{r}
g <- 7
h <- 8
```

minna minna awasete hyakunin kazoku

```{r bar}
i <- 9
j <- 10
```")

    processed2 <- extractChunks(unlist(strsplit(payload2, "\n")))
    expect_identical(processed, processed2[1:3]) 
    expect_identical(names(processed2), c("foo", "bar", "foo-bar", "", "bar"))
    expect_identical(processed2[[4]], c("g <- 7", "h <- 8"))
    expect_identical(processed2[[5]], c("i <- 9", "j <- 10"))
})

test_that("extractChunks works with inline chunks", {
    payload <- "
```{r foo}
a <- 1
b <- 2
```

yancha na `r 'yaki-dango'` yasashii `an-dango`

```{r bar}
c <- 3
d <- 4
```

sukoshi `r paste0('yume', 'migachi')` `na` `r paste0('tsukimi', 'dango', sep='-')`

```{r foo-bar}
e <- 5
f <- 6
```"

    processed <- extractChunks(unlist(strsplit(payload, "\n")))
    expect_identical(length(processed), 6L)
    expect_identical(names(processed), c("foo", "", "bar", "", "", "foo-bar"))

    expect_identical(processed[[1]], c("a <- 1", "b <- 2"))
    expect_identical(processed[[2]], "'yaki-dango'")
    expect_identical(processed[[3]], c('c <- 3', 'd <- 4'))
    expect_identical(processed[[4]], "paste0('yume', 'migachi')")
    expect_identical(processed[[5]], "paste0('tsukimi', 'dango', sep='-')")
})

test_that("extractChunks fails with mismatching delimiters", {
    payload <- "```{r}
```{r}
```"
    expect_error(extractChunks(unlist(strsplit(payload, "\n"))), "unclosed chunk")

    payload <- "```{r}"
    expect_error(extractChunks(unlist(strsplit(payload, "\n"))), "unclosed chunk")

    payload <- " 
Testing what happens when the end delimiter is not the same as the starting delimiter. 

    ```{r}

```"
    expect_error(extractChunks(unlist(strsplit(payload, "\n"))), "unclosed chunk")
})



