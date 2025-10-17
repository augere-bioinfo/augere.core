# library(testthat); library(augere.core); source("test-parseRmdTemplate.R")

template <- "
This is some text at the top level.

```{r}
characters <- 'nagisa'
```

:BEGIN top-block 

Now we enter the first tag pair.

:BEGIN nested-block

And now entering a nested tag pair.

```{r}
characters <- c(characters, 'ushio')
```

:END

Exiting out of the nested tag pair, back into the first tag pair.

:BEGIN another-nested-block

Now entering the second nested tag pair.

```{r}
characters <- c(characters, 'fuko')
```

:END

:END

And finally, backing out of all tag pairs to the top-level."

test_that("template parsing works as expected" ,{
    out <- parseRmdTemplate(template)
    expect_true("top-block" %in% names(out))
    expect_type(out$`top-block`, "list")
    expect_true("nested-block" %in% names(out$`top-block`))
    expect_true("another-nested-block" %in% names(out$`top-block`))
})

test_that("template parsing preserves order" ,{
    out <- parseRmdTemplate(template)
    processed <- unlist(out, use.names=FALSE)

    all.lines <- strsplit(template, "\n")[[1]]
    leftover <- all.lines[!grepl(":(BEGIN|END)", all.lines)]
    expect_identical(processed, leftover)
})

test_that("parsing fails for unpaired BEGIN/END tags", {
    template <- "
Kimi dake ga kimi dake ga

:BEGIN clannad

```{r}
ushio <- TRUE
```

:BEGIN after-story

Soba ni inai yo
Kinou made sugu soba de boku o miteta yo

:END"
    expect_error(parseRmdTemplate(template), "unpaired BEGIN")

    template <- "
Kimi dake ga kimi dake ga

:BEGIN clannad

```{r}
ushio <- TRUE
```

Soba ni inai yo
Kinou made sugu soba de boku o miteta yo

:END

:END"
    expect_error(parseRmdTemplate(template), "unpaired END")
})

test_that("template parsing fails for bad names", {
    template <- "
Nakayoshi dango te o tsunagi o-oki na marui wa ni naru yo

:BEGIN dango

Machi o tsukuri dango hoshi no ue minna de waraiau yo

:END

Usagi mo sora de te o futte miteru dekkai o-tsuki-sama

:BEGIN dango

Ureshii koto kanashii koto mo zenbu marumete

:END"
    expect_error(parseRmdTemplate(template), "duplicate")

    template <- "
Aka-chan dango wa itsumo shiawase no naka de

:BEGIN 

Toshiyori dango wa me o hosometeru

:END"
    expect_error(parseRmdTemplate(template), "unnamed")

    template <- "
Osumashi goma-dango yotsugo kushi dango

:BEGIN dango

Minna minna awasete hyakunin kazoku

:END daikazoku"
    expect_error(parseRmdTemplate(template), "different")
})

test_that("parsing works with consistent BEGIN/END names", {
    template <- "
Kimi dake wo kimi dake wo

:BEGIN nagisa

Suki de ita yo
Kaze de me ga nijinde

```{r}
a <- 1
```

Tooku naru yo
                   
:END nagisa"
    expect_error(parseRmdTemplate(template), NA)
})
