# library(testthat); library(augere.core); source("test-replacePlaceholders.R")

test_that("replacePlaceholders works for character vectors", {
    test <- replacePlaceholders("<%=hello %> super foobar <%=super%> whee <%= foo-bar %>", c(hello="hi", super="X", `foo-bar`="Y"))
    expect_identical(test, "hi super foobar X whee Y")

    # Tightly packed.
    test <- replacePlaceholders("<%=hello %><%=super%><%= foo-bar %>", c(hello="hi", super="X", `foo-bar`="Y"))
    expect_identical(test, "hiXY")

    # No replacements to make.
    test <- replacePlaceholders("we are irreplaceable!", c(hello="hi", super="X", `foo-bar`="Y"))
    expect_identical(test, "we are irreplaceable!")

    # Multiple uses in the same string
    test <- replacePlaceholders("<%= first_name %> <%= last_name %>, <%= daughter_name %> <%= last_name %>", c(first_name="nagisa", last_name="furukawa", daughter_name="ushio"))
    expect_identical(test, "nagisa furukawa, ushio furukawa")

    # Multiple strings, multiple uses.
    test <- replacePlaceholders(c("whee<%= name %>foo<%= hobby %>bar", "<%= name %> loves <%=food%>"), c(food="dango", hobby="drama", name="nagisa"))
    expect_identical(test, c("wheenagisafoodramabar", "nagisa loves dango"))

    # Properly respects backslashes.
    test <- replacePlaceholders("insert <%= back_slashed %> here", c(back_slashed="\\t"))
    expect_identical(test, "insert \\t here")

    # Works if replacements is a list.
    test <- replacePlaceholders("<%=hello %> super foobar <%=super%> whee <%= foo-bar %>", list(hello="hi", super="X", `foo-bar`="Y"))
    expect_identical(test, "hi super foobar X whee Y")
})

test_that("replacePlaceholders works for input lists", {
    template <- list(
        "best character is <%= alpha %>",
        `after-story`=list(
            c("or maybe it's <%= bravo %> instead", "or even <%= charlie %>?"),
            `toki`=list("but let's go back to back to <%= alpha %>")
        ),
        c("<%= alpha %> is clearly the best", "with <%= bravo %> a close second")
    )

    test <- replacePlaceholders(template, c(alpha="nagisa", bravo="ushio", charlie="fuko"))
    expect_identical(test,
        list(
            "best character is nagisa",
            `after-story`=list(
                c("or maybe it's ushio instead", "or even fuko?"),
                `toki`=list("but let's go back to back to nagisa")
            ),
            c("nagisa is clearly the best", "with ushio a close second")
        )
    )
})

test_that("replacePlaceholder fails if it can't find the placeholder", {
    expect_error(replacePlaceholders("<%= YAY %>", character()), "no available replacements")
    expect_error(replacePlaceholders("<%= YAY %>", list()), "no available replacements")

    expect_error(replacePlaceholders("<%= FOO %> <%=YAY%> <%= BAR %>", list()), "no available replacements")
    partial <- replacePlaceholders("<%= FOO %> <%=YAY%> <%= STUFF %> <%= BAR %>", list(FOO=1, BAR=2), error=FALSE)
    expect_identical(partial, "1 <%=YAY%> <%= STUFF %> 2")
})
