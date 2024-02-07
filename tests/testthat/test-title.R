test_that("pptitle works", {
    p <- pptitle(pppicture(PSTricks()),"title")
    n <- length(p$lines)
    expect_true(p$lines[[n]] == "\\uput[r](2,28.5){title}\n")
})
