test_that("creating a picture works", {

    p <- pppicture(PSTricks())

    expect_equal(p$dx, p$x)

    expect_equal(p$dy, p$y)

})
