test_that("ppxticks and ppyticks work", {

    expect_equal(ppxticks(PSTricks(),1)$xticks$nticks, 1)

    expect_equal(ppyticks(PSTricks(),1)$yticks$nticks, 1)

})
