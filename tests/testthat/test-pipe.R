test_that("pipe works", {
    p1 <- PSTricks()
    p2 <- p1 %>% {function(p)p}()
    expect_identical(p1,p2)
})
