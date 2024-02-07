test_that("sifelse works", {
    expect_equal(sifelse(TRUE,1,2), 1)
    expect_equal(sifelse(FALSE,1,2), 2)
})

test_that("merge.list works", {
    expect_equal(merge(list(a=3,b=4),list(a=30,c=40)),list(a=3,b=4,c=40))
})

test_that("cvtname works", {
    fun <- function() {
        cvtname(match.call()[[1]])
    }
    expect_equal(fun(),"fun")
})
