test_that("xaspect calculates correct x", {
    
    y <- 29
    aspect <- 1.5
    nx <- 2
    ny <- 4
    nxaxes <- 1.2
    nyaxes <- 1.3
    ntitle <- 1.4
    width <- 1.6
    height <- 1.7
    margin <- 0.9

    p <- PSTricks() %>%
        ppsetmargins(margin) %>%
        pppicture(xaspect(y, aspect, nx, ny, nxaxes, nyaxes, ntitle, width, height, margin), y) %>%
        ppsubplot(nx, ny, 1, nxaxes, nyaxes, ntitle, width, height)

    expect_equal(p$hy/p$hx, aspect)
    
})

test_that("yaspect calculates correct y", {
    
    x <- 21
    aspect <- 1.5
    nx <- 2
    ny <- 4
    nxaxes <- 1.2
    nyaxes <- 1.3
    ntitle <- 1.4
    width <- 1.6
    height <- 1.7
    margin <- 0.9

    p <- PSTricks() %>%
        ppsetmargins(margin) %>%
        pppicture(x, yaspect(x, aspect, nx, ny, nxaxes, nyaxes, ntitle, width, height, margin)) %>%
        ppsubplot(nx, ny, 1, nxaxes, nyaxes, ntitle, width, height)

    expect_equal(p$hy/p$hx, aspect)
    
})
