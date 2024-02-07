test_that("conversion functions work", {

    xy <- c(1,2)

    p <- ppaxis(pppicture(PSTricks(),10,12),'x',lim=c(0,1))

    expect_equal(round(cx(p,xy),digits=3), c(9.5,9.972))

    expect_equal(round(icx(p,cx(p,xy)),digits=3), c(1,1.063))

    p <- ppaxis(pppicture(PSTricks(),10,12),'y',lim=c(0,1))

    expect_equal(round(cy(p,xy),digits=3), c(11,11.472))

    expect_equal(round(icy(p,cy(p,xy)),digits=3), c(1,1.052))

})
