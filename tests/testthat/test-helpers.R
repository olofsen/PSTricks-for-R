test_that("ppappendline works", {
    p <- PSTricks()
    p <- ppappendline(p,"test")
    n <- length(p$lines)
    expect_identical(n,1L)
    expect_identical(p$lines[[n]],"test\n")
})

test_that("ppappend works", {
    p <- PSTricks()
    p <- ppappend(p,"test")
    n <- length(p$lines)
    expect_identical(n,11L)
    expect_identical(p$lines[[n]],"test\n")
})

test_that("pparg works", {
    arg <- pparg("arg")
    expect_identical(arg,"{arg}")
})

test_that("ppopt works", {
    opt <- ppopt("opt")
    expect_identical(opt,"[opt]")
})

test_that("ppcoords works", {
    coords <- ppcoords(,1,2) # single pair
    expect_identical(coords,"(1,2)")
    coords <- ppcoords(,c(1,2),c(3,4)) # double pair
    expect_identical(coords,"(1,3)(2,4)")
    coords <- ppcoords(list(polar=TRUE,degrees=400),c(1,2),c(50,-50)) # polar with grad
    expect_identical(coords,"(0.707,0.707)(1.414,-1.414)")
    coords <- ppcoords(,"A","B") # node names
    expect_identical(coords,"(A)(B)")
})

test_that("ppbuild works", {
    cmd <- ppbuild("ppbuild",1,2,"opt","arg","arg1","arg2","arg3","arg4","arg0",TRUE)
    expect_identical(cmd,"\\ppbuild*{arg0}[opt]{arg}(1,2){arg1}{arg2}{arg3}{arg4}")
})
