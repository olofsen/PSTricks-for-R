test_that("setting options works", {

    value <- -1 # will never be used as a default

    expect_equal(ppsetmargins(PSTricks(),value)$margin,value)
    expect_equal(ppsetmargins(PSTricks(),,value)$mrgaxes,value)

    expect_equal(pplinewidth(PSTricks(),value)$linewidth,value/10)

    expect_equal(ppsetxlabsep(PSTricks(),value)$xlabsep,value)
    expect_equal(ppsetylabsep(PSTricks(),value)$ylabsep,value)
    expect_equal(ppxlabsep(PSTricks(),value)$xlabsep,value)
    expect_equal(ppylabsep(PSTricks(),value)$ylabsep,value)

    expect_false(ppsetpsttoeps(PSTricks(),FALSE)$psttoeps)

    expect_false(ppsetlogx(PSTricks(),FALSE)$logx)
    expect_true(ppsetlogy(PSTricks())$logy)
    expect_false(ppsetnologx(PSTricks())$logx)
    expect_false(ppsetnology(PSTricks())$logy)

    expect_true({function (p) p$logx && p$logy} (ppsetlogxy(PSTricks())))
    expect_false({function (p) p$logx || p$logy} (ppsetlogxy(PSTricks(),FALSE)))
    expect_false({function (p) p$logx || p$logy} (ppsetnologxy(PSTricks())))

    expect_false(ppsetprimary(PSTricks(),'x')$secondx)
    expect_true(ppsetprimary(PSTricks(),'y',TRUE)$secondy)
    expect_true(ppsetsecondary(PSTricks(),'x')$secondx)
    expect_false(ppsetsecondary(PSTricks(),'y',FALSE)$secondy)

    expect_false(ppsetprimaryx(PSTricks())$secondx)
    expect_true(ppsetprimaryy(PSTricks(),TRUE)$secondy)
    expect_true(ppsetsecondaryx(PSTricks())$secondx)
    expect_false(ppsetsecondaryy(PSTricks(),FALSE)$secondy)

    expect_true(ppsetpolar(PSTricks())$polar)
    expect_false(ppsetcartesian(PSTricks())$polar)
})
