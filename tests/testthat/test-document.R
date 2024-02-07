test_that("PSTricks saves a correct document", {

    path <- paste0(getwd(),"/test.tex")

    f <- function()
    {
        p <- PSTricks()
        ppwrite(p, path, topdf=FALSE)
        path
    }

    expect_snapshot_file(f(), compare=compare_file_text)

    file.remove(path)

})
