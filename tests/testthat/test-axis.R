test_that("axis works", {

    path <- paste0(getwd(),"/test.tex")

    f <- function()
    {
        xlims <- range(mtcars$hp)
        ylims <- range(mtcars$mpg)

        p <- pppicture(PSTricks())

        p <- ppsubplot(p,2,3)
        p <- ppaxis(p, 'x', xlims, "hp")
        p <- ppaxis(p, 'y', ylims, "mpg")
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        p <- ppsubplot(p)
        p <- ppaxis(p, 'x', rev(xlims), "hp")
        p <- ppaxis(p, 'y', rev(ylims), "mpg")
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        p <- ppsetlogxy(p)

        p <- ppsubplot(p)
        p <- ppaxis(p, 'x', xlims, "hp")
        p <- ppaxis(p, 'y', ylims, "mpg")
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        p <- ppsubplot(p)
        p <- ppaxis(p, 'x', rev(xlims), "hp")
        p <- ppaxis(p, 'y', rev(ylims), "mpg")
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        p <- ppsubplot(p)
        p <- ppaxis(p, 'x', xlims, "hp", noshow=TRUE)
        p <- ppaxis(p, 'y', ylims, "mpg")
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        p <- ppsubplot(p)
        p <- ppaxis(p, 'x', rev(xlims), "hp")
        p <- ppaxis(p, 'y', rev(ylims), "mpg", noshow=TRUE)
        p <- psdots(p, cx(p,mtcars$hp), cy(p,mtcars$mpg))

        ppwrite(p, path, topdf=FALSE)

        path
    }

    expect_snapshot_file(f(), compare=compare_file_text)

    file.remove(path)
})
