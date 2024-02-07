## helper functions
## no examples, but tested by testthat

#' Append Line to Lines Attribute in the PSTricks Object
#' @param p The PSTricks object.
#' @param s The string to append.
#' @return The updated PSTricks object.
#' @noRd

ppappendline <- function(p,s)
{
    p$lines <- append(p$lines,paste0(s,"\n"))
    p
}

#' Append Line to Lines Attribute in the PSTricks Object
#' @param p The PSTricks object.
#' @param s The string to append.
#' @return The updated PSTricks object.
#' @export

ppappend <- function(p,s)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (!p$docOpened) p <- ppopendoc(p)
    ppappendline(p,s)
}

#' Construct `pstricks` Argument
#' @param arg Argument.
#' @return Argument string (using curly braces), or empty string if `arg` is `NULL`.
#' @export

pparg <- function(arg=NULL)
{
    sifelse(is.null(arg),"",paste0('{',arg,'}'))
}

#' Construct `pstricks` Option
#' @param opt Option.
#' @return Option string (using brackets), or empty string if `arg` is `NULL`.
#' @export

ppopt <- function(opt=NULL)
{
    sifelse(is.null(opt),"",paste0('[',opt,']'))
}

#' Construct `pstricks` Macro Coordinates
#' @param p The PSTricks object.
#' @param x,y Coordinates.
#' @return Coordinates string (using parentheses), or empty string if `x` or `y` is `NULL`.
#' @export

ppcoords <- function(p=NULL,x,y)
{
    if (is.null(x) || is.null(y)) return("")

    if (is.numeric(x)) {
        if (!is.null(p)) if (!is.null(p$polar)) if (p$polar) {
            r <- y*2*pi/p$degrees # y are angles in degrees
            y <- x*sin(r) # x are radii
            x <- x*cos(r)
        }
        s <- paste(paste0('(',round(x,3),',',round(y,3),')'),collapse='')
    } else {
## for pc* node variants where x and y may be node names
        s <- paste0('(',x,')(',y,')')
    }
    s
}

## for the PSTricks macros, p may be NULL so that the constructed string is
## is returned by ppbuild

#' Construct `pstricks` Macro Command
#' @param psname The name of the macro command to construct.
#' @param x,y Coordinates.
#' @param opt Optional parameters.
#' @param arg,arg1,arg2,arg3,arg4,arg0 Arguments.
#' @param star Flag to indicate starred version.
#' @param p The PSTricks object.
#' @return The string or an updated PSTricks object.
#' @export
#' @examples
#' ppbuild("ppbuild",1,2,"opt","arg","arg1","arg2","arg3","arg4","arg0",TRUE)

ppbuild <- function(psname, x=NULL, y=NULL, opt=NULL,
                    arg=NULL, arg1=NULL, arg2=NULL, arg3=NULL, arg4=NULL, arg0=NULL,
                    star=FALSE, p=NULL)
{
    if (isa(p,"PSTricks") || is.null(p)) {

        psname <- cvtname(psname)
        s <- paste0("\\",psname,
                    sifelse(star,'*',''),
                    pparg(arg0),
                    ppopt(opt),
                    pparg(arg),
                    ppcoords(p,x,y),
                    pparg(arg1),
                    pparg(arg2),
                    pparg(arg3),
                    pparg(arg4))
        if (is.null(p)) {
            return(s)
        } else {
            return(ppappend(p,s))
        }
    }

    stop(paste0(psname,": first argument is not a PSTricks or NULL object"))
}

#' Construct `pstricks` Macro Coordinates
#' @param p The PSTricks object.
#' @param x,y,z Coordinates.
#' @return Coordinates string (using parentheses), or empty string if `x` or `y` or `z` is `NULL`.
#' @export

ppcoords3D <- function(p=NULL,x,y,z)
{
    if (is.null(x) || is.null(y) || is.null(z)) return("")

    paste(paste0('(',round(x,3),',',round(y,3),',',round(z,3),')'),collapse='')
}

#' Construct `pstricks` Macro Command
#' @param psname The name of the macro command to construct.
#' @param x,y,z Coordinates.
#' @param opt Optional parameters.
#' @param arg,arg1,arg2,arg3,arg4,arg0 Arguments.
#' @param star Flag to indicate starred version.
#' @param p The PSTricks object.
#' @return The string or an updated PSTricks object.
#' @export
#' @examples
#' ppbuild3D("ppbuild3D",1,2,3,"opt","arg","arg1","arg2","arg3","arg4","arg0",TRUE)

ppbuild3D <- function(psname, x=NULL, y=NULL, z=NULL, opt=NULL,
                    arg=NULL, arg1=NULL, arg2=NULL, arg3=NULL, arg4=NULL, arg0=NULL,
                    star=FALSE, p=NULL)
{
    if (isa(p,"PSTricks") || is.null(p)) {

        psname <- cvtname(psname)
        s <- paste0("\\",psname,
                    sifelse(star,'*',''),
                    pparg(arg0),
                    ppopt(opt),
                    pparg(arg),
                    ppcoords3D(p,x,y,z),
                    pparg(arg1),
                    pparg(arg2),
                    pparg(arg3),
                    pparg(arg4))
        if (is.null(p)) {
            return(s)
        } else {
            return(ppappend(p,s))
        }
    }

    stop(paste0(psname,": first argument is not a PSTricks or NULL object"))
}
