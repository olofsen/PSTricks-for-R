#' Set Overall Margin
#' @param p The PSTricks object.
#' @param margin Parameter that determines the layout of a graph.
#' @param mrgaxes A factor for the margins between the axes.
#' @return The updated PSTricks object with respect to the attributes `margin` and `mrgaxes`.
#' @seealso [ppgrid()] for an example.
#' @export

ppsetmargins <- function(p, margin=1, mrgaxes=1)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$margin <- margin
    p$mrgaxes <- mrgaxes
    p
}

#' Set Line Width
#' @details
#' Parameter linewidth is a special one because it is needed at some
#' places for proper alignment (`geom_frame()`,`geom_hist()`,`ppgrid()`,`pplegend()`,`cx()`,`cy()`,`endP2E()`).
#' @param p The PSTricks object.
#' @param linewidth The new default line width in mm.
#' @return The updated PSTricks object.
#' @seealso [geom_set()] for an example.
#' @export

pplinewidth <- function(p, linewidth) {
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$linewidth <- linewidth/10
    psset(p, paste0("linewidth=",linewidth,"mm"))
}

#' Set x label separation distance
#' @param p The PSTricks object.
#' @param labsep The distance.
#' @return The updated PSTricks object.
#' @seealso [geom_line()] to view the default distances.
#' @export
#' @examples
#' geom_line(PSTricks(),data=data.frame(x=c(4,0,2),y=c(2,1,0)),
#'     par="linewidth=2pt,linearc=.25,arrows=->") %>%
#'     ppsetxlabsep(1.5) %>% ppsetylabsep(2)

ppsetxlabsep <- function(p, labsep=0.7)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$xlabsep <- labsep
    p
}

#' Set y label separation distance
#' @param p The PSTricks object.
#' @param labsep The distance.
#' @return The updated PSTricks object.
#' @seealso [ppsetxlabsep()] for an example.
#' @export

ppsetylabsep <- function(p, labsep=1)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$ylabsep <- labsep
    p
}

## aliases

#' @rdname ppsetxlabsep
#' @export
ppxlabsep <- ppsetxlabsep

#' @rdname ppsetylabsep
#' @export
ppylabsep <- ppsetylabsep

#' Set Flag to use PSTtoEPS Feature
#' @details
#' The PSTtoEPS feature is explained in the original manual in section 55.
#' It may be used for efficient EPS file processing, in particular in cases
#' where TeX's capacity becomes exceeded with many plotting commands.
#' It is needed only for the "latex" engine; "xelatex" and "lualatex" do not
#' handle it properly.
#' The "pstpkgs="pst-eps" must be used when creating the `PSTricks()` object.
#' @param p The PSTricks object.
#' @param psttoeps A flag to indicate that the PSTtoEPS feature should be used with geoms.
#' @export

ppsetpsttoeps <- function(p, psttoeps=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$psttoeps <- psttoeps
    p
}

#' Set Flag to use Logarithmic X Axis
#' @param p The PSTricks object.
#' @param logx The flag.
#' @return The updated PSTricks object.
#' @export

ppsetlogx <- function(p, logx=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logx <- logx
    p
}

#' Set Flag to use Logarithmic Y Axis
#' @param p The PSTricks object.
#' @param logy The flag.
#' @return The updated PSTricks object.
#' @export

ppsetlogy <- function(p, logy=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logy <- logy
    p
}

#' Set Flags to use Logarithmic X and Y Axes
#' @param p The PSTricks object.
#' @param logxy The flag.
#' @return The updated PSTricks object.
#' @seealso [geom_abline()] for an example.
#' @export

ppsetlogxy <- function(p, logxy=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logx <- p$logy <- logxy
    p
}

#' Reset Flag to use Logarithmic X Axis
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export

ppsetnologx <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logx <- FALSE
    p
}

#' Reset Flag to use Logarithmic Y Axis
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export

ppsetnology <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logy <- FALSE
    p
}

#' Reset Flags to use Logarithmic X and Y Axes
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @export

ppsetnologxy <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$logx <- p$logy <- FALSE
    p
}

#' Set Flag to use Primary X Axis
#' @param p The PSTricks object.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @export

ppsetprimaryx <- function(p, secondary=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$secondx <- secondary
    p
}

#' Set Flag to use Primary Y Axis
#' @param p The PSTricks object.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @export

ppsetprimaryy <- function(p, secondary=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$secondy <- secondary
    p
}

#' Set Flag to use Primary X or Y Axis
#' @param p The PSTricks object.
#' @param xory A character 'x' or 'y' designating the axis.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @export

ppsetprimary <- function(p, xory, secondary=FALSE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (xory == 'x') {
        p$secondx <- secondary
    } else {
        p$secondy <- secondary
    }
    p
}

#' Set Flag to use Secondary X Axis
#' @param p The PSTricks object.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @seealso [ppgeoms()] for an example.
#' @export

ppsetsecondaryx <- function(p, secondary=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$secondx <- secondary
    p
}

#' Set Flag to use Secondary Y Axis
#' @param p The PSTricks object.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @seealso [ppgeoms()] for an example.
#' @export

ppsetsecondaryy <- function(p, secondary=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$secondy <- secondary
    p
}

#' Set Flag to use Secondary X or Y Axis
#' @param p The PSTricks object.
#' @param xory A character 'x' or 'y' designating the axis.
#' @param secondary The flag.
#' @return The updated PSTricks object.
#' @seealso [ppgeoms()] for an example.
#' @export

ppsetsecondary <- function(p, xory, secondary=TRUE)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    if (xory == 'x') {
        p$secondx <- secondary
    } else {
        p$secondy <- secondary
    }
    p
}

#' Set Interpretation of Coordinates to Polar
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @seealso [degrees()] and [ppsetcartesian()], and [psarcn()] for an example.
#' @export

ppsetpolar <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$polar <- TRUE
    p
}

#' Set Interpretation of Coordinates to Cartesian
#' @param p The PSTricks object.
#' @return The updated PSTricks object.
#' @seealso [ppsetpolar()].
#' @export

ppsetcartesian <- function(p)
{
    if (!isa(p,"PSTricks")) stop("first argument is not a PSTricks object")
    p$polar <- FALSE
    p
}
