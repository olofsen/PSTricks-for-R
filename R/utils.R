#' Conditional Object Selection
#' @details
#' This is like `ifelse`, but for a scalar test, and any object may be returned.
#' @param test An object which can be coerced to logical mode.
#' @param rt Return value if `test` is true.
#' @param rf Return value if `test` is false.
#' @return Appropriate return value.
#' @export

sifelse <- function(test, rt, rf)
{
    if (test) return(rt) else return(rf)
}

#' Merge Two Lists
#' @param x The first list.
#' @param y The second list, used to add missing elements in the first list.
#' @param ... Not used.
#' @return The merged lists.
#' @export
#' @examples
#' merge(list(a=3,b=4),list(a=30,c=40))

merge.list <- function(x, y, ...)
{
    idx <- !(names(y) %in% names(x))
    c(x, y[idx])
}

## Convert `match.call()[[1]]` result to string,
## removing namespace name if present
## for geoms

cvtname <- function(name)
{
    name <- as.character(name)
    n <- length(name)
    if (n > 1) name <- name[n]
    name
}
