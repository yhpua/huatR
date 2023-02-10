
#' compute prop with Wilson 95pct for continuous and ordinal variables
#'
#' @param y1
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' yhprop (mtcars$hp >200)
#' yhprop (mtcars$cyl>=6)
#' }
#'
#'
yhprop <- function(y1) {

  # compute prop with Wilson 95% for continuous and ordinal variables
  # yhprop is exactly the same as yhsum1 (July 2022)
  # yhprop (mtcars$hp >200)
  # yhprop (mtcars$cyl>=6)

  # ensure logical and numeric (0,1)
  # ensure binary if numeric
  y <- y1[!is.na(y1)]  ## remove NAs
  y <- if(is.factor(y1)) as.numeric(y1)-1 else y  ## convert factor to (0, 1)
  if(!is.logical(y) & !all( y %in% 0:1))  stop("supply binary (0,1) var")

  n <- length(y)
  # p <- mean(y)
  # se <- sqrt(p * (1. - p) / n)

  tibble(
    binconf(sum(y), n,
            include.x= T,
            include.n=T,
            return.df = TRUE) %>% round(2) ) %>%
    rename(num = 1, total_n=2,
           prop=3, prop_lcl=4, prop_ucl = 5 ) %>%
    mutate(across(contains("prop"), ~ yhround(., 2)))

}
