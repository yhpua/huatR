

#' yhestci returns vector with estimate and CI pasted together
#' inspired by https://rdrr.io/github/malcolmbarrett/mbmisc/src/R/formatrs.R
#'
#'
#' @param mydf
#' @param estimate
#' @param lcl
#' @param ucl
#' @param divider
#' @param est_digit
#' @param ci_digit
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>% data.frame() %>% mutate(myestci = yhestci(mydf=mtcars, "hp", "cyl", "mpg", est_digit = 0))
#' est/ci_digits determined by another variable (e.g., cyl)
#' mtcars %>% group_by(am) %>% nest() %>%
#' mutate(myestci = map(data, ~ yhestci(mydf=.x, "hp", "wt", "mpg", est_digit = 0, ci_digit = .x$cyl[[1]] ))) %>%  unnest(myestci)
# for negative estimate with negative CIs, swap lcl and ucl
#' }
#'
#'
#'
#'

yhestci <-
  function(mydf,
           estimate,
           lcl,
           ucl = NULL,
           divider = c(" to "),   ## a - b, a to b, a,b
           est_digit = 2,         ## round value for est
           ci_digit =  2          ## round value for ci
  ) {

    #' yhestci returns vector with estimate and CI pasted together
    #' sometimes does not play nice with tibble + group_by()
    #' yhestci() supersedes yhcorprint()
    #' mtcars %>% data.frame() %>% mutate(myestci = yhestci(mydf=mtcars, "hp", "cyl", "mpg", est_digit = 0))
    #' est/ci_digits determined by another variable (e.g., cyl)
    #' mtcars %>% group_by(am) %>% nest() %>%
    #' mutate(myestci = map(data, ~ yhestci(mydf=.x, "hp", "wt", "mpg", est_digit = 0, ci_digit = .x$cyl[[1]] ))) %>%  unnest(myestci)
    # for negative estimate with negative CIs, swap lcl and ucl
    # https://rdrr.io/github/malcolmbarrett/mbmisc/src/R/formatrs.R  est_ci.R


    ## arg checks
    if (!is.character(estimate)) stop("`estimate` must be of type character")
    if (!is.character(lcl))      stop("`lcl` must be of type character")
    if (!is.null(ucl)) if(!is.character(ucl))      stop("`ucl` must be of type character")

    ## create data-set
    mydata <- mydf %>% dplyr::select (est =   all_of(estimate), lcl =   all_of(lcl))
    if(!is.null(ucl))
      mydata <- mydata %>% bind_cols(mydf %>% dplyr::select (ucl =  all_of(ucl)))


    ## if est, lcl, ucl are negative, swap lcl and ucl
    if(!is.null(ucl)){
      mydata <- mydata %>%
        mutate( lcl_adj  = ifelse(est<0 & lcl<0,  ucl, lcl)) %>%
        mutate( ucl      = ifelse(est<0 & lcl<0,  lcl, ucl)) %>%
        mutate( lcl = lcl_adj) %>%
        dplyr::select(-lcl_adj)
    }


    # reduce2() to round values
    ci_digits <- if(dim(mydata)[[2]]==3) c(ci_digit, ci_digit) else ci_digit

    mydata <-
      mydata %>%
      reduce2(
        names(mydata),
        c(est_digit, ci_digits),
        ~ ..1 %>% mutate(!!..2 := yhround(get(!!..2), ..3)),
        .init = .)


    ## generate estci
    if(is.null(ucl))
      sprintf("%s (%s)", mydata[["est"]], mydata[["lcl"]]) else
        paste0(
          mydata[["est"]],
          " (",
          mydata[["lcl"]],
          divider,
          mydata[["ucl"]],
          ")"
        )

  }



