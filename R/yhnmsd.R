#' Descriptive statistics of response variable by group and timepoint
#'
#' @param data    (dataframe) dataframe of the study
#' @param mydv    (string)    response variable
#' @param dtpoint (string)    discrete timepoints
#' @param tgroup  (string)    treatment group variable
#' @param est_d   (numeric)   number of decimal places for point estimate
#' @param ci_d    (numeric)   number of decimal places for CI
#' @param propcut (numeric)   cutpoint to create proportions (descriptive purposes)
#'
#' @return a dataframe
#' @export
yhnmsd <- function(data, mydv, dtpoint, tgroup, est_d = 2, ci_d = 2, propcut){

  # yhnmsd summarizes RCT results
  # estimate n() and mean-sd/prop by treatment group (tgroup) and discrete timepoints (dtpoint)
  # yhnmsd(data = mtcars, mydv = "qsec", dtpoint = "am", tgroup = "vs")


  options(dplyr.summarise.inform = FALSE)

  if(!is.character(mydv)) mydv <- deparse(substitute(mydv))


  df_groupby <-
    data %>%
    dplyr::select(all_of(dtpoint), all_of(tgroup), all_of(mydv) ) %>%
    group_by(.data[[dtpoint]], .data[[tgroup]])


  if( missing(propcut) || is.na(propcut)   ) {
    ## compute mean and SD

    nmsd <-
      df_groupby %>%
      summarise(across(!!mydv, ~list(
        c( sum(!is.na(.)), smean.sd(.x) ) %>%            # create vector
          set_names(c("n", "m", "SD")) %>%    # name vector elements
          as_tibble_row))) %>%                # convert output to wide tibble
      unnest_wider(.data[[mydv]]) %>%
      data.frame() %>%                        # so that yhestci() will work
      mutate(myestci1 = yhestci(mydf=., "m", "SD", est_digit = est_d, ci_digit = ci_d)) %>%
      mutate(myestci = str_glue("{myestci1}; {n}")) %>%
      dplyr::select(.data[[dtpoint]], .data[[tgroup]], myestci) %>%
      pivot_wider(names_from = .data[[tgroup]], values_from = myestci) %>%
      mutate(mydv = mydv) %>%
      dplyr::select(mydv, everything())

  } else {

    nmsd <-
      df_groupby %>%
      summarise(across(c(!!mydv), ~ list(yhprop(.x >= propcut))  )) %>%
      unnest_wider(.data[[mydv]]) %>%
      mutate(myestci = str_glue("{num} ({prop}); {total_n}") ) %>%
      dplyr::select(.data[[dtpoint]], .data[[tgroup]], myestci) %>%
      pivot_wider(names_from = .data[[tgroup]], values_from = myestci)
  }


  return(nmsd)
}




#' round variables
#'
#' @param x variable
#' @param digits number of digits
#' @param trail trailing zeros
yhround_sub <- function(x, digits, trail=TRUE){
  ## yhround create either numeric and character vector with specified decimal places
  ## trail = TRUE: character vector with trailing zeros
  ## if missing digits = yhround() will choose appropriate decimal places
  ## 070920: yhround() balks with auto digits + trail = FALSE

  x <- as.numeric(as.character(x))  ## ensure x is numeric
  if ( is.na(x) | is.nan(x) )  return(NA) ## end of function

  ## if digits are missing, auto select digits
  if (missing(digits)) {
    if (x == 0)       digits = 0 else
      if (abs(x) >= 100)  digits = 0 else
        if (abs(x) >= 10 )       digits = 1 else
          if ( abs(x) >= 0.01 )   digits = 2 else digits = 4
  }
  # print(digits)

  x <- ifelse (trail, format(round(x, digits = digits), nsmall = digits),
               as.numeric(format(round(x, digits = digits), nsmall = digits)))

  return(x)
}
yhround <- Vectorize(yhround_sub)


