#' Recode age to range
#'
#' Recodes numeric age value to age range
#'
#' @param num a numeric age value
#'
#' @return string
#' @export
#'
#' @examples
#' age_lookup(16)
age_lookup <- function(num, traffic = T){

  if (!is.numeric(num)){
    stop("Error; non-numeric input")
  }

  # some error values in ages
  if (traffic == T){
    string <- dplyr::case_when(is.na(num) | num < 15 | num >= 100 ~ "Missing",
            num < 20 ~ "15-19",
            num < 30 ~ "20-29",
            num < 40 ~ "30-39",
            num < 50 ~ "40-49",
            num < 60 ~ "50-59",
            num < 70 ~ "60-69",
            num >= 70 ~ "70 or higher")
  }

  else if (traffic == F){
    string <- dplyr::case_when(is.na(num) | num < 10 | num >= 100 ~ "Missing",
                     num < 18 ~ "10-17",
                     num < 30 ~ "18-29",
                     num < 40 ~ "30-39",
                     num < 50 ~ "40-49",
                     num < 60 ~ "50-59",
                     num < 70 ~ "60-69",
                     num >= 70 ~ "70 or higher")
  }

  return(string)
}
