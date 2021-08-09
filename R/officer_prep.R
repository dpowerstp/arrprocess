#' function to re-factor anonymous officer values by values present in given dataset
#'
#' @param df df with officer_anon and officer_order columns
#'
#' @return df with officer names ordered by order
#' @export
#'
#' @examples
officer_prep <- function(df, officer_anon_df){

  officer_unique <- df[["officer_anon"]] %>%
    unique()

  officer_anon_present <- officer_anon_df %>%
    dplyr::filter(officer_anon %in% officer_unique) %>%
    dplyr::arrange(officer_order) %>%
    dplyr::pull(officer_anon)

  df <- df %>%
    dplyr::mutate(officer_anon = factor(officer_anon, officer_anon_present, officer_anon_present))

  return(df)
}
