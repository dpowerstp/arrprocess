#' Order race values before plot
#'
#' Function to order race consistently for graph colors. Dataframe must have a race column with values matching those in race_vector_order. Should run before running plotly.
#'
#' @param df a stops or arrests dataframe with a race column
#' @param race_col a column for race/ethnicity values in the arrest/stops dataframe
#'
#' @return df with race column ordered as factor
#' @export
#'
#' @examples
race_prep <- function(df,
                      race_col = "race",
                      race_vector_order = c("Black",
                                            "Hispanic",
                                            "White",
                                            "Asian",
                                            "Other",
                                            "Native American")){

  # race cols df
  race_vector <- df[[race_col]] %>%
    unique()

  if (!all(race_vector %in% race_vector_order)){
    stop("Error; some race values not found in ordered race vector; re-define ordered race vector to capture all")
  }

  race_present <- race_vector_order[race_vector_order %in% race_vector]

  df <- df %>%
    dplyr::mutate(!!dplyr::sym(race_col) := factor(!!dplyr::sym(race_col), race_present, race_present))

  return(df)
}
