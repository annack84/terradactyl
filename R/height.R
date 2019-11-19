#' Calculate the vegetation height
#' @param height_tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param omit_zero Logical. If \code{TRUE} the results omit height measurements of \code{0}. Defaults to \code{FALSE}.
#' @param method Character string. Indicates the type of indicator, \code{"max"}, which yields the average maximum height (of the herbaceous or woody heights) on the plot or \code{"mean"} which yields the mean height by functional group (woody/herbaceous).
#' @param by_line Logical. If \code{TRUE} then the results will be calculated on a per-line basis. If \code{FALSE} then the results will be calculated on a per-plot basis. Defaults to \code{FALSE}.
#' @param ... Optional bare variable names. One or more variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @export mean_height

mean_height <- function(height_tall,
                        method = "mean",
                        omit_zero = TRUE,
                        by_line = FALSE,
                        by_year = FALSE,
                        by_sampleperiod = 0,
                        tall = FALSE,
                        ...) {
  ## Get a list of the variables the user wants to group by.
  grouping_variables <- rlang::quos(...)

  if (!is.data.frame(height_tall)) {
    stop("height_tall must be a data frame.")
  }

  if (!(method %in% c("mean", "max"))) {
    stop("method must be either 'mean' or 'max'.")
  }
  if (by_year & by_sampleperiod!=0) {
    stop("Cannot use both by_year and by_sampleperiod. Set either by_year = TRUE
         and by_sampleperiod = 0 to group by year, or by_year = FALSE and by_sampleperiod
         as an integer number of days greater than 0")
  }

  if (by_sampleperiod<0) {
    stop("Sample period must be a number of days >=0")
  }

  # add Year column if grouping by year
  if(by_year){
    height_tall$Year <- lubridate::year(height_tall$FormDate)
  }

  # Create a sample period variable to group by for by_sampleperiod = T
  if(by_sampleperiod!=0){
    height_sampkeys <- height_tall %>%
      dplyr::select(PrimaryKey, FormDate) %>%
      dplyr::distinct() %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::arrange(FormDate) %>%
      dplyr::mutate(DaysSincePrev = FormDate - dplyr::lag(x=FormDate, n=1)) %>%
      dplyr::mutate(SamplePeriodStart = ifelse(test = DaysSincePrev>by_sampleperiod|is.na(DaysSincePrev), # NA values indicate the first sampling date for a given plot
                                               yes = as.character(FormDate),
                                               no = NA)) %>%
      tidyr::fill(., SamplePeriodStart, .direction = "down")

    height_tall <- left_join(height_tall, select(height_sampkeys, -DaysSincePrev), by = c("PrimaryKey", "FormDate"))
  }

  # For how deep to group. Always by plot, sometimes by line, sometimes by year, sometimes by sample period
  if (by_line & !by_year & by_sampleperiod==0) {
    level <- rlang::quos(PrimaryKey, LineKey)
  }
  if(by_line & by_year){
    level <- rlang::quos(PrimaryKey, LineKey, Year)
  }
  if(by_line & by_sampleperiod!=0){
    level <- rlang::quos(PrimaryKey, LineKey, SamplePeriodStart)
  }
  if(!by_line & by_year){
    level <- rlang::quos(PrimaryKey, Year)
  }
  if(!by_line & by_sampleperiod!=0){
    level <- rlang::quos(PrimaryKey, SamplePeriodStart)
  }
  if(!by_line & !by_year & by_sampleperiod==0) {
    level <- rlang::quos(PrimaryKey)
  }

  # If height of zero is dropped by the calculation, filter out zeros
  if (omit_zero) {
    height_tall <- dplyr::filter(height_tall, Height != 0)
  }

  # Calculate mean height by grouping variable, if method == "mean"
  if (method == "mean") {
    summary <- height_tall %>%
      dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(
        !!!level,
        !!!grouping_variables
      ) %>%
      dplyr::summarize(mean_height = mean(as.numeric(Height))) %>%
      tidyr::unite(indicator,
        !!!grouping_variables,
        sep = "."
      ) %>%
      dplyr::ungroup()

    summary <- summary[!grepl(summary$indicator, pattern = "NA.|.NA"), ]
  }
  # Calculate the max height by grouping variable, if method =="max"
  if (method == "max") {
    height_tall_spread <- height_tall %>% tidyr::spread(
      key = type,
      value = Height
    )
    height_tall_spread$max <- pmax(height_tall_spread$herbaceous,
      height_tall_spread$woody,
      na.rm = TRUE
    )
    summary <- height_tall_spread %>%
      dplyr::group_by(!!!level, !!!grouping_variables) %>%
      dplyr::summarize(max_height = mean(max)) %>%
      dplyr::filter(!grepl(max_height, pattern = "^[NA.]{0,100}NA$")) %>%
      dplyr::ungroup()
  }


  # Convert to wide format
  if (!tall) {
    summary <- summary %>% tidyr::spread(key = indicator, value = mean_height, fill = 0)
  }

  return(summary)
}
