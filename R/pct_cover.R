#' Percent cover
#' @description Calculate the percent cover by plot for variables or
#'   combinations of variables. Percent cover will be calculated for every
#'   combination of the variables requested, so if the variables are
#'   \code{GrowthHabitSub} and \code{Duration} then the output will contain
#'   fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual},
#'   \code{Shrub.Perennial}, etc. whereas using just the variable \code{code}
#'   will produce one column per species code. Any number of indicator variables
#'   can be used. These are calculated as cover from anywhere in the canopy
#'   column or as only the first hit in the canopy column. Any groupings where
#'   all the variable values were \code{NA} will be dropped.
#' @param lpi_tall A tall/long-format data frame. Use the data frame
#'   \code{"layers"} from the \code{gather.lpi()} output.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall
#'   rather than wide and will not have observations for non-existent values
#'   e.g., if no data fell into a group on a plot, there will be no row for that
#'   group on that plot. Defaults to \code{FALSE}.
#' @param hit Character string. If \code{"any"} then percent cover will be
#'   calculated using any hit in the canopy column (so a single pin drop record
#'   may be counted more than once if it had hits that corresponded to different
#'   groups). If \code{"first"} then only the first canopy hit at a pin drop
#'   will be used to calculate cover.  If \code{"basal"}, then only the soil
#'   surface hit will be used to calculate cover. Defaults to \code{"any"}.
#' @param by_year Logical. If \code{TRUE} then results will be reported further
#'   grouped by year using the \code{FormDate} field from the data forms.
#'   Defaults to \code{FALSE}.
#' @param by_line Logical. If \code{TRUE} then results will be reported further
#'   grouped by line using the \code{LineID} and \code{LineKey} fields from the
#'   data forms. Defaults to \code{FALSE}.
#' @param by_sampleperiod Numeric >=\code{0} indicating the number of days in
#'   the desired sampling window. If >\code{0}, results will be reported further
#'   grouped by sampling period, where \code{FormDate} values for each plot that
#'   fall within the given number of days of each other are considered a single
#'   sampling period. If =\code{0}, no sample period grouping is performed.
#'   Defaults to \code{0}.
#' @param ... Optional character strings. One or more variable name from
#'   \code{lpi_tall} to calculate percent cover for, e.g.
#'   \code{"GrowthHabitSub"} to calculate percent cover by growth habits or
#'   \code{"GrowthHabitSub", "Duration"} to calculate percent cover for
#'   categories like perennial forbs, annual graminoids, etc.
#' @export

pct_cover <- function(lpi_tall,
                      tall = FALSE,
                      hit = "any",
                      by_year = FALSE,
                      by_sampleperiod = 0, # set to an integer number of days to define a sample period
                      # e.g. by_sampleperiod = 14 would consider any sampling within 14 days of a previous
                      # sampling date to fall within one sampling period.
                      # by_sampleperiod=0 does not group by sampling period
                      by_line = FALSE,
                      ...) {
  ## Get a list of the variables the user wants to group by.
  grouping_variables <- rlang::quos(...)

  if (!is.data.frame(lpi_tall)) {
    stop("lpi_tall must be a data frame.")
  }

  if (!(hit %in% c("any", "first", "basal"))) {
    stop("hit must be either 'any','first' or 'basal'.")
  }

  if (by_year & by_sampleperiod!=0) {
    stop("Cannot use both by_year and by_sampleperiod. Set either by_year = TRUE
         and by_sampleperiod = 0 to group by year, or by_year = FALSE and by_sampleperiod
         as an integer number of days greater than 0")
  }

  if (by_sampleperiod<0) {
    stop("Sample period must be a number of days >=0")
  }

  # Create a year variable to group by for by_year = T
  if(by_year){
    lpi_tall$Year <- lubridate::year(lpi_tall$FormDate)
  }

  # Create a sample period variable to group by for by_sampleperiod = T
  if(by_sampleperiod!=0){
    lpi_sampkeys <- lpi_tall %>%
      dplyr::select(PrimaryKey, FormDate) %>%
      dplyr::distinct() %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::arrange(FormDate) %>%
      dplyr::mutate(DaysSincePrev = FormDate - dplyr::lag(x=FormDate, n=1)) %>%
      dplyr::mutate(SamplePeriodStart = ifelse(test = DaysSincePrev>by_sampleperiod|is.na(DaysSincePrev), # NA values indicate the first sampling date for a given plot
                                   yes = as.character(FormDate),
                                   no = NA)) %>%
      tidyr::fill(., SamplePeriodStart, .direction = "down")

    lpi_tall <- left_join(lpi_tall, select(lpi_sampkeys, -DaysSincePrev), by = c("PrimaryKey", "FormDate"))
  }

  # For how deep to group. Always by plot, sometimes by line, sometimes by year, sometimes by sample period
  if (by_line & !by_year & by_sampleperiod==0) {
    level <- rlang::quos(PrimaryKey, LineKey)
    join_fields <- c("PrimaryKey", "LineKey")
  }
  if(by_line & by_year){
    level <- rlang::quos(PrimaryKey, LineKey, Year)
    join_fields <- c("PrimaryKey", "LineKey", "Year")
  }
  if(by_line & by_sampleperiod!=0){
    level <- rlang::quos(PrimaryKey, LineKey, SamplePeriodStart)
    join_fields <- c("PrimaryKey", "LineKey", "SamplePeriodStart")
  }
  if(!by_line & by_year){
    level <- rlang::quos(PrimaryKey, Year)
    join_fields <- c("PrimaryKey", "Year")
  }
  if(!by_line & by_sampleperiod!=0){
    level <- rlang::quos(PrimaryKey, SamplePeriodStart)
    join_fields <- c("PrimaryKey", "SamplePeriodStart")
  }
  if(!by_line & !by_year & by_sampleperiod==0) {
    level <- rlang::quos(PrimaryKey)
    join_fields <- c("PrimaryKey")
  }

  # Drop data where there is no code value
  # (i.e. layers where there was no recorded hit)
  lpi_tall <- dplyr::filter(
    .data = lpi_tall,
    !is.na("code"),
    code != "",
    code != "None",
    code != "N",
    !is.na("PrimaryKey"),
    !is.na("LineKey"),
    code != "<NA>"
  )

  # Convert all codes to upper case
  lpi_tall$code <- toupper(lpi_tall$code)

  lpi_tall <- lpi_tall %>%
    dplyr::mutate_at(dplyr::vars(!!!grouping_variables), toupper)

  # Within a plot, we need the number of pin drops, which we'll calculate
  # taking the unique combination of PrimaryKey, LineKey and Point number
  # for each group level
  if(by_year){
    point_totals <- dplyr::distinct(
      .data = lpi_tall,
      PrimaryKey, LineKey, PointNbr, Year
    ) %>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(point_count = dplyr::n())
  }
  if(by_sampleperiod!=0){
    point_totals <- dplyr::distinct(
      .data = lpi_tall,
      PrimaryKey, LineKey, PointNbr, SamplePeriodStart
    ) %>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(point_count = dplyr::n())
  }
  if(!by_year & by_sampleperiod==0){
    point_totals <- dplyr::distinct(
      .data = lpi_tall,
      PrimaryKey, LineKey, PointNbr
    ) %>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(point_count = dplyr::n())
  }

  # Add the point_counts field
  # (it'll be the same for every record associated with a plot)
  lpi_tall <- dplyr::left_join(
    x = lpi_tall,
    y = point_totals,
    by = join_fields
  )

  # make sure layer is a character field
  lpi_tall$layer <- as.character(lpi_tall$layer)

  # Get the layers into the correct order
  lpi_tall <- dplyr::mutate(
    .data = lpi_tall,
    layer = factor(layer,
                   levels = c(
                     "TopCanopy",
                     unique(lpi_tall$layer)[grepl(unique(lpi_tall$layer),
                                                  pattern = "^Lower[1-7]")],
                     "SoilSurface"
                   )
    )
  ) %>% dplyr::arrange(layer)

  if (hit == "basal") {
    hit <- "any"
    lpi_tall <- dplyr::filter(
      .data = lpi_tall,
      layer == "SoilSurface"
    )
  }


  summary <- switch(hit,
                    "any" = {
                      summary <- lpi_tall %>%
                        # Remove records where there are NAs for the grouping variables
                        dplyr::filter(complete.cases(!!!grouping_variables)) %>%
                        {if(by_year){ dplyr::group_by(.,
                          PrimaryKey, LineKey, PointNbr, Year, point_count,
                          !!!grouping_variables)
                        }else if(by_sampleperiod!=0){ dplyr::group_by(.,
                           PrimaryKey, LineKey, PointNbr, SamplePeriodStart, point_count,
                           !!!grouping_variables)
                          }else if(!by_year & by_sampleperiod==0){
                          dplyr::group_by(.,
                            PrimaryKey, LineKey, PointNbr, point_count,
                            !!!grouping_variables)
                          }
                        }%>%
                        ## Here's the breakdown of the gnarly parts:
                        # Because this is a tall format, we want just
                        # presence/absence for the indicator at a given point
                        # so we'll write in 1 if any of the layers within that indicator
                        # has a non-NA and non-"" value
                        dplyr::summarize(present = dplyr::if_else(any(!is.na(code) &
                                                                        code != ""), 1, 0)) %>%
                        tidyr::unite(indicator, !!!grouping_variables, sep = ".") %>%
                        dplyr::ungroup() %>%
                        dplyr::group_by(!!!level, indicator) %>%
                        # Within a plot, find the sum of all the "presents"
                        # then divide by the number of possible hits, which
                        # we added in point_count
                        dplyr::summarize(percent = 100 * sum(present, na.rm = TRUE) / dplyr::first(point_count))
                    },
                    "first" = {
                      summary <- lpi_tall %>%
                        # Remove records where there are NAs for the grouping variables
                        dplyr::filter(complete.cases(!!!grouping_variables))%>%
                        # Strip out all the non-hit codes
                        dplyr::filter(!(code %in% c("", NA, "None", "N"))) %>%
                        {if(by_year){ dplyr::group_by(., PrimaryKey, LineKey, PointNbr, Year, point_count)
                        }else if(by_sampleperiod!=0){ dplyr::group_by(., PrimaryKey, LineKey, PointNbr, SamplePeriodStart, point_count)
                          }else if(!by_year & by_sampleperiod==0){
                          dplyr::group_by(., PrimaryKey, LineKey, PointNbr, point_count)
                            }
                        }%>%
                        # Get the first hit at a point
                        dplyr::summarize(code = dplyr::first(code)) %>%
                        # Get all the other fields back
                        merge(
                          x = dplyr::distinct(dplyr::select(lpi_tall,
                                                            all_of(join_fields),
                                                            #"PrimaryKey",
                                                            "LineKey",
                                                            "PointNbr",
                                                            "code",
                                                            !!!grouping_variables)),
                          y = .,
                          all.y = TRUE
                        ) %>%
                        tidyr::unite(indicator,
                                     !!!grouping_variables,
                                     sep = ".") %>%
                        dplyr::ungroup() %>%
                        dplyr::group_by(!!!level, indicator) %>%
                        dplyr::summarize(percent = 100 * dplyr::n() / dplyr::first(point_count)) %>%
                        dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
                    }
  )

  # remove rows with no grouping applied
  summary <- subset(summary, indicator != ".")

  # add zeros where no cover occurred
  if(by_line & !by_year & by_sampleperiod==0){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          LineKey = unique(lpi_tall$LineKey),
                          indicator = unique(summary$indicator))
  }
  if(by_line & by_year){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          LineKey = unique(lpi_tall$LineKey),
                          Year = unique(lpi_tall$Year),
                          indicator = unique(summary$indicator))
  }
  if(by_line & by_sampleperiod!=0){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          LineKey = unique(lpi_tall$LineKey),
                          SamplePeriodStart = unique(lpi_tall$SamplePeriodStart),
                          indicator = unique(summary$indicator))
  }
  if(!by_line & by_year){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          Year = unique(lpi_tall$Year),
                          indicator = unique(summary$indicator))
  }
  if(!by_line & by_sampleperiod!=0){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          SamplePeriodStart = unique(lpi_tall$SamplePeriodStart),
                          indicator = unique(summary$indicator))
  }
  if(!by_line & !by_year & by_sampleperiod==0){
    expgrd <- expand.grid(PrimaryKey = unique(lpi_tall$PrimaryKey),
                          indicator = unique(summary$indicator))
  }
  summary <- suppressWarnings(
    expgrd %>%
      dplyr::left_join(., summary) %>%
      dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))))

  # Remove indicators that have incomplete grouping variable combinations
  summary <- summary %>% subset(!grepl(
    x = indicator,
    pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."
  ))

  if (!tall) {
    summary <- tidyr::spread(summary, key = indicator, value = percent) %>%
      # Replace the NA values with 0s because they represent 0% cover for that indicator
      tidyr::replace_na(replace = setNames(
        as.list(rep.int(0,
                        # Make a list of 0s named with the newly-created field names for replace_na()
                        times = length(unique(names(.)[!(names(.) %in% c("PrimaryKey",
                                                                         "PlotKey",
                                                                         "PlotID",
                                                                         "LineKey",
                                                                         "LineID"))]))
        )),
        unique(names(.)[!(names(.) %in% c("PrimaryKey", "LineKey"))])
      ))
  }

  return(summary)
}


