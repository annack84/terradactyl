#' Line-point intercept indicator calculations
#' @description Calculate the percent cover by plot, and optionally line and year,
#' for ground cover, total foliar cover, and a suite of plant traits including
#' photosynthetic pathway, noxious status, duration, growth habit sub (e.g. forb,
#' grass, shrub), growth habit (woody and non-woody), and native status. Returns
#' values for all LPI hits and first LPI hits. Different from \code{lpi_calc()}
#' because it includes native species status and photosynthetic pathway in the
#' indicators calculated, and because it allows the user to input a separate
#' species file.
#' @param header Data frame. Use the data frame from the \code{header_build()}
#' output. Must be a .Rdata file.
#' @param lpi_tall Character. File path to a tall/long-format data frame. Use the data frame
#' from the \code{gather_lpi()} output. Must be a .Rdata file.
#' @param species_file Character. The full file path (including file extension)
#' to the file containing the species list
#' @param source Character. Specifies data source, "AIM", "LMF", "I&M", "DIMA"
#' @param overwrite_generic_species Logical. Updates attributes of generic species
#' codes to those contained in tblSpeciesGeneric.Defaults to \code{FALSE}.
#' @param by_year Logical. If \code{TRUE} then results will be reported further
#' grouped by year using the \code{FormDate} field from the data forms.
#' Defaults to \code{TRUE}.
#' @param by_sampleperiod Numeric >=\code{0} indicating the number of days in
#' the desired sampling window. If >\code{0}, results will be reported further
#' grouped by sampling period, where \code{FormDate} values for each plot that
#' fall within the given number of days of each other are considered a single
#' sampling period. If =\code{0}, no sample period grouping is performed.
#' Defaults to \code{0}.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Calculate the LPI indicators
#' @export lpi_calc_usgs
#' @rdname usgs_specialty_functions
lpi_calc_usgs <- function(header,
                          lpi_tall,
                          species_file,
                          source,
                          overwrite_generic_species=FALSE,
                          by_year=TRUE,
                          by_sampleperiod = 0) {

  # Join the lpi data to the header PrimaryKeys and add the StateSpecies Key
  lpi_tall_header <- readRDS(lpi_tall) %>%
    dplyr::left_join(dplyr::select(
      header,
      "PrimaryKey",
      "DBKey",
      "SpeciesState"
    ),
    .,
    by = c("PrimaryKey", "DBKey")
    )

  # Join to the state species list via the SpeciesState value
  lpi_species <- species_join(
    data = lpi_tall_header,
    species_file = species_file,
    overwrite_generic_species = overwrite_generic_species
  ) %>%
    dplyr::distinct()

  # Correct the Non-Woody to NonWoody
  lpi_species$GrowthHabit[grepl(
    pattern = "Non-woody|Nonwoody|Non-Woody",
    x = lpi_species$GrowthHabit
  )] <- "NonWoody"

  # Correct the Sub-shrub to SubShrub
  lpi_species$GrowthHabitSub[grepl(
    pattern = "Sub-Shrub|subshrub|Sub-shrub|Subshrub",
    x = lpi_species$GrowthHabitSub
  )] <- "SubShrub"

  lpi_species$Noxious[grepl(
    pattern = "NO",
    x = lpi_species$Noxious
  )] <- "NonNox"

  lpi_species$Noxious[grepl(
    pattern = "YES",
    x = lpi_species$Noxious
  )] <- "Nox"

  # Calculate Total Foliar Cover ----
  total_foliar <- pct_cover_total_foliar(
    lpi_tall = lpi_species,
    tall = TRUE,
    by_year = by_year,
    by_sampleperiod = by_sampleperiod
  )

  # Calculate between plant cover (includes bare soil) ----
  between.plant.cover <- pct_cover_between_plant(
    lpi_tall = lpi_species,
    by_year = by_year,
    by_line = FALSE,
    tall = TRUE,
    by_sampleperiod = by_sampleperiod
  )

  # Clean up indicator names so they are compatible with the AIM.gdb schema

  # Assign string replacements
  between.plant.replace <- c(
    "AL" = "HerbLitter",
    "\\bL\\b" = "HerbLitter",
    "HL" = "HerbLitter",
    "AM" = "HerbLitter",
    "DN" = "HerbLitter",
    "ER" = "HerbLitter",
    "HT" = "NonVegLitter",
    "NL" = "NonVegLitter",
    "DS" = "DepSoil",
    "\\bD\\b" = "Duff",
    "LC" = "Lichen",
    "\\bM\\b" = "Moss",
    "WL" = "WoodyLitter",
    "CY" = "Cyanobacteria",
    "EL" = "EmbLitter",
    "\\bW\\b" = "Water",
    "WA" = "Water",
    "RF" = "Rock",
    "\\bR\\b" = "Rock",
    "GR" = "Rock",
    "CB" = "Rock",
    "ST" = "Rock",
    "BY" = "Rock",
    "VL" = "VagrLichen",
    "AG" = "BareSoil",
    "CM" = "BareSoil",
    "LM" = "BareSoil",
    "FG" = "BareSoil",
    "PC" = "BareSoil",
    "BR" = "Bedrock",
    "\\bS\\b" = "BareSoil",
    "[[:punct:]]" = ""
  )

  # Perform replacements
  between.plant.cover <- between.plant.cover %>%
    # Substitute the field names for those that are human readable
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., between.plant.replace)) %>%

    # Add FH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = "")) %>%

    # Remove "FH_" from the BareSoilCover indicator
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace(., "FH_BareSoilCover", "BareSoilCover"))

  # Because the renaming processing lumps categories,
  # we need to get a summed value (e.g., Soil =S+FG+LM_CM+AG)
  if(!by_year & by_sampleperiod==0){
    between.plant.cover <- between.plant.cover %>%
      dplyr::group_by(PrimaryKey, indicator) %>%
      dplyr::summarise(percent = sum(percent))

    # Add a Total Litter Indicator
    between.plant.cover <- between.plant.cover %>%
      # Filter Litter Indicators
      dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
      # Sum all indicator hits
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(
        indicator = "FH_TotalLitterCover",
        percent = sum(percent)
      ) %>%
      # Add back to the rest of the between plant cover indicators
      dplyr::bind_rows(between.plant.cover, .)
  }
  if(by_year){
    between.plant.cover <- between.plant.cover %>%
      dplyr::group_by(PrimaryKey, Year, indicator) %>%
      dplyr::summarise(percent = sum(percent))

    # Add a Total Litter Indicator
    between.plant.cover <- between.plant.cover %>%
      # Filter Litter Indicators
      dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
      # Sum all indicator hits
      dplyr::group_by(PrimaryKey, Year) %>%
      dplyr::summarize(
        indicator = "FH_TotalLitterCover",
        percent = sum(percent)
      ) %>%
      # Add back to the rest of the between plant cover indicators
      dplyr::bind_rows(between.plant.cover, .)
  }
  if(by_sampleperiod>0){
    between.plant.cover <- between.plant.cover %>%
      dplyr::group_by(PrimaryKey, SamplePeriodStart, indicator) %>%
      dplyr::summarise(percent = sum(percent))

    # Add a Total Litter Indicator
    between.plant.cover <- between.plant.cover %>%
      # Filter Litter Indicators
      dplyr::filter(grepl(pattern = "Litter", x = indicator)) %>%
      # Sum all indicator hits
      dplyr::group_by(PrimaryKey, SamplePeriodStart) %>%
      dplyr::summarize(
        indicator = "FH_TotalLitterCover",
        percent = sum(percent)
      ) %>%
      # Add back to the rest of the between plant cover indicators
      dplyr::bind_rows(between.plant.cover, .)
  }

  # Species Group Cover ----
  # Set the replacement values for valid indicator names ----
  spp.cover.replace <- c(
    "NONWOODY" = "ForbGrass",
    "NON" = "Non",
    "^NO\\." = "NonNox",
    "NO$" = "NonNox",
    "C3NO" = "C3NonNox",
    "C4NO" = "C4NonNox",
    "^YES" = "Nox",
    "C3YES" = "C3Nox",
    "C4YES" = "C4Nox",
    "NOX" = "Nox",
    "ANNUAL" = "Ann",
    "PERENNIAL" = "Peren",
    "[[:punct:]]" = "",
    "GRAMINOID" = "Grass",
    "FORB" = "Forb",
    "NON" = "No",
    "SUBSHRUB" = "SubShrub",
    "SHRUB" = "Shrub",
    "SUCCULENT" = "Succulent",
    "TREE" = "Tree",
    "SEDGE" = "Sedge",
    " " = "",
    "STATURE" = "",
    "SAGEBRUSH" = "Sagebrush",
    "GRASS" = "Grass",
    "SHORT" = "Short",
    "TALL" = "Tall",
    "0" = "Live",
    "1" = "Dead",
    "PREFERRED" = "Preferred",
    "NATIVE" = "Native",
    "INTRODUCED" = "Introduced",
    "NOTPJ" = "NotPJ",
    "WOODY" = "Woody"

  )


  # Any hit cover ----
  ah_spp_group_cover <- dplyr::bind_rows(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, Duration, GrowthHabitSub
    ),
    # cover by Native, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Native, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, GrowthHabitSub
    ),
    # Cover by Native and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway, Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway, Native and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious
    ),
    # Cover by PhotosyntheticPathway status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway
    ),
    # Cover by Native status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native
    )%>% dplyr::mutate(indicator = paste(indicator, ".", sep = "")),


    # Cover by Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, Duration, GrowthHabit
    ),
    # Cover by Native, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Native, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Duration, GrowthHabit
    ),

    # Sage Grouse Groups
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              SG_Group
    ),

    # Pinyon-Juniper Groups
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PinyonJuniper
    ),

    # Cover Duration and GrowthHabit
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "any",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Duration, GrowthHabit
    )
  )


  if (source %in% c("TerrADat", "Survey123", "AIM")) {
    # Sagebrush live or dead
    ah_spp_group_cover <- dplyr::bind_rows(
      ah_spp_group_cover,
      pct_cover(lpi_species,
                tall = TRUE,
                hit = "any",
                by_year = by_year,
                by_line = FALSE,
                by_sampleperiod = by_sampleperiod,
                SG_Group, chckbox
      )
    ) }


  # Fix to indicator names so they are valid for AIM.gdb
  ah_spp_group_cover <- ah_spp_group_cover %>%
    # Substitute "NonNox" for "NO
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("AH_", indicator, "Cover", sep = "") %>%
                    # Change the Sagebrush Live indicator sine it's slightly different
                    stringr::str_replace_all(
                      string = .,
                      pattern = "AH_SagebrushLiveCover",
                      replacement = "AH_SagebrushCover_Live"
                    )) %>%
    # Remove NA cover lines
    dplyr::filter(., !indicator=="AH_NACover")



  # First hit cover ----
  fh_spp_group_cover <- rbind(
    # cover by Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, Duration, GrowthHabitSub
    ),
    # cover by Native, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Noxious, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Native, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, Duration, GrowthHabitSub
    ),
    # cover by PhotosyntheticPathway, Duration, and GrowthHabitSub combination
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Duration, GrowthHabitSub
    ),
    # Add the indicators are only based on Duration and GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Duration, GrowthHabitSub
    ),
    # Cover by GrowthHabitSub only
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              GrowthHabitSub
    ),
    # Cover by Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, GrowthHabitSub
    ),
    # Cover by Native and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway, Noxious and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway, Native and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, GrowthHabitSub
    ),
    # Cover by PhotosyntheticPathway and GrowthHabitSub combo
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, GrowthHabitSub
    ),
    # Cover by Noxious status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious
    ),
    # Cover by Native status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native
    ),
    # Cover by PhotosyntheticPathway status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway
    ),
    # Cover by Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Noxious, Duration, GrowthHabit
    ),
    # Cover by Native, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              Native, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Noxious, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Noxious, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Native, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Native, Duration, GrowthHabit
    ),
    # Cover by PhotosyntheticPathway, Duration, GrowthHabit status
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              PhotosyntheticPathway, Duration, GrowthHabit
    ),
    # Sage Grouse Groupings
    pct_cover(lpi_species,
              tall = TRUE,
              hit = "first",
              by_year = by_year,
              by_line = FALSE,
              by_sampleperiod = by_sampleperiod,
              SG_Group
    )
  )

  fh_spp_group_cover <- fh_spp_group_cover %>%
    # Substitute for Field friendly names
    dplyr::mutate(indicator = indicator %>%
                    stringr::str_replace_all(., spp.cover.replace)) %>%

    # Add AH to the beginning of the indicator to signify "any hit"
    dplyr::mutate(indicator = paste("FH_", indicator, "Cover", sep = ""))




  # Combine  all LPI based cover indicators----
  lpi_cover <- dplyr::bind_rows(
    ah_spp_group_cover,
    fh_spp_group_cover,
    total_foliar,
    between.plant.cover
  ) %>%
    # Spread to a wide format
    tidyr::spread(key = indicator, value = percent, fill = 0)



  #   SageBrush Shape, this is dependent on Shrub shape existing ----
  # TODO Need to check this with sagebrush state data

  # NEED TO ADD BY YEAR and BY SAMPLE PERIOD FUNCTIONALITY IF USING FOR USGS INDICATORS!
  sagebrush_shape_calc <- sagebrush_shape(
    lpi_tall = lpi_species,
    # NRI and LMF don't collect live v. dead
    live = dplyr::if_else(source %in% c("LMF", "NRI"),
                          FALSE, TRUE
    )
  )

  lpi_indicators <- dplyr::left_join(lpi_cover,
                                     sagebrush_shape_calc,
                                     by = "PrimaryKey"
  )

  # For TerrADat only, get the data visited from the first line in LPI
  if (source %in% c("TerrADat", "AIM")) {
    lpi_indicators <- lpi_species %>%
      dplyr::select(PrimaryKey, FormDate) %>%
      dplyr::group_by(PrimaryKey) %>%
      dplyr::summarize(DateVisited = dplyr::first(FormDate,
                                                  order_by = FormDate
      ) %>%
        as.POSIXct()) %>%
      # Join to the lpi.cover data
      dplyr::left_join(lpi_indicators, ., by = "PrimaryKey")
  }

  # Return lpi_indicators
  return(lpi_indicators
  )
}
