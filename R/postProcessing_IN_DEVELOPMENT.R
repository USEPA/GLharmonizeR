
# KV: This file should be moved into a development folder or otherwise clearly identified as code in progress (not included as part of the core package functions here for the first version). This code has not been reviewed.

# [x] Add in less than RL's here too
# [x] Count the values that are <RL separately
# - if both mdl and rl igve priority to mdl
.dlImputation <- function(df, imputeMethod = NULL){
  # [ ] impute secchi depth as a separate function
  if (imputeMethod == "halfMDL"){
    imputeFunction = function(mdl) mdl/2
  } else if (imputeMethod == "uniform") {
    imputeFunction = function(mdl) runif(n = 1, min = 0, max=mdl)
  } else {
    # don't do anything
    imputeFunction = function(mdl) mdl
  }

  df <- df %>%
    dplyr::mutate(
      # resolve duplicates as average
      RESULT = mean(RESULT, na.rm =T),
      # fill in missing data
      Study = toString(unique(Study)),
      Unified_Flag = toString(unique(Unified_Flag)),
      Unified_Comment = toString(unique(Unified_Comment)),
      Study = toString(unique(Study)),
      MDL = mean(MDL, na.rm =T),
      # PQL = mean(PQL, na.rm =T),  dropped PQL might need to add back imputeMethodn
      RL = mean(RL, na.rm =T),
      .by= c(Latitude, Longitude, sampleDateTime, CodeName, sampleDepth)) %>%
    # get yera for imputing
    dplyr::distinct() %>%
    dplyr::mutate(Year = lubridate::year(sampleDateTime)) %>%
    dplyr::mutate(
      # impute using DLs
      # https://19january2017snapshot.epa.gov/sites/production/files/2015-06/documents/whatthel.pdf
      # MDL < PQL
      # [x] find how to incorporate RL
      # NOTE keep those reported as below MDL because their estimate is still likely better
      MDL = mean(MDL, na.rm =T),
      # PQL = mean(PQL, na.rm =T),  dropped PQL might need to add back imputeMethodn
      RL = mean(RL, na.rm =T),
      RESULT = dplyr::case_when(
        !is.na(RESULT) ~ RESULT,
        # NOTE that taking the min of results would also include estimated values where they exist
        # Follow up- this means that values aren't consistently imputed when comparing those that are
        # reported as estimates and what this program imputes
        grepl("N", Unified_Flag) & (!is.na(MDL) | sum(!is.na(RESULT)) > 1) ~ imputeFunction(min(c(MDL, RL, RESULT), na.rm=T)),
        is.na(RESULT) & !is.na(MDL) ~ imputeFunction(MDL),
        is.na(RESULT) & !is.na(RL) ~ imputeFunction(RL),
        .default = RESULT
      ),
      # make sure to manage different mdls over studies, times, and analytes
      .by = c(Study, Year, CodeName)
    )

  return(df)
}

# pivot
.exactPivot <- function(df){
  df_wide <- df %>%
    # excludes units, flags, etc. to keep neat and since its in documentation
    # [ ] append units to codename
    pivot_wider(
      id_cols = c(Latitude, Longitude, sampleDateTime, sampleDepth, stationDepth, SITE_ID),
      names_from = c(CodeName, TargetUnits),
      values_from = RESULT,
      values_fn = function(x) mean(x,na.rm = T)
      ) %>%
    tidyr::drop_na(Latitude, sampleDateTime, sampleDepth)
  return(df_wide)
}

# Match observations in time and space
.imputeNearestMatch <- function(..., CodeName, matchingSet = NULL, dayThresh = 3, latlonThres = 0.01){
  observation <- tibble::tibble(...)
  # different threshold depending on depth
  # depth Threshold could defintiely be done better using asymetric bound
  depthThres <- ifelse(observation$sampleDepth > 30, 15, 5)
  nearestMatch <- matchingSet %>%
    # only look within some box
    dplyr::filter(
      dplyr::between(sampleDateTime,
        observation$sampleDateTime - days(dayThresh),
        observation$sampleDateTime + days(dayThresh)
        ),
      dplyr::between(Latitude,
        observation$Latitude - latlonThres,
        observation$Latitude + latlonThres
        ),
      dplyr::between(sampleDepth,
        observation$sampleDepth - depthThres,
        observation$sampleDepth + depthThres
        ),
    # calculate multivariate distance
    )
    if (nrow(nearestMatch) != 0) {
      nearestMatch <- nearestMatch %>%
      dplyr::mutate(
        dlat = observation$Latitude - Latitude,
        dlng = observation$Longitude - Longitude,
        d = sqrt(dlat **2 + dlng ** 2),
        # DISTANCE FOR DEPTH
        # # mean is shape * scale
        # # variance is shape * scale ** 2
        # expand.grid(q = 1:60, shape = 10, scale = 1.5) %>%
        #   mutate(
        #     cu = paste(shape, scale),
        #     p = pgamma(q, shape, scale = scale)
        #     ) %>%
        #   ggplot(aes(y = q, x = p, col = cu)) +
        #   geom_line()  +
        #   scale_y_reverse() +
        #   scale_x_reverse()
        #######################
        ddepth = abs(
          pgamma(sampleDepth, 10, scale = 1.5) - pgamma(observation$sampleDepth, shape = 10, scale = 1.5 )),
        ddays = sampleDateTime - observation$sampleDateTime,
        # compute a scalar distance (formula up for debate)
        # standardizing by 3 day, 20km cutoff
        D = ((1/latlonThres * d) + (1 / depthThres) * ddepth + (1/dayThresh) * abs(ddays))
      ) %>%
      # select minimum on this distance metric
      dplyr::filter(D == min(D, na.rm = T)) %>%
      dplyr::slice(1) %>%
      dplyr::pull({{ CodeName }})
      # if nothing within tolerance return NA
    } else {
      nearestMatch <- NA
    }
    return(nearestMatch)
}

.imputeNearestMatchColumn <- function(df, column, outcomeColumn = Chla_ugl, dayThresh = 6, latlonThres = 0.01){
  missing <- df %>% dplyr::filter(is.na({{ column }}))
  nonmissing <- df %>% tidyr::drop_na({{ column }})

  # [ ] Add ifelse to only impute for observations that have nonmising Chla
  missing <- missing %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      {{ column }} := ifelse(
        # only consider imputing if desired outcome is observed
        !is.na({{ outcomeColumn }}),
        .imputeNearestMatch(Latitude, Longitude, sampleDepth, sampleDateTime,
        matchingSet = nonmissing, dayThresh = dayThresh, latlonThres = latlonThres, CodeName = {{ column }}),
        {{ column }}
        )) %>%
    dplyr::bind_rows(nonmissing) %>%
    dplyr::ungroup()

  return(missing)
}



