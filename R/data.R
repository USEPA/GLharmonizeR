#' Lake Michigan Water quality data compiled across multiple studies.
#'
#' A dataset containing water quality measurements from NCCA, CSMI, GLNPO,
#'  and NOAA data sources. The variables are as follows:
#'
#' @name allWQ
#' @author Kelsey Vitense \email{vitense.kelsey@epa.gov}
#' @author Christian Coffman
#' @keywords datasets
#' @usage data(allWQ)
#' @docType data
#' @format A data frame with 372824 rows and 27 variables:
#' \describe{
#'   \item{UID}{Unique sample ID across all sources}
#'   \item{Study}{Name of study}
#'   \item{SITE_ID}{Name of site}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#'   \item{stationDepth}{Depth in meters of the station / site }
#'   \item{sampleDateTime}{Date time of sampling event}
#'   \item{sampleDepth}{Depth at which sample was taken}
#'   \item{CodeName}{Harmonized short names for all water quality parameters.}
#'   \item{LongName}{Harmonized descriptive names for all water quality parameters.}
#'   \item{RESULT}{Value of the measurement.}
#'   \item{Units}{Units of associated measurement.}
#'   \item{MDL}{Method detection limit. Matched per analyte, method, study, and year when available.}
#'   \item{RL}{Reporting limit, when available.}
#'   \item{Unified_Flag}{Unified QC flag to relate quality of the corresponding measurement.}
#'   \item{Unified_Comment}{Unified QC comment to relate quality of the corresponding measurement.}
#'   \item{Category}{WQX Category}
#'   \item{METHOD}{Associated measurement method.}
#'   \item{LAB}{Associated lab taking measurement}
#'   \item{Orig_QAcode}{QA code in the original data source}
#'   \item{Orig_QAcomment}{QA comment in the original data source}
#'   \item{Orig_QAdefinition}{QA definition in the original data source}
#'   \item{ANALYTE_Orig_Name}{ANALYTE name in the original data source}
#'   \item{ReportedUnits}{Measurement units in the original data source}
#'   \item{ConversionFactor}{Multiplicative conversion factor if conversion is necessary to convert from ReportedUnits to Units}
#' }
#'
#' @source {allWQ} GLharmonizeR Great Lakes water quality R package.
"allWQ"
