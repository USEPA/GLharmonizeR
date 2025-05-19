#' Lake Michigan Water quality data compiled across multiple studies.
#'
#' A dataset containing water quality measurements from NCCA, CSMI, GLENDA,
#'  and NOAA data sources. The variables are as follows:
#'
#' @name lakeMichigan
#' @author Kelsey Vitense \email{vitense.kelsey@epa.gov}
#' @author Christian Coffman
#' @references GLENDA \url{https://cdx.epmeea.gov/}, NCCA \url{ncca}, \url{csmi}
#' @keywords datasets
#' @usage data(lakeMichigan)
#' @docType data
#' @format A data frame with 338739 rows and 28 variables:
#' \describe{
#'   \item{UID}{Unique ID across all sources}
#'   \item{Study}{Name of study}
#'   \item{SITE_ID}{Name of site}
#'   \item{Latitude}{Latitude in degreees decimals (41.66336--55.30979)}
#'   \item{Longitude}{Longitude in degreees decimals (-90.14804--83.32752)}
#'   \item{stationDepth}{Depth in meters of the station / site (0.2--273.4056)}
#'   \item{sampleDateTime}{Date time of sampling event (1983/04/19 12:00:00 UTC--2023/08/05 04:14:54 UTC)}
#'   \item{sampleDepth}{Depth at which sample was taken (0.1--258.5)}
#'   \item{CodeName}{Unified, unique names for all of the measured quantities along with units separated by underscore. (Examples: Diss_Cl, Tot_P, DOC)}
#'   \item{LongName}{Unified, unique full names for all of the measured quantities.}
#'   \item{RESULT}{Value of the measurement.}
#'   \item{Units}{Units of associated measurement.}
#'   \item{MDL}{Method detection limit. Matched per analyte, method, study, and year when available.}
#'   \item{RL}{Reporting limit. Matched per analyte, method, study, and year when available.}
#'   \item{Unified_Flag}{Unified qc flag to relate quality of the corresponding measurement.}
#'   \item{Unified_Comment}{Unified qc comment to relate quality of the corresponding measurement.}
#'   \item{Category}{WQX Category}
#'   \item{METHOD}{Associated measurement method.}
#'   \item{LAB}{Associated lab taking measurement}
#'   \item{Orig_QAcode}{QA code in the original data source}
#'   \item{Orig_QAcomment}{QA comment in the original data source}
#'   \item{Orig_QAdefinition}{QA definition in the original data source}
#'   \item{ANALYTE_Orig_Name}{ANALYTE name in the original data source}
#'   \item{ReportedUnits}{Measurement units in the original data source}
#'   \item{TargetUnits}{Desired measurement units for the output data}
#'   \item{ConversionFactor}{Multiplicative conversion factor if conversion is necessary to convert from ReportedUnits to TargetUnits}
#'   \item{Retain_InternalUse}{Internal decision on what data to retain based on QC flags}
#'   \item{Action_InternalUse}{Verbose description of decision made off of qa flags}
#' }
#'
#' @source {LMChla} Lake Michigan water quality R package.
"lakeMichigan"
