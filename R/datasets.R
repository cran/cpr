#' Simulated Pregnanediol glucuronide (PDG) Data
#'
#' A Simulated data set based on the Study of Women's Health Across the Nation
#' (SWAN) Daily Hormone Study (DHS).
#'
#' Pregnanediol glucuronide (PDG) is the urine metabolite of progesterone.  This
#' data set was simulated to have similar
#' characteristics to a subset of the SWAN DHS data.  The SWAN DHS data was the
#' motivating data set for the method development that lead to the \code{cpr}
#' package.  The DHS data cannot be made public, so this simulated data set has
#' been provided for use in examples and instructions for use of the \code{cpr}
#' package.
#'
#' @references
#' Santoro, Nanette, et al. "Body size and ethnicity are associated with
#' menstrual cycle alterations in women in the early menopausal transition: The
#' Study of Women's Health across the Nation (SWAN) Daily Hormone Study." The
#' Journal of Clinical Endocrinology & Metabolism 89.6 (2004): 2622-2631.
#'
#' @format a \code{data.frame}. Variables in the data
#' set:
#' \describe{
#'  \item{id}{Subject ID}
#'  \item{age}{Age, in years of the subject}
#'  \item{ttm}{Time-to-menopause, in years}
#'  \item{ethnicity}{Ethnicity, a factor with five levels: Caucasian, Black,
#'  Chinese, Hispanic, and Japanese}
#'  \item{bmi}{Body Mass Index}
#'  \item{day_from_dlt}{A integer value for the number of days from Day of
#'  Luteal Transition (DLT).  The DLT is \code{day_from_dlt == 0}.  Negative
#'  values indicate the follicular phase, positive values for the luteal phase.}
#'  \item{day_of_cycle}{the day of cycle}
#'  \item{day}{A scaled day-of-cycle between [-1, 1] with 0 for the DLT.  See
#'  Details}
#'  \item{pdg}{A simulated PDG value}
#' }
#'
#' @source This is simulated data.  To see the script that generated the data
#' set please visit \url{https://github.com/dewittpe/cpr} and look at the
#' scripts in the data-raw directory.
"spdg"

#' United States Laboratory Confirmed COVID-19 Cases
#'
#' Number of laboratory-confirmed COVID-19 cases in the United States, as
#' reported by the Centers for Disease Control, between January 1 2020 and May
#' 11, 2023, the end of the public health emergency declaration.
#'
#' @format a \code{data.frame} with two columns
#' \describe{
#'   \item{date}{year, month, day}
#'   \item{cases}{number of reported laboratory-confirmed COVID-19 cases}
#' }
#'
#' @source
#' Download original data from
#' <https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf>
#' on December 5, 2023.  The reported data set was last updated on November 3,
#' 2023.
#'
"us_covid_cases"

