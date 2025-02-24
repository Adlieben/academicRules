#' Synthetic IBDP-like Dataset
#'
#' @description A small synthetic dataset of four students, illustrating the columns
#' needed for typical IBDP pass/fail rules (CAS, total points, HL/SL sums, etc.).
#'
#' @format A data frame with 4 rows and 10 columns:
#' \describe{
#'   \item{student_id}{Unique ID string for each student.}
#'   \item{CAS_met}{Logical indicating whether CAS requirement is fulfilled.}
#'   \item{Total}{Numeric: total points across 6 subjects + any core points.}
#'   \item{lowest_grade}{Numeric: lowest subject grade (1–7).}
#'   \item{count_2}{Number of subjects scored as 2.}
#'   \item{count_3orbelow}{Number of subjects scored 3 or below.}
#'   \item{HL_sum}{Sum of the top 3 HL scores.}
#'   \item{SL_sum}{Sum of SL scores.}
#'   \item{n_HL}{Number of HL subjects taken.}
#'   \item{n_SL}{Number of SL subjects taken.}
#' }
#'
#' @examples
#' # Load the dataset
#' data("SynthIB")
#' head(SynthIB)
"SynthIB"
