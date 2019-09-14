#' @title
#' Mayo Clinic Primary Biliary Cirrhosis Data
#'
#' @description
#' A nearly identical data set found in appendix D of Fleming and Harrington.
#'
#' The data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of
#' the liver conducted between 1974 and 1984. A total of 424 PBC patients,
#' referred to Mayo Clinic during that ten-year interval, met eligibility
#' criteria for the randomized placebo controlled trial of the drug
#' D-penicillamine. The first 312 cases in the data set participated in the
#' randomized trial and contain largely complete data. The additional 112 cases
#' did not participate in the clinical trial, but consented to have basic
#' measurements recorded and to be followed for survival. Six of those cases
#' were lost to follow-up shortly after diagnosis, so the data here are on an
#' additional 106 cases as well as the 312 randomized participants.
#'
#' @format A tibble with 418 observations on 20 variables.
#' \describe{
#'   \item{id}{Patient ID}
#'   \item{time}{Number of days between registration and the earlier of death, transplantion, or study analysis time in July, 1986}
#'   \item{status}{Histologic stage of disease (needs biopsy)}
#'   \item{trt}{Drug treatment}
#'   \item{age}{Age in years}
#'   \item{sex}{Sex}
#'   \item{ascites}{Presence of ascites}
#'   \item{hepato}{Presence of hepatomegaly or enlarged liver}
#'   \item{spiders}{Blood vessel malformations in the skin}
#'   \item{edema}{Presence of edema}
#'   \item{bili}{Serum bilirunbin (mg/dl)}
#'   \item{chol}{Serum cholesterol (mg/dl)}
#'   \item{albumin}{Serum albumin (g/dl)}
#'   \item{copper}{Urine copper (ug/day)}
#'   \item{alk_phos}{Alkaline phosphotase (U/liter)}
#'   \item{ast}{Aspartate aminotransferase, once called SGOT (U/ml)}
#'   \item{trig}{Triglycerides (mg/dl)}
#'   \item{platelet}{Platelets per cubic ml/1000}
#'   \item{protime}{Standardised blood clotting time}
#'   \item{stage}{Histologic stage of disease}
#' }
#'
#' @source
#' Therneau and P Grambsch (2000), Modeling Survival Data: Extending the Cox Model, Springer-Verlag, New York. ISBN: 0-387-98784-3.
#'
#' @examples
#' pbc_mayo
#'
#' library(dplyr)
#' dplyr::glimpse(pbc_mayo)
"pbc_mayo"
