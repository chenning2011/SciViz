#' BarData1
#'
#' depression scores over time for two treatment groups (Control vs. Treatment)
#'
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{\code{Group}}{character. Group: This variable indicates the treatment group to which participants belong. \n It has two possible values: \n Control: Participants who did not receive the treatment. \n Treatment: Participants who received the treatment.}
#'   \item{\code{Time}}{character. Time: This variable represents the specific days on which depression scores were measured. \n It includes five time points: Day 1, Day 2, Day 3, Day 4, Day 5.}
#'   \item{\code{Depression_Scores}}{double. Depression_Scores: These are the depression scores recorded for each participant on the specified time points. Higher scores generally indicate more severe depression symptoms.}
#'   \item{\code{Error_Values}}{double. Error_Values: These are the error values (e.g., standard error or confidence interval) associated with each depression score. They help indicate the variability or uncertainty in the measured depression scores.}
#' }
"barData1"
