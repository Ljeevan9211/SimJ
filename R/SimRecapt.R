#' Looking at how number of marked individuals affect the recaptures in Mark-Recapture estimates
#'
#' Gives the recaptures when given population, marked individuals and sample size
#' @param N Total population of the species
#' @param M Marked individuals at the first trapping or initially
#' @param c Total individuals captured in the second trapping
#' @return Recaptures of marked individuals for the sample size
#' @export
#' @examples # A simple situation with 1000 individauls in a population with 88 marked individuals and sample size 128
#' @examples SimPop(1000, 88, 128)

# Creating a function to return the recaptures
SimRecapt <- function(N, M,c){ 			# N is population, M is marked ind and c is captured individuals in second trapping
  Prob <- M/N 						# this is the probability of recapture (marked individuals/total population)
  recapt <- rbinom(1, c, Prob)		# recaptures are binomal probabilities
  message("Total population: ", N, "     Marked individual: ", M)
  message("Sample size: ", c, "          Probability of recaptures: ", Prob)
  message("Simulated recaptures: ", recapt)
  return(recapt)
}
