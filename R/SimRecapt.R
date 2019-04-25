#' Looking at how number of marked individuals affect the recaptures in Mark-Recapture estimates
#'
#' Gives the recaptures when given population, marked individuals and sample size
#' @param N Total population of the species
#' @param M Marked individuals at the first trapping or initially
#' @param c Total individuals captured in the second trapping
#' @param iter Total number of iterations you want to simulate
#' @return Recaptures of marked individuals for the sample size
#' @export
#' @examples # A simple situation with population of 1000 with 88 marked and sample size 128
#' @examples SimRecapt(1000, 88, 128)
#'
#' @examples # Save the recaptures and do a population estimation
#' @examples N <- 300
#' @examples M <- 72
#' @examples c <- 50
#' @examples iter <- 10
#' @examples r <- SimRecapt(N, M, c, iter)
#' @examples PopEst(M, c, r)

# Creating a function to return the recaptures
SimRecapt <- function(N, M,c, iter = 1){ 			# N is population, M is marked ind and c is captured individuals in second trapping
  Prob <- M/N 						# this is the probability of recapture (marked individuals/total population)
  Recapt <- rbinom(iter, c, Prob)		# recaptures are binomal probabilities
  message("Total population: ", N, "     Marked individual: ", M)
  message("Sample size: ", c, "          Probability of recaptures: ", Prob)
  Recaptures <- data.frame(Recaptures = as.numeric(Recapt))
  return(Recaptures)
}
