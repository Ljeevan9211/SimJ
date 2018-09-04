#' Simulate Occupancy estimates
#'
#' Gives the estimates of your simulated study design from 1000 simulations for better occupancy study designs.
#' @param N Number of sites
#' @param k Number of replicates
#' @param psi True occupancy
#' @param p True detection
#' @return Plots with estimates of occupancy and detection
#' @export
#' @examples simOccSSO(100, 20, 0.8, 0.2)

simOCCSS0 <- function(N,k,psi,p){
  require(wiqid)
  library(wiqid)
  DH <- matrix(nrow = N, ncol=k)

  iter <- 1000 # no. of simulations we want to generate
  # creating pockets to keep our estimates of occupancy and detection probability
  estimate.OCC <- numeric(iter)
  estimate.DETC <- numeric(iter)

  # Creating detection/non-detection in occupied sites with the iterations = 1000
  for(l in 1:iter){
    (occ <- rbinom(N,1, psi))
    for(i in 1:N){
      if(occ[i] == 0){
        DH[i,] <- 0
      }
      else{
        for(j in 1:k){
          DH[i,j] <-rbinom(1,1, p)
        }}}
    y <- rowSums(DH, na.rm=TRUE)  # detection per site
    n <- rowSums(!is.na(DH))      # no of replicates per site
    estimate <- occSS0(y,n)
    estimate.OCC[l] <- estimate$real[1,1]
    estimate.DETC[l] <- estimate$real[2,1]
  }

  #  Checking the mean occupancy and detection probability from the simulations
  mean(estimate.OCC)
  mean(estimate.DETC)

  # proportion of estimates that differ the true value (occupancy by 0.1 and detection by 0.05)
  mean(abs(estimate.OCC - psi) > 0.1)
  mean(abs(estimate.DETC - p) > 0.05)

  # plotting the estimates
  par(mfrow=c(2,1))
  plotPost(estimate.OCC, showCurve=TRUE)
  plotPost(estimate.DETC, showCurve=TRUE)
  par(mfrow=c(1,1))
}

