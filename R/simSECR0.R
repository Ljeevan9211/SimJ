#' Simulate Density estimates from SECR
#'
#' Gives the estimates of your simulated study design from 1000 simulations for better occupancy study designs.
#' @param Den True density of your interest species in the study area
#' @param Buf True value of buffer in the study area
#' @param G0 True g0 of your interest species
#' @param S True S of your interest species
#' @param iter Number of simulations you want to create
#' @param occasions Number of occassions you will put the traps out.
#' @param trapD Your detector design. Assing this with x <- read.traps("cameratrap.txt", detector ="proximity")
#' @return Plots with estimates of density as per your study desing
#' @export
#' @examples x<- read.traps("cameratrapSECR1.txt", detector = "proximity")
#' @examples simSECR0(0.003, 200, 0.1, 50, 100, 12, trapD = x)


simSECR0 <- function(Den = 1, Buf = 200, G0 = 0.1, S = 50, iter = 100, occasions = 15, trapD){
require(secr)
library(secr)
require(wiqid)
library(wiqid)
require(parallel)
library(parallel)

## Study design model
Density <- numeric(iter) # creating pockets to store our density estimates
minDensity <- 40
buffer <- Buf # Buffer you want to use

# Now going for the loop
for(i in 1:iter){
  y <- sim.capthist(trapD, popn = list(D = Den, buffer = Buf), detectpar = list(g0 = G0, sigma = S),noccasions = occasions)# generating simulated capture history using trap data and biological model
  plot(y, tracks = TRUE, varycol = TRUE, icolours = c("green", "red", "black","yellow","orange","white","blue","magenta","cyan","grey"))
  if(max((animalID(y))) > 6){
    stt <- secr.fit(y, buffer = Buf, ncores = detectCores() - 1)
    values <- predict(stt)
    Density[i] <- values$estimate[1] # storing the value of density estimate in our pocket
  }
  else{
    j <- i-1
    i <- j
  }
}
plotPost(Density, showCurve = TRUE)

plotPost(Density, showCurve=TRUE)
return(Density)
library(beeswarm)
beeswarm(Density, method='hex', cex=0.5, col='blue', las=1,
         xlab="Sample size", ylab="Estimated Density")

}
