#' Looking at how Lincoln-Peterson and Seber methods give different estimates
#'
#' Gives the estimates and plots from two different methods of population estimate from MarkRecapture method.
#' @param M Marked individuals at the first trapping or initially
#' @param c Total individuals captured in the second trapping
#' @param R Marked recaptures in the second trapping
#' @return Plots and estimates of population from Lincoln-Peterson and Seber methods
#' @export
#' @examples # Putting the values in variables with their respective nubmers
#' @examples c <- c(46, 72, 50, 79, 91, 55) # total samples
#' @examples r <- c(1, 7, 8, 8, 10, 6)	# no. of marked ind. caught
#' @examples M <- 54 # total marked ind. captured
#' @examples
#' @examples # Using the custom function
#' @examples PopEst(M, c,R )
#' @examples PopEst(34, 24, 3) # for single survey



# Better function with built-in loop so that we don't have to use for function later.

# creating a function to have all the values and calculate the normal and seber
PopEst <- function(M, c,r){ # M is marked ind, c is captured, r is recaptured
  message("Population estimation using")
  message("Lincoln-Peterson and Seber methods with confidence interval")
  message("")
  DT <- matrix(nrow = 2,ncol = length(c))
  rownames(DT) <- c("Lincoln-Peterson","Seber")
  colnames(DT) <- 1:length(c)
  for(i in 1:length(c)){
    norm <- ((M*c[i])/r[i])
    seb <- (((M+1)*(c[i]+1))/(r[i]+1)) - 1
    Var <- (((M+1)*(c[i]+1)*(M-r[i])*(c[i]-r[i]))/(((r[i]+1)^2)*(r[i]+2)))
    cI_n_hi <- norm + (1.96) * (Var^0.5)
    cI_n_low <- norm - (1.96) * (Var^0.5)
    cI_s_hi <- seb + (1.96) * (Var^0.5)
    cI_s_low <- seb - (1.96) * (Var^0.5)
    DT[ ,i] <- c(norm, seb)
    message("SAMPLE ", i)
    message("......................................................................... ")
    message("Marked: ", M, "    Marked recaptures: ", r[i], "    Second sample size: ", c[i])
    message("Normal (Lincoln-Peterson): ", round(norm, digits =2))
    message("Range: high -> ", round(cI_n_hi,digits = 2), "    Low -> ", round(cI_n_low, digits = 2))
    message(" ")
    message("Seber: ", round(seb, digits =2))
    message("Range: high -> ", round(cI_s_hi,digits =2), "    Low -> ", round(cI_s_low, digits = 2))
    message("Variance: ", round(Var,digits = 2))
    message("......................................................................... ")
  }
  # plotting using  barplot
  barplot(DT,
          main = "Population estimates with Lincoln-Peterson and Seber methods",
          beside = TRUE,
          ylim = c(0, max(DT) + 100),
          xlab = "Samples",
          col = c("blue", "green")
  )
  #putting up the legends
  legend("topright",
         c("Lincoln-Peterson","Seber"),
         fill = c("blue","green")
  )
}
