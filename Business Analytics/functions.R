
######################################################################## 1 Sample Z Test ###############################################################################

z.test <- function(sampleMean, populationMean, sd, n, confidenceLevel, alternative)
{
  
  #Argument control 
  
  if(tolower(alternative) != "two.sided" && tolower(alternative) != "less" && tolower(alternative) != "greater")
  {
    print("alternative must be either two.sided, less, or greater")
    return ()
  }
  
  if(confidenceLevel >= 1 || confidenceLevel <= 0)
  {
    print("Confidence Level must be between 0 and 1")
    return()
  }
  
  cat("------------------------------------------------RUNNING 1 SAMPLE Z TEST --------------------------------------------------------------------------","\n")
  
  #Printing givens
  
  cat("xbar: ", sampleMean, "\n")
  cat("mu: ", populationMean, "\n")
  cat("sd: ", sd, "\n")
  cat("n: ", n, "\n")
  
  #z-statistic
  zStat <- (sampleMean - populationMean)/(sd/sqrt(n))

  cat("Z-Statistic is: ", zStat, "\n\n")
  
  zStar <- NULL
  if(tolower(alternative) == "two.sided")
  {
    zStar <- qnorm(confidenceLevel + (1-confidenceLevel)/2, 0 ,1)
  } else 
  {
    zStar <- qnorm(confidenceLevel, 0, 1)
  }
  
  cat("Z-Star is: ", zStar, "\n\n")
  
  pVal <- NULL
  if(tolower(alternative) == "two.sided")
  {
    pVal <- 2*(1-pnorm(zStat,0,1))
  } else if(tolower(alternative) == "greater")
  {
    pVal <- 1-pnorm(zStat, 0, 1)
  } else {
    pVal <- pnorm(zStat, 0, 1)
  }
  
  cat("P-Value is: ", pVal, "\n\n")
  
  cat(confidenceLevel, "Confidence Interval: ", sampleMean - zStar * sd/sqrt(n), "   ", sampleMean + zStar * sd/sqrt(n), "\n")
  
  cat("------------------------------------------------FINISHED 1 SAMPLE Z TEST --------------------------------------------------------------------------","\n")
  
}

