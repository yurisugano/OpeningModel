othreshold <- .875
x <- 1
ProbIncrease <- 4 ## How more likely is an animal to open the following day
Iterations <- 10000 ## How many iterations should the model run
  


OpeningVector <- 1
LatencyVector <- 1
OpeningMatrix <- matrix(nrow=Iterations,ncol=12)
LatencyMatrix <- matrix(nrow=Iterations,ncol=12)
AvgLatency <- 1
MedianLatency <- 1
OpeningN <- 1

OpeningDayList <- 1

      ## Number of iterations
for (i in 1:Iterations){
  
      ## Determine step increase in probability, and add noise
oincrease <- othreshold*(rnorm(1,mean=8.5,sd=1))/100 ##Add skewed distribution 

      ## Create matrices of random numbers and matrices of probability/day of opening
ProbabilityMatrix <- matrix(ncol=2,nrow=12)
ProbabilityMatrix [,1] <- sapply(12,runif,min=0,max=1)
ProbabilityMatrix [,2] <- sapply(12,rnorm,mean=1,sd=.001)
ProbabilityMatrix [,2] <- 1-(othreshold*ProbabilityMatrix[,2])

for (d in 2:2){
  ProbabilityMatrix[2,2] <- ProbabilityMatrix[2,2]
}
for (d in 3:4){
  ProbabilityMatrix[d,2] <- ProbabilityMatrix[d,2]
}
for (d in 5:6){
  ProbabilityMatrix[d,2] <- ProbabilityMatrix[d,2]
  }
for (d in 7:10){
  ProbabilityMatrix[d,2] <- ProbabilityMatrix[d,2]
}
for (d in 11:11){
  ProbabilityMatrix[d,2] <- ProbabilityMatrix[d,2]
}
for (d in 12:12){
  ProbabilityMatrix[12.2] <- ProbabilityMatrix[12,2]
}

      ## Create Opening and Latency vectors
Opening <- logical(length=12)
Latency <- as.numeric(c("40", "40", "40", "40", "40", "40", "40", "40", "40", "40", "40", "40"))


      ## Determine day of opening based on set probabilities
e <- 2
  while (ProbabilityMatrix[e-1,1] > ProbabilityMatrix[e-1,2] && e <= 12){
    e <- e+1
    }

if (e == 13){
  if (ProbabilityMatrix[12,1] < ProbabilityMatrix[12,2]){
    OpeningDay <- 12
    Latency[12] <- ProbabilityMatrix[12,1]/ProbabilityMatrix[12,2] * 40 ## Control for 40
  } else {
  OpeningDay <- NA
  }
} else {
  OpeningDay <- e-1
  Opening[e-1] <- TRUE
  pcoef <- 1-ProbabilityMatrix[e-1,2]
  Latency[e-1] <- ProbabilityMatrix[(e-1),1]/ProbabilityMatrix[(e-1),2] * 40
  
}
  
    
if (any(Opening) == FALSE) {
  OpeningDay <- NA
   ## Calculate probability of opening on subsequent days
} else if (OpeningDay == 12){
  Latency[12] <- 40*.5 ## Figure out this number
} else if (OpeningDay == 11){
  Latency[11] <- 40*.5 
  ProbabilityMatrix[12,1] <- .9
} else {
  ProbabilityMatrix[OpeningDay+1,2] <- .9
  for (w in (OpeningDay+1):11){
     ## Estimate from data 
      if (ProbabilityMatrix[w,1] < ProbabilityMatrix[w,2]){
          Opening[w] <- TRUE
          Latency[w] <- (Latency[w-1]*ProbabilityMatrix[w,1]/ProbabilityMatrix[w,2])+1*rnorm(1,1,.4)
          ProbabilityMatrix[w+1,2] <- ProbabilityMatrix[w,2] * 1.03
      } else {
          Opening[w] <- FALSE
          ProbabilityMatrix[w+1,2] <- ProbabilityMatrix[w+1,2]
          Latency[w] <- 40
        }
  }
  
  if(ProbabilityMatrix[12,1] < ProbabilityMatrix[12,2]){
    Opening[12] <- TRUE
    Latency[12] <- Latency[11]*.25
  }
}

AvgLatency[i] <- mean(Latency)
MedianLatency[i] <- MedianLatency
OpeningN[i] <- sum(Opening)
LatencyMatrix[i,] <- Latency
OpeningMatrix[i,] <- Opening
rm(d)
OpeningDayList[i] <- OpeningDay  

}