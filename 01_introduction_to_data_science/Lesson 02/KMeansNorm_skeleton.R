# KMeansNorm.R

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    
    meanD1 <- mean(observations[,1]) # print(mean(observations[,1]))
    sdD1 <- sd(observations[,1])
    # normalize 1st dimension of observations
    observations[,1] <- (observations[,1] - meanD1) / sdD1
    # normalize 1st dimension of clusterCenters
    clusterCenters[,1] <- (clusterCenters[,1] - meanD1) / sdD1
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    meanD2 <- mean(observations[,2])
    sdD2 <- sd(observations[,2])
    # normalize 2nd dimension of observations
    observations[,2] = (observations[,2] - meanD2) / sdD2
    # normalize 2nd dimension of clusterCenters
    clusterCenters[,2] = (clusterCenters[,2] - meanD2) / sdD2
  }
  clusterCenters <- KMeans(observations, clusterCenters)
  if (normD1)
  {
    # denormalize in first dimension
    observations[,1] = (observations[,1] * sdD1) + meanD1
    clusterCenters[,1] = (clusterCenters[,1] * sdD1) + meanD1
  } 
  if (normD2)
  {
    # denormalize in second dimension
    observations[,2] = (observations[,2] * sdD2) + meanD2
    clusterCenters[,2] = (clusterCenters[,2] * sdD2) + meanD2
  } 
  return(clusterCenters)
}

