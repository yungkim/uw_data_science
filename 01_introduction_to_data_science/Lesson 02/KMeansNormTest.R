# KMeansNormTest.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

TestObservations <- as.matrix(read.csv("TestObservations.csv"))
TestCenters <- matrix(c(1, 1, -2, -2, 2, -2), nrow=3)

source("KMeans.R")
source("KMeansHelper.R")
source("KMeansNorm_skeleton.R")

# TestObservations Distribution in second dimension
#TestCenters <- matrix(c(1,1,-2,-2,2,-2), byrow-T, ncol=2)
hist(TestObservations[,2], col=rgb(1,1,0,1))

# TestObservations Distribution in first dimension
hist(TestObservations[,1], col=rgb(0,0,1,0.25), add=T)

# 4a. What is the single most obvious difference between these two distributions?
summary(TestObservations)
# X2.1              X143         
# Min.   :-22.000   Min.   :-227.000  
# 1st Qu.: -5.175   1st Qu.: -69.250  
# Median :  0.050   Median : -30.000  
# Mean   : -2.228   Mean   :  -1.744  
# 3rd Qu.:  2.600   3rd Qu.:  86.750  
# Max.   : 11.900   Max.   : 241.000  
sd(TestObservations[,1]) # 8.194793
sd(TestObservations[,2]) # 98.88749
# Answer: The mean values in both columns are almost the same; -2.228, -1.744
# but the range of the first column is from min -22 to max 11.9, 
# whereas the range of the second column is approx. 10 times larger from min -227 to max 241.

# Test 1
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# [,1]       [,2]
# [1,] -5.886111  -51.50000
# [2,]  2.975000   94.33333
# [3,] -7.790000 -168.50000
# Answer: Clustering occurs along the Y axis only.   
# This is because points are approximately 10 times further apart in this dimension, 
# which prevents the centroids from migrating in the Y axis as easily in the X axis direction.

# Test 2
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# [,1]       [,2]
# [1,] -5.886111  -51.50000
# [2,]  2.975000   94.33333
# [3,] -7.790000 -168.50000
# Answer : Clustering occurs along the Y axis only. The result is same as Test 1.
# In Test 1, the first column values (X) were already more condensed than the second value (Y). 
# By normalizing X values only, this intensify the mismatch in distributions and prevents clustering on the X axis.

# Test 3
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# [,1]      [,2]
# [1,]  -1.374194 -10.32258
# [2,]   4.681250  58.00000
# [3,] -15.257895 -88.36842
# Answer: The clustering occurs on the X axis only, 
# because the range of values is approx. 10 times the range of values in the Y axis. 

# Test 4
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# [,1]       [,2]
# [1,]   0.1346154  -55.88462
# [2,]   2.9750000   94.33333
# [3,] -14.6650000 -104.30000
# Answer: The clustering occurs on both X and Y axes.
# Because the standard deviations have been set to 1 for both distributions, 
# centroids can migrate in both directions with the same ease.

# Put answers to assignment questions here:
# 5. Why is normalization important in K-means clustering?
# Answer: Normalization in K-means clustering is important because different ranges 
# of standard deviations make the centroids more fixed and difficult to move around for the 
# features with a large minimum and maximum value. 
# The features you record may have a naturally different range, 
# and without normalization, the importance could be given to only certain variables.
# Normalization gives the same importance to all the variables. 
# The standard example is considering age (in year) and height (in cm). 
# The age may range in [18 50], while the height may range in [130 180].

# 6. How do you encode categorical data in a K-means clustering? 
# Answer: You can binarize the categories into a one-hot encoding. In this situation, 
# normalizing the distributions of the other features is very important. The 
# binarized bins have a binomial distribution with values at 0 or 1. This 
# is probably very different from the distribution of other features.

# 7. Why is clustering un-supervised learning as opposed to supervised learning? 
# Answer: Clustering doesn't use labeled data to train the algorithm. 
# Supervised learning is the machine learning task of inferring a function from labeled training data. 
# Unsupervised learning allows us to approach problems with little or no idea what our results should look like. 
# We can derive structure from data where we don't necessarily know the effect of the variables.
# We can derive this structure by clustering the data based on relationships among the variables in the data.
# With unsupervised learning there is no feedback based on the prediction results, 
