# Assignment01.R
# Copyright 2016 by Yung Kim
####################################################

# Clear Workspace and Console
rm(list=ls()) 
cat("\014")

# 3. Follow the patterns described in DataScience01a.R and use R 
# to get the Indian Liver Patient Dataset from the UCI machine learning repository.

# Assign a url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"

# Download a rectangular dataset
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)

# How many rows and columns are in the dataframe?
nrow(ILPD) # 583
ncol(ILPD) # 11

# See all the data
ILPD

# View the column names
names(ILPD) # "V1"  "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11"

# 4. Manually construct a vector of column headers from this page:
# http://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset)
headers <- c("Age", "Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "ALB", "AG_Ratio", "Selector")
names(ILPD) <- headers

# 5. View the first 6 rows of the data
head(ILPD)

# 6. Determine the standard deviation, mean, and median for each vector
ILPD$Alkphos <- as.numeric(ILPD$Alkphos)
ILPD$Gender <- ifelse(ILPD$Gender=="Male",0,1)  # convert male to 0, female to 1. or null vector: ILPD$Gender <- NULL
sapply(ILPD, sd, na.rm=TRUE)
sapply(ILPD, mean, na.rm=TRUE)
sapply(ILPD, median, na.rm=TRUE)

is(ILPD)
sapply(ILPD, is)
sapply(ILPD, length)
summary(ILPD)

# 7. Get a profile of each column
hist(ILPD$Age, col=rgb(0,1,0,.5)) # hist(ILPD[, 1])
hist(ILPD$Gender, col=rgb(0,1,0,.5)) # hist(ILPD[, 2])
hist(ILPD$TB, col=rgb(0,1,0,.5)) # hist(ILPD[, 3])
hist(ILPD$DB, col=rgb(0,1,0,.5)) # hist(ILPD[, 4])
hist(ILPD$Alkphos, col=rgb(0,1,0,.5)) # hist(ILPD[, 5])
hist(ILPD$Sgpt, col=rgb(0,1,0,.5)) # hist(ILPD[, 6])
hist(ILPD$Sgot, col=rgb(0,1,0,.5)) # hist(ILPD[, 7])
hist(ILPD$TP, col=rgb(0,1,0,.5)) # hist(ILPD[, 8])
hist(ILPD$ALB, col=rgb(0,1,0,.5)) # hist(ILPD[, 9])
hist(ILPD$AG_Ratio, col=rgb(0,1,0,.5)) # hist(ILPD[, 10])
hist(ILPD$Selector, col=rgb(0,1,0,.5)) # hist(ILPD[, 11])

# 8. Correlate columns
plot(ILPD)

# 9. Look at the plots from plot(ILPD) and answer:
#  • How can you tell if a vector contains continuous numbers or binary data?
#    - If a vector contains binary data, there exist only 2 values such as 0 and 1.
#      Gender and Selector's plots show only 2 values, and form straight perpendicular line segments on graph.
#      On the other hand, continuous data are scatter plotted with many different values, and not perpendicularly aligned on graph.
#  • Which two vectors are most strongly correlated?
#    - SGPT and SGOT.
#      Two continuous variables are correlated when the points on the scatter plot appear to coalesce along a line, 
#      which tends to be around 45 positive or negative degree slope.
#  • Give an example of two vectors that have little correlation
#    - Age and SGOT.


# 10. Follow the patterns described in DataScience01b.R Write code to remove outliers
# from the following vector and present the result in the console: c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
goodFlag <- (x < highLimit) & (x > lowLimit)
x[goodFlag]
x
x <- x[goodFlag]
x # 1 -1 -1  1  1 -3  1  1  3

# 11. Follow the patterns described in DataScience01b.R Write code to relabel the
# following vector. Use the shortest strings for each category in the relabeled
# version. Present the result in the console: c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'MS', 'BS', 'MS')
y <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'MS', 'BS', 'MS')
y[y == 'Bachelors'] <- 'BS'
y[y == 'Masters'] <- 'MS'
y[y == 'High School'] <- 'HS'
y # "BS"  "MS"  "PhD" "HS"  "BS"  "MS"  "HS"  "MS"  "BS"  "MS" 

# 12. Follow the patterns described in DataScience01b.R Write code to normalize the
# following vector using a Min-Max normalization and present the result in the
# console. Do not remove outliers. c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
z <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- min(z)
b <- max(z) - min(z)
normalized <- (z - a) / b
normalized # 0.2 0.1 0.1 0.2 0.2 1.0 0.0 0.2 0.2 0.3

# 13. Follow the patterns described in DataScience01b.R Write code to normalize the
# following vector using a Z-score normalization and present the result in the
# console. Do not remove outliers. c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
z <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- mean(z)
b <- sd(z)
normalized <- (z - a) / b
normalized # -0.1812366 -0.5437099 -0.5437099 -0.1812366 -0.1812366  2.7185494 -0.9061831 -0.1812366 -0.1812366  0.1812366

# 14. Follow the patterns described in DataScience01b.R Write code to binarize: 
# c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Red', 'Blue', 'Green', 'Blue') and present the result in the console
colors <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Red', 'Blue', 'Green', 'Blue')
isRed <- colors == 'Red'
isGreen <- colors == 'Green'
isBlue <- colors == 'Blue'

isRed # TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
isGreen # FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
isBlue # FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE

# 15. Follow the patterns described in DataScience01b.R Write code to discretize the
# following vector into 3 bins of equal range and present the result in the console.
# Do not remove outliers. c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized
xDiscretized[bin1Min < x & x <= bin1Max] <- "Low"
xDiscretized[bin1Max < x & x <= bin2Max] <- "Middle"
xDiscretized[bin2Max < x & x <= bin3Max] <- "High"
xDiscretized
# [1] "High" "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low" 
# [10] "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low" 
# [19] "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low"  "Low" 
# [28] "Low" 

# 16. The following is a vector of ages of people in a kindergarten class. Included are
# some older siblings, teachers, and somebody’s grandfather. Discretize this vector
# into 3 bins of equal of near equal number of people. No Code is necessary, just
# present the results as commented text in the R file. c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
ages <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
sort(ages)
# Low: 3  3  4  4  5  5  5  5  5  5  5  6  6  6  6  6  7  7  7  7  8  8  9 12 
# Middle: 24 24 25 
# High: 81
