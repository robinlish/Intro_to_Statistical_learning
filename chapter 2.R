#7.The table below provides a training data set containing six observa- tions, three predictors, and one qualitative response variable.
a <- c(0,3,0)
b <- c(2,0,0)
c <- c(0,1,3)
d <- c(0,1,2)
e <- c(-1,0,1)
f <- c(1,1,1)
x <- c(0,0,0)

## (a) Compute the Euclidean distance between each observation and thetestpoint,X1 =X2 =X3 =0.
distance <- function(x,y){
  distance <- sqrt(sum((x-y)^2))
  return(distance)
}

distance(a,x)
distance(b,x)
distance(c,x)
distance(d,x)
distance(e,x)
distance(f,x)

## (b) What is our prediction with K = 1? Why?
#green

## (c) What is our prediction with K = 3? Why?
#red

## (d) If the Bayes decision boundary in this problem is highly non- linear, then would we expect the best value for K to be large or small? Why?
# K should be small to be able to fit the non-linear distribution of observations.

# 8. This exercise relates to the College data set, which can be found in the file College.csv. It contains a number of variables for 777 different universities and colleges in the US. The variables are
install.packages("ISLR")
library(ISLR)
data("College")

## b. looking at the data, making sure there are row names and col names
dimnames(College)

## c.
###i. Use the summary() function to produce a numerical summary of the variables in the data set.
summary(College)
###ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. Recall that you can reference the first ten columns of a matrix A using A[,1:10].
pairs(College[,1:10])
###iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(College$Private, College$Outstate)
###iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities into two groups based on whether or not the proportion of students coming from the top 10% of their high school classes exceeds 50 %.
Elite=rep("No",nrow(College))
Elite[College$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(College ,Elite)
summary(college)
plot(college$Elite, college$Outstate)
###v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative vari- ables.
par(mfrow=c(2,2))
hist(college$Apps, breaks = 20)
hist(college$Accept, breaks = 20)
hist(college$Enroll, breaks = 20)
hist(college$Top10perc, breaks = 20)
par(mfrow=c(1,1))

### vi. Continue exploring the data, and provide a brief summary of what you discover.
library(ggplot2)
ggplot(data=college,aes(x=college$Apps,y=college$Enroll)) + 
  geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))

# 9. This exercise involves the Auto data set studied in the lab. Make sure that the missing values have been removed from the data.
## (a) Which of the predictors are quantitative, and which are quali- tative?
data(Auto)
summary(Auto)
dimnames(Auto)
###qualitative: origin and name

## (b) What is the range of each quantitative predictor? You can answer this using the range() function.
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)

sapply(Auto[1:6], range)
## (c) What is the mean and standard deviation of each quantitative predictor?
mean(Auto$mpg); mean(Auto$cylinders)
sd(Auto$mpg); sd(Auto$cylinders)

sapply(Auto[1:6], mean)
sapply(Auto[1:6], sd)

## (d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
auto_d <- Auto[-(10:85),]
sapply(auto_d[,1:6], range)
sapply(auto_d[,1:6], mean)
sapply(auto_d[,1:6], sd)

## (e) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.
pairs(Auto)

ggplot(data=Auto,aes(x=cylinders,y=mpg)) + 
  geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))
ggplot(data=Auto,aes(x=horsepower,y=mpg)) + 
  geom_point() + geom_smooth(method = "glm", method.args = list(family = "gaussian"))

# 10. This exercise involves the Boston housing data set.
## (a) To begin, load in the Boston data set. The Boston data set is part of the MASS library in R. 
library(MASS)
## Now the data set is contained in the object Boston.
View(Boston)
##Read about the data set:
?Boston
##How many rows are in this data set? How many columns? What do the rows and columns represent?
str(Boston)

## (b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
pairs(Boston)

## (c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
library(reshape2)
boscrim <- melt(Boston, id="crim")

ggplot(boscrim, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()
##From the graph, it's clear that there is a positive correlation between rad and crime, and a positive correlation between
##tax and crime.

corrmatrix <- cor(Boston, use="complete.obs")[1,]
class(corrmatrix)
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5][-1]

## (d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
bosgraph1 <- ggplot(Boston, aes(x=1:nrow(Boston), y=crim))
bosgraph1+ geom_point()
bosgraph2 <- ggplot(Boston, aes(x=1:nrow(Boston), y=tax))
bosgraph2+ geom_point()
bosgraph3 <- ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio))
bosgraph3+ geom_point()

## (e) How many of the suburbs in this data set bound the Charles river?
table(Boston$chas)
barplot(table(Boston$chas))

ggplot(Boston, aes(chas))+geom_bar()

##(f) What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)

##(g) Which suburb of Boston has lowest median value of owner- occupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
sort(Boston$medv, decreasing = F)[1]
Boston[which(Boston$medv==5),]
Boston[Boston$medv==min(Boston$medv),]

sapply(Boston, quantile)
##(h) In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.
nrow(Boston[Boston$rm>=7,])
nrow(Boston[Boston$rm>=8,])

