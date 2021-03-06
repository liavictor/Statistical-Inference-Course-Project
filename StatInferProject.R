# In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. 
# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. 
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. 
# Note that you will need to do a thousand simulations.
# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials. You should

# R environment and reproducibility

sessionInfo()

set.seed(2021)

# Load required libraries

library(stats)
library(ggplot2)
library(sqldf)
library(dplyr, warn.conflicts = F)

# Part 1 (P1) Simulation Exercise

## P1 Objectives

# + Show the sample mean and compare it to the theoretical mean of the distribution.
# + Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# + Show that the distribution is approximately normal.

## P1: Setup variables and create simulation dataset

# investigate the distribution of averages of 40 exponentials
n <- 40
# set lambda = 0.2 for all of the simulations
lambda <- 0.2
# need to do a thousand simulations 
nosim <- 1000
# Create simulations dataset
simdata <- matrix(rexp(n*nosim, lambda), nrow = nosim, ncol = n)

## P1: Compare sample means to theoretical means of the distribution

mns <- apply(simdata,1,mean)
hist(mns,col="light blue",breaks=50, xlab = "Mean",main = "Distribution of simulated means")
abline(v=1/lambda,col="red",lwd=4)
abline(v=round(mean(mns),3),col="green",lwd=3)
legend('topright', c("theoretical mean","avg sample mean"), col=c("red", "green"), lty=c(1,1), lwd=c(3,3), bty = "n")
comMeans<-paste('sample mean =', round(mean(mns),3), ', theoretical mean=', 1/lambda, sep = "", collapse = NULL)
comMeans

# Above histogram shows: the sample mean is very closed to theoretical mean of distribution 
# when having enough samples of the exponential distribution.

## P1: Compare sample variance to the theoretical variance of the distribution

vars <- apply(simdata,1,var)
hist(vars,col="pink",breaks=50, xlab = "Variance", main = "Distribution of simulated variances")
abline(v=1/lambda^2,col="red",lwd=3)
abline(v=round(mean(vars),3),col="yellow",lwd=2)
legend('topright', c("theoretical variance","sample variance"), col=c("red", "yellow"), lty=c(1,1), lwd=c(3,3), bty = "n")
comVars<-paste('sample variance =', round(mean(vars),3), ', theoretical variance=', 1/lambda^2, sep = "", collapse = NULL)
comVars

# Above histogram shows: the sample variance is very closed to theoretical variance of distribution 
# when having enough samples of the exponential distribution.

## P1: Compare the results to a normal distribution

## step 1 draw the histgram 
hist(mns,  prob=T,
     main="Density distributions between sample and normal",
     xlab="Sample means from 1000 simulations")

# step 2 draw simulated sample mean density distribution
lines(density(mns), col="green", lty=1)

# step 3 draw therotical mean desnity distibution
x <- seq(min(mns), max(mns), length=100)
y <- dnorm(x, mean=1/lambda, sd=(1/lambda/sqrt(n)))
lines(x, y, col="red", lty=1)

# step 4 draw both sample mean and theoretical Mean
abline(v=1/lambda, col='red', lwd=2)
abline(v=round(mean(mns),3),col="green",lwd=2)

# step 5 insert legends
legend('topright', c("simulation density", "theoretical density"), 
       lty=c(1,1), col=c("green", "red"), lwd=c(2,2))

# Above histogram with overlay of sample/normal curves shows: the two distribution curves 
# are very similar and normally distributed.

## P1 Conclusion 

# + The sample mean is very close to the theoretical mean of the distribution.
# + The sample variance is very close to the theoretical variance of the distribution.
# + The sample distribution is approximately normal.

# Part 2 (P2) Analyze the tooth growth

## P2 Objectives
# + Load the ToothGrowth data and perform some basic exploratory data analyses.
# + Provide a basic summary of the data.
# + Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
# + State your conclusions and the assumptions needed for your conclusions

## P2: Load data and basic exploratory analyses

data(ToothGrowth)
str(ToothGrowth)

summary(ToothGrowth)

qplot(dose, len, data = ToothGrowth, color = supp, geom = "point") +  
        geom_smooth(method = "lm") + 
        labs(title = "Basic exploratory data analyses - tooth growth ") + 
        labs(x = "Supplement dose", y = "Teeth growth length ")

# Findings from the above plot:
#  1. When supplement dose increases, the teeth length goes up for both OJ and VC.
#  2. Supplement OJ has a higher length increase than VC at the lower dose.

## P2: Provide a basic summary of the data

GroupSumStats <- sqldf('select supp, dose, max(len) as Max, min(len) as Min,
                        median(len) as Median, avg(len) as Mean, 
                        round(stdev(len),2) as stdev, count(*) as n 
                        from ToothGrowth group by supp, dose')
GroupSumStats

#update dose as factor for analysis
ToothGrowth$dose<-as.factor(ToothGrowth$dose)

ggplot(aes(x=dose, y=len), data=ToothGrowth) + 
        geom_boxplot(aes(fill=dose)) + 
        xlab("Dose") +
        ylab("Tooth growth length") + 
        facet_grid(~ supp) + 
        ggtitle("Tooth length by supplement type and dose")

ggplot(aes(x=supp, y=len), data=ToothGrowth) + 
        geom_boxplot(aes(fill=supp)) + 
        xlab("Supplement type") +
        ylab("Tooth growth length") + 
        facet_grid(~ dose) + 
        ggtitle("Tooth length by dose and supplement type") 

## P2: Compare tooth growth by supplement type and dose

### P2: tooth growth by supplement type


## OJ and VC data
OJ <- filter(ToothGrowth, supp == 'OJ')$len
VC <- filter(ToothGrowth, supp == 'VC')$len

## t- test by supplement type OJ and VC
t.test(OJ, VC, paired = FALSE, var.equal = FALSE)


# Note: the p-value of this test is 0.06, which is greater than 0.05 and the confidence interval 
# of the test contains zero. therefore, the supplement types seems to have no impact on tooth 
# growth based on this test result.

### P2: tooth growth by dose


##  data by dose: 0.5,1.0 and  2.0
dose_0.5 <- filter(ToothGrowth, dose == 0.5)$len
dose_1.0 <- filter(ToothGrowth, dose == 1.0)$len
dose_2.0 <- filter(ToothGrowth, dose == 2.0)$len

## t- test by dose 0.5 and 1.0
t.test(dose_0.5, dose_1.0, paired = FALSE, var.equal = FALSE)

## t- test by dose 0.5 and 2.0
t.test(dose_0.5, dose_2.0, paired = FALSE, var.equal = FALSE)

## t- test by dose 1.0 and 2.0
t.test(dose_1.0, dose_2.0, paired = FALSE, var.equal = FALSE)

# Note: from above three tests: the p-value of each test was essentially zero and 
# the confidence interval of each test does not cross over zero (0).  Therefore, 
# the average tooth length increases with an inceasing dose.


## P2: Conclusion
# + Supplement type has no effect on tooth growth.
# + The dose level increasing leads to increased tooth growth.
