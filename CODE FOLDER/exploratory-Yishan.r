library(ggplot2)
#The goal is to come up with a simple, precise and accurate way of determing 
#...(Y) based on readily available ...(X) measurements.

#The data set contains measurements from ...n sample points...
#(252 men who had their body fat percentage accurately measured via underwater weighting)


## what is our Y?
## what is our possible set of Xs(predictors)?
setwd("/Users/MacBook/Desktop/STAT 628/module 1") # set work directory
BodyFat= read.csv("BodyFat.csv",header = T) #read data into R
attach(BodyFat) # attaches the data into R
dim(BodyFat) # obtain the dimension of the data= numberof rows/columns
colnames(BodyFat) #obtain variables names in the data 

########################STEP 1: EXPLORING RAW DATA
# 1.what i typically do first is look at the raw data to see if i see anything interesting(e.g. pricision, units of measurement, outliers,general patterns, etc.)
# 2.i also read about any background information related to the data
# 3.if the data is too large, say a million data points, i simply take multiple, small random subset of the original data and examine this subsets.

head(BodyFat) #look at the first few data points
tail(BodyFat) #look at the last few data points
summary(BodyFat) #give you a brief summary statistic of all the variables in the data

###### "need more background reading"...

########################STEP 2: VISUALIZING DATA

# next, i start looking for the "big picture" by visualizing the data with histograms, scatterplots, and boxplots.
# 1. let's start with histograms. The first plot shows one single histogram of body fat %

hist(BODYFAT, breaks=30,main = "histogram of body fat %",xlab = "bodyfat %")
# commment: breaks= inside hist() controls the resolution of the histogram.
#High breaks number indicates higher resolution
#small breaks number indicates low resolution

#you can also create multiple plots in the same window using mfrow option inside the par() function
#then plot histograms of different variables in the same window
par(mfrow=c(2,2)) #make a two-by-two plotting window
par(mgp=c(1.8,0.5,0),mar=c(3,3,1,1))
#"beautifies" plots when creating multiple figures. 
hist(BODYFAT, breaks=30,
     main = "Histogram of bodyfat%",xlab = "bodyfat%")
hist(AGE, breaks=30,
     main = "Histogram of age",xlab = "age")
hist(WEIGHT, breaks=30,
     main = "Histogram of weight",xlab = "weight(lbs)")
hist(KNEE,breaks=30,
     main = "Histogram of knee cir", xlab="knee cir(cm)")
# it seems like majority of individuals weight around 175 lbs
# from the histogram, sometimes look at subsets of the data, these help me dectect outliers or notice general patterns about the measurements
# Tangent: many companies typically use 
#SQL(database language), Python(general-purpose language),
#and R(scientific language)-----industry typically uses the packages 
#"dplyr" or "data.table" for data manipulation(i.e. merge, subset,summary statistics,etc.)

#subeset of the data whose weight is greater than 350 pounds
BodyFat[WEIGHT>350,]  
#subset the data whose weight is less than 150 and taller than 6ft(skinny, tall people)
BodyFat[WEIGHT<150 & HEIGHT>=72,]

#Outliers display
BodyFat[39,] 
BodyFat[182,] 

# Variance display
body_cir <- BodyFat[, c(8:17)]
body_cirvar <- data.frame("Type" = as.factor(colnames(body_cir)),
                          "Var" = rep(0, length(c(8:17))))
var_m <- cov(body_cir)
for (i in 1:ncol(var_m)) {
  body_cirvar[i, 2] <- var_m[i, i]
}
ggplot(data = body_cirvar) + geom_point(aes(x = Type, y = Var, size = Var)) + 
  labs(title = "Variance of each circumference measurements")

## correlation display
corrgram(BodyFat, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,
         main=" Correlation coefficient plot for original variables")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# the first two steps help me get a better picture of the data, specifically
# 1. how to clean up data(measurements incorrect?)
# 2. how measurements are take?(units, precision, etc.)
# 3. any odd individuals in the data?
# 4. general demographic information(age group, height range, etc.)

####tangent: in real life you'll spend A LOT of time doing these two steps
#ususally 70% of time.if you have a better idea of your data and can clean it at this stage,
#subsequent statistical analysis becoms much easier.


#>>>>>>>>> STEP 3:MODELING AND ANALYSIS
# try to decide on what type of statistical analysis I want to use
# look at the relationship between bodyfat% and other variables
par(mfrow=c(1,1))
plot(AGE,BODYFAT,pch=23,bg="red",cex=1.2,
     xlab = "age(yrs)",ylab = "bodyfat%",
     main = "scatterplot of bodyfat" )
#######visual questions:
#1. ordering of the x and y axes?
#2. units and scaling on y and x axes?
#3. jittering? any others?

#high-density scatter plot: when the sample size is pretty much, there can be many samplepoints
#overlapping each other, which shows covaiance of variables badly.
#solutions: a.jitter(numeric vector,factor=1)
############b. using color difference to highlight the high-density region 
smoothScatter(x=AGE ,y=BODYFAT , xlab = "age(yrs)",ylab = "bodyfat%",cex.main=0.8,
              main="high-density scatterplot")
library(hexbin)
bin<-hexbin(AGE,BODYFAT,xbins = 30)
plot(bin, xlab = "age(yrs)",ylab = "bodyfat%", main = "high-density scatterplot")

#######statistical questions:
#1.any dependencies between the two variables?
#2.elliptical pattern like the mother-daughter data?
#3.extreme points?

#########STEP 4:Diagnostics

#diagnose the SLR assumptions with residual plot and a QQ plot
#1. residual plot:1) linearity violations ;2)homoskedasticity violations;3)detecting outliers
#2. qq plot: normality



 