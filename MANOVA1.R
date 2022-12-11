## Load Libraries 
library("mvnormtest")
library("car")
library("IDPmisc")

# Load in data 
Kickstarter <- read.csv("/Users/christinasimbenga/Desktop/kickstarter.csv")


# Question set up 
#Does the country the project originated in influence the number of backers 
#and the amount of money pledged?

#Data Wrangling 

#Ensure Variables are Numeric
str(Kickstarter$pledged)
str(Kickstarter$backers)

# Change them to numeric 
Kickstarter$pledged <- as.numeric(Kickstarter$pledged)
Kickstarter$backers <- as.numeric(Kickstarter$backers)

# Subsetting 

keeps <- c("pledged", "backers")
Kickstarter1 <- Kickstarter[keeps]
#Limit the number of rows 
kickstarter2 <- Kickstarter1[1:5000,]


# Format as a matrix 
Kickstarter3 <- as.matrix(kickstarter2)

# Test Assumptions 

# Sample Size <- We are good here have plently of data 

# Multivariate Normality 
#Taking out missing data first 
Kickstarter4 <- na.omit(Kickstarter3)

mshapiro.test(t(Kickstarter4))

#This violates the assumption of multivariate continuing for learning purposes 

# Homogeneity of variance 
leveneTest(Kickstarter$pledged, Kickstarter$country, data=Kickstarter)
leveneTest(Kickstarter$backers, Kickstarter$country, data=Kickstarter)

# This violates the Homogeneity of Variance continuing for learning purposes only 


# Absence of Multicollinearity 

cor.test(Kickstarter$pledged, Kickstarter$backers, method = "pearson", use="complete.obs")
cor.test(Kickstarter$pledged, Kickstarter$backers, method="pearson", use="complete.obs")


# This is above .7 meaning we have presence of multicollinearity 

# The analysis 

MANOVA <- manova(cbind(pledged, backers) ~ country, data= Kickstarter)
summary(MANOVA)

# Post Hocs 
# ANOVAs as Post Hocs 

summary.aov(MANOVA, test = "wilks")


























