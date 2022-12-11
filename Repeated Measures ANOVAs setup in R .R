# Load Libraries

library("rcompanion")
install.packages("fastR2")
library("fastR2")
library("car")

breakfast<- read.csv('/Users/christinasimbenga/Desktop/breakfast.csv')

# Overall, regardless of whether participants ate breakfast or not, 
#did people in this study show improvement in their resting metabolic rate?  

# Data Wrangling

#Removing Extra Rows
breakfast1 <- breakfast[1:33,]
# Reshaping the Data

keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
breakfast2 <- breakfast1[keeps]

breakfast3 <- breakfast2[,1:5]
breakfast3$repdat <- breakfast2$Baseline.Resting.Metabolic.Rate..kcal.d.
breakfast3$contrasts <- "T1"

breakfast4 <- breakfast2[,1:5]
breakfast4$repdat <- breakfast2$Follow.Up.Resting.Metabolic.Rate..kcal.d.
breakfast4$contrasts <- "T2"
# rebind them 
breakfast5 <- rbind(breakfast3, breakfast4)

# Testing Assumptions
# Normality
plotNormalHistogram(breakfast2$Baseline.Resting.Metabolic.Rate..kcal.d.)

plotNormalHistogram(breakfast2$Follow.Up.Resting.Metabolic.Rate..kcal.d.)

# both pretty decent 

# Homogeneity of Variance

leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfast5)

# looks like it is not significant we are good on the homogenity of variance 

# Analysis

RManova <- aov(repdat~contrasts+Error(Participant.Code), breakfast5)
summary(RManova)

#Post Hocs
#The overall test wasn't significant, so no need to worry about post hocs















