# Load Libraries
library("rcompanion")
install.packages("fastR2")
library("fastR2")
library("car")

View(breakfast)

# Subsetting the data 

breakfast1 <- breakfast[1:33,1:7]

keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
breakfast2 <- breakfast1[keeps]

# Getting the data in the right shape for the baseline measure.
breakfast3 <- breakfast2[,1:5]
breakfast3$repdat <- breakfast2$Baseline.Body.Mass..kg.
breakfast3$contrasts <- "T1"

# Getting the data in the right shape for the folow-up measure.
breakfast4 <- breakfast2[,1:5]
breakfast4$repdat <- breakfast2$Follow.Up.Body.Mass..kg.
breakfast4$contrasts <- "T2"

# Binding 
breakfast5 <- rbind(breakfast3, breakfast4)

# Testing for Normality Assumption 

plotNormalHistogram(breakfast1$Baseline.Body.Mass..kg.)
plotNormalHistogram(breakfast1$Follow.Up.Body.Mass..kg.)

# both look normal will run with it 

# Testing for Homogeneity of Variance

leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfast5)


RManova2 <- aov(repdat~contrasts+Error(Participant.Code), breakfast5)
summary(RManova2)

# not sure why it is not letting me run due to the repdat 







