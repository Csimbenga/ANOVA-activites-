# One way between subjects ANOVAs in R 

# load libraries 

library("dplyr")
library("rcompanion")
library("car")

# load data 

apps <- read.csv("/Users/christinasimbenga/Desktop/NEWgoogleplaystore.csv")

# Question setup 
#Is there a difference in price among the three app 
# categories of beauty, food and drink, and photography? 

# Data wrangling 

# filter the data and remove missing values 
apps1<- na.omit(apps %>% filter(Category %in% c("BEAUTY", "FOOD_AND_DRINK", "PHOTOGRAPHY")))

# make the price numeric

apps1$Price <- as.numeric(apps1$Price)

# Test the assumptions 

# Normality 
plotNormalHistogram(apps1$Price)
# using SQRT 
apps1$PriceSQRT <- sqrt(apps1$Price)
plotNormalHistogram(apps1$PriceSQRT)
# Using CUBE as SQRT did not make it any better 
apps1$PriceCUBE <- apps1$Price ^ 3
plotNormalHistogram(apps1$PriceCUBE)
# wow that did nothing 
# might as well keep the original data to ease interpretation


# Homogeneity of Variance

# Bartlett's Test
bartlett.test(Price ~ Category, data=apps1)

# Fligner's Test
fligner.test(Price ~ Category, data=apps1)

# WE have violated the assumption of homogeneity of variance

# Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

apps1ANOVA <- lm(Price ~ Category, data=apps1)
Anova(apps1ANOVA, Type="II", white.adjust=TRUE)

# Post Hocs
# Computing Post Hocs with No Adjustment

pairwise.t.test(apps1$Price, apps1$Category, p.adjust="none")

# Computing Post Hocs with Bonferroni Adjustment

pairwise.t.test(apps1$Price, apps1$Category, p.adjust="bonferroni")

# Computing Post Hocs When You've Violated the Assumption of Homogeneity of Variance

pairwise.t.test(apps1$Price, apps1$Category, p.adjust="bonferroni", pool.sd = FALSE)

# Determine Means and Draw Conclusions
apps1Means <- apps1 %>% group_by(Category) %>% summarize(Mean = mean(Price))
apps1Means



