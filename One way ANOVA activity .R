# One way between subjects ANOVAs in R activity 

# load libraries 

library("dplyr")
library("rcompanion")
library("car")

# load data 

YoutubeChannels <- read.csv("/Users/christinasimbenga/Desktop/YouTubeChannels.csv")

# Question 
#determine if there is a difference in the number of views 
#(Video.views differs between all the different grade categories (Grade)

# Data wrangling 

# Test the assumptions 

# Normality 
plotNormalHistogram(YoutubeChannels$Video.views)
# Totally positively skwed 
# using SQRT 
YoutubeChannels$Video.viewsSQRT <- sqrt(YoutubeChannels$Video.views)
plotNormalHistogram(YoutubeChannels$Video.viewsSQRT)
# it is better but still not normal 

# Using CUBE as SQRT did not make it any better 
YoutubeChannels$Video.viewsCUBE <- YoutubeChannels$Video.views ^ 3
plotNormalHistogram(YoutubeChannels$Video.viewsCUBE)
# looks like that made it worse 
# lets try to log 
YoutubeChannels$Video.viewsLOG <- log(YoutubeChannels$Video.views)
plotNormalHistogram(YoutubeChannels$Video.viewsLOG)
# well it looks better but now it looks negatively skwed 
# just out of curiouslty will run TUKS 
YoutubeChannels$Video.viewsTUK<- transformTukey(YoutubeChannels$Video.views, plotit=FALSE)
plotNormalHistogram(YoutubeChannels$Video.viewsTUK)
# beautiful and normal 


# Homogeneity of Variance

# Bartlett's Test
bartlett.test(Video.viewsTUK ~ Grade, data=YoutubeChannels)
# there is a significant difference 

# Fligner's Test
fligner.test(Video.viewsTUK ~ Grade, data=YoutubeChannels)
# There is still a significant difference 


# WE have violated the assumption of homogeneity of variance

# Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

YoutubeChannelsANOVA <- lm(Video.viewsTUK ~ Grade, data=YoutubeChannels)
Anova(YoutubeChannelsANOVA, Type="II", white.adjust=TRUE)

# There is a significant difference between the views and the grade given 

# Computing Post Hocs with Bonferroni Adjustment

pairwise.t.test(YoutubeChannels$Video.views, YoutubeChannels$Grade, p.adjust="bonferroni")

# Computing Post Hocs When You've Violated the Assumption of Homogeneity of Variance

pairwise.t.test(YoutubeChannels$Video.views, YoutubeChannels$Grade, p.adjust="bonferroni", pool.sd = FALSE)

# looks to be that all of them are significant 

# Determine Means and Draw Conclusions
YoutubeChannelsMeans <- YoutubeChannels %>% group_by(Grade) %>% summarize(Mean = mean(Video.views))
YoutubeChannelsMeans

# The more views on a specific channel the better grade is given. There are some channels that did have a grade at all assuming they are not great enough to be graded. 

