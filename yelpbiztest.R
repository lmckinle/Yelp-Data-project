setwd("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics")
yelpbiztest <- read.csv("yelp_1000_business_trunc.csv", stringsAsFactors = FALSE)
str(yelpbiztest)
head(yelpbiztest)
?subset

bizAZ <- subset(yelpbiztest, state=="AZ")
str(bizAZ)
unique(bizAZ$city)

bizPA <- subset(yelpbiztest, state=="PA")
str(bizPA)
unique(bizPA$city)

bizLV <- subset(yelpbiztest, state=="NV")
str(bizLV)
unique(bizLV$city)

AZlm <- lm(stars ~ review_count, data=bizAZ)
summary(AZlm)

require("ggplot2")

qplot( x=stars, y=review_count, data=bizAZ, type="boxplot")
qplot(x=bizAZ$stars, binwidth=.5)
qplot(x=bizPA$stars, binwidth=.5)
qplot(x=bizLV$stars, y=bizLV$review_count)
qplot(x=bizLV$review_count, binwidth=.5)
qplot(x=yelpbiztest$stars, binwidth=.5)
