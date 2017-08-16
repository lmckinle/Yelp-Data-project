yelp_all <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/yelp project/business.csv", stringsAsFactors = FALSE)
str(yelp_all)
head(yelp_all)

yelp_att <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/yelp project/businessAttributes.csv", stringsAsFactors = FALSE)

yelpAZ <- yelp_all[yelp_all$state=="AZ",]
yelpON <- yelp_all[yelp_all$state=="ON",]

head(yelpON$name)

yelpAZ$restaurant <- 0
for (i in 1:length(yelpAZ$categories)){
	if (grepl(pattern = "Restaurants", ignore.case=TRUE, yelpAZ$categories[i])==TRUE){
		yelpAZ$restaurant[i] <- 1
	} else yelpAZ$restaurant
}
head(yelpAZ$restaurant)

yelpON$restaurant <- 0
for (i in 1:length(yelpON$categories)){
	if (grepl(pattern = "Restaurants", ignore.case = TRUE, yelpON$categories[i])==TRUE){
		yelpON$restaurant[i] <- 1
	} else yelpON$restaurant
}
head(yelpON$restaurant, 50)

AZrest <- subset(yelpAZ, yelpAZ$restaurant == 1)
head(AZrest)

ONrest <- subset(yelpON, yelpON$restaurant == 1)
length(ONrest$name)

AZrest <- merge(AZrest[, 1:11], yelp_att, by="business_id", all.x=TRUE)
head(AZrest)

ONrest <- merge(ONrest[, 1:11], yelp_att, by="business_id", all.x=TRUE)
head(ONrest)

head(ONrest$name)
length(ONrest$business_id)
length(AZrest$business_id)

str(ONrest)


rm(yelp_all)
rm(yelpAZ)
rm(yelpON)
rm(yelp_att)
rm(AZrest)
rm(ONrest)


##-----------------------------------##
##read in the review data##
azReviews <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/Yelp Project/reviews_azo.csv", stringsAsFactors = FALSE) 
head(azReviews)
str(azReviews)
azReviews <- azReviews[,c(1, 2, 4, 6, 7, 9)]

##playing with the reviews some...
hist(azReviews$useful)##maybe there's some outliers here??
qplot(x=useful, y=stars, data=azReviews)


summary(lm(stars ~ useful, data=azReviews))


##-----------------------------------##
##downloading RSentiment package##

install.packages("RSentiment")
library("RSentiment")
require("RSentiment")

firsttry <- calculate_custom_sentiment(azReviews[2,5], c("good"),c("bad"))
firsttry

azReviews[1:20 , ]
secondtry <- calculate_custom_sentiment(azReviews[1:10, 5], good.food, bad.food)
secondtry

##trying to read through all reviews and bin them##

good.food <- c("delicious", "tasty", "authentic", "yummy")
bad.food <- c("bland", "gross", "yuck", "tasteless", "dry", "inedible")

azReviews$foodSent <- rep("NA", nrow(azReviews))
az.foodRV <- c("NA", "NA)

library("data.table")
azReviews <- data.table(business_id = character(1094414), 
				cool = integer(1094414),
				date = as.Date(1:1094414, origin=Sys.Date()),
				funny = integer(1094414),
				review_id = character(1094414),
				stars = integer(1094414),
				text = character(1094414),
				type = character(1094414),
				useful = integer(1094414),
				user_id = character(1094414))
n <- 5
s <- n-1
azReviews <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/Yelp Project/reviews_azo.csv", 
		stringsAsFactors = FALSE, nrows=n)
for (i in 1){
  azReviews[i] <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/Yelp Project/reviews_azo.csv", 
		stringsAsFactors = FALSE, nrows=5) 
  az.foodRV <- calculate_custom_score(azReviews[c(1:5), 7], good.food, bad.food)
  if (az.foodRV[2] == "Very Positive"){
      azReviews$text[i] <- 5
  } else if (az.foodRV[2] == "Positive"){
    	azReviews$text[i] <- 4
  } else if (az.foodRV[2] == "Neutral"){
    	azReviews$text[i] <- 3
  } else if (az.foodRV[2] == "Negative"){
    	azReviews$text[i] <- 2
  } else if (az.foodRV[2] == "Very Negative"){
    	azReviews$text[i] <- 1
  } else {
	azReviews$text[i] <- 0}
}
az.foodRV
head(azReviews)


good.service <- c("prompt", "friendly", "fast", "attentive")
bad.service <- c("rude", "slow", "forgot", "argue")

good.overall <- c("satisfied", "return", "great", "good", "exceptional", "authentic")
bad.overall <- c("overpriced", "busy", "wait", "ripped off", "terrible")





head(azReviews)









