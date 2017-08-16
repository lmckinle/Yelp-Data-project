##THESE ARE REVIEWS FOR RESTAURANTS IN ALL CITIES THAT HAVE BEEN OPEN FOR AT LEAST 1 YEAR AND HAVE AT LEAST 10 REVIEWS##
bus_merged <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/Yelp Project/bus_merged2.csv", stringsAsFactors = FALSE)
head(bus_merged)

install.packages("sentimentr")
require("sentimentr")
?sentimentr

##looking at what the dictionary for sentimentr looks like
lexicon::hash_sentiment_jockers

foo <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/foo.csv")
head(foo)
for (i in 1:100) {
    foo<-sentiment(rt[i,6])
    rt[i,11] <- sum(foo$sentiment)/nrow(foo)
  if (i %% 100 == 0) {
    print(i)
  }
}

##test 
test1 <- sentiment(foo[4, 6], polarity_dt = lexicon::hash_sentiment_huliu)
test1
mean(test1$sentiment)


##read in Sam's sentimentr

reviewScored <- read.csv("C:/Users/Laura Mc/Documents/MSBA/MSBX 5410 Fund Data Analytics/Yelp Project/review_sentiment.csv", stringsAsFactors = FALSE)
head(reviewScored)
head(bus_merged)

##find the most heavily reviewed restaurants
##subset by state first
LVrest <- bus_merged[bus_merged$state == 'NV' ,]
head(LVrest)
head(LVrest[order(-LVrest$review_count[]),], 1)
##Las Vegas, 6414, Mon Ami Gabi, 4JNXUYY8wbaaDmk3BPzlWw

AZrest <- bus_merged[bus_merged$state == 'AZ' ,]
head(AZrest)
head(AZrest[order(-AZrest$review_count[]),], 1)
##Phoenix, 1755, Four Peaksk Brewing, JzOp695tclcNCNMuBl7oxA
##1735, Pizzeria Bianco

BWrest <- bus_merged[bus_merged$state == 'BW' ,]
head(BWrest)
head(BWrest[order(-BWrest$review_count[]),])
##Stuttgart, 133, Vapiano

EDHrest <- bus_merged[bus_merged$state == 'EDH' ,]
head(EDHrest)
head(EDHrest[order(-EDHrest$review_count[]),])
##Edinburgh, 205, Oink

ELNrest <- bus_merged[bus_merged$state == 'ELN' ,]
head(ELNrest)
head(ELNrest[order(-ELNrest$review_count[]),])
##SKIP, only 2 reviews

HLDrest <- bus_merged[bus_merged$state == 'HLD' ,]
head(HLDrest)
head(HLDrest[order(-HLDrest$review_count[]),])
##SKIP, highest has 53 reviews

ILrest <- bus_merged[bus_merged$state == 'IL' ,]
head(ILrest)
head(ILrest[order(-ILrest$review_count[]),], 2)
##Urbana, 697, Black Dog Smoke & Ale House, 9MnbQg7kfb_WgxoV0hXKSQ
##Champaign, 414, Maize Mexican Grill, VIJ2KiDKhUVhhpNylEIfog

NCrest <- bus_merged[bus_merged$state == 'NC' ,]
head(NCrest)
head(NCrest[order(-NCrest$review_count[]),], 1)
##Charlotte, 1141, Amelie's French Bakery
##Charlotte, 1123, The Cowfish Sushi Burger Bar

NYrest <- bus_merged[bus_merged$state == 'NY' ,]
head(NYrest)
head(NYrest[order(-NYrest$review_count[]),])
##SKIP, has 2 reviews

OHrest <- bus_merged[bus_merged$state == 'OH' ,]
head(OHrest)
head(OHrest[order(-OHrest$review_count[]),])
##Cleveland, 829, The Greenhouse Tavern
##Cleveland, 802, Lola

ONrest <- bus_merged[bus_merged$state == 'ON' ,]
head(ONrest)
head(ONrest[order(-ONrest$review_count[]),], 1)
##Toronto, 1145, Khao San Road, aLcFhMe6DDJ430zelCpd2A

PArest <- bus_merged[bus_merged$state == 'PA' ,]
head(PArest)
head(PArest[order(-PArest$review_count[]),], 1)
##Pittsburgh, 1249, Meat & Potatoes

QCrest <- bus_merged[bus_merged$state == 'QC' ,]
head(QCrest)
head(QCrest[order(-QCrest$review_count[]),],1)
##Montreal, 1603, Schwartz's
##Montreal, 1106, La Banquise

SCrest <- bus_merged[bus_merged$state == 'SC' ,]
head(SCrest)
head(SCrest[order(-SCrest$review_count[]),])
##Fort Mill, 247, The Flipside Cafe

WIrest <- bus_merged[bus_merged$state == 'WI' ,]
head(WIrest)
head(WIrest[order(-WIrest$review_count[]),])
##Madison, 1236, The Old Fahsioned, RJNAeNA-209sctUO0dmwuA

##Now try to plot the reviews for these restaurants
install.packages("ggplot2")
require("ggplot2")

##for WI restaurant
ggplot( data=reviewScored[reviewScored$business_id =='RJNAeNA-209sctUO0dmwuA', ], 
	aes( x=date, y=jitter(stars)))+
	geom_point( alpha=.5 )+
	geom_smooth( method="lm" )+
	theme_bw()

##for Phoenix restaurant , yay this one works!!! shows more denisty in stars over time
ggplot( data=reviewScored[reviewScored$business_id =='JzOp695tclcNCNMuBl7oxA', ], 
	aes( x=date, y=jitter(stars)))+
	labs(x='Over Time', y='Stars', title='Four Peaks Brewing AZ')+
	geom_point( alpha=.5 )+
	geom_smooth( method="lm" )+
	theme_bw()


##density
ggplot( data=topRest[topRest$name != 'Khao San Road',], 
	aes( x=date, y=jitter(review_stars), color=name))+
	labs(x='Year (Binned by Month)', y='Avg. Stars', title='Khao San Road')+
	geom_point( alpha=.5)+
	geom_smooth( method="lm" )+
	theme_bw()

ggplot( data=topRest[topRest$name == 'Maize Mexican Grill',], 
	aes( x=date, y=jitter(review_stars), color=name))+
	labs(x='Year (Binned by Month)', y='Avg. Stars', title='Maize Mexican Grill')+
	geom_point( alpha=.5, color='purple' )+
	geom_smooth( method="lm", color = 'purple' )+
	theme_bw()

ggplot( data=topRest[topRest$name == 'Four Peaks Brewing',], 
	aes( x=date, y=jitter(review_stars), color=name))+
	labs(x='Year (Binned by Month)', y='Avg. Stars', title='Four Peaks Brewing')+
	geom_point( alpha=.5, color='red' )+
	geom_smooth( method="lm", color = 'red' )+
	theme_bw()

##but does the same go for restaurants with few reviews?
str(bus_merged, 2)
newRest <-(bus_merged[bus_merged$review_count < 50 & bus_merged$review_count >20 , c(1, 2, 5, 9, 10, 13:15, 57)])
head(newRest)
str(newRest)

newRated <- merge(newRest[, c(1, 2, 2)], reviewScored[, c(1, 4, 5, 10)])
head(newRated)
newRated[newRated$business_id == '9MnbQg7kfb_WgxoV0hXKSQ' ,]
str(reviewScored)

newRated$date <- as.Date(newRated$date, "%Y-%m-%d")
newRated$year <- as.Date(cut(newRated$date, breaks = "year"))
newRated$month <- as.Date(cut(newRated$date, breaks = "month"))

newRatedMonth<- aggregate(stars ~ month*name, FUN=mean, data = newRated)
tail(newRatedMonth, 20)

ggplot( data=newRatedMonth, 
	aes( x=month, y=stars))+
	labs(x='Year (Binned by Month)', y='Avg. Stars')+
	geom_point( alpha=.15 )+
	geom_smooth()+
	theme_bw()


##thinking about building avg. stars over time, month by month
topRest <- reviewScored[reviewScored$business_id == 'aLcFhMe6DDJ430zelCpd2A' |reviewScored$business_id == '4JNXUYY8wbaaDmk3BPzlWw' | reviewScored$business_id == 'JzOp695tclcNCNMuBl7oxA'|
				reviewScored$business_id =='gG9z6zr_49LocyCTvSFg0w ' | reviewScored$business_id =='JLbgvGM4FXh9zNP4O5ZWjQ'| reviewScored$business_id == '0W4lkclzZThpx3V65bVgig' |
				reviewScored$business_id =='VIJ2KiDKhUVhhpNylEIfog' | reviewScored$business_id =='9MnbQg7kfb_WgxoV0hXKSQ',]
colnames(topRest) <- c('business_id', 'review_id', 'user_id', 'review_stars', 'date', 'useful', 'funny', 'cool', 'type', 'score')
head(bus_merged, 1)
head(bus_merged[, c(1, 2, 5, 9, 10, 16)])
topRest <- merge(topRest, bus_merged[, c(1, 2, 5, 9, 10, 16)])
head(topRest[order(topRest$score) ,])
head(order(topRest$score[topRest$name == 'Khao San Road' ,]))
str(topRest)

topRest$date <- as.Date(topRest$date, "%Y-%m-%d")
topRest$year <- as.Date(cut(topRest$date, breaks = "year"))
topRest$month <- as.Date(cut(topRest$date, breaks = "month"))

topRestmonth <- aggregate(review_stars ~ month*name, FUN=mean, data = topRest)
head(topRestmonth, 20)

newAvgMonth <- data.frame(avg = 0, month=rep(NA, nrow(topRestmonth)), name = rep(NA, nrow(topRestmonth)))

for (i in 1:nrow(topRestmonth)){
	newAvgMonth[i,2] <- as.Date(topRestmonth[i, 1])
	newAvgMonth[i,1] <- mean(topRestmonth[c(1:i), 3])
	newAvgMonth[i,3] <- topRestmonth[i, 2]
}
tail(newAvgMonth)

ggplot( data=newAvgMonth, 
	aes( x=month, y=avg, color=name))+
	labs(x='Over Time', y='Avg. Stars')+
	geom_point( alpha=.5)+
	geom_smooth()+
	theme_bw()

##for khao san road
khao <- topRestmonth[topRestmonth$name == 'Khao San Road' ,]
head(khao)
mean(khao[c(1:6), 3])
khaoMonth <- data.frame(avg = 0, month=rep(NA, nrow(khao)), name = rep(NA, nrow(khao)))

for (i in 1:nrow(khao)){
	khaoMonth[i,2] <- as.Date(khao[i, 1])
	khaoMonth[i,1] <- mean(khao[c(1:i), 3])
	khaoMonth[i,3] <- khao[i, 2]
}
tail(khaoMonth)

ggplot( data=khaoMonth, 
	aes( x=month, y=avg))+
	labs(x='Over Time', y='Avg. Stars', title = 'Khao San Road')+
	geom_point( alpha=.5, color ='red' )+
	geom_smooth( color='red')+
	theme_bw()

meat <- topRestmonth[topRestmonth$name == 'Meat & Potatoes' ,]
head(meat)
mean(meat[c(1:6), 3])
meatMonth <- data.frame(avg = 0, month=rep(NA, nrow(meat)), name = rep(NA, nrow(meat)))

for (i in 1:nrow(meat)){
	meatMonth[i,2] <- as.Date(meat[i, 1])
	meatMonth[i,1] <- mean(meat[c(1:i), 3])
	meatMonth[i,3] <- meat[i, 2]
}
tail(meatMonth)

ggplot( data=meatMonth, 
	aes( x=month, y=avg))+
	labs(x='Over Time', y='Avg. Stars', title = 'Meat & Potatoes')+
	geom_point( alpha=.5, color ='dark blue' )+
	geom_smooth( color='dark blue')+
	theme_bw()


##now sentiment over time
topSentMonth <- aggregate(score ~ month*name, FUN=mean, data = topRest)
head(topSentMonth)

newSentMonth <- data.frame(score = 0, month=rep(NA, nrow(topSentMonth)), name = rep(NA, nrow(topSentMonth)))

for (i in 1:nrow(topSentMonth)){
	newSentMonth[i,2] <- as.Date(topSentMonth[i, 1])
	newSentMonth[i,1] <- mean(topSentMonth[c(1:i), 3])
	newSentMonth[i,3] <- topSentMonth[i, 2]
}
tail(newSentMonth)

ggplot( data=newSentMonth, 
	aes( x=month, y=score, color=name))+
	labs(x='Over Time', y='Avg. Score')+
	geom_point( alpha=.5)+
	geom_smooth()+
	theme_bw()

##for Four Peaks

ggplot( data=newAvgMonth[newAvgMonth$name == 'Four Peaks Brewing',], 
	aes( x=month, y=avg))+
	labs(x='Over Time', y='Avg. Stars', title ='Four Peaks Brewing', subtitle='Average Stars')+
	geom_point( alpha=.5, color='red')+
	geom_smooth(color='red')+
	theme_bw()

ggplot( data=newSentMonth[newSentMonth$name == 'Four Peaks Brewing',], 
	aes( x=month, y=score))+
	labs(x='Over Time', y='Avg. Score', title ='Four Peaks Brewing', subtitle='Average Sentiment (-1 to 1)')+
	geom_point( alpha=.5, color='red')+
	geom_smooth(color='red')+
	theme_bw()


##with few ratings?
ggplot( data=newAvgMonth[newAvgMonth$name == 'Black Dog Smoke & Ale House',], 
	aes( x=month, y=avg))+
	labs(x='Over Time', y='Avg. Stars', title ='Black Dog Smoke & Ale House')+
	geom_point( alpha=.5, color='dark green')+
	geom_smooth(color='dark green')+
	theme_bw()
