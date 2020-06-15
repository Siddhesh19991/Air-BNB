nyc<- read.csv("~/Desktop/AB_NYC_2019.csv")


str(nyc)
library(dplyr)
library(ggplot2)


sapply(nyc, function(x) sum(is.na(x)))

nyc<-nyc[-26260,]

nyc[sapply(nyc,is.character)] <- lapply(nyc[sapply(nyc, is.character)], 
                                                   as.factor)





#EDA

ggplot(nyc,aes(price))+geom_histogram()

nyc%>%select(price,room_type)%>%group_by(room_type)%>%summarise(mean(price))
#entire home has a way higer price compare to the rest


table(nyc$neighbourhood_group)
ggplot(nyc,aes(longitude,latitude))+geom_point(aes(color=neighbourhood_group))


#handling outliers
Q<-quantile(nyc$price, probs=c(.25, .75))
iqr <- IQR(nyc$price)
nyc<- nyc%>% filter(price > (Q[1] - 1.5*iqr) &  +price< (Q[2] + 1.5*iqr))




ggplot(nyc,aes(price))+geom_boxplot(aes(color=neighbourhood_group))
#manhattan prices are more

ggplot(nyc,aes(number_of_reviews ))+geom_boxplot(aes(color=neighbourhood_group))

a<-subset(nyc,neighbourhood_group=="Manhattan")
sort(table(a$neighbourhood))
#harlem has the most 


Q2<-quantile(nyc$minimum_nights, probs=c(.25, .75))
iqr2 <- IQR(nyc$minimum_nights)
nyc<- nyc%>% filter(minimum_nights> (Q[1] - 1.5*iqr) &  +minimum_nights< (Q[2] + 1.5*iqr))

nyc%>%select(minimum_nights,neighbourhood_group)%>%group_by(neighbourhood_group)%>%summarise(mean(minimum_nights))

#minimum nights satyed is more manhattan 



cor(nyc$price,nyc$number_of_reviews)

#price has no correlation with the number of reviews written

library(lubridate)
nyc$Date<-as.Date(nyc$last_review)
nyc$Date<-year(nyc$Date)
nyc$last_review<-NULL



ggplot(nyc,aes(number_of_reviews))+geom_boxplot(aes(color=neighbourhood_group))+facet_grid(~Date)
#reviews rises as the years go by 

cor<-subset(nyc,number_of_reviews!=0 & na.omit(reviews_per_month))
cor(cor$number_of_reviews,cor$reviews_per_month)
#they are correlated


nyc%>%select(host_name,calculated_host_listings_count)%>%group_by(host_name)%>%summarise(sum(calculated_host_listings_count))%>%top_n(5)

#sonder has the most.

#text analysis

low<-nyc[nyc$price<100,]
medium<-nyc[nyc$price<200,]
high<-nyc[nyc$price<=333,]

low_nlp<-low$name
medium_nlp<-medium$name
high_nlp<-high$name

library(tm)
low_docs<-Corpus(VectorSource(low_nlp))
medium_docs<-Corpus(VectorSource(medium_nlp))
high_docs<-Corpus(VectorSource(high_nlp))

inspect(low_docs[1])
inspect(medium_docs[1])
inspect(high_docs[1])

low_docs<-tm_map(low_docs,removePunctuation)
high_docs<-tm_map(high_docs,removePunctuation)

low_docs<-tm_map(low_docs,content_transformer(tolower))
high_docs<-tm_map(high_docs,content_transformer(tolower))

low_docs<-tm_map(low_docs,removeWords,stopwords("english"))
high_docs<-tm_map(high_docs,removeWords,stopwords("english"))

dtm1<-DocumentTermMatrix(low_docs)
dtm2<-DocumentTermMatrix(high_docs)

freq1<-colSums(as.matrix(dtm1))
freq2<-colSums(as.matrix(dtm2))

ord1<-order(freq1,decreasing=TRUE)
ord2<-order(freq2,decreasing = TRUE)

freq1[head(ord1)]
freq1[tail(ord1)]

freq2[head(ord2)]
freq2[tail(ord2)]


library(wordcloud)
#low price
wordcloud(names(freq1),freq1,min.freq = 500,max.words=100,colors = rainbow(20))

#high price
wordcloud(names(freq2),freq2,min.freq = 500,max.words=100,colors = rainbow(20))


#model
#splitting the data
library(caret)
nyc_new<-nyc[,-c(1,2,3,4)]
nyc_new<-nyc_new[,-c(3,4,12)]
nyc_new<-nyc_new[,-c(6,7)]
#intrain<-createDataPartition(y=nyc_new$price,p=0.7,list = FALSE)
#training<-nyc_new[intrain,]
#test<-nyc_new[-intrain,]


library(h2o)
h2o.init()

nyc_new<-as.h2o(nyc_new)
splits<-h2o.splitFrame(data=nyc_new,ratios = c(0.7,0.15))

train<-splits[[1]]
test<-splits[[2]]
testing<-splits[[3]]


y<-"price"
x<-setdiff(names(nyc_new),"price")




aml<-h2o.automl(y=y,x=x,training_frame = train,validation_frame = test,max_models = 10)

lb<-aml@leaderboard

model_id<-as.data.frame(lb$model_id)[,1]


gbm <- h2o.getModel(grep("GBM", model_id, value = TRUE)[1])

h2o.varimp(gbm)

gbm@parameters


values<-h2o.predict(gbm,newdata=testing)
best<- h2o.performance(model = gbm,newdata = testing)


#gradient boosting is the best model for this dataset 
#mse=2010.27
#rmse=44.83

