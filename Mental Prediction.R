df<-read.csv("C:/Users/Shweta/Downloads/Health.csv")
sum(is.na(df))
mean(df[,15],na.rm=TRUE)
median(df[,15],na.rm=TRUE)
mean(df[,25],na.rm=TRUE)
median(df[,25],na.rm=TRUE)
pct<-round(100*df$Employment.Status/sum(df$Employment.Status))
pie(table(df$Employment.Status,labels=paste(df$Employment.Status,sep=" ",pct,"%")))
barplot(table(df$Has.the.COVID.19.pandemic.affected.your.mental.well.being.,xlab="n",ylab="u"))
df[is.na(df$Has.the.COVID.19.pandemic.affected.your.mental.well.being.)]<-"Maybe"
df$Has.the.COVID.19.pandemic.affected.your.mental.well.being.<- replace(df$Has.the.COVID.19.pandemic.affected.your.mental.well.being., df$Has.the.COVID.19.pandemic.affected.your.mental.well.being.=='', "Maybe")
barplot(table(df$eating.and.sleeping))


x<-table(df$Has.there.been.a.sudden.and.huge.change.in.your.life.)
x
piepercent<- round(100*x/sum(x), 1)
pie(x, labels = piepercent)
pie(x, labels = piepercent,col = rainbow(length(x)))
legend("topright", c("No","Not sure","Yes"), cex = 0.8,fill = rainbow(length(x)))


df[is.na(df$How.many.hours.do.you.spend.per.day.on.watching.mobile.phone..laptop..computer..television..etc..)]<-"2-5 hours"
x<-table(df$How.many.hours.do.you.spend.per.day.on.watching.mobile.phone..laptop..computer..television..etc..)
pie(x)




y<-table(df$Your.stress.is.related.to.which.of.the.following.areas.)
y
plot(y,type="o")

support=df$Describe.how..supported..you.feel.by.others.around.you...your.friends..family..or.otherwise.
q<-table(support)
barplot(q)


df$Do.you.feel.bad.about.yourself...or.that.you.are.a.failure.or.have.let.yourself.or.your.family.down.<- replace(df$Do.you.feel.bad.about.yourself...or.that.you.are.a.failure.or.have.let.yourself.or.your.family.down., df$Do.you.feel.bad.about.yourself...or.that.you.are.a.failure.or.have.let.yourself.or.your.family.down.=='', "Maybe")
interference=df$If.you.have.a.mental.health.condition..do.you.feel.that.it.interferes.with.your.work.
a=table(interference)
barplot(a,col=rainbow(length(a)))


df$How.often.do.you.get.offended.or.angry.or.start.crying..<- replace(df$How.often.do.you.get.offended.or.angry.or.start.crying..,df$How.often.do.you.get.offended.or.angry.or.start.crying..=='', "Often")
moodswings=df$How.often.do.you.get.offended.or.angry.or.start.crying..
x<-table(moodswings)
x
piepercent<- round(100*x/sum(x), 1)
pie(x, labels = piepercent)
pie(x, labels = piepercent,col = rainbow(length(x)))
legend("topright", c("Never","Often","Sometimes","Very Often"), cex = 0.8,fill = rainbow(length(x)))


#1
x<-table(df$Gender)
piepercent<- round(100*x/sum(x), 1)
pie(x, labels = piepercent)
pie(x, labels = piepercent,col = rainbow(length(x)))
legend("topright", c("Female","Male","Do not prefer to say"), cex = 0.8,fill = rainbow(length(x)))


colname


#2

x<-table(df$How.are.you.feeling.today.)
x
boxplot(x,xlab="Mental Feeling",ylab="Count")


df$How.likely.do.you.feel.yourself.vulnerable.or.lonely.

#df_test<-df[samp,]
#df_ctrl<-df[-samp,]
#ggpairs(df_test)


#df<-df[1:120,31]
#set.seed(100)
#samp<-sample(1:100,80)
#df_test<-df[samp,]
#df_ctrl<-df[-samp,]

#ML
df<read.csv("C:/Users/Shweta/Downloads/health.csv")
#Splitting
split<-sample.split(df,SplitRatio=0.7)
train_cl<-subset(df,split=="TRUE")
test_cl<-subset(df,split=="FALSE")
#Feature Scaling
train_scale<-scale(train_cl[,c(15,25,28)])
test_scale<-scale(test_cl[,c(15,25,28)])
#Fitting KNN Model
classifier_knn<-knn(train=train_scale,test=test_scale,cl=train_cl$Prediction_status,k=1)
classifier_knn
#Confusion matrix
cm<-table(test_cl)











