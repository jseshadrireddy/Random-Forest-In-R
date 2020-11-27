fraud=read.csv(file.choose())
View(fraud)
fraud$Taxable.Income<- ifelse(fraud$Taxable.Income<=30000,"risky","good")
View(fraud)
table(fraud$Taxable.Income)
summary(fraud)
sum(is.na(fraud))
###visualization on density plot
library(ggplot2)
ggplot(data=fraud,aes(x=fraud$Undergrad,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
 
##
ggplot(data=fraud,aes(x=fraud$Marital.Status,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
 
##
ggplot(data=fraud,aes(x=fraud$Taxable.Income,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
 
##
ggplot(data=fraud,aes(x=fraud$City.Population,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
  
##
ggplot(data=fraud,aes(x=fraud$Work.Experience,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
  
##
ggplot(data=fraud,aes(x=fraud$Urban,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')
##splitting the data in to train and test
set.seed(1234)
id=sample(2,nrow(fraud),prob = c(0.8,0.2),replace = T)
training=fraud[id==1,]
testing=fraud[id==2,]
library(randomForest)
fraud$Taxable.Income=as.factor(fraud$Taxable.Income)
fore=randomForest(Taxable.Income~.,data =fraud)
fore
pred1=predict(fore,newdata = testing,type = 'class')
pred1
library(caret)
con=confusionMatrix(table(pred1,testing$Taxable.Income))
con
varImpPlot(fore)
