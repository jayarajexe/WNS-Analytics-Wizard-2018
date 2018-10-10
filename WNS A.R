getwd()
setwd("E:\\Jayaraj Books\\Data Scientist\\WNS")


library(dplyr)
library(ggplot2)
library(ggplot)
library(stats)
#library (plyr)
library (stringr)

library(caret)
library(e1071)
library(randomForest)
library(rpart)
library(elmNN)
library(kernlab)
library(pastecs)
library(gmodels)
library(car)

library(ROCR)
library(pROC)
library(gains)
library(rattle)
library(irr)
library(RColorBrewer)
library(mlr)        


# import all the datasets

train <-read.csv("E:\\Jayaraj Books\\Data Scientist\\WNS\\train_LZdllcl.csv")
test <-read.csv("E:\\Jayaraj Books\\Data Scientist\\WNS\\test_2umaH9m.csv")
sam_sub <-read.csv("E:\\Jayaraj Books\\Data Scientist\\WNS\\sample_submission_M0L0uXE.csv")


both <- merge(x=train, y=test, all = TRUE)


summary(train)
str(both)

colSums(is.na(train))
colSums(is.na(test))
colSums(is.na(both))



both$is_promoted <- as.factor(both$is_promoted)
both$KPIs_met..80. <- as.factor(both$KPIs_met..80.)
both$awards_won. <- as.factor(both$awards_won.)
both$previous_year_rating <- as.factor(both$previous_year_rating)
both$no_of_trainings <- as.factor(both$no_of_trainings)


#train_con <- subset(train, select = c(avg_training_score, age, length_of_service))
#train_cat <- subset(train, select = -c(avg_training_score, age, length_of_service,employee_id))


##Univariate Analysis, multivariate, feature engineering

summary(both)


#continuous variables

options(scipen = 100)
options(digits = 2)
stat.desc(both)

par(mfrow=c(2,2))

boxplot(both$avg_training_score)
hist(train$avg_training_score)
hist(test$avg_training_score)

p<-ggplot(both,aes(x=avg_training_score))
p+geom_histogram()

p<-ggplot(both,aes(x=age))
p+geom_histogram()

p<-ggplot(both,aes(x=length_of_service))
p+geom_histogram()



#one by one variables analysis

# Department

table(train$department)
as.matrix((prop.table(table(train$department))))

table(both$department)
as.matrix((prop.table(table(both$department))))


str(both)
summary(both$department)


both$Analytics <- if_else(both$department=="Analytics", 1,0)
both$Finance <- if_else(both$department=="Finance", 1,0)
both$HR <- if_else(both$department=="HR", 1,0)
both$Legal <- if_else(both$department=="Legal", 1,0)
both$Operations <- if_else(both$department=="Operations", 1,0)
both$Procurement <- if_else(both$department=="Procurement", 1,0)
both$RnD <- if_else(both$department=="R&D", 1,0)
both$Sales_Marketing <- if_else(both$department=="Sales & Marketing", 1,0)
both$Technology <- if_else(both$department=="Technology", 1,0)



# Region

table(train$region)
sort(table(train$region),decreasing = TRUE)

as.matrix(round(sort(prop.table(table(train$region)),decreasing = TRUE),4))
as.matrix(round(sort(prop.table(table(both$region)),decreasing = TRUE),4))
as.matrix(round(sort(prop.table(table(test$region)),decreasing = TRUE),4))


CrossTable(train$region, train$is_promoted)


both$region <- recode(both$region, "c('region_15','region_13')='region13_15'")
both$region <- recode(both$region, "c('region_26','region_31','region_27','region_16','region_11')='region_avg1'")
both$region <- recode(both$region, "c('region_4','region_28','region_23','region_17','region_25','region_12')='region_high'")
both$region <- recode(both$region, "c('region_29','region_32','region_19','region_20','region_5','region_6','region_24','region_9','region_21','region_34','region_33', 'region_18')='region_low'")
both$region <- recode(both$region, "c('region_14','region_30','region_8','region_10','region_1','region_3')='region_avg2'")


table(both$region)
CrossTable(both$region, both$is_promoted)


both$region_2 <- if_else(both$region=="region_2", 1,0)
both$region_22 <- if_else(both$region=="region_22", 1,0)
both$region_7 <- if_else(both$region=="region_7", 1,0)
both$region_avg1 <- if_else(both$region=="region_avg1", 1,0)
both$region_avg2 <- if_else(both$region=="region_avg2", 1,0)
both$region_high <- if_else(both$region=="region_high", 1,0)
both$region_low <- if_else(both$region=="region_low", 1,0)
both$region13_15 <- if_else(both$region=="region13_15", 1,0)



## Education

table(both$education)

both$edu_b <- if_else(both$education=="Bachelor's", 1,0)
both$edu_m <- if_else(both$education=="Master's & above", 1,0)
both$edu_12 <- if_else(both$education=="Below Secondary", 1,0)
both$edu_no <- if_else(both$education=="", 1,0)



## Gender

table(both$gender)

both$gender_m <- if_else(both$gender=="m", 1,0)
both$gender_f <- if_else(both$gender=="f", 1,0)



# Recruitment channel

table(both$recruitment_channel)

both$recr_otr <- if_else(both$recruitment_channel=="other", 1,0)
both$recr_ref <- if_else(both$recruitment_channel=="referred", 1,0)
both$recr_sor <- if_else(both$recruitment_channel=="sourcing", 1,0)



##No of training

table(both$no_of_trainings)

hist(train$no_of_trainings)
ggplot(both, aes(no_of_trainings, fill= department))+geom_bar()+labs(title = "stacked bar chart", x="department", y="gender")+theme_bw()
ggplot(both, aes(no_of_trainings, fill= gender))+geom_bar()+labs(title = "stacked bar chart", x="department", y="gender")+theme_bw()
ggplot(both, aes(no_of_trainings, fill= is_promoted))+geom_bar()+labs(title = "stacked bar chart", x="department", y="gender")+theme_bw()
ggplot(both, aes(no_of_trainings, fill= education))+geom_bar()+labs(title = "stacked bar chart", x="department", y="gender")+theme_bw()


table(train$no_of_trainings)
as.matrix((prop.table(table(train$no_of_trainings))))

table(both$no_of_trainings)
as.matrix((prop.table(table(both$no_of_trainings))))

CrossTable(both$no_of_trainings, both$length_of_service)

ggplot(both, aes(no_of_trainings, fill= age))+geom_boxplot()+labs(title = "stacked bar chart", x="department", y="age")+theme_bw()


p<-ggplot(train,aes(x=no_of_trainings,y=age))
p+geom_point(aes(color=gender))

p<-ggplot(train,aes(x=no_of_trainings,y=age))
p+geom_point(aes(color=is_promoted))


p<-ggplot(train,aes(x=no_of_trainings,y=length_of_service))
p+geom_point(aes(color=is_promoted))


CrossTable(both$no_of_trainings, both$is_promoted)



both$no_of_trainings <- recode(both$no_of_trainings, "c('10','9','8','7','6','5')='training5_10'")


both$train_1 <- if_else(both$no_of_trainings=="1", 1,0)
both$train_2 <- if_else(both$no_of_trainings=="2", 1,0)
both$train_3 <- if_else(both$no_of_trainings=="3", 1,0)
both$train_4 <- if_else(both$no_of_trainings=="4", 1,0)
both$train5_10 <- if_else(both$no_of_trainings=="training5_10", 1,0)




# previous year rating


table(both$previous_year_rating)
summary(train$previous_year_rating)
summary(both$previous_year_rating)



p<-ggplot(train,aes(x=previous_year_rating,y=length_of_service))
p+geom_point(aes(color=is_promoted))


p<-ggplot(train,aes(x=previous_year_rating,y=age))
p+geom_point(aes(color=is_promoted))


colSums(is.na(train))

both$previous_year_rating <- recode(both$previous_year_rating, "NA='No'")

both$previous_year_rating <- as.factor(both$previous_year_rating)

both$rating_1 <- if_else(both$previous_year_rating=="1", 1,0)
both$rating_2 <- if_else(both$previous_year_rating=="2", 1,0)
both$rating_3 <- if_else(both$previous_year_rating=="3", 1,0)
both$rating_4 <- if_else(both$previous_year_rating=="4", 1,0)
both$rating_5 <- if_else(both$previous_year_rating=="5", 1,0)
both$rating_no <- if_else(both$previous_year_rating=="No", 1,0)



## KPI met

table(both$KPIs_met..80.)
table(train$KPIs_met..80.)
str(both$KPIs_met..80.)
summary(train$KPIs_met..80.)


CrossTable(both$KPIs_met..80., both$is_promoted)


#awards won

table(both$awards_won.)
table(train$awards_won.)
str(both$awards_won.)
summary(train$awards_won.)


CrossTable(both$awards_won., both$is_promoted)



## age

table(both$age)
hist(both$age)
boxplot(both$age)
summary(both$age)

CrossTable(both$age, both$is_promoted)



both <- both%>%group_by(x2=cut(age, breaks = seq(20,65,by=5), right = F))

both$x2 <- recode(both$x2, "c('[40,45)','[45,50)')='[40,50)'")
both$x2 <- recode(both$x2, "c('[50,55)','[55,60)','[60,65)')='[50,60)'")

table(both$x2)

both$age_25 <- ifelse(both$x2=="[20,25)", 1,0)
both$age_30 <- ifelse(both$x2=="[25,30)", 1,0)
both$age_35 <- ifelse(both$x2=="[30,35)", 1,0)
both$age_40 <- ifelse(both$x2=="[35,40)", 1,0)
both$age_50 <- ifelse(both$x2=="[40,50)", 1,0)
both$age_60 <- ifelse(both$x2=="[50,60)", 1,0)


colSums(is.na(both))
summary(both)


## length of service

table(both$length_of_service)
hist(both$length_of_service)
boxplot(both$length_of_service)
summary(both$length_of_service)

CrossTable(both$length_of_service, both$is_promoted)
ggplot(both, aes(x2, length_of_service))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(department, length_of_service))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(is_promoted, length_of_service))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(is_promoted, length_of_service))+geom_col()+labs(title="Boxplot")

ggplot(train, aes(department, length_of_service))+geom_boxplot()+labs(title="Boxplot")

as.matrix((prop.table(table(both$length_of_service))))

as.matrix(round(sort(prop.table(table(both$length_of_service)),decreasing = TRUE),4))



both$service10_37 <- ifelse(both$length_of_service >10, 1, 0)
both$service8_10 <- ifelse(both$length_of_service <11 & both$length_of_service>7, 1, 0)
both$service_1 <- if_else(both$length_of_service=="1", 1,0)
both$service_2 <- if_else(both$length_of_service=="2", 1,0)
both$service_3 <- if_else(both$length_of_service=="3", 1,0)
both$service_4 <- if_else(both$length_of_service=="4", 1,0)
both$service_5 <- if_else(both$length_of_service=="5", 1,0)
both$service_6 <- if_else(both$length_of_service=="6", 1,0)
both$service_7 <- if_else(both$length_of_service=="7", 1,0)






## average training score

table(both$avg_training_score)
as.matrix(round(sort(prop.table(table(both$avg_training_score)),decreasing = TRUE),4))
CrossTable(both$avg_training_score, both$is_promoted)



hist(both$avg_training_score)
boxplot(both$avg_training_score)
summary(both$avg_training_score)

ggplot(both, aes(x2, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(department, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(gender, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(recruitment_channel, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(no_of_trainings, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(previous_year_rating, avg_training_score))+geom_boxplot()+labs(title="Boxplot")
ggplot(both, aes(length_of_service, avg_training_score))+geom_point()+labs(title="Boxplot")
ggplot(both, aes(is_promoted, avg_training_score))+geom_col()+labs(title="Boxplot")
ggplot(both, aes(is_promoted, avg_training_score))+geom_boxplot()+labs(title="Boxplot")




both <- both%>%group_by(score=cut(avg_training_score, breaks = seq(39,102,by=7), right = F))
summary(both$score)

both$score <- recode(both$score, "c('[88,95)','[95,102)')='[88,99)'")



table(both$avg_training_score)
summary(both$avg_training_score)


both$score39_46 <- ifelse(both$score=="[39,46)", 1,0)
both$score46_53 <- ifelse(both$score=="[46,53)", 1,0)
both$score53_60 <- ifelse(both$score=="[53,60)", 1,0)
both$score60_67 <- ifelse(both$score=="[60,67)", 1,0)
both$score67_74 <- ifelse(both$score=="[67,74)", 1,0)
both$score74_81 <- ifelse(both$score=="[74,81)", 1,0)
both$score81_88 <- ifelse(both$score=="[81,88)", 1,0)
both$score88_99 <- ifelse(both$score=="[88,99)", 1,0)


#####


summary(both)

both1 <- both

names(both)

both2 <- both[,-c(2:10,13,52,68)]

summary(both2)


#######

# splitting original training and test dataset


train_f <- filter(both2, is_promoted!="NA")
both2$is_promoted <- recode(both2$is_promoted, "NA=''")
test_f <- filter(both2, is_promoted=="")

####


## simple model building - logistic

names(train_f)

model1 <-glm(is_promoted~.,data=train_f[,-1],family = "binomial")
summary(model1)



model2 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
                HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
                region_2 + region_22 + region_7 + region_avg1 + 
                region_avg2 + region_high + region_low + edu_b + 
                edu_m + edu_12 + gender_m + recr_otr + 
                recr_ref + train_1 + train_2 + train_3 + train_4 + 
                rating_1 + rating_2 + rating_3 + rating_4 + rating_5 + 
                age_25 + age_30 + age_35 + age_40 + age_50 + 
                service10_37 + service8_10 + service_1 + service_2 + service_3 + 
                service_4 + service_5 + service_6 + score39_46 + score46_53 + 
                score53_60 + score60_67 + score67_74 + score74_81 + score81_88,data=train_f[,-1],family = "binomial")


summary(model2)

prediction11 <- predict(model2,newdata = test_f,type = "response")
prediction11 <- ifelse(prediction11 > 0.25,1,0)

summary(prediction11)

submission11 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction11)



submission11 <- write.csv(submission11, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission11.csv",  row.names = F)


#####50% thereshold
summary(model2)

prediction12 <- predict(model2,newdata = test_f,type = "response")
prediction12 <- ifelse(prediction12 > 0.3,1,0)

summary(prediction12)

submission12 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction12)



submission12 <- write.csv(submission12, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission12.csv",  row.names = F)


##k-fold cross validation

train_control <- trainControl(method="cv", number=10)



model3 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_avg1 + 
               region_avg2 + region_high + region_low + edu_b + 
               edu_m + edu_12 + gender_m + recr_otr + 
               recr_ref + train_1 + train_2 + train_3 + train_4 + 
               rating_1 + rating_2 + rating_3 + rating_4 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 + age_50 + 
               service10_37 + service8_10 + service_1 + service_2 + service_3 + 
               service_4 + service_5 + service_6 + score39_46 + score46_53 + 
               score53_60 + score60_67 + score67_74 + score74_81 + score81_88,data=train_f[,-1],trControl=train_control, family = "binomial")






#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(train_f),0.70*nrow(train_f),replace=F)
trainS<-train_f[index,]
testS<-train_f[-index,]



model2 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_avg1 + 
               region_avg2 + region_high + region_low + edu_b + 
               edu_m + edu_12 + gender_m + recr_otr + 
               recr_ref + train_1 + train_2 + train_3 + train_4 + 
               rating_1 + rating_2 + rating_3 + rating_4 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 + age_50 + 
               service10_37 + service8_10 + service_1 + service_2 + service_3 + 
               service_4 + service_5 + service_6 + score39_46 + score46_53 + 
               score53_60 + score60_67 + score67_74 + score74_81 + score81_88,data=trainS[,-1],family = "binomial")


summary(model2)

pred<-predict(model2,type="response",newdata=testS)

head(pred)

table(train_f$is_promoted)/nrow(train_f)
pred<-ifelse(pred>=0.085,1,0)

pred <- as.factor(pred)
kappa2(data.frame(testS$is_promoted,pred))

confusionMatrix(pred,testS$is_promoted,positive="1")




######





model3 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_22 + region_7 + region_avg1 + 
                region_high + region_low + edu_b + 
               edu_m + 
               
               rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 + 
               service10_37 + service8_10 + score39_46 + score46_53 + 
               score53_60 + score60_67 + score67_74 + score74_81 + score81_88,data=trainS[,-1],family = "binomial")




summary(model3)

pred3<-predict(model3,type="response",newdata=testS)

head(pred3)

table(train_f$is_promoted)/nrow(train_f)
pred3<-ifelse(pred>=0.07,1,0)

pred3 <- as.factor(pred3)
kappa2(data.frame(testS$is_promoted,pred3))

confusionMatrix(pred3,testS$is_promoted,positive="1")



prediction13 <- predict(model3,newdata = test_f,type = "response")
prediction13 <- ifelse(prediction13 > 0.085,1,0)

summary(prediction13)



summary(prediction13)

submission13 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction13)

submission13 <- write.csv(submission13, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission13.csv",  row.names = F)



####revamp model based on vif

######





model4 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics +  Finance +
               HR + Operations + Legal + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 +  
                region_high + region_low + edu_b + 
               edu_m + recr_otr +recr_sor + 
                 train_1 + 
               rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 + 
               service10_37 + service8_10 + service_1 + 
               score39_46 + score46_53 + 
               score53_60 + score60_67 + score67_74 + score74_81 + score81_88, data=trainS[,-1],family = "binomial")






summary(model4)

vif(model4)

#confint(model4)  



pred4<-predict(model4,type="response",newdata=testS)

head(pred4)

table(train_f$is_promoted)/nrow(train_f)
table(testS$is_promoted)/nrow(testS)
table(trainS$is_promoted)/nrow(trainS)


pred4<-ifelse(pred4>=0.085,1,0)

pred4 <- as.factor(pred4)
kappa2(data.frame(testS$is_promoted,pred4))

confusionMatrix(pred4,testS$is_promoted,positive="1")



prediction14 <- predict(model4,newdata = test_f,type = "response")
prediction14 <- ifelse(prediction14 >=0.085,1,0)

summary(prediction14)



submission14 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction14)

submission14 <- write.csv(submission14, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission14.csv",  row.names = F)



######

step(model2,direction="both")

####



##############random forest




model5 <-randomForest(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
                        HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
                        region_2 + region_22 + region_7 + region_high + region_low + 
                        edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
                        age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
                        score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
                        score74_81 + score81_88,data=trainS[,-1],family = "rf")



summary(model5)


pred5<-predict(model5,type="response",newdata=testS)

head(pred5)

table(train_f$is_promoted)/nrow(train_f)
table(testS$is_promoted)/nrow(testS)
table(trainS$is_promoted)/nrow(trainS)


#pred5<-ifelse(pred5>=0.085,1,0)

#pred5 <- as.factor(pred5)

kappa2(data.frame(testS$is_promoted,pred5))

confusionMatrix(pred5,testS$is_promoted,positive="1")



prediction15 <- predict(model5,newdata = test_f,type = "response")
#prediction15 <- ifelse(prediction15 >=0.086,1,0)

summary(prediction15)



submission15 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction15)

submission15 <- write.csv(submission15, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission15.csv",  row.names = F)




#######
# stepwise logistic



model6 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_high + region_low + 
               edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
               score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
               score74_81 + score81_88,data=trainS[,-1],family = "binomial")


summary(model6)

vif(model6)

#confint(model4)  



pred6<-predict(model6,type="response",newdata=testS)

head(pred6)

table(train_f$is_promoted)/nrow(train_f)
table(testS$is_promoted)/nrow(testS)
table(trainS$is_promoted)/nrow(trainS)


pred6<-ifelse(pred6>=0.086,1,0)

pred6 <- as.factor(pred6)
kappa2(data.frame(testS$is_promoted,pred6))

confusionMatrix(pred6,testS$is_promoted,positive="1")



prediction16 <- predict(model6,newdata = test_f,type = "response")
prediction16 <- ifelse(prediction16 >=0.086,1,0)

summary(prediction16)



submission16 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction16)

submission16 <- write.csv(submission16, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission16.csv",  row.names = F)





######
# NB model




model7 <-naiveBayes(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_high + region_low + 
               edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
               score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
               score74_81 + score81_88,data=trainS[,-1],family = "binomial")


summary(model7)
class(model7)
#vif(model6)

#confint(model4)  



pred7<-predict(model7,newdata=testS)

head(pred7)

table(train_f$is_promoted)/nrow(train_f)
table(testS$is_promoted)/nrow(testS)
table(trainS$is_promoted)/nrow(trainS)


#pred6<-ifelse(pred6>=0.086,1,0)

#pred6 <- as.factor(pred6)
kappa2(data.frame(testS$is_promoted,pred7))

confusionMatrix(pred7,testS$is_promoted,positive="1")



prediction17 <- predict(model7,newdata = test_f)
#prediction17 <- ifelse(prediction16 >=0.086,1,0)

summary(prediction17)



submission17 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction17)

submission17 <- write.csv(submission17, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission17.csv",  row.names = F)




#############

# decision tree model



model8 <-rpart(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_high + region_low + 
               edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
               score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
               score74_81 + score81_88,data=trainS[,-1],method = "class")


summary(model8)

#vif(model6)

#confint(model4)  



pred8<-predict(model8,newdata=testS)

head(pred8)

table(train_f$is_promoted)/nrow(train_f)
table(testS$is_promoted)/nrow(testS)
table(trainS$is_promoted)/nrow(trainS)


pred8<-ifelse(pred8>=0.07,1,0)

pred8 <- as.factor(pred8)
kappa2(data.frame(testS$is_promoted,pred8))

confusionMatrix(pred8,testS$is_promoted,positive="1")



prediction18 <- predict(model8,newdata = test_f,type = "response")
prediction18 <- ifelse(prediction18 >=0.085,1,0)

summary(prediction18)


#############

## changing sample size in logistic

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(train_f),0.60*nrow(train_f),replace=F)
trainS1<-train_f[index,]
testS1<-train_f[-index,]




model9 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_high + region_low + 
               edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
               score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
               score74_81 + score81_88,data=trainS1[,-1],family = "binomial")


summary(model9)

#vif(model6)

#confint(model4)  



pred9<-predict(model9,type="response",newdata=testS1)

head(pred9)

table(train_f$is_promoted)/nrow(train_f)
table(testS1$is_promoted)/nrow(testS1)
table(trainS1$is_promoted)/nrow(trainS1)


pred9<-ifelse(pred9>=0.085,1,0)

pred9 <- as.factor(pred9)
kappa2(data.frame(testS1$is_promoted,pred9))

confusionMatrix(pred9,testS1$is_promoted,positive="1")



prediction19 <- predict(model9,newdata = test_f,type = "response")
prediction19 <- ifelse(prediction19 >=0.085,1,0)

summary(prediction19)



submission19 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction19)

submission19 <- write.csv(submission19, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission19.csv",  row.names = F)





#############

## changing sample size in logistic

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(train_f),0.80*nrow(train_f),replace=F)
trainS2<-train_f[index,]
testS2<-train_f[-index,]




model20 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
               HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
               region_2 + region_22 + region_7 + region_high + region_low + 
               edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
               age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
               score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
               score74_81 + score81_88,data=trainS2[,-1],family = "binomial")


summary(model20)

#vif(model6)

#confint(model4)  



pred20<-predict(model20,type="response",newdata=testS2)

head(pred20)

table(train_f$is_promoted)/nrow(train_f)
table(testS2$is_promoted)/nrow(testS2)
table(trainS2$is_promoted)/nrow(trainS2)


pred20<-ifelse(pred20>=0.085,1,0)

pred20 <- as.factor(pred20)
kappa2(data.frame(testS2$is_promoted,pred20))

confusionMatrix(pred20,testS2$is_promoted,positive="1")



prediction20 <- predict(model20,newdata = test_f,type = "response")
prediction20 <- ifelse(prediction20 >=0.085,1,0)

summary(prediction20)



submission20 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction20)

submission20 <- write.csv(submission20, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission20.csv",  row.names = F)



##################

#model 20 with different thresholder

pred20<-predict(model20,type="response",newdata=testS2)

head(pred20)

table(train_f$is_promoted)/nrow(train_f)
table(testS2$is_promoted)/nrow(testS2)
table(trainS2$is_promoted)/nrow(trainS2)


pred21<-ifelse(pred20>=0.085,1,0)

pred21 <- as.factor(pred21)
kappa2(data.frame(testS2$is_promoted,pred21))

confusionMatrix(pred21,testS2$is_promoted,positive="1")



prediction21 <- predict(model20,newdata = test_f,type = "response")
prediction21 <- ifelse(prediction21 >=0.22,1,0)

summary(prediction21)



submission21 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction21)

submission21 <- write.csv(submission21, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission21.csv",  row.names = F)


############
##########

## Final Logistic model with 50%-50% sample size stepwise with accuracy 78.7%




#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(train_f),0.5*nrow(train_f),replace=F)
trainS3<-train_f[index,]
testS3<-train_f[-index,]




model22 <-glm(is_promoted ~ KPIs_met..80. + awards_won. + Analytics + Finance + 
                HR + Legal + Operations + Procurement + RnD + Sales_Marketing + 
                region_2 + region_22 + region_7 + region_high + region_low + 
                edu_b + edu_m + recr_ref + train_1 +rating_1 + rating_3 + rating_5 + 
                age_25 + age_30 + age_35 + age_40 +  service10_37 + service8_10 + service_1 +
                score39_46 + score46_53 + score53_60 + score60_67 + score67_74 + 
                score74_81 + score81_88,data=trainS3[,-1],family = "binomial")


summary(model22)

#vif(model6)

#confint(model4)  



pred22<-predict(model22,type="response",newdata=testS3)

head(pred22)

#xyz <- as.factor(pred22)
#summary(xyz)

table(train_f$is_promoted)/nrow(train_f)
table(testS3$is_promoted)/nrow(testS3)
table(trainS3$is_promoted)/nrow(trainS3)


summary(pred22)
pred22<-ifelse(pred22>0.085,1,0)

pred22 <- as.factor(pred22)


summary(pred22)


kappa2(data.frame(testS3$is_promoted,pred22))

confusionMatrix(pred22,testS3$is_promoted,positive="1")



prediction22 <- predict(model22,newdata = test_f,type = "response")
prediction22 <- ifelse(prediction22 >0.085,1,0)

summary(prediction22)



submission22 <- data.frame(employee_id=test_f$employee_id, is_promoted=prediction22)

submission22 <- write.csv(submission22, file = "E:\\Jayaraj Books\\Data Scientist\\WNS\\submission22.csv",  row.names = F)


###### 