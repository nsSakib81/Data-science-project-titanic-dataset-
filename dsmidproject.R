mydata<-read.csv("D:/projectData.csv",header= TRUE,sep=",")
mydata


is.na(mydata)

mydata


columns_to_impute <- c("Gender", "age", "sibsp", "parch", "survived")

for (col in columns_to_impute) {
  mydata[[col]][is.na(mydata[[col]])] <- mean(mydata[[col]], na.rm = TRUE)
}


mydata

mydata$Gender <-as.integer(mydata$Gender)
mydata$age <-as.integer(mydata$age)

mydata


mydata$fare <- as.numeric(mydata$fare)
mydata

mean_fare <- mean(mydata$fare, na.rm = TRUE)
mydata$fare[is.na(mydata$fare)] <- mean_fare


mydata$fare <- as.integer(mydata$fare)


mydata


mydata$class<- ifelse(mydata$class=="Frist",1,
                      ifelse(mydata$class=="Second",2,
                             ifelse(mydata$class=="Third",3,NA)))

mydata

mydata$class[is.na(mydata$class)]<-mean(mydata$class,na.rm = TRUE)
mydata$class<-as.integer(mydata$class)
mydata

mydata$class<- ifelse(mydata$class==1,"Frist",
                      ifelse(mydata$class==2,"Second",
                             ifelse(mydata$class==3,"Third",NA)))

mydata

sum(is.na(mydata))





Q1 <- quantile(mydata$age, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$age, 0.75, na.rm = TRUE)

IQR <- Q3 - Q1
lower_fence <- round(Q1 - 1.5 * IQR)
upper_fence <- round(Q3 + 1.5 * IQR)

outliers <- mydata$age[mydata$age < lower_fence | mydata$age > upper_fence]
print("Outliers:")
print(outliers)

dataset_cleaned <- mydata$age
dataset_cleaned[dataset_cleaned < lower_fence] <- lower_fence
dataset_cleaned[dataset_cleaned > upper_fence] <- upper_fence

("Cleaned dataset:")
(dataset_cleaned)





str(mydata)

summary(mydata)

nrow(mydata)
ncol(mydata)

table(mydata$Gender)

hist(mydata$age)

plot(mydata$age, mydata$fare)

boxplot(mydata$fare ~ mydata$class)

