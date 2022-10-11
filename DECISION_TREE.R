#Creating the dataframe
data <- read.csv("cars.csv", header = TRUE)


#Inspect the dataset
names(data)
head(data)
tail(data)
summary(data)
str(data)

#Dimensions
nrow(data)
ncol(data)
dim(data)

#Funtion convert to factor column
data$year <- as.factor(data$year)
str(data)

#Implementing our Tress
set.seed(1234)
pd <- sample(2, nrow(data),replace = TRUE,prob = c(0.8,0.2)) 
pd
train<- data[pd==1,] 
validate<- data[pd==2,]

#Checking observations
dim(train)
dim(validate)

#Installing Party Package
install.packages("party")
library(party)
install.packages("sandwich")
library(sandwich)

#Model building of dataset
data_tree <- ctree(year~mpg+cylinders+cubicinches+hp,data = train)
print(data_tree)

#Ploting the Tree
plot(data_tree)
plot(data_tree, type="simple")

#Predicting the data
predict(data_tree)

#Frequency table
tab <- table(predict(data_tree), train$year)
print(tab)

#Accuracy and error
sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)

#Validating the model on test data
test_predict <- table(predict(data_tree, newdata= validate), validate$year)
print(test_predict)

sum(diag(test_predict))/sum(test_predict)
1-sum(diag(tab))/sum(tab)

#Ploting using rpart function
library(rpart)
library(rpart.plot)
myresults <- rpart(year~., method = "class", data = data)
rpart.plot(myresults, type = 5, fallen.leaves = TRUE, cex = 0.5)



