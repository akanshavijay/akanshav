# Personal Loan Campaign 
setwd("C:/Users/akansha v/Desktop/R Programming")
library(readxl)
library(readr)
library(rlang)
bankloan = read_excel("Thera Bank_Personal_Loan_Modelling-dataset.xlsx")
dim(bankloan)
ncol(bankloan)
names(bankloan)
str(bankloan)
summary(bankloan)
head(bankloan)
attributes(bankloan)
attach(bankloan)


## convert binary variables into factor
bankloan$`Personal Loan` = as.factor(bankloan$`Personal Loan`)
bankloan$`Securities Account`= as.factor(bankloan$`Securities Account`)
bankloan$`CD Account` = as.factor(bankloan$`CD Account`)
bankloan$Online = as.factor(bankloan$Online)
bankloan$CreditCard = as.factor(bankloan$CreditCard)
bankloan$Education = as.factor(bankloan$Education)

attach(bankloan)
cols = c()

### check missing values

table(is.na(bankloan))
colSums(is.na(bankloan))

### treat na values

bankloan[is.na(bankloan)] = 0
summary(bankloan)

### check outliers

boxplot(bankloan[,2:14])


### Univariate analysis
### creating plot of age using ggplot

library(ggplot2)
ggplot(aes(x= `Age (in years)`), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('`Age (in years)`')+ ylab('Number of customers')
ggplot(aes(x= `Experience (in years)`), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('`Experience (in years)`')+ ylab('Number of customers')
ggplot(aes(x= `Income (in K/month)`), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('`Income (in K/month)`')+ ylab('Number of customers')
ggplot(aes(x= `ZIP Code`), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('`ZIP Code`')+ ylab('Number of customers')
ggplot(aes(x= `Family members`), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('`Family members`')+ ylab('Number of customers')
ggplot(aes(x= Mortgage), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('Mortgage')+ ylab('Number of customers')
ggplot(aes(x= CCAvg), data = bankloan) + geom_histogram(binwidth = 1.25,color = 'black', fill = 'pink')+ xlab('CCAvg')+ ylab('Number of customers')

###ploting categorical variables

table(bankloan$Education)
ggplot(bankloan, aes(x= bankloan$Education))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(bankloan$`Personal Loan`)
ggplot(bankloan, aes(x= bankloan$`Personal Loan`))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(bankloan$`Securities Account`)
ggplot(bankloan, aes(x= bankloan$`Securities Account`))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(bankloan$`CD Account`)
ggplot(bankloan, aes(x= bankloan$`CD Account`))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(bankloan$Online)
ggplot(bankloan, aes(x= bankloan$Online))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(bankloan$CreditCard)
ggplot(bankloan, aes(x= bankloan$CreditCard))+ geom_bar()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


### bivariate analysis


plot(`Personal Loan`,`Age (in years)`, main = "Scatter plot",
     xlab = "Personal Loan", ylab = "Age",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`Experience (in years)`, main = "Scatter plot",
     xlab = "Personal Loan", ylab = "Experience ",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`Income (in K/month)`, main = "Scatter plot",
     xlab = "Personal Loan", ylab = "Income",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`ZIP Code`, main = "Scatter plot",
     xlab = "Personal Loan", ylab = "Zip",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`Family members`, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "Family Members",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,CCAvg, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "CCAvg",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,Education, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "Education",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,Mortgage, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "Mortgage",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`Securities Account`, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "Securities Account",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,`CD Account`, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "CD Account",
     pch = 19, frame = FALSE)

plot(`Personal Loan`,Online, main = "Scatter plot",
     xlab = "X Personal Loan", ylab = "online",
     pch = 19, frame = FALSE)



### kmeans clustering

seed=1000

set.seed(seed)
bankloan.scaled = scale(bankloan[,2:14])
print(bankloan.scaled)
summary(bankloan.scaled)
clust2 = kmeans(bankloan.scaled, centers = 2, nstart = 5)
print(clust2)

library(cluster)
clusplot(bankloan.scaled,clust2$cluster, color = TRUE, shade = TRUE, labels = 2, lines=1)
totwss = rep(0,5)
for(k in 1:5){
  set.seed(seed)
  clust = kmeans(x = bankloan.scaled, centers = k, nstart = 5)
  totwss[k] = clust$tot.withinss 
}
print(totwss)
plot(c(1:5), totwss, type = "b")

library(NbClust)
set.seed(seed)
nc = NbClust(bankloan[,-1], min.nc = 2, max.nc = 5, method = "kmeans")
print(nc)
table(nc$Best.n[1,])
clust2$centers
table(bankloan$`Personal Loan_c`,clust2$cluster)
clustprofile = aggregate(bankloan[, -c(1,8)], list(bankloan$cluster), FUN = "mean")
print(clustprofile)

### cart
library(caTools)
set.seed(1000)
sample = sample.split(bankloan, SplitRatio = 0.70)
train = subset(bankloan[,-1], sample== TRUE)
test = subset(bankloan[,-1], sample== FALSE)
str(train)
summary(train)
head(train)
dim(train)
nrow(train)
sum(train$`Personal Loan`=="1")/nrow(train)
plot(train[,-9])
points(train$`Age (in years)`[train$`Personal Loan`=="1"], train$`Experience (in years)`[train$`Personal Loan`=="1"], train$`Income (in K/month)`[train$`Personal Loan`=="1"],train$`ZIP Code`[train$`Personal Loan`=="1"],train$`Family members`[train$`Personal Loan`=="1"],train$CCAvg[train$`Personal Loan`=="1"],train$Education[train$`Personal Loan`=="1"],train$Mortgage[train$`Personal Loan`=="1"],train$`Securities Account`[train$`Personal Loan`=="1"],train$`CD Account`[train$`Personal Loan`=="1"],train$Online[train$`Personal Loan`=="1"],train$CreditCard[train$`Personal Loan`=="1"],col="blue", pch=19)
attach(train)
library(rpart)
library(rpart.plot)

tree = rpart(formula = `Personal Loan` ~ ., data = train, method = "class", minbucket= 3, cp=0)
summary(tree)
tree1 = rpart.plot(tree)
printcp(tree)
plotcp(tree)
ptree = prune(tree,cp = 0.015,"CP")
printcp(ptree)
rpart.plot(ptree)
ptree

test$cart.pred = predict(tree, test, type = 'class')
t = table(predictions = test$cart.pred , actual = test$`Personal Loan`)
sum(diag(t))/sum(t)

train$cart.pred = predict(tree, train, type = 'class')
t1 = table(predictions = train$cart.pred, actual = train$`Personal Loan`)
sum(diag(t1))/sum(t1)

install.packages("pROC")
library(pROC)
predict(tree, newdata = test)

test$cart.score = predict(tree, test, type = 'prob')
auc = auc(test$`Personal Loan`, test$cart.score[,2])
auc
plot(roc(test$`Personal Loan`, test$cart.score[,2]))

train$cart.score = predict(tree, train, type = 'prob')
auc = auc(train$`Personal Loan`, train$cart.score[,2])
auc
plot(roc(train$`Personal Loan`, train$cart.score[,2]))


### confusion matrix for cart

cart_cm_train = table(train$`Personal Loan`, train$cart.pred)
cart_cm_test = table(test$`Personal Loan`, test$cart.pred)

### Error rate

er_train = (cart_cm_train[1,2]+ cart_cm_train[2,1])/ nrow(train)
er_test = (cart_cm_test[1,2] + cart_cm_test[2,1])/ nrow(test)

### Accuracy

accuracy_train = (cart_cm_train[1,1]+ cart_cm_train[2,2])/ nrow(train)
accuracy_test = (cart_cm_test[1,1] + cart_cm_test[2,2])/ nrow(test)

#### Random Forest

library(randomForest)
library(caTools)
summary(train)

seed = 1000
set.seed(seed)

rndforest = randomForest(train$`Personal Loan` ~ . , data = train, ntree = 501, mtry = sqrt(12), nodesize = 10, importance = TRUE)
print(rndforest)
plot(rndforest)
print(rndforest$err.rate)
print(rndforest$importance)
varImpPlot(rndforest)

### Tune the random forest

trndforest = tuneRF(x = train[,-c(1,9)], y = train$`Personal Loan`, mtryStart = 3, stepFactor = 1.5, nTreeTry =300, improve = 2,nodesize = 10, trace= TRUE, plot = TRUE, doBest = TRUE, importance= TRUE)
trndforest
train$predict = predict(trndforest,train,type = "class")
train$prob = predict(trndforest,train,type = "prob")
test$predict = predict(trndforest,test,type = "class")
test$prob = predict(trndforest,test,type = "prob")

## confusion matrix for cart

rf_cm_train = table(train$Personal.Loan, train$predict)
rf_cm_test = table(test$Personal.Loan, test$predict)

### Error rate

er_train = (rf_cm_train[1,2]+ rf_cm_train[2,1])/ nrow(train)
er_test = (rf_cm_test[1,2] + rf_cm_test[2,1])/ nrow(test)

### Accuracy

accuracy_train = (rf_cm_train[1,1]+ rf_cm_train[2,2])/ nrow(train)
accuracy_test = (rf_cm_test[1,1] + rf_cm_test[2,2])/ nrow(test)
