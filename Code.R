
########################## Exploratory Data Analysis ######################################

require(plyr)
require(ggplot2)
require(reshape2)
require(gridExtra)


dat <- read.csv("movies.csv", header = T)

dat$Director.Rating <- as.factor(dat$Director.Rating)
dat$Actor.Rating <- as.factor(dat$Actor.Rating)



# Average Budget of Movies in Each Decade

meanbudget <- tapply(dat$budget, dat$year, mean)
meanbudget <- as.data.frame(meanbudget)
meanbudget$years1 <- c("1910s", "1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s")

plot1 <- ggplot(data=meanbudget, aes(x = years1, y = meanbudget)) + 
  geom_histogram(stat="identity", fill = "blue", alpha = 0.5) +  
  coord_cartesian(ylim=c(1000, 40000000)) +
  xlab("Decades") + ylab("Average budget") + ggtitle("Average Budget of Movies in Each Decade") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 14)) 


# Avearge rating of movies in each deacade

meanrating <- tapply(dat$rating, dat$year, mean)
meanrating <- as.data.frame(meanrating)
meanrating$years1 <- c("1910s", "1920s", "1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s")

plot2 <- ggplot(data=meanrating, aes(x = years1, y = meanrating)) + 
  geom_histogram(stat = "identity", colour = "blue", fill = "red", alpha = 0.5) + 
  coord_cartesian(ylim = c(5, 8.5)) + 
  xlab("Decades") + ylab("Average rating of movies") + ggtitle("Average Rating of Movies in Each Decade")


grid.arrange(plot1, plot2, ncol=2, nrow=1)


# Type of genres over the decades

dat$Drama <- as.numeric(dat$Drama)
movie_data_sub <- dat[, c(1,7,8,9,10,11,23,24,25,26,27,28)]
movie_data_sub <- melt(movie_data_sub, c(1,2,3,4,5,6))
names(movie_data_sub)[7] <- c("Genre")
movie_data_sub <- subset(movie_data_sub, value == 1)

plot3 <- ggplot(movie_data_sub, aes(year)) + geom_bar(fill = "red", alpha = 0.4) + 
  facet_wrap(~ Genre) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
                              plot.title = element_text(size = 20, face = "bold"),
                              axis.title.x = element_text(size = 20, face = "bold"),
                              axis.title.y = element_text(size = 20, face = "bold")) +
  xlab("Decades") + ylab("Number of Movies") + ggtitle("Distribution of Each Genre over Decades")
plot3


# Box plot rating and genre


plot4 <- ggplot(data = movie_data_sub, aes(x = Genre, y = rating, fill = Genre)) + geom_boxplot() + 
  xlab("Genre") + ylab("Rating") + ggtitle("Distribution of ratings for various genre") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 14)) 


# Distribution of budget for various genre boxplot

plot5 <- ggplot(data = movie_data_sub, aes(x = Genre, y = budget, fill = Genre)) + 
  geom_boxplot() + xlab("Genre") + ylab("Budget") + ggtitle("Distribution of budget for various genre") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 14))


grid.arrange(plot4, plot5, ncol=2, nrow=1)



# Scatter plot between length and rating over director rating

empty <- ggplot()+geom_point(aes(1,1), colour="white") +theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())

# Scatterplot of x and y variables

scatter <- ggplot(data = dat,aes(x = length, y = rating)) + geom_point(aes(color = Director.Rating)) + 
  scale_color_manual(values = c("orange", "purple", "green")) + 
  ggtitle("Scatter Plot") + xlab("Length of Movies") + 
  ylab("Rating of Movies") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"))

# Marginal density of x - plot on top

plot_top <- ggplot(data = dat, aes(x = length, fill = Director.Rating)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple", "green")) + 
  theme(legend.position = "none") +  ggtitle("Length Distribution") + xlab("Length of Movies") + 
  ylab("Density")

# Marginal density of y - plot on the right

plot_right <- ggplot(data = dat, aes(x = rating, fill = Director.Rating)) + 
  geom_density(alpha=.5) + 
  coord_flip() + 
  scale_fill_manual(values = c("orange", "purple", "green")) + 
  theme(legend.position = "none") + ggtitle("Rating Distribution") + xlab("Rating of Movies") + 
  ylab("Density")


# The above three plots combined

plot6 <- grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1.5, 4))



# Drama v/s All Genre Scatter

dat$Drama <- as.factor(dat$Drama)
scatter2 <- ggplot(data = dat,aes(x = length, y = rating)) + geom_point(aes(color = Drama)) + 
  scale_color_manual(values = c("orange", "purple")) + 
  ggtitle("Sactter plot: Length vs Rating for Drama Genre") +
  xlab("Length of Movies") + ylab("Rating of Movies") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"))

# Drama and all other genre length density plot

plot_top2 <- ggplot(data = dat, aes(x = length, fill = Drama)) + geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple")) +
  ggtitle("Density Plot Over Length of Movies") + xlab("Length of Movies") + ylab("Density")

# Drama and all other genre rating density plot

plot_right2 <- ggplot(data = dat, aes(x = rating, fill = Drama)) + geom_density(alpha=.5) +
  scale_fill_manual(values = c("orange", "purple")) + 
  ggtitle("Density Plot Over Rating of Movies") + xlab("Rating of Movies") + ylab("Density")


# The above three plots combined

plot7 <- grid.arrange(plot_top2, empty, scatter2, plot_right2, ncol=2, nrow=2, widths=c(4, 1.5), heights=c(1.5, 4))



# Director names and number of movies

dat2 <- dat[(dat$Director != " "), ]  
dat2 <- dat2[(dat2$Director != "De"), ] 
dat2 <- dat2[(dat2$Director != " De"), ] 
dat2 <- within(dat2, Director <- factor(Director, levels=names(sort(table(Director), decreasing=TRUE))))

plot8 <- ggplot(data=dat2, aes(x = Director, fill = Actor.Rating)) + geom_histogram(stat="bin", width = .5) + 
  coord_cartesian(xlim=c(0.5, 12.5), ylim=c(5, 30)) +  ggtitle("Directors with Maximum Movies") +
  xlab("Directors") + ylab("Movies Directed") + 
  theme(legend.position = c(.9, .8), axis.text.x = element_text(angle = 30, hjust = 1, size=19), 
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold")) 
plot8



####################################### Trees #############################################


#Use final movies dataset
movies <- read.csv("movies.csv",header = TRUE)

#Splitting the rating into three categories
#Rating below 6.5 is category 3
#Rating below 7.3 and above 6.5 is category 2
#Rating below 9.1 and above 7.3 is category 1

movies$rating <- as.factor(ifelse(movies$rating < 6.5, 3,ifelse(movies$rating <= 7.3, 2,ifelse(movies$rating <=9.1,1,NA))))

#MPAA rating
#PG 13, NC- 17, PG - category 1
#R -category 2
#None - category 3

movies$mpaa <- as.factor(ifelse(movies$mpaa == "PG-13",1,ifelse(movies$mpaa == "PG",1,ifelse(movies$mpaa == "NC-17",1,ifelse(movies$mpaa =="R",2,3)))))

#Adding an ID column
movies$ID <- seq.int(nrow(movies))

#Converting actor and director ratings to factor variables

movies$Actor.Rating <- as.factor(movies$Actor.Rating)
movies$Director.Rating <- as.factor(movies$Director.Rating)

#Removing all columns that are not predictors
movies <- movies[,c(29,1,5:9,22:28,10)]

#Understanding non -factor variables(Length and budget)
#Length

hist(movies$length, xlab = "length of movie", col = "grey", prob = TRUE)
lines(density(movies$length), col = "blue", lwd = 2)
lines(density(movies$length, adjust = 2),lty = "dotted",col = "darkgreen", lwd = 2)
plot(density(movies$length), xlab = "length of movie")
polygon(density(movies$length), col = "Purple",border = "black")

#Testing for normality - Shapiro -Wilk Test

shapiro.test(movies$length)

#Comments - P value is very less. This implies we can reject the null hypothesis
#and go with the alternate. Hence, it can be said that the sample does not 
#typically follow a normal distribution. 

#QQplots

qqnorm(movies$length, col = "grey")
qqline(movies$length, col = 2)

#From the above graph, it can be said that, there is a deviation from the 
#normal distribution at the tails. Also, since the rest of the sample follows
#a typical order and lies largely within a bell shape(as seen above), the 
#lack of normality can be overlooked

#Testing for budget

hist(movies$budget, xlab = "Budget of movie", col = "grey", prob = TRUE)
lines(density(movies$budget), col = "blue", lwd = 2)
lines(density(movies$budget, adjust = 2),lty = "dotted",col = "darkgreen", lwd = 2)
plot(density(movies$budget), xlab = "length of movie")
polygon(density(movies$budget), col = "Purple",border = "black")

#Sahpiro- Wilk normality test

shapiro.test(movies$budget)

#QQ plots
qqnorm(movies$budget, col = "grey")
qqline(movies$budget, col = 2)

#Comments: same as the predictor length

##############################################################################

#Classification Trees

library(tree)

#Sampling the dataset into train and test

set.seed(10)
s <- sample(movies$ID, 1500, replace = FALSE)
train <- movies[s,]
test <- movies[-s,]

movies$ID <- NULL
train$ID <- NULL
test$ID <- NULL
train$Title <- NULL
test$Title <- NULL

#Tree object with all predictors

tree.obj <- tree(train$rating ~ ., data = train)
summary(tree.obj)
plot(tree.obj)
text(tree.obj, pretty = 0, cex = 0.6)

#Misclassification error rate = 43%

#Testing error

pred <- predict(tree.obj, test, type = "class")
t <- table(pred,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#Accuracy = 54%
#error rate = 46%

#Pruning the trees

set.seed(10)
cv.obj <- cv.tree(tree.obj, FUN = prune.misclass)
names(cv.obj)
cv.obj
plot(cv.obj)

#Error rates

par(mfrow =c(1,3))
plot(cv.obj$dev ~ cv.obj$size, type = "b", xlab = "size", ylab = "deviation")
plot(cv.obj$dev ~ cv.obj$k, type = "b", xlab = "k", ylab = "deviation")

#Plotting the pruned tree
prune.obj <- prune.misclass(tree.obj, best = 5)
plot(prune.obj)
text(prune.obj, pretty = 0)
summary(prune.obj)

#On test dataset

pred <- predict(prune.obj, test, type = "class")
t <- table(pred,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#Accuracy after pruning - 54%

#Bagging

library(randomForest)
set.seed(10)
bag.tree <- randomForest(train$rating ~ ., data = train, mtry = 12, importance = TRUE)
bag.tree
plot(bag.tree, main = "Bagging")

#prediction using bagging

bag.pred <- predict(bag.tree, newdata = test)
t <- table(bag.pred,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#accuracy after bagging = 59%

#RandomForest

set.seed(10)
random.tree <- randomForest(train$rating ~ ., data = train, mtry = 4, importance = TRUE)
random.tree
plot(random.tree, main = "RandomForest")

#prediction using randomForest

random.pred <- predict(random.tree, newdata = test)
t <- table(random.pred,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#accuracy after random forest = 61%

#Finding important predictors

importance(random.tree)
varImpPlot(random.tree)
summary(random.tree)

#Budget > length > year > director rating
#Tree object using important predictors only

tree.imp <- tree(train$rating ~ train$Director.Rating + train$year + train$length + train$budget, data = train)
summary(tree.imp)
plot(tree.imp)
text(tree.imp, pretty = 0)

#Random Forest for important predictors

data1 <- train[,c(2:5,13)]
rand.imp <- randomForest(data1$rating ~ ., data = data1, mtry = 2, importance = TRUE)
rand.imp
plot(rand.imp)

#Accuracy using random forest

data <- test[,c(2:5)]
random.pred.imp <- predict(rand.imp, newdata = data)
t <- table(random.pred.imp,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#accuracy after using important predictors = 58%

#prediction using randomForest

dat <- test[,c(2:5,10,6,1,13)]
random.pred <- predict(random.tree, newdata = dat)
t <- table(random.pred,test$rating)
t

accuracy <- sum(diag(t))/sum(t)
accuracy

#accuracy after random forest = 61%

#Finding important predictors

importance(random.tree)
varImpPlot(random.tree)
summary(random.tree)

#budget > length > year > director.rating > drama > mpaa > actor rating



###################### Multinomial Logistic Regression ####################################


data <- read.csv("movies.csv")

hist(data$rating)      # Histogram of rating values
summary(data$rating)   # Summary of rating column


# Rating values below median(=6.5) go into category-3,values between median and 3rd quartile(=7.3) go into category-2 and values greater than 3rd quartile go into category-1 

data$rating[data$rating<6.5] <- 3  
data$rating[data$rating>7.3] <- 1
data$rating[data$rating>=6.5 & data$rating<=7.3] <- 2

unique(data$rating)           #unique values of rating column

length(data$rating[data$rating==1])         # total= 593  
length(data$rating[data$rating==2])         # total= 722
length(data$rating[data$rating==3])         # total= 1222

table(data$rating)

require(mlogit)

data$rating <- as.factor(data$rating)

# The square root of the budget column values has been done to make values small enough to fit in the model 

data$budget <- sqrt(data$budget)


# Divide the data set in training and testing data set approximately in 60:40 ratio
set.seed(10)
tr <- sample(1:nrow(data), size = 1500, replace=F)
train <- data[tr,]
test <- data[-tr,]

likelihood <- rep(0,14)

#reshaping the data from wide to long format

mydata <- mlogit.data(train,varying=NULL,choice="rating",shape="wide")
head(mydata)


# Forward variable selection method is applied here to get the most significant variables in the model

model1 <- mlogit(rating ~ 1|Actor.Rating,data=mydata,reflevel="1")
summary(model1)

likelihood[1] <- summary(model1)$logLik[1]

model2 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating,data=mydata,reflevel="1")
summary(model2)

likelihood[2] <- summary(model2)$logLik[1]

model3 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+year,data=mydata,reflevel="1")
summary(model3)

likelihood[3] <- summary(model3)$logLik[1]

model4 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+length,data=mydata,reflevel="1")
summary(model4)

likelihood[4] <- summary(model4)$logLik[1]

model5 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+length+budget,data=mydata,reflevel="1")
summary(model5)

likelihood[5] <- summary(model5)$logLik[1]

model6 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+length+budget+mpaa,data=mydata,reflevel="1")
summary(model6)

likelihood[6] <- summary(model6)$logLik[1]

model7 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+mpaa+Action,data=mydata,reflevel="1")
summary(model7)

likelihood[7] <- summary(model7)$logLik[1]

model8 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+mpaa+Action+Comedy,data=mydata,reflevel="1")
summary(model8)

likelihood[8] <- summary(model8)$logLik[1]

model9 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+mpaa+Action+Comedy+Documentary,data=mydata,reflevel="1")
summary(model9)

likelihood[9] <- summary(model9)$logLik[1]

model10 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+budget+mpaa+Action+Comedy+Documentary+Romance,data=mydata,reflevel="1")
summary(model10)

likelihood[10] <- summary(model10)$logLik[1]

model11 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+mpaa+Action+Comedy+Documentary+Romance+Animation,data=mydata,reflevel="1")
summary(model11)

likelihood[11] <- summary(model11)$logLik[1]

model12 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating+budget+length+mpaa+Animation+Drama,data=mydata,reflevel="1")
summary(model12)

likelihood[12] <- summary(model12)$logLik[1]

model13 <- mlogit(rating ~ 1|Director.Rating+budget+length+mpaa+Action+Comedy+Documentary+Romance+Animation+Drama,data=mydata,reflevel="1")
summary(model13)

likelihood[13] <- summary(model13)$logLik[1]

model14 <- mlogit(rating ~ 1|Director.Rating+length+budget+mpaa+Action+Comedy+Documentary+Romance+Animation+Drama,data=mydata,reflevel="1")
summary(model14)

likelihood[14] <- summary(model14)$logLik[1]

x <- c(1:14)

# plot of log-likelihood values of all the models  

plot(x,likelihood,main="Log-likelihood vs Model Number",xlab="Model Number",ylab="Log-likelihood",col="red",type = "b")
likelihood

# based on the log-likelihood and R square values we choose 12th model 
# and is selected and analysed further to give better results 


# Trying different combinations of interactive predictors, the final model for predicting the ratings of movie is the below:

model20 <- mlogit(rating ~ 1|Actor.Rating+Director.Rating*length+budget*mpaa+Animation*Drama,data=mydata,reflevel="1")
summary(model20)


# Exponential function of logistic coefficents is calculated below:
# This tells us the log-odds of of being in category 2 versus 1 and 3 versus 1. 

exp(coef(model20))


#reshaping the test data from wide to long format

testdata <- mlogit.data(test,varying=NULL,choice="rating",shape="wide")


# predict function is used to predict the rating on the basis of proposed model

p <- predict(model20, newdata = testdata)
p <- data.frame(p)


l <- length(p$X1)
m <- rep(0,l)


# taking out the category from the predicted values which gives the highest probability

for (i in 1:l){
  
  m[i] <- which.max(p[i,])
  
}


#table to form 3 by 3 matrix using predicted and the original values from the data

tab <- table(m,test$rating)
tab

# accuracy is calculated 

accuracy <- (tab[1] + tab[5] + tab[9])/sum(tab)
accuracy_percentage <- accuracy*100
accuracy_percentage


# error rate is calculated

error <- 1-accuracy
error_percentage <- error*100
error_percentage



############################# Support Vector Machines #####################################


### Required Library

library(e1071)
library(ROCR)

### Importing DataSet

movies1 <- read.csv("movies.csv")

movies1$rating <- as.factor(ifelse(movies1$rating < 6.5, 3,ifelse(movies1$rating <= 7.3, 2,ifelse(movies1$rating <=9.1,1,NA))))


movies1$rating <- as.factor(movies1$rating)
movies1 <- data.frame(movies1)

### Sampling Train and Test Data
set.seed(15)
s <- sample(nrow(movies1), 1500)
train <- movies1[s,]
test <- movies1[-s,]

### Checking Number of Classes in Training and Testing
table(train$rating)
table(test$rating)

### Training Data Frame
x = train[,-10]
y = train[,10]
dat=data.frame(x=x , y=as.factor(y))


### Testing Data Frame
xt = test[,-10]
yt = test[,10]
datt=data.frame(x=xt , y=as.factor(yt))

### Building SVM Model using Linear Kernal
svm.model=svm(y ~ ., data=dat , kernel ="linear", cost=10, gamma = 1)
summary(svm.model)

### Confusion Matrix for Training Data
train.tab = table(svm.model$fitted , dat$y)
train.tab
### Checking Accuracy
classAgreement(tab)

### Confusion Matrix for Test Data
svm.pred <- predict(svm.model, datt)
test.tab=table(pred = svm.pred, true = datt$y)
### Checking Accuracy
classAgreement(test.tab)

### Tuning The model for Test data
tune.out=tune(svm ,y~.,data=datt ,kernel ="linear",
              ranges =list(cost=c(0.001, 0.01, 0.1, 1,5,10,100,1000))
              , gamma=c(0.5,1,2,3,4))
summary(tune.out)














