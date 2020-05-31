# DATA IMPORT AND TRANSFORMATION:

library("dplyr")
library("tidyr")
library("htmlwidgets")
library("rpivotTable")
library("plot3D")
library("rpart")
library("rpart.plot")
library("caret")
library("gtools")
library("e1071")

data = read.csv(file.choose())

# checking missing values
sapply(data,function(x) sum(is.na(x)))

# replacing na values
data$Age = replace_na(data$Age,mean(data$Age,na.rm=T))

sapply(data,function(x) sum(is.na(x)))#just to check

# converting age to 3 categories
data$Age = ifelse(data$Age<18,"Child",ifelse(data$Age>=60,"Senior Citizen","Adult"))

df = data

df1 = data

df1$Survived = ifelse(data$Survived==0,"Died","Survived")


## Contingency Tables:

table(df1$Survived)

table(df1$Sex)
table(df1$Survived,df1$Sex)

table(df1$Pclass)
table(df1$Pclass,df1$Survived)

table(df1$Survived,df1$Age)
table(df1$Sex,df1$Age)

table(df1$Survived,df1$Sex,df1$Age)


## DATA VISUALISATION:


#Overall Sex:
a = matrix(table(data$Sex),ncol=2)
a = as.data.frame(a)
colnames(a) = c("Female","Male")
par(mar=c(4,4,4,4))
png("Overall Sex.png")
xx = barplot(as.matrix(a),col=c("hotpink"),xlab="Sex",main="Overall Sex",ylab="Frequency")
text(x = xx , label = a, pos = 3, cex = 1.8, col = "black")
dev.off()

# Sex Wise Survival:
table(data$Survived,data$Sex)
par(mar=c(4,4,4,4))
png("sex_wise_survival.png")
barplot(table(data$Survived,data$Sex), ylab="Frequency",ylim = c(0,650) ,xlab="Sex", main="Sex wise Survival", col=c("turquoise4", "turquoise2", "turquoise" ), beside <- FALSE, xlim=c(0,1), width=.4)
legend("right", title="Survived", legend= sort(unique(data$Survived)), fill =c("turquoise4", "turquoise2", "turquoise" ), box.lty=0)
dev.off()


#Overall Classes:
a = matrix(table(data$Pclass),ncol=3)
a = as.data.frame(a)
colnames(a) = c("First Class","Second Class","Third Class")
par(mar=c(4,4,4,4))
png("Classes.png")
xx = barplot(table(data$Pclass),ylim = c(0,500),col=c("hotpink"),main="Classes",ylab="Frequency")
text(x = xx , label = a, pos = 3, cex = 1.8, col = "black")
dev.off()

# Class wise Survival
table(data$Survived,data$Pclass)
par(mar=c(4,4,4,4))
png("Class_wise_survival.png")
barplot(table(data$Survived,data$Pclass), ylab="Frequency",ylim = c(0,600) ,xlab="Classes", main="Class wise Survival", col=c("turquoise4", "turquoise" ), beside <- FALSE, xlim=c(0,1), width=.275)
legend("right", title="Survived", legend= sort(unique(data$Survived)), fill =c("turquoise4", "turquoise" ))
dev.off()


#Overall Survival:
a = matrix(table(df1$Survived),ncol=2)
a = as.data.frame(a)
colnames(a) = c("Died","Survived")
par(mar=c(4,4,4,4))
png("Overall Survival.png")
xx = barplot(as.matrix(a),col=c("darkorchid"),main="Survival",ylab="Frequency")
text(x = xx , label = a, pos = 3, cex = 1.8, col = "black")
dev.off()

# 3d survival acc. to sex and class
plot_data = matrix(table(df$Survived,df$Sex,df$Pclass),ncol=3)
colnames(plot_data) = c("First Class","Second Class","Third Class")
rownames(plot_data) = c("Died Females","Survived Females","Died Males","Survived Males")
plot_data
table(df$Pclass,df$Survived,df$Sex)

par(mar=c(2,3,2,3))
hist3D (x = 1:4, y = 1:3, z = plot_data,expand=1,
        bty = "g", phi = 45,  theta = 45,
        xlab = "Survival", ylab = "Class", zlab = "Frequency", main = "Survival on Titanic",
        col = "chartreuse", border = "black", shade = 0.5,
        ticktype="detailed",space = 0.4, d = 2, cex.axis = 1e-9)
# Use text3D to label x axis
text3D(x = 1:4, y = rep(0.5, 4), z = rep(3, 4),col="brown",
       labels = rownames(plot_data),add = TRUE, adj = 0.75)
# Use text3D to label y axis
text3D(x = rep(5,3),  y = 1:3, z = rep(5, 3),col="brown",phi =40,theta = 80,adj=0.5,
       labels  = colnames(plot_data),
       add = TRUE)


# decision tree:

modela = lm(data$Survived~data$Age+data$Sex+data$Pclass,data=data)
summary(modela)
modela.rpart<-rpart(data$Survived~data$Age+data$Sex+data$Pclass,data=data)
#rpart.plot.version1(modela.rpart,fallen.leaves=TRUE,extra="auto",type=4,cex=0.5,main="Survival of the Passengers")
rpart.plot(modela.rpart,extra="auto",type=4,cex=0.6,main="Survival of the Passengers")



## Pivot Table:

rpivotTable(df1,rows = "Survived", cols=c("Sex","Pclass"),width="100%", height="400px")



## Naive Bayes:

# Converting to factor:

levels(df$Survived)
table(df$Survived)
df$Survived = factor(df$Survived)
str(df$Survived)


levels(df$Sex)
table(df$Sex)
str(df$Sex)

levels(df$Pclass)
table(df$Pclass)
df$Pclass = factor(df$Pclass,levels = c(1,2,3),label = c(1,2,3))
str(df$Pclass)

levels(df$Age)
table(df$Age)
df$Age = factor(df$Age,levels = c("Child","Adult","Senior Citizen"),label = c(1,2,3))
str(df$Age)

levels(df$SibSp)
table(df$SibSp)
df$SibSp = factor(df$SibSp)
str(df$SibSp)

levels(df$Parch)
table(df$Parch)
df$Parch = factor(df$Parch)
str(df$Parch)

str(df)
df2 = df %>% select(Survived,Pclass,Sex,Age,SibSp,Parch)
str(df2)


# Creating Training and testing data:
set.seed(112)
train = createDataPartition(df2$Survived,p=0.65,list=F)
training = df2[train,]
testing = df2[-train,]

# Model Building:

nb_model1 = naiveBayes(Survived~.,data=training)
nb_model1


# Prediction on training data:
prob = predict(nb_model1,training,type="raw")
prob
training = cbind(training,prob[,2])
training$predict = as.factor(ifelse(training$prob>0.65,1,0))

# Accuracy of the training  model:
confusionMatrix(training$Survived,training$predict)

# Prediction on testing data:
prob_test = predict(nb_model1,testing,type="raw")
testing = cbind(testing,prob_test[,2])
testing$predict = as.factor(ifelse(testing$prob_test>0.65,1,0))

# Accuracy of the testing model:
confusionMatrix(testing$Survived,testing$predict)

