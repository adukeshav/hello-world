install.packages("caret")
install.packages("e1071")

dataset <- read.csv("blogData_train.csv",header = TRUE,sep = ",")
range(dataset[,281])
plot(dataset[,281])

##for basicfeatures

###linear regression

library(data.table)
basicFeatures <- fread("blogData_train.csv", select = c(51:60,281))
textualFeatures <- fread("blogData_train.csv", select = c(63:262,281))
fit1.train<-lm(V281~V51+V52+V53+V54+V55+V56+V57+V58+V59+V60,data=basicFeatures)
summary(fit1.train)

########################
 ## The equation of my model is
yhat=2.14 +0.080*V51+0.299*V52-0.017*V53-0.088*V54+0*V56-2.996*V56+1.126*V57+0.228*V58+3.214*V59+0*V60

######

basicFeaturestest1<-fread("blogData_test-2012.02.28.00_00.csv",select = c(51:60,281))
basicFeaturestest2<-fread("blogData_test-2012.02.29.00_00.csv",select = c(51:60,281))
basicFeaturestest3<-fread("blogData_test-2012.03.30.01_00.csv",select = c(51:60,281))
basicFeaturestest4<-fread("blogData_test-2012.03.31.01_00.csv",select = c(51:60,281))


pred1<-predict(fit1.train,basicFeaturestest1,se.fit = TRUE)
pred1a <- predict(fit1.train,basicFeaturestest2,se.fit = TRUE)
pred1b<- predict(fit1.train,basicFeaturestest3,se.fit = TRUE)
pred1c<- predict(fit1.train,basicFeaturestest4,se.fit = TRUE)
pred1d<- predict(fit1.train,basicFeatures,se.fit = TRUE)

mse=mean((basicFeaturestest1$V281 - pred1$fit)^2)
mse1a=mean((basicFeaturestest2$V281-pred1a$fit)^2)
mse1b=mean((basicFeaturestest3$V281 - pred1b$fit)^2)
mse1c=mean((basicFeaturestest4$V281 - pred1c$fit)^2)
mse1d=mean((basicFeatures$V281 - pred1d$fit)^2)


##logisticregression
meanTarget <- mean(basicFeatures[,V281])

basicFeatures$V281 <- sapply(basicFeatures$V281, function(x) {ifelse(x >= meanTarget, 1, 0)})
basicFeaturestest1$V281<- sapply(basicFeaturestest1$V281, function(x) {ifelse(x >= meanTarget, 1, 0)})
basicFeaturestest2$V281<- sapply(basicFeaturestest2$V281, function(x) {ifelse(x >= meanTarget, 1, 0)})
basicFeaturestest3$V281<- sapply(basicFeaturestest3$V281, function(x) {ifelse(x >= meanTarget, 1, 0)})
basicFeaturestest4$V281<- sapply(basicFeaturestest4$V281, function(x) {ifelse(x >= meanTarget, 1, 0)})

fit3.train<- glm(V281~V51+V52+V53+V54+V55+V56+V57+V58+V59+V60,data=basicFeatures, family = binomial())
summary(fit3.train)

##(Intercept) -2.6260975

pred3<-predict(fit3.train,basicFeaturestest1,se.fit = TRUE)
pred3a <- predict(fit3.train,basicFeaturestest2,se.fit = TRUE)
pred3b<- predict(fit3.train,basicFeaturestest3,se.fit = TRUE)
pred3c<- predict(fit3.train,basicFeaturestest4,se.fit = TRUE)
pred3d<- predict(fit3.train,basicFeatures,se.fit = TRUE)

mse3=mean((basicFeaturestest1$V281 - pred3$fit)^2)
mse3a=mean((basicFeaturestest2$V281- pred3a$fit)^2)
mse3b=mean((basicFeaturestest3$V281 - pred3b$fit)^2)
mse3c=mean((basicFeaturestest4$V281 - pred3c$fit)^2)
mse3d=mean((basicFeatures$V281 - pred3d$fit)^2)

predictionsExp1 = sapply(pred3$fit, function(x) {ifelse(x > .5, 1, 0)})

library(caret)
print(confusionMatrix(basicFeaturestest1$V281, predictionsExp1))

##########
#for textualFeatures
##########

##linearregression

xnam <- paste("V", 63:262, sep="")
fmla <- as.formula(paste("V281 ~ ", paste(xnam, collapse= "+")))

fit2.train<-lm(formula = fmla,data=textualFeatures)

summary(fit2.train)

##(Intercept)   3.39266 

textualFeaturestest1<-fread("blogData_test-2012.02.28.00_00.csv",select = c(63:262,281))
textualFeaturestest2<-fread("blogData_test-2012.02.29.00_00.csv",select = c(63:262,281))
textualFeaturestest3<-fread("blogData_test-2012.03.30.01_00.csv",select = c(63:262,281))
textualFeaturestest4<-fread("blogData_test-2012.03.31.01_00.csv",select = c(63:262,281))

pred2<-predict(fit2.train,textualFeaturestest1,se.fit = TRUE)
pred2a<-predict(fit2.train,textualFeaturestest2,se.fit = TRUE)
pred2b<-predict(fit2.train,textualFeaturestest3,se.fit = TRUE)
pred2c<-predict(fit2.train,textualFeaturestest4,se.fit = TRUE)
pred2d<-predict(fit2.train,textualFeatures,se.fit = TRUE)


mse2=mean((textualFeaturestest1$V281 - pred2$fit)^2)
mse2a=mean((textualFeaturestest2$V281 - pred2a$fit)^2)
mse2b=mean((textualFeaturestest3$V281 - pred2b$fit)^2)
mse2c=mean((textualFeaturestest4$V281 - pred2c$fit)^2)
mse2d=mean((textualFeatures$V281 - pred2d$fit)^2)


###logistic regression

meanTargetnew <- mean(textualFeatures[,V281])

textualFeatures$V281 <- sapply(textualFeatures$V281, function(x) {ifelse(x >= meanTargetnew, 1, 0)})
textualFeaturestest1$V281<- sapply(textualFeaturestest1$V281, function(x) {ifelse(x >= meanTargetnew, 1, 0)})
textualFeaturestest2$V281<- sapply(textualFeaturestest2$V281, function(x) {ifelse(x >= meanTargetnew, 1, 0)})
textualFeaturestest3$V281<- sapply(textualFeaturestest3$V281, function(x) {ifelse(x >= meanTargetnew, 1, 0)})
textualFeaturestest4$V281<- sapply(textualFeaturestest4$V281, function(x) {ifelse(x >= meanTargetnew, 1, 0)})

xnam <- paste("V", 63:262, sep="")
fmla <- as.formula(paste("V281 ~ ", paste(xnam, collapse= "+")))

fit4.train<- glm(formula=fmla,data=textualFeatures, family = binomial())
summary(fit4.train)

##(Intercept)  -2.481124

pred4<-predict(fit4.train,textualFeaturestest1,se.fit = TRUE)
pred4a <- predict(fit4.train,textualFeaturestest2,se.fit = TRUE)
pred4b<- predict(fit4.train,textualFeaturestest3,se.fit = TRUE)
pred4c<- predict(fit4.train,textualFeaturestest4,se.fit = TRUE)
pred4d<- predict(fit4.train,textualFeatures,se.fit = TRUE)

mse4=mean((textualFeaturestest1$V281 - pred4$fit)^2)
mse4a=mean((textualFeaturestest2$V281- pred4a$fit)^2)
mse4b=mean((textualFeaturestest3$V281 - pred4b$fit)^2)
mse4c=mean((textualFeaturestest4$V281 - pred4c$fit)^2)
mse4d=mean((textualFeatures$V281 - pred4d$fit)^2)




