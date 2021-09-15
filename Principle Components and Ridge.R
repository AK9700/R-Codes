meat<-read.table('http://math.uttyler.edu/nathan/data/meatspec.data',header=TRUE)
head(meat)
dim(meat)
train<-sample(1:215,165)

#Use thestepto automagically compute a variable-selected linear model for this data.
m1<-lm(fat~.,data=meat[train,])
step(m1)
m.step<-lm(formula = fat ~ V1 + V2 + V4 + V5 + V7 + V8 + V12 + V15 + 
             V17 + V20 + V21 + V22 + V23 + V25 + V27 + V28 + V29 + V31 + 
             V32 + V34 + V35 + V36 + V37 + V39 + V43 + V45 + V46 + V48 + 
             V49 + V50 + V51 + V52 + V54 + V55 + V57 + V59 + V60 + V61 + 
             V62 + V64 + V66 + V67 + V69 + V71 + V72 + V73 + V74 + V75 + 
             V78 + V79 + V80 + V82 + V84 + V85 + V86 + V89 + V90 + V91 + 
             V94 + V96 + V97 + V100, data = meat[train, ])
sum( (meat$fat[-train] - predict(m.step,newdata=meat[-train,]))^2 )/50
d<-sqrt(.Last.value) #1.41459

vif(m.step)

summary(m.step)

#Use principal components analysis to develop a model for this data
pc <- prcomp(meat[train,-101],center=TRUE,scale=TRUE)
plot(pc$sd)
summary(pc)
pc$rot[,1]
prec.model<-lm(meat$fat[train]~pc$x[,1:7])

myoo <- apply(meat[train,-101],2,mean)
ess.dee <- apply(meat[train,-101],2,sd)

test.0 <- meat[-train,-101]
test.1 <- sweep(test.0,2,myoo)
test.2 <- sweep(test.1,2,ess.dee,FUN='/')

# now we apply the principal components rotation
test.3 <- as.matrix(test.2) %*% pc$rot[,1:2]
test.4 <- cbind(rep(1,50),test.3)

test.yhat <- test.4 %*% prec.model$coeff
test.y <- meat$fat[-train]

sum((test.y - test.yhat)^2 )/50
c<-sqrt(.Last.value) #11.28553

#Use ridge regression to develop a model for this data.
m.ridge0 <- glmnet(meat[train,-101],meat[train,101],alpha=0,standardize=TRUE)
plot(m.ridge0)
m.ridge0$lambda

m.ridge <- glmnet(meat[train,-101],meat[train,101],alpha=0,standardize=TRUE,lambda=seq(.001,1,length=100))
plot(m.ridge)

cv.glmnet(as.matrix(meat[train,-101]),meat[train,101],alpha=0,standardize=TRUE,folds=5)$lambda.min

m.ridge1 <- glmnet(meat[train,-101],meat[train,101],alpha=0,standardize=TRUE,lambda= 0.9690467)

bet<-m.ridge1$beta

build.data <- scale(meat[train,],center=TRUE,scale=TRUE)
apply(build.data,2,mean)
apply(build.data,2,sd)
test.data <- sweep(build.data[-train,],2,attr(build.data,"scaled:center"))
test.data <- sweep(test.data,2,attr(build.data,"scaled:scale"),FUN='/')
build.data <- data.frame(build.data)

las.hat <- as.matrix(test.data[,-101]) %*% bet

sum((test.data[,101]-las.hat)^2)/50
b<-sqrt(.Last.value) #134.0224


#Use lasso to develop a model for this data.
m.lasso0 <- glmnet(meat[train,-101],meat[train,101],alpha=1,standardize=TRUE)
plot(m.lasso0)
m.lasso0$lambda

m.lasso <- glmnet(meat[train,-101],meat[train,101],alpha=1,standardize=TRUE,lambda=seq(.001,1,length=100))
plot(m.lasso)

cv.glmnet(as.matrix(meat[train,-101]),meat[train,101],alpha=1,standardize=TRUE,folds=5)$lambda.min

m.lasso1 <- glmnet(meat[train,-101],meat[train,101],alpha=0,standardize=TRUE,lambda=0.01088553)

bet.l<-m.lasso1$beta

build.data <- scale(meat[train,],center=TRUE,scale=TRUE)
apply(build.data,2,mean)
apply(build.data,2,sd)
test.data <- sweep(build.data[-train,],2,attr(build.data,"scaled:center"))
test.data <- sweep(test.data,2,attr(build.data,"scaled:scale"),FUN='/')
build.data <- data.frame(build.data)

las.hat <- as.matrix(test.data[,-101]) %*% bet.l

sum((test.data[,101]-las.hat)^2)/50
a<-sqrt(.Last.value) #44.86041


print(c(a,b,c,d))






