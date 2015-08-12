#dataset1
wbcd <- read.csv("Desktop/wisc_bc_data.csv" )
str(wbcd)
#str(wbcdnew)
wbcd$id <- NULL
wbcdnew <-wbcd
str(wbcdnew)
wbcd$diagnosis<- NULL
library(fastICA)
#convert dataframe to matrix
A <- matrix(c(1, 1, -1, 3), 30, 30, byrow = TRUE)
S  <- data.matrix(wbcd)
X <- S %*% A
View(X)
a <- fastICA(X, 10, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")
#View(a$S)
icad <-as.data.frame(a$S)
icadn <-as.data.frame(a$S)

#dataset2
mgc <- read.csv("Desktop/magic041.csv" )
str(mgc)
#str(wbcdnew)
mgc$class<- NULL
library(fastICA)
#mixing matrix
A <- matrix(c(1, 1, -1, 3), 10, 10, byrow = TRUE)
#convert dataframe to matrix
S  <- data.matrix(mgc)
X <- S %*% A
View(X)
at <- fastICA(X, 5, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(at$X, main = "Pre-processed data")
plot(at$X %*% a$K, main = "PCA components")
plot(at$S, main = "ICA components")
#View(a$S)
magc <-as.data.frame(at$S)

## add clusterin for reduced dimension
wbcd <- read.csv("Desktop/magic041.csv" )
str(wbcd)
wbcd$id <- NULL
wbcednew <-wbcd
wbcd$diagnosis<- NULL
str(wbcd)
wbcd_n <- as.data.frame(lapply(icad, scale))
results <- kmeans(icad, 2)
results
table(wbcdnew$diagnosis, results$cluster)
plot(wbcdnew[c("radius_mean", "concavity_mean")], col = results$cluster)
#plot(wbcd, col = results$cluster)
#points(results$centers, col = 1:5, pch = 8)


## em for wbcd
wbcd <- read.csv("Desktop/wisc_bc_data.csv" )
str(wbcd)
wbcd$id <- NULL
str(wbcd)
library(mclust)
mc <- Mclust(icad, 2)   # mclust clusters based on gaussian em.
plot(mc, what=c('classification'),
     dimens=c(2,3))
table(wbcd$diagnosis, mc$classification)



#### experiment with second dataset.
mgc <- read.csv("Desktop/magic041.csv" )
str(mgc)
#wbcd$id <- NULL
#wbcednew <-wbcd
mgcnew <-mgc
mgc$class<- NULL

str(mgc)
mgc_n <- as.data.frame(lapply(magc, scale))
str(mgc_n)
results1 <- kmeans(mgc_n, 2)
results1
table(mgcnew$class, results1$cluster)
plot(mgcnew[c("fWidth", "fM3Trans")], col = results1$cluster)
#plot(mgc, col = results1$cluster)
#points(results1$centers, col = 1:5, pch = 8)


### second dataset for em
mgc <- read.csv("Desktop/magic041.csv" )
str(mgc)
library(mclust)
mc <- Mclust(mgc[,1:10], 2)
plot(mc, what=c('classification'),
     dimens=c(3,4))
table(mgc$class, mc$classification)

## add neural network here

View(svcbn)
# create training and test data data 
wbcd_train <- svcbn[1:469, ]   # 80 % of data
wbcd_test <- svcbn[467:569, ]   # 20% of data

## begin to use nnet package "a feedforward system" for neural nets in r
### do some magic here
a = nnet(tar~., data=wbcd_train,size=20,maxit=1000,decay=.001)
testnnt<- predict(a, wbcd_test, type = ("class"))
table(wbcd_test$tar, testnnt)

num.it<-200 #number of training iterations

err.dat<-matrix(ncol=2,nrow=num.it) #object for appending errors

for(i in 1:num.it){
  
  #to monitor progress
  cat(i,'\n') 
  flush.console()
  
  #to ensure same set of random starting weights are used each time
  set.seed(5)
  
  #temporary nnet model
  mod.tmp<-nnet(wbcd_train$tar ~ ., data = wbcd_train[,-10], decay=5e-4,size=10,linout=T,maxit=i,trace=F)
  
  #training and test errors
  train.err<-sqrt(mod.tmp$value/sum(wbcd_train))
  test.pred<-predict(mod.tmp,newdata=wbcd_test[,-10])
  test.err<-sqrt(sum((test.pred-wbcd_test$Tdiagnosis)^2)/sum(wbcd_test))
  
  #append error after each iteration
  err.dat[i,]<-c(train.err,test.err)
  
}
