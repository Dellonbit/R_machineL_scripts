wbcd <- read.csv("Desktop/wisc_bc_data.csv" )
wbcd$id <- NULL
head(wbcd, 3)
str(wbcd)

#   
l.wbcd <- (wbcd[, 2:31])
wbcd.species <- wbcd[, 1]

#correlation
#round(cor(l.wbcd[,2:31]), 2)
round(cor(l.wbcd), 2) 

pc <- princomp(l.wbcd, cor=TRUE, scores=TRUE)
pcd <- as.data.frame(pc$scores[,1:10])     # 10 components make 95% of variance
pcdn <- as.data.frame(pc$scores[,1:10])   #create dataframe for nueral net
pcdn$tar <- as.factor(wbcd$diagnosis)                 # add target diagnosis t new dataframe
#View(pcdn)
summary(pc)
# scree plot
plot(pc,type="lines")

# a biplot of the pcs
#biplot(pc)

#library(rgl) pplots 4 components
plot3d(pc$scores[,1:3])

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pc$, obs.scale = 1, var.scale = 1, 
              groups = wbcd.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



#dataset 2
mgc <- read.csv("Desktop/magic041.csv" )

head(mgc, 3)
str(mgc)
#
mgcn <- (mgc[, 1:10])
mgc.species <- wbcd[, 11]

#correlation
#round(cor(l.wbcd[,2:31]), 2)
round(cor(mgcn), 2) 

pcm <- princomp(mgcn, cor=TRUE, scores=TRUE)
pcmgc <- as.data.frame(pc$scores[,1:7]) 
#summary showing variance of components
summary(pcm)
# scree plot
plot(pcm,type="lines")


#biplot(pcm)

#library(rgl)
plot3d(pc$scores[,1:3])

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, 
              groups = wbcd.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)




##clustering for reduced dimensions for dataset 1
wbcd <- read.csv("Desktop/wisc_bc_data.csv" )
str(pcd)
wbcd$id <- NULL
wbcednew <-wbcd
wbcd$diagnosis<- NULL
str(wbcd)
wbcd_n <- as.data.frame(lapply(pcd, scale))
str(wbcd_n)
results <- kmeans(wbcd_n, 2)
results
table(wbcednew$diagnosis, results$cluster)
plot(wbcednew[c("radius_mean", "concavity_mean")], col = results$cluster)
#plot(wbcd, col = results$cluster)
#points(results$centers, col = 1:5, pch = 8)

# em clustering for dataset1
wbcd <- read.csv("Desktop/wisc_bc_data.csv" )
str(wbcd)
wbcd$id <- NULL
str(wbcd)
library(mclust)
mc <- Mclust(pcd, 2)   # mclust clusters based on gaussian em.
plot(mc, what=c('classification'),
     dimens=c(2,3))
table(wbcd$diagnosis, mc$classification)



## clustering of reduced dimensions dataset 2
#### experiment with second dataset.
mgc <- read.csv("Desktop/magic041.csv" )
str(pcmgc)
mgcnew <-mgc

str(mgc)
mgc_n <- as.data.frame(lapply(pcmgc, scale))
str(mgc_n)
results1 <- kmeans(mgc_n, 2)
results1
table(mgcnew$class, results1$cluster)
plot(mgcnew[c("fWidth", "fM3Trans")], col = results1$cluster)
#plot(mgc, col = results1$cluster)
#points(results1$centers, col = 1:5, pch = 8)

### second dataset for em
mgc <- read.csv("Desktop/magic041.csv" )
str(pcmgc)
library(mclust)
mc <- Mclust(pcmgc, 2)
plot(mc, what=c('classification'),
     dimens=c(3,4))
table(mgc$class, mc$classification)

##add neuralpart here for anyone dataset (preferrable wbcd)

View(pcdn)
# create training and test data data 
wbcd_train <- pcdn[1:469, ]   # 80 % of data
wbcd_test <- pcdn[467:569, ]   # 20% of data

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
  mod.tmp<-nnet(wbcd_train$tar ~ ., data = wbcd_train[,-11], decay=5e-4,size=10,linout=T,maxit=i,trace=F)
  
  #training and test errors
  train.err<-sqrt(mod.tmp$value/sum(wbcd_train))
  test.pred<-predict(mod.tmp,newdata=wbcd_test[,-11])
  test.err<-sqrt(sum((test.pred-wbcd_test$Tdiagnosis)^2)/sum(wbcd_test))
  
  #append error after each iteration
  err.dat[i,]<-c(train.err,test.err)
  
}
#str(err.dat[,2])




