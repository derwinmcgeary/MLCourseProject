library(downloader)
library(caret)
library(reshape2)

sourceurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
grabme <- function(URL,FILE)  {if(!file.exists(FILE)) {
  download(URL, FILE) }}
grabme(sourceurl, "trainset.csv")
datas <- read.csv("trainset.csv")
inTrain <- createDataPartition(y=datas$classe, p=0.75, list=F)

# strip Near Zero Variance columns
nzvcols <- nearZeroVar(datas)
datas <- datas[,-nzvcols]

training <- datas[inTrain,]
mytesting <- datas[-inTrain,]

testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
grabme(testurl,"testset.csv")
testing <- read.csv("testset.csv")

# strip the *same* near zero columns out of testing
testing <- testing[,-nzvcols]

### Now to write a filter function. Writing it once so we can run the same thing on training and testing data
philtre <- function(datas) {
  datas <- datas[sapply(datas, function(x) !any(is.na(x)))]
  
  zerotest <<- NULL
  for(i in 1:length(datas))
  {
    zerotest <<- c(zerotest, nrow(datas[datas[,i] == 0,])/nrow(datas))
  }
  removecols <- colnames(datas[,which(zerotest >.10)])
  datas[,removecols] <- list(NULL)
  datas[datas == 0] <- NA
  datas <- datas[complete.cases(datas),]
  datas
}

training<-philtre(training)

accset <- data.frame(training$classe,training[,grep("^accel", names(training))])
names(accset)[names(accset)=="training.classe"] <- "classe"

### the following line takes AGES (55 minutes)on my machine
### so we can load("accelonly.RData") instead
if(!file.exists("accelmodel.RData")) 
  {
  system.time(accmodel <- train(accset$classe ~ ., data=accset, preProcess="pca"))
  save(accmodel,file="accelmodel.RData")
  } else {
    load("accelmodel.RData")
  }

z <- confusionMatrix(mytesting$classe, predict(mymodel, mytesting))
cm <- z$table

ggplot(melt(cm),
       aes(Reference,
           Prediction, 
           fill=value, 
           label=value)) + 
  geom_tile() + 
  geom_text() + ggtitle("Accel-only pca model")
##################### End of accmodel ###########

############## Now a gyro-only model ####################
gyroset <- data.frame(training$classe,training[,grep("^gyro", names(training))])
names(gyroset)[names(gyroset)=="training.classe"] <- "classe"

### the following line takes AGES (55 minutes)on my machine
### so we can load("accelonly.RData") instead
if(!file.exists("gyromodel.RData")) { 
  system.time(gyromodel <- train(gyroset$classe ~ ., data=gyroset, preProcess="pca"))
  save(gyromodel,file="gyromodel.RData")
  } else {
    load("gyromodel.RData")    
  }

z <- confusionMatrix(mytesting$classe, predict(gyromodel, mytesting))
cm <- z$table

ggplot(melt(cm),
       aes(Reference,
           Prediction, 
           fill=value, 
           label=value)) + 
  geom_tile() + 
  geom_text() + ggtitle("gyro-only pca model")
################ dumbell-only model ########
dumbset <- data.frame(training$classe,training[,grep("dumbbell", names(training))])
names(dumbset)[names(dumbset)=="training.classe"] <- "classe"

if(!file.exists("gyromodel.RData")) { 
  system.time(dumbmodel <- train(dumbset$classe ~ ., data=dumbset, preProcess="pca"))
  save(dumbmodel,file="dumbmodel.RData")
} else {
  load("dumbmodel.RData")    
}

z <- confusionMatrix(mytesting$classe, predict(dumbmodel, mytesting))
cm <- z$table

ggplot(melt(cm),
       aes(Reference,
           Prediction, 
           fill=value, 
           label=value)) + 
  geom_tile() + 
  geom_text() + ggtitle("dumb-only pca model")


######### Now to combine these three with a voting model
## accmodel is the best, so we take that but if the other two models disagree identically,
## we take that value instead, giving us an accuracy of 95.8%, which is pretty acceptable

total <- data.frame(acc=predict(accmodel,mytesting),gyro=predict(gyromodel,mytesting),dumb=predict(dumbmodel,mytesting),actual=mytesting$classe,voted=mytesting$classe)
total$voted <- ifelse(total$dumb==total$gyro,as.character(total$gyro),as.character(total$acc))
z <- confusionMatrix(total$actual,total$voted)
cm <- z$table
ggplot(melt(cm),
aes(Reference,
Prediction,
fill=value,
label=value)) +
geom_tile() +
geom_text() + ggtitle("voted model")


results <- data.frame(acc=predict(accmodel,testing),gyro=predict(gyromodel,testing),dumb=predict(dumbmodel,testing),voted=predict(dumbmodel,testing))
results$voted <- ifelse(results$dumb==results$gyro,as.character(results$gyro),as.character(results$acc))
