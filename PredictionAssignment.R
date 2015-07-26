library(caret)
library(RCurl)

  # get Data using Curl and persist CSVs.
data <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
               ssl.verifypeer=0L, followlocation=1L)

writeLines(data,'pml-training.csv')
#read.csv('pml-training.csv')

data <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
               ssl.verifypeer=0L, followlocation=1L)

writeLines(data,'pml-testing.csv')
#read.csv('pml-testing')

  # clean csv; remove CSVs and DIV by zero.
trainingdata <- read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!", ""))
testdata <- read.csv('pml-testing.csv' , na.strings=c("NA", "#DIV/0!", ""))

summary(trainingdata,1)
summary(testdata,1)

  #remove rows where most columns are NA
trainnna <- which((colSums(!is.na(trainingdata)) >= 0.7 * nrow(trainingdata)))
testnna <- which((colSums(!is.na(testdata)) >= 0.7 * nrow(testdata)))

cleantrainingdata <- trainingdata[,trainnna]
cleantestdata     <- testdata[,testnna]

  #remove uneccessary columns
#cleantrainingdata_a <- cleantrainingdata[,-c(1,2,3,4,5,6,7)]
#cleantestdata_a     <- cleantestdata[,-c(1,2,3,4,5,6,7)]

cleantrainingdata_a <- cleantrainingdata[,-c(1,5)]
cleantestdata_a     <- cleantestdata[,-c(1,5)]

summary(cleantrainingdata_a,10)
summary(cleantestdata_a,10)

# remove problem id
testSrc <- testSrc[-ncol(testSrc)]
# fix factor levels
testSrc$new_window <- factor(testSrc$new_window, levels=c("no","yes"))

inTraining  <- createDataPartition(cleantrainingdata_a$classe, p = 0.6, list = F)
training    <- cleantrainingdata_a[inTraining, ]
testing     <- cleantrainingdata_a[-inTraining, ]

rf_model<-train(classe~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=4), allowParallel=T)

smallValidData <- subset(validData, 
                         select=c(roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_y, pitch_belt, magnet_dumbbell_z, roll_forearm, accel_dumbbell_y, 
                                  roll_dumbbell, magnet_dumbbell_x,classe))

smallModel <- train(classe ~ ., data=smallValidData[inTrain,], model="rf", trControl=trainControl(method="cv",number=4), allowParallel=T)

plot(varImp(rf_model))

pred <- predict(rf_model, newdata=testing)

cnfMatrix <- confusionMatrix(testing$classe, pred)
confMatrix <- confusionMatrix(pred,testing$classe)$table

confMatrix
prop.table(confMatrix)
cnfMatrix$overall[1]
