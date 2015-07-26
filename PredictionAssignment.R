  #dependencies

library(caret)
library(RCurl)

  # get Data using Curl and persist CSVs.
#data <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
#               ssl.verifypeer=0L, followlocation=1L)

#writeLines(data,'pml-training.csv')
#read.csv('pml-training.csv')

#data <- getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
#               ssl.verifypeer=0L, followlocation=1L)

#writeLines(data,'pml-testing.csv')
#read.csv('pml-testing')

  # clean csv; remove CSVs and DIV by zero.
trainingdata <- read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!", ""))
testdata <- read.csv('pml-testing.csv', na.strings=c("NA", "#DIV/0!", ""))

#summary(trainingdata,1)
#summary(testdata,1)

  #remove rows where >70% columns are NA
trainnna <- which((colSums(!is.na(trainingdata)) >= 0.7 * nrow(trainingdata)))
testnna <- which((colSums(!is.na(testdata)) >= 0.7 * nrow(testdata)))

cleantrainingdata <- trainingdata[,trainnna]
cleantestdata     <- testdata[,testnna]

  #remove uneccessary columns
#cleantrainingdata_a <- cleantrainingdata[,-c(1,2,3,4,5,6,7)]
#cleantestdata_a     <- cleantestdata[,-c(1,2,3,4,5,6,7)]

  #final column canditates
cleantrainingdata_a <- cleantrainingdata[,-c(1,5)]
cleantestdata_a     <- cleantestdata[,-c(1,5)]

#summary(cleantrainingdata_a,10)
#summary(cleantestdata_a,10)

  #Use a 60:40 trainng:test ratio.
inTraining  <- createDataPartition(cleantrainingdata_a$classe, p = 0.6, list = F)
training    <- cleantrainingdata_a[inTraining,]
testing     <- cleantrainingdata_a[-inTraining,]

# Use Random Forest method
rf_model<-train(classe~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=4), allowParallel=T)

# Its slow ...

# Calculate variable importance for column objects produced by train and model specific methods.
# ... Could use result set to improve model creation efficiency.
plot(varImp(rf_model))

# Test Model
pred <- predict(rf_model, newdata=testing)

confMatrix <- confusionMatrix(pred,testing$classe)

# Output Results.
confMatrix
#prop.table(confMatrix)

# Output Accuracy.
confMatrix$overall[1]

# Used a Random Forest model and therefore there is no need for cross-validation.
# There is also no need for a separate Test Set to get an unbiased estimate of the Test Set Error.