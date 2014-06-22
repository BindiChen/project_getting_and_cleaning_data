# Step 1
trainDataX <- read.csv("./train/X_train.txt", header = F, sep="", colClasses = "numeric")
trainSubject <- read.csv("./train/subject_train.txt", header = F, sep="", colClasses = "integer")
trainDataY <- read.csv("./train/y_train.txt", header = F, sep="", colClasses = "integer")
trainData <- cbind(trainDataX,trainSubject)
trainData <- cbind(trainData,trainDataY)

testDataX <- read.csv("./test/X_test.txt", header = F, sep="", colClasses = "numeric")
testSubject <- read.csv("./test/subject_test.txt", header = F, sep="", colClasses = "integer")
testDataY <- read.csv("./test/y_test.txt", header = F, sep="", colClasses = "integer")
testData <- cbind(testDataX,testSubject)
testData <- cbind(testData,testDataY)

data <- rbind(trainData, testData)

# Step 2

label <- read.csv("./features.txt", header = F, sep="", colClasses="character")  # Read as Character
index <- which(grepl("mean()",label$V2) | grepl("std()",label$V2))

extractedLabel <- label[index, 2]
# adding the subject and activity column
index[length(index) + 1] <- 562
index[length(index) + 1] <- 563

extractedData <- data[,index]

# Step 3
extractedData <- extractedData[sort(extractedData[,ncol(extractedData)]),]
extractedData[,ncol(extractedData)] <- as.factor(extractedData[,ncol(extractedData)])
levels(extractedData[,ncol(extractedData)]) <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

# Step 4

for(i in 1:length(extractedLabel))
{
  currentLabel <- extractedLabel[i]
  
  currentLabel <- gsub("tBody", "Time.Body.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("fBody", "Frequency.Body.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("tGravity", "Time.Gravity.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("fGravity", "Frequency.Gravity.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("Acc", "Acceleration.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("Gyro", "Gyroscopic.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("Mag", "Magnitude.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("Jerk", "Jerk.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("-std", "StdDev.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("-mean", "Mean.", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("-", ".", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("[.][.]", ".", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("[.][(][)]", "", currentLabel, ignore.case = FALSE)
  currentLabel <- gsub("[(][)]", "", currentLabel, ignore.case = FALSE)
  extractedLabel[i] <- currentLabel
}

# adding label for the subject and activity
extractedLabel[length(extractedLabel)+1] <- "Subject"
extractedLabel[length(extractedLabel)+1] <- "Activity"
colnames(extractedData) <- extractedLabel

write.csv(extractedData, 'extractedData.txt')

