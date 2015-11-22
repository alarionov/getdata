library(dplyr)
library(rapportools)
library(reshape2)

set.seed(31337)

# Merges the training and the test sets to create one data set
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = F)[,2]

train <- data.frame(
  read.table("UCI HAR Dataset/train/subject_train.txt"),
  read.table("UCI HAR Dataset/train/y_train.txt"),
  read.table("UCI HAR Dataset/train/X_train.txt")
)

test <- data.frame(
  read.table("UCI HAR Dataset/test/subject_test.txt"),
  read.table("UCI HAR Dataset/test/y_test.txt"),
  read.table("UCI HAR Dataset/test/X_test.txt")
)

combined <- rbind(train, test)
colnames(combined) <- c('Subject', 'ActivityType', features)

combined <- combined[ , !duplicated(colnames(combined))]

# Extracts only the measurements on the mean and standard deviation for each measurement. 
combined <- select(combined, Subject, ActivityType, matches('mean|sd'))

# Uses descriptive activity names to name the activities in the data set
combined$ActivityType <- as.factor(combined$ActivityType)
levels(combined$ActivityType) <- read.table("UCI HAR Dataset/activity_labels.txt")[,2]

# Appropriately labels the data set with descriptive variable names
col_names <- colnames(combined)
col_names <- gsub('^t',       "Time",          col_names)
col_names <- gsub('^f',       "Frequency",     col_names)
col_names <- gsub("Acc",      "Accelerometer", col_names)
col_names <- gsub('Freq',     "Frequency",     col_names)
col_names <- gsub("tBody",    "TimeBody",      col_names)
col_names <- gsub("Gyro",     "Gyroscope",     col_names)
col_names <- gsub("BodyBody", "Body",          col_names)
col_names <- gsub("Mag",      "Magnitude",     col_names)
col_names <- gsub("angle",    "Angle",         col_names)
col_names <- gsub("gravity",  "Gravity",       col_names)
colnames(combined) <- tocamel(col_names, upper = T)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
melted <- melt(combined, id=c("Subject","ActivityType"))
tidy   <- dcast(melted, Subject + ActivityType ~ variable, mean)

write.table(tidy, 'tidy.txt', row.name=FALSE)