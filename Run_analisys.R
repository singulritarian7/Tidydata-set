library(dplyr)
setwd("C:/Users/pruebas/Documents/TIDY_DATASET")

# I download the file straightforard from the instructions asingment to my computer 
# and also unzip it, so i didn´t used download.file and unzip functions.

# load train data
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")

# load test data
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")

# load variables 
features <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Merges the training and the test sets to create one data set.
X <- rbind(X_train,X_test)
Y <- rbind(Y_train,Y_test)
subject <- rbind(subject_train,subject_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.

# First i set variable name to variable X and then i used grep function with REGEX to search 
# indices that contain mean and std in the features variable. \\ means escape character.

names(X) <- features[ ,2]
indices <- grep("-mean\\(\\)|-std\\(\\)",features[ ,2])
extracted <- X[ ,indices]

# Uses descriptive activity names to name the activities in the data set

colnames(Y) <- "humanactivity"
Y$activity <- factor(Y$humanactivity, labels = as.character(activity_labels[ ,2]))
activity <- Y$activity

# Appropriately labels the data set with descriptive variable names.
# I only set names for the variables that don´t have names,because i have set some names 
# of some variables before

names(extracted) <- features[indices,2]
names(extracted) <- gsub("\\(|\\)","",names(extracted))
names(extracted) <- tolower(names(extracted))

names(subject) <- "subject"

# Create a new dataset with variables extracted, activity, subject

new <- cbind(extracted,activity)
new <- cbind(new,subject)

# From the data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject.

summary <- new %>% group_by(activity,subject) %>% summarize_each(list(mean=mean))

# Write final tidy data

write.table(summary,file="tidydata.txt",row.name=FALSE)


