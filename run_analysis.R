#  Coursera: Getting and Cleaning Data, getdata-015
#  Course Project - run_analysis.R
## The goal is to prepare tidy data that can be used for later analysis. 
## You will be graded by your peers on a series of yes/no questions related to the project. 
## You will be required to submit: 
###  1) a tidy data set as described below, 
###  2) a link to a Github repository with your script for performing the analysis, and 
###  3) a code book that describes the variables, the data, and any transformations or 
####   work that you performed to clean up the data called CodeBook.md. 
## You should also include a README.md in the repo with your scripts. 
## This repo explains how all of the scripts work and how they are connected.
#
#  Steps -  You should create one R script called run_analysis.R that does the following. 
#  1. Merges the training and the test sets to create one data set.
#  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#  3. Uses descriptive activity names to name the activities in the data set
#  4. Appropriately labels the data set with descriptive variable names. 
#  5. From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.

# The Input Data files:
## The data are split between two files: X_train.txt and X_test.txt
## The description of the data columns for both data files is contained in a single file, features.txt
## The subject description of the data rows is contained in two files: subject_train.txt and subject_test.txt
## The activity description of the data rows is contained in two files: y_train.txt and y_test.txt

# Location of the Data files
print( 'Step 0 - File Reads and set up')
Train_Dat <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/train/X_train.txt"
Test_Dat <-  "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/test/X_test.txt"

# Descriptors of the Train File:   Column Names, Row Names, and Activites
Train_Columns <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/features.txt"
Train_Rows <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/train/subject_train.txt"
Train_acts <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/train/y_train.txt"

# Reading in the Descriptor Data for the train file
Tcolumns <- read.table(Train_Columns,  header=FALSE)
Trows <- read.table(Train_Rows,  header=FALSE, col.names='Subject')
TAct <- read.table( Train_acts, header=FALSE,  col.names='Activity')
dim(TAct)
# TAct[ ,1]
table( TAct)
# TAct is a list containing the type of activity: 1: Walking , . . ., 6: Laying
activities <- c('Walking', 'Walking Upstairs', 'Walking Downstairs', 'Sitting', 
    'Standing', 'Laying')
# Make the TAct list into a factor
Activity <- factor( TAct[ ,1], labels = activities)
dim(Activity)
is.factor( Activity)
table( Activity)

typeof(Tcolumns)
names(Tcolumns)
head(Tcolumns$V2)
dim( Trows)
head( Trows)


# Reading in train data - Note Column Names or Labels Are Included
train <- read.table(Train_Dat, header=FALSE, col.names = Tcolumns$V2 )
typeof(train)
dim(train)
head(train[1, 1:10])

# Adds Subject ID and activity as the 1st columns
train_all <- cbind(Trows, Activity, train )
dim(train)
train[ 1:2 , c(1,2,3,560:561)]

# The Test File Location and Its Descriptors - Column Names Have Already Been loaded
Test_rows <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/test/subject_test.txt"
TestAct <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/UCI_HAR_Dataset/test/y_test.txt"

TrowsTest <- read.table(Test_rows,  header=FALSE, col.names='Subject')
TestActivities <- read.table( TestAct, header=FALSE,  col.names='Activity')
head(TrowsTest)
table(TestActivities)
Activity <- factor( TestActivities[ ,1], labels = activities)
table(Activity)

# Reading in the test data with Column Names or Labels
test <- read.table(Test_Dat, header=FALSE, col.names = Tcolumns$V2 )
dim( test)
head(test[1, 1:10])

# Adds Subject ID and activities to the data
test_All <- cbind(TrowsTest, Activity, test)
dim(test)
head(test[ 1, 1:10])
# names( test)
# End of Data Setup

# Step 1:  Merges the training and the test sets to create one data set.
## The two files have identical column names.  The same Column Name applies to the 
## train and test data.  In addition, the Subject and Activity Variables are added
## in the same location
Full_Sample <- data.frame( rbind( train_all, test_All) )
dim( Full_Sample)
typeof( Full_Sample)
names(Full_Sample)
head( Full_Sample[1:8])

# Step 2:  Extracts only the measurements on the mean and standard deviation
# for each measurement.
## Means_Std_Column_Names <- Tcolumns[grep("mean()|std()", Tcolumns$V2), ]
## dim(Means_Std_Column_Names)
## Means_Std_Column_Names


library(dplyr)
# New file contains: subject, activity, and any variable containing the label 'mean' or 'std'
Sample_Means_Stds <- select(Full_Sample, matches("Subject|Activity|mean|std"))
head(Sample_Means_Stds[1:10])
dim( Sample_Means_Stds )
namesColumns <- names(Sample_Means_Stds )
namesColumns[1:5]

# Step 3: Uses descriptive activity names to name the activities in the data set
# Already inserted when the data were set up
print( 'Step 3: descriptive activity names added')
table(Sample_Means_Stds$Activity )

# Step 4: Appropriately labels the data set with descriptive variable names.
# Already inserted when the data read in
print('Step 4: Variable Names')
names(Sample_Means_Stds)

# Step 5: From the data set in step 4, creates a second, 
#  independent tidy data set with the average of each variable for each activity 
#  and each subject.
#  The Final Dataset, Subject_Means2, is a summary by subject and activity for 86 vars
print( 'Step 5: Means by subject and activity')
table(Sample_Means_Stds$Subject, Sample_Means_Stds$Activity )

Ordered <- Sample_Means_Stds[order(Sample_Means_Stds$Subject),]
dim(Ordered)
Ordered[1:2, 1:5]
# Mean or average for each activity and subject
Subject_Means2 <- aggregate(Ordered[,3:88], by=list(Ordered$Subject, Ordered$Activity), FUN=mean )
dim(Subject_Means2)
Subject_Means2[1:4, 1:6]


# Subject_Means <- by(Ordered[,2:562] , Ordered$Subject , colMeans)
# dim(Subject_Means)
# Subject_Means[ 1]
#apply( Full_Sample[  2:562] ,2, mean)
#Subject_Means <- sapply( split( Full_Sample[, 2:562], Full_Sample[$Subject[,1]), mean)
#warnings()
#Matrix_Data <- data.matrix(Full_Sample[2:562])
#dim(Matrix_Data)
#is.atomic( Full_Sample)
#is.atomic( Matrix_Data)
#Matrix_Data[ ,2]
#testing <- tapply(Matrix_Data[ ,2] , INDEX = list(Matrix_Data[,1]), mean)
#dim(testing)
#aggregate( list(Full_Sample[, 2:562]),by=list(Full_Sample[1]), mean)

# Please upload the tidy data set created in step 5 of the instructions.
# Please upload your data set as a txt file created with write.table() using 
# row.name=FALSE (do not cut and paste a dataset directly into the text box, 
# as this may cause errors saving your submission).
Output_File <- "/home/dybalskig/Getting_and_Cleaning_Data/Project/Project.txt"
Output_File
write.table(Subject_Means2, file =Output_File, row.name=FALSE)

