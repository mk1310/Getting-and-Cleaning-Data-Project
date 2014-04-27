##Provide the directory location for main folder under which test and train data folders are located
directory <- "D:/csra/getting&cleaningdata/assign/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset"

## Read traing data files

X_train <- read.table(paste(directory, "/train/X_train.txt",sep=""), quote="\"")
y_train <- read.table(paste(directory, "/train/y_train.txt",sep=""), quote="\"")
subject_train <- read.table(paste(directory, "/train/subject_train.txt",sep=""), quote="\"")

## Read test data files

X_test <- read.table(paste(directory, "/test/X_test.txt",sep=""), quote="\"")
y_test <- read.table(paste(directory, "/test/y_test.txt",sep=""), quote="\"")
subject_test <- read.table(paste(directory, "/test/subject_test.txt",sep=""), quote="\"")

## rename variable names for subject & activity variables before merging
colnames(subject_train)[1] <- "subject"
colnames(y_train)[1] <- "activity"

colnames(subject_test)[1] <- "subject"
colnames(y_test)[1] <- "activity"

## merge subject, y_test & X_test to create single test dataset
mtest <- cbind(subject_test, y_test, X_test)

## merge subject, y_train & X_train to create single training dataset
mtrain <- cbind(subject_train, y_train, X_train)

## merge mtrain & mtest to create single combined training & test data dataset
m_test_train <- rbind(mtrain, mtest)

################Extracting only the measurements on the mean and standard deviation for each measurement. ################
################List of variables ################
################"V1",  ################
################"V2",  ################
################"V3",  ################
################"V4",  ################
################"V5",  ################
################"V6",  ################
################"V41", ################
################"V42", ################
################"V43", ################
################"V44", ################
################"V45", ################
################"V46", ################
################"V81", ################
################"V82", ################
################"V83", ################
################"V84", ################
################"V85", ################
################"V86", ################
################"V121",################
################"V122",################
################"V123",################
################"V124",################
################"V125",################
################"V126",################
################"V161",################
################"V162",################
################"V163",################
################"V164",################
################"V165",################
################"V166",################
################"V201",################
################"V202",################
################"V214",################
################"V215",################
################"V227",################
################"V228",################
################"V240",################
################"V241",################
################"V253",################
################"V254",################
################"V266",################
################"V267",################
################"V268",################
################"V269",################
################"V270",################
################"V271",################
################"V345",################
################"V346",################
################"V347",################
################"V348",################
################"V349",################
################"V350",################
################"V424",################
################"V425",################
################"V426",################
################"V427",################
################"V428",################
################"V429",################
################"V503",################
################"V504",################
################"V516",################
################"V517",################
################"V529",################
################"V530",################
################"V542",################
################"V543",################


##creating new dataset with selected variables
m_tt_new <- m_test_train[c("activity",
			   "subject",
			   "V1",
                           "V2",
                           "V3",
                           "V4",
                           "V5",
                           "V6",
                           "V41",
                           "V42",
                           "V43",
                           "V44",
                           "V45",
                           "V46",
                           "V81",
                           "V82",
                           "V83",
                           "V84",
                           "V85",
                           "V86",
                           "V121",
                           "V122",
                           "V123",
                           "V124",
                           "V125",
                           "V126",
                           "V161",
                           "V162",
                           "V163",
                           "V164",
                           "V165",
                           "V166",
                           "V201",
                           "V202",
                           "V214",
                           "V215",
                           "V227",
                           "V228",
                           "V240",
                           "V241",
                           "V253",
                           "V254",
                           "V266",
                           "V267",
                           "V268",
                           "V269",
                           "V270",
                           "V271",
                           "V345",
                           "V346",
                           "V347",
                           "V348",
                           "V349",
                           "V350",
                           "V424",
                           "V425",
                           "V426",
                           "V427",
                           "V428",
                           "V429",
                           "V503",
                           "V504",
                           "V516",
                           "V517",
                           "V529",
                           "V530",
                           "V542",
                           "V543"
)]


##Appropriately labeling the data set with descriptive activity names using the following activity definitions
## Activity definitions
################1 WALKING		################
################2 WALKING_UPSTAIRS	################
################3 WALKING_DOWNSTAIRS	################
################4 SITTING		################
################5 STANDING		################
################6 LAYING		################


## converting the variable activity from integer to factor datatype
m_tt_new$activity <- as.factor(m_tt_new$activity)

## assigning value labeling to the activity variable
m_tt_new$activity <- factor(m_tt_new$activity,
levels = c(1,2,3,4,5,6),
labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))


##Creating a second, independent tidy data set with the average of each variable for each activity and each subject.

##calling data.table library for grouping data in an efficient way
library(data.table)

dt <- data.table(m_tt_new)
newdat <- dt[,list(Avg_tBodyAcc_mean_X = mean(V1) ,
		Avg_tBodyAcc_mean_Y = mean(V2) ,
		Avg_tBodyAcc_mean_Z = mean(V3) ,
		Avg_tBodyAcc_std_X = mean(V4) ,
		Avg_tBodyAcc_std_Y = mean(V5) ,
		Avg_tBodyAcc_std_Z = mean(V6) ,
		Avg_tGravityAcc_mean_X = mean(V41) ,
		Avg_tGravityAcc_mean_Y = mean(V42) ,
		Avg_tGravityAcc_mean_Z = mean(V43) ,
		Avg_tGravityAcc_std_X = mean(V44) ,
		Avg_tGravityAcc_std_Y = mean(V45) ,
		Avg_tGravityAcc_std_Z = mean(V46) ,
		Avg_tBodyAccJerk_mean_X = mean(V81) ,
		Avg_tBodyAccJerk_mean_Y = mean(V82) ,
		Avg_tBodyAccJerk_mean_Z = mean(V83) ,
		Avg_tBodyAccJerk_std_X = mean(V84) ,
		Avg_tBodyAccJerk_std_Y = mean(V85) ,
		Avg_tBodyAccJerk_std_Z = mean(V86) ,
		Avg_tBodyGyro_mean_X = mean(V121) ,
		Avg_tBodyGyro_mean_Y = mean(V122) ,
		Avg_tBodyGyro_mean_Z = mean(V123) ,
		Avg_tBodyGyro_std_X = mean(V124) ,
		Avg_tBodyGyro_std_Y = mean(V125) ,
		Avg_tBodyGyro_std_Z = mean(V126) ,
		Avg_tBodyGyroJerk_mean_X = mean(V161) ,
		Avg_tBodyGyroJerk_mean_Y = mean(V162) ,
		Avg_tBodyGyroJerk_mean_Z = mean(V163) ,
		Avg_tBodyGyroJerk_std_X = mean(V164) ,
		Avg_tBodyGyroJerk_std_Y = mean(V165) ,
		Avg_tBodyGyroJerk_std_Z = mean(V166) ,
		Avg_tBodyAccMag_mean = mean(V201) ,
		Avg_tBodyAccMag_std = mean(V202) ,
		Avg_tGravityAccMag_mean = mean(V214) ,
		Avg_tGravityAccMag_std = mean(V215) ,
		Avg_tBodyAccJerkMag_mean = mean(V227) ,
		Avg_tBodyAccJerkMag_std = mean(V228) ,
		Avg_tBodyGyroMag_mean = mean(V240) ,
		Avg_tBodyGyroMag_std = mean(V241) ,
		Avg_tBodyGyroJerkMag_mean = mean(V253) ,
		Avg_tBodyGyroJerkMag_std = mean(V254) ,
		Avg_fBodyAcc_mean_X = mean(V266) ,
		Avg_fBodyAcc_mean_Y = mean(V267) ,
		Avg_fBodyAcc_mean_Z = mean(V268) ,
		Avg_fBodyAcc_std_X = mean(V269) ,
		Avg_fBodyAcc_std_Y = mean(V270) ,
		Avg_fBodyAcc_std_Z = mean(V271) ,
		Avg_fBodyAccJerk_mean_X = mean(V345) ,
		Avg_fBodyAccJerk_mean_Y = mean(V346) ,
		Avg_fBodyAccJerk_mean_Z = mean(V347) ,
		Avg_fBodyAccJerk_std_X = mean(V348) ,
		Avg_fBodyAccJerk_std_Y = mean(V349) ,
		Avg_fBodyAccJerk_std_Z = mean(V350) ,
		Avg_fBodyGyro_mean_X = mean(V424) ,
		Avg_fBodyGyro_mean_Y = mean(V425) ,
		Avg_fBodyGyro_mean_Z = mean(V426) ,
		Avg_fBodyGyro_std_X = mean(V427) ,
		Avg_fBodyGyro_std_Y = mean(V428) ,
		Avg_fBodyGyro_std_Z = mean(V429) ,
		Avg_fBodyAccMag_mean = mean(V503) ,
		Avg_fBodyAccMag_std = mean(V504) ,
		Avg_fBodyBodyAccJerkMag_mean = mean(V516) ,
		Avg_fBodyBodyAccJerkMag_std = mean(V517) ,
		Avg_fBodyBodyGyroMag_mean = mean(V529) ,
		Avg_fBodyBodyGyroMag_std = mean(V530) ,
		Avg_fBodyBodyGyroJerkMag_mean = mean(V542) ,
		Avg_fBodyBodyGyroJerkMag_std = mean(V543) ) ,
	by=c("activity","subject")]
	
head(newdat)
