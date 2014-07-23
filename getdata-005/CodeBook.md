DATA DICTIONARY - Human Activity Recognition Using Smartphones Tidy Data Set
====

###subject
Numeric subject identifier  
*1..30*

###activity
Activity label  
*LAYING  
SITTING  
STANDING  
WALKING  
WALKING_DOWNSTAIRS  
WALKING_UPSTAIRS*

###Features
Average (mean) floating point value calculated for each feature in the "Human Activity Recognition Using Smartphones" data set, across each activity and each subject. In the original data set, each subject has multiple measurements of activity for each feature. These measurements are averaged together to produce a single value for each feature/activity combination. *See /UCI HAR Dataset/features_info.txt*.  

*tBodyAccMeanX  
tBodyAccMeanY  
tBodyAccMeanZ  
tBodyAccStdX  
tBodyAccStdY  
tBodyAccStdZ  
tGravityAccMeanX  
tGravityAccMeanY  
tGravityAccMeanZ  
tGravityAccStdX  
tGravityAccStdY  
tGravityAccStdZ  
tBodyAccJerkMeanX  
tBodyAccJerkMeanY  
tBodyAccJerkMeanZ  
tBodyAccJerkStdX  
tBodyAccJerkStdY  
tBodyAccJerkStdZ  
tBodyGyroMeanX  
tBodyGyroMeanY  
tBodyGyroMeanZ  
tBodyGyroStdX  
tBodyGyroStdY  
tBodyGyroStdZ  
tBodyGyroJerkMeanX  
tBodyGyroJerkMeanY  
tBodyGyroJerkMeanZ  
tBodyGyroJerkStdX  
tBodyGyroJerkStdY  
tBodyGyroJerkStdZ  
tBodyAccMagMean  
tBodyAccMagStd  
tGravityAccMagMean  
tGravityAccMagStd  
tBodyAccJerkMagMean  
tBodyAccJerkMagStd  
tBodyGyroMagMean  
tBodyGyroMagStd  
tBodyGyroJerkMagMean  
tBodyGyroJerkMagStd  
fBodyAccMeanX  
fBodyAccMeanY  
fBodyAccMeanZ  
fBodyAccStdX  
fBodyAccStdY  
fBodyAccStdZ  
fBodyAccJerkMeanX  
fBodyAccJerkMeanY  
fBodyAccJerkMeanZ  
fBodyAccJerkStdX  
fBodyAccJerkStdY  
fBodyAccJerkStdZ  
fBodyGyroMeanX  
fBodyGyroMeanY  
fBodyGyroMeanZ  
fBodyGyroStdX  
fBodyGyroStdY  
fBodyGyroStdZ  
fBodyAccMagMean  
fBodyAccMagStd  
fBodyAccJerkMagMean  
fBodyAccJerkMagStd  
fBodyGyroMagMean  
fBodyGyroMagStd  
fBodyGyroJerkMagMean  
fBodyGyroJerkMagStd*  

### Note

The original data set is part of the Human Activity Recognition Using Smartphones data, available at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#

The following abbreviations are used within feature names.  
*t: Time domain signals captured at a constant rate of 50 Hz  
f: Fast Fourier Transform values  
Mean: Mean value  
Std: Standard deviation  
XYZ: 3-axial signals in the X, Y and Z directions  
Acc: Accelerometer measurement  
Body: Body measurement  
Gravity: Gravity acceleration signal  
Jerk: Body linear acceleration and angular velocity derived in time  
Gyro: Gyroscope measurement  
Mag: Magnitude of these three-dimensional signals, calculated using the Euclidean norm*  

Refer to /UCI HAR Dataset/features_info.txt for descriptions of each feature.

Author
Kory Becker http://www.primaryobjects.com/kory-becker.aspx
