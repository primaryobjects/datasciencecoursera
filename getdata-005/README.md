Human Activity Recognition Using Smartphones Data Set Analysis
=====

The program run_analysis.R is part of a project for the online class, "[Getting and Cleaning Data](https://class.coursera.org/getdata-005)". The program analyzes the [Human Activity Recognition](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) data set to transform and model the data.

The program produces the following two files: meanStd.csv and tidy.csv.

## Usage

Download the [data set](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip) and set your working directory so the folder *"UCI HAR Dataset"* exists within the directory.

    R run_analysis.R

The program will automatically read the train and test data sets to merge them into a single set, returning a data.frame named *data*. Columns are labeled from the list of features and activity labels are provided for each row. The method createMeanStdData(data) is called to produce meanStd.csv. The method createTidyData(data) is called to produce tidy.csv.

## Output


### meanStd.csv

Contains the merged set of train and test data, extracting only features relating to *mean* and *standard deviation*. Each row is labeled with an activity name. Each column is labeled with a feature name, sourced from *UCI HAR Dataset/features.txt*.

### tidy.csv

Contains a tidy data set from the merged set of train and test data. The tidy data set contains the average of each feature for each activity and each subject. Data is displayed in a *wide* table format, with each column including a specific feature, and each row including a subject/activity combination.

The original data set contains 30 subjects and 6 activities. A row exists in the tidy data set for each subject's activity. This results in 6 rows per subject, displaying the average (mean) value for each feature.

## Features

See CodeBook.md for details on data contents and labels.

