Human Activity Recognition Using Smartphones Data Set Analysis
=====

The program run_analysis.R is part of a project for the online class, "[Getting and Cleaning Data](https://class.coursera.org/getdata-005)". The program analyzes the [Human Activity Recognition](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) data set to transform and model the data.

## Usage

Download the [data set](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip) and set your working directory so the folder *"UCI HAR Dataset"* exists within the directory.

    R run_analysis.R

The program produces the following two files: [meanStd.csv](https://raw.githubusercontent.com/primaryobjects/datasciencecoursera/master/getdata-005/meanStd.csv) and [tidy.csv](https://github.com/primaryobjects/datasciencecoursera/blob/master/getdata-005/tidy.csv).

## Output


### meanStd.csv

Contains the merged set of train and test data, extracting only features relating to *mean* and *standard deviation* measurements (-mean() and -std() columns). Each row is labelled with a descriptive activity name. Each column is labelled with a descriptive feature name, sourced from *UCI HAR Dataset/features.txt*.

This file contains the dimensions 10299x68, consisting of 10,299 rows (train and test rows merged together) and 68 columns (66 features related to (33) mean and (33) standard deviation + activity + subject).

### tidy.csv

Contains a tidy data set from the merged set of train and test data. The tidy data set contains the average of each feature from meanStd.csv for each activity and each subject. Data is displayed in a *wide* table format, with each column including a specific feature, and each row including a subject/activity combination.

The original data set contains 30 subjects and 6 activities. A row exists in the tidy data set for each subject's activity. This results in 6 rows per subject, displaying the average (mean) value for each feature.

This file contains the dimensions 180x68, consisting of 180 rows (30 * 6 = subjects * activities) and 68 columns (66 features + subject + activity).

## Features

See [CodeBook.md](https://github.com/primaryobjects/datasciencecoursera/blob/master/getdata-005/CodeBook.md) for details on data contents and labels.

## Author

Kory Becker http://www.primaryobjects.com/kory-becker.aspx
