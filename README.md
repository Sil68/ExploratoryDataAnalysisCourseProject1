**********

# Purpose

In the course of partaking an online class regarding data analysis, in particular the class _*Exploratory Data Analysis*_, one assignment was a project with the purpose to demonstrate the student's ability to collect, work with, and plot a data set in various ways.

**********

# Introduction

This assignment uses data from the [UC Irvine Machine Learning Repository][ucihome], a popular repository for machine learning datasets. In particular, we will be using the "Individual household electric power consumption Data Set" which have been made available on the [course web site][].

**********

# Assignment

Create a set of R script which carry out the following steps:

1. Load the data;
2. Create four R scripts (_*```plot1.R```*_, _*```plot2.R```*_, _*```plot3.R```*_, _*```plot4.R```*_);
3. Create four plots by facilitating merely the base plotting system, and by executing each of the four aforementioned R scripts correspondingly;
4. Save each of these plots to a corresponding PNG file, each of size 480x480 pixels;
5. Submit the script and the generate PNG files to a GitHub repository.

**********

# Quick start

1. Download the [repository][ghrepo] files to your local machine;
2. Open the R project file (_*```course project 1.Rproj```*_);
3. Open and run the main R script (_*```plot.R```*_).

**********

# Project

The project comprises several directories and files, all of which are outlined below.

## Directory structure

In the project directory you will find several subdirectories.

```
<project directory>/
  +- course project 1.Rproj
  +- data/
  |    +- exdata%2Fdata%2Fhousehold_power_consumption.zip
  |    +- household_power_consumption.txt
  |    +- reference-plot-1.png
  |    +- reference-plot-2.png
  |    +- reference-plot-3.png
  |    +- reference-plot-4.png
  +- doc/
  +- lib/
  |    +- bldurl.R
  |    +- chkdir.R
  |    +- chkurl.R
  |    +- dldat.R
  |    +- estuciepc.R
  |    +- plot1.R
  |    +- plot2.R
  |    +- plot3.R
  |    +- plot4.R
  |    +- rduciepc.R
  |    +- setusragnt.R
  |    +- sstuciepc.R
  +- plot.R
  +- publishing/
  +- README.md (GitHub compatible)
  +- README.Rmd
```

The _*```data```*_ directory contains the downloaded data file, both as a compressed archive (zip), as well as the uncompress individual data files.

Any additional, supporting scripts or libraries are stored inside the _*```lib```*_ directory.

## Files

For reasons of readability, testing, profiling, benchmarking and re-usability, instead of creating one single script file, an approach of splitting the script into several functions, each put into a corresponding script file of its own, placed into the _*```lib```*_ directory, has been taken.

### plot.R

The main entry point, the main script, Upon which execution all required libraries and supporting scripts are getting loaded, as well as global variables and constants are defined.

Steps carried out by this script:

1. check for existence of the _*```doc```*_, _*```data```*_, and _*```publishing```*_ directories, and create these if required;
2. download and uncompress data file (raw data) if not already available;
3. estimate the size of main memory required for loading the data set into the memory;
4. load the data set into a data.table _*```dtInDat```*_;
5. extract a subset of data.table _*```dtInDat```*_;
6. create the four plots ([plot1.png][plot1], [plot2.png][plot2], [plot3.png][plot3], and [plot4.png][plot4]) by executing the corresponding scripts (_*```lib/plot1.R```*_, _*```lib/plot2.R```*_, _*```lib/plot3.R```*_, and _*```lib/plot4.R```*_).

### lib/bldurl.R

Assemble an URL from individual parts by concatenating these, encode it and/or check it for existance if requested.

|Parameter|Description|
|---------|-----------|
|...|individual components of the URL|
|encURL|if TRUE, the URL gets encoded after assembly|
|chkURL|if TRUE, the URL's existance gets verified|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; URL has been assembled (and is existing)|
|FALSE|failure; URL couldn't be assembled (or is not existing)|

Table: Output value

### lib/chkdir.R

Check whether a specific directory is already existing, and if not create that directory.

|Parameter|Description|
|---------|-----------|
|dname|vector of directory names to check/create|
|mkdir|create directories if non-existing|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; directories are existing/have been created successfully|
|FALSE|failure; directories are not existing/cannot be created|

Table: Output value

### lib/chkurl.R

Check whether a specific URL is valid, and if it can be accessed.

|Parameter|Description|
|---------|-----------|
|valURLurl|vector of URLs to check/validate|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; vector; URLs are valid and accessible|
|FALSE|failure: vector; URLs are not existing/cannot be accessed|

Table: Output value

### lib/dldat.R

Download data into indicated directory, expand if requested, and rename as specified.

|Parameter|Description|
|---------|-----------|
|dlname|vector of files to downloads|
|dldir|vector of directories files to download to|
|fname|local filenames of downloaded files|
|exp|expand downloaded files?|
|redl|re-download files?|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; directory is existing/has been created successfully|
|FALSE|failure; directory does not exist/cannot be created|

Table: Output value

**Note**  
Case #1) _*```dlname```*_, _*```dldir```*_, _*```fname```*_ have to be of identical length, -or-  
Case #2) _*```dlname```*_ and _*```fname```*_ have to be of identical length, and _*```dldir```*_ has to be of length 1.

### lib/estuciepc.R

Estimate the aount of memory required to load and store the read raw data.

|Parameter|Description|
|---------|-----------|
|basedir|base directory to read files from|
|fname|file to read|
|unts|unit of size returned ("b", "k", "m", "g")|

Table: Input parameter

|Result|Description|
|------|-----------|
|estimated memory size|success; rough estimate of the memory required to store the raw data|
|NULL|failure|

Table: Output value

**Note**  

+ both--baseDir and fname--have to be provided, and set to a non-NULL value;
+ "b" = bytes; "k" = kilo bytes, "m" = mega bytes, "g" = giga bytes.

### lib/plot1.R

Plot the data provided both to the screen as well as a file (format png) by facilitating the R base plot system only

**Plot specification:**  

+ histogram;
+ global active power (x-axis);
+ frequency (y-axis).

|Parameter|Description|
|---------|-----------|
|dtPlt|data table containing the data to plot|
|fname|name of the file the plot has to sent to|
|imgSize|size/dimension (<pixels height>x<pixels width>x<resolution>) of the plot generated and written to file|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; data have been plot both the screen as well as the file|
|FALSE|failure|

Table: Output value

**Note**  
All of dtPlt, fname, and imgSize have to be provided, and set to a non-NULL value.  

### lib/plot2.R

Plot the data provided both to the screen as well as a file (format png) by facilitating the R base plot system only

**Plot specification:**  

+ _"time series"_;
+ date & time of day (x-axis);
+ global active power (y-axis).

|Parameter|Description|
|---------|-----------|
|dtPlt|data table containing the data to plot|
|fname|name of the file the plot has to sent to|
|imgSize|size/dimension (<pixels height>x<pixels width>x<resolution>) of the plot generated and written to file|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; data have been plot both the screen as well as the file|
|FALSE|failure|

Table: Output value

**Note**  
All of dtPlt, fname, and imgSize have to be provided, and set to a non-NULL value.  

### lib/plot3.R

Plot the data provided both to the screen as well as a file (format png) by facilitating the R base plot system only

**Plot specification:**  

+ _"time series"_;
+ date & time of day (x-axis);
+ sub metering 1, sub metering 2, sub metering 3 (y-axis).

|Parameter|Description|
|---------|-----------|
|dtPlt|data table containing the data to plot|
|fname|name of the file the plot has to sent to|
|imgSize|size/dimension (<pixels height>x<pixels width>x<resolution>) of the plot generated and written to file|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; data have been plot both the screen as well as the file|
|FALSE|failure|

Table: Output value

**Note**  
All of dtPlt, fname, and imgSize have to be provided, and set to a non-NULL value.  

### lib/plot4.R

Plot the data provided both to the screen as well as a file (format png) by facilitating the R base plot system only

**Plot specification:**  

+ four plots (2x2) on one single page;
+ _"time series"_;
+ plot #1 (top left):
    - date & time of day (x-axis);
    - global active power (y-axis);
+ plot #2 (top right):
    - date & time of day (x-axis);
    - voltage (y-axis);
+ plot #3 (bottom left):
    - date & time of day (x-axis);
    - sub metering 1, sub metering 2, sub metering 3 (y-axis);
+ plot #4 (bottom right):
    - date & time of day (x-axis);
    - global re-active power (y-axis).

|Parameter|Description|
|---------|-----------|
|dtPlt|data table containing the data to plot|
|fname|name of the file the plot has to sent to|
|imgSize|size/dimension (<pixels height>x<pixels width>x<resolution>) of the plot generated and written to file|

Table: Input parameter

|Result|Description|
|------|-----------|
|TRUE|success; data have been plot both the screen as well as the file|
|FALSE|failure|

Table: Output value

**Note**  
All of dtPlt, fname, and imgSize have to be provided, and set to a non-NULL value.  

### lib/rduciepc.R

Read raw data (electric power consumption imported from the UC Irvine Machine Learning Repository) into a corresponding data structure.

The variable _*```Date```*_ is getting converted into class _*```POSIXct```*_, and an additional variable _*```DateTime```*_ time is created by executing

    rc[, DateTime := dmy_hms(paste(Date, Time))]
    rc[, Date := dmy(Date)]

(_*```dmy_hms```*_ and _*```dmy```*_ are provided by the _*```lubridate```*_ package)

|Parameter|Description|
|---------|-----------|
|basedir|base directory to read files from|
|fname|file to read|

Table: Input parameter

|Result|Description|
|------|-----------|
|data table|success; data table containing the read raw data|
|NULL|failure|

Table: Output value

**Note**  

+ both--baseDir and fname--have to be provided, and set to a non-NULL value.

### lib/setusragnt.R

Set user agent to a "real" browser (instead of pointing to the R environment/session).

|Parameter|Description|
|---------|-----------|
|NONE||

Table: Input parameter

|Result|Description|
|------|-----------|
|NONE||

Table: Output value

### lib/sstuciepc.R

Extract a subset of the read raw data (electric power consumption imported from the UC Irvine Machine Learning Repository).

|Parameter|Description|
|---------|-----------|
|dtAll|data table containing the original data to be subsetted|
|fromDate|subset dtAll with Date >= fromDate|
|toDate|subset dtAll with Date <= toDate|

Table: Input parameter

|Result|Description|
|------|-----------|
|data table|success; data table containing the requested subset of the raw data|
|NULL|failure|

Table: Output value

**Note:**  

+ all of dtAll, fromDate, toDate have to be provided, and set to a non-NULL value;  
+ fromDate and toDate are getting swapped in case fromDate is greater/later than toDate;  
+ fromDate and toDate have to be provided in the format _*year*_ _*month*_ _*day*_ (with or without separators in between).  

*****
[ucihome]: <http://archive.ics.uci.edu/ml/>
[ucidata]: <https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip>
[ghrepo]: <https://github.com/Sil68/ExploratoryDataAnalysisCourseProject1.git>
[plot1]: <https://github.com/Sil68/ExploratoryDataAnalysisCourseProject1/blob/master/plot1.png>
[plot2]: <https://github.com/Sil68/ExploratoryDataAnalysisCourseProject1/blob/master/plot2.png>
[plot3]: <https://github.com/Sil68/ExploratoryDataAnalysisCourseProject1/blob/master/plot3.png>
[plot4]: <https://github.com/Sil68/ExploratoryDataAnalysisCourseProject1/blob/master/plot4.png>
