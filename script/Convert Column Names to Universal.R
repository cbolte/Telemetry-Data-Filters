#Write code that will do the following:
  #Sort by Tag
  #Arrange detections in chronological order
  #Find detections that make a movement over 100 river kilometers, then remove those detections
  #Output a new data.frame with edited information.

#Import your dataset, convert from vemco/jsats to universal column names. Need the following pieces of information.
  #Detection Datetime
  #Tag ID
  #Location Name
  #River Kilometer

#import jsats and vemco test data sets
#library(dplyr)
#library(ggplot2)
#library(readr)

library(tidyverse) # tidyverse includes all three of the above packages
library(readxl) # in case the data comes from an excel file, 'read_excel()' can parse data formats (ie times)
library(here) # useful package for filing and recalling files

jsats_det <- read_csv("C:/Users/cbolte/Desktop/R Projects/Telemetry-Data-Filters/CSVs/jsats data.csv")
vemco_det <- read_csv("C:/Users/cbolte/Desktop/R Projects/Telemetry-Data-Filters/CSVs/vemco data.csv")


convert_colnames <- function(df) {   #Depending on where you data.frame came from, get the important column names and
  #convert them into a universal format
  det <- df
  cat('\n', 'JSATS or VEMCO?', '\n\n')
  a <- scan(what='character', n=1)
  if (a == 'JSATS') { #select(dataframe, new_colname = old_colname)
    print('Selecting JSATS Values')
    det <- select(det, detectdate=dtf, tagid=Hex, rkm=rkm, location=`GPS Names`)
  }
  if (a == 'VEMCO') {
    print('Selecting Vemco Values')
    det <- select(det, detectdate=DetectDate, tagid=TagID, rkm=Detect_rkm, location=Location)
  }
  if (a != 'JSATS' & a != 'VEMCO') {
    print('Input Not Recognized')
  }
  return(det)
}



########################################################################


