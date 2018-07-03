library(dplyr)

#Create Sample Data Frame with Errant Detections in all possible instances of Errant Detections

#Errant first detection
bad_first <- data.frame(rkm = c(100, 5, 4, 3, 2, 1), tag = rep('A', 6))

#Multiple Errant First Detections
bad_firsts <- data.frame(rkm = c(100, 100, 100, 5, 4, 3, 2, 1), tag = rep('B', 8))

#Errant Final Detection
bad_final <- data.frame(rkm = c(5, 4, 3, 2, 1, 100),fake_rkm = c(1,1,1,1,1,1), tag = rep('C', 6))
#If it is not converted to a matrix, then apply() will not work unless there are more than 1 rows/columns,
#depending on what you need to do.

#Multiple Errant Final Detections
bad_finals <- data.frame(rkm = c(5, 4, 3, 2, 1, 100, 100, 100), tag = rep('D', 8))

#Single Errant Middle Detection
bad_middle <- data.frame(rkm = c(5, 4, 100, 3, 2, 1), tag = rep('E', 6))

#Multiple Errant Middle Detection
bad_middles <- data.frame(rkm = c(5, 4, 100, 100, 100, 3, 2, 1), rep('F', 8))

########

#Write a Function that will determine:
#1. What type of errancy has occured
#2. Identify those errors
#3. Remove those detections from the data.frame

#I think that it is possibleto use the apply function to determine the position of all river kilometers
##I think that if you can see what rkms are on either side of a single element, if the difference in rkm
#is much larger, you could then use the position of the elements in order to remove them.

#might be able to use the position in order to go back into the original data.frame and look at the elements
#above and below the specified element.

#Practice Apply Function
matrix <- as.matrix(bad_final$rkm)
matrix

apply(matrix, 2, sum)
apply(bad_final[,1:2], 2, sum)

remove_errant_middle_detections <- function(df_detections, diff_rkm, x) { #this should act on a single row/column
  det <- df_detections #df_detections already has an index associated with it.
  det$validity <- NA #Make a new column that will have binary values, either 'V' (VALID) or 'I' (INVALID)
  df_x <- as.data.frame(x) #make a data.frame with the row and index easily accessible.
  df_x <- t(df_x)
  #global_df <<- df_x
  df_x <- data.frame(df_x)
  t_rkm <- df_x$rkm #t stands for target
  t_index <- df_x$index #t stands for target
  #print(paste('rkm:', t_rkm))
  #print(paste('index', t_index))
  #should be able to use t_rkm and t_index to get back into original data.frame
  det_position <- which(det$rkm == t_rkm & det$index == t_index)
  v_three_rows <- c(det_position-1, det_position, det_position+1) 
  df_three_rows <- det[v_three_rows, ] #Get the target row from the detection data.frame, along with the ones immediately before and after it.
  #print(df_three_rows)
  #A series of if statements should allow the user to determine whether or not a detection is valid.
  #I'm not sure how to handle this if there is more than one bad detection, like if a receiver was swapped without being properly recorded and the rkm is simply wrong.
  
  #Get the RKMs so that they can be compared in if statements
  first <- df_three_rows$rkm[1]
  mid <- df_three_rows$rkm[2]
  last <- df_three_rows$rkm[3]
  
  diff_one <- abs(mid - first) #distance between prior detection and current
  diff_two <- abs(mid - last) #distance between current detection and final detection

  #There should be about 6 scenarios that should exist. These should each identify a specified potential example.
  #If the detections meet these criteria, then it will assign a variable that indicates that the detection at that position should be removed.
  if (diff_one > diff_rkm & diff_two > diff_rkm) { 
    #This means that there is a detection at higher/lower rkm that should not be there. This detection should be marked as invalid. 
    det$validity[t_index] <- 'I'
  }
  else {  
    det$validity[t_index] <- 'V'
  }
  target_row <- det[t_index, ] #get row at position t index, with I or V.
  return(target_row)
}

#Order
#Add a vector that goes from 1:nrow(df) with the numbers 1:nrow(df). This will be used for reference.
#Need to have the ability to look at each element in the data.frame in relationship to the ones above and below it.

#df_det => establish reference => use apply function => function that I will use should
#Take the individual element and location, and use those to go back into original data.frame
#Figure out what values are above and below it
#Based on the relationship between all three of these values, do something.
  #This will be the hardest part.


execute_middle_detection_removal <- function(df_det, rkm_threshold) {
   df <- df_det
   df$index <- c(1:nrow(df)) #now have a data.frame with rkm, tag, and index
   df_matrix <- select(df, rkm, index) #select elements that will eventually use apply()
   matrix <- as.matrix(df_matrix) #convert new df into a matrix
   print(matrix)
   result <- apply(matrix, 1, remove_errant_middle_detections, df_detections = df, diff_rkm = rkm_threshold)
   #print(result)
   final_result <- data.frame(matrix(unlist(result), nrow = nrow(df_det), byrow = TRUE), stringsAsFactors = TRUE)
   final_result <- select(final_result, rkm=X1, tag=X2, index=X3, validity=X4) #Names of your output data.frame columns
   #Not preserving the Letter values of the tags in the new data.frame, this would be easy to adjust. Not sure why this is happening?
   final_result <- final_result[final_result$validity == 'V', ]
   return(final_result)
}

input_function(bad_middles, 50)

#Problems to Address
#Currently will ONLY work if there is only ONE errant detection at a errant rkm.
