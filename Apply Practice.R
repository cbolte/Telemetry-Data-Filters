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

test_function <- function(input_matrix, element) {
  location <- which(input_matrix == element)
}

input_function <- function(df) {
   matrix <- as.matrix(df$rkm)
   print(matrix)
   result <- apply(matrix, 2, test_function, input_matrix = matrix)
   return(result)
}

input_function(bad_final)




