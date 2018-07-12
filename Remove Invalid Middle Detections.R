#Write a script that will remove detections where there is a receiver that has multiple detections that is ALSO
#more than a specified number of rkm from the preceding and subsequent receiver detections.

#Steps for this program
#iterate through all rows
# if it gets to a point where there is a faulty receiver it should do the following:
# Determine the the start and stop position of that row
# remove those detections
# reassign an index
# start the for loop over again, but at the position of the next receiver.

#Make your test data
bad_middles1 <- data.frame(rkm = c(5, 4, 100, 100, 100, 3, 2, 1), tag = rep('A', 8))
bad_middles2 <- data.frame(rkm = c(5, 100, 100, 100, 4, 3, 2, 1), tag = rep('B', 8))
bad_middles3 <- data.frame(rkm = c(5, 4, 3, 2, 100, 100, 100, 1), tag = rep('C', 8))

all_bad_middles <- rbind(bad_middles1, bad_middles2, bad_middles3)

remove_multiple_middle_detections <- function(df_detections, rkm_threshold) {
  df <- df_detections
  all_tags <- unique(df$tag)
  clean_det <- NULL
  for (t in all_tags) {
    print(t)
    ind_tag <- subset(df, tag == t)
    #NEED TO ADD ORDER BY DETECT_DATE
    ind_tag$index <- c(1:nrow(ind_tag)) #makes reference numbers for each row that can be called on with a which function
    all_rows <- c(1:nrow(ind_tag))
    on_off <- 'OFF'
    row_remove_vector <- c()
    for (row in all_rows) {
      current_row <- row #integer value used to find position later
      next_row <- row + 1 #integer value used to find position later
      current_rkm <- ind_tag$rkm[row]
      next_rkm <- ind_tag$rkm[row + 1]
      diff_rkm <- abs(current_rkm - next_rkm)
      if (row == nrow(ind_tag)) {
        break
      }
      if (current_rkm != next_rkm & diff_rkm > rkm_threshold & on_off == 'OFF') { #Fish made a movement over the rkm threshold and should be treated as a bad detection, so long as all other conditions are met.
        rkm_position <- current_row #last row where there are valid detections
        new_rkm_position <- next_row #first row where there MAY be invalid detections 
        on_off <- 'ON'
      }
      if (current_rkm != next_rkm & diff_rkm > rkm_threshold & on_off == 'ON') { #Fish has made a movement over rkm_threshold and the detections prior to this receiver also were over the rkm threshold
        #Action should be taken to remove the detections from the data.frame
        new_rkm_position <- current_row #last position of invalid detections
        removal_rows <- which(ind_tag$index > rkm_position & ind_tag$index <= new_rkm_position)
        removal_rows <- c(removal_rows)
        row_remove_vector <- c(row_remove_vector, removal_rows)
        #on_off <- 'OFF'
      }
      if (current_rkm != next_rkm & diff_rkm < rkm_threshold & on_off == 'ON') {#this means that the potentially bad receiver does not fit the removal criteria.
        #resume the normal loop without removing anything.
        on_off <- 'OFF'
        }
      }
    ind_tag <- ind_tag[!ind_tag$index %in% row_remove_vector, ] #remove invalid detections
    ind_tag$index <- NULL
    clean_det <- rbind(clean_det, ind_tag)
    }
  return(clean_det)
}

remove_multiple_middle_detections(all_bad_middles, 50)


which(bad_middles$rkm == 100 & bad_middles$rep..F...8.== 'F')
bad_middles

