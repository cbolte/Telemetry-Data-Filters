#Write a function that, if the initial rkm is wrong, will be detected whether it is one or multiple detections.

#Errant first detection
bad_first <- data.frame(rkm = c(100, 5, 4, 3, 2, 1), tag = rep('A', 6))

#Multiple Errant First Detections
bad_firsts <- data.frame(rkm = c(100, 100, 100, 5, 4, 3, 2, 1), tag = rep('B', 8))

remove_bad_initial_detections <- function(df_detections, rkm_threshold) {
  df <- df_detections
  df <- df[order(df$rkm, decreasing = FALSE), ] #this will be substituted for the detectdate option, arranged from earliest to latest detection.
  all_rows <- 2:nrow(df)
  init_rkm <- df$rkm[1] #change rkm to detectdate
  for (row in all_rows) {
    new_rkm <- df$rkm[row] #change rkm to detectdate. 
    diff_rkm <- abs(init_rkm - new_rkm)
    if (diff_rkm > 0 & diff_rkm < rkm_threshold) {#rkm changed, but not more than the threshold. Keep detections.
      break  
    }
    if (diff_rkm >= rkm_threshold) { #remove detections from 1: row-1, since row is where it changed.
      df <- df[row:nrow(df), ]
      break
    }
  }
  return(df)
}

remove_bad_initial_detections(bad_firsts, 50)

remove_bad_final_detections <- function(df_detections, rkm_threshold) {
  df <- df_detections
  df <- df[order(df$rkm, decreasing = TRUE), ] #this will be substituted for the detectdate option, arranged from earliest to latest detection.
  #Decreasing = TRUE means that it will iterate through the last detections first
  all_rows <- 2:nrow(df)
  init_rkm <- df$rkm[1] #change rkm to detectdate
  for (row in all_rows) {
    new_rkm <- df$rkm[row] #change rkm to detectdate. 
    diff_rkm <- abs(init_rkm - new_rkm)
    if (diff_rkm > 0 & diff_rkm < rkm_threshold) {#rkm changed, but not more than the threshold. Keep detections.
      break  
    }
    if (diff_rkm >= rkm_threshold) { #remove detections from 1: row-1, since row is where it changed.
      df <- df[row:nrow(df), ]
      break
    }
  }
  df <- df[order(df$rkm, decreasing=TRUE), ] #change
  return(df)
}


remove_bad_final_detections(bad_finals, 50)
