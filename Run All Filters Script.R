#Write a function that will run the data.frames through each filter.

bad_multi <- data.frame(rkm=c(100, 1, 2, 3, 100, 4, 5, 100), detectdate=c(1:8))


filter_telemetry_data <- function(df_detections, threshold_rkm) {
  df <- df_detections
  print(df)
  print('Removing Errant Initial Detections')
  df <- remove_bad_initial_detections(df, threshold_rkm)
  print(df)
  print('Removing Errant Final Detections')
  df <- remove_bad_final_detections(df, threshold_rkm)
  print(df)
  print('Removing Errant Middle Detections')
  df <- execute_middle_detection_removal(df, threshold_rkm)
  return(df)
}

filter_telemetry_data(bad_multi, 50)

