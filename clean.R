
clean <- function(){
  # print(getwd())
  if(!"database.csv" %in% list.files()) unzip("us-homicide-reports.zip")
  if(!"database.csv" %in% list.files()) stop("database.csv still not in directory")
  df <- readr::read_csv("database.csv", progress = F)
  names(df) <- gsub(" ", "", names(df))
  df <- dplyr::mutate(df,
                      VictimAge = ifelse((VictimRace == "Unknown" & VictimSex == "Unknown") | VictimAge %in% c(99, 998), NA, VictimAge),
                      victim_unknown = ifelse(is.na(VictimAge), 1, 0),
                      PerpetratorAge = ifelse(PerpetratorAge == 0, NA, PerpetratorAge),
                      perpetrator_unknown = ifelse(is.na(PerpetratorAge), 1, 0))
  return(df)
}

