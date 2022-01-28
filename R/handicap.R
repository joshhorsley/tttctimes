


get_handicap <- function(names_full,
                         course_use = c("full","int")[1],
                         date_last_race = NULL,
                         old_season_add_mins = 3,
                         n_old_seasons_use = 3,
                         taget_time_hour = 7.5) {
  
  
  if(!(course_use %in% c("full","int"))) stop("Must set course as full or int")
  
  # Which dates to use for handicap calculation
  # date_last_race:  last date to use for handicap, assume max date with valid race result if not given
  
  # If date_last_race not given
  if(is.null(date_last_race)) {
    date_last_race <- dt_all_long[course == course_use, max(date_ymd)]
  }
  
  
  dt_all_long[, is_past := (valid_overall) & date_ymd <= date_last_race & course == course_use]
  
  season_current <- dt_all_long[date_ymd==date_last_race, unique(season)]
  
  seasons_avail <- sort(unique(dt_all_long$season))
  
  n_seasons <- length(seasons_avail)
  
  ind_season_old_use <- tail(which(seasons_avail < season_current)[-n_seasons],n_old_seasons_use)
  seasons_old_use <- seasons_avail[ind_season_old_use]

  dt_all_long[(is_past), handicap_eligible := TRUE]
  
  
  
  # Catch names without eligible results
  names_avail <- unique(dt_all_long[(handicap_eligible), .(Name)]$Name)
  ind_avail <- names_full %in% names_avail
  
 
  # stop if there are no results available
  if(!any(ind_avail)) {
    stop("No eligible race results for those names")
  }
  
  names_use <- names_full[ind_avail]
  
  
  # flag those with no previous results
  if(any(!ind_avail)) {
    
    msg <- paste0("No results for:\n\n",
                  paste0(names_full[!ind_avail],
                         collapse = "\n"))
    
    message(msg)
  }

 
  # Calculate handicaps
  
  dt_all_long[(handicap_eligible) & Name %in% names_use, do_handicap := TRUE]
  dt_best_times <- dt_all_long[(do_handicap), .(best_time_mins = min(total_overall_mins)), by = .(Name, season)]

  dt_best_times[season==season_current, best_time_mins_adjust := best_time_mins]
  dt_best_times[season!=season_current, best_time_mins_adjust := best_time_mins + old_season_add_mins]
  
  
  dt_handicap <- dt_best_times[, .(best_time_mins_adjust = min(best_time_mins_adjust)), by = .(Name)]
  dt_handicap[, start_time_mins := taget_time_hour*60 - best_time_mins_adjust]
  # dt_handicap[, start_time_hms := to_hms(seconds_to_period(start_time_mins*60))]
  dt_handicap[, start_time_hms :=seconds_to_hms(start_time_mins*60, "floor", FALSE)]
  
  
  
  return(dt_handicap[order(start_time_mins)][, .(Name, Start = start_time_hms)])
 

  
}


if(FALSE) {
  source("R/req_packages.R")
  source("R/utils.R")
  
  dt_all_long <- readRDS("data_derived/dt_all_long.rds")

  names_full <- c("Josh Horsley","Kev Bannerman", "Darren Jones","Sam Colman")
  
  
  # calculate
  dt_starts <- get_handicap(names_full)
  
  # apply requests
  dt_set <- data.table(Name = c("Josh Horsley",
                                "Kev Bannerman"),
                       Start = c("06:00:00",
                                 "06:00:00"))
  
  dt_starts[dt_set, on = .(Name), Start := i.Start]
  
  setorder(dt_starts, "Start" )
}
