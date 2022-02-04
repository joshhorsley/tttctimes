get_all_handicaps <- function(dt_all_long,
                              course_use = c("full","int")[1],
                              date_last_race = NULL,
                              n_old_seasons_use = 3,
                              old_season_add_mins = 3,
                              taget_time_hour = 7.5) {
  
  # Which dates to use for handicap calculation
  # date_last_race:  last date to use for handicap, assume max date with valid race result if not given
  
  # If date_last_race not given
  if(is.null(date_last_race)) {
    date_last_race <- dt_all_long[,max(date_ymd)]
  }
  
  
  dt_all_long[, is_past := (valid_overall) & date_ymd <= date_last_race]
  
  season_current <- dt_all_long[date_ymd==date_last_race, unique(season)]
  
  seasons_avail <- sort(unique(dt_all_long$season))
  
  n_seasons <- length(seasons_avail)
  
  ind_season_old_use <- tail(which(seasons_avail < season_current)[-n_seasons],n_old_seasons_use)
  seasons_old_use <- seasons_avail[ind_season_old_use]
  
  dt_all_long[(is_past), handicap_eligible := TRUE]
  
  # Calculate handicaps
  # dt_all_long[(handicap_eligible) & Name %in% names_use, do_handicap := TRUE]
  dt_best_times <- dt_all_long[(handicap_eligible), .(best_time_mins = min(total_overall_mins)), by = .(Name, season, course)]
  
  dt_best_times[season==season_current, best_time_mins_adjust := best_time_mins, by = .(course)]
  dt_best_times[season!=season_current, best_time_mins_adjust := best_time_mins + old_season_add_mins, by = .(course)]
  
  
  dt_handicap <- dt_best_times[, .(best_time_mins_adjust = min(best_time_mins_adjust)), by = .(Name, course)]
  dt_handicap[, start_time_mins := taget_time_hour*60 - best_time_mins_adjust]
  # dt_handicap[, start_time_hms := to_hms(seconds_to_period(start_time_mins*60))]
  dt_handicap[, start_time_hms :=seconds_to_hms(start_time_mins*60, "floor", FALSE)]
  
  # adjust names
  dt_handicap[course=="full", course := "Full"]
  dt_handicap[course=="int", course := "Intermediate"]
  
  
  
  return(dt_handicap)
  
  
}