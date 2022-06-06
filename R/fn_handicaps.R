get_all_handicaps <- function(dt_all_long,dt_season,
                              course_use = c("full","int")[1],
                              date_last_race = NULL,
                              n_old_seasons_use = 3,
                              old_season_add_mins = 3,
                              taget_time_hour = 7.5) {
  
  # Which dates to use for handicap calculation
  # date_last_race:  last date to use for handicap, assume max date with valid race result if not given
  
  # If date_last_race not given
  if(is.null(date_last_race)) {
    date_last_race <- dt_season[(have_results), max(date_ymd)]
  }
  
  
  
  season_current <- dt_season[date_ymd> date_last_race, min(season)]
  seasons_avail <- sort(unique(dt_all_long$season))
  
  n_seasons <- length(seasons_avail)
  seasons_old_use <- seasons_avail[tail(which(seasons_avail < season_current)[-n_seasons],n_old_seasons_use)]

  dt_all_long[, is_past := (valid_overall) & date_ymd <= date_last_race]
  dt_all_long[(is_past) & season %in% c(seasons_old_use,season_current), handicap_eligible := TRUE]
  
  # Calculate handicaps
  # dt_all_long[(handicap_eligible) & Name %in% names_use, do_handicap := TRUE]
  dt_best_times <- dt_all_long[(handicap_eligible), .(best_time_mins = min(total_overall_mins),
                                                      name_last = name_last[1],
                                                      name_first = name_first[1]),
                               by = .(Name, season, course)]
  
  dt_best_times[season==season_current, best_time_mins_adjust := best_time_mins, by = .(course)]
  dt_best_times[season!=season_current, best_time_mins_adjust := best_time_mins + old_season_add_mins, by = .(course)]
  
  
  dt_handicap <- dt_best_times[, .(best_time_mins_adjust = min(best_time_mins_adjust),
                                   name_last = name_last[1],
                                   name_first = name_first[1]),
                               by = .(Name, course)]
  dt_handicap[, start_time_mins := taget_time_hour*60 - best_time_mins_adjust]
  dt_handicap[, best_time_adjust_hms :=seconds_to_hms(best_time_mins_adjust*60, "floor", FALSE)]
  dt_handicap[, start_time_hms :=seconds_to_hms(start_time_mins*60, "floor", FALSE)]
  
  # adjust names
  # dt_handicap[course=="full", course := "Full"]
  # dt_handicap[course=="int", course := "Intermediate"]
  
  
  
  return(dt_handicap)
  
  
}



# Table -------------------------------------------------------------------


tab_handicaps <- function(dt_handicap, dt_all_long, course_j = "full") {
  
  dt_temp <- dt_handicap[course %in% course_j]
  
  
  dt_temp[nchar(best_time_adjust_hms)==8 & grepl("^0", best_time_adjust_hms), best_time_adjust_hms := substr(best_time_adjust_hms, 2, 8)]
  dt_temp[nchar(start_time_hms)==8 & grepl("^0", start_time_hms), start_time_hms := substr(start_time_hms, 2, 8)]
  
  setorder(dt_temp, name_last, name_first)
  setcolorder(dt_temp, c("name_last", "name_first", "best_time_adjust_hms", "start_time_hms"))
  
  setnames(dt_temp,
           c("name_last", "name_first", "best_time_adjust_hms", "start_time_hms"),
           c("Last Name", "First Name", "Best Time (Adjusted)", "Start Time"))
  
  col_ref_hide <- which(!(names(dt_temp) %in% c("Last Name", "First Name", "Best Time (Adjusted)", "Start Time") ))-1 # columns are indexed from 0 - row name?
  
  
  tab_out <- datatable_std(dt_temp, col_ref_hide, escape=FALSE, filter = "top")
  
  return(tab_out)
}
