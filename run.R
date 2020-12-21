

# Packages ----------------------------------------------------------------


source("req_packages.R")


# Races and files ---------------------------------------------------------


dt_season_import <- fread("data_provided/season_definition/2020-2021.csv")
dt_season_import[, date_ymd := as.IDate(date_ymd, format = "%d/%m/%y")]

n_weeks <- nrow(dt_season_import)

dt_season <- dt_season_import[rep(seq(.N),2)]

dt_season[, course := "int"]
dt_season[1:n_weeks, course := "full"]

# Add double-distance
dt_double <- dt_season[race_type=="Double Distance" & course == "int"]
dt_double[, course := "double"]
dt_season <- rbind(dt_season, dt_double)

dt_season[course=="full", course_nice := "Full"]
dt_season[course=="int", course_nice := "Intermediate"]
dt_season[course=="double", course_nice := "Double Distance"]

# load data
dt_season[race_type=="Double Distance" & course == "double", path_webscorer := paste0("data_provided/webscorer/",course,"/Double Distance ", mday(date_ymd), " ", format(date_ymd, "%b"), " ", substring(year(date_ymd), 1,2), ".txt")]
dt_season[!(race_type=="Double Distance" & course == "double"), path_webscorer := paste0("data_provided/webscorer/",course,"/TTTC ", mday(date_ymd), " ", format(date_ymd, "%b"), " ", year(date_ymd), ".txt")]
dt_season[, path_webscorer := gsub("Sep","Sept",path_webscorer)]
dt_season[, have_results := file.exists(path_webscorer)]

dt_season[, row_id := seq(.N)]

# Load race results
dt_season[(have_results), dt_race := .(list(fread(path_webscorer))), by = row_id]

# Flatten available results
id_results <- which(dt_season$have_results)

# dt_all <- foreach(i=id_results, .combine = rbind) %do% {
dt_all <- foreach(i=id_results, .combine = function(x,y) rbind(x,y,fill=TRUE)) %do% {
  dt_season[i, .(row_id, dt_race[[1]])]
}

new_cols <- c("race_number", "date_ymd", "race_type", "course")
dt_all[dt_season, on = .(row_id), (new_cols) := mget(new_cols)]
setcolorder(dt_all, new_cols)


# Cleaning ----------------------------------------------------------------

# Formats
dt_all[, place_overall := as.numeric(Place)]

dt_all[, Swim := as.ITime(`Lap.1`, format = "%M:%S")]
dt_all[, Ride := as.ITime(`Lap.2`, format = "%M:%S")]
dt_all[, Run := as.ITime(`Lap.3`, format = "%M:%S")]
dt_all[, Swim_total := Swim]
dt_all[, Ride_total := Swim + Ride]
dt_all[, Run_total := Swim + Ride + Run]
dt_all[, total_overall := Run_total]

dt_all[, started := TRUE]
dt_all[`Lap.1` %in% c("","-"), started := FALSE]


# Convert to long
dt_all_long <- melt.data.table(dt_all,
                               id.vars = c("race_number", "date_ymd", "race_type", "course",
                                           "Name","place_overall","total_overall","started"),
                               measure.vars = list(c("Swim",
                                                  "Ride",
                                                  "Run"),
                                                  c("Swim_total",
                                                    "Ride_total",
                                                    "Run_total")),
                                 variable.name = c("part"),
                               value.name = c("duration","total"))

dt_all_long[part=="1", part := "Swim"]
dt_all_long[part=="2", part := "Ride"]
dt_all_long[part=="3", part := "Run"]
dt_all_long[, part := as.character(part)]

dt_all_long[, duration_mins := as.numeric(duration/60)]
dt_all_long[, total_mins := as.numeric(total/60)]


# Timing errors -----------------------------------------------------------


dt_problems <- fread("data_provided/webscorer/webscorer_problems.csv")
dt_problems[, date_ymd := as.IDate(date_ymd, format = "%d/%m/%y")]

dt_all_long[dt_problems, on = .(race_number, date_ymd, Name, part), `:=`(split_valid = i.split_valid, cumulative_valid = i.cumulative_valid)]
dt_all_long[is.na(split_valid), split_valid := TRUE]
dt_all_long[is.na(cumulative_valid), cumulative_valid := TRUE]


# overall_invalid if cumulative time at end of run is invalid
dt_all_long[, valid_overall := cumulative_valid[which(part=="Run")], by = .(race_number, Name)]


dt_all_long[is.na(valid_overall), valid_overall := TRUE]

# dt_all_long[Name=="Rowena Smith" & race_number ==4]
# dt_problems[Name=="Rowena Smith" & race_number ==4]
# dt_problems[Name=="Rowena Smith" & race_number ==4]


# Entry errors ------------------------------------------------------------


dt_all_long[race_number==3 & Name == "Lydia Kuschmirz", course := "int"]


# Recalculate places ------------------------------------------------------


dt_all_long[(split_valid), duration_mins_sort := duration_mins]
dt_all_long[!(split_valid), duration_mins_sort := NA]

dt_all_long[(cumulative_valid), total_mins_sort := total_mins]
dt_all_long[!(cumulative_valid), total_mins_sort := NA]

dt_all_long[(valid_overall), total_overall_sort := total_overall]
dt_all_long[!(valid_overall), total_overall_sort := NA]

# dt_all_long[race_number==4 & course=="double" & Name =="Ben Hall"]
# dt_all_long[race_number==4 & course=="double" & Name =="Rowena Smith"]


# dt_all_long[!(data_problem_part) & (started), place_lap := as.integer(rank(duration_mins, ties.method = "first")), by = .(race_number, course, part)]
dt_all_long[(started), place_lap := as.integer(rank(duration_mins_sort, ties.method = "first")), by = .(race_number, course, part)]
dt_all_long[(started), place_cum_recalc := as.integer(rank(total_mins_sort, ties.method = "first")), by = .(race_number, course, part)]
dt_all_long[(started), place_overall_recalc := as.integer(rank(total_overall_sort, ties.method = "first")), by = .(race_number, course, part)]

dt_all_long[(started) & (split_valid), place_lap_display := place_lap]
dt_all_long[(started) & !(split_valid), place_lap_display := NA]

# dt_all_long[!(split_valid), .(place_lap, place_lap_display, place_lap_nice)]


dt_all_long[(started),                               place_lap_nice := paste0(place_lap_display, "th")]
dt_all_long[(started) & place_lap_display %% 10 ==1, place_lap_nice := paste0(place_lap_display, "st")]
dt_all_long[(started) & place_lap_display %% 10 ==2, place_lap_nice := paste0(place_lap_display, "nd")]
dt_all_long[(started) & place_lap_display %% 10 ==3, place_lap_nice := paste0(place_lap_display, "rd")]
dt_all_long[(started) & place_lap_display %in% c(11,12,13), place_lap_nice := paste0(place_lap_display, "th")]
dt_all_long[(started) & is.na(place_lap_display), place_lap_nice := "NA"]

dt_all_long[(started),                              place_cum_nice := paste0(place_cum_recalc, "th")]
dt_all_long[(started) & place_cum_recalc %% 10 ==1, place_cum_nice := paste0(place_cum_recalc, "st")]
dt_all_long[(started) & place_cum_recalc %% 10 ==2, place_cum_nice := paste0(place_cum_recalc, "nd")]
dt_all_long[(started) & place_cum_recalc %% 10 ==3, place_cum_nice := paste0(place_cum_recalc, "rd")]
dt_all_long[(started) & place_cum_recalc %in% c(11,12,13), place_cum_nice := paste0(place_cum_recalc, "th")]


# Race plots --------------------------------------------------------------
# i=4;j="double"
# i=10;j="full"

race_numbers <- unique(dt_all_long$race_number)


list_plotly_race <- list()

for(i in race_numbers) {
  # list_plotly_race[[paste0("race_",as.character(i))]] <- list()
  # for( j in c("full", "int"))
  
  i_courses <- dt_season[race_number==i]$course
  for( j in i_courses) {
    
    
  dt_i <- dt_all_long[race_number== i & course == j][order(place_overall)][(started)]
  
  dt_i[!(split_valid), part := paste0(part, " (invalid)")]

  dt_i[(valid_overall), place_name := paste0(place_overall_recalc, " ", Name)]
  dt_i[!(valid_overall), place_name := paste0("DNF ", Name)]
  
  dt_i[, tooltext := paste0(Name, "\n",
                            part, ": ", duration,ifelse(!(split_valid),"",{paste0(" (",place_lap_nice,")")}),
                            "\nCumulative", ifelse(!(cumulative_valid), paste0(" (invalid): ", total), paste0( ": ",total," (", place_cum_nice,")")))]
  
  g <- ggplot(dt_i,
              # aes(x = duration_mins, y = - place_overall, fill = part, group = Name, text = tooltext)) +
              aes(x = duration_mins, y = - place_overall_recalc, fill = part, group = Name, text = tooltext)) +
    geom_col(orientation = "y") +
    # geom_col(aes(x = swim_duration/60 + ride_duration/60, y = Name),width = 2, orientation = "y", fill = "#3B8544") +
    # geom_col(aes(x = swim_duration/60, y = Name),width = 2, orientation = "y", fill = "#2E63BC")  +
    scale_fill_manual("Part",
                      labels = c(Swim = "Swim",
                                 Ride = "Ride",
                                 Run = "Run",
                                 `Swim (invalid)` = "Swim (invalid)",
                                 `Ride (invalid)` = "Ride (invalid)",
                                 `Run (invalid)` = "Run (invalid)"),
                      values = c(Swim = "#2E63BC",
                                 Ride = "#3B8544",
                                 Run = "#BF5324",
                                 `Swim (invalid)` = "grey",
                                 `Ride (invalid)` = "black",
                                 `Run (invalid)` = "grey")) +
    scale_x_continuous("Time (mins)", breaks = seq(0,150, 10), minor_breaks = seq(0,150, 5),position = "top") +
    scale_y_continuous("Athlete", breaks = -dt_i$place_overall_recalc,
                       labels = dt_i$place_name, minor_breaks = NULL,
                       limits = range(c(0,min(-dt_i$place_overall_recalc)-1))) +
    # scale_alpha_discrete("Valid Split", range = c(0.5, 1)) +
    theme_minimal() +
    theme(legend.position="top")
  
  ggsave(filename = paste0("figures/",dt_i$date_ymd[1],"_",j,".pdf"),
         plot = g, width = 6, height = 7)
  
  
    
  list_plotly_race[[paste0("race_",as.character(i))]][[j]] <- ggplotly(g,width = 800, height = 700, tooltip = "text",layerData = TRUE, style = "mobile")
  }
}

# test <- foreach(i=race_numbers, .combine = list) %do% {
#   foreach(j = c("full","int"), .combine = list, .final = function(x) setNames(x, c("full","int"))) %do% {
#     paste0(i, j)
#   }
# }
# 
# list_plotly_race <- 
#   # foreach(i=race_numbers, .combine = list, .final = function(x) setNames(x, paste0("race_",race_numbers))) %do% {
#   foreach(i=c(1,10,11,2)) %do% {
#     foreach(j = c("full","int"), .combine = list, .final = function(x) setNames(x, c("full","int"))) %do% {
# 
#       dt_i <- dt_all_long[race_number== i & course == j][order(place_overall)][!(data_problem)]
#       dt_i[, place_name := paste0(place_overall, " ", Name)]
#       dt_i[, tooltext := paste0(Name, "\n", part, ": ", duration)]
#       
#       g <- ggplot(dt_i,
#                   aes(x = duration_mins, y = - place_overall, fill = part, group = Name, text = tooltext)) +
#         geom_col(orientation = "y") +
#         # geom_col(aes(x = swim_duration/60 + ride_duration/60, y = Name),width = 2, orientation = "y", fill = "#3B8544") +
#         # geom_col(aes(x = swim_duration/60, y = Name),width = 2, orientation = "y", fill = "#2E63BC")  +
#         scale_fill_manual("Part",
#                           labels = c(Swim = "Swim",
#                                      Ride = "Ride",
#                                      Run = "Run"),
#                           values = c(Swim = "#2E63BC",
#                                      Ride = "#3B8544",
#                                      Run = "#BF5324")) +
#         scale_x_continuous("Time (mins)", breaks = seq(0,100, 10), minor_breaks = seq(0,100, 5),position = "top") +
#         scale_y_continuous("Athlete", breaks = -dt_i$place_overall,
#                            labels = dt_i$place_name, minor_breaks = NULL) +
#         theme_minimal() +
#         theme(legend.position="top")
#       
#       ggsave(filename = paste0("figures/",dt_i$date_ymd[1],"_",j,".pdf"),
#              plot = g, width = 6, height = 7)
#       
#       ggplotly(g,width = 800, height = 700, tooltip = "text",layerData = TRUE, style = "mobile")
#       
#     
#   }
# }


# Export for website ------------------------------------------------------


saveRDS(dt_season, "data_derived/dt_season.rds")
saveRDS(dt_all_long, "data_derived/dt_all_long.rds")
saveRDS(list_plotly_race, "data_derived/list_plotly_race.rds")


# Update website ----------------------------------------------------------


source("make_race_rmd.R")
bookdown::render_book("index.Rmd")
