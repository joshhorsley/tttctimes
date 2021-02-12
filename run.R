

# Packages ----------------------------------------------------------------


source("R/req_packages.R")


# Utilities ---------------------------------------------------------------


source("R/utils.R")


# Colours -----------------------------------------------------------------


tri_cols <- list(pb = "pink",
                 record = "gold",
                 invalid = "grey",
                 swim = "#2E63BC",
                 ride = "#3B8544",
                 run = "#BF5324")


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

dt_season[, missing_results := !cancelled & !have_results]

dt_season[, row_id := seq(.N)]

# Load race results
dt_season[(have_results), dt_race := .(list(fread(path_webscorer))), by = row_id]

# Flatten available results
id_results <- which(dt_season$have_results)

dt_all <- foreach(i=id_results, .combine = function(x,y) rbind(x,y,fill=TRUE)) %do% {
  dt_season[i, .(row_id, dt_race[[1]])]
}

new_cols <- c("race_number", "date_ymd", "race_type", "course")
dt_all[dt_season, on = .(row_id), (new_cols) := mget(new_cols)]
setcolorder(dt_all, new_cols)

setnames(dt_all, c("Lap.1","Lap.2","Lap.3"), c("Swim", "Ride", "Run"))


# Cleaning ----------------------------------------------------------------


# Formats
dt_all[, place_import := as.numeric(Place)]
dt_all[, time_overall_import := Time]

dt_all[, started := TRUE]
dt_all[Swim %in% c("","-"), started := FALSE]


# Convert to long
dt_all_long <- melt.data.table(dt_all,
                               id.vars = c("race_number", "date_ymd", "race_type", "course",
                                           "Name","place_import","time_overall_import","started"),
                               measure.vars = c("Swim",
                                                  "Ride",
                                                  "Run"),
                                 variable.name = c("part"),
                               value.name = c("duration_import"))


# Name inconsistencies ----------------------------------------------------


dt_all_long[Name %in% c("Dave de Closey","Dave De Closey","Dave DE CLOSEY","David De Closey"), Name := "David De Closey"]
dt_all_long[Name %in% c("Peta EDGE"), Name := "Peta Edge"]
dt_all_long[Name %in% c("Jo Ward"), Name := "Jolyon Ward"]
dt_all_long[Name %in% c("Joshua Horsley"), Name := "Josh Horsley"]
dt_all_long[Name %in% c("Robyn BARRY"), Name := "Robyn Barry"]
dt_all_long[Name %in% c("Ian Driffil"), Name := "Ian Driffill"]
dt_all_long[Name %in% c("Stephen RING"), Name := "Stephen Ring"]
dt_all_long[Name %in% c("Philip SALTER"), Name := "Philip Salter"]
dt_all_long[Name %in% c("Sally KINGSTON"), Name := "Sally Kingston"]
dt_all_long[Name %in% c("Aaron NEYLAN"), Name := "Aaron Neylan"]
dt_all_long[Name %in% c("Greg Freeman"), Name := "Gregory Freeman"]
dt_all_long[Name %in% c("Lydia Kuschnirz"), Name := "Lydia Kuschmirz"]
dt_all_long[Name %in% c("Samatha Leonard"), Name := "Samantha Leonard"]
dt_all_long[Name %in% c("Valerie Lambard"), Name := "Val Lambard"]
dt_all_long[Name %in% c("Amanda Hall"), Name := "Manda Hall"]
dt_all_long[Name %in% c("Virginia Jones"), Name := "Ginny Jones"]
dt_all_long[Name %in% c("Melanie DUFF"), Name := "Melanie Duff"]
dt_all_long[Name %in% c("Jo COLJA"), Name := "Joanne Colja"]
dt_all_long[Name %in% c("ZOE TAYLOR-WEST"), Name := "Zoe Taylor-West"]


# Separate Names ----------------------------------------------------------


dt_all_long[, row_id := seq(.N)]
dt_all_long[, name_first := strsplit(Name, " ")[[1]][1],  by = row_id]

dt_all_long[, name_last := gsub(paste0(name_first, " "), "", Name), by = row_id]
dt_all_long[name_first == Name, name_last := ""] # Catch any people with only a first name


# Timing errors -----------------------------------------------------------


dt_problems <- fread("data_provided/webscorer/webscorer_problems.csv")
dt_problems[, date_ymd := as.IDate(date_ymd, format = "%d/%m/%y")]

dt_all_long[dt_problems, on = .(race_number, date_ymd, Name, part), `:=`(split_valid = i.split_valid, cumulative_valid = i.cumulative_valid)]
dt_all_long[is.na(split_valid), split_valid := TRUE]
dt_all_long[is.na(cumulative_valid), cumulative_valid := TRUE]


# overall_invalid if cumulative time at end of run is invalid
dt_all_long[, valid_overall := cumulative_valid[which(part=="Run")], by = .(race_number, Name)]
dt_all_long[is.na(valid_overall), valid_overall := TRUE]


# Cleaning ----------------------------------------------------------------


# Set row order
dt_all_long[, part := ordered(as.character(part), levels = c("Swim","Ride","Run"))]
setorder(dt_all_long, Name, race_number, part)


# Format time
dt_all_long[, n_char_duration := nchar(duration_import)]
dt_all_long[n_char_duration==6, duration_hms := paste0("0:0", duration_import)]
dt_all_long[n_char_duration==7, duration_hms := paste0("0:", duration_import)]
dt_all_long[n_char_duration==9, duration_hms := duration_import]

dt_all_long[, n_char_overall := nchar(time_overall_import)]
dt_all_long[n_char_overall==6, overall_hms := paste0("0:0", time_overall_import)]
dt_all_long[n_char_overall==7, overall_hms := paste0("0:", time_overall_import)]
dt_all_long[n_char_overall==9, overall_hms := time_overall_import]

dt_all_long[, duration_seconds := as.numeric(seconds(hms(duration_hms)))]
dt_all_long[is.na(duration_seconds) & (started), duration_seconds := 0]

dt_all_long[, cumulative_seconds := cumsum(duration_seconds), by = .(Name, race_number)]
dt_all_long[, overall_seconds := as.numeric(seconds(hms(overall_hms)))]

# Check for any large disagreement between cumulative run and recorded total and resolve
dt_all_long[part == "Run" & abs(cumulative_seconds - overall_seconds) > 1, total_disagree := TRUE]
dt_all_long[,total_disagree := any(total_disagree,na.rm = TRUE),by = .(Name, race_number)]

dt_all_long[(total_disagree) & part=="Run" & (cumulative_valid), total_resolve := "cumulative"]
dt_all_long[(total_disagree) & part=="Run" & !(cumulative_valid), total_resolve := "imported"]
dt_all_long[(total_disagree),total_resolve := total_resolve[which(part=="Run")],by = .(Name, race_number)]

dt_all_long[(total_disagree) & total_resolve =="cumulative",
            `:=`(total_resolved = TRUE,
                 overall_seconds = cumulative_seconds[which(part=="Run")]), by = .(Name, race_number)]

dt_all_long[(total_disagree) & total_resolve =="imported" & part=="Run", cumulative_seconds := overall_seconds]
dt_all_long[(total_disagree) & total_resolve =="imported", total_resolved := TRUE]


n_total_resolve_errors <- nrow(dt_all_long[(total_disagree) & !(total_resolved)])
stopifnot(n_total_resolve_errors==0)


# Catch cases where overall time is invalid but a run time has been recorded
dt_all_long[(started) & is.na(overall_seconds), overall_seconds := max(cumulative_seconds), by = .(Name, race_number)]


dt_all_long[, duration_mins := duration_seconds/60]
dt_all_long[, cumulative_mins := cumulative_seconds/60]
dt_all_long[, total_overall_mins := overall_seconds/60]


to_hms <- function(x, nsmall_seconds = 1L) {
  paste0(hour(x), ":",
         format(minute(x), width=2),
         ":",
         format(round(second(x),nsmall_seconds),nsmall = nsmall_seconds, width=4)
         )
}

dt_all_long[, duration_hms_short := to_hms(seconds_to_period(duration_seconds))]
dt_all_long[, duration_hms_short := gsub("^0:","",duration_hms_short)]
dt_all_long[, duration_hms_short := gsub(": ",":0",duration_hms_short)]
dt_all_long[, duration_hms_short := trimws(duration_hms_short)]

dt_all_long[, cumulative_hms_short := to_hms(seconds_to_period(cumulative_seconds))]
dt_all_long[, cumulative_hms_short := gsub("^0:","",cumulative_hms_short)]
dt_all_long[, cumulative_hms_short := gsub(": ",":0",cumulative_hms_short)]

dt_all_long[, total_overall_hms := to_hms(seconds_to_period(overall_seconds))]
dt_all_long[, total_overall_hms := gsub(": ",":0",total_overall_hms)]
dt_all_long[, total_overall_hms_short := gsub("^0:","",total_overall_hms)]


# Use recorded overall time for run cumulative
dt_all_long[(started) & part == "Run", cumulative_seconds := overall_seconds]


# Course entry errors -----------------------------------------------------


dt_all_long[race_number==3 & Name == "Lydia Kuschmirz", course := "int"]
dt_all_long[race_number==21 & Name == "Colin Woodward", course := "int"]


# Order names -------------------------------------------------------------


athletes_ordered <- unique(dt_all_long[(started)][order(tolower(name_last))]$Name)


# Recalculate places ------------------------------------------------------


dt_all_long[(split_valid), duration_mins_sort := duration_mins]
dt_all_long[!(split_valid), duration_mins_sort := NA]

dt_all_long[(cumulative_valid), total_mins_sort := cumulative_mins]
dt_all_long[!(cumulative_valid), total_mins_sort := NA]

dt_all_long[(valid_overall), total_overall_sort := total_overall_mins]
dt_all_long[!(valid_overall), total_overall_sort := NA]


dt_all_long[(started), place_lap := as.integer(rank(duration_mins_sort, ties.method = "first")), by = .(race_number, course, part)]
dt_all_long[(started), place_cum_recalc := as.integer(rank(total_mins_sort, ties.method = "first")), by = .(race_number, course, part)]
dt_all_long[(started), place_overall_recalc := as.integer(rank(total_overall_sort, ties.method = "first")), by = .(race_number, course, part)]

dt_all_long[(started), athlete_rank_split := as.integer(rank(duration_mins_sort, ties.method = "first")), by = .(Name, course, part)]
dt_all_long[(started), athlete_rank_cumulative := as.integer(rank(total_mins_sort, ties.method = "first")), by = .(Name, course, part)]
dt_all_long[(started), athlete_rank_overall := as.integer(rank(total_overall_sort, ties.method = "first")), by = .(Name, course, part)]

dt_all_long[(started) & (split_valid), place_lap_display := place_lap]
dt_all_long[(started) & !(split_valid), place_lap_display := NA]


dt_all_long[(started), place_lap_nice := format_place(place_lap_display)]
dt_all_long[(started), place_cum_nice := format_place(place_cum_recalc)]


# PBs ---------------------------------------------------------------------


dt_all_long[(started), isFirstRace := race_number == min(race_number), by = .(Name, part, course) ]


# PB change over seasons
dt_all_long[(started) & (split_valid), pb_split_running := cummin(duration_mins), by = .(Name, part, course) ]
dt_all_long[(started) & (split_valid), isNewPB_split := duration_mins == pb_split_running, by = .(Name, part, course) ]

dt_all_long[is.na(isNewPB_split), isNewPB_split := FALSE]
dt_all_long[(isFirstRace), isNewPB_split := FALSE]

dt_all_long[(started) & (cumulative_valid), pb_cum_running := cummin(cumulative_mins), by = .(Name, part, course) ]
dt_all_long[(started) & (cumulative_valid), isNewPB_cum := cumulative_mins == pb_cum_running, by = .(Name, part, course) ]

dt_all_long[is.na(isNewPB_cum), isNewPB_cum := FALSE]
dt_all_long[(isFirstRace), isNewPB_cum := FALSE]

dt_all_long[(started) & (valid_overall), pb_overall_running := cummin(total_overall_mins), by = .(Name, course) ]
dt_all_long[(started) & (valid_overall), isNewPB_overall := total_overall_mins == pb_overall_running, by = .(Name, course) ]

dt_all_long[is.na(isNewPB_overall), isNewPB_overall := FALSE]
dt_all_long[(isFirstRace), isNewPB_overall := FALSE]

# season pB
dt_all_long[(started) & (valid_overall), pb_overall := min(total_overall_mins), by = .(Name, course) ]
dt_all_long[(started) & (valid_overall), isPB_overall := total_overall_mins == pb_overall, by = .(Name, course) ]
dt_all_long[is.na(isPB_overall), isPB_overall := FALSE]

dt_all_long[(started) & (split_valid), pb_split := min(duration_mins), by = .(Name, part, course) ]
dt_all_long[(started) & (split_valid), isPB_split := duration_mins == pb_split, by = .(Name, part, course) ]
dt_all_long[is.na(isPB_split), isPB_split := FALSE]

dt_all_long[(started) & (cumulative_valid), pb_cumulative := min(cumulative_mins), by = .(Name, part, course) ]
dt_all_long[(started) & (cumulative_valid), isPB_cumulative := cumulative_mins == pb_cumulative, by = .(Name, part, course) ]
dt_all_long[is.na(isPB_cumulative), isPB_cumulative := FALSE]


# Records -----------------------------------------------------------------


dt_all_long[(started) & (isPB_overall), rank_pb_overall := rank(total_overall_mins, ties.method = "first"), by = .(course, part)]
dt_all_long[(started) & (isPB_split), rank_pb_split := rank(duration_mins, ties.method = "first"), by = .(course, part)]


# Part naming -------------------------------------------------------------


part_levels <- c("Swim",
                 "Ride",
                 "Run",
                 "Swim (PB)",
                 "Ride (PB)",
                 "Run (PB)",
                 "Swim (record)",
                 "Ride (record)",
                 "Run (record)",
                 "Swim (invalid)",
                 "Ride (invalid)",
                 "Run (invalid)")


dt_all_long[, part_plot := part]
dt_all_long[!(split_valid), part_plot := paste0(part, " (invalid)")]
dt_all_long[rank_pb_split==1, part_plot := paste0(part, " (record)")]

dt_all_long[, part_plot_pb := part_plot]
dt_all_long[(isPB_split) & rank_pb_split!=1, part_plot_pb := paste0(part, " (PB)")]
dt_all_long[, part_plot := ordered(part_plot, levels = part_levels)]
dt_all_long[, part_plot_pb := ordered(part_plot_pb, levels = part_levels)]


# Plot prep ---------------------------------------------------------------


set.seed(100)
setWidgetIdSeed(100)
#' Plotly sets ids via plotly::new_id()
#' which calls base::tempfile()
#' which doesn't seem reproducible - annoying

site_path_relative <- "docs"
if(!dir.exists(site_path_relative)) dir.create(site_path_relative)
libpath <- file.path(getwd(), "docs/libs")


# Race plots --------------------------------------------------------------


race_numbers <- sort(unique(dt_all_long$race_number))

for(i in race_numbers) {

  i_courses <- dt_season[race_number==i]$course
  
  for( j in i_courses) {
    
  dt_i <- dt_all_long[race_number== i & course == j][(started)]

  dt_i[(valid_overall), place_name := paste0(place_overall_recalc," ", Name)]
  dt_i[(valid_overall) & (isNewPB_overall), place_name := paste0(place_overall_recalc, " ", Name, " (New PB!)")]
  dt_i[(valid_overall) & (isPB_overall), place_name := paste0(place_overall_recalc, " ", Name, " (Season PB!)")]
  dt_i[(isPB_overall) & rank_pb_overall==1, place_name := paste0(place_overall_recalc, " ", Name, " (Season Record!)")]
  dt_i[!(valid_overall), place_name := paste0("TBC ", Name)]
  
  dt_i[, tooltext := paste0(Name,
                            ifelse((isFirstRace) & i !=1, " - First race this season",""),"\n",
                            
                            part_plot,
                            ifelse(split_valid & !(cumulative_valid)," (valid)",""),
                            ": ", 
                            duration_hms_short,
                            ifelse((split_valid),paste0(" (",place_lap_nice,")"),""),
                            ifelse((isNewPB_split) & !(isPB_split)," New PB!",""),
                            ifelse((isPB_split),ifelse(rank_pb_split==1," Season record!"," Season PB!"),""),
                            # ifelse(ifelse(!is.na(rank_pb_split),rank_pb_split==1, FALSE)," Season record!",""),
                            "\n",
                            
                            "Cumulative",
                            ifelse((cumulative_valid), "", " (invalid)"),
                            ifelse((cumulative_valid) & !(split_valid), " (valid)",""),
                            ": ",
                            cumulative_hms_short,
                            ifelse((cumulative_valid),paste0(" (", place_cum_nice,")"),""),
                            ifelse((isNewPB_cum)," New PB!",""))]
  
  g <- ggplot(dt_i,
              aes(x = duration_mins, y = - place_overall_recalc, fill = part_plot, col = part_plot, group = Name, text = tooltext)) +
    geom_col(orientation = "y", width = 0.9, size = 0.3) +
    scale_x_continuous("Time (mins)", breaks = seq(0,150, 10), minor_breaks = seq(0,150, 5),position = "top") +
    scale_y_continuous("", breaks = -dt_i$place_overall_recalc,
                       labels = dt_i$place_name, minor_breaks = NULL,
                       limits = range(c(0,min(-dt_i$place_overall_recalc)-1))) +
    theme_minimal() +
    theme(legend.position="top")
  
  g <- apply_col(g, tri_cols)
  
  n_athletes <- length(unique(dt_i$Name))
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE, side = "top"),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=125, r=0, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 150 + 16*n_athletes, width = 700, scale = 2,
                                       format = "png",
                                       filename = paste0(dt_i$date_ymd[1],"_",j))) %>% 
    set_names_plotly() %>% 
    set_margin_plotly() %>% 
    clean_legend_names_plotly()
  
  savepath = paste0(getwd(), "/",site_path_relative,"/",dt_i$date_ymd[1],"_",j,".html")
  saveWidget(p, file = savepath ,selfcontained = FALSE,libdir = libpath)
    
  }
}


# Athlete plots -----------------------------------------------------------


for(k in athletes_ordered) {
  
  dt_k <- dt_all_long[(started) & Name==k][order(race_number)]
  
  dt_k[course == "int", course := "Intermediate"]
  dt_k[course == "full", course := "Full"]
  dt_k[course == "double", course := "Double Distance"]
  dt_k[, course := ordered(course, levels = c("Full", "Intermediate","Double Distance"))]
  
  dt_k[(valid_overall), place_name := paste0(place_overall_recalc, " ", Name)]
  dt_k[!(valid_overall), place_name := paste0("TBC ", Name)]
  
  dt_k[, tooltext := paste0("Race #: ", race_number,"\n",
                            "Date: ", date_ymd, "\n",
                            
                            part_plot_pb,
                            ifelse(split_valid & !(cumulative_valid)," (valid)",""),
                            ": ", 
                            duration_hms_short,
                            ifelse((isNewPB_split) & !(isPB_split)," New PB!",""),
                            ifelse((isPB_split)," Season PB!",""),
                            ifelse(ifelse(!is.na(rank_pb_split),rank_pb_split==1, FALSE)," Season record!",""),
                            "\n",
                            
                            
                            
                            "Cumulative",
                            ifelse((cumulative_valid),""," (invalid)"),
                            ifelse((cumulative_valid) & !(split_valid)," (valid)",""),
                            ": ",
                            cumulative_hms_short,
                            ifelse((isNewPB_cum)," New PB!",""))]
  
  n_courses <- length(unique(dt_all_long[Name==k, .(course)]$course))
  
  g <- ggplot(dt_k,
         aes(y = duration_mins, x = race_number, fill = part_plot_pb, col = part_plot_pb, group = race_number, text = tooltext)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    facet_grid(rows = "course") +
  scale_y_continuous("Time (mins)", breaks = seq(0,150, 10), minor_breaks = seq(0,150, 5),position = "left") +
    scale_x_continuous("Race (number)", breaks = 1:26, limits = c(0,27)) +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  g <- apply_col(g, tri_cols)
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=75, r=25, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = -0.1, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 500*n_courses, width = 700, scale = 2,
                                       format = "png",
                                       filename = paste0(k))) %>% 
    set_names_plotly() %>% 
    set_margin_plotly() %>% 
    clean_legend_names_plotly()
  
  savepath = paste0(getwd(),"/",site_path_relative,"/",k,".html")
  saveWidget(p, file = savepath,selfcontained = FALSE,libdir = libpath)
  
}


# Athlete tables ----------------------------------------------------------


for(k in athletes_ordered) {
  

  dt_k <- dt_all_long[(started) & Name==k]
  
  k_courses <- unique(dt_k$course)
  
  for(j in k_courses) {
  
    dt_k_wide <- dcast(dt_k[course == j], rank_pb_overall + athlete_rank_overall + total_overall_hms + date_ymd + race_number + valid_overall + isPB_overall + rank_pb_overall ~ part,
                       value.var = c("duration_hms", "athlete_rank_split","isPB_split", "isPB_cumulative", "split_valid",
                                     "rank_pb_split","cumulative_valid"))[order(athlete_rank_overall)]
    
    dt_k_wide[, Rank := athlete_rank_overall]
    
    setcolorder(dt_k_wide,
                c("Rank", "total_overall_hms",
                  "date_ymd","race_number",
                  "duration_hms_Swim", "duration_hms_Ride","duration_hms_Run",
                  "athlete_rank_split_Swim",
                  "athlete_rank_split_Ride",
                  "athlete_rank_split_Run"))
    
    
    cols_retain_old_names <- c("total_overall_hms",
                               "date_ymd",
                               "race_number",
                               "duration_hms_Swim",
                               "duration_hms_Ride",
                               "duration_hms_Run",
                               "athlete_rank_split_Swim",
                               "athlete_rank_split_Ride",
                               "athlete_rank_split_Run")
    
    cols_retain_new_names <- c("Time",
                     "Date",
                     "Race #",
                     "Swim",
                     "Ride",
                     "Run",
                     "Rank (Swim)",
                     "Rank (Ride)",
                     "Rank (Run)")
    
    setnames(dt_k_wide,
             cols_retain_old_names,
             cols_retain_new_names)
    
    col_ref_hide <- which(!(names(dt_k_wide) %in% c("Rank",cols_retain_new_names)))-1 # columns are indexed from 0 - row name?
    

    tab_k <- DT::datatable(dt_k_wide,
                  rownames = FALSE,
                  elementId = paste0("tab_", gsub("'","",k), "_", j),
                  # extensions = c('Buttons', 'Responsive'),
                  extensions = c('Buttons'),
                  options = list(autoWidth=FALSE,
                                 paging=FALSE,
                                 dom = 'Brtp',
                                 buttons = c('copy', 'csv', 'excel'),
                                 columnDefs = 
                                   list(list(visible=FALSE, targets=col_ref_hide)))) %>% 
      apply_col(tri_cols)

    
    savepath = paste0(getwd(),"/",site_path_relative,"/tab_",k,"_",j,".html")
    saveWidget(tab_k, file = savepath,selfcontained = FALSE,libdir = libpath)

  }
}


# Season record tables and plots ------------------------------------------



for(j in c("full", "int")) {
  
  dt_record_j <- dt_all_long[(started) & (isPB_overall) & course==j][order(rank_pb_overall)]
  
  dt_record_j_wide <- dcast(dt_record_j, valid_overall + rank_pb_overall + Name + total_overall_hms + date_ymd + race_number  ~ part,
                            value.var = c("duration_hms","isPB_split", "split_valid","rank_pb_split",
                                          "cumulative_valid"))
  
  dt_record_j_wide[, Rank := rank_pb_overall]
  
  
  setcolorder(dt_record_j_wide,
              c("Rank", "Name","total_overall_hms",
                "date_ymd","race_number",
                "duration_hms_Swim", "duration_hms_Ride","duration_hms_Run"))
  
  cols_train_old_names <- c("total_overall_hms",
                            "date_ymd",
                            "race_number",
                            "duration_hms_Swim",
                            "duration_hms_Ride",
                            "duration_hms_Run")
  
  cols_retain_new_names <-  c("Time",
                              "Date",
                              "Race #",
                              "Swim",
                              "Ride",
                              "Run")
  
  setnames(dt_record_j_wide,
           cols_train_old_names,
           cols_retain_new_names)
  
  col_ref_hide <- which(!(names(dt_record_j_wide) %in% c("Rank",cols_retain_new_names)))-1 # columns are indexed from 0 - row name?
  
  
  tab_j <- DT::datatable(dt_record_j_wide,
                         rownames = FALSE,
                         elementId = paste0("tab_record_", j),
                         # extensions = c('Buttons', 'Responsive'),
                         extensions = c('Buttons'),
                         options = list(autoWidth=FALSE,
                                        paging=FALSE,
                                        dom = 'Brtp',
                                        buttons = c('copy', 'csv', 'excel'),
                                        columnDefs = 
                                          list(list(visible=FALSE, targets=col_ref_hide)))) %>% 
    apply_col(tri_cols)
  
  savepath = paste0(getwd(),"/",site_path_relative,"/tab_overall_",j,".html")
  saveWidget(tab_j, file = savepath,selfcontained = FALSE,libdir = libpath)
  
  
  # Plot
  dt_record_j[, place_name := paste0(rank_pb_overall, " ", Name)]
  dt_record_j[(isPB_overall) & rank_pb_overall==1, place_name := paste0(place_overall_recalc, " ", Name, " (Season Record!)")]
  
  
  dt_record_j[, tooltext := paste0(Name, "\n",
                            "Race #: ", race_number,"\n",
                            "Date: ", date_ymd, "\n",

                            part_plot,
                            ifelse(split_valid & !(cumulative_valid)," (valid)",""),
                            ": ", 
                            duration_hms_short,
                            ifelse((isPB_split)," PB!",""),

                            ifelse(ifelse(!is.na(rank_pb_split),rank_pb_split==1, FALSE)," Season record!",""),
                            
                            "\n",
                            
                            "Cumulative",
                            ifelse((cumulative_valid), "", " (invalid)"),
                            ifelse((cumulative_valid) & !(split_valid), " (valid)",""),
                            ": ",
                            cumulative_hms_short
                            )]
  
  g <- ggplot(dt_record_j,
              aes(x = duration_mins, y = - rank_pb_overall, fill = part_plot, col = part_plot, group = Name, text = tooltext)) +
    geom_col(orientation = "y", size = 0.3) +
    scale_x_continuous("Time (mins)", breaks = seq(0,150, 10), minor_breaks = seq(0,150, 5),position = "top") +
    scale_y_continuous("", breaks = -dt_record_j$rank_pb_overall,
                       labels = dt_record_j$place_name, minor_breaks = NULL,
                       limits = range(c(0,min(-dt_record_j$rank_pb_overall)-1))) +
    theme_minimal() +
    theme(legend.position="top")
  
  g <- apply_col(g, tri_cols)
  
  n_athletes <- length(unique(dt_record_j$Name))
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE, side = "top"),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=125, r=0, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 150 + 16*n_athletes, width = 700, scale = 2,
                                       format = "png",
                                       filename = paste0("season_record_",j))) %>% 
    set_names_plotly() %>% 
    set_margin_plotly() %>% 
    clean_legend_names_plotly()

  
  savepath = paste0(getwd(), "/",site_path_relative,"/plot_record_",j,".html")
  saveWidget(p, file = savepath ,selfcontained = FALSE,libdir = libpath)
  
}

# Export for website ------------------------------------------------------


saveRDS(dt_season, "data_derived/dt_season.rds")
saveRDS(dt_all_long, "data_derived/dt_all_long.rds")


# Update website ----------------------------------------------------------


source("make_race_rmd.R")
source("make_athlete_rmd.R")
bookdown::render_book("index.Rmd",output_dir = site_path_relative)
unlink("data_derived/*.rds")
