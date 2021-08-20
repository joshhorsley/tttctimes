

# Functions ---------------------------------------------------------------


dummy <- lapply(list.files("R", full.names = TRUE), source)


# Colours -----------------------------------------------------------------


tri_cols <- list(pb = "pink",
                 record = "gold",
                 invalid = "grey",
                 swim = "#2E63BC",
                 ride = "#3B8544",
                 run = "#BF5324")


# Season ------------------------------------------------------------------


dt_season_import <- fread("data_provided/season_definitions.csv")
dt_season_import[, date_ymd := as.IDate(date_ymd, format = "%d/%m/%y")]


# Add multiple courses per race
dt_season_import[race_type %in% c("Standard", "Club Championships"), course_list := list(list("full","int"))]
dt_season_import[race_type == "Double Distance", course_list := list(list("double","full","int"))]
dt_season_import[race_type == "Super Teams", course_list := list(list("teams"))]

dt_season_import2 <- dt_season_import[, .(season,
                     race_number,
                     race_type,
                     cancelled,
                     cancelled_reason,
                     report_id,
                     course = unlist(course_list)),
                     by = date_ymd][order(season, race_number)]


# Add champtionship to full course
dt_season_import2[, is_champ := list(list(list(FALSE)))]
dt_season_import2[race_type == "Club Championships" & course == "full", 
                  is_champ := list(list(list(TRUE,FALSE)))]

dt_season <- dt_season_import2[, .(season,
                                   race_number,
                                   date_ymd,
                                   race_type,
                                   cancelled,
                                   cancelled_reason,
                                   report_id,
                                   is_champ = unlist(is_champ)),
                               by =.(date_ymd, course)][order(season, race_number)]


dt_season[course=="full", course_nice := "Full"]
dt_season[course=="full" & (is_champ), course_nice := "Club Championships"]
dt_season[course=="int", course_nice := "Intermediate"]
dt_season[course=="double", course_nice := "Double Distance"]

# Race report links
dt_season[, has_report := report_id !=""]
dt_season[(has_report), report_link := paste0('<a href = "http://www.twintownstriathlon.org.au/?p=',report_id,'" target="_blank">link</a>')]
dt_season[(has_report), report_link_md := paste0('[Club Report](http://www.twintownstriathlon.org.au/?p=',report_id,'){target="_blank"}')]

# race data paths
path_base <- "data_provided/webscorer"

dt_season[!(is_champ), dir_webscorer := file.path(path_base, season, date_ymd, course)]
dt_season[(is_champ), dir_webscorer := file.path(path_base, season, date_ymd, "champ")]

dummy <- dt_season[, if(!dir.exists(dir_webscorer)) dir.create(dir_webscorer,recursive = TRUE), by = dir_webscorer]

dt_season[, path_webscorer := list.files(dir_webscorer,full.names = TRUE, pattern = "txt$"), by = dir_webscorer]

dt_season[, have_results := file.exists(path_webscorer)]

dt_season[, missing_results := !cancelled & !have_results]

dt_season[, row_id := seq(.N)]

# add race page links
dt_season[, race_ref := paste0('r-', date_ymd)]
dt_season[(have_results), race_link := paste0('<a href="', race_ref,'.html#',race_ref, '">',date_ymd,'</a>')]
dt_season[!(have_results), race_link := format(date_ymd, "%Y-%m-%d")]


# Load race results -------------------------------------------------------


dt_season[(have_results), dt_race := .(list(fread(path_webscorer))), by = row_id]

# Flatten available results
id_results <- which(dt_season$have_results)

dt_all <- foreach(i=id_results, .combine = function(x,y) rbind(x,y,fill=TRUE)) %do% {
  dt_season[i, .(row_id, dt_race[[1]])]
}

new_cols <- c("season","race_number", "date_ymd", "race_type", "course", "is_champ","race_ref", "race_link")
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
                               id.vars = c("season","race_number", "date_ymd", "race_type", "course",
                                           "Name","place_import","time_overall_import","started","is_champ", "race_ref", "race_link"),
                               measure.vars = c("Swim",
                                                  "Ride",
                                                  "Run"),
                                 variable.name = c("part"),
                               value.name = c("duration_import"))


# Name inconsistencies ----------------------------------------------------


dt_all_long[Name %in% c("Andrew CARR"), Name := "Andrew Carr"]
dt_all_long[Name %in% c("Robyn BARRY"), Name := "Robyn Barry"]
dt_all_long[Name %in% c("Scott BORNHOLT"), Name := "Scott Bornholt"]
dt_all_long[Name %in% c("Jo COLJA"), Name := "Joanne Colja"]
dt_all_long[Name %in% c("Dave de Closey","Dave De Closey","Dave DE CLOSEY","David De Closey"), Name := "David De Closey"]
dt_all_long[Name %in% c("Ian Driffil"), Name := "Ian Driffill"]
dt_all_long[Name %in% c("Melanie DUFF"), Name := "Melanie Duff"]
dt_all_long[Name %in% c("Peta EDGE"), Name := "Peta Edge"]
dt_all_long[Name %in% c("Greg FREEMAN", "Greg Freeman"), Name := "Gregory Freeman"]
dt_all_long[Name %in% c("Amanda Hall"), Name := "Manda Hall"]
dt_all_long[Name %in% c("Cassie HEASLIP"), Name := "Cassandra Heaslip"]
dt_all_long[Name %in% c("Joshua Horsley"), Name := "Josh Horsley"]
dt_all_long[Name %in% c("Sally KINGSTON"), Name := "Sally Kingston"]
dt_all_long[Name %in% c("Lydia Kuschnirz"), Name := "Lydia Kuschmirz"]
dt_all_long[Name %in% c("Valerie Lambard"), Name := "Val Lambard"]
dt_all_long[Name %in% c("Samatha Leonard", "Sam Leonard"), Name := "Samantha Leonard"]
dt_all_long[Name %in% c("Virginia Jones", "Ginny JONES"), Name := "Ginny Jones"]
dt_all_long[Name %in% c("Aaron NEYLAN"), Name := "Aaron Neylan"]
dt_all_long[Name %in% c("Karen Nixon-Hind"), Name := "Karen Nixon"]
dt_all_long[Name %in% c("Stephen RING"), Name := "Stephen Ring"]
dt_all_long[Name %in% c("Hollie ROBARDS"), Name := "Hollie Robards"]
dt_all_long[Name %in% c("Philip SALTER"), Name := "Philip Salter"]
dt_all_long[Name %in% c("Wendy Saunders"), Name := "Wendy Sanders"]
dt_all_long[Name %in% c("Vaughan SKELLY"), Name := "Vaughan Skelly"]
dt_all_long[Name %in% c("Terence SIMPSON"), Name := "Terence Simpson"]
dt_all_long[Name %in% c("STEVE WEST"), Name := "Steve West"]
dt_all_long[Name %in% c("ZOE TAYLOR-WEST"), Name := "Zoe Taylor-West"]
dt_all_long[Name %in% c("Scott THOMSON"), Name := "Scott Thomson"]
dt_all_long[Name %in% c("Sebastian THOMSON"), Name := "Sebastian Thomson"]
dt_all_long[Name %in% c("Ava THOMSON"), Name := "Ava Thomson"]
dt_all_long[Name %in% c("Jo Ward", "Joe Ward"), Name := "Jolyon Ward"]


# Separate Names ----------------------------------------------------------


dt_all_long[, row_id := seq(.N)]
dt_all_long[, name_first := strsplit(Name, " ")[[1]][1],  by = row_id]

dt_all_long[, name_last := gsub(paste0(name_first, " "), "", Name), by = row_id]
dt_all_long[name_first == Name, name_last := ""] # Catch any people with only a first name


# Athlete refs and links
dt_all_long[, athlete_ref := gsub(pattern = " ", "-", Name)]  
dt_all_long[, athlete_ref := gsub(pattern = "'", "", athlete_ref)]
dt_all_long[, athlete_ref := paste0("a-",athlete_ref)]
dt_all_long[, athlete_link := paste0('<a href="', athlete_ref,'.html#',athlete_ref, '">',Name,'</a>')]


# Timing errors -----------------------------------------------------------


dt_problems <- fread("data_provided/webscorer/webscorer_problems.csv")
dt_problems[, date_ymd := as.IDate(date_ymd, format = "%d/%m/%y")]

dt_all_long[dt_problems, on = .(race_number, date_ymd, Name, part), `:=`(split_valid = i.split_valid, cumulative_valid = i.cumulative_valid)]
dt_all_long[is.na(split_valid), split_valid := TRUE]
dt_all_long[is.na(cumulative_valid), cumulative_valid := TRUE]


# overall_invalid if cumulative time at end of run is invalid
dt_all_long[, valid_overall := cumulative_valid[which(part=="Run")], by = .(season, race_number, Name)]
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
dt_all_long[(total_disagree),total_resolve := total_resolve[which(part=="Run")],by = .(Name, season, race_number)]

dt_all_long[(total_disagree) & total_resolve =="cumulative",
            `:=`(total_resolved = TRUE,
                 overall_seconds = cumulative_seconds[which(part=="Run")]), by = .(Name, season, race_number)]

dt_all_long[(total_disagree) & total_resolve =="imported" & part=="Run", cumulative_seconds := overall_seconds]
dt_all_long[(total_disagree) & total_resolve =="imported", total_resolved := TRUE]


n_total_resolve_errors <- nrow(dt_all_long[(total_disagree) & !(total_resolved)])
stopifnot(n_total_resolve_errors==0)


# Catch cases where overall time is invalid but a run time has been recorded
dt_all_long[(started) & is.na(overall_seconds), overall_seconds := max(cumulative_seconds), by = .(Name, season, race_number)]


dt_all_long[, duration_mins := duration_seconds/60]
dt_all_long[, cumulative_mins := cumulative_seconds/60]
dt_all_long[, total_overall_mins := overall_seconds/60]

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


dt_all_long[(started), place_lap := as.integer(rank(duration_mins_sort, ties.method = "first")), by = .(season, race_number, course, part, is_champ)]
dt_all_long[(started), place_cum_recalc := as.integer(rank(total_mins_sort, ties.method = "first")), by = .(season, race_number, course, part, is_champ)]
dt_all_long[(started), place_overall_recalc := as.integer(rank(total_overall_sort, ties.method = "first")), by = .(season, race_number, course, part, is_champ)]

dt_all_long[(started), athlete_rank_split := as.integer(rank(duration_mins_sort, ties.method = "first")), by = .(Name, season, course, part)]
dt_all_long[(started), athlete_rank_cumulative := as.integer(rank(total_mins_sort, ties.method = "first")), by = .(Name, season, course, part)]
dt_all_long[(started), athlete_rank_overall := as.integer(rank(total_overall_sort, ties.method = "first")), by = .(Name, season, course, part)]

dt_all_long[(started) & (split_valid), place_lap_display := place_lap]
dt_all_long[(started) & !(split_valid), place_lap_display := NA]


dt_all_long[(started), place_lap_nice := format_place(place_lap_display)]
dt_all_long[(started), place_cum_nice := format_place(place_cum_recalc)]



# Participation -----------------------------------------------------------


dt_all_long[(started), entries_cumulative := cumsum(started), by = .(Name, season, part)]
dt_all_long[(started), entries_total := max(entries_cumulative), by = .(Name, season)]
dt_all_long[(started), is_last_entry := entries_total == entries_cumulative]

dt_all_long[(started) & (is_last_entry), entries_total_rank := rank(-entries_total, ties.method = "min"), by = .(season,part)]


# PBs ---------------------------------------------------------------------


dt_all_long[(started), isFirstRace := entries_cumulative == 1, by = .(Name, season, part) ]


# PB change over seasons
dt_all_long[(started) & (split_valid), pb_split_running := cummin(duration_mins), by = .(Name, season, part, course) ]
dt_all_long[(started) & (split_valid), isNewPB_split := duration_mins == pb_split_running, by = .(Name, season, part, course) ]

dt_all_long[is.na(isNewPB_split), isNewPB_split := FALSE]
dt_all_long[(isFirstRace), isNewPB_split := FALSE]

dt_all_long[(started) & (cumulative_valid), pb_cum_running := cummin(cumulative_mins), by = .(Name, season, part, course) ]
dt_all_long[(started) & (cumulative_valid), isNewPB_cum := cumulative_mins == pb_cum_running, by = .(Name, season, part, course) ]

dt_all_long[is.na(isNewPB_cum), isNewPB_cum := FALSE]
dt_all_long[(isFirstRace), isNewPB_cum := FALSE]

dt_all_long[(started) & (valid_overall), pb_overall_running := cummin(total_overall_mins), by = .(Name, season, course) ]
dt_all_long[(started) & (valid_overall), isNewPB_overall := total_overall_mins == pb_overall_running, by = .(Name, season, course) ]

dt_all_long[is.na(isNewPB_overall), isNewPB_overall := FALSE]
dt_all_long[(isFirstRace), isNewPB_overall := FALSE]

# season PB
dt_all_long[(started) & (valid_overall), pb_overall := min(total_overall_mins), by = .(Name, season, course) ]
dt_all_long[(started) & (valid_overall), isPB_overall := total_overall_mins == pb_overall, by = .(Name, season, course) ]
dt_all_long[is.na(isPB_overall), isPB_overall := FALSE]

dt_all_long[(started) & (split_valid), pb_split := min(duration_mins), by = .(Name, season, part, course) ]
dt_all_long[(started) & (split_valid), isPB_split := duration_mins == pb_split, by = .(Name, season, part, course) ]
dt_all_long[is.na(isPB_split), isPB_split := FALSE]

dt_all_long[(started) & (cumulative_valid), pb_cumulative := min(cumulative_mins), by = .(Name, season, part, course) ]
dt_all_long[(started) & (cumulative_valid), isPB_cumulative := cumulative_mins == pb_cumulative, by = .(Name, season, part, course) ]
dt_all_long[is.na(isPB_cumulative), isPB_cumulative := FALSE]


# Records -----------------------------------------------------------------


dt_all_long[(started) & (isPB_overall), rank_pb_overall := rank(total_overall_mins, ties.method = "first"), by = .(course, season, part)]
dt_all_long[(started) & (isPB_split), rank_pb_split := rank(duration_mins, ties.method = "first"), by = .(course, season, part)]

dt_all_long[(started), includes_pb_split := any(isPB_split), by = .(Name, season, course, race_number)]

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


dt_all_long[course=="double", course_nice := "Double Distance"]
dt_all_long[course=="full", course_nice := "Full"]
dt_all_long[course=="int", course_nice := "Intermediate"]

dt_all_long[, course_nice := ordered(course_nice, levels = c("Intermediate", "Full", "Double Distance"))]


# Plot prep ---------------------------------------------------------------


set.seed(100)
setWidgetIdSeed(100)
#' Plotly sets ids via plotly::new_id()
#' which calls base::tempfile()
#' which doesn't seem reproducible - annoying

site_path_relative <- "docs"
if(!dir.exists(site_path_relative)) dir.create(site_path_relative)
libpath <- file.path(getwd(), "docs/libs")


# Export for website ------------------------------------------------------


if(!dir.exists("data_derived")) dir.create("data_derived")
saveRDS(dt_season, "data_derived/dt_season.rds")
saveRDS(dt_all_long, "data_derived/dt_all_long.rds")


# Update website ----------------------------------------------------------


source("make_seasons_rmd.R")
source("make_athlete_rmd.R")
bookdown::render_book("index.Rmd",output_dir = site_path_relative)
