# Calculate handicaps



# Path to webscorer start list --------------------------------------------


path_webscorer_confirmation <- "handicap/2022-02-05/TTTC 5 Feb 2022 start list.txt"


# Load --------------------------------------------------------------------


# Functions
dummy <- lapply(list.files("R", full.names = TRUE), source)

# Race results
dt_all_long <- readRDS("data_derived/dt_all_long.rds")
dt_season <- readRDS("data_derived/dt_season.rds")

# Start list
dt_start_list <- fread(path_webscorer_confirmation,
                     col.names = c("Name","name_first_in","name_last_in","course","chip"))


# Standardise names -------------------------------------------------------


# standardise names
dt_start_list[, Name_import := Name]
dt_start_list[, row_id := seq(.N)]
dt_start_list[, Name := standardise_names(Name), by = row_id ]

dt_start_list[, Name_lower := tolower(Name)]
dt_name_fix <- fread("data_provided/name_variations.csv")
dt_name_fix[, name_in := tolower(name_in) ]
dt_start_list[dt_name_fix, on = c(Name_lower = "name_in"), Name := i.name_out]


# Add missing chip numbers? -----------------------------------------------

# TODO


# Get handicaps -----------------------------------------------------------


date_last_race <- dt_season[(have_results), max(date_ymd)]
date_for_handicap <- dt_season[date_ymd > date_last_race, min(date_ymd)]

dt_handicaps_all <- get_all_handicaps(dt_all_long, dt_season)
dt_handicap <- dt_handicaps_all

# Join handicaps
new_cols <- setdiff(names(dt_handicaps_all), c("Name","course"))
dt_start_list[dt_handicaps_all, on = .(Name, course), (new_cols) := mget(new_cols)]



# Special requests --------------------------------------------------------


# apply requests
# dt_start_list[Name %in% c("Kev Bannerman", "Josh Horsley"), ]
# 
# dt_starts[dt_set, on = .(Name), Start := i.Start]
# 
# setorder(dt_starts, "Start" )


# Set all intermediate to 6:15?


# Save --------------------------------------------------------------------


path_out <- paste0(tools::file_path_sans_ext(path_webscorer_confirmation)," handicaps.csv")
fwrite(dt_start_list[, .(Name, chip, course, `Start time` = start_time_hms)],path_out)
