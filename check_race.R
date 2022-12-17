
# Check race --------------------------------------------------------------



date_check <- "2022-12-17"

p_races <- dt_season[date_ymd==date_check,list(list((plotly_race(dt_all_long, tri_cols, season, race_number, course,is_champ)))), by = .(course)]

p_races$V1
