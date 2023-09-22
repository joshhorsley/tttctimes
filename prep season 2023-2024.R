


# Total races -------------------------------------------------------------


# Best time ---------------------------------------------------------------


dt_best_2022 <- dt_all_long[season=="2022-2023" & (started) & (isPB_overall) & course==j & part == "Swim",  .(Bib, Name, overall_seconds)]

dt_best_2022[, overall_hms := seconds_to_hms(overall_seconds)]
dt_best_2022[, overall_seconds := NULL]
      
dt_best_2022[, Bib := as.numeric(Bib)]                                                                                 

setorder(dt_best_2022, Bib)


setnames(dt_best_2022, c("Bib","Name","overall_hms"),c("bib_2023","name","best_time_2022_hms"))

dt_best_2022[name=="Tracy Foyster", bib_2023 := 77]
dt_best_2022[name=="Bec McLucas", bib_2023 := 1]

dt_best_2022[name=="Tim Mehlert", bib_2023 := NA]

setorder(dt_best_2022, bib_2023, na.last = TRUE)

dt_repeated <- dt_best_2022[, .(N = .N, name = name), by = bib_2023][N>1]

if(nrow(dt_repeated)) stop("Repeated bibs")

# Save --------------------------------------------------------------------


openxlsx::write.xlsx(dt_best_2022, "data_derived/previous_season_summary.xlsx")
