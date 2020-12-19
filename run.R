

# Packages ----------------------------------------------------------------


source("req_packages.R")


# Initial analysis --------------------------------------------------------


dt_times <- fread("data_provided/webscorer/TTTC 5 Dec 2020.txt")

dt_times[, place_overall := as.numeric(Place)]

dt_times[, Swim := as.ITime(`Lap 1`, format = "%M:%S")]
dt_times[, Ride := as.ITime(`Lap 2`, format = "%M:%S")]
dt_times[, Run := as.ITime(`Lap 3`, format = "%M:%S")]

dt_times_long <- melt.data.table(dt_times, id.vars = c("Name","place_overall"),
                                 measure.vars = c("Swim",
                                                  "Ride",
                                                  "Run"),
                                 variable.name = "part",value.name = "duration")

dt_times_long[, duration_mins := as.numeric(duration/60)]

problems_names <- c("Matt Gabb", "Val Lambard", "Rob Gillies")

dt_times_long[Name %in% problems_names, data_problem := TRUE]
dt_times_long[is.na(data_problem), data_problem := FALSE]

dt_times_long[, place_name := paste0(place_overall, " ", Name)]

dt_times_long[, tooltext := paste0(Name, "\n", part, ": ", duration)]



# Plots -------------------------------------------------------------------


g <- ggplot(dt_times_long[order(place_overall)][!(data_problem)],
            aes(x = duration_mins, y = - place_overall, fill = part, group = Name, text = tooltext)) +
  geom_col(orientation = "y") +
  # geom_col(aes(x = swim_duration/60 + ride_duration/60, y = Name),width = 2, orientation = "y", fill = "#3B8544") +
  # geom_col(aes(x = swim_duration/60, y = Name),width = 2, orientation = "y", fill = "#2E63BC")  +
  scale_fill_manual("Part",
                    labels = c(Swim = "Swim",
                               Ride = "Ride",
                               Run = "Run"),
                    values = c(Swim = "#2E63BC",
                               Ride = "#3B8544",
                               Run = "#BF5324")) +
  scale_x_continuous("Time (mins)", breaks = seq(0,100, 10), minor_breaks = seq(0,100, 5)) +
  scale_y_continuous("Athlete", breaks = -dt_times_long$place_overall,
                     labels = dt_times_long$place_name) +
  theme_minimal() +
  theme(legend.position="top")


ggsave(filename = "figures/2020-12-02_full.pdf",width = 6, height = 7)


p <- ggplotly(g,width = 800, height = 700, tooltip = "text",layerData = TRUE, style = "mobile")



# Export for website ------------------------------------------------------


saveRDS(p, "data_derived/plotly.rds")
saveRDS(dt_times_long, "data_derived/dt_times_long.rds")


# Update website ----------------------------------------------------------


bookdown::render_book("index.Rmd")
