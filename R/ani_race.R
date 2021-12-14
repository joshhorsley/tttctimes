library(ggplot2)
library(gganimate)
library(data.table)


# Example https://stackoverflow.com/questions/52830938/how-to-gganimate-a-stacked-bar-graph

# df <- structure(list(name = c("variable", "variable", "variable",    
#                               "variable", "variable", "variable", "variable", "variable", "variable", 
#                               "variable", "variable", "variable"),
#                      groups = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 4L, 4L, 4L),
#                                         .Label = c("group 1", "group 2", "group 3", "group 4"),
#                                         class = "factor"),
#                      score = structure(c(4L,3L, 2L, 1L, 2L, 1L, 3L, 2L, 1L, 3L, 2L, 1L),
#                                        .Label = c("4","3", "2", "1"),
#                                        class = c("ordered", "factor")),
#                      percentage = c(8,38, 38, 16, 40, 42.8571428571429, 40, 20, 40,5, 65, 30),
#                      percentage2 = c("8%", "38%", "16%", "17.1%","40%", "42.9%", "40%", "20%", "40%", "5%", "65%", "30%"),
#                      label = c(0.04,0.27, 0.65, 0.0857142857142857, 0.371428571428571, 0.785714285714286,0.2, 0.5, 0.8, 0.025, 0.375, 0.85)),
#                 row.names = c(NA, -12L), class = "data.frame")
# 
# 
# df$c <- ave(df$percentage, df$group, FUN=cumsum)
# df <- df[order(df$groups, df$score, df$c), ]
# 
# 
# g <- ggplot(df, aes(x = name, y = c, fill = score, group = score)) +
#   geom_col(position = "identity", width = 0.8) +
#   coord_flip() +
#   labs(title = "{closest_state}") +
#   # geom_label(aes(y = c, label = percentage2)) +
#   scale_fill_manual(values = c("blue", "green", "red", "brown"), 
#                     guide= guide_legend(reverse = TRUE), drop = FALSE) +
#   transition_states(groups, transition_length = 2, state_length = 1)
# 
# 
# df$c <- round(runif(n = nrow(df))*100,0)
# 
# df$c <- c(0,0,0,
#           10,10,10,
#           10,40,40,
#           10,40,50
#           )


race_video <- function(i_date_ymd, j,j_is_champ,fps) {
  
  
  dt_i <- dt_all_long[date_ymd==i_date_ymd & course == j & j_is_champ == (is_champ)][(started)]


  dt_i[(valid_overall), place_name := paste0(place_overall_recalc," ", Name)]
  dt_i[!(valid_overall), place_name := paste0("TBC ", Name)]
  
  
  # Prep dataframe for animation
  dt_i[, stage_0 := 0]
  
  dt_i[, stage_1 := total_mins_sort[which(part=="Swim")], by = place_name]
  dt_i[, stage_2 := total_mins_sort[c(which(part=="Swim"),rep(which(part=="Ride"),2))], by = place_name]
  
  dt_i[, stage_3 := total_mins_sort]
  
  dt_i_prep <- melt.data.table(dt_i,
                  id.vars = c("place_name", "part", "total_mins_sort","place_overall_recalc","total_overall_mins","part_plot"),
                  measure.vars = c("stage_0","stage_1","stage_2","stage_3"),
                  variable.name = "stage",
                  value.name = "duration_at_stage")
  
  setorder(dt_i_prep,place_name, stage, -part)
  
  dt_i_prep[, stage := as.integer(substr(stage, 7,7))]
  
  dt_i_prep[, part_plot := droplevels(part_plot)]
  
  # dt_i_prep[, group := paste0(place_name, "-",part_plot)]


  g <- ggplot(dt_i_prep, aes(y = - place_overall_recalc, x = duration_at_stage, fill = part_plot, col = part_plot, group = place_name)) +
  # g <- ggplot(dt_i_prep, aes(y = - place_overall_recalc, x = duration_at_stage, fill = part_plot, col = part_plot,group = group)) +
    geom_col(orientation = "y", position = "identity", width = 0.8) +
    labs(title = paste0(format(as.Date(i_date_ymd), "%d %b %Y"), " ", dt_i$course_nice[1])) +
    # labs(title = "{closest_state}") +
    # labs(title = "11 Dec 2021", subtitle = "{next_state}") +
    scale_y_continuous("", breaks = -dt_i$place_overall_recalc,
                       labels = dt_i$place_name, minor_breaks = NULL,
                       limits = range(c(0,min(-dt_i$place_overall_recalc)-1))) +
    theme_minimal() +
    theme(legend.position="bottom",
          plot.title = element_text(size=20))
  
  g <- myscale_x_racetime(g)
  g <- apply_col(g, tri_cols)
  
  g <- g + transition_states(stage, transition_length = c(10,30,20), state_length = 3, wrap = FALSE)
  
  # g <- g + transition_reveal(total_mins_sort, keep_last = TRUE)

  n_athletes <- length(unique(dt_i$Name))

  g_an <- animate(g, duration = 10, fps = fps,
                  height = 150 + 16*n_athletes, width=700, res = 100, device = "png",
                  renderer = ffmpeg_renderer(format = "mp4",options = list(codec="libx264", pix_fmt ="yuv420p")))

  path_out <- file.path("video",
                        paste0(paste0(c(i_date_ymd, j,j_is_champ),collapse = "-"),".mp4"))
  
  anim_save(path_out, g_an)

}


if(!dir.exists("video")) dir.create("video")

if(FALSE) {
  race_video("2021-12-11", "full",FALSE,30)
  race_video("2021-12-11", "int",FALSE,30)
}