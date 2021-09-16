
# Plot --------------------------------------------------------------------



plotly_athlete <- function(dt_all_long, tri_cols, i_season,k, len_season) {
  
  dt_k <- dt_all_long[(started) & Name==k & season==i_season][order(race_number)]
  
  any_cancelled <- nrow(dt_season[season==i_season & (cancelled)]) > 0
  
  dt_k[part=="Run" & course=="double", plot_label := "D"]
  dt_k[part=="Run" & course=="int", plot_label := "I"]
  dt_k[part=="Run" & course=="teams", plot_label := "T"]
  
  dt_k[!is.na(plot_label), y_plot_label := total_overall_mins]
  dt_k[!is.na(plot_label) & is.na(y_plot_label), y_plot_label := 0]
  
  # dt_k[, course := course_nice]
  # dt_k[course == "int", course := "Intermediate"]
  # dt_k[course == "full", course := "Full"]
  # dt_k[course == "double", course := "Double Distance"]
  # dt_k[, course := ordered(course, levels = c("Full", "Intermediate","Double Distance"))]
  
  dt_k[(valid_overall), place_name := paste0(place_overall_recalc, " ", Name)]
  dt_k[!(valid_overall), place_name := paste0("TBC ", Name)]
  
  dt_k[course !="teams", tooltext := paste0("Race #: ", race_number,"\n",
                            "Date: ", date_ymd, "\n",
                            "Course: ", course_nice, "\n",
                            
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
  
  dt_k[course=="teams",  tooltext := paste0("Race #: ", race_number,"\n",
                                                      "Date: ", date_ymd, "\n",
                                                      "Course: ", course_nice, "\n",
                                                      "No timing avaialble")]
  
  
  # n_courses <- length(unique(dt_all_long[(started) & Name==k, .(course)]$course))
  
  g <- ggplot(dt_k,
              aes(y = duration_mins, x = race_number, fill = part_plot_pb, col = part_plot_pb, group = race_number, text = tooltext)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    # facet_grid(rows = "course") +
    scale_y_continuous("Time (mins)", breaks = seq(0,150, 10), minor_breaks = seq(0,150, 5),position = "left") +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white")) +
    annotate("text", x = dt_k[!is.na(plot_label)]$race_number, y = 2+ dt_k[!is.na(plot_label)]$y_plot_label, label = dt_k[!is.na(plot_label)]$plot_label )
    # geom_text(data = dt_k[!(is.na(plot_label))],aes(y = total_mins_sort, x = race_number, label = plot_label), color = "black", nudge_y = 2, show.legend = FALSE)
    # geom_text(x = 20, y = 90, label = "test")
  
  if(any_cancelled){
    races_cancalled <- unique(dt_season[season==i_season & (cancelled)]$race_number)
  
    g <- g + annotate("text", x= races_cancalled, y = 2, label = "C")
  }

  
  g <- myscale_x_racenumber(g, len_season)
  g <- apply_col(g, tri_cols)
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE), # axis put at bottom because plotly can't do this well with ggplot2 faceting
           yaxis = list(fixedrange = TRUE),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=75, r=25, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = -0.1, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           # toImageButtonOptions = list(height = 500*n_courses, width = 700, scale = 2,
           toImageButtonOptions = list(height = 500, width = 700, scale = 2,
                                       format = "png",
                                       filename = paste0(k))) %>% 
    set_names_plotly() %>% 
    set_margin_plotly() %>% 
    clean_legend_names_plotly()
  
  p$height <- 500

  return(p)
}



# Table -------------------------------------------------------------------


table_athlete_course  <- function(dt_all_long, tri_cols, i_season, k, j) {
  
  
  dt_k_wide <- dcast(dt_all_long[(started) & Name==k & course == j & season==i_season], rank_pb_overall + athlete_rank_overall + total_overall_hms + race_link + race_number + valid_overall + isPB_overall + rank_pb_overall ~ part,
                     value.var = c("duration_hms", "athlete_rank_split","isPB_split", "isPB_cumulative", "split_valid",
                                   "rank_pb_split","cumulative_valid"))[order(athlete_rank_overall)]
  
  dt_k_wide[, Rank := athlete_rank_overall]
  
  setcolorder(dt_k_wide,
              c("Rank", "total_overall_hms",
                "race_link","race_number",
                "duration_hms_Swim", "duration_hms_Ride","duration_hms_Run",
                "athlete_rank_split_Swim","athlete_rank_split_Ride","athlete_rank_split_Run"))
  
  
  cols_retain_old_names <- c("total_overall_hms",
                             "race_link","race_number",
                             "duration_hms_Swim","duration_hms_Ride","duration_hms_Run",
                             "athlete_rank_split_Swim","athlete_rank_split_Ride","athlete_rank_split_Run")
  
  cols_retain_new_names <- c("Time",
                             "Date","Race #",
                             "Swim","Ride","Run",
                             "Rank (Swim)","Rank (Ride)","Rank (Run)")
  
  setnames(dt_k_wide, cols_retain_old_names, cols_retain_new_names)
  
  col_ref_hide <- which(!(names(dt_k_wide) %in% c("Rank",cols_retain_new_names)))-1 # columns are indexed from 0 - row name?
  
  
  tab_k <- datatable_std(dt_k_wide, col_ref_hide, escape = FALSE) %>%
    apply_col(tri_cols)

  
  return(tab_k)
}
