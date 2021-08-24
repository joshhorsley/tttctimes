
# Plots -------------------------------------------------------------------

plotly_race <- function(dt_all_long, tri_cols, i, j,j_is_champ) {
  
  
  dt_i <- dt_all_long[race_number== i & course == j & j_is_champ == (is_champ)][(started)]
  
  # Plot
  dt_i[(valid_overall), place_name := paste0(place_overall_recalc," ", Name)]
  # dt_i[(valid_overall) & (isNewPB_overall), place_name := paste0(place_overall_recalc, " ", Name, " (New PB!)")]
  # dt_i[(valid_overall) & (isPB_overall), place_name := paste0(place_overall_recalc, " ", Name, " (Season PB!)")]
  # dt_i[(isPB_overall) & rank_pb_overall==1, place_name := paste0(place_overall_recalc, " ", Name, " (Season Record!)")]
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
    scale_y_continuous("", breaks = -dt_i$place_overall_recalc,
                       labels = dt_i$place_name, minor_breaks = NULL,
                       limits = range(c(0,min(-dt_i$place_overall_recalc)-1))) +
    theme_minimal() +
    theme(legend.position="top")
  
 
  g <- myscale_x_racetime(g)
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
  
  p$height <- 50 + 16*n_athletes
  
  return(p)
}



# Tables ------------------------------------------------------------------


table_race <- function(dt_all_long, tri_cols, i, j,j_is_champ) {
  
  dt_i <- dt_all_long[race_number== i & course == j & j_is_champ == (is_champ) & (started)]
  
  dt_i_wide <- dcast(dt_i, rank_pb_overall + athlete_link+ place_overall_recalc + athlete_rank_overall + total_overall_hms + date_ymd + race_number + valid_overall + isPB_overall + rank_pb_overall ~ part,
                     value.var = c("duration_hms", "athlete_rank_split","isPB_split", "isPB_cumulative", "split_valid","place_lap_nice","place_lap",
                                   "rank_pb_split","cumulative_valid"))[order(athlete_rank_overall)]
  
  dt_i_wide[, Rank := place_overall_recalc]
  
  setnames(dt_i_wide,"athlete_link","Name")
  
  
  setcolorder(dt_i_wide,
              c("Rank", "Name","total_overall_hms",
                "date_ymd","race_number",
                "duration_hms_Swim", "duration_hms_Ride","duration_hms_Run",
                "athlete_rank_split_Swim",
                "athlete_rank_split_Ride",
                "athlete_rank_split_Run"))
  
  cols_retain_old_names <- c("total_overall_hms","duration_hms_Swim","duration_hms_Ride","duration_hms_Run",
                             "place_lap_Swim","place_lap_Ride","place_lap_Run")
  
  cols_retain_new_names <- c("Time","Swim","Ride","Run",
                             "Rank (Swim)","Rank (Ride)","Rank (Run)")
  
  setnames(dt_i_wide,cols_retain_old_names,cols_retain_new_names)
  
  col_ref_hide <- which(!(names(dt_i_wide) %in% c("Rank","Name", cols_retain_new_names)))-1 # columns are indexed from 0 - row name?
  
  
  tab_i <- datatable_std(dt_i_wide[order(Rank)], col_ref_hide, escape = FALSE) %>% 
    apply_col(tri_cols)
  
  tab_i$sizingPolicy$browser$fill <- TRUE
  

  return(tab_i)
}
