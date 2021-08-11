

# Record plot -------------------------------------------------------------



plotly_record <- function(dt_all_long, tri_cols, j) {
  
  dt_record_j <- dt_all_long[(started) & (isPB_overall) & course==j][order(rank_pb_overall)]
  
  
  dt_record_j[, place_name := paste0(rank_pb_overall, " ", Name)]
  
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
  
  p$height <- 50 + 16*n_athletes
  
  
  
  return(p)
}

# Record table ------------------------------------------------------------


table_record <- function(dt_all_long, tri_cols, j, l ) {
  
  if(l=="overall"){
    dt_record_j <- dt_all_long[(started) & (isPB_overall) & course==j][order(rank_pb_overall)]
  }
  
  if(l!="overall"){
    dt_record_j <- dt_all_long[(started) & (includes_pb_split) & course==j][order(part, rank_pb_split)]
  }
  
  dt_record_j[, Rank := rank_pb_split]
  dt_record_j_wide <- dcast(dt_record_j, valid_overall + isPB_overall + rank_pb_overall + Name + total_overall_hms + date_ymd + race_number  ~ part,
                            value.var = c("duration_hms","isPB_split", "split_valid","rank_pb_split","Rank","cumulative_valid"))
  dt_record_j_wide[, Rank_overall := rank_pb_overall]
  
  
  setcolorder(dt_record_j_wide,
              c(paste0("Rank_",l), "Name","total_overall_hms",
                "date_ymd","race_number",
                "duration_hms_Swim", "duration_hms_Ride","duration_hms_Run"))
  
  
  cols_train_old_names <- c("Rank_overall","Rank_Swim","Rank_Ride","Rank_Run",
                            "total_overall_hms","date_ymd","race_number",
                            "duration_hms_Swim","duration_hms_Ride","duration_hms_Run")
  
  cols_retain_new_names <-  c("Rank (Overall)","Rank (Swim)","Rank (Ride)","Rank (Run)",
                              "Time","Date","Race #",
                              "Swim","Ride","Run")
  
  setnames(dt_record_j_wide, cols_train_old_names, cols_retain_new_names, skip_absent = TRUE)
  
  col_ref_hide <- which(!(names(dt_record_j_wide) %in% c("Name",cols_retain_new_names)))-1 # columns are indexed from 0 - row name?
  
  
  # tab_j <- DT::datatable(data = ,
  # rownames = FALSE,
  # # elementId = paste0("tab_record_", j,"_",l),
  # extensions = c('Buttons'),
  # options = list(autoWidth=FALSE,
  #                paging=FALSE,
  #                dom = 'Brtp',
  #                scrollY = "500px",
  #                scrollX = "500px",
  #                buttons = c('copy', 'csv', 'excel'),
  #                columnDefs = 
  #                  list(list(visible=FALSE, targets=col_ref_hide)))) 
  
  dt_record_use <- if(l=="overall") {
    dt_record_j_wide
  } else {
    dt_record_j_wide[(get(paste0("isPB_split_",l)))][order(get(paste0("rank_pb_split_",l)))]
  }
  
  tab_j <- datatable_std(dt_record_use, col_ref_hide) %>%
    apply_col(tri_cols)
  
  return(tab_j)

}
