

# Time series -------------------------------------------------------------


plotly_time_series <- function(dt_all_long, len_season){
  
  dt_entries <- dt_all_long[part=="Swim" & (started)]
  
  
  dt_entries[, tooltext := paste0(Name, "\n",
                                  "Race #: ", race_number, "\n",
                                  "Date: ", date_ymd, "\n",
                                  "Entries so far: ", entries_cumulative)]
  
  setorder(dt_entries, name_last)
  
  dt_entries[, name_plot := ordered(factor(Name), levels = unique(Name))]
  
  
  cols_athlete <- rainbow(n_athletes_season)[order(runif(n_athletes_season))]
  style_athlete <- rep(1:6, (n_athletes_season %/% 6)+1)[1:n_athletes_season]
  
  g <- ggplot(data = dt_entries,
              aes(x = race_number, y = entries_cumulative,
                  col = name_plot, group = name_plot, linetype = name_plot, shape = name_plot,
                  text = tooltext) ) +
    geom_point() +
    geom_line() +
    
    scale_x_continuous("Race", breaks = 1:len_season, limits = c(0,len_season+1)) +
    scale_y_continuous("Entries", breaks = seq(0,len_season,5), limits = c(0,len_season),position = "left") +
    scale_color_manual("Name", values = cols_athlete) +
    scale_shape_manual("Name", values = style_athlete) +
    scale_linetype_manual("Name", values = style_athlete) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE, side = "top"),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=10, r=0, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = TRUE, itemdoubleclick  = TRUE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 500, width = 700, scale = 2,
                                       format = "png",
                                       filename = "participation_")) %>% 
    set_margin_plotly()
  
  
  return(p)
}



# Plot by race ------------------------------------------------------------


plot_race_count <- function(dt_all_long, len_season){

  dt_entries_race <- dt_all_long[part=="Swim" & (started),
                            .(count = .N,
                              date_ymd = date_ymd[1]),
                            by = .(race_number, course_nice)]
  
  dt_entries_race[, count_all_course := sum(count), by = race_number]
  

  
  
  dt_entries_race[, tooltext := paste0("Race #: ", race_number, "\n",
                                       "Date: ", date_ymd, "\n",
                                       count, " entered the ", course_nice, " course\n",
                                       count_all_course, " entered all courses")]
  
  setorder(dt_entries_race, race_number, course_nice)
  
  
  g <- ggplot(dt_entries_race,
         aes(y = count, x = race_number, text = tooltext, col = course_nice, fill = course_nice, group = race_number)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    scale_y_continuous("Number of atheletes") +
    scale_x_continuous("Race", breaks = 1:len_season, limits = c(0,len_season+1)) +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE, side = "top"),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=15, r=0, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 500, width = 700, scale = 2,
                                       format = "png",
                                       filename = "participation_by_race")) %>% 
    set_margin_plotly()
  
  
  return(p)

}



# Total histogram ---------------------------------------------------------


plotly_part_hist <- function(dt_all_long, len_season){
  
  
  dt_entries_hist <- dt_all_long[order(name_last)][(started) & part == "Swim" & (is_last_entry),
                                                   .(name_list = list(Name), count = .N),
                                                   by = entries_total]
  
  setorder(dt_entries_hist, -entries_total)
  
  dt_entries_hist[,tooltext :=paste0(count, " entered ",entries_total, " ", ifelse(entries_total==1, "race", "races"),":\n",
                                     paste0(name_list[[1]], collapse = "\n")),
                  by = entries_total]
  
  g <- ggplot(dt_entries_hist,
              aes(y = count, x = entries_total, text = tooltext)) +
    # aes(y = count, x = entries_total)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    scale_y_continuous("Number of atheletes") +
    scale_x_continuous("Total Entries", breaks = 1:len_season, limits = c(0,len_season+1)) +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=15, r=0, t=0,b=0, pad=0),
           legend = list(orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 500, width = 700, scale = 2,
                                       format = "png",
                                       filename = "participation_hist")) %>% 
    set_margin_plotly()
  
  
  return(p)
}



# Total table -------------------------------------------------------------


table_part_total <- function(dt_all_long, tri_cols) {
  
  if(nrow(dt_all_long)==0) return("no data")
  

  dt_entries_tab <- dt_all_long[(started) & part == "Swim" & (is_last_entry)]
  setorder(dt_entries_tab, -entries_total, name_last)
  
  setcolorder(dt_entries_tab,
              c("entries_total_rank", "entries_total", "athlete_link" ))
  
  setnames(dt_entries_tab, "Name", "Name_old")
  setnames(dt_entries_tab,
           c("entries_total_rank", "entries_total", "athlete_link" ),
           c("Rank","Entries", "Name"),
           skip_absent = TRUE)
  
  col_ref_hide <- which(!(names(dt_entries_tab) %in% c("Rank","Entries","Name")))-1 # columns are indexed from 0 - row name?
  
  
  tab_part <- datatable_std(dt_entries_tab, col_ref_hide, escape=FALSE) %>% 
    formatStyle(columns = "Rank", valueColumns = "Rank",
                background = styleEqual(1,tri_cols$record))
  
  return(tab_part)

}