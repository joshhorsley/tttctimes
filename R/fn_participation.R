

# Time series -------------------------------------------------------------


plotly_time_series <- function(dt_all_long, len_season){
  
  dt_entries <- dt_all_long[part=="Swim"]
  
  
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
    
    scale_y_continuous("Entries", breaks = seq(0,len_season,5), limits = c(0,len_season),position = "left") +
    scale_color_manual("Name", values = cols_athlete) +
    scale_shape_manual("Name", values = style_athlete) +
    scale_linetype_manual("Name", values = style_athlete) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  g <- myscale_x_racenumber(g, len_season)
  
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


plot_race_count <- function(dt_all_long,i_season, len_season){
  
  any_cancelled <- nrow(dt_season[season==i_season & (cancelled)]) > 0
  

  dt_entries_race <- dt_all_long[season==i_season & part=="Swim",
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
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"))
  
  if(any_cancelled){
    races_cancalled <- unique(dt_season[season==i_season & (cancelled)]$race_number)
    
    g <- g + annotate("text", x= races_cancalled, y = 2, label = "C")
  }
  
  g <- myscale_x_racenumber(g, len_season)
  
  p <- ggplotly(g, width = NULL, tooltip = "text",layerData = TRUE, style = "mobile") %>% 
    layout(xaxis = list(fixedrange = TRUE, side = "top"),
           yaxis = list(fixedrange = TRUE, tickfont = list(size = 10)),
           dragmode = FALSE,
           autosize = TRUE,
           margin = list(l=15, r=0, t=0,b=0, pad=0),
           legend = list(title = list(text="Course"),
                         orientation = "h", y = 0, x= 0.5, xanchor = "center",
                         itemclick = FALSE, itemdoubleclick  = FALSE)) %>% 
    config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")), displaylogo=FALSE,
           toImageButtonOptions = list(height = 500, width = 700, scale = 2,
                                       format = "png",
                                       filename = "participation_by_race")) %>% 
    set_margin_plotly()
  
  
  return(p)

}



# Total histogram ---------------------------------------------------------


tool_tip_names <- function(names_i, max_row = 20){
  
  
  n_names <- length(names_i)
  
  n_cols <- (n_names %/% max_row) + 1
  
  if(n_cols> 1) {
    joins <- c(rep_len(c(rep(", ", n_cols -1),"\n"),length.out = n_names-1), "")
    
    return(paste0(paste0(names_i, joins), collapse = ""))
    
  }
  
  return(paste0(names_i, collapse = "\n"))
  
  
}

plotly_part_hist <- function(dt_all_long, len_season = NULL, do_all = FALSE){
  
  
  if(!do_all) {
    
    dt_entries_hist <- dt_all_long[order(name_last)][part == "Swim" & (is_last_entry),
                                                     .(name_list = list(Name), count = .N),
                                                     by = entries_total]
    
    setorder(dt_entries_hist, -entries_total)
    
    dt_entries_hist[,tooltext :=paste0(count, " entered ",entries_total, " ", ifelse(entries_total==1, "race", "races"),":\n",
                                       tool_tip_names(name_list[[1]])),
                    by = entries_total]
    
    max_entries <- len_season
    
    setnames(dt_entries_hist, "entries_total", "entries_total_plot")
    
  }
  
  if(do_all){
    
    dt_entries_hist <- dt_all_long[order(name_last)][part == "Swim" & (is_last_entry_all),
                                                     .(name_list = list(Name), count = .N),
                                                     by = entries_total_all]
    
    setorder(dt_entries_hist, -entries_total_all)
    
    dt_entries_hist[,tooltext :=paste0(count, " entered ",entries_total_all, " ", ifelse(entries_total_all==1, "race", "races"),":\n",
                                       tool_tip_names(name_list[[1]])),
                                                         by = entries_total_all]
    
    max_entries <- max(dt_entries_hist$entries_total_all, na.rm=TRUE)
    
    setnames(dt_entries_hist, "entries_total_all", "entries_total_plot")
    
    
  }
  
  step_options <- c(2,5,10,20,50)
  n_ticks <- 15
  break_step <- step_options[min(which(step_options > (max_entries %/% n_ticks)))]
  breaks <- seq(0,max_entries,by = break_step)

  
  g <- ggplot(dt_entries_hist,
              aes(y = count, x = entries_total_plot, text = tooltext)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    scale_y_continuous("Number of atheletes") +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"))+
    scale_x_continuous("Total Entries", breaks = breaks, limits = c(0,max_entries+1))
  
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


table_part_total <- function(i_season=NULL, do_all = FALSE) {
  
  if(nrow(dt_all_long)==0) return("no data")
  
  
  if(!do_all) {

    dt_entries_tab <- dt_all_long[season==i_season & part == "Swim" & (is_last_entry)]
    setorder(dt_entries_tab, -entries_total, name_last)
    
    setcolorder(dt_entries_tab,
                c("entries_total_rank", "entries_total","entries_total_fd", "athlete_link" ))
    
    setnames(dt_entries_tab, "Name", "Name_old")
    setnames(dt_entries_tab,
             c("entries_total_rank", "entries_total","entries_total_fd", "athlete_link" ),
             c("Rank (All)","All","Full and Double", "Name"),
             skip_absent = TRUE)
  }
  
  if(do_all) {
    
    dt_entries_tab <- dt_all_long[ part == "Swim" & (is_last_entry_all)]
    setorder(dt_entries_tab, -entries_total_all, name_last)
    
    setcolorder(dt_entries_tab,
                c("entries_total_all_rank", "entries_total_all","entries_total_all_fd", "athlete_link" ))
    
    setnames(dt_entries_tab, "Name", "Name_old")
    setnames(dt_entries_tab,
             c("entries_total_all_rank", "entries_total_all","entries_total_all_fd","athlete_link" ),
             c("Rank (All)","All","Full and Double", "Name"),
             skip_absent = TRUE)
    
  }
  
  col_ref_hide <- which(!(names(dt_entries_tab) %in% c("Rank (All)","All","Full and Double","Name")))-1 # columns are indexed from 0 - row name?
  
  
  tab_part <- datatable_std(dt_entries_tab, col_ref_hide, escape=FALSE) %>% 
    formatStyle(columns = "Rank (All)", valueColumns = "Rank (All)",
                background = styleEqual(1,tri_cols$record))
  
  return(tab_part)

}



table_part_total_all <- function() {
  
  dt_temp <- dt_summary_all[TRUE]
  
  setorder(dt_temp, -entries)
  
  setcolorder(dt_temp, c("athlete_link","entries_fd", "entries","season_count","first_season", "latest_season"))
  
  setnames(dt_temp,
           c("athlete_link","Name","entries","entries_fd","season_count","first_season","latest_season"),
           c("Name","Name_old", "All", "Full and Double", "Seasons", "First Season", "Latest Season"))
  
  col_ref_hide <- which(!(names(dt_temp) %in% c("Name", "All", "Full and Double", "Seasons", "First Season", "Latest Season") ))-1 # columns are indexed from 0 - row name?
  
  tab_part <- datatable_std(dt_temp, col_ref_hide, escape=FALSE, filter = "top")
        
  return(tab_part)
}