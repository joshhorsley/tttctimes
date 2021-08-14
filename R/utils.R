
# Colour ------------------------------------------------------------------


apply_col <- function(x,...) {
  UseMethod("apply_col",x)
}

apply_col.gg <- function(g, tri_cols){
  
  g + scale_fill_manual("Part",
                        values = c(Swim = tri_cols$swim,
                                   Ride = tri_cols$ride,
                                   Run = tri_cols$run,
                                   `Swim (PB)` = tri_cols$pb,
                                   `Ride (PB)` = tri_cols$pb,
                                   `Run (PB)` = tri_cols$pb,
                                   `Swim (record)` = tri_cols$record,
                                   `Ride (record)` = tri_cols$record,
                                   `Run (record)` = tri_cols$record,
                                   `Swim (invalid)` = tri_cols$invalid,
                                   `Ride (invalid)` = tri_cols$invalid,
                                   `Run (invalid)` = tri_cols$invalid)) +
    scale_color_manual("Part",
                       values = c(Swim = NA,
                                  Ride = NA,
                                  Run = NA,
                                  `Swim (PB)` = tri_cols$swim,
                                  `Ride (PB)` = tri_cols$ride,
                                  `Run (PB)` = tri_cols$run,
                                  `Swim (record)` = tri_cols$swim,
                                  `Ride (record)` = tri_cols$ride,
                                  `Run (record)` = tri_cols$run,
                                  `Swim (invalid)` = tri_cols$swim,
                                  `Ride (invalid)` = tri_cols$ride,
                                  `Run (invalid)` = tri_cols$run))
}

apply_col.datatables <- function(tab, tri_cols){
  
  tab %>% 
    formatStyle(columns = c("Swim","Ride","Run"),
                valueColumns = c("isPB_split_Swim","isPB_split_Ride","isPB_split_Run"),
                background = styleEqual(TRUE, tri_cols$pb)) %>% 
    
    formatStyle(columns = c("Swim","Ride","Run"),
                valueColumns = c("rank_pb_split_Swim","rank_pb_split_Ride","rank_pb_split_Run"),
                background = styleEqual(1, tri_cols$record)) %>% 
    
    {if("isPB_overall" %in% names(tab$x$data)){
      formatStyle(table = ., columns = "Time", valueColumns = "isPB_overall",
                background = styleEqual(TRUE,tri_cols$pb))
      } else {.}
      } %>% 
    
    formatStyle(columns = "Time", valueColumns = "rank_pb_overall",
                background = styleEqual(1,tri_cols$record)) %>% 
    
    formatStyle(columns = "Time", valueColumns = "valid_overall",
                background = styleEqual(FALSE, tri_cols$invalid)) %>%
    
    formatStyle(columns = c("Swim","Ride","Run"),
                valueColumns = c("split_valid_Swim","split_valid_Ride","split_valid_Run"),
                background = styleEqual(FALSE, tri_cols$invalid))

}


# Table formatting --------------------------------------------------------


datatable_std <- function(df, col_ref_hide, scrollY_override = FALSE) {
  DT::datatable(df,
                rownames = FALSE,
                extensions = c('Buttons'),
                options = list(autoWidth=FALSE,
                               paging=FALSE,
                               dom = 'rtBp',
                               scrollY = if(!scrollY_override & (nrow(df) > 12)) { "500px"} else {NULL},
                               scrollX = "500px",
                               buttons = c('copy', 'csv', 'excel'),
                               columnDefs = 
                                 list(list(visible=FALSE, targets=col_ref_hide))))
    
}



# Plotly ------------------------------------------------------------------

#' Reference for plotly options
#' https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js
#' https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
#' https://plotly.com/javascript/configuration-options/#customize-download-plot-options


set_names_plotly <- function(p, new_name = "removed"){
  
  p$x$cur_data <- new_name
  names(p$x$attrs) <- new_name
  names(p$x$visdat) <- new_name
  names(p$x$attrs) <- new_name
  
  p
}

set_margin_plotly <- function(p, pad = 0) {
  
  p$sizingPolicy$padding <- pad
  
  p$sizingPolicy$browser$defaultWidth <- "100%"	
  p$sizingPolicy$browser$fill <- TRUE
  
  p$width <- "100%"
  
  
  p
}

#' plotly legends had (Swim,1) etc formatting for athlete and record plots
#' but not athelete

clean_legend_names_plotly <- function(p) {
  
  n_legend <- length(p$x$data)
  
  for (i_legend in seq(n_legend)) {
    p$x$data[[i_legend]]$name <- gsub(",1\\)$","",gsub("^\\(","",p$x$data[[i_legend]]$name))
    
  }
  
  p
}


# Time --------------------------------------------------------------------


to_hms <- function(x, nsmall_seconds = 1L) {
  paste0(hour(x), ":",
         format(minute(x), width=2),
         ":",
         format(round(second(x),nsmall_seconds),nsmall = nsmall_seconds, width=4)
  )
}

format_place <- function(x, na_string = "NA") {
  
  x[is.na(x)] <- 0
  y <- paste0(x, "th")

  y[x %% 10 == 1 & x != 11] <- paste0(x[x %% 10 == 1 & x != 11], "st")
  y[x %% 10 == 2 & x != 12] <- paste0(x[x %% 10 == 2 & x != 12], "nd")
  y[x %% 10 == 3 & x != 13] <- paste0(x[x %% 10 == 3 & x != 13], "rd")
  
  y[x == 0 ] <- na_string
  
  y
}



# Grammar -----------------------------------------------------------------


list_with_and <- function(parts) {
  n_parts <- length(parts !="")
  
  if(n_parts==0L) return("")
  if(n_parts==1L) return(parts)
  if(n_parts==2L) return(paste0(parts[1], " and ", parts[2]))
  
  paste0(paste0(parts[1:(n_parts-1)],collapse = ", "), ", and ",parts[n_parts])
  
  
}
