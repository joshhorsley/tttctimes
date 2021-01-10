
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
    
    formatStyle(columns = "Time", valueColumns = "rank_pb_overall",
                background = styleEqual(c(1),c(tri_cols$record))) %>% 
    
    formatStyle(columns = "Time", valueColumns = "valid_overall",
                background = styleEqual(FALSE, tri_cols$invalid)) %>%
    
    formatStyle(columns = c("Swim","Ride","Run"),
                valueColumns = c("split_valid_Swim","split_valid_Ride","split_valid_Run"),
                background = styleEqual(FALSE, tri_cols$invalid))

}
