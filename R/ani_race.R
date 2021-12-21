race_video <- function(i_date_ymd, j,j_is_champ,fps, duration  =18L, scaling = 2L) {
  
  
  dt_i <- dt_all_long[date_ymd==i_date_ymd & course == j & j_is_champ == (is_champ)][(started)]
  
  n_athletes <- length(unique(dt_i$Name))
  
  dt_prep_new <- dt_i[rep(1:(n_athletes*3), each = 7), .(Name, part, part_plot, name_last, name_first, cumulative_mins, place_cum_recalc, isPB_cumulative )]
  
  
  # Define animation stages
  dt_prep_new[, stage_E := rep(c(0L,1L,1L,2L,2L,3L,3L),n_athletes*3)] # Extend
  dt_prep_new[, stage_R := rep(c(0L,0L,1L,1L,2L,2L,3L),n_athletes*3)] # Re-order
  dt_prep_new[, stage_overall := rep(1:7, n_athletes*3)]
  
  setorder(dt_prep_new, name_last, stage_E, stage_R, part)
  
  # set widths for extensions
  dt_prep_new[stage_E==0L, width := 0]
  dt_prep_new[stage_E==1L, width := cumulative_mins[which(part=="Swim")], by = .(Name, stage_overall)]
  dt_prep_new[stage_E==2L, width := cumulative_mins[c(which(part=="Swim"),rep(which(part=="Ride"),2))], by = .(Name, stage_overall)]
  dt_prep_new[stage_E==3L, width := cumulative_mins]
  
  
  # set vertical orders
  dt_prep_new[stage_R==0L, place := order(name_last, name_first), by = .(part, stage_overall)]
  dt_prep_new[stage_R==1L, place := place_cum_recalc[which(part=="Swim")], by = .(Name, stage_overall)]
  dt_prep_new[stage_R==2L, place := place_cum_recalc[which(part=="Ride")], by = .(Name, stage_overall)]
  dt_prep_new[stage_R==3L, place := place_cum_recalc[which(part=="Run")], by = .(Name, stage_overall)]
  
  
  dt_prep_new[, place_last := shift(place,2), by = .(Name, part)]
  dt_prep_new[stage_overall >=4, place_diff := place - place_last]

  
  dt_prep_new[stage_overall %in% c(5,7) & place_diff < 0, `:=`(change_type = "up", place_text_up = paste0("\u2191", " ", -place_diff))]
  dt_prep_new[stage_overall  %in% c(5,7) & place_diff > 0, `:=`(change_type = "down", place_text_down = paste0("\u2193", " ", place_diff))]
  dt_prep_new[stage_overall  %in% c(5,7) & place_diff == 0, `:=`(change_type = "equal",place_text_eq = paste0("-"))]
  
  
  dt_prep_new[Name=="Josh Horsley"]
  
  setorder(dt_prep_new, name_last, stage_E, stage_R, -part)
  
  
  break_step <- ifelse( max(dt_prep_new$cumulative_mins, na.rm=TRUE) > 100, 20, 10)
  lim_max <- max(dt_prep_new$cumulative_mins, na.rm=TRUE)
  
  name_position <- -35
  place_change_offset <- -10
  
  g <- ggplot(dt_prep_new, aes(y = - place, x = width, fill = part_plot, col = part_plot, group = Name)) +
    geom_col(orientation = "y", position = "identity", width = 0.8) +
    labs(title = paste0(format(as.Date(i_date_ymd), "%d %b %Y"), " ", ifelse(j_is_champ,"Club Championship",as.character(dt_i$course_nice[1])))) +
    geom_text(x=name_position, aes(label = Name), show.legend = FALSE, hjust = "left", col = "Black") +
    geom_text(x = name_position + place_change_offset, aes(label = place_text_up), show.legend = FALSE, hjust = "left", col = tri_cols$ride) +
    geom_text(x = name_position + place_change_offset, aes(label = place_text_down), show.legend = FALSE, hjust = "left", col = tri_cols$run) +
    geom_text(x = name_position + place_change_offset, aes(label = place_text_eq), show.legend = FALSE, hjust = "left", col = tri_cols$swim) +
    scale_y_continuous("",
                       breaks = -seq(n_athletes),
                       minor_breaks = -seq(n_athletes),
                       labels = seq(n_athletes))+
    theme_minimal() +
    theme(legend.position="bottom",
          plot.title = element_text(size=20))

  g <- g + scale_x_continuous("Time (mins)", breaks = seq(0,150, break_step), minor_breaks = seq(0,150, 5),position = "top", limits = c(name_position + place_change_offset,lim_max))
  
  g <- apply_col(g, tri_cols)
  
  # Apply animation
  g <- g + transition_states(stage_overall, transition_length = 1, state_length = 6, wrap = FALSE)
  
  g_an <- animate(g, duration = duration, fps = fps,
                  height = scaling*(150 + 16*n_athletes), width=scaling*700, res = scaling*100, device = "png",
                  renderer = ffmpeg_renderer(format = "mp4",
                                             options = list(codec="libx264",
                                                            pix_fmt ="yuv420p",
                                                            vf = 'pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2"'))) # avoid error: [libx264 @ 0x7fe29e811c00] height not divisible by 2 (700x869)
  
  
  path_out <- file.path("video",
                        paste0(paste0(c(i_date_ymd, j,{ if(j_is_champ) {"champ"}}),collapse = "-"),".mp4"))
  
  anim_save(path_out, g_an)
  
}
  

# n_stages <- 7
# 
# for(i in 1:(n_stages-1)) {
#   do_animation_stage(dt_prep_new, i, 60)
# }
# 
# control_content <- paste0(paste0("file '", seq(n_stages-1), ".mp4'"),collapse = "\n")
# 
# 
# control_file <- "video_temp/video_list.txt"
# write(control_content, control_file)

# system(command = paste0("ls video_temp/"))
# system(command = paste0("cd video_temp && ffmpeg -y -f concat -i video_list.txt -c copy out.mp4"))





# if(!dir.exists("video_temp")) dir.create("video_temp")
if(!dir.exists("video")) dir.create("video")

if(FALSE) {
  i_date_ymd <- "2021-12-11"
  j <- "full"
  j_is_champ <- FALSE
  fps <- 2
  fps <- 60
  
  race_video("2021-12-18", "full",FALSE,fps)
  race_video("2021-12-18", "int",FALSE,fps)
  
  race_video("2021-12-11", "full",FALSE,fps)
  race_video("2021-12-11", "int",FALSE,fps)

}
