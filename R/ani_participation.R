
participation_video <- function(i_season, fps, duration, scaling = 2L, summary_offset = 25L) {
  
  
  len_season <- max(dt_season[season==i_season, race_number])

  dt_entries_race_prep <- dt_all_long[season==i_season & part=="Swim",
                                 .(entries = .N,
                                   firstRace = sum(isFirstRace),
                                   date_ymd = date_ymd[1]),
                                 by = .(race_number)]
  
  dt_entries_race <- dt_season[season==i_season,
                               .(date_ymd = unique(date_ymd),
                                 cancelled = unique(cancelled),
                                 scheduled = unique(scheduled),
                                 have_results = unique(have_results),
                                 cancelled_reason = unique(cancelled_reason)),
                               by = race_number]
  
  new_cols <- setdiff(names(dt_entries_race_prep), "race_number")
  
  dt_entries_race[dt_entries_race_prep, on = .(race_number),  (new_cols) := mget(new_cols) ]
  dt_entries_race[is.na(cancelled), cancelled := FALSE]
  
  dt_entries_race[, cancellations_cumulative := cumsum(cancelled)]
  
  
  setorder(dt_entries_race, race_number)
  setcolorder(dt_entries_race, c("race_number","date_ymd","entries"))
  
  max_entries <- max(dt_entries_race$entries, na.rm=TRUE)
  
  
  dt_entries_race[(cancelled), `:=`(entries = 0, firstRace = 0)]
  dt_entries_race[!(scheduled), `:=`(entries = 0, firstRace = 0)]
  dt_entries_race[, entries_cumulative := cumsum(entries)]
  dt_entries_race[, athletes_cumulative := cumsum(firstRace)]
  
  dt_entries_race[cancelled_reason=="COVID", emo := emoji("mask")]
  dt_entries_race[cancelled_reason=="XMAS", emo := emoji("christmas_tree")]
  dt_entries_race[cancelled_reason=="New Years", emo := emoji("clinking_glasses")]

  dt_entries_race[(have_results), entries_label := entries]
  
  dt_entries_race[, summary_text := paste0("As of Race ", race_number," - ",
                                           as.integer(format(date_ymd,'%d')), " ", 
                                           format(date_ymd,'%b %Y'),"\n",
                                           "Entries so far: ", entries_cumulative, "\n",
                                           "Competitors so far: ", athletes_cumulative, "\n",
                                           "Cancelled Races so far: ", cancellations_cumulative)]

  
  # Define animation stages
  
  stage_max <- dt_entries_race[(have_results), max(race_number)]
  
  dt_prep_new <- dt_entries_race[rep(1:len_season, stage_max+1)]
  
  dt_prep_new[, stage_overall := rep(0:stage_max, each = len_season)]
  
  dt_prep_new[race_number > stage_overall, `:=`(emo = NA, entries = 0, entries_label = NA)]
  dt_prep_new[race_number != stage_overall, summary_text := NA]
  
  dt_prep_new[, emo_pos := 6 - 3*(seq(race_number) %% 2)]

  
  g <- ggplot(dt_prep_new,
              aes(y = entries, x = race_number, group = race_number, label = emo)) +
    # geom_col(orientation = "x", width = 0.9, size = 0.3, col = c(tri_cols$club_1, tri_cols$club_2, tri_cols$club_3)) +
    geom_col(orientation = "x", width = 0.9, size = 0.3) +
    geom_text(family="EmojiOne", size = 6, aes(y = emo_pos)) +
    geom_text(aes(y = entries + 2, x =race_number, group = race_number, label = entries_label), size = 6) +
    geom_text(aes(label = summary_text), y = max_entries + summary_offset + 1, x = -1, hjust = "left", vjust = "top",size = 6) +
    ggtitle(paste0(i_season, " Entries")) + 
    scale_y_continuous("Entries",limits = c(0, max_entries + summary_offset)) +
    theme_minimal() +
    theme(legend.position="top",
          strip.background = element_rect(colour="black",
                                          fill="white"),
          plot.title = element_text(size=20))
  
  g <- myscale_x_racenumber(g, len_season)
  
  
  # apply animation
  g <- g + transition_states(stage_overall, transition_length = 1, state_length = 1, wrap = FALSE)
  
  
  if(missing(duration)) duration <- (stage_max + 1)
  
  g_an <- animate(g, duration = duration, fps = fps,
                  height = scaling*812, width=scaling*375, res = scaling*100, device = "png",
                  renderer = ffmpeg_renderer(format = "mp4",
                                             options = list(codec="libx264",
                                                            pix_fmt ="yuv420p",
                                                            vf = 'pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2"'))) # avoid error: [libx264 @ 0x7fe29e811c00] height not divisible by 2 (700x869)
  
  
  path_out <- file.path("video",paste0("participation_", i_season,".mp4"))
  
  anim_save(path_out, g_an) 

}

if(FALSE) {
  source("R/req_packages.R")
  source("R/utils.R")
  source("R/tricols.R")
  
  
  
  dt_season <- readRDS("data_derived/dt_season.rds")
  dt_all_long <- readRDS("data_derived/dt_all_long.rds")
  
  
  i_season <- "2021-2022"
  
  participation_video(i_season, 60)
  
  
}