ggplot(dt_i[dt_i_prep==1], aes(y = -place_cum_recalc, x = duration = duration_at_stage, fill = part_plot, col = part_plot, group = place_name)) +
  geom_col(orientation = "y", position = "identity", width = 0.8)


ggplot(dt_i_prep[stage ==1], aes(y = - place_cum_recalc, x = duration_at_stage, fill = part_plot, col = part_plot, group = place_name)) +
  # g <- ggplot(dt_i_prep, aes(y = - place_overall_recalc, x = duration_at_stage, fill = part_plot, col = part_plot,group = group)) +
  geom_col(orientation = "y", position = "identity", width = 0.8) 




dt_test <- data.table(duration_at_stage = c(20,20, 10,10, 15, 15),
           place_cum_recalc = c(1,3,2,1,3,2),
           place_name = c("Josh","Josh","Patrick","Patrick","Other","Other"),
           stage_order = c(1,2,1,2,1,2))[order(stage_order)]


g <- ggplot(dt_test, aes(y = -place_cum_recalc, x = duration_at_stage, fill = place_name, group = place_name)) +
         geom_col(orientation = "y", position = "identity", width = 0.8) +
  labs(title = "Testing position change") +
  transition_states(stage_order, transition_length = 1, wrap = FALSE) +
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title = element_text(size=20))

g_an <- animate(g, duration = 2, fps = fps, start_pause = 5,
                height = 200, width=700, res = 100, device = "png",
                renderer = ffmpeg_renderer(format = "mp4",options = list(codec="libx264", pix_fmt ="yuv420p")))

anim_save("video/test.mp4", g_an)
