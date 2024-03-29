
# Create record pages -----------------------------------------------------


seasons <- rev(unique(dt_season$season))
# seasons <- "2020-2021"

header <- paste0({
"<!-- Do not edit this file -->
<!-- This Rmd file is created using make_seasons_rmd.R -->\n\n"
})

  body <- foreach(i_season = seasons, .combine = paste0) %do% {
    
    course_avail_season <- unique(dt_all_long[season == i_season]$course)
    course_avail_season <- sort(ordered(course_avail_season, c("full", "int", "double")))
    
    race_numbers <- sort(unique(dt_all_long[season==i_season]$race_number))
    
    any_entries <- i_season %in% unique(dt_all_long$season)
    
    len_season <- max(dt_season[season==i_season]$race_number)
    
  
    paste0('# Season ', i_season, ' {#season-',i_season,'}\n\n',
           
           'Find race results from the schedule below. Or see:',
           '\n',
           
           {if(any_entries) {
             
             paste0("\nParticipation by:"
                    ,"\n\n- [Total entries](#participation-total-",i_season,")",
                    "\n- [Race and course](#participation-trends-",i_season,")",
                    
                    "\n\nSeason course records:\n",
                    {if("full" %in% course_avail_season) paste0('\n- [Full](#best-times-full-',i_season,")")},
                    {if("int" %in% course_avail_season) paste0('\n- [Intermediate](#best-times-int-',i_season,")")},
                    {if("double" %in% course_avail_season) paste0('\n- [Double Distance](#best-times-double-',i_season,")")}
                    )
             
             
           }},
           
           
         
'\n\n#### Schedule {#schedule-',i_season,'}

```{r schedule-',i_season,'}

dt_season_show <- dt_season[season=="',i_season,'"][(course=="full" | race_type == "Super Teams") & !(is_champ),
                          .(`#` = race_number,
                            # Date = date_ymd,
                            `Event type` = race_type,
                            cancelled,
                            scheduled,
                            cancelled_reason,
                            have_results,
                            has_report,
                            participation_only,
                            Date = race_link,
                            `Club Report` = report_link)]


dt_season_show[, Note := ""]
dt_season_show[`Event type` != "Standard", Note := `Event type`]
dt_season_show[(participation_only), Note:= paste0(Note, ifelse(Note=="",""," - "),"Participation results only ")]
dt_season_show[(cancelled), Note:= paste0(Note, ifelse(Note=="",""," - "),"Cancelled due to ", cancelled_reason)]
dt_season_show[!(scheduled), Note:= paste0(Note, ifelse(Note=="",""," - "),"No race due to ", cancelled_reason)]
dt_season_show[(scheduled) & !(cancelled) & !(have_results), Note := paste0(Note,ifelse(Note=="",""," - "), "Results not available")]


col_ref_hide <- which(!(names(dt_season_show) %in% c("#","Date","Note","Club Report")))-1 # columns are indexed from 0 - row name?

datatable_std(dt_season_show,col_ref_hide,escape = FALSE,ordering = FALSE)
```

', {if(any_entries) {paste0('## Participation ',i_season,' - Total {#participation-total-',i_season,'}

```{r}
n_entries_all_season <- nrow(dt_all_long[season=="',i_season,'"][part == "Swim"])
n_athletes_season <- length(unique(dt_all_long[season=="',i_season,'"][part == "Swim"]$Name))

```

There have been `r prettyNum(n_entries_all_season, big.mark = ",")` entries by `r n_athletes_season` competitors in the ',i_season,' season

```{r part_hist-',i_season,'}
plotly_part_hist(dt_all_long[season=="',i_season,'"],len_season=',len_season,')
```

```{r part-total-table-',i_season,'}
table_part_total("',i_season,'")
```

## Participation ',i_season,' - Trends {#participation-trends-',i_season,'}


### By Race

```{r course_totals_',i_season,'}
dt_entry_type <- dt_all_long[season=="',i_season,'"][part == "Swim", .(count = .N), by = course_nice]
setorder(dt_entry_type, course_nice)
dt_entry_type[, text := paste0(count, " for ", course_nice)]
```

The total number of entries by course are `r list_with_and(dt_entry_type$text)`. The plot below shows the number of entries by course over the ',i_season,' season. Race cancellations are shown on the plot with the letter \"C\". 

```{r plot-race-count-',i_season,'}
plot_race_count(dt_all_long,"',i_season,'",len_season=',len_season,')
```

',{if(FALSE) {paste0('

### By Competitor


This plot shows the cumulative number of races entered by each competitor over the ',i_season,' season. It\'s a bit crowded showing everyone, each competitor\'s entries can be toggled by clicking (or double-clicking) their names in the legend.

```{r plot-race-series-',i_season,'}
plotly_time_series(dt_all_long[season=="',i_season,'"],len_season=',len_season,')
```

')}},

  
  
  
  foreach(j = course_avail_season, .combine = paste0 ) %do% {
    
    
    j_course_nice <- c("Double Distance","Full","Intermediate")[which(j==c("double","full","int"))]
    
    
    
    paste0("\n## Best Times ",i_season," - ", j_course_nice,' {#best-times-',j,'-',i_season,"}",
  "\n\nThe fastest valid overall and split times in the ",i_season," season for each competitor are ranked in the plot and tables below.\n\n",
  "\n\n### Overall\n\n",
  '
```{r plot-record-',j,'-',i_season,'}
plotly_record(dt_all_long[season=="',i_season,'"], tri_cols, "',j,'")
```
\n',
    
    foreach(l = c("overall","Swim","Ride","Run"), .combine = paste0 ) %do%  {
      
      
      paste0(ifelse(l=="overall","",paste0('### ', l)),
             "\n\nSeason records are show in gold, split PBs are shown in pink, and invalid times in grey.",
             if(l!="overall") paste0(" Ranks for other parts and overall are provided only for PBs."),
             if(l=="overall") paste0(" Ranks for individual parts are provided only for PBs."),
'
  
```{r tab-record-',j,'-',l,'-',i_season,'}
table_record(dt_all_long[season=="',i_season,'"], tri_cols, "',j,'", "',l,'")
```
\n')
      
      
      
    }
    
    )
  }

,

foreach (i=rev(race_numbers), .combine = paste0 ) %do% {
  
  
  n_athletes <- nrow(dt_all_long[season==i_season & part == "Swim" & race_number == i])
  
  n_new_athletes <- nrow(dt_all_long[(isFirstRace )& season == i_season & part == "Swim" & race_number == i])
  text_new_athletes <- if(i==1 | n_new_athletes == 0) {
    ""
  } else {
    paste0(", ", n_new_athletes, " for the first time this season")
  }
  
  i_date_file <- format(dt_season[season==i_season  & race_number == i]$date_ymd[1], "%Y-%m-%d")
  i_date <- format(dt_season[season==i_season  & race_number == i]$date_ymd[1], "%d %b %Y")
  
  i_race_type <- dt_season[season==i_season  & race_number == i]$race_type[1]
  
  # Available course results - place club championship or double-distance first
  
  dt_season_i <- dt_season[season==i_season  & race_number==i & (have_results)][order(-is_champ,course )]
  
  i_courses <- dt_season_i$course
  i_courses_nice <- dt_season_i$course_nice
  i_is_champ <- dt_season_i$is_champ
  
  j_options <- seq(length(i_courses))
  
  race_report_link <- dt_season[season==i_season  & race_number == i]$report_link_md[1]
  if(is.na(race_report_link)) {
    race_report_link <- ""
  } else {
    race_report_link <- paste0(race_report_link, ".")
  }
  
  race_ref <- dt_season[season==i_season  & race_number == i]$race_ref[1]
  
  paste0(
    "
",
"## Race ", i,": ", i_date, ifelse(i_race_type=="Standard", "", paste0(" - ",i_race_type))," {#", race_ref, "}\n\n",

"A total of ", n_athletes,  " competitors entered", text_new_athletes, ". ",race_report_link,"\n\n",

foreach (j_counter=j_options, .combine = paste0 ) %do% {
  
  j <- i_courses[j_counter]
  j_is_champ <- i_is_champ[j_counter]
  
  dt_i <- dt_all_long[season==i_season & race_number== i & course == j & j_is_champ == (is_champ)]
  
  n_athletes_j <- length(unique(dt_i[part=="Swim"]$Name))
  
  dt_pb_split_new <- dt_i[(isNewPB_split), .(.N), by = part]
  
  dt_pb_split_new[part=="Swim", `:=`(where="in the pool", ord=1)]
  dt_pb_split_new[part=="Ride", `:=`(where="on the bike", ord=2)]
  dt_pb_split_new[part=="Run", `:=`(where="on the run", ord=3)]
  
  dt_pb_split_new[, phrase := paste0(N, " ", where)]
  
  setorder(dt_pb_split_new, ord)
  
  pb_split_sentence <- list_with_and(dt_pb_split_new$phrase)
  
  n_pb_splits_new <- sum(dt_pb_split_new$N)
  pb_overall_new <- nrow(dt_i[(isNewPB_overall) & part=="Swim"])
  
  part_only_j <- dt_i[1]$participation_only
  
  if(j=="teams"){
    n_pb_splits_new=0
    pb_overall_new=0
  }
  
  
  
  paste0(
    '
### ',i_courses_nice[j_counter],'\n\n',
    
    n_athletes_j,' competitors entered the course',
    if(pb_overall_new!=0 | n_pb_splits_new !=0) ", achieving ",
    list_with_and(
      c(if(pb_overall_new!=0) {paste0(pb_overall_new,' new overall PBs')},
        if(n_pb_splits_new!=0) {
          paste0( n_pb_splits_new, ' new split PBs: ', pb_split_sentence)
        })),
    '.',
    
    '\n\n',
    {if(j!="teams" & !part_only_j){
      
paste0('```{r plot-race-',i,'-',j,'-',j_is_champ,'-',i_season,'}
plotly_race(dt_all_long, tri_cols, "',i_season,'",',i,', "',j,'",',j_is_champ,')
```\n\n')
    }},

{if(part_only_j) {
  "No timing available for this race. Competitors are listed below.\n"
} else {
  
    paste0(i_season ,' season records are show in gold, season PBs are shown in pink, and invalid times in grey. Ranks compare efforts in this race.\n')
}
    
  },
{if(j!="teams" & !part_only_j){
  
  paste0(
'```{r tab-race-',i,'-',j,'-',j_is_champ,'-',i_season,'}
table_race(dt_all_long[season=="',i_season,'"], tri_cols, ',i,', "',j,'",',j_is_champ,')
```

')}},


{if(j!="teams" & part_only_j){
  
  paste0(
    '```{r tab-race-',i,'-',j,'-',j_is_champ,'-',i_season,'}
table_race_part_only(dt_all_long, "',i_season,'", ',i,', "',j,'",',j_is_champ,')
```

')}
  
  
},
  
  {if(j=="teams"){
  # team membership only
  paste0(
    '```{r tab-race-',i,'-',j,'-',j_is_champ,'-',i_season,'}
table_race_teams(dt_all_long[season=="',i_season,'"], tri_cols, ',i,', "',j,'",',j_is_champ,')
```

')
  
}}
  )
  
  
}
  )
  
  
}

)}}
)
}

write(paste0(header, body),file = "02-seasons_generated.Rmd")
