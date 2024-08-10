
source("R/req_packages.R")
dummy <- lapply(list.files("R", full.names = TRUE), source)

library(leaflet.extras2)


# Icons -------------------------------------------------------------------


list_icons <- awesomeIconList(
  swimmer = makeAwesomeIcon(text = fa("person-swimming"),markerColor = "blue"),
  traffic = makeAwesomeIcon(text = fa("traffic-light"), markerColor = "black"),
  marshal = makeAwesomeIcon(text = fa("circle-user"), markerColor = "pink"),
  timer = makeAwesomeIcon(text = fa("stopwatch"), markerColor = "black"),
  circle = makeAwesomeIcon(text = fa("circle-xmark"), markerColor = "black", iconColor = "yellow")
)

list_icons_athlete <- list(ride = "https://www.twintownstriathlon.org.au/wp-content/uploads/2023/07/icons8-bike-100.png",
                           run = "https://www.twintownstriathlon.org.au/wp-content/uploads/2023/07/icons8-run-100.png")


# Items to map ------------------------------------------------------------


sf_courses <- st_read("data_provided/courses/shp/courses.shp", quiet = TRUE)
sf_poi <- st_read("data_provided/courses/shp/poi.shp", quiet = TRUE)

sf_sprint_run2 <- st_read("data_provided/courses/shp/trac2sprintrun.shp") |> 
  st_set_crs(st_crs(sf_courses))
sf_sprint_run2$version <- "trac2"
sf_sprint_run2$part <- "run"
sf_sprint_run2$distance <- "sprint"
sf_sprint_run2$id <- NA

sf_tempta_ride2 <- st_read("data_provided/courses/shp/trac2temptaride.shp")# |> 
# st_make_valid() # animated marker was making extra step to maximum y value at end - this fixed it

sf_courses <- rbind(sf_courses,
                    sf_sprint_run2,
                    sf_tempta_ride2
)

sf_courses$distance[sf_courses$distance=="full"] <- "sprint"
sf_courses$distance[sf_courses$distance=="int"] <- "tempta"




# sf_poi <- sf_poi[sf_poi$name != "Marshall: Cnr Oxley & Cunningham (corner of Netball Courts)",]
sf_poi <- sf_poi[!(sf_poi$name %in% c("Marshal: RAB Vintage Lakes & Stradbroke roundabout (SW corner)", 
                                      "Marshal: Cnr Oxley & Cunningham (corner of Netball Courts)")),]


# Random text for unique file names ---------------------------------------


# wordpress keeps old version cached and does not overwrite them
v_text <- 7

# Functions ---------------------------------------------------------------


make_map <- function(sf_course,
                     sf_poi,
                     i_part) {
  
  # browser()
  
  buffer_dist <- 10000
  units(buffer_dist) <- "m"
  sf_bbox_use <- sf_use |> 
    st_buffer(dist = buffer_dist) |>
    st_bbox()
  
  bbox_use <- as.list(sf_bbox_use)
  names(bbox_use) <- names(sf_bbox_use)
  
  
  leaflet(width = "100%", height = 400) |> 
    setMaxBounds(lng1 = bbox_use$xmin,lat1 = bbox_use$ymin, lng2 = bbox_use$xmax,lat2 = bbox_use$ymax) |>
    addTiles(group = "Detailed") |>
    addProviderTiles(providers$CartoDB.Positron, group = "Simple") |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    
    
    addPolylines(data = sf_use,
                 col = tri_cols[[i_part]],
                 group = "Course") |> 
    
    addAwesomeMarkers(data = sf_poi,
                      icon =  ~list_icons[type],
                      label =~name,
                      group = "Info") |>
    
    leaflet.extras2::addMovingMarker(data = sf_use,
                                     duration = 20*1000,
                                     icon = makeIcon(iconUrl = list_icons_athlete[[i_part]],
                                                     iconWidth = 25,
                                                     iconHeight = 25),
                                     movingOptions = movingMarkerOptions(autostart = TRUE,
                                                                         loop = TRUE)) |> 
    
    addFullscreenControl() |>  
    addResetMapButton() |> 
    addScaleBar(position = "bottomleft") |> 
    addLayersControl(position = "topleft",
                     baseGroups = c("Simple","Detailed", "Satellite"),
                     options = layersControlOptions(collapsed = TRUE))
  
}


save_map <- function(l_out, course, part, version,v_text="") {
  
  path_out <- paste0("widgets/out/course-",course,"-",version,"-",part,"_",v_text,".html")
  dir_out <- dirname(path_out)
  if(!dir.exists(dir_out)) dir.create(dir_out)
  
  path_files <- gsub(".html$","_files",path_out)
  
  htmlwidgets::saveWidget(l_out, path_out,selfcontained = TRUE)
  unlink(path_files, recursive = TRUE)
  
  
}


which_poi <- function(sf_course, sf_poi_all, buffer_m = 20) {
  buffer_dist <- 20
  units(buffer_dist) <- "m"
  
  ind_poi_include <- st_contains(
    sf_use |> 
      st_buffer(dist = buffer_dist) |> 
      st_bbox() |> 
      st_as_sfc()
    ,
    sf_poi
  ) |> 
    unlist()
  
  sf_poi_all[ind_poi_include,]
} 


new_course_names <- list(full = "sprint",
                         int = "tempta")



# Generate ----------------------------------------------------------------


for(i_row in seq(nrow(sf_courses))) {
  
  sf_use <- sf_courses[i_row,]
  
  sf_poi_use <- which_poi(sf_use, sf_poi)
  
  i_map <- make_map(sf_use,
                    sf_poi_use,
                    sf_use$part)
  
  save_map(i_map, sf_use$distance, sf_use$part, sf_use$version, v_text)
  
}


# Marshal map -------------------------------------------------------------


make_map_marshal <- function() {
  
  buffer_dist <- 10000
  units(buffer_dist) <- "m"
  sf_bbox_use <- sf_poi |> 
    st_buffer(dist = buffer_dist) |>
    st_bbox()
  
  bbox_use <- as.list(sf_bbox_use)
  names(bbox_use) <- names(sf_bbox_use)
  
  leaflet(width = "100%", height = 400) |> 
    setMaxBounds(lng1 = bbox_use$xmin,lat1 = bbox_use$ymin, lng2 = bbox_use$xmax,lat2 = bbox_use$ymax) |>
    addTiles(group = "Detailed") |>
    addProviderTiles(providers$CartoDB.Positron, group = "Simple") |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    
    addAwesomeMarkers(data = sf_poi,
                      icon =  ~list_icons[type],
                      label =~name,
                      group = "Info") |>
    
    
    addFullscreenControl() |>  
    addResetMapButton() |> 
    addScaleBar(position = "bottomleft") |> 
    addLayersControl(position = "topleft",
                     baseGroups = c("Simple","Detailed", "Satellite"),
                     options = layersControlOptions(collapsed = TRUE))
  
}

l_marshals <- make_map_marshal()

save_map(l_marshals, "marshal","all","trac2",v_text)
