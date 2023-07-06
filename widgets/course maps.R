
source("R/req_packages.R")
dummy <- lapply(list.files("R", full.names = TRUE), source)

library(leaflet.extras2)

list_icons <- awesomeIconList(
  swimmer = makeAwesomeIcon(text = fa("person-swimming"),markerColor = "blue"),
  traffic = makeAwesomeIcon(text = fa("traffic-light"), markerColor = "black"),
  marshall = makeAwesomeIcon(text = fa("circle-user"), markerColor = "pink"),
  timer = makeAwesomeIcon(text = fa("stopwatch"), markerColor = "black"),
  circle = makeAwesomeIcon(text = fa("circle-xmark"), markerColor = "black", iconColor = "yellow")
)


sf_courses <- st_read("data_provided/courses/shp/courses.shp")
sf_poi <- st_read("data_provided/courses/shp/poi.shp")

sf_use <- sf_courses[with(sf_courses, which(version == "trac" & distance=="full" & part =="ride")),]


buffer_dist <- 1000
units(buffer_dist) <- "m"
sf_bbox_use <- sf_use |> 
  st_buffer(dist = 10000) |>
  st_bbox()

bbox_use <- as.list(sf_bbox_use)
names(bbox_use) <- names(sf_bbox_use)

l_ride <- leaflet(width = "100%", height = 400) |> 
  setMaxBounds(lng1 = bbox_use$xmin,lat1 = bbox_use$ymin, lng2 = bbox_use$xmax,lat2 = bbox_use$ymax) |>
  addTiles(group = "Detailed") |>
  addProviderTiles(providers$CartoDB.Positron, group = "Simple") |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
  
  
  addPolylines(data = sf_use,
               col = tri_cols$ride,
               group = "Ride") %>%
  # addPolylines(data = sf_courses[with(sf_courses, which(version == "trac" & distance=="full" & part =="run")),], col = tri_cols$run, group = "Run") |>
  
  addAwesomeMarkers(data = sf_poi,
                    icon =  ~list_icons[type],
                    # popup = ~name,
                    label =~name,
                    group = "Info") |>
  
  leaflet.extras2::addMovingMarker(data = sf_use,duration = 20*1000, label = "Rider",
                                   # icon = makeIcon(iconUrl = "data_provided/icons/icons8-bike-100.png",
                                   # icon = makeIcon(iconUrl = "/Users/joshuahorsley/Desktop/tttctimes/data_provided/icons/icons8-bike-100.png",
                                   # icon = makeIcon(iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
                                   icon = makeIcon(iconUrl = "https://www.twintownstriathlon.org.au/wp-content/uploads/2023/07/icons8-bike-100.png",
                                                   # icon = icons(iconUrl = "person-biking-solid.svg",
                                                   # iconRetinaUrl= "data_provided/icons/icons8-bike-100.png",
                                                   iconWidth = 25,iconHeight = 25),
                                   movingOptions = movingMarkerOptions(autostart = TRUE, loop = TRUE)) |> 
  
  addFullscreenControl() |>  
  addResetMapButton() |> 
  addScaleBar(position = "bottomleft") |> 
  # addLegend(position = "topleft",colors = c(tri_cols$ride, tri_cols$run),labels = c("Ride","Run")) %>%
  addLayersControl(position = "topleft",baseGroups = c("Simple","Detailed", "Satellite"),
                   # overlayGroups = c("Ride","Run", "Info"),
                   options = layersControlOptions(collapsed = TRUE))



path_out <- "widgets/out/course-full-bike.html"
dir_out <- dirname(path_out)
if(!dir.exists(dir_out)) dir.create(dir_out)
path_files <- gsub(".html$","_files",path_out)

htmlwidgets::saveWidget(l_ride, path_out,selfcontained = TRUE)
unlink(path_files, recursive = TRUE)