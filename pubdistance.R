library(sf)
library(sqldf)
library(dplyr)
library(ggmap)
library(rgdal)
library(ggvoronoi)
library(tidyr)
library(rworldmap)


setwd("C:/Users/Erin/Documents/DataViz/Pubs")

#create a grid of points
points <- NULL
for (y in seq(49, 61, .0625)) {
  for (x in seq(-10, 2, .125)){
    points <- rbind(points, data.frame(x, y))
  }
}

#categorize pts and pubs by island
pubs <- read.csv("allpubs.csv", stringsAsFactors = FALSE)
pubs$island <- "britain"
pubs$island[pubs$region %in% c('ireland', 'northireland')] <- "ireland"
pubs$island[pubs$region == 'isleofman'] <- "isleofman"

#get shapefiles
world <- getMap(resolution = "high")
ire <- world[world@data$ADMIN == 'Ireland', ] %>% st_as_sf %>%  st_transform(27700)    
uk <- world[world@data$ADMIN == 'United Kingdom', ] %>% st_as_sf %>%  st_transform(27700)   
im <- world[world@data$ADMIN == 'Isle of Man', ] %>% st_as_sf %>%  st_transform(27700)   

#northern ireland from here: http://osni-spatial-ni.opendata.arcgis.com/datasets/d9dfdaf77847401e81efc9471dcd09e1_0.zip
nire <- read_sf("C:/Users/Erin/Documents/DataViz/Pubs/NI", "OSNI_Open_Data_Largescale_Boundaries__NI_Outline")
nire <-   sf::st_simplify(nire, preserveTopology = TRUE, dTolerance = .005) %>%  st_transform(27700)   

#transform everything to same projection
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(points), 
                                     function(i) {st_point(as.numeric(points[i, ]))}), list("crs" = 4326))) 
pnts_trans <- st_transform(pnts_sf, 27700) 

# intersect to see if point is in polygon
points$ire <- apply(st_intersects(ire, pnts_trans, sparse = FALSE), 2, 
                    function(col) {ire[which(col), ]$NAME_FORMA})
points$uk <- apply(st_intersects(uk, pnts_trans, sparse = FALSE), 2, 
                    function(col) {uk[which(col), ]$NAME_FORMA})
points$nire <- apply(st_intersects(nire, pnts_trans, sparse = FALSE), 2, 
                   function(col) {nire[which(col), ]$NAME})
points$im <- apply(st_intersects(im, pnts_trans, sparse = FALSE), 2, 
                     function(col) {im[which(col), ]$NAME_FORMA})

irelandpts  <- unnest(points, ire)
irelandpts$ire <- NULL
irelandpts$island <- "ireland"

nirepts  <- unnest(points, nire)
nirepts$nire <- NULL
nirepts$island <- "ireland"

impts  <- unnest(points, im)
impts$im <- NULL
impts$island <- "isleofman"

ukpts  <- unnest(points, uk)
ukpts$uk <- NULL
ukpts$island <- "britain"
ukpts <- anti_join(ukpts, nirepts, by = c("x", "y"))

points <- do.call("rbind", list(irelandpts, nirepts, ukpts, impts))




#-----------find top 5 nearest pubs to each point [as the crow flies]------------
#for each point, find the closest 5 pubs (as the crow flies). 
#I'm assuming one of these will be closest in driving distance--it's overkill to check every single point against every single pub
#not the most beautiful method but it gets the job done
candidates <- sqldf("Select x, y, lat, long, pubs.ID from points P, pubs where lat < y+1 and lat > y-1 and long<x+1 and long>x-1")
pubdistance <- sqldf("Select x, y, ID, sqrt(square(x-long)+square(y-lat)) as D from candidates")
top5 <- pubdistance %>%
  group_by(x, y) %>%
  top_n(n = -5, wt = D)
pubstosearch <- sqldf("Select top5.x, top5.y, pubs.ID, pubs.long, pubs.lat from top5 inner join pubs on pubs.ID = top5.ID")

#use google api to get walking distances between two points
key <- "my api key kere"
register_google(key = key)

pubstosearch$from <- paste0(pubstosearch$y, ",", pubstosearch$x)
pubstosearch$to <- paste0(pubstosearch$lat, ",", pubstosearch$long)
dist <- NULL

for (i in 1:nrow(pubstosearch)){
  tryCatch({
    temp <- mapdist(pubstosearch$from[i], pubstosearch$to[i], "walking")$miles
    dist <- rbind(dist, data.frame(temp, pubstosearch$x[i], pubstosearch$y[i], pubstosearch$ID[i]))
    
    if (i%%50==0){  
      Sys.sleep(1)
      print(i)}
  }, error=function(e){})
}

colnames(dist) <- c("D", "x", "y", "ID")
distfinal <- dist %>%
  group_by(x, y) %>%
  top_n(n = -1, wt = D)
distfinal <- sqldf("Select D, x, y, min(ID) as ID from distfinal group by D, x, y")

#write.csv(distfinal, "distfinal.csv", row.names = FALSE)



#-----------------round2: double check extra long distances-----
#This time, only look at pubs that are on the same island as the point
checkdist <- head(arrange(distfinal,desc(D)), n = 500)
checkdist <- sqldf("select C.*, P.island from checkdist C inner join points P on C.x = P.x and C.y = P.y")
candidates <- sqldf("Select x, y, lat, long, pubs.ID, pubs.island from checkdist P, pubs 
                        where lat < y+2.5 and lat > y-2.5 
                          and long<x+2.5 and long>x-2.5
                          and P.island = pubs.island")
pubdistance <- sqldf("Select x, y, ID, sqrt(square(x-long)+square(y-lat)) as D from candidates")

top15 <- pubdistance %>%
  group_by(x, y) %>%
  top_n(n = -15, wt = D)

pubstosearch <- sqldf("Select top15.x, top15.y, pubs.ID, pubs.long, pubs.lat from top15 inner join pubs on pubs.ID = top15.ID")

#then check distances, same as before
pubstosearch$from <- paste0(pubstosearch$y, ",", pubstosearch$x)
pubstosearch$to <- paste0(pubstosearch$lat, ",", pubstosearch$long)
newdist <- NULL

for (i in 1:nrow(pubstosearch)){
  tryCatch({
    temp <- mapdist(pubstosearch$from[i], pubstosearch$to[i], "walking")$miles
    newdist <- rbind(newdist, data.frame(temp, pubstosearch$x[i], pubstosearch$y[i], pubstosearch$ID[i]))
    
    if (i%%50==0){  
      Sys.sleep(1)
      print(i)}
  }, error=function(e){})
}

colnames(newdist) <- c("D", "x", "y", "ID")
distfinal <- rbind(newdist, distfinal)
distfinal <- distfinal %>%
  group_by(x, y) %>%
  top_n(n = -1, wt = D)
distfinal <- sqldf("Select x, y, min(ID) as ID, D from distfinal group by D, x, y")

#write.csv(distfinal, "distfinal_updated.csv")



#------------------voroni diagram----------------------
#distfinal <- read.csv('distfinal_updated.csv')
proj <- proj <- "+init=epsg:27700"

world <- getMap(resolution = "high")

ire <- world[world@data$ADMIN == 'Ireland', ] %>% spTransform(CRS(proj)) %>% fortify()
uk <- world[world@data$ADMIN == 'United Kingdom', ] %>% spTransform(CRS(proj)) %>% fortify()
im <- world[world@data$ADMIN == 'Isle of Man', ]  %>% spTransform(CRS(proj)) %>% fortify()

#csv file of cities to plot.
ptsofinterest <- read.csv("ptsofinterest.csv")
coordinates(ptsofinterest) <- ~ x + y
proj4string(ptsofinterest) =CRS("+proj=longlat")
ptsofinterest <- spTransform(ptsofinterest, CRS(proj))
ptsofinterest <- as.data.frame(ptsofinterest)

coordinates(distfinal) <- ~ x + y
proj4string(distfinal) =CRS("+proj=longlat")
distfinal <- spTransform(distfinal, CRS(proj))
distfinal <- as.data.frame(distfinal)
distfinal$plotval <- distfinal$D

distfinal$plotval[distfinal$D == 0] <- 51

#makes voroni diagrams. Trimming them to coastlines takes a long time
vor_uk <- voronoi_polygon(data=distfinal,x="x",y="y", outline = uk)
vor_uk <- fortify_voronoi(vor_uk)
vor_ire <- voronoi_polygon(data=distfinal,x="x",y="y", outline = ire)
vor_ire <- fortify_voronoi(vor_ire)
vor_im <- voronoi_polygon(data=distfinal,x="x",y="y", outline = im)
vor_im <- fortify_voronoi(vor_im)

blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

plotcolors <- c("#fff7fb", '#ece2f0', '#d0d1e6',
                '#a6bddb', '#67a9cf', '#3690c0',
                '#02818a', '#016c59', '#014636')

ggplot() +  coord_equal() + blankbg+
  geom_polygon(data = vor_uk, aes(x=x,y=y,group=group, fill = plotval)) +
  geom_polygon(data = vor_ire, aes(x=x,y=y,group=group, fill = plotval)) +
  geom_polygon(data = vor_im, aes(x=x,y=y,group=group, fill = plotval)) +
  geom_polygon(data = ire, mapping = aes(long, lat, group = group), color = "#000000", size = .25, fill =NA) +
  geom_polygon(data = im, mapping = aes(long, lat, group = group), color = "#000000", size = .25, fill =NA) +
  geom_polygon(data = uk, mapping = aes(long, lat, group = group), color = "#000000", size = .25, fill =NA) +
  scale_fill_gradientn(colors = plotcolors,guide=FALSE) +
  geom_point(aes(x = x, y = y), data = ptsofinterest, size = 1)

ggsave("./plots/distance.png", plot = last_plot(),
       scale = 1, width = 7, height = 9, units = "in",
       dpi = 500)


