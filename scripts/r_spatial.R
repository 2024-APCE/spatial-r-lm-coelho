# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/leono/Desktop/APCE2024/apce2024gis")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot
library(ggplot2)
install.packages("psych")
install.packages("vegan")

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10)) # rep selects the number of colours
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
#each layer of my GQIS project
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area_utm")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
# build map with ggplot and add all the layers
woody_map <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), #color scale details
                       limits=c(0.77, 6.65),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue", linewidth=0.5) + #col, for colour bc is not a poligon
  labs(title = "woody biomass") +
  coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) + #specific coord of my map
  theme(axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location= "bl", width_hint=0.2)
woody_map
  

# plot the rainfall map
rainfall_map <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=pal_zissou1, #color scale details
                       limits=c(300, 2100),
                       oob=squish, #"out of bound"
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue", linewidth=0.5) + #col, for colour bc is not a poligon
  labs(title ="rainfall") +
  coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) + #specific coord of my map
  theme(axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location= "bl", width_hint=0.2)
rainfall_map

# plot the elevation map
elevation_map <- ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=terrain.colors(10), #color scale details
                       limits=c(500, 2100),
                       oob=squish, #"out of bound"
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue", linewidth=0.5) + #col, for colour bc is not a poligon
  labs(title ="elevation") +
  coord_sf(xlimits, ylimits, datum=sf::st_crs(32736)) + #specific coord of my map
  theme(axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location= "bl", width_hint=0.2)
elevation_map


# combine the different maps  into one composite map using the patchwork library
all_maps <- woody_map + elevation_map + rainfall_map +
  patchwork::plot_layout(ncol=1)
all_maps
# and save it to a high resolution png
ggsave("./figures/all_maps.png", width=18, height=18, units="cm", dpi = 300) #it saves in the working directory!!! design in the begining of the script

## CHOOSE TWO MORE VARIABLES


############################
### explore your study area
# set the limits of your study area
xlimits_sa<-sf::st_bbox(studyarea)[c(1,3)]
ylimits_sa<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt

# crop the woody biomass to the extent of the studyarea
woodybiom_sa <- terra::crop(woodybiom, saExt)

# plot the woody biomass
woody_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), #color scale details
                       limits=c(0.77, 6.65),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue", linewidth=0.5) + #col, for colour bc is not a poligon
  labs(title = "woody biomass") +
  coord_sf(xlimits_sa, ylimits_sa, expand=F,
           datum=sf::st_crs(32736)) + #specific coord of my map
  theme(axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location= "bl", width_hint=0.2)
woody_map_sa

#rainfall plot
# plot rainfall map for 30m for the study area
# first you need to increase the raster resolution to 30 m
# Define the extent and resolution for the new raster
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_30m_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
rainfall_30m_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_30m_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(600,1300),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits_sa,ylimits_sa,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_30m_map_sa  

# plot the elevation map
# crop the elevation to the extent of the studyarea
elevation_sa <- terra::crop(elevation, saExt)

elevation_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(10), #color scale details
                       limits=c(500, 2100),
                       oob=squish, #"out of bound"
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue", linewidth=0.5) + #col, for colour bc is not a poligon
  labs(title ="elevation") +
  coord_sf(xlimits_sa, ylimits_sa, datum=sf::st_crs(32736)) + #specific coord of my map
  theme(axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location= "bl", width_hint=0.2)
elevation_map_sa



# make maps also for the other layers that you found
# combine the different maps  into one composite map using the patchwork library
all_maps_sa <- woody_map_sa + elevation_map_sa + rainfall_30m_map_sa +
  patchwork::plot_layout(ncol=1)
all_maps_sa
# and save it to a high resolution png
ggsave("./figures/all_maps_sa.png", width=18, height=18, units="cm", dpi = 300) #it saves in the working directory!!! design in the begining of the script


# plot distance to river
dist2river_sa<-terra::rast("./rivers/DistanceToRiver.tif")
dist2river_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,12000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Distance to river") +
  coord_sf(xlimits_sa,ylimits_sa,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2river_map_sa


# plot valleys
valleys_sa<-terra::rast("./valleys/valleys.tif")
valleys_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(valleys_sa)) +
  scale_fill_manual(values=c("black","white"),
                    labels=c("non-valleys","valleys")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Valleys") +
  coord_sf(xlimits_sa,ylimits_sa,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
valleys_map_sa










###########################
# create 250 random points in your study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")

# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits_sa,ylimits_sa,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa


# combine the maps with patchwork
all_maps_sa<-woody_map_sa + elevation_map_sa + rainfall_30m_map_sa +
  rpoints_map_sa +
  patchwork::plot_layout(ncol=4)
all_maps_sa
ggsave("./figures/all_maps_sa.png", width = 297, height = 210, units = "mm",dpi=300)


# extract your the values of the different raster layers to the points
# Extract raster values at the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points

dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points

elevation_points <- terra::extract(elevation_sa, rpoints) |> 
  as_tibble() 
elevation_points

rainfall_points <- terra::extract(rainfall_30m_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points

valleys_points <- terra::extract(valleys_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(valleys=remapped)
valleys_points


# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],rainfall_points[,2],
                 valleys_points[,2],
                 woody_points[,2]) |>
  as_tibble()
pointdata

pointdata <- pointdata[complete.cases(pointdata),]
pointdata

# plot how woody cover is predicted by different variables
# Create a correlation panel plot
# slope reflects the correlation significance
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)



# make long format
# ggplot prefers long data
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:valleys, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 



# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)



# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", #pch=pointdata$CorProtAr+1, col = pointdata$hills+1,
       bg = "blue", cex = 1)

# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")

#in this case woody is more explian by PC2
#the majority of PC1 is explain by elevation


