##//The Regional Ecosystem Soil Hydraulics Project
##// ReESH Database introduction and simple functions
##//Downloading, compiling, and analyzing soil hydraulic data

##//Required packages
library(tidyverse)
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")
##//Before beginning, download the data from Zenodo data base (or google drive)
##  unpack folders and place into an accessable directory

##  link to data

##// Set your root local directory
##//  <<Change this to your target directory>>

root_dir <- "D:/Dropbox/Projects/Indiana/SoilLab/Zenodo_DataBaseFiles"

##//View list of ReESH Sites included in the current version of the database
(ReESHSites <- dir(root_dir, pattern = "-"))

##//Recursively find all Soil Characteristics files in the root directory and subdirectories
SoilCharacter_files <- list.files(path = root_dir, pattern = "_SoilCharacteristics.csv", 
                        recursive = TRUE, full.names = TRUE)

##//Read and combine all CSVs into a single data frame
Soil_df <- SoilCharacter_files %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows()  # Adds a column to identify source file (optional)


##//You can save the compiled dataframe as a CSV if you would like to work with Excel or other software
# output_dir <- "D:/Dropbox/Projects/Indiana/SoilLab"  ##// update with you directory
# write.csv(Soil_df, paste(output_dir, "SoilCharactersCompiled.csv", sep = "/"), row.names = FALSE)

##//Simple map of the data sets
library(mapproj)
library(ggthemes)

##//Map data
states <- map_data("state")

##//Base map
gg <- ggplot() + 
   geom_map(data=states, map=states,
                    aes(x=long, y=lat, map_id=region),
                    color="grey", fill="white", size=1) +
  coord_map("albers", lat0=30, lat1=40) +
  theme_map()
gg


MapOut <- gg + 
  geom_point(data = Soil_df, aes(x = Longitude, y = Latitute, color = Site),
                          alpha = 1, size = 7, shape = 13, stroke = 2,
                          show.legend = TRUE, position = position_dodge(1)) + 
  theme(legend.position="bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=10))

MapOut

##//Simple and clean box plots
##//Plot bulk density and Ksat for different veg types

##//Plot 1
ggplot() +
  geom_boxplot(data = Soil_df,
               aes(x = Veg, y = Bulk_Den_g_m3), color = "black", 
               show.legend = TRUE,  size = 1, alpha = 0.4, 
               notch = F, notchwidth = 0.5) +
  ylab(expression(paste("Bulk Density [g m-3]" ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.7))

##//Plot 2
##//Add a filter to subset and use only best K Sat data
Subs <- Soil_df$KS_Soil_Flag==0
ggplot() +
  geom_boxplot(data = Soil_df[Subs,],
               aes(x = Veg, y = Med_KsSoil_m_s), color = "black", 
               show.legend = TRUE,  size = 1, alpha = 0.4, 
               notch = F, notchwidth = 0.5) +
  ylab(expression(paste("Saturated Cond. [m s-1]" ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.7))


#//Plot bulk density and ksat for different Veg
##//Plot 3
ggplot() +
  geom_point(data = Soil_df[Subs,],
               aes(x = Bulk_Den_g_m3, y = Med_KsSoil_m_s, color = Veg),
               show.legend = TRUE,  size = 3, alpha = 1) +
  geom_smooth(data = Soil_df[Subs,],
              aes(x = Bulk_Den_g_m3, y = Med_KsSoil_m_s, 
                  color = Veg, fill = Veg), show.legend = FALSE,
              size = 2, alpha = 0.5, method = "lm", se = FALSE)+
  ylab(expression(paste("Saturated Cond. [m s-1]" ))) +
  xlab(expression(paste("Bulk Density [g m-3]" ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.7))

#//Plot bulk density and ksat for different Depth_cm
##//Making depths a factor 
Soil_df$DepthFactor <- ifelse(Soil_df$Depth_cm>10, "Deep", Soil_df$Depth_cm)
Soil_df$DepthFactor <- as.factor(Soil_df$DepthFactor)

##//Plot 4
ggplot() +
  geom_point(data = Soil_df[Subs,],
             aes(x = Bulk_Den_g_m3, y = Med_KsSoil_m_s, color = DepthFactor),
             show.legend = TRUE,  size = 3, alpha = 1) +
  geom_smooth(data = Soil_df[Subs,],
              aes(x = Bulk_Den_g_m3, y = Med_KsSoil_m_s, 
                  color = DepthFactor, fill = DepthFactor),
              size = 2, alpha = 0.5, method = "lm", se = FALSE)+
  ylab(expression(paste("Saturated Cond. [m s-1]" ))) +
  xlab(expression(paste("Bulk Density [g m-3]" ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.7))

##// Make a couple of plots based on your own interest
##//Feel free to play with the data in other software if more comfortable

##//Plot 5 





##//Plot 6





##//Playing with soil water retention curve data
##//Recursively find all Soil Characteristics files in the root directory and subdirectories
RetentionCurve_files <- list.files(path = root_dir, pattern = "_SoilWaterRetentionCurves.csv", 
                                  recursive = TRUE, full.names = TRUE)

##//Read and combine all CSVs into a single data frame
SWRC_df <- RetentionCurve_files %>%
  lapply(read.csv, stringsAsFactors = FALSE)

##//Update the plots to being factors not integers 
for(i in 1:length(SWRC_df)){ 
  SWRC_df[[i]]$Plot <- as.factor(SWRC_df[[i]]$Plot)
  }

##//Combine the lists into a single dataframe
SWRC_df <- bind_rows(SWRC_df)

##//You can save the compiled dataframe as a CSV if you would like to work with Excel or other software
# output_dir <- "D:/Dropbox/Projects/Indiana/SoilLab"  ##// update with you directory
# write.csv(SWRC_df, paste(output_dir, "SWRC_Compiled.csv", sep = "/"), row.names = FALSE)

##//Making depths a factor 
SWRC_df$DepthFactor <- ifelse(SWRC_df$Depth_cm>10, "Deep", SWRC_df$Depth_cm)
SWRC_df$DepthFactor <- as.factor(SWRC_df$DepthFactor)

##//Simple plots with soil water retention curves

##//Plot 7
ggplot() +
  geom_point(data = SWRC_df,
             aes(x = Vol_Water, y = log10(MPa_Abs), color = DepthFactor),
             show.legend = TRUE,  size = 3, alpha = 1) +
  ylab(expression(paste("Log10 Soil Water Potential [ MPa] " ))) +
  xlab(expression(paste("Soil Volumetric Water Content [ % ] " ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.7))

##//Plot 8
ggplot() +
  geom_point(data = SWRC_df,
             aes(x = Vol_Water, y = log10(MPa_Abs), color = Site),
             show.legend = TRUE,  size = 3, alpha = 1) +
  ylab(expression(paste("Log10 Soil Water Potential [ MPa] " ))) +
  xlab(expression(paste("Soil Volumetric Water Content [ % ] " ))) +
  theme(legend.position="right",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.7))



