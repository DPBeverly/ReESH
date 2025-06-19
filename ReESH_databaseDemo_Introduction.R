##//The Regional Ecosystem Soil Hydraulics Project
##// ReESH Database introduction and simple functions
##//Downloading, compiling, and analyzing soil hydraulic data

##//Required packages
library(tidyverse)

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


##//Simple map of the data sets

##//Simple and clean box plots
##//Plot bulk density and Ksat for different veg types

##//

##//

#//Plot bulk density and ksat for different depths

##//

##//


##// Make a couple of plots based on your own interest




str(Soil_df)

cc<-Soil_df$KS_Soil_Flag==0
plot(as.factor(Soil_df$Veg[cc]), Soil_df$Bulk_Den_g_m3[cc])
plot(as.factor(Soil_df$Veg[cc]), Soil_df$Med_KsSoil_m_s[cc])

plot(as.factor(Soil_df$Depth_cm[cc]), Soil_df$Med_KsSoil_m_s[cc])
plot(as.factor(Soil_df$Depth_cm[cc]), Soil_df$Bulk_Den_g_m3[cc])
plot(as.factor(Soil_df$Depth_cm[cc]), Soil_df$Pct_OM[cc])


plot(Soil_df$CLAY, Soil_df$SAND)

plot(Soil_df$CLAY[cc], Soil_df$Med_KsSoil_m_s[cc])
plot(Soil_df$SAND[cc], Soil_df$Med_KsSoil_m_s[cc])
plot(Soil_df$Pct_OM[cc], Soil_df$Med_KsSoil_m_s[cc])
plot(Soil_df$RootBiomass_g[cc], Soil_df$Med_KsSoil_m_s[cc])

plot(Soil_df$Pct_OM[cc], Soil_df$RootBiomass_g[cc])
plot(Soil_df$Pct_OM[cc], Soil_df$SAND[cc])

plot(Soil_df$SAND, Soil_df$Med_KsSoil_m_s)
