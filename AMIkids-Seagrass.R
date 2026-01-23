# setup ------------------------------------------------------------------
# Install if not already installed
# install.packages("readxl", dependencies = TRUE) 
# install.packagess(c('dplyr', 'ggplot2'))
# install.packages('tbeptools', repos = c('https://fawda123.r-universe.dev', 'https://cloud.r-project.org'))
# install.packages("lubridate")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(tbeptools)

# get and read training data
seagrass <- read_xlsx("SeagrassDataOct2025-AMIkids.xlsx")


# learning about raw data
# see first six rows
  head(seagrass)
  
# structure, see all columns
  str(seagrass)

# tidying up, transforming the data - woohoo piping!!
  Seagrass1 <- seagrass %>% 
  # Remove unecessary variables
    select(-Time, -EpiphyteType, -BaySegment, -Month, -yr, -grp, -grpact, -MonitoringAgency, -Depth, -Comments) %>%
  # get average and SD for Blade Length and Shoot Count    
    mutate(AvgBL = (Blade1 + Blade2 + Blade3 + Blade4 + Blade5) / 5) %>% 
    mutate(AvgSht = (Shoot1 + Shoot2 + Shoot3) / 3) %>% 
  # Remove raw root and shoot data    
    select(-Shoot1, -Shoot2, -Shoot3, -Blade1, -Blade2, -Blade3, -Blade4, -Blade5)
# show 1st 6 rows  
  head(Seagrass1) 

#data summary and graph: day, species, and abundance
  mean_AbApp <- Seagrass1 %>%
    group_by(Day, Species) %>%
    summarise(AvgAbu = mean(Abundance), AvgApp = mean(Appearance)) 
  head(mean_AbApp)
  #removing empty rows - no SAV observed  
  NoZeroAb <- mean_AbApp[apply(mean_AbApp!=0, 1, all),] %>% 
    drop_na(Day, Species, AvgAbu)
  head(NoZeroAb)
  #bar chart
  ggplot(NoZeroAb, aes(fill=Species, y=AvgAbu, x=Day)) + 
    geom_bar(stat = "identity", position = position_dodge2(preserve = 'single')) +
    scale_x_continuous(breaks=seq(20,24,by=1)) +
    labs(
      title = "Braun-Blanquet coverage for each day and species", y="Braun-Blanquet Coverage", x="Day in October 2025"
      )
#get mean of means for abundance
  NoZeroesAbu <- Seagrass1 %>%
    group_by(Species) %>%
    summarise(MnAbu = mean(Abundance)) 
  head(NoZeroesAbu)
  
#data summary and graph: day, species, and appearance
  mean_App <- Seagrass1 %>%
    group_by(Day, Species) %>%
    summarise(AvgApp = mean(Appearance)) 
  head(mean_App)
  #removing empty rows - no SAV observed  
  NoZeroApp <- mean_App[apply(mean_App!=0, 1, all),] %>% 
    drop_na(Day, Species, AvgApp)
  head(NoZeroApp)
  #bar chart
  ggplot(NoZeroApp, aes(fill=Species, y=AvgApp, x=Day)) + 
    geom_bar(stat = "identity", position = position_dodge2(preserve = 'single')) +
    scale_x_continuous(breaks=seq(20,24,by=1)) +
    labs(
      title = "Appearance for each day and species", y="Appearance", x="Day in October 2025"
    )
#get mean of means for appearance
  NoZeroesApp <- Seagrass1 %>%
    group_by(Species) %>%
    summarise(MnApp = mean(Appearance)) 
  head(NoZeroesApp)
  
#data summary and graph: day, species, and epiphyte conver
  mean_Epi <- Seagrass1 %>%
    group_by(Day, Species) %>%
    summarise(AvgEpi = mean(EpiphyteDensity)) 
  head(mean_Epi)
  #removing empty rows - no SAV observed  
  NoZeroEpi <- mean_Epi[apply(mean_Epi!=0, 1, all),] %>% 
    drop_na(Day, Species, AvgEpi)
  head(NoZeroEpi)
  #bar chart
  ggplot(NoZeroEpi, aes(fill=Species, y=AvgEpi, x=Day)) + 
    geom_bar(stat = "identity", position = position_dodge2(preserve = 'single')) +
    scale_x_continuous(breaks=seq(20,24,by=1)) +
    labs(
      title = "Epiphyte Coverage for each day and species", y="Epiphyte Coverage Score", x="Day in October 2025"
    )
  

#mean of all Epiphyte coverage data by species
  EpiDen <- read_xlsx("C:/Users/gmang/Documents/Gman/Ecology/AMIkids/DataWranglingAMIkids/SeagrassDataOct2025-AMIkids.xlsx") %>% 
    group_by(Species)
  # Removing zeros from column
  NoZerosEpi <- EpiDen[EpiDen$EpiphyteDensity != 0, ] %>% 
    summarise(OverEpi = mean(EpiphyteDensity))  
  head(NoZerosEpi)
