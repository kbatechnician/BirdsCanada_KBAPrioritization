###############################################################################

# Project: Birds Canada - Key Biodiversity Areas Prioritization

# Script Title: Assigning prioritization scores to Key Biodiversity Areas (KBAs)
# for bird species in Canada

# Script Author: Courtney Donkersteeg (cdonkersteeg@birdscanada.org) and 
# Chelsea Aristone (caristone@birdscanada.org)

# Creation Date: 2026-10-07

########################### Initial Setup #####################################

### Clean Environment
rm(list = ls())

### Load necessary packages

if (system.file(package = "librarian") == "") {
  install.packages("librarian")
}

librarian::shelf(
  dplyr,
  RPostgres,
  lubridate,
  DBI,
  readxl,
  tidyverse,
  sf,
  terra,
  units,
  measurements,
  mapview,
  tidyterra,
  leaflet,
  reporter,
  svMisc,
  htmlwidgets
)

############################# TESTING SECTION #################################

require(Microsoft365R)

sharepoint <- get_sharepoint_site(site_url = "https://birdscanadaorg-my.sharepoint.com/:f:/g/personal/kbasupport_birdscanada_org1/IgDWQe4AurPlQaTPN2JTV0q0ATlphi3dIwj1Gy2WuZ1TNDM?e=Pezii0")


###############################################################################

### Set up database connection

### When running on the KBA Database Server

# Environment variables 
env_vars <- c("kbapipeline_pswd", "postgres_user", "postgres_pass", "database_name", "database_host", "mailtrap_pass", "database_port", "geoserver_pass", "docker_env", "obsoleteReasonURL", "galleryItemsURL")

for(env in env_vars){
  
  # Get variable
  var <- Sys.getenv(toupper(env))
  
  # Assign variable
  assign(env, var)
}
rm(env, env_vars, var)

# KBA Registry database information
# Registry database connection
kbadb <- dbConnect(
  Postgres(), 
  user = postgres_user,
  password = postgres_pass,
  dbname = database_name,
  host = database_host,
  port = database_port
)

### When running on the Staging Server

### Set working directory to wherever you have the TBI and threats tables saved
setwd("C:\\Users\\kbasupport\\OneDrive - birdscanada.org\\Desktop\\KBACanada_Code\\BirdsCanada_KBAPrioritization")

kbadb = dbConnect(
  Postgres(), 
  user = 'admin',
  password = 'm455t3r.p',
  dbname = 'kbacanada',
  host = '143.110.218.194',
  port = 5533,
  sslmode = 'require')


###############################################################################

### If you would like to write a second .csv of the priority table with all
### categories BEFORE numerical rankings, remove all "#" before the object
### "original" and run as normal

############### 1. PA/OECM Overlap AND 2. Effective Protection ################

KBA_Site <- kbadb %>% read_sf("KBA_Site")
site <- KBA_Site[, c(1,2,5,13)] %>% st_drop_geometry()
site <- site %>% rename(PA.OECM = PercentProtected)
#original <- site
site$PA.OECM <- replace(site$PA.OECM, site$PA.OECM <= 20, 5)
site$PA.OECM <- replace(site$PA.OECM, site$PA.OECM > 20 & site$PA.OECM <= 40, 4)
site$PA.OECM <- replace(site$PA.OECM, site$PA.OECM > 40 & site$PA.OECM <= 60, 3)
site$PA.OECM <- replace(site$PA.OECM, site$PA.OECM > 60 & site$PA.OECM <= 80, 2)
site$PA.OECM <- replace(site$PA.OECM, site$PA.OECM > 80 & site$PA.OECM <= 100, 1)

KBA_PA <- kbadb %>% read_sf("KBA_ProtectedArea")
pa <- KBA_PA[, c(2,7,9)] 
#original <- original %>% full_join(pa, by = "SiteID") %>% rename(effective.cover = PercentCover)
pa$IUCNCat_EN <- replace(pa$IUCNCat_EN, pa$IUCNCat_EN == "Not reported", 5)
pa$IUCNCat_EN <- replace(pa$IUCNCat_EN, pa$IUCNCat_EN == "Not applicable", 5)
pa$IUCNCat_EN[grepl(paste(c("Ia", "Strict nature reserve"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 1
pa$IUCNCat_EN[grepl(paste(c("Ib", "Wilderness area"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 1
pa$IUCNCat_EN[grepl(paste(c("II", "National park"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 1
pa$IUCNCat_EN[grepl(paste(c("III", "Natural monument or feature"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 2
pa$IUCNCat_EN[grepl(paste(c("IV", "Habitat/species management area"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 3
pa$IUCNCat_EN[grepl(paste(c("V", "Protected landscape or seascape"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 4
pa$IUCNCat_EN[grepl(paste(c("VI", "Protected area with sustainable use of natural resources"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T)] <- 3
pa$IUCNCat_EN <- as.numeric(pa$IUCNCat_EN)
pa$effective.protection <- pa$IUCNCat_EN * (pa$PercentCover/100)
pa$IUCNCat_EN <- NULL
pa$PercentCover <- NULL
pa <- pa %>% group_by(SiteID) %>% summarise(effective.protection = sum(effective.protection))


p <- full_join(site, pa, by = "SiteID")


########################### 3. Priority Habitat ###############################

KBA_Habitat <- kbadb %>% read_sf("KBA_Habitat")
Habitat <- kbadb %>% read_sf("Habitat")
KBA_System <- kbadb %>% read_sf("KBA_System")
System <- kbadb %>% read_sf("System")
hab <- KBA_Habitat[, c(2,3,5)]
hab <- full_join(hab, Habitat, by = "HabitatID")
hab <- full_join(hab, KBA_System, by = "SiteID")
hab <- full_join(hab, System, by = "SystemID")
hab <- hab[, c(1,3,6,11)]
hab <- hab %>% filter(Group_EN %in% c("Grassland", "Forest", "Wetland", "Water"))
hab <- hab[-which(hab$Type_EN != "Marine" & hab$Group_EN == "Water"),]
hab$Type_EN <- NULL
hab <- hab %>% unique() %>% group_by(SiteID) %>% summarise(PriorityHabitat = sum(PercentCover))
#original <- original %>% full_join(hab, by = "SiteID")
hab$PriorityHabitat <- replace(hab$PriorityHabitat, hab$PriorityHabitat <= 20, 1)
hab$PriorityHabitat <- replace(hab$PriorityHabitat, hab$PriorityHabitat > 20 & hab$PriorityHabitat <= 40, 2)
hab$PriorityHabitat <- replace(hab$PriorityHabitat, hab$PriorityHabitat > 40 & hab$PriorityHabitat <= 60, 3)
hab$PriorityHabitat <- replace(hab$PriorityHabitat, hab$PriorityHabitat > 60 & hab$PriorityHabitat <= 80, 4)
hab$PriorityHabitat <- replace(hab$PriorityHabitat, hab$PriorityHabitat > 80 & hab$PriorityHabitat <= 100, 5)

p <- full_join(p, hab, by = "SiteID")


############ 4. Number of Trigger elements AND 5. Species at Risk #############

KBA_Subcri <- kbadb %>% read_sf("Subcriterion")
KBA_SpASubcri <- kbadb %>% read_sf("SpeciesAssessment_Subcriterion")
KBA_SpA <- kbadb %>% read_sf("KBA_SpeciesAssessments")
spa <- KBA_SpA %>% full_join(KBA_SpASubcri, by = "SpeciesAssessmentsID") %>% full_join(KBA_Subcri, by = "SubcriterionID")
spa <- spa[, c(2,3,25,30)]
KBA_EA <- kbadb %>% read_sf("KBA_EcosystemAssessments")
ea <- KBA_EA[, c(2,3)]
p$NumberOfTriggers <- NA
#original$NumberOfTriggers <- NA
for(i in 1:nrow(p)){
  p$NumberOfTriggers[i] <- nrow(spa[spa$SiteID == p$SiteID[i], ]) + nrow(ea[ea$SiteID == p$SiteID[i], ])
  #original$NumberOfTriggers[i] <- nrow(spa[spa$SiteID == original$SiteID[i], ]) + nrow(ea[ea$SiteID == original$SiteID[i], ])
}
p$NumberOfTriggers <- replace(p$NumberOfTriggers, p$NumberOfTriggers <= 5, 1)
p$NumberOfTriggers <- replace(p$NumberOfTriggers, p$NumberOfTriggers > 5 & p$NumberOfTriggers <= 10, 2)
p$NumberOfTriggers <- replace(p$NumberOfTriggers, p$NumberOfTriggers > 10 & p$NumberOfTriggers <= 15, 3)
p$NumberOfTriggers <- replace(p$NumberOfTriggers, p$NumberOfTriggers > 15, 4)
#original <- original %>% full_join(spa, by = "SiteID")
spa$Subcriterion[!grepl(paste(c("A1a", "A1c", "A1b", "A1d", "A1e", "B", "D"), collapse = "|"), spa$Subcriterion)] <- 0
spa$Subcriterion[grepl(paste(c("A1a", "A1c"), collapse = "|"), spa$Subcriterion)] <- 3
spa$Subcriterion[grepl(paste(c("A1b", "A1d", "A1e"), collapse = "|"), spa$Subcriterion)] <- 2
spa$Subcriterion[grepl("B", spa$Subcriterion)] <- 1
spa$Subcriterion[grepl("D", spa$Subcriterion)] <- 0


################ 6. Thriving Bird Index AND 7. Responsibility #################

tbi <- read_xlsx("ThrivingBirdIndex.xlsx") %>% select(english_name, responsibility, indicator_lt)
spa <- spa %>% rename(english_name = Original_CommonNameEN)
spa <- full_join(spa, tbi, by = "english_name")
#original <- original %>% rename(english_name = Original_CommonNameEN) %>% full_join(tbi, by = "english_name")
spa$indicator_lt[grepl("RED", spa$indicator_lt)] <- 0.5
spa$indicator_lt[grepl("YELLOW", spa$indicator_lt)] <- 0.3
spa$indicator_lt[grepl("GREEN", spa$indicator_lt)] <- 0.1
spa$indicator_lt[grepl("GRAY", spa$indicator_lt)] <- 0


spa$responsibility[grepl("VH", spa$responsibility)] <- 0.5
spa$responsibility[grepl("H", spa$responsibility)] <- 0.4
spa$responsibility[grepl("M", spa$responsibility)] <- 0.3
spa$responsibility[grepl("L", spa$responsibility)] <- 0.2
spa$responsibility[grepl("VL", spa$responsibility)] <- 0.1
spa$responsibility[grepl("NA", spa$responsibility)] <- 0
spa <- unique(spa)

spaind <- spa %>% group_by(SiteID) %>% summarise(TBI = sum(as.numeric(indicator_lt), na.rm = T))
spares <- spa %>% group_by(SiteID) %>% summarise(responsibility = sum(as.numeric(responsibility), na.rm = T))
spasum <- spa %>% group_by(SiteID) %>% summarise(SpeciesStatus = sum(as.numeric(Subcriterion), na.rm = T))


p <- full_join(p, spasum, by = "SiteID")
p <- full_join(p, spaind, by = "SiteID")
p <- full_join(p, spares, by = "SiteID")


############## 8. Overall Threats AND 9. Individual Threats ###################

threat <- read_xlsx("SARThreatDataExport30April2026.xlsx", sheet = 1) %>% select(`COSEWIC common name and population`, `Overall threat impact (assigned)`, `Threat impact`) %>%
  rename(english_name = `COSEWIC common name and population`, overall.threat = `Overall threat impact (assigned)`, individual.threat = `Threat impact`)
#original <- original %>% full_join(threat, by = "english_name") %>% unique()
threat <- full_join(threat, spa, by = "english_name")
threat$overall.threat[which(threat$overall.threat == "Very High")] <- 0.7
threat$overall.threat[which(threat$overall.threat == "Very High - High")] <- 0.6
threat$overall.threat[which(threat$overall.threat == "High")] <- 0.5
threat$overall.threat[which(threat$overall.threat == "High - Medium")] <- 0.4
threat$overall.threat[which(threat$overall.threat == "Medium")] <- 0.3
threat$overall.threat[which(threat$overall.threat == "Medium - Low")] <- 0.2
threat$overall.threat[which(threat$overall.threat == "Low")] <- 0.1

threat$individual.threat[which(threat$individual.threat == "Very High")] <- 0.7
threat$individual.threat[which(threat$individual.threat == "Very High - High")] <- 0.6
threat$individual.threat[which(threat$individual.threat == "High")] <- 0.5
threat$individual.threat[which(threat$individual.threat == "High - Medium")] <- 0.4
threat$individual.threat[which(threat$individual.threat == "High - Low")] <- 0.3
threat$individual.threat[which(threat$individual.threat == "Medium")] <- 0.3
threat$individual.threat[which(threat$individual.threat == "Medium - Low")] <- 0.2
threat$individual.threat[which(threat$individual.threat == "Low")] <- 0.1
threat$individual.threat[grepl(paste(c("Not applicable", "Not a threat", "Negligible", "Unknown", "Not calculated"), collapse = "|"), threat$individual.threat)] <- 0
threat <- threat[-which(is.na(threat$SiteID) | is.na(threat$overall.threat) | is.na(threat$individual.threat)),]
threatover <- threat %>% unique() %>% group_by(SiteID) %>% summarise(overall.threat = sum(as.numeric(overall.threat)))
threatind <- threat %>% unique() %>% group_by(SiteID) %>% summarise(individual.threat = sum(as.numeric(individual.threat)))


p <- full_join(p, threatover, by = "SiteID")
p <- full_join(p, threatind, by = "SiteID")
p <- p[!is.na(p$SiteCode),]


########################## 10. Human Modification #############################

# First time processing
# hm <- rast("Data/HM_CA_2022_r90_20251030.tif") # Load the raster data
# hm <- hm %>% trim() # Trim any NAs out
# hm <- project(hm, "ESRI:102001") %>% trim() # Reproject raster layer to Canada Albers Equal Area Conic
# writeRaster(hm, "Data/HM_CA_2022_r90_20251030.tif",overwrite = TRUE) # Save and overwrite the old file

# If raster has already been processed, read it in using the following code
hm <- rast("Data/HM_CA_2022_r90_20251030.tif")
  
# Store Human Modification CRS for use throughout script.
hm_crs <- crs(hm)

# Prep KBA geometry for analysis
KBA_geom <- KBA_Site %>% select(SiteCode,geometry) %>% st_transform(hm_crs)

# Assign an HM value for each KBA based on mean HM at the site
KBA_geom$HM <- NA # Create a blank column to store HM values
for (i in KBA_geom$SiteCode) {
  # Extract human modification scores for KBAs
  KBA_geom$HM[KBA_geom$SiteCode == i] <- extract(
        hm,
        KBA_geom[KBA_geom$SiteCode == i,],
        fun = "mean",
        na.rm = TRUE,
        ID = FALSE
      )
}


################### Calculate total score and save output #####################

p$total.score <- rowSums(p[,c(4:12)], na.rm = T)
write.csv(p, "priority_v2.csv")
#write.csv(original, "original_priority_v2.csv")

