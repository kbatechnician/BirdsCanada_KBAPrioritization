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
  htmlwidgets,
  gsheet
)

############################# TESTING SECTION #################################

require(Microsoft365R)

sharepoint <- get_sharepoint_site(site_url = "https://birdscanadaorg-my.sharepoint.com/:f:/g/personal/kbasupport_birdscanada_org1/IgDWQe4AurPlQaTPN2JTV0q0ATlphi3dIwj1Gy2WuZ1TNDM?e=Pezii0")


######################## Set up database connection ###########################

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

# Set working directory to wherever you have the TBI and threats tables saved
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


############################# 1. PA/OECM Overlap ##############################

KBA_Site <- kbadb %>% read_sf("KBA_Site") # Load KBA_Site table from the KBA database
site <- KBA_Site[, c(1,2,5,13)] %>% 
  st_drop_geometry() %>% 
  rename(PA.OECM = PercentProtected)
#original <- site
site <- site %>%
  mutate(PA.OECM = case_when(
    PA.OECM <= 20 ~ 5,
    PA.OECM > 20 & PA.OECM <= 40 ~ 4,
    PA.OECM > 40 & PA.OECM <= 60 ~ 3,
    PA.OECM > 60 & PA.OECM <= 80 ~ 2,
    PA.OECM > 80 & PA.OECM <= 100 ~ 1
  ))


########################### 2. Effective Protection  ##########################

KBA_PA <- kbadb %>% read_sf("KBA_ProtectedArea") # Load KBA_ProtectedArea table from the KBA datbase
pa <- KBA_PA[, c(2,7,9)] 
#original <- original %>% full_join(pa, by = "SiteID") %>% rename(effective.cover = PercentCover)
pa <- pa %>%
  mutate(IUCNCat_EN = case_when(
    IUCNCat_EN == "Not reported" ~ 5,
    IUCNCat_EN == "Not applicable" ~ 5,
    grepl(paste(c("Ia", "Strict nature reserve"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 1,
    grepl(paste(c("Ib", "Wilderness area"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 1,
    grepl(paste(c("II", "National park"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 1,
    grepl(paste(c("III", "Natural monument or feature"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 2,
    grepl(paste(c("IV", "Habitat/species management area"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 3,
    grepl(paste(c("V", "Protected landscape or seascape"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 4,
    grepl(paste(c("VI", "Protected area with sustainable use of natural resources"), collapse = "|"), pa$IUCNCat_EN, ignore.case = T) ~ 3
  ))
pa$effective.protection <- pa$IUCNCat_EN * (pa$PercentCover/100)
pa <- pa %>% group_by(SiteID) %>% 
  select(-IUCNCat_EN, -PercentCover) %>%
  summarise(effective.protection = sum(effective.protection))

p <- full_join(site, pa, by = "SiteID") # Creates a table called p that will hold all the index values calculated


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

url_tbi <- "https://docs.google.com/spreadsheets/d/1wk3I6WgYIlCqsobkmIb4m2-xa5CLZmOh/edit?usp=sharing&ouid=108741135940790475545&rtpof=true&sd=true"

tbi <- gsheet::gsheet2tbl(url_tbi) %>% select(english_name, responsibility, indicator_lt)
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

# Loading the SAR Threat Data
# URL location of the SARThreatDataExport file
url_threat <- "https://docs.google.com/spreadsheets/d/1a3B6P_NvbiWVwZsc8oyLaygHBX35ynwE/edit?usp=sharing&ouid=108741135940790475545&rtpof=true&sd=true"

threat <- gsheet::gsheet2tbl(url_threat) %>% select(`COSEWIC common name and population`, `Listing scientific name`, `COSEWIC ID`, `Overall threat impact (assigned)`, `Threat (level 1)`, `Threat (level 2)`, `Threat impact`) %>%
  rename(english_name = `COSEWIC common name and population`, scientific_name = `Listing scientific name`, COSEWICTaxonID = `COSEWIC ID`, overall.threat = `Overall threat impact (assigned)`, threat_level1 = `Threat (level 1)`, threat_level2 = `Threat (level 2)`, individual.threat = `Threat impact`)
threat$COSEWICTaxonID <- as.integer(threat$COSEWICTaxonID)

### Overall Threat Analysis
## 1. Pull species at site information from KBA Database
############## 8. Overall Threats AND 9. Individual Threats ###################

# Loading the SAR Threat Data
# URL location of the SARThreatDataExport file
url_threat <- "https://docs.google.com/spreadsheets/d/1a3B6P_NvbiWVwZsc8oyLaygHBX35ynwE/edit?usp=sharing&ouid=108741135940790475545&rtpof=true&sd=true"

threat <- gsheet::gsheet2tbl(url_threat) %>% select(`COSEWIC common name and population`, `Listing scientific name`, `COSEWIC ID`, `Overall threat impact (assigned)`, `Threat (level 1)`, `Threat (level 2)`, `Threat impact`) %>%
  rename(english_name = `COSEWIC common name and population`, scientific_name = `Listing scientific name`, COSEWICTaxonID = `COSEWIC ID`, overall.threat = `Overall threat impact (assigned)`, threat_level1 = `Threat (level 1)`, threat_level2 = `Threat (level 2)`, individual.threat = `Threat impact`)
threat$COSEWICTaxonID <- as.integer(threat$COSEWICTaxonID)

### Overall Threat Analysis
## 1. Pull species at site information from KBA Database
KBA_Species <- kbadb %>% read_sf("Species") %>% select(SpeciesID, COSEWICTaxonID) #Pulling the COSEWIC Taxon ID for species to link SARA threat table with KBA SpeciesID
spa_COSEWIC <- left_join(spa, KBA_Species) # add the COSEWICTaxinIDs to the existing species at site table
spa_COSEWIC <- spa_COSEWIC %>% select(SiteID, SpeciesID, COSEWICTaxonID) %>% distinct(SiteID, SpeciesID, COSEWICTaxonID) # keep only necessary columns

## 2. Summarise the SAR Threat Data to show overall threat for each species
threat_species <- threat %>% distinct(english_name, COSEWICTaxonID, overall.threat)

## 3. Create a new table with overall threat status for species at KBA sites
threatover_analysis <- left_join(spa_COSEWIC, threat_species, by = "COSEWICTaxonID") # 1582 records have no overall.threat information - Check this w Amanda


### Overall Threat Analysis
#original <- original %>% full_join(threatover, by = "english_name") %>% unique()
threatover_analysis <- threatover_analysis %>%
  mutate(overall.threat = case_when(
    overall.threat == "Low" ~ 0.1,
    overall.threat == "Medium - Low" ~ 0.2,
    overall.threat == "Medium" ~ 0.3,
    overall.threat == "High - Low" ~ 0.3, # Check this w Amanda
    overall.threat == "High - Medium" ~ 0.4,
    overall.threat == "Very High - Low" ~ 0.4, # Check this w Amanda
    overall.threat == "Very High - Medium" ~ 0.5, # Check this w Amanda
    overall.threat == "High" ~ 0.5,
    overall.threat == "Very High - High" ~ 0.6,
    overall.threat == "Very High" ~ 0.7,
    overall.threat == "Unknown" ~ 0 # Check this w Amanda
  )) %>%
  drop_na(overall.threat)

threatover <- threatover_analysis %>% unique () %>% group_by(SiteID) %>% summarise(overall.threat = sum(as.numeric(overall.threat)))


### Individual Threat Analysis

# 1. Pull Threats at KBA Sites
KBA_SiteThreats <- kbadb %>% read_sf("KBA_Threats") # reading in the KBA_Threats table from the KBA database
KBA_Threats <- kbadb %>% read_sf("Threats") # reading in the Threats table from the KBA database
SiteThreats_join <- left_join(KBA_SiteThreats, KBA_Threats) # joining the two tables above based on ThreatID

# 2. Pull Species at KBA Sites and relevant threats for those species from the SARA database
SpAtSite <- KBA_SpA %>% distinct(SiteID, SpeciesID, Original_CommonNameEN, Original_ScientificName) %>% drop_na(Original_ScientificName)
SpAtSite_join <- left_join(SpAtSite, KBA_Species)

# Table with SAR Threats for KBA Species at Site
SpAtSite_Threats <- left_join(SpAtSite_join, threat, relationship = "many-to-many")
SpAtSite_Threats <- SpAtSite_Threats %>% select(-english_name, -scientific_name, -overall.threat, -COSEWICTaxonID) %>% drop_na(threat_level2)

SpAtSite_SubThreats <- SpAtSite_Threats
SpAtSite_SubThreats$ThreatCode <- sub(" .*", "", SpAtSite_SubThreats$threat_level2) #Making a ThreatCode column to match the column in the SiteThreats_join table

## 3. Create new table that keeps only the site threats relevant to species at KBA sites
SpSiteThreat_Final <- inner_join(SpAtSite_SubThreats, SiteThreats_join, join_by(SiteID, ThreatCode))
SpSiteThreat_Final <- SpSiteThreat_Final %>% select(-ThreatsSiteID, -ThreatID, -Threat_EN, -Threat_FR, -ThreatCode)

## 4. Assigning values based on severity of each threat
threatind_analysis <- SpSiteThreat_Final %>%
  mutate(individual.threat = case_when(
    individual.threat == "Negligible" ~ 0,
    individual.threat == "Not a threat" ~ 0,
    individual.threat == "Not applicable" ~ 0,
    individual.threat == "Low" ~ 0.1,
    individual.threat == "Medium - Low" ~ 0.2,
    individual.threat == "Medium" ~ 0.3,
    individual.threat == "High - Low" ~ 0.3,
    individual.threat == "High - Medium" ~ 0.4,
    individual.threat == "High" ~ 0.5,
    individual.threat == "Very High - Medium" ~ 0.5, # Check this w Amanda
    individual.threat == "Very High - High" ~ 0.6, # Check this w Amanda
    individual.threat == "Very High" ~ 0.7
  )) %>%
  drop_na(individual.threat)

# Sum assigned values for all relevant species threats at each site
threatind <- threatind_analysis %>% unique() %>% group_by(SiteID) %>% summarise(individual.threat = sum(as.numeric(individual.threat)))

## 5. Joining the species and site threat values to the prioritization table
p <- full_join(p, threatover, by = "SiteID")
p <- full_join(p, threatind, by = "SiteID")
p <- p[!is.na(p$SiteCode),]


########################## 10. Human Modification #############################

# First time processing
# hm <- rast("Data/HM_CA_2022_r90_20251030.tif") # Load the raster data
# hm <- hm %>% trim() # Trim any NAs out
# hm <- project(hm, "ESRI:102001") %>% trim() # Reproject raster layer to Canada Albers Equal Area Conic
# writeRaster(hm, "Data/HM_CA_2022_r90_20251030.tif", overwrite = TRUE) # Save and overwrite the old file

# If raster has already been processed, read it in using the following code
hm <- rast("Data/HM_CA_2022_r90_20251030.tif")

# Store Human Modification CRS for use throughout script.
hm_crs <- crs(hm)

# Prep KBA geometry for analysis
KBA_geom <- KBA_Site %>% select(SiteID,SiteCode,Name_EN,geometry) %>% st_transform(hm_crs)

# Assign an HM value for each KBA based on mean HM at the site
KBA_geom$HM <- NA # Create a blank column to store HM values
for (i in KBA_geom$SiteID) {
  # Extract human modification scores for KBAs
  KBA_geom$HM[KBA_geom$SiteID == i] <- extract(
    hm,
    KBA_geom[KBA_geom$SiteID == i,],
    fun = "mean",
    na.rm = TRUE,
    ID = FALSE
  )
}
KBA_geom$HM <- as.numeric(KBA_geom$HM)


################### Calculate total score and save output #####################

p$total.score <- rowSums(p[,c(4:12)], na.rm = T)
write.csv(p, "priority_2026.11.11.2.csv")
#write.csv(original, "original_priority_v2.csv")

