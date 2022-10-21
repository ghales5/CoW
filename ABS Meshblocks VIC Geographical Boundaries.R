#
# ABS meshblocks suburbs, postcodes, LGA validation and mapping
# Date: 27/05/2022
# ---------------------------------------------------------------------------
# Notes: Validate region matching
# Summary: Read in Meshblocks, LGA, Postcode and Suburb data from the ABS, narrow it down to WPHU geographies 
# by matching it using meshblocks
# Creates geography excel files mapping postcodes to suburbs/LGAs.
# They are located in: S:\WH Covid-19 Community Support\LOCAL PUBLIC HEALTH UNIT\9. Data and Reporting\Data and data sources\Spatial files
# Get the distinct rows of the data and export it to excel files
# Input:
# 1) ABS region files
# 2) LGA PBI region files
#-------------------------------------------------------------------------
# clear workspace
rm(list = ls()) 

#library and load packages
library(tidyr)
library(readxl)
library(dplyr,warn.conflicts=F)
library(writexl)
library(stringr)
library(lubridate)
library(sf)
library(ggplot2)

#--------------------------------Variables------------------------------------------
#Today's date
currentDate <- Sys.Date()

#--------------------------------Import files------------------------------------------
setwd("S:/WH Covid-19 Community Support/LOCAL PUBLIC HEALTH UNIT/9. Data and Reporting/Data and data sources/ABS Data/Spatial files")

#read files - 2021 abs data
#Meshblock file
mb2021 <- read_excel("MB_2021_AUST.xlsx") %>%
  filter(STATE_NAME_2021 == "Victoria") %>%
  select(MB_CODE_2021)

#LGA file
lga2021 <- read_excel("LGA_2021_AUST.xlsx") %>%
  filter(STATE_NAME_2021 == "Victoria") %>%
  select(MB_CODE_2021,LGA_CODE_2021,LGA_NAME_2021)

#Suburb file
sub2021 <- read_excel("SuburbsLocalities_2021_AUST.xlsx") %>%
  filter(STATE_NAME_2021 == "Victoria") %>%
  select(MB_CODE_2021,SAL_NAME_2021) %>%
  dplyr::rename(Suburb_Name = SAL_NAME_2021)

#Postcode file
poa2021 <- read_excel("POA_2021_AUST.xlsx") %>%
  select(MB_CODE_2021,POA_CODE_2021) %>%
  filter(MB_CODE_2021 %in% mb2021$MB_CODE_2021)

# PHU boundaries
PHU <- read_excel("PHU LGA Boundaries 2022.xlsx") 

#Join all files to create a merged regions file
regions2021 <- left_join(lga2021,mb2021,by="MB_CODE_2021") %>%
  left_join(.,sub2021,by="MB_CODE_2021") %>%
  left_join(.,poa2021,by="MB_CODE_2021") %>%
  dplyr::rename(MB_Code = MB_CODE_2021,
                LGA_code = LGA_CODE_2021,
                LGA_Name = LGA_NAME_2021,
                Postcode = POA_CODE_2021)

#get distinct values of postcode, suburb and lga name
regions_final2021 <- regions2021 %>%
  select(-MB_Code) %>%
  distinct() %>%
  mutate(Postcode = as.integer(Postcode)) %>%
  select(Postcode,Suburb_Name,LGA_Name)


#get list of distinct lga and suburbs from abs
lga_abs <- regions_final2021 %>%
  select(Postcode, LGA_Name) %>%
  distinct() %>%
  arrange(Postcode,LGA_Name)

sub_abs <- regions_final2021 %>%
  select(Postcode, Suburb_Name) %>%
  distinct() %>%
  mutate(Suburb_Name = gsub("\\s*\\([^\\)]+\\)","",Suburb_Name))
#write_xlsx(sub_abs, "suburb_postcode.xlsx")
#concatenate suburbs that have the same postcodes together, separating it with a comma
sub_abs_merged <- sub_abs %>%
  group_by(Postcode) %>%
  summarise(Suburb = str_c(Suburb_Name, collapse = ", "))

## Summarise postcode crossover
# by LGA
poa_summary <- lga_abs %>%
  group_by(Postcode)%>%
  summarise(LGAcross_no = n())

poa_summary2 <- poa_summary %>%
  group_by(LGAcross_no)%>%
  summarise(n = n_distinct(Postcode))

# by PHU
PHU_postcodes <- lga_abs%>%
  left_join(.,PHU, by="LGA_Name")%>%
  drop_na(PHU)

PHU_cross_count <- PHU_postcodes %>%
  select(-LGA_Name, -`Metro\Regional`)%>%
  distinct()%>%
  group_by(Postcode)%>%
  summarise(PHUCross_no = n())%>%
  left_join(.,PHU_postcodes, by="Postcode")

PHU_summary <- PHU_cross_count%>%
  group_by(PHUCross_no)%>%
  summarise(n = n_distinct(Postcode))
  
PHU_NoCross <- PHU_cross_count %>%
  filter(PHUCross_no == 1)%>%
  group_by(Postcode) %>%
  mutate(counter = seq_along(Postcode))%>%
  ungroup()%>%
  pivot_wider(.,
              names_from = counter,
              values_from = LGA_Name,
              names_sep = ".")

PHU_Cross <- PHU_cross_count %>%
  select(-LGA_Name, -`Metro/Regional`, -PHUCross_no)%>%
  distinct()%>%
  group_by(Postcode) %>%
  mutate(counter = seq_along(Postcode))%>%
  ungroup()%>%
  pivot_wider(.,
          names_from = counter,
          values_from = PHU,
          names_sep = ".")%>%
  drop_na(`2`)

#--------------------------------Export------------------------------------------
#output postcode to lga file
#write_xlsx(lga_abs, paste(currentDate,"Postcode to LGA.xlsx",sep=" "))
#write_xlsx(poa_summary, paste(currentDate,"LGA count by postcodes.xlsx",sep=" "))
#write_xlsx(poa_summary2, paste(currentDate,"Postcode count by no. of LGA crossovers.xlsx",sep=" "))

#output postcode to suburb file
#write_xlsx(sub_abs, paste(currentDate,"Postcode to Suburb.xlsx",sep=" "))

#output postcode to suburb merged file
#write_xlsx(sub_abs_merged, paste(currentDate,"Postcode to merged Suburb.xlsx",sep=" "))

# Export all regions
#write_xlsx(regions_final2021,paste(currentDate,"Postcode, Suburb, LGA.xlsx",sep=" "))

# PHU crossover info
# write_xlsx(PHU_summary, "PHU postode summary.xlsx")
# write_xlsx(PHU_NoCross, "No PHU crossover list.xlsx")
# write_xlsx(PHU_Cross, "PHU crossover by postcode.xlsx")

# ============================= MAPS

# Shape file for all of Australia's Postcode
Australia.SF <- st_read('Postal Areas/POA_2021_AUST_GDA2020.shp')%>%
  filter(as.integer(POA_CODE21)> 2999 & as.integer(POA_CODE21)<4000)


# Shape file for all of Victoria's LGA's
VIC.LGA.Map <- st_read('VICLGA/vic_lga.shp')

# Overlaying the Postcodes and LGA Maps
LGA.Postcode.Overlay <- Australia.SF %>%
  ggplot() +
  geom_sf(data = Australia.SF, color = 'black') +
  geom_sf(data = VIC.LGA.Map, alpha = 0.5, color = 'blue')

LGA.Postcode.Overlay

# Total Cases by Postcode
Total.Cases.Map <- WPHU.SF %>%
  inner_join(aggregate.data.postcode %>% select(Postcode, `Total Cases`), by = "Postcode") %>%
  ggplot() +
  geom_sf(color = 'black', aes(fill = `Total Cases`)) +
  scale_fill_gradientn(name = "",colours = c("#009E73","#F0E442","#D55E00")) +
  geom_sf(data = WPHU.LGA.SF, color = 'orange', alpha = 0, size = 1)

Total.Cases.Map
