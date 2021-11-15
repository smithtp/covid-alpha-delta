# combine alpha/delta STP Rts with climate variables
source("code/packages.R")

# alpha/delta data for 9 weeks:
STP_rts <- read.csv("data/sgtf_output.csv")

STP_cases <- readRDS("data/cases_data.rds")
names(STP_cases) <- c("week", "area", "cases_alpha", "cases_delta")

STP_rts <- left_join(STP_rts, STP_cases, by = c("area", "week"))
STP_rts$delta_proportion <- STP_rts$cases_delta/(STP_rts$cases_alpha+STP_rts$cases_delta)

# get environmental data
stp_temp <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/temp-dailymean-UK-STP-cleaned.RDS")))
stp_spechumid <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/spechumid-dailymean-UK-STP-cleaned.RDS")))
stp_relhumid <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/relhumid-dailymean-UK-STP-cleaned.RDS")))
stp_uv <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/uv-dailymean-UK-STP-cleaned.RDS")))
stp_precip <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/precip-dailymean-UK-STP-cleaned.RDS")))
stp_pop <- as.data.frame(readRDS(url("https://github.com/pearselab/areadata/raw/main/output/population-density-UK-STP.RDS")))

# put everything into a function
# to return a climate df in long form
make_long <- function(df, clim_var){
  df$area <- row.names(df)
  long_df <- pivot_longer(df,
                      cols = c(1:(ncol(df)-1)),
                      names_to = "date",
                      values_to = clim_var)
  long_df$date <- as.Date(long_df$date)
  return(long_df)
}

stp_temp_long <- make_long(stp_temp, "temperature")
stp_spechumid_long <- make_long(stp_spechumid, "specific_humidity")
stp_relhumid_long <- make_long(stp_relhumid, "relative_humidity")
stp_uv_long <- make_long(stp_uv, "uv")
stp_precip_long <- make_long(stp_precip, "precipitation")

# merge long data together
climate_long <- merge(merge(merge(merge(stp_temp_long, stp_spechumid_long, by = c("area", "date")),
                      stp_relhumid_long, by = c("area", "date")),
                      stp_uv_long, by = c("area", "date")), 
                      stp_precip_long, by = c("area", "date"))

# chop to 2021
climate_long <- climate_long[climate_long$date >= "2021-01-01",]
# add the epiweek
climate_long$epiweek <- as.numeric(format(as.Date(climate_long$date), "%V"))
# and get rid of the trailing week from the previous year
climate_long <- climate_long[climate_long$epiweek < 53,]
# summarize by week
summ_dat <- climate_long[,c("area", "relative_humidity", "temperature", "specific_humidity", "uv", "precipitation", "epiweek")] %>%
  group_by(area = area, epiweek = epiweek) %>%
  dplyr::summarize(temperature = mean(temperature, na.rm=TRUE),
                   relative_humidity = mean(relative_humidity, na.rm = TRUE),
                   specific_humidity = mean(specific_humidity, na.rm=TRUE),
                   uv = mean(uv, na.rm=TRUE),
                   precipitation = mean(precipitation, na.rm=TRUE))

# add population density
stp_pop$area <- rownames(stp_pop)
summ_dat <- merge(summ_dat, stp_pop, by = "area", all.x = TRUE)

# fix the place names
summ_dat$area <- gsub("_", " ", summ_dat$area)

summ_dat[summ_dat$area == "Cornwall and the Isles of Scilly Health and Social Care Partnership",]$area <-
  "Cornwall and the Isles of Scilly"
summ_dat[summ_dat$area == "Sussex and East Surrey Health and Care Partnership",]$area <-
  "Sussex and East Surrey"
summ_dat[summ_dat$area == "Surrey Heartlands Health and Care Partnership",]$area <-
  "Surrey Heartlands"
summ_dat[summ_dat$area == "Frimley Health and Care ICS",]$area <-
  "Frimley Health"

# merge with transmission data
# transmission_output_joint <- left_join(transmission_output_joint, stp_temp, by =  c("name", "epiweek"))
names(STP_rts) <- c("area", "epiweek", "delta_Rt", "alpha_Rt", "Ratio", "alpha_cases", "delta_cases", "delta_proportion")
STP_rts <- left_join(STP_rts, summ_dat, by =  c("area", "epiweek"))

##----- add total population ----- #

total_pop <- read.csv("data/stp_population_long.csv")
total_pop <- total_pop[total_pop$subgroup == "All",]
total_pop <- total_pop[9:50,c("Y2018", "AREA")]
names(total_pop) <- c("Population", "area")
total_pop$Population <- ceiling(total_pop$Population) # shouldn't be decimals in population count

STP_rts <- left_join(STP_rts, total_pop, by = "area")

##----- add in vaccine data ----- #
# involves mapping LTLAS (the level that vaccine data is in)
# to STPs (the level that Rt data is in)

stp_shp <- st_read("data/gis/Sustainability_and_Transformation_Partnerships_(April_2021)_EN_BFC.shp")
ltla_shp <- st_read("data/gis/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp")

# scotland, wales and NI ltlas to remove...
ltla_shp <- ltla_shp[1:317,]

# now the hard part
# find which LTLA polygons are inside STP polygons
p <- st_intersection(stp_shp, ltla_shp)

# this might actually work if we remove the duplicates
# but how do we know which duplicate to remove?
# remove the smallest area duplicates, lets calculate areas ourselves
p$st_areas_new <- as.numeric(st_area(p$geometry))

stp_ltla_df <- p %>%
  arrange(lad19nm, -st_areas_new) %>% # arrange the data by LTLA, sorted by highest area to lowest
  filter(duplicated(lad19nm) == FALSE) # remove duplicate LTLAs

# load in the ltla vacc data to add to the shapefile
ltla_vacc <- read.csv("data/ltla-vacc.csv")

# get weekly averages (or weekly max?) for each region
# first get some date columns
ltla_vacc$date <- as.Date(ltla_vacc$date, format = "%Y-%m-%d")
# chop to 2021
ltla_vacc <- ltla_vacc[ltla_vacc$date >= "2021-01-01",]
# add the epiweek
ltla_vacc$epiweek <- as.numeric(format(as.Date(ltla_vacc$date), "%V"))
# and get rid of the trailing week from the previous year
ltla_vacc <- ltla_vacc[ltla_vacc$epiweek < 53,]

summ_vacc <- ltla_vacc[,c("areaName", "cumPeopleVaccinatedCompleteByVaccinationDate", "cumPeopleVaccinatedFirstDoseByVaccinationDate",
                          "cumPeopleVaccinatedSecondDoseByVaccinationDate", "epiweek")] %>%
  group_by(area = areaName, epiweek = epiweek) %>%
  dplyr::summarize(first_dose = max(cumPeopleVaccinatedFirstDoseByVaccinationDate),
                   second_dose = max(cumPeopleVaccinatedSecondDoseByVaccinationDate),
                   full_vax = max(cumPeopleVaccinatedCompleteByVaccinationDate))

# things we need to fix:
# in the shapefile, hackney and city of london are seperate, but they are together in the vacc data
# nothing else matters, because its all non-England stuff

Hackney <- London <- summ_vacc[summ_vacc$area == "Hackney and City of London",]

Hackney$first_dose <-  floor(0.5*London$first_dose)
Hackney$second_dose <-  floor(0.5*London$second_dose)
Hackney$full_vax <-  floor(0.5*London$full_vax)
London$first_dose <-  ceiling(0.5*London$first_dose)
London$second_dose <-  ceiling(0.5*London$second_dose)
London$full_vax <-  ceiling(0.5*London$full_vax)
Hackney$area <- "Hackney"
London$area <- "City of London"

summ_vacc <- rbind(summ_vacc[summ_vacc$area != "Hackney and City of London",], Hackney, London)

# merge this with the geometry shapefile
vacc_geom <- merge(summ_vacc, stp_ltla_df, by.x = "area", by.y = "lad19nm")

# now summarise into STPs instead of LTLAs
vacc_stps <- vacc_geom[,c("epiweek", "first_dose", "second_dose", "full_vax", "STP21NM")] %>%
  group_by(STP21NM, epiweek) %>%
  dplyr::summarize(first_dose = sum(first_dose),
                   second_dose = sum(second_dose),
                   full_vac = sum(full_vax))

# merge into the Rt data
names(vacc_stps) <- c("area", "epiweek", "first_dose", "second_dose", "full_vax")

# quick bit of renaming to fix missing values
vacc_stps[vacc_stps$area == "Frimley Health and Care ICS",]$area <- "Frimley Health"
vacc_stps[vacc_stps$area == "Surrey Heartlands Health and Care Partnership",]$area <- "Surrey Heartlands"
vacc_stps[vacc_stps$area == "Sussex and East Surrey Health and Care Partnership",]$area <- "Sussex and East Surrey"

STP_rts <- left_join(STP_rts, vacc_stps, by =  c("area", "epiweek"))

# now get % of population vaccinated
STP_rts$full_vax_rate <- STP_rts$full_vax/STP_rts$Population

## ---- add in NPI data ------ #

# read npis
npi_data <- read.csv("data/oxford-interventions.csv")
england_npis <- npi_data[npi_data$RegionName == "England",]
england_npis$Date <- as.Date(as.character(england_npis$Date), format = "%Y%m%d")

# chop to 2021
england_npis <- england_npis[england_npis$Date >= "2021-01-01",]
# add the epiweek
england_npis$epiweek <- as.numeric(format(as.Date(england_npis$Date), "%V"))
# and get rid of the trailing week from the previous year
england_npis <- england_npis[england_npis$epiweek < 53,]
# summarize by week
summ_npi <- england_npis[,c("Date", "StringencyIndex", "GovernmentResponseIndex", "ContainmentHealthIndex",
                            "EconomicSupportIndex", "epiweek")] %>%
  group_by(epiweek = epiweek) %>%
  dplyr::summarize(stringency = mean(StringencyIndex, na.rm=TRUE),
                   gov_response = mean(GovernmentResponseIndex, na.rm=TRUE),
                   containment_health = mean(ContainmentHealthIndex, na.rm=TRUE),
                   economic_support = mean(EconomicSupportIndex, na.rm = TRUE))

# time-lag these variables too
summ_npi <- summ_npi %>%
  mutate(stringency_lag1 = lag(stringency, 1),
         stringency_lag2 = lag(stringency, 2),
         gov_response_lag1 = lag(gov_response, 1),
         gov_response_lag2 = lag(gov_response, 2),
         containment_health_lag1 = lag(containment_health, 1),
         containment_health_lag2 = lag(containment_health, 2),
         economic_support_lag1 = lag(economic_support, 1),
         economic_support_lag2 = lag(economic_support, 2))

# now merge this into the transmission data
STP_rts <- left_join(STP_rts, summ_npi, by =  "epiweek")

### --- DONE!

# save the outputs
write.csv(STP_rts, "data/delta_alpha_climate.csv", row.names = FALSE)
