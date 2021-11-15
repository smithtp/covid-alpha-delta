# combine background/alpha STP Rts with climate, etc.
source("code/packages.R")

# background/alpha data for 9 weeks:
STP_rts <- readRDS("data/df_rt.rds")
STP_cases <- readRDS("data/sgss_stp_new_43_56_weeks.rds")
names(STP_cases) <- c("area", "epiweek", "epistop", "background_cases", "alpha_cases", "total_cases", "background_cases_adj1",
                      "alpha_cases_adj1", "total_cases_adj1", "regcd", "probs", "alpha_cases_corrected", "background_cases_corrected", 
                      "alpha_cases_corrected_adj1", "background_cases_corrected_adj1")
names(STP_rts) <- c("area", "epiweek", "alpha_Rt", "background_Rt", "generation_time")
STP_rts <- STP_rts[STP_rts$generation_time == "original",]
STP_rts$epiweek <- rep(seq(45, 55, 1), 42)

STP_rts <- left_join(STP_rts, STP_cases[,c(1:2,12:13)], by = c("area", "epiweek"))
STP_rts$alpha_proportion <- STP_rts$alpha_cases_corrected/(STP_rts$alpha_cases_corrected+STP_rts$background_cases_corrected)

# get environmental data from AREAdata
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

# add the epiweek
climate_long$epiweek <- as.numeric(format(as.Date(climate_long$date), "%V"))
# need to alter the epiweeks so it runs on from 2020 to 2021 at 2021-01-04
climate_long[climate_long$date >= "2021-01-04",]$epiweek <- climate_long[climate_long$date >= "2021-01-04",]$epiweek+53
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
STP_rts <- left_join(STP_rts, summ_dat, by =  c("area", "epiweek"))

##----- add total population ----- #

total_pop <- read.csv("data/stp_population_long.csv")
total_pop <- total_pop[total_pop$subgroup == "All",]
total_pop <- total_pop[9:50,c("Y2018", "AREA")]
names(total_pop) <- c("Population", "area")
total_pop$Population <- ceiling(total_pop$Population) # shouldn't be decimals in population count

STP_rts <- left_join(STP_rts, total_pop, by = "area")

## ---- add in NPI data ------ #

# read npis
npi_data <- read.csv("data/oxford-interventions.csv")
england_npis <- npi_data[npi_data$RegionName == "England",]
england_npis$Date <- as.Date(as.character(england_npis$Date), format = "%Y%m%d")

# add the epiweek
england_npis$epiweek <- as.numeric(format(as.Date(england_npis$Date), "%V"))
# need to alter the epiweeks so it runs on from 2020 to 2021 at 2021-01-04
england_npis[england_npis$Date >= "2021-01-04",]$epiweek <- england_npis[england_npis$Date >= "2021-01-04",]$epiweek+53
# summarize by week
summ_npi <- england_npis[,c("Date", "StringencyIndex", "GovernmentResponseIndex", "ContainmentHealthIndex",
                            "EconomicSupportIndex", "epiweek")] %>%
  group_by(epiweek = epiweek) %>%
  dplyr::summarize(stringency = mean(StringencyIndex, na.rm=TRUE),
                   gov_response = mean(GovernmentResponseIndex, na.rm=TRUE),
                   containment_health = mean(ContainmentHealthIndex, na.rm=TRUE),
                   economic_support = mean(EconomicSupportIndex, na.rm = TRUE))


# now merge this into the transmission data
STP_rts <- left_join(STP_rts, summ_npi, by =  "epiweek")

### --- DONE!

# save the outputs
write.csv(STP_rts, "data/alpha_background_climate.csv", row.names = FALSE)
