########################################
# Code for supplementary things
# e.g. climate correlates
# 

source("code/packages.R")

# load the data
# alpha sweep
alpha_sweep <- read.csv("data/alpha_background_climate.csv")

# delta sweep
delta_sweep <- read.csv("data/delta_alpha_climate.csv")

# remove cornwall - too few cases for accurate Rt estimates
alpha_sweep <- alpha_sweep[alpha_sweep$area != "Cornwall and the Isles of Scilly" &
                             alpha_sweep$epiweek <= 52,]
delta_sweep <- delta_sweep[delta_sweep$area != "Cornwall and the Isles of Scilly",]


# check how correlated different climate variables are with each other

pairs(alpha_sweep[,c("temperature", "specific_humidity", "relative_humidity", "uv", "precipitation")])
pairs(delta_sweep[,c("temperature", "specific_humidity", "relative_humidity", "uv", "precipitation")])

length(unique(alpha_sweep$epiweek))
length(unique(delta_sweep$epiweek))

alphapalette <- brewer.pal(8, "Greens")
deltapalette <- brewer.pal(9, "Greens")


alphacols <- character(nrow(alpha_sweep))
alphacols[] <- alphapalette[1]

alphacols[alpha_sweep$epiweek == 46] <- alphapalette[2]
alphacols[alpha_sweep$epiweek == 47] <- alphapalette[3]
alphacols[alpha_sweep$epiweek == 48] <- alphapalette[4]
alphacols[alpha_sweep$epiweek == 49] <- alphapalette[5]
alphacols[alpha_sweep$epiweek == 50] <- alphapalette[6]
alphacols[alpha_sweep$epiweek == 51] <- alphapalette[7]
alphacols[alpha_sweep$epiweek == 52] <- alphapalette[8]

svg("results/figures/alpha_climate_correlates.svg", width = 9, height = 9)
pairs(alpha_sweep[,c("temperature", "specific_humidity", "relative_humidity", "uv", "precipitation")], col=alphacols, pch=19)
dev.off()


deltacols <- character(nrow(delta_sweep))
deltacols[] <- deltapalette[1]

deltacols[delta_sweep$epiweek == 18] <- deltapalette[2]
deltacols[delta_sweep$epiweek == 19] <- deltapalette[3]
deltacols[delta_sweep$epiweek == 20] <- deltapalette[4]
deltacols[delta_sweep$epiweek == 21] <- deltapalette[5]
deltacols[delta_sweep$epiweek == 22] <- deltapalette[6]
deltacols[delta_sweep$epiweek == 23] <- deltapalette[7]
deltacols[delta_sweep$epiweek == 24] <- deltapalette[8]
deltacols[delta_sweep$epiweek == 25] <- deltapalette[9]

svg("results/figures/delta_climate_correlates.svg", width = 9, height = 9)
pairs(delta_sweep[,c("temperature", "specific_humidity", "relative_humidity", "uv", "precipitation")], col=deltacols, pch=19)
dev.off()