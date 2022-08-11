########################################
# Code to produce figures/sub-figures
# for giant overview figure of
# transmission dynamics

source("code/packages.R")

# load the data
# alpha sweep
alpha_sweep <- read.csv("data/alpha_background_climate.csv")

# delta sweep
delta_sweep <- read.csv("data/delta_alpha_climate.csv")

# omicron sweep
omicron_sweep <- read.csv("data/omicron_delta_climate.csv")

# remove cornwall - too few cases for accurate Rt estimates
alpha_sweep <- alpha_sweep[alpha_sweep$area != "Cornwall and the Isles of Scilly" &
                             alpha_sweep$epiweek <= 52,]
delta_sweep <- delta_sweep[delta_sweep$area != "Cornwall and the Isles of Scilly",]

# also remove some unreasonable estimates from the omicron data
# (probably data being very sparse for those combination of stp and week)
omicron_sweep <- omicron_sweep %>% filter(Ratio < 18)

####################################
# Maps: Spatial variation in sweeps
####################################

# map data
map_data <- read_sf("~/Documents/areadata/data/gis/Sustainability_and_Transformation_Partnerships_(April_2021)_EN_BFC.shp")
names(map_data)[3] <- "area"
map_data <- map_data[map_data$area != "Cornwall and the Isles of Scilly Health and Social Care Partnership",]

map_data[map_data$area == "Sussex and East Surrey Health and Care Partnership",]$area <- "Sussex and East Surrey"
map_data[map_data$area == "Surrey Heartlands Health and Care Partnership",]$area <- "Surrey Heartlands"
map_data[map_data$area == "Frimley Health and Care ICS",]$area <- "Frimley Health"

# simplify the map for lighter plotting
library(rmapshaper)
map_data_simple <- ms_simplify(map_data, keep = 0.001,
                                keep_shapes = FALSE)


delta_map_data <- left_join(map_data_simple, delta_sweep, by = "area")
alpha_map_data <- left_join(map_data_simple, alpha_sweep, by = "area")
omicron_map_data <- left_join(map_data_simple, omicron_sweep, by = "area")

# plot out maps showing where each variant first took hold

svg("results/figures/delta_spread.svg", width = 6, height = 7)
ggplot(delta_map_data[delta_map_data$epiweek == 18,]) +
  geom_sf(aes(fill = delta_proportion)) +
  scale_fill_gradient(low = "white", high = "red", name = "Delta Proportion") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank()) +
  coord_sf(ndiscr = 0)
dev.off()

svg("results/figures/alpha_spread.svg", width = 6, height = 7)
ggplot(alpha_map_data[alpha_map_data$epiweek == 49,]) +
  geom_sf(aes(fill = alpha_proportion)) +
  scale_fill_gradient(low = "white", high = "orange", name = "Alpha Proportion") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank()) +
  coord_sf(ndiscr = 0)
dev.off()

svg("results/figures/omicron_spread.svg", width = 6, height = 7)
ggplot(omicron_map_data %>% filter(epiweek == 50)) +
  geom_sf(aes(fill = omicron_proportion)) +
  scale_fill_gradient(low = "white", high = "purple", name = "Omicron Proportion") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank()) +
  coord_sf(ndiscr = 0)
dev.off()

# and we'll plot out population density by comparison
svg("results/figures/pop_density.svg", width = 6, height = 7)
ggplot(alpha_map_data[alpha_map_data$epiweek == 49,]) +
  geom_sf(aes(fill = log10(Pop_density))) +
  scale_fill_gradient(low = "white", high = "blue", name = expression(paste("log"[10], "(People/km"^2, ")"))) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank()) +
  coord_sf(ndiscr = 0)
dev.off()

# were there differences in how vaccines progressed spatially?
ggplot(delta_map_data) +
  geom_sf(aes(fill = full_vax_rate)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Fully Vaccinated") +
  facet_wrap(~epiweek) +
  theme_bw()

# and how about Rt?
ggplot(delta_map_data) +
  geom_sf(aes(fill = delta_Rt)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Delta Rt") +
  facet_wrap(~epiweek) +
  theme_bw()

ggplot(alpha_map_data) +
  geom_sf(aes(fill = alpha_Rt)) +
  scale_fill_gradient(low = "white", high = "blue", name = "Alpha Rt") +
  facet_wrap(~epiweek) +
  theme_bw()


################################
# Temporal variation in sweeps
################################

head(alpha_sweep)

# lets get means which we can plot as lines
alpha_summary <- alpha_sweep %>%
  group_by(epiweek) %>%
  summarise(mean_stringency = mean(stringency),
            mean_temp = mean(temperature),
            mean_Rt = mean(alpha_Rt))

delta_summary <- delta_sweep %>%
  group_by(epiweek) %>%
  summarise(mean_stringency = mean(stringency),
            mean_temp = mean(temperature),
            mean_Rt = mean(delta_Rt),
            mean_vax = mean(full_vax_rate))

omicron_summary <- omicron_sweep %>%
  group_by(epiweek) %>%
  summarise(mean_stringency = mean(stringency),
            mean_temp = mean(temperature),
            mean_Rt = mean(omicron_Rt),
            mean_vax = mean(second_dose))

# add some nicely formatted dates for axes
alpha_summary$week <- c("Nov-02", "Nov-09", "Nov-16", "Nov-23", "Nov-30", "Dec-07", "Dec-14", "Dec-21")
delta_summary$week <- c("May-03", "May-10", "May-17", "May-24", "May-31", "Jun-07", "Jun-14", "Jun-21", "Jun-28")
omicron_summary$week <- c("Nov-22", "Nov-29", "Dec-06", "Dec-13")

#############################################
# FIG START
svg("results/figures/alpha_temporal_fig.svg", width = 10, height = 6)
#Define Margins. The trick is to use give as much space possible on the left margin (second value)
par(mar=c(5, 12, 4, 4) + 0.1)

#Plot the first time series. Notice that you don’t have to draw the axis nor the labels
with(alpha_sweep, boxplot(alpha_Rt ~ epiweek, axes=F, xlab="", ylab="", main="", xlim = c(0.8, 8.2), ylim = c(0.5, 3.5)))
# with(alpha_sweep, boxplot(alpha_Rt ~ epiweek))
# with(alpha_summary, plot(epiweek, mean_Rt, axes=F, ylim=c(min(mean_Rt),max(mean_Rt+0.2)), xlab="", ylab="", type="l", col="black", main="", 
#                          xlim=c(45, 52)))
# with(alpha_summary, points(epiweek, mean_Rt, pch=20,col="black"))
with(alpha_summary, axis(2, ylim=c(min(mean_Rt),max(mean_Rt)), col="black", lwd=2))
mtext(2,text=expression("R"[t]),line=1.8, cex = 1.3)

#Plot the second time series. The command par(new=T) is handy here. 
#If you just need to plot two timeseries, you could also use the right vertical axis as well. 
#In that case you have to substitute “2” with “4” in the functions axis() and mtext(). 
#Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. 
#You don’t need to increase the value if you use the right vertical axis.

par(new=T)
with(alpha_summary, plot(epiweek, mean_stringency, axes=F, ylim=rev(c(min(mean_stringency-1),max(mean_stringency+1))), xlab="", ylab="", 
                         type="l", lty=2, main="", col = "red", xlim=c(44.8,52.2), lwd=2))
with(alpha_summary, axis(2, ylim=c(min(mean_stringency),max(mean_stringency)),lwd=2,line=3.5,cex=1))
with(alpha_summary, points(epiweek, mean_stringency,pch=20, col = "red", cex = 1.5))
mtext(2,text="Stringency Index",line=5.5, cex = 1.3)

#Plot the third time series. Again the line parameter are both further increased.
par(new=T)
with(alpha_summary, plot(epiweek, mean_temp, axes=F, ylim=rev(c(min(mean_temp-1),max(mean_temp+1))), xlab="", ylab="",
                         type="l", lty=3, main="", col = "blue", xlim=c(44.8,52.2),lwd=2))
with(alpha_summary, axis(2, ylim=c(min(mean_temp),max(mean_temp)), lwd=2, line=7))
with(alpha_summary, points(epiweek, mean_temp, col = "blue", pch=20, cex = 1.5))
mtext(2,text=expression(paste("Temperature (",degree, "C)")),line=9, cex = 1.3)

#We can now draw the X-axis, which is of course shared by all the three time-series.
with(alpha_summary, axis(1,at = epiweek, labels = week))
mtext("2020 Week Beginning", side=1, col="black", line=2.5, cex = 1.3)

#And then plot the legend.
legend(x=44.6, y=4.1,legend=c(expression("R"[t]), "Stringency Index", expression(paste("Temperature (",degree, "C)"))), lwd = 3, lty=c(1,2,3), 
       col = c("black", "red", "blue"), cex = 1.3, bty = "n")
dev.off()
# FIG END
##########################################################


#
### repeat similar (plus Vaccinations?) for Delta
#

#############################################
# FIG START
svg("results/figures/delta_temporal_fig.svg", width = 10, height = 6)
#Define Margins. The trick is to use give as much space possible on the left margin (second value)
par(mar=c(5, 12, 4, 4) + 0.1)

#Plot the first time series. Notice that you don’t have to draw the axis nor the labels
with(delta_sweep, boxplot(delta_Rt ~ epiweek, axes=F, xlab="", ylab="", main="", xlim = c(0.8,9.2), ylim = c(0.5,2.5)))
# with(delta_sweep, boxplot(delta_Rt ~ epiweek))
# with(delta_summary, plot(epiweek, mean_Rt, axes=F, ylim=c(min(mean_Rt),max(mean_Rt+0.2)), xlab="", ylab="", type="l", col="black", main="", 
#                          xlim=c(45, 52)))
# with(delta_summary, points(epiweek, mean_Rt, pch=20,col="black"))
with(delta_summary, axis(2, ylim=c(min(mean_Rt),max(mean_Rt)), col="black", lwd=2))
mtext(2,text=expression("R"[t]),line=1.8, cex = 1.3)

#Plot the second time series. The command par(new=T) is handy here. 
#If you just need to plot two timeseries, you could also use the right vertical axis as well. 
#In that case you have to substitute “2” with “4” in the functions axis() and mtext(). 
#Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. 
#You don’t need to increase the value if you use the right vertical axis.

par(new=T)
with(delta_summary, plot(epiweek, mean_stringency, axes=F, ylim=rev(c(min(mean_stringency-1),max(mean_stringency+1))), xlab="", ylab="", 
                         type="l", lty=2, main="", col = "red", xlim=c(17.8,26.2), lwd=2))
with(delta_summary, axis(2, ylim=c(min(mean_stringency),max(mean_stringency)),lwd=2,line=3.5))
with(delta_summary, points(epiweek, mean_stringency,pch=20, col = "red", cex = 1.5))
mtext(2,text="Stringency Index",line=5.5, cex = 1.3)

#Plot the third time series. Again the line parameter are both further increased.
par(new=T)
with(delta_summary, plot(epiweek, mean_temp, axes=F, ylim=rev(c(min(mean_temp-1),max(mean_temp+1))), xlab="", ylab="",
                         type="l", lty=3, main="", col = "blue", xlim=c(17.8,26.2),lwd=2))
with(delta_summary, axis(2, at = c(9:17), lwd=2, line=7))
with(delta_summary, points(epiweek, mean_temp, col = "blue", pch=20, cex = 1.5))
mtext(2,text=expression(paste("Temperature (",degree, "C)")),line=9, cex = 1.3)

#We can now draw the X-axis, which is of course shared by all the three time-series.
with(delta_summary, axis(1,at = epiweek, labels = week))
mtext("2021 Week Beginning", side=1, col="black", line=2.5, cex = 1.3)

#And then plot the legend.
# legend(x=46.8, y=12.4,legend=c(expression("R"[t]), "Stringency Index", expression(paste("Temperature (",degree, "C)"))), lwd = 3, lty=c(1,2,3), 
#        col = c("black", "red", "blue"), cex = 1.3)
dev.off()
# FIG END
##########################################################

#
### repeat again for Omicron!
#

#############################################
# FIG START
svg("results/figures/omicron_temporal_fig.svg", width = 10, height = 6)
#Define Margins. The trick is to use give as much space possible on the left margin (second value)
par(mar=c(5, 12, 4, 4) + 0.1)

#Plot the first time series. Notice that you don’t have to draw the axis nor the labels
with(omicron_sweep, boxplot(omicron_Rt ~ epiweek, axes=F, xlab="", ylab="", main="", xlim = c(0.5,4.5), ylim = c(0,8.5)))
# with(delta_sweep, boxplot(delta_Rt ~ epiweek))
# with(delta_summary, plot(epiweek, mean_Rt, axes=F, ylim=c(min(mean_Rt),max(mean_Rt+0.2)), xlab="", ylab="", type="l", col="black", main="", 
#                          xlim=c(45, 52)))
# with(delta_summary, points(epiweek, mean_Rt, pch=20,col="black"))
with(omicron_summary, axis(2, ylim=c(min(mean_Rt),max(mean_Rt)), col="black", lwd=2))
mtext(2,text=expression("R"[t]),line=1.8, cex = 1.3)

#Plot the second time series. The command par(new=T) is handy here. 
#If you just need to plot two timeseries, you could also use the right vertical axis as well. 
#In that case you have to substitute “2” with “4” in the functions axis() and mtext(). 
#Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. 
#You don’t need to increase the value if you use the right vertical axis.

par(new=T)
with(omicron_summary, plot(epiweek, mean_stringency, axes=F, ylim=rev(c(min(mean_stringency-1),max(mean_stringency+1))), xlab="", ylab="", 
                         type="l", lty=2, main="", col = "red", xlim=c(46.5,50.5), lwd=2))
with(omicron_summary, axis(2, ylim=c(min(mean_stringency),max(mean_stringency)),lwd=2,line=3.5))
with(omicron_summary, points(epiweek, mean_stringency,pch=20, col = "red", cex = 1.5))
mtext(2,text="Stringency Index",line=5.5, cex = 1.3)

#Plot the third time series. Again the line parameter are both further increased.
par(new=T)
with(omicron_summary, plot(epiweek, mean_temp, axes=F, ylim=rev(c(min(mean_temp-1),max(mean_temp+1))), xlab="", ylab="",
                         type="l", lty=3, main="", col = "blue", xlim=c(46.5,50.5),lwd=2))
with(omicron_summary, axis(2, at = c(4:8), lwd=2, line=7))
with(omicron_summary, points(epiweek, mean_temp, col = "blue", pch=20, cex = 1.5))
mtext(2,text=expression(paste("Temperature (",degree, "C)")),line=9, cex = 1.3)

#We can now draw the X-axis, which is of course shared by all the three time-series.
with(omicron_summary, axis(1,at = epiweek, labels = week))
mtext("2021 Week Beginning", side=1, col="black", line=2.5, cex = 1.3)

#And then plot the legend.
# legend(x=46.8, y=12.4,legend=c(expression("R"[t]), "Stringency Index", expression(paste("Temperature (",degree, "C)"))), lwd = 3, lty=c(1,2,3), 
#        col = c("black", "red", "blue"), cex = 1.3)
dev.off()
# FIG END
##########################################################



dates_2021 <- data.frame("Date" = seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))
dates_2021$week <- format(as.Date(dates_2021$Date), "%V")
