## Alpha Delta Sweep analysis

source("code/packages.R")

# load the data
# alpha sweep
alpha_sweep <- read.csv("data/alpha_background_climate.csv")

# delta sweep
delta_sweep <- read.csv("data/delta_alpha_climate.csv")

# Remove cornwall because too few cases for accurate Rt estimates
alpha_sweep <- alpha_sweep[alpha_sweep$area != "Cornwall and the Isles of Scilly",]
delta_sweep <- delta_sweep[delta_sweep$area != "Cornwall and the Isles of Scilly",]


# Gelman suggests "Subtract mean and divide by 2 stdevs" 
# "Scaling regression inputs by dividing by two standard deviations", Stats Medicine, 2008
new_scale <- function(var){
  var_mean <- mean(var, na.rm = TRUE)
  var_stdev <- sd(var, na.rm = TRUE)
  rescaled <- sapply(var, function(x) (x - var_mean)/(2*var_stdev))
  return(rescaled)
}

alpha_sweep$s.temp <- new_scale(alpha_sweep$temperature)
alpha_sweep$s.spec.humid <- new_scale(alpha_sweep$specific_humidity)
alpha_sweep$s.rel.humid <- new_scale(alpha_sweep$relative_humidity)
alpha_sweep$s.uv <- new_scale(alpha_sweep$uv)
alpha_sweep$s.precip <- new_scale(alpha_sweep$precipitation)
alpha_sweep$s.pop <- new_scale(log10(alpha_sweep$Pop_density))
alpha_sweep$s.stringency <- new_scale(alpha_sweep$stringency)
alpha_sweep$s.proportion <- new_scale(alpha_sweep$alpha_proportion)
alpha_sweep$s.time <- new_scale(alpha_sweep$epiweek)

delta_sweep$s.temp <- new_scale(delta_sweep$temperature)
delta_sweep$s.spec.humid <- new_scale(delta_sweep$specific_humidity)
delta_sweep$s.rel.humid <- new_scale(delta_sweep$relative_humidity)
delta_sweep$s.uv <- new_scale(delta_sweep$uv)
delta_sweep$s.precip <- new_scale(delta_sweep$precipitation)
delta_sweep$s.pop <- new_scale(log10(delta_sweep$Pop_density))
delta_sweep$s.stringency <- new_scale(delta_sweep$stringency)
delta_sweep$s.proportion <- new_scale(delta_sweep$delta_proportion)
delta_sweep$s.vax <- new_scale(delta_sweep$full_vax_rate)
delta_sweep$s.time <- new_scale(delta_sweep$epiweek)

####################################
## --------- MAIN CODE  --------- ##
####################################
# write a function where we supply the dataframe and variable name
# and it spits out a graph and results table

model_linear <- function(data, variable, vax){
  # first partial out the the effects of non-climate factors
  # and the temporal and spatial effects
  if(vax == TRUE){
    r.model <- lm(data[[variable]] ~ s.stringency + s.vax + s.pop + area + poly(s.time, 3), data = data)
  }else{
    r.model <- lm(data[[variable]] ~ s.stringency + s.pop + area + poly(s.time, 3), data = data) 
  }
  print(summary(r.model))
  data$cor.r0 <- residuals(r.model)
  # then compare the residuals of this model to the climate effects
  with(data, plot(cor.r0 ~ temperature, pch=20, ylab=expression(Corrected~R0)))
  # set up some prediction data for temperature
  pred <- data.frame(temperature=seq(-20,40, .01))
  # try an asymptotic model
  asymp.uk <- NA
  try(asymp.uk <- nls2(cor.r0 ~ SSasymp(temperature, a, b, c), data=data, algorithm="brute", start=data.frame(a=c(-10,10), b=c(0,10), c=c(-5,5))))
  try(asymp.uk <- nls(cor.r0 ~ SSasymp(temperature, a, b, c), data=data, algorithm="port", start=coef(asymp.uk)))
  pred$pred.asym <- NA
  try(pred$pred.asym <- predict(asymp.uk, pred))
  # do a linear model for good measure
  linear.uk <- lm(cor.r0 ~ temperature, data = data)
  pred$pred.lm <- predict(linear.uk, pred)
  lines(pred$temperature, pred$pred.asym, col="red", lwd=3)
  lines(pred$temperature, pred$pred.lm, col="blue", lwd=3)
  print(summary(lm(cor.r0 ~ s.temp + s.spec.humid + s.rel.humid + s.uv + s.precip, data = data)))
}


# -- Alpha analysis -- #
model_linear(data = alpha_sweep[alpha_sweep$epiweek<=52,], variable = "alpha_Rt", vax = FALSE)
model_linear(data = alpha_sweep[alpha_sweep$epiweek<=52,], variable = "background_Rt", vax = FALSE)

# -- Delta analysis -- #
model_linear(data = delta_sweep, variable = "delta_Rt", vax = TRUE)
model_linear(data = delta_sweep, variable = "alpha_Rt", vax = TRUE)

# Results:
# Strong climate effects during Alpha sweep - temperature is strongest predictor
# No significant climate effects during Delta sweep
# Moreover, there is less unexplained variation missing during Delta that could be explained by climate