## Alpha Delta Sweep analysis

source("code/packages.R")

# load the data
# alpha sweep
alpha_sweep <- read.csv("data/alpha_background_climate.csv")

# delta sweep
delta_sweep <- read.csv("data/delta_alpha_climate.csv")

# omicron sweep
omicron_sweep <- read.csv("data/omicron_delta_climate.csv")

# Remove cornwall because too few cases for accurate Rt estimates
alpha_sweep <- alpha_sweep[alpha_sweep$area != "Cornwall and the Isles of Scilly",]
delta_sweep <- delta_sweep[delta_sweep$area != "Cornwall and the Isles of Scilly",]

# also remove some unreasonable estimates from the omicron data
# (probably data being very sparse for those combination of stp and week)
omicron_sweep <- omicron_sweep %>% filter(Ratio < 18)

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

omicron_sweep$s.temp <- new_scale(omicron_sweep$temperature)
omicron_sweep$s.spec.humid <- new_scale(omicron_sweep$specific_humidity)
omicron_sweep$s.rel.humid <- new_scale(omicron_sweep$relative_humidity)
omicron_sweep$s.uv <- new_scale(omicron_sweep$uv)
omicron_sweep$s.precip <- new_scale(omicron_sweep$precipitation)
omicron_sweep$s.pop <- new_scale(log10(omicron_sweep$Pop_density))
omicron_sweep$s.stringency <- new_scale(omicron_sweep$stringency)
omicron_sweep$s.proportion <- new_scale(omicron_sweep$omicron_proportion)
omicron_sweep$s.vax <- new_scale(omicron_sweep$full_vax_rate)
omicron_sweep$s.time <- new_scale(omicron_sweep$epiweek)

####################################
## --------- MAIN CODE  --------- ##
####################################
# write a function where we supply the dataframe and variable name
# and it spits out a graph and results table

model_linear <- function(data, variable, vax, filename, asymp){
  # first partial out the the effects of non-climate factors
  # and the temporal and spatial effects
  if(vax == TRUE){
    r.model <- lm(data[[variable]] ~ s.stringency + s.vax + s.pop + poly(s.time, 3), data = data)
  }else{
    r.model <- lm(data[[variable]] ~ s.stringency + s.pop + area + poly(s.time, 3), data = data) 
  }
  print(summary(r.model))
  data$cor.r0 <- residuals(r.model)
  # then compare the residuals of this model to the climate effects
  # overall model summary
  print(summary(lm(cor.r0 ~ s.temp + s.spec.humid + s.rel.humid + s.uv + s.precip, data = data)))
  
  # set up some prediction data for temperature
  pred <- data.frame(temperature=seq(-20,40, .01))
  
  # make function better
  # if linear then do the linear model and make a figure
  # if asymptotic, do both models, check the AIC and do a figure with the asymptotic model
  if (asymp == FALSE){
    # just do a linear model
    linear.uk <- lm(cor.r0 ~ temperature, data = data)
    linear.predict <- predict(linear.uk, pred, se.fit = TRUE)
    pred$pred.lm <- linear.predict$fit
    pred$pred.upr <- linear.predict$fit+linear.predict$se.fit
    pred$pred.lwr <- linear.predict$fit-linear.predict$se.fit
    
    # Plot it
    outfile <- paste("results/figures/", filename, "_linear.svg", sep = "")
    
    svg(outfile)
    with(data, plot(cor.r0 ~ temperature, pch=20, ylab="", xlab = "", 
                    bty = "n", xaxt = "n", cex = 1.5))
    # add axes
    temps <- seq(round(min(data$temperature)), round(max(data$temperature)), 1)
    axis(side = 1, at = temps, labels = TRUE)
    # add fill
    polygon(c(rev(pred$temperature), pred$temperature), c(rev(pred$pred.upr), pred$pred.lwr), 
            col = adjustcolor("grey80",alpha.f=0.6), border = NA)
    # model
    lines(pred$temperature, pred$pred.lm, col="blue", lwd=3)
    # intervals
    lines(pred$temperature, pred$pred.upr, lty = 'dashed', col = "blue")
    lines(pred$temperature, pred$pred.lwr, lty = 'dashed', col = "blue")
    dev.off()
    
    print(summary(linear.uk))
  }
  
  if (asymp == TRUE){
    # try an asymptotic model
    asymp.uk <- NA
    try(asymp.uk <- nls2(cor.r0 ~ SSasymp(temperature, a, b, c), data=data, algorithm="brute", start=data.frame(a=c(-10,10), b=c(0,10), c=c(-5,5))))
    try(asymp.uk <- nls(cor.r0 ~ SSasymp(temperature, a, b, c), data=data, algorithm="port", start=coef(asymp.uk)))
    asymp.predict <- NA
    try(asymp.predict <- predict2_nls(asymp.uk, newdata = pred, interval = "conf"))
    try(asymp.predict.dat <- data.frame(method = "Delta-Method", pred,
                                        fit = asymp.predict[,1],
                                        lwr = asymp.predict[,3],
                                        upr = asymp.predict[,4]))
    # do a linear model for good measure
    linear.uk <- lm(cor.r0 ~ temperature, data = data)
    
    # Plot it
    outfile <- paste("results/figures/", filename, "_asymptotic.svg", sep = "")
    svg(filename = outfile)
    with(data, plot(cor.r0 ~ temperature, pch=20, ylab="", xlab = "", 
                    bty = "n", xaxt = "n", cex = 1.5))
    # add axes
    temps <- seq(round(min(data$temperature)), round(max(data$temperature)), 1)
    axis(side = 1, at = temps, labels = TRUE)
    # add fill
    polygon(c(rev(asymp.predict.dat$temperature), asymp.predict.dat$temperature), c(rev(asymp.predict.dat$upr), asymp.predict.dat$lwr), 
            col = adjustcolor("grey80",alpha.f=0.6), border = NA)
    # model
    lines(asymp.predict.dat$temperature, asymp.predict.dat$fit, col="red", lwd=3)
    # intervals
    lines(asymp.predict.dat$temperature, asymp.predict.dat$upr, lty = 'dashed', col = 'red')
    lines(asymp.predict.dat$temperature, asymp.predict.dat$lwr, lty = 'dashed', col = 'red')
    dev.off()
    
    print(summary(linear.uk))
    print(summary(asymp.uk))
    # check AICs
    print(paste("Asymptote model AIC:", AIC(asymp.uk)))
    print(paste("Linear model AIC:", AIC(linear.uk)))
    # do a likelihood ratio test
    # Homebrew log-likelihood test
    like.diff = logLik(asymp.uk) - logLik(linear.uk)
    df.diff = linear.uk$df.residual - summary(asymp.uk)$df[2]
    print(paste("Log-likelihood ratio test p-val", pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)))
  }
}


# -- Alpha analysis -- #
model_linear(data = alpha_sweep[alpha_sweep$epiweek<=52,], variable = "alpha_Rt", filename = "alpha", vax = FALSE, asymp = TRUE)
model_linear(data = alpha_sweep[alpha_sweep$epiweek<=52,], variable = "background_Rt", vax = FALSE)

# -- Delta analysis -- #
model_linear(data = delta_sweep, variable = "delta_Rt", filename = "delta", vax = TRUE, asymp = FALSE)
model_linear(data = delta_sweep, variable = "alpha_Rt", vax = TRUE)

# -- Omicron analysis -- #
model_linear(data = omicron_sweep, variable = "omicron_Rt", filename = "omicron", vax = TRUE, asymp = FALSE)
model_linear(data = omicron_sweep, variable = "delta_Rt", vax = TRUE)


# Results:
# Strong climate effects during Alpha sweep - temperature is strongest predictor
# No significant climate effects during Delta sweep
# Moreover, there is less unexplained variation missing during Delta that could be explained by climate
# No significant climate effects during Omicron sweep
# Hardly any left-over variation to be explained by climate.