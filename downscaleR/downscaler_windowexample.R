#downscaleR - working with a moving window (introducing seasonality)

#based on temperature

#observation
data(VALUE_Iberia_tas)
y.t <- VALUE_Iberia_tas
#predictors
data(NCEP_Iberia_tas)
x.t <- (NCEP_Iberia_tas)

#window = NONE
cal.t <- biasCorrection(y = y.t, x = x.t,
                      precipitation = TRUE,
                      method = "eqm",
                      wet.threshold = 0.1)

quickDiagnostics(y.t, x.t, cal.t, type = "daily", location = c(-2.0392, 43.3075))

#window = calibration window of 30 days to correct each 15 day time step
#argument specifies time window width used to calibrate and target
#days (days that are being corrected).
#Window is centered on the target day/days (window width > target days)
#default setting is NULL, which considers the whole period
#quantile-quantile plot shows not much difference with this addition of argument
#based on temperature
windowcal.t <- biasCorrection(y = y.t, x = x.t,
                          precipitation = TRUE,
                          method = "eqm",
                          window = c(30, 15),
                          wet.threshold = 0.1)

quickDiagnostics(y.t, x.t, windowcal.t, type = "daily", location = c(-2.0392, 43.3075))