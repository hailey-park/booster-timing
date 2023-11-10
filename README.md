# Comparison of Timing of Booster Vaccination for COVID-19 to Prevent Severe Disease by Risk Group in the United States

The code will be updated at the time of publication

This repository contains analytic code for estimating the risk of severe COVID-19 under different frequencies of booster vaccination, accounting for waning of protection against nonsevere and severe disease and differential risk by age group and immunocompromised status.

Data sources used for this analysis is publicly available. 

## Structure
* `data`: contains all data used in this analysis (both raw and processed forms)
* `analysis`:
  * `1-waning model`: contains code for constructing the waning protection curves by risk group, and waning curves used for sensitivity analyses
  * `2-model calibration`: contains code for constructing hypothetical cohorts of 1 million individuals for each risk group and calibrating age-specific estimates of severe COVID-19 risk 
  * `3-main analysis`: contains code for running different frequencies of booster vaccination (no booster, one-time booster, annual booster, semiannual booster) over a two year time horizon
  * `4-dynamic model analysis`: contains code for running different frequencies of booster vaccination using a dynamic transmission model
  *  `5-variant scenario analysis`: contains code for running different frequencies of booster vaccination under various novel variant scenarios
  *  `6-sensitivity analysis`: contains code for running different frequencies of booster vaccination under varying assumptions and scenarios
  *  `7-model validation`: contains code for running a model validation over a 3-month period
  *  `8-data cleaning`: contains all code needed for initial cleaning of data
* `results`: contains results for waning model predictions, calibration, and unprocessed simulation outputs
* `figures`: contains figure outputs (jpg and csv)
* `tables`: contains table outputs (jpg and csv)
