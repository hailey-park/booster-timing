# Comparing Frequency of Booster Vaccination to Prevent Severe COVID-19 by Risk Group in the United States

This repository contains analytic code for estimating the risk of severe COVID-19 under different frequencies of booster vaccination, accounting for waning of protection against nonsevere and severe disease and differential risk by age group and immunocompromised status.

Data sources used for this analysis is publicly available. All data used for this analysis can be found in the `data` folder.

This study is now published in _Nature Communications_.

The full citation for this article is: Park et al. Comparing frequency of booster vaccination to prevent severe COVID-19 by risk group in the United States. _Nature Communications_. 2024.

## Structure
* `data`: contains all data used in this analysis (both raw and processed forms)
* `analysis`:
  * `1-waning model`: contains code for constructing the waning protection curves by risk group, and waning curves used for sensitivity analyses
  * `2-model calibration`: contains code for constructing hypothetical cohorts of 1 million individuals for each risk group and calibrating age-specific estimates of severe COVID-19 risk 
  * `3-main analysis`: contains code for running different frequencies of booster vaccination (no booster, one-time booster, annual booster, semiannual booster) over a two year time horizon
  * `4-dynamic model analysis`: contains code for running different frequencies of booster vaccination using a dynamic transmission model. See the `README.md` inside this folder for more information.
  *  `5-variant scenario analysis`: contains code for running different frequencies of booster vaccination under various novel variant scenarios
  *  `6-sensitivity analysis`: contains code for running different frequencies of booster vaccination under varying waning curve and baseline seroprevalence assumptions and scenarios (five year simulation; delayed vaccine administration, lower vaccine effectiveness after 1st dose, higher subclinical infection). See the `README.md` inside this folder for more information.
  *  `7-model validation`: contains code for running a model validation over a 3-month period
  *  `8-results processing`: contains all code needed for processing of simulation results
  *  `9-data cleaning`: contains all code needed for initial cleaning of data

* `results`: contains results for waning model predictions, calibration, and unprocessed simulation outputs. (Calibration and simulation outputs folders are empty because files are too large to be pushed to github. Users will have to generate the calibration/simulation results files on their own local machine to run the analysis).
* `figures`: contains figure outputs (jpg and pdf)
* `tables`: contains table outputs (pdf)

## Running the Main Analysis
The main analysis can be run using the `MAIN SCRIPT (USE THIS).R` script inside the `3-main analysis` folder. Running the full script should automatically run the calibration and all booster intervention simulations for each risk group (age groups, immunocompromised groups). Simulation results can be processed by calling the `simulation-results-processing.R` script inside the `8-results processing` folder. 

## Software
Analysis was conducted in R (version 4.2.1). The installation time for R and library packages necessary for this study is only a few minutes. The dynamic transmission model analyses were run on a high-performance computing cluster (Sherlock). See the `README.md` inside the `4-dynamic model analysis` folder for more information. 

## Contact 
Please direct any questions to the study authors:

Hailey Park, Stanford University, contact: Hailey.Park@stanford.edu

Nathan Lo, Stanford University, contact: Nathan.Lo@stanford.edu

