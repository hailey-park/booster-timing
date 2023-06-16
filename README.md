# Comparison of Timing of Bivalent Booster Vaccination for COVID-19 to Prevent Severe Disease by Risk Group
This repository contains analytic code for estimating the risk of severe COVID-19 under different frequencies of bivalent booster vaccination, accounting for waning of protection against severe disease and differential risk by age group.

Data sources used for this analysis is publicly available. 


## Structure
* `1-data cleaning`: contains all code needed for initial cleaning of data
* `2-analysis`:
 *`1-waning model`: contains code for constructing the waning protection curves by risk group, and waning curves used for sensitivity analyses
 * `2-model calibration`: contains code for constructing hypothetical cohorts of 1 million individuals for each risk group and calibrating age-specific estimates of severe COVID-19 risk 
 * `3-simulation main`: contains code for running different frequencies of bivalent booster vaccination (no booster, one-time booster, annual booster, semiannual booster) over a two year time horizon
 * `4-simulation five years`: contains code for running different frequencies of bivalent booster vaccination over a five year time horizon
 * `5-simulation delayed vaccine admin`: contains code for running different frequencies of bivalent booster vaccination with delayed vaccine rollout (6-months)
 * `6-simulation lower ve after 1st dose`: contains code for running different frequencies of bivalent booster vaccination with assumption of lower vaccine effectiveness after the 1st bivalent booster dose
 * `7-model validation`: contains code for running a model validation over a 3-month period 
* `figures tables`: contains figure and table outputs (jpg and csv)
