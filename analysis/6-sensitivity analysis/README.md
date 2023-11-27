# Sensitivity Analyses

This folder contains code for running all the sensitivity analyses. We created separate code folders for analyses that utilize the calibrated risk groups from the main analysis but make adjustments to the booster interventions themselves. These include the `delayed-vacc-admin`, `five-year-sim`, and `lower-vax-eff` analyses. All other sensitivity analyses testing varying assumptions on waning curves and baseline seroprevalence levels can be run in the `all-other-sa` folder.


## Structure
* `delayed-vacc-admin`:  Contains code for running different frequencies of booster vaccination with delayed vaccine rollout (6-months). 
* `five-year-sim`: Contains code for running different frequencies of booster vaccination over a five year time horizon
* `lower-vax-eff`: Contains code for running different frequencies of booster vaccination with assumption of lower vaccine effectiveness after the 1st booster dose
* `all-other-sa`: Contains code for running different frequencies of booster vaccination under varying waning curve and baseline seroprevalence assumptions. A separate calibration must be run for each of these analyses (see `2-model calibration` for more information) prior to running the booster interventions. However, all of these analyses utilize the same booster intervention scripts. A list of the sensitivity analyses with varied waning curve and seroprevalence assumptions are listed below:
  * `optimistic-ve`
  * `optimistic-waning`
  * `pessimistic-ve`
  * `pessimistic-waning`
  *  `100-sero`
  *  `low-sero`
  *  `high-sero`
    
