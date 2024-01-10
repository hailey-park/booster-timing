# Dynamic Model Analyses

This folder contains code for running the dynamic transmission model analyses. This set of analyses were run on a high-performance computing cluster (Sherlock) due to it being more computationally intensive to run. Code was split into `sherlock-one-time` and `sherlock-sequential-analysis` folders, where "one-time" scripts ran the calibration and the "sequential-analysis" scripts ran each booster intervention (one-time, annual, semi-annual boosters) under each population targeting strategy (18+, 65+, 75+) and booster uptake assumptions (realistic, optimistic). Each combination of these (booster intervention, population-targeting strategy, uptake assumption) represents a single job. Average run-time for a single job on Sherlock is ~6 hours.


## Structure
* `data`: Contains data needed to run the dynamic model.
* `sherlock-one-time`: Contains code for running the calibration scripts.
* `sherlock-sequential-analysis`: Contains code for running each booster intervention (one-time, annual, semi-annual boosters) under each population targeting strategy (18+, 65+, 75+) and booster uptake assumptions (realistic, optimistic).

    
