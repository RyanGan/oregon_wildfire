# Association between smoke from the 2013 Douglas-Complex fire and cardiopulmonary morbidity


### Created by: Ryan Gan and Jingyang Liu
Created on: 2017-11-07
### Maintained by: Ryan Gan
Last Major Update: 2017-11-07

If you have any questions or issues regarding this repository, please contact me via GitHub or the cooresponding author contact in the manuscript below.

## Overview

In the summer of 2013, the Douglas-Complex fires occured in southwest Oregon. Oregonians in this part of the state were at risk of exposure to extreme levels of particulate matter (PM). This project aim is to determine if there is an association to smoke from the Douglas-Complex fires and acute cardiopulmonary morbidity in the state of Oregon.

Data came from the Oregon All Payer All Claims Database (APAC) in the year 2013. The APAC records the health care billing data for Oregon's insured populations. APAC include individual billing records for both diagnoses codes (International Classification of Diseases, Clinical Modification (ICD-9-CM) diagnoses codes) and pharmacy codes (National Drug Codes (NDC)). 

Our previous research that found an association with wildfire smoke and respiratory outcomes in Washington state in 2012 wildfire season using a novel estimate of smoke concentration, geographically weighted ridge regression (GWR) guided the methodological approaches used in this project. As Oregon contains pharmacy records, we evaluate the association between smoke and respiratory rescue medications (beta 2 agonists) (abbreviate to SABA). 

*Research question*
We evaluated the association between smoke concentrations using the GWR method and cardiopulmonary morbidity, including ED/urgent care visits and SABA fills in Oregon state during the 2013 wildfire season.

## What's in this repository

This repository contains exposure data and shapefiles necessary to reproduce results in manuscript.

data files for smoke and shapefiles

### File naming convention
Implementing a naming structure for files

*Health outcomes naming structure:*
studyyear4digit-studystate_casecross_outcome

Naming the asthma timestratified case-crossover
Example: 2013-oregon_casecross_asthma

*Exposure naming structure:*
studyyear4digit-studystate_smoke_method

Naming the absolute PM2.5 values estimated using GWR blended method
Example: 2013-oregon_pm25_gwr
Naming the smoke PM2.5 values estimated using GWR 
Example: 2013-oregon_smokepm25_gwr

Consider doing the same for shapefiles.

Note from Ryan. Oregon git repo needs to be organized. Also, ask Jingyang to commit to development branch.

