# Transport Health Assessment Tool for Melbourne 
(THAT-Melbourne)

## Citation

Zapata-Diomedi Belen, Ali Abbas and Alan Both (2021). Transport Health Assessment Tool for Melbourne (THAT-Melbourne). R code.

## Introduction 

The Transport Health Assessment Tool for Melbourne (THAT-Melbourne) quantifies physical activity-related health impacts arising from a range of scenarios where short car trips are replaced by walking, cycling or a combination of both, based on data from metropolitan Melbourne.

The Transport Health Assessment Tool for Melbourne models scenarios for replacing short car trips with walking, cycling or a combination of both, accommodating options for trip purpose; age groups; and sex (Table below). The model always replaces car trips. The maximum possible distance to be replaced with walking is 2 kilometres and 10 kilometres for cycling. Note that distances replaced correspond to stages of a trip, for example, a trip to work may include multiple stages including walking to a train station, train ride itself, and walking from the station to the place of employment. Trip purposes were grouped into: 1) all trips, which includes work-related, education, leisure, shopping, pick-up or drop-off someone/something, personal business, other, accompanying someone, and at or going home; 2) commuting, which includes work-related and education trips.


| Scenarios                  | Options                                            |
| -------------------------- | -------------------------------------------------- |
| Replace distance walking   | 0-1 km, 0-2 km                                     |
| Replace distance cycling   | 0-2km, 0-5km, 0-10km                               |
| Both walking and cycling   | Walking trips 0-1km & cycling trips between 1-2km  |
|                            | Walking trips 0-1km & cycling trips between 1-5km  | 
|                            | Walking trips 0-1km & cycling trips between 1-10km |  
|                            | Walking trips 0-2km & cycling trips between 2-5km  |   
|                            | Walking trips 0-2km &cycling trips between 2-10km  |   
| Trip purpose               | All, Commuting         

## Methods 
###	Overview
The model consists of three main sections: 1) Scenarios; 2) Potential Impact Fractions (PIF); and 3) Proportional multi-state life table (PMSLT). We developed a hybrid model [3], where the scenarios and PIF calculations are at the individual level (micro-simulation) and the PMSLT calculations are at the macro level (aggregated by age and sex groups) (macro-simulation). 
The micro-simulation calculates the impact of changes in physical activity due to the scenarios on disease risk at the individual level. Seven diseases related to physical activity were included: ischaemic heart disease, stroke, diabetes mellitus type 2, colon cancer, lung cancer, and for females only: breast and uterine cancers. Disease risk measures the probability of becoming diseased given exposure to a risk factor, such as physical inactivity. The potential impact fraction (PIF) measures the proportional change in disease risk when exposure to a risk factor changes. PIFs for the included diseases were estimated at the individual level and the average PIFs by age and sex were used to estimate the effect of changes in physical activity on incidence rates for the included diseases. We refer to this as the disease process in the macro-level simulation. Incidence is the number of new cases of disease over a time period and changes in incidence impact on the number of people living with a disease in later years (prevalence), quality of life and mortality. Overall changes in quality of life and mortality from all included diseases are measured within the macro-simulation via the life table, which is discussed in the sections that follow. 

The macro-simulation model used was the proportional multi-state life table (PMSLT). The model simulates 5-year age groups and sex cohorts for 1) the base case or business-as-usual scenario; and 2) for the chosen scenario, with baseline year 2017. PMSLTs for each age and sex cohort are simulated for the base case and scenario, and the difference amongst them is initiated by changes in incidence in the scenario disease processes. The population cohorts within the PMSLT are simulated until everyone dies or reaches the age of 100. 
Estimated outcomes are prevented new cases of chronic diseases and mortality, Health-adjusted life years (HALYs) and Life Years gained. HALYs represent life years adjusted for a reduction in the quality of life attributable to disability of diseases and injuries. 
 

The PMSLT for physical activity was originally developed for the Assessing the Cost-Effectiveness in Prevention project (ACE-prevention). [4] The version used here also borrows from developments for the Integrated Transport Health Impact Model (ITHIM) [5] originally developed for the United Kingdom with later adaptations used in other locations including United States, Canada and Brazil (https://www.mrc-epid.cam.ac.uk/research/research-areas/public-health-modelling/ithim/). The model was initially developed in Excel and for the version presented here we used the statistical computing software R (https://github.com/alanboth/dot-hia) and Shiny (REPO from Gus). Dr Zapata-Diomedi completed some components of the code development while on placement at the MRC Epidemiology Unit, University of Cambridge under the supervision of Dr James Woodcock.

