---
title: "eg social"
author: "CK, WRC"
date: "2018-12-07"
output: 
  html_document:
    keep_md: true
    highlight: pygments
    theme: spacelab
    toc: true
    toc_float:
       collapsed: false
---

# Redefining association: North Atlantic right whales within communication range

### Introduction

> Baleen whales lack the strong and stable long-term bonds characteristic of odontocetes such as killer whales and sperm whales, and therefore, the social structure of baleen whales has received comparatively little attention. However, recent findings of long-term associations in both right whales and humpback whales have challenged the notion that baleen whale society is characterized by short and unstable associations. Previous research has used “within 2 body lengths” to define association, while recognizing that this definition underestimates the range at which whales interact acoustically. We take a new approach and define association in North Atlantic right whales based on communication range. We analyzed a rich dataset of 41,301 sightings of individually identified right whales from 1981-2009 throughout their geographic range. We calculated the mean and maximum association indices between and within age-sex classes (adult males, adult lactating females, adult non-lactating females, juvenile males, and juvenile females) and found significant preferred associations using permutation tests. Lactating females had the lowest rates of association across all age-sex classes as is common among mammals. The highest rates of association were found among juveniles with other juveniles, and adult males with other adult males. High rates of association among juveniles are common in mammals as the period between weaning and adulthood is important for the formation of social relationships and learning. The high rates of association among adult males are consistent with previous work on right whales and in contrast to associations in adult male humpback whales, likely due to differences in the mating systems of the two species. These results are consistent with our current understanding of right whale social structure using the “within 2 body lengths” definition of association and support the hypothesis that right whales are interacting acoustically at ranges out to at least 10 kilometers.

----

### Analysis Decisions


**numsamp** > 35

**distance** = 10km for most analysis 
(also looked at 0.03 "2 body lengths", 1km, 5km, 10km, 15km, 20km)

**sampling period:** use 'Day' (see Whitehead book p.79)

**swim speed** = 3.1 km/hr

**Mantel tests** : used "multiple measures" module as that made it easier to import the custom adjusted matrix

----

### Dataset Summary

Photographs of right whales and associated life history data were obtained from The North Atlantic Right Whale Consortium

Phil Hamilton retrieved the data for this request on 11/6/2017 which consisted of 

65,827 sightings of individually identified right whales from 1980-2016


----

### Data Prep


#### 0\_RUN\_TESTER\_prepsightdat.r

this script is set up to run egsocial\_agesex\_assignment.R and then calculate\_available.R.

#### egsocial\_agesex\_assignment.R

right now this script loads the raw data files. this would be better to do up a level in the organization script (right now 0\_RUN\_TESTER\_prepsightdat.r).

**inputs**: 5-Khan-data-cleaned-up.csv, 2017-11-06-Khan-data-request-calving.csv

**outputs**: birthdata.csv, `dat` (dataframe, not currently exported)

to create 5-Khan-data-cleaned-up.csv the following steps were taken:

- open data and Save As in Excel file with a new name
- delete 'Calving Data' worksheet
- delete 'Code Explanation' worksheet
- delete top row header that says 'All sightings of identified whales from 1980 through 2016'
- highlight all and remove shading and borders   
- highlight top row and replace all spaces with nothing
- highlight all and replace all spaces with a period
- highlight all and replace all commas with a period
- highlight all and replace all double periods with a single period
- highlight all and replace all blanks with 'NA' values
- delete column "Association Id"
- delete column "Association Type"
- delete column "Singleton"
- delete column "Observer"
- delete column "Area Code"
- delete column "Letter"
- delete any whales (EGNo) of unknown gender 'X'
- moved "Latitude" and "Longitude" columns over to the left, and formatted to 5 decimal places
- insert a RID column for row names
- save
- save as a .txt file

this script applies the following rules:

- retain records which have at least a day mgonth year.
- calves are excluded
- you are a calf until December 1st of birth year and afterwards you will be a juv
- considered an adult if:
    1. it has been nine years or more since known birth year
    2. OR eight years or more ellapsed since date of first sighting (when birth year is unknown)
    3. OR Jan 1st of the year prior to giving birth to a calf
- whales with ak nown birth year seen for less than eight years were excluded (unknown age class)
- lactating:
    1. female are considered lactating from their first sighting with a dependent calf until dec 1st of the calving year.
    2. if a calf is lost; mother was considered lactating until the last sighting of calf, and non-lactating afterwards.

#### calculate\_available.R

this script calculates the daily overlap for "alive" ids.

**inputs**: `dat` (dataframe)

**outputs**: `dat` (dataframe), `nax_avail` (matrix)

- `nax_avail` is a matrix is in the style of Hal Whitehead's nax in [SocProg](http://http://whitelab.biology.dal.ca/SOCPROG/social.htm).

**functions**: `naxlook(nax, nids, ny, uids, uy, ylabs, xgrid, ygrid, ...)`
a helper function to make a graphical representation of nax for error checking etc.

notes:

- this script ignores unknown age ids: UM, UF.
- UUs, JUs, and AUs have already been removed at this stage

- rules for assigning agesex class is the same as in egsocial\_agesex\_assignment.R with the following addition made neccessary by going day by day:
    - lactating females are lactating from the first day they are seen with a calf until 11-30 of the calfcohort year or until the last sighting of the mom and calf. this could be modified if for instance a standard start day is preferable.