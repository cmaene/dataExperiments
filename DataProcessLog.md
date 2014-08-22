Chicago Hospital Discharge Data
========================================================

Data was obtained from Dr. Peek/Nhyane on 6/2/2014. Original data format was Excel (.xlsx), size of about 17MB and burned on a DVD.

Based on MRN (patientID?) a, patients (and thus "date of birth", "residential addresses") are repeated. However, associated ICD9 codes (icd9dx_problem_list, icd9dx_encounter) are different each time of admission. Missing values are coded as NULL.

Problems:
- duplicates: a combination of MRN and BILL_NUM can be used as unique ID (to be more precise, we can also combine HOSP_ADMSN_TIME and HOSP_DISCH_TIME). However, there are still duplicates. By comparing duplicates, it appears they are identical and thus one (or two sometimes) of the duplicated records should be dropped (later, perhaps)
- icd9dx_problem_list (patient's current and active problem list)= codes can be repeated up to 44 times
- icd9dx_encounter (discharge diagnosis/reason of enounter, in the order of diagnosis/dx)= codes can be repeated up to 10 times

## Important note to myself
Because some cell values are so long (str417 for problem_list), R cannot read this Excel file using read.xlsx(xlsx). Also, when saving in CSV or Tab-delimited, Excel replaces the long string (e.g. bill_num==813987427) to a bunch of ####################################### which is not good. To work around this problem, the best is to:
(1) Import the Excel file to Stata (12 or higher because 11 or prior version can't handle long strings either)
    i.e. import excel "F:\Prof_Peek\Validation\DREQ_7906_v2.xlsx", sheet("DREQ_7906_v2") firstrow
(2) Then, export to a text file with "|" as a separator (i.e. export delimited using "F:\Prof_Peek\Validation\DREQ_7906.txt", delimiter("|") replace) This appear to work best for me.
    i.e. export delimited using "F:\Prof_Peek\Validation\DREQ_7906_v2.txt", delimiter("|") replace

ICD-9-CM codes:
I am using version 26 (10-1-2008) because it's in a nice
source: https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
(see also: http://stackoverflow.com/questions/3653811/icd-9-code-list-in-xml-csv-or-database-format)



```r
## Original variables and example of data:
## +-------------------------------------------------------------------------+
## 1. | mrn | bill_num | birth_date | ethnic_group | | 1000111 | 321987654 |
## 1/1/1950 | Not Hispanic or Latino |
## |-------------------------------------------------------------------------|
## | RACE | add_line_1 | add_li~2 | city | | White | 1234 5TH ST | NULL |
## CHICAGO |
## |-------------------------------------------------------------------------|
## | state | zip | hosp_admsn_time | hosp_disch_time | room_id | | IL | 60123
## | 01jan2010 16:09:00 | 02jan2010 13:34:00 | 123 |
## |-------------------------------------------------------------------------|
## | depart~e | center_name | service_name | admit_c~t | ed_flag | | T3NE |
## Mitchell Hospital | OTOLARYNGOLOGY | Confirmed | 0 |
## |-------------------------------------------------------------------------|
## | ed_arrival_time | bill_num_~e | icd9dx_~t | | NULL | Inpat or ED | NULL
## |
## |-------------------------------------------------------------------------|
## | icd9dx_encounter | | dx0:[145.0], dx4:[401.9], dx1:[145.0],
## dx3:[V43.64], dx2:[V58.69] |
## +-------------------------------------------------------------------------+

rm(list = ls())
# setwd('~/ProfPeek')
setwd("F:\\Prof_Peek\\Validation")
```

```
## Error: cannot change working directory
```

```r
source("install_load.R")
install_load("plyr", "stringr", "knitr", "Hmisc", "RColorBrewer", "classInt")
# read.xlsx doesn't work this data file, granted that some cell strings are
# way too long colindex<-1:20 rowindex<-1:88756
# dcdata<-read.xlsx('DREQ_7906.xlsx', sheetIndex=1, header=T,
# colIndex=colindex, rowIndex=rowindex)

# also, the following canNOT handle long strings either
# connect<-odbcConnectExcel2007('DREQ_7906.xlsx')
# dcdata<-sqlFetch(connect,'DREQ_7906') odbcClose(connect)

# some apt number has #/hash symbol, so make sure to use comment.char='',
# which disable the default behavior, i.e. treating # as a comment
# character. Read: R data import/export PDF.
dcdata <- read.table("DREQ_7906_v2.txt", header = TRUE, na.strings = "NULL", 
    fill = TRUE, sep = "|", quote = "", comment.char = "", stringsAsFactors = FALSE)
```

```
## Error: cannot open the connection
```

```r
str(dcdata)
```

```
## Error: object 'dcdata' not found
```

```r
head(dcdata)
```

```
## Error: object 'dcdata' not found
```

```r
# I don't like RACE is in upper case
colnamelist <- tolower(names(dcdata))
```

```
## Error: object 'dcdata' not found
```

```r
colnames(dcdata) <- colnamelist
```

```
## Error: object 'colnamelist' not found
```

```r

# check to see if the long string was imported/exported correctly..
weird <- dcdata[which(dcdata$bill_num == 813987427), ]  # long problem_list string..
```

```
## Error: object 'dcdata' not found
```

```r
weird <- dcdata[which(dcdata$mrn == 47820), ]  # address fields has #/hash symbol..
```

```
## Error: object 'dcdata' not found
```

```r

# convert date variables to time/POSIX objects
dcdata$dob <- strptime(dcdata$birth_date, "%d%b%Y")
```

```
## Error: object 'dcdata' not found
```

```r
head(dcdata[, c("dob", "birth_date")])  # see if it worked
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$admitdate <- strptime(dcdata$hosp_admsn_time, "%d%b%Y")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$admityear <- format(dcdata$admitdate, "%Y")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$admitmonth <- format(dcdata$admitdate, "%m")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$admitday <- format(dcdata$admitdate, "%d")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$dischargedate <- strptime(dcdata$hosp_disch_time, "%d%b%Y")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$dischargeyear <- format(dcdata$dischargedate, "%Y")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$dischargemonth <- format(dcdata$dischargedate, "%m")
```

```
## Error: object 'dcdata' not found
```

```r
dcdata$dischargeday <- format(dcdata$dischargedate, "%d")
```

```
## Error: object 'dcdata' not found
```

```r

# calculate length of stay (los) remember, our admitdate & dischargedate
# doesn't have 'time' (I didn't add when 'strptime'-ing) so los is just by
# counting days (i.e. no minding admit hour/time and discharge hour/time
# difference)
dcdata$los <- as.integer(as.numeric(difftime(dcdata$dischargedate, dcdata$admitdate, 
    units = "days")))
```

```
## Error: object 'dcdata' not found
```

```r
# check the result
head(dcdata[, c("admitdate", "dischargedate", "los")])
```

```
## Error: object 'dcdata' not found
```

```r
# plot
hist(dcdata$los)
```

```
## Error: object 'dcdata' not found
```

```r
plot(dcdata$admitdate, dcdata$los)  # not much year difference
```

```
## Error: object 'dcdata' not found
```

```r

# calculate age at the time of admission - this is not straight-forward!  if
# either birth_date or hosp_admsn_time are NULL/missing, age will be NULL as
# well first calculate a year difference, without minding month..  remember
# - we can do this because admitdate & dob are POSIX data type
dcdata$tmpyear <- dcdata$admitdate$year - dcdata$dob$year
```

```
## Error: object 'dcdata' not found
```

```r
## then calculate age by considering month & date difference
## age=year_difference if (admit month is larger than birth mongh) or (admit
## month is equal to birth month & admit date is larger than or equal to
## birth day) else age=year_difference-1.  remember - we can do this because
## admitdate & dob are POSIX data type
dcdata$age <- ifelse(dcdata$admitdate$mon > dcdata$dob$mon | (dcdata$admitdate$mon == 
    dcdata$dob$mon & dcdata$admitdate$mday >= dcdata$dob$mday), dcdata$tmpyear - 
    1, dcdata$tmpyear)
```

```
## Error: object 'dcdata' not found
```

```r
# check the result
head(dcdata[, c("admitdate", "birth_date", "dob", "tmpyear", "age")])
```

```
## Error: object 'dcdata' not found
```

```r
# drop the temp year variable
dropcol <- which(colnames(dcdata) == "tmpyear")  # get the column# of tmpyear..
```

```
## Error: object 'dcdata' not found
```

```r
dcdata <- dcdata[, -c(dropcol)]  # drop the specific column#..
```

```
## Error: object 'dcdata' not found
```

```r

# admit date & discharge dates appear to be not just 2009-2011
sort <- dcdata[order(dcdata$admitdate, na.last = FALSE), ]
```

```
## Error: object 'dcdata' not found
```

```r
tail(sort)  # what are the largest 'year' values?
```

```
##                                                                                                     
## 1 function (x, decreasing = FALSE, ...)                                                             
## 2 {                                                                                                 
## 3     if (!is.logical(decreasing) || length(decreasing) != 1L)                                      
## 4         stop("'decreasing' must be a length-1 logical vector.\\nDid you intend to set 'partial'?")
## 5     UseMethod("sort")                                                                             
## 6 }
```

```r
# see the distribution
table(format(dcdata$admitdate, "%Y"))
```

```
## Error: object 'dcdata' not found
```

```r
# create a flag variable, wrongyear==0 if either 2009|2010|2011, or ==0 if
# not
dcdata$wrongyear <- ifelse(format(dcdata$admitdate, "%Y") == 2009 | format(dcdata$admitdate, 
    "%Y") == 2010 | format(dcdata$admitdate, "%Y") == 2011, 0, 1)
```

```
## Error: object 'dcdata' not found
```

```r
badyears <- dcdata[which(dcdata$wrongyear == 1), ]  # total 2790 bad year obs.
```

```
## Error: object 'dcdata' not found
```

```r
table(badyears$department_name)  #2293/2790 (82%) are 'HOSP OUTPATIENT DEPT'
```

```
## Error: object 'badyears' not found
```

```r
table(badyears$service_name)  ##2298/2790 (82%) are 'TRANSPLANT ADMISSIONS'
```

```
## Error: object 'badyears' not found
```

```r
table(badyears$center_name)  # see if missing has to do with center_name
```

```
## Error: object 'badyears' not found
```

```r

# examine missing ICD-9 codes
sum(is.na(dcdata$icd9dx_problem_list))  #57948
```

```
## Error: object 'dcdata' not found
```

```r
sum(is.na(dcdata$icd9dx_encounter))  #5607
```

```
## Error: object 'dcdata' not found
```

```r
missingEncounter <- dcdata[which(is.na(dcdata$icd9dx_encounter)), ]
```

```
## Error: object 'dcdata' not found
```

```r
sum(is.na(missingEncounter$icd9dx_problem_list))
```

```
## Error: object 'missingEncounter' not found
```

```r
table(missingEncounter$service_name)  # see if missing has to do with Service_Name
```

```
## Error: object 'missingEncounter' not found
```

```r
# perhaps, we can use problem_list code if enounter doesn't have any code?
missingEncounter.sub <- missingEncounter[which(!is.na(missingEncounter$icd9dx_problem_list)), 
    ]  #568 obs.
```

```
## Error: object 'missingEncounter' not found
```


##STAGE 2: ICD-9 selection/flagging


```r
# sort and then add an sorted unique ID in case our merge gets lost
icd9selected <- read.table("ChicagoHospitalDischargeDataICD9codes_050214doc.txt", 
    header = TRUE, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE)
```

```
## Error: cannot open the connection
```

```r
# create a new var and fill it with ICD9 found in the ICD.9 column
icd9selected$icd9 <- regmatches(icd9selected$ICD.9, regexpr("^[0-9]*[.]*[0-9]*", 
    icd9selected$ICD.9))
```

```
## Error: object 'icd9selected' not found
```

```r
# icd9selected$icd9<-str_trim(icd9selected$icd9)
icd9selected$description <- sub("^[0-9]*[.]*[0-9]*[ ]", "", icd9selected$ICD.9)
```

```
## Error: object 'icd9selected' not found
```

```r
# drop the temp year variable
dropcol <- which(colnames(icd9selected) == "ICD.9")  # get the column# for 'ICD.9'..
```

```
## Error: object 'icd9selected' not found
```

```r
icd9selected <- icd9selected[, -c(dropcol)]  # drop the specific column#..
```

```
## Error: object 'icd9selected' not found
```

```r

# make a list of diabetes' ICD9 codes
icd9selected.diabetes <- icd9selected[icd9selected$Category == 1, 2]
```

```
## Error: object 'icd9selected' not found
```

```r
# make a list of diabetes-conditions' ICD9 codes
icd9selected.conditions <- icd9selected[icd9selected$Category != 1, 2]
```

```
## Error: object 'icd9selected' not found
```

```r

# flag using 'encounters' ICD9 codes
dcdata$encountDX_diabetes <- 0
```

```
## Error: object 'dcdata' not found
```

```r
count <- 0
for (i in 1:length(icd9selected.diabetes)) {
    exp <- paste0("\\[", icd9selected.diabetes[i])
    for (j in 1:nrow(dcdata)) {
        encounters <- dcdata[j, c("icd9dx_encounter")]
        count <- ifelse(grepl(exp, encounters), 1, 0)
        dcdata[j, c("encountDX_diabetes")] <- dcdata[j, c("encountDX_diabetes")] + 
            count
    }
}
```

```
## Error: object 'icd9selected.diabetes' not found
```

```r
dcdata$encountDX_conditions <- 0
```

```
## Error: object 'dcdata' not found
```

```r
count <- 0
for (i in 1:length(icd9selected.conditions)) {
    exp <- paste0("\\[", icd9selected.conditions[i])
    for (j in 1:nrow(dcdata)) {
        encounters <- dcdata[j, c("icd9dx_encounter")]
        count <- ifelse(grepl(exp, encounters), 1, 0)
        dcdata[j, c("encountDX_conditions")] <- dcdata[j, c("encountDX_conditions")] + 
            count
    }
}
```

```
## Error: object 'icd9selected.conditions' not found
```

```r

# flag using 'problem_list' ICD9 codes
dcdata$problemDX_diabetes <- 0
```

```
## Error: object 'dcdata' not found
```

```r
count <- 0
for (i in 1:length(icd9selected.diabetes)) {
    exp <- paste0("\\[", icd9selected.diabetes[i])
    for (j in 1:nrow(dcdata)) {
        problems <- dcdata[j, c("icd9dx_problem_list")]
        count <- ifelse(grepl(exp, problems), 1, 0)
        dcdata[j, c("problemDX_diabetes")] <- dcdata[j, c("problemDX_diabetes")] + 
            count
    }
}
```

```
## Error: object 'icd9selected.diabetes' not found
```

```r
dcdata$problemDX_conditions <- 0
```

```
## Error: object 'dcdata' not found
```

```r
count <- 0
for (i in 1:length(icd9selected.conditions)) {
    exp <- paste0("\\[", icd9selected.conditions[i])
    for (j in 1:nrow(dcdata)) {
        problems <- dcdata[j, c("icd9dx_problem_list")]
        count <- ifelse(grepl(exp, problems), 1, 0)
        dcdata[j, c("problemDX_conditions")] <- dcdata[j, c("problemDX_conditions")] + 
            count
    }
}
```

```
## Error: object 'icd9selected.conditions' not found
```

```r

# # # renaming is not easy..  renamecol<-which(colnames(dcdata)=='diabetes')
# colnamelist<-colnames(dcdata) colnamelist[renamecol]<-'encountDX_diabetes'
# colnames(dcdata)<-colnamelist # repeat for another
# renamecol<-which(colnames(dcdata)=='others') colnamelist<-colnames(dcdata)
# colnamelist[renamecol]<-'encountDX_conditions'
# colnames(dcdata)<-colnamelist

save(dcdata, file = "dcdata060614.Rda")
```

```
## Error: object 'dcdata' not found
```

```r
load("dcdata060614.Rda")

dcdata$encountDX_diabetes <- ifelse(dcdata$encountDX_diabetes > 0, 1, 0)
```

```
## Error: replacement has 0 rows, data has 88755
```

```r
dcdata$encountDX_conditions <- ifelse(dcdata$encountDX_conditions > 0, 1, 0)
```

```
## Error: replacement has 0 rows, data has 88755
```

```r
dcdata$encountDX_flag <- ifelse(dcdata$encountDX_diabetes > 0 & dcdata$encountDX_conditions > 
    0, 1, 0)
```

```
## Error: replacement has 0 rows, data has 88755
```


##STAGE 3: Geocoding and Geoprocessing (adding 2010 census tracts & community area codes)


```r
# sort and then add an sorted unique ID in case our merge gets lost
dcdata <- dcdata[order(dcdata$mrn, dcdata$bill_num, dcdata$add_line_1, dcdata$city, 
    dcdata$state, dcdata$zip), ]
dcdata$order <- seq(nrow(dcdata))
# keep only columns we need for geocoding
dcdata.geocodeready <- dcdata[, c("order", "mrn", "add_line_1", "city", "state", 
    "zip")]
# before removing duplicates, sort again with c('mrn','add_line_1', 'city',
# 'state', 'zip') just in case..
dcdata.geocodeready <- dcdata.geocodeready[order(dcdata.geocodeready$mrn, dcdata.geocodeready$add_line_1, 
    dcdata.geocodeready$city, dcdata.geocodeready$state, dcdata.geocodeready$zip), 
    ]
dcdata.geocodeready <- ddply(dcdata.geocodeready, c("mrn", "add_line_1", "city", 
    "state", "zip"), head, 1)
dcdata.geocodeready <- dcdata.geocodeready[order(dcdata.geocodeready$order), 
    ]
# spit it out for geocoding in ArcGIS Streetmap Premium
write.table(dcdata.geocodeready, file = "dcdata.geocodeready.csv", sep = ",", 
    quote = T, row.names = F)

############# geocoding in ArcGIS using Streetmap Premium 2013 Release 13 ############
############# 3452 out of 56366 obs were still not matched move to another geocoder..

############# geocoding in Stata using ArcGIS Online geocoder API ############ addrtype
############# | Freq.  Percent Cum.  --------------+-----------------------------------
############# Locality | 42 1.22 1.22 POI | 71 2.07 3.29 PointAddress | 891 25.92 29.21
############# Postal | 1,480 43.06 72.27 StreetAddress | 614 17.86 90.14 StreetName |
############# 334 9.72 99.85 SubAdmin | 5 0.15 100.00
############# --------------+----------------------------------- Total | 3,437 100.00

# PointAddress & StreetAddress are successfully geocoded ones Locality &
# SubAdmin handled well for foreign addresses (also partially 'Postal')
# still 1947 were not geocoded - mostly Locality, Postal & StreetName I
# tried to clean some obviously bad addresses/typos but they are quite a few
# to move forward, I will use another geocoder, GoogleMap (better at
# parsing/guessing) 1818 (from Postal, StreetName & the ones I cleaned
# manually) might be geocoded using GoogleMap API

############# geocoding in Stata using GoogleMap geocoder API ############ install SSC
############# geocode3

# g_quality | Freq.  Percent Cum.
# -------------------------+----------------------------------- | 2 0.11
# 0.11 APPROXIMATE | 882 48.51 48.62 - ok, as long as has g_number &
# g_street filled GEOMETRIC_CENTER | 166 9.13 57.76 - not usable, mostly
# street name only RANGE_INTERPOLATED | 336 18.48 76.24 - good!, street
# address geocoded nicely ROOFTOP | 432 23.76 100.00 - great!, street
# address geocoded perfectly
# -------------------------+----------------------------------- Total |
# 1,818 100.00 'ROOFTOP' indicates that the returned result is a precise
# geocode for which we have location information accurate down to street
# address precision.  'RANGE_INTERPOLATED' indicates that the returned
# result reflects an approximation (usually on a road) interpolated between
# two precise points (such as intersections). Interpolated results are
# generally returned when rooftop geocodes are unavailable street address.
# 'GEOMETRIC_CENTER' indicates that the returned result is the geometric
# center of a result such as a polyline (for example, a street) or polygon
# (region).  'APPROXIMATE' indicates that the returned result is
# approximate.  921 were geocoded by GoogleMap API 897 need further
# examination

############# 2nd cleaning & geocoding in Stata using GoogleMap geocoder API
############# ############ of 897, I cleaned odd addresses, POBox, by running my
############# 'parseaddress.ado'' Then, I focused on Chicago addresses only, total 291
############# (i.e. city=='CHICAGO' | city=='Chicago' | regexm(g_addr, 'Chicago')) 385
############# addresses might be saved after cleaning - sent to GoogleMap API again
############# g_quality | Freq.  Percent Cum.
############# -------------------------+----------------------------------- | 2 0.52
############# 0.52 APPROXIMATE | 174 45.19 45.71 GEOMETRIC_CENTER | 161 41.82 87.53
############# RANGE_INTERPOLATED | 16 4.16 91.69 ROOFTOP | 32 8.31 100.00
############# -------------------------+----------------------------------- Total | 385
############# 100.00 after reviewing the result carefully, I determined that only 45
############# were properly geocoded

############# FINAL cleaning & geocoding in Stata using GoogleMap geocoder API
############# ############ after reviewing results, there were a few that could have
############# been geocoded manually expecting, cleaning and then run GoggleMap geocoder
############# API 45 more were geocoded Then again, reviewing the results, finding and
############# correcting wrongly geocoded results 55373 out of 56366 (98.2%) were
############# geocoded at the street address level 993 (1.8%) were not geocoded at the
############# street address level tab levelgeocoded, missing levelgeocoded | Freq.
############# Percent Cum.  -------------------+----------------------------------- |
############# 993 1.76 1.76 APPROXIMATE | 128 0.23 1.99 PointAddress | 891 1.58 3.57
############# RANGE_INTERPOLATED | 346 0.61 4.18 ROOFTOP | 474 0.84 5.02 StreetAddress |
############# 53,528 94.97 99.99 SubAdmin | 6 0.01 100.00
############# -------------------+----------------------------------- Total | 56,366
############# 100.00 tab geocoder, missing geocoder | Freq.  Percent Cum.
############# ------------------------+----------------------------------- | 993 1.76
############# 1.76 arcgis_online | 1,511 2.68 4.44 arcgis_streetmap2013R13 | 52,914
############# 93.88 98.32 googlemap_api | 948 1.68 100.00
############# ------------------------+----------------------------------- Total |
############# 56,366 100.0

dcdata_geocoded <- read.table("dcdata_geocoded56366.txt", header = TRUE, fill = TRUE, 
    sep = "|", na.strings = ".", quote = "", comment.char = "", stringsAsFactors = FALSE)
str(dcdata_geocoded)
```

```
## 'data.frame':	56366 obs. of  13 variables:
##  $ order        : int  1 2 5 6 8 10 13 14 18 19 ...
##  $ mrn          : int  26356 47820 49778 53038 61973 71869 79959 86336 109939 112431 ...
##  $ add_line_1   : chr  "4950 S CHICAGO BEACH DR" "5550 S. SHORE" "1271 BRANDYWINE RD" "11600 HOLMES" ...
##  $ city         : chr  "CHICAGO" "CHICAGO" "CROWN POINT" "PALOS PARK" ...
##  $ state        : chr  "IL" "IL" "IN" "IL" ...
##  $ zip          : chr  "60615" "60637" "46307" "60464" ...
##  $ x            : num  -87.6 -87.6 -87.2 -87.8 -87.6 ...
##  $ y            : num  41.8 41.8 41.4 41.7 41.8 ...
##  $ matchzip     : int  60615 60637 46307 60464 60615 60615 60637 60649 60637 60619 ...
##  $ matchstate   : chr  "" "" "" "" ...
##  $ addrgeocoded : chr  "4950 S Chicago Beach Dr, Chicago, Illinois, 60615" "5550 S Shore Dr, Chicago, Illinois, 60637" "1271 Brandywine Rd, Crown Point, Indiana, 46307" "11600 S Holmes Ave, Palos Park, Illinois, 60464," ...
##  $ levelgeocoded: chr  "StreetAddress" "StreetAddress" "StreetAddress" "StreetAddress" ...
##  $ geocoder     : chr  "arcgis_streetmap2013R13" "arcgis_streetmap2013R13" "arcgis_streetmap2013R13" "arcgis_online" ...
```

```r
# get 2010 census tract FIPS
library(maptools)
# remove NA
keep <- !is.na(dcdata_geocoded[, c(7)])
dcdata_geocoded.keep <- dcdata_geocoded[keep, ]
dcdatapt <- SpatialPoints(dcdata_geocoded.keep[, c(7, 8)])
# download census tract file () tractdbf<-read.dbf('tl_2010_us_tract10.dbf')
tracts <- readShapeSpatial("tl_2010_us_tract10.shp", ID = "GEOID10")
overlay <- overlay(tracts, dcdatapt)
dcdata_geocoded.keep_tr10fips <- cbind(dcdata_geocoded.keep, overlay)
dcdata_geocoded.keep_tr10fips <- dcdata_geocoded.keep_tr10fips[, c("order", 
    "GEOID10")]
# now do it again with Community Areas
ca <- readShapeSpatial("CommAreasNAD83.shp", ID = "AREA_NUMBE")
overlay2 <- overlay(ca, dcdatapt)
dcdata_geocoded.keep_ca <- cbind(dcdata_geocoded.keep, overlay2)
dcdata_geocoded.keep_ca <- dcdata_geocoded.keep_ca[, c("order", "AREA_NUMBE", 
    "COMMUNITY")]
# since we removed NA..
dcdata_geocoded <- merge(dcdata_geocoded, dcdata_geocoded.keep_tr10fips, by.x = c("order"), 
    by.y = c("order"), all.x = TRUE)
dcdata_geocoded <- merge(dcdata_geocoded, dcdata_geocoded.keep_ca, by.x = c("order"), 
    by.y = c("order"), all.x = TRUE)
# check data
sum(is.na(dcdataxy$x))
```

```
## Error: object 'dcdataxy' not found
```

```r

# save at this point
colnamelist <- tolower(names(dcdata_geocoded))
colnames(dcdata_geocoded) <- colnamelist
save(dcdata_geocoded, file = "dcdata_geocoded.Rda")

dcdata_all <- merge(dcdata, dcdata_geocoded, by.x = c("mrn", "add_line_1", "city", 
    "state", "zip"), by.y = c("mrn", "add_line_1", "city", "state", "zip"), 
    all.x = TRUE)

# # rename is an hasstle renamecol<-which(colnames(dcdata_all)=='RACE')
# colnamelist<-colnames(dcdata_all) colnamelist[renamecol]<-'race'
# colnames(dcdata_all)<-colnamelist

save(dcdata_all, file = "dcdata_all.Rda")
load("dcdata_all.Rda")
```


##STAGE 4: Summary


```r
# keep admission years 2009-2011 only..
dcdata_0911 <- dcdata_all[dcdata_all$wrongyear != 1 & !is.na(dcdata_all$wrongyear), 
    ]
count(dcdata_0911, c("admityear"), )
```

```
##   admityear  freq
## 1      2009 28979
## 2      2010 28270
## 3      2011 28710
```

```r

# how many has diabetes-related DX?
count(dcdata_0911, c("encountDX_flag"), )
```

```
## Error: object 'encountDX_flag' not found
```

```r
dcdata_0911diabetes <- dcdata_0911[dcdata_0911$wrongyear != 1 & !is.na(dcdata_0911$wrongyear) & 
    dcdata_0911$encountDX_flag == 1, ]

# by year?
count(dcdata_0911diabetes, c("admityear", "encountDX_flag"), )
```

```
## Error: object 'encountDX_flag' not found
```

```r

# by sex
count(dcdata_0911diabetes, c("sex"), )
```

```
## [1] sex  freq
## <0 rows> (or 0-length row.names)
```

```r

# by sex & year
count(dcdata_0911diabetes, c("admityear", "sex"), )
```

```
## [1] admityear sex       freq     
## <0 rows> (or 0-length row.names)
```

```r

# by race & hispanic origin
count(dcdata_0911diabetes, c("race", "ethnic_group"), )
```

```
## Error: object 'race' not found
```

```r

# by age
summary(dcdata_0911diabetes$age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
```

```r
histg <- hist(dcdata_0911diabetes$age, col = "red")
```

```
## Error: invalid number of 'breaks'
```

```r
xfit <- seq(min(dcdata_0911diabetes$age), max(dcdata_0911diabetes$age), length = 25)
```

```
## Error: 'from' cannot be NA, NaN or infinite
```

```r
yfit <- dnorm(xfit, mean = mean(dcdata_0911diabetes$age), sd = sd(dcdata_0911diabetes$age))
```

```
## Error: object 'xfit' not found
```

```r
yfit <- yfit * diff(histg$mids[1:2]) * length(dcdata_0911diabetes$age)
```

```
## Error: object 'yfit' not found
```

```r
lines(xfit, yfit, col = "blue", lwd = 2)
```

```
## Error: object 'xfit' not found
```

```r

# by race & hispanic origin
count(dcdata_0911diabetes, c("race", "ethnic_group"), )
```

```
## Error: object 'race' not found
```

```r

# by center
count(dcdata_0911diabetes, c("center_name"), )
```

```
## [1] center_name freq       
## <0 rows> (or 0-length row.names)
```

```r

# by department
count(dcdata_0911diabetes, c("department_name"), )
```

```
## [1] department_name freq           
## <0 rows> (or 0-length row.names)
```

```r

# by service
count(dcdata_0911diabetes, c("service_name"), )
```

```
## [1] service_name freq        
## <0 rows> (or 0-length row.names)
```

```r

# by ed_flag
count(dcdata_0911diabetes, c("ed_flag"), )
```

```
## [1] ed_flag freq   
## <0 rows> (or 0-length row.names)
```

```r

# by community
count(dcdata_0911diabetes, c("community"), )
```

```
## [1] community freq     
## <0 rows> (or 0-length row.names)
```

```r

stateplane <- CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.999975 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192")
ca <- readShapeSpatial("CommAreas.shp", ID = "AREA_NUMBE", proj4string = stateplane)
dcdata_0911map <- dcdata_0911diabetes[!is.na(dcdata_0911diabetes$area_numbe), 
    c("area_numbe", "community")]
dcdata_0911map$count <- 1
```

```
## Error: replacement has 1 row, data has 0
```

```r
dcdata_mapready <- aggregate(dcdata_0911map["count"], by = dcdata_0911map["area_numbe"], 
    FUN = sum)
```

```
## Error: undefined columns selected
```

```r
ca@data <- merge(ca@data, dcdata_mapready, by.x = c("AREA_NUMBE"), by.y = c("area_numbe"), 
    all.x = TRUE)
```

```
## Error: error in evaluating the argument 'y' in selecting a method for function 'merge': Error: object 'dcdata_mapready' not found
```

```r
cntdiabetes <- ca@data$count
nclr <- 5
colors <- brewer.pal(nclr, "YlOrBr")
class <- classIntervals(cntdiabetes, nclr, style = "quantile")
```

```
## Error: var is not numeric
```

```r
colorcode <- findColours(class, colors)
```

```
## Error: Class interval object required
```

```r
plot(ca, col = colorcode)
```

```
## Error: object 'colorcode' not found
```

```r
title(main = "Diabetes-related Discharge \nCount by Community Area")
```

![plot of chunk summary](figure/summary.png) 

```r
legend("topright", legend = names(attr(colorcode, "table")), fill = attr(colorcode, 
    "palette"), cex = 0.6, bty = "n")
```

```
## Error: object 'colorcode' not found
```

```r

ca@data = data.frame(ca@data, ca[match(ca@data[, c("AREA_NUMBE")], dcdata_mapready[, 
    1]), ])
```

```
## Error: object 'dcdata_mapready' not found
```

