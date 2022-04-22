# NY_SHOOTING

---
title: "NYPD_Final_1"
author: "H. Gravela"
date: "4/21/2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.
## from: https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
## First specify the packages needed
packages = c("ggmap", "ggplot2", "knitr", "lubridate", "readxl", "tidyverse", "caTools", "rpart.plot")
## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

```

## Data Background

List of every shooting incident that occurred in NYC going back to 2006 through the end of 2020.

This is a breakdown of every shooting incident that occurred in NYC going back to 2006 through the end of the year of 2020. This data is manually extracted every quarter and reviewed by the Office of Management Analysis and Planning before being posted on the NYPD website. Each record represents a shooting incident in NYC and includes information about the event, the location and time of occurrence. In addition, information related to suspect and victim demographics is also included. This data can be used by the public to explore the nature of shooting/criminal activity.


The data and data dictionary are available at <https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8>.
Footnotes accompanying the data are available at
<https://data.cityofnewyork.us/api/views/833y-fsy8/files/e4e3d86c-348f-4a16-a17f-19480c089429?download=true&filename=NYPD_Shootings_Incident_Level_Data_Footnotes.pdf>.

## Importing "NYPD Shooting Incident" dataset

```{r get raw data, echo=FALSE}
url_in <-"https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
nypd_shooting <- read.csv(url_in)
# preliminary checking of data
str(nypd_shooting)
```


## Data Summary

```{r summarize, echo=FALSE}

summary(nypd_shooting)
```

## Data Exploration

**1. Start exploring the whole data.**

```{r eval=  FALSE}

head(nypd_shooting)
str(nypd_shooting)
summary(nypd_shooting)
colnames(nypd_shooting)
ncol(nypd_shooting)
nrow(nypd_shooting)

```


```{r eval = TRUE,  include = FALSE}

head(nypd_shooting)

```

**2. Exploring different columns.**

```{r eval =  FALSE}

table(nypd_shooting$PERP_AGE_GROUP)
table(nypd_shooting$PERP_SEX)
table(nypd_shooting$BORO)

```

```{r echo =  FALSE}

table(nypd_shooting$PERP_AGE_GROUP)
table(nypd_shooting$PERP_SEX)
table(nypd_shooting$BORO)

```

## Data Cleaning and Wrangling

**1.  Change the date and time format.**


```{r eval = TRUE}

nypd_shooting$OCCUR_TIME = strptime(nypd_shooting$OCCUR_TIME, format = '%H:%M')

nypd_shooting = nypd_shooting %>%
  mutate(OCCUR_DATE = mdy(OCCUR_DATE)) %>%
  mutate(WEEKDAY = weekdays(OCCUR_DATE)) %>%
  mutate(HOUR = hour(OCCUR_TIME)) %>%
  mutate(YEAR = year(OCCUR_DATE))

```


**2. Impute missing values in PERP_SEX and PERP_AGE_GROUP.**

```{r eval = TRUE, include = TRUE}

nypd_shooting = nypd_shooting %>%
  mutate(across(where(is.character), ~ na_if(.,"")))

set.seed(8888) #set seed to ensure single randomization
perp_sex_logical = complete.cases(nypd_shooting$PERP_SEX) #determine which rows are NA'S
nypd_shooting = cbind(nypd_shooting, perp_sex_logical)  #adding column
nypd_shooting_perp_sex = subset(nypd_shooting, perp_sex_logical == FALSE) #subsetting NA's
sexes = c("M", "F", "U")
prob = c(0.88,0.02,0.1)
nypd_shooting_perp_sex$PERP_SEX = sample(sexes, 8261, replace = TRUE, prob = prob) #simulation
nypd_shooting = subset(nypd_shooting, perp_sex_logical == TRUE) #rows that are complete
nypd_shooting = rbind(nypd_shooting, nypd_shooting_perp_sex) #combine imputed values



set.seed(9999)
perp_age_logical = complete.cases(nypd_shooting$PERP_AGE_GROUP)
nypd_shooting = cbind(nypd_shooting,perp_age_logical)
nypd_shooting_perp_age = subset(nypd_shooting, perp_sex_logical == FALSE)
ages = c("<18", "18-24", "25-44", "45-64", "65+", "UNKNOWN")
nypd_shooting_perp_age$PERP_AGE_GROUP = sample(ages, 8261, TRUE, c(0.09,0.36,0.31, 0.03, 0.00, .21))
nypd_shooting = subset(nypd_shooting, perp_age_logical == TRUE)
nypd_shooting = rbind(nypd_shooting, nypd_shooting_perp_age)
```


**3. Removing outliers.**

``` {r eval = TRUE, include = TRUE}
nypd_shooting = subset(nypd_shooting, PERP_AGE_GROUP != ('1020'))
nypd_shooting = subset(nypd_shooting, PERP_AGE_GROUP != ('224'))
nypd_shooting = subset(nypd_shooting, PERP_AGE_GROUP != ('940'))

```


**4. Selecting columns needed.**


```{r eval = TRUE, include = TRUE}
nypd_shooting = nypd_shooting %>%
  select(c(YEAR, WEEKDAY, HOUR, BORO, PERP_SEX, PERP_AGE_GROUP, Latitude, Longitude, STATISTICAL_MURDER_FLAG, LOCATION_DESC))


```


## Visualization 

**1. Visualize shooting incidents occured annually**.

```{r echo = FALSE}

annual = as.data.frame(table(nypd_shooting$YEAR))
annual_shooting = ggplot(annual,aes(x = Var1, y = Freq))+geom_line(group =1, color = "red", size = 2)
annual_shooting + xlab("Year") + ylab("Incidents") + ggtitle("Annual Shootings Incidents in New York")


```


**2. Visualize Boro**

``` {r echo = FALSE, fig.height = 10, fig.width = 10}
boroughs = ggplot(nypd_shooting) + geom_bar(aes(x = BORO), fill = 'blue')
boroughs + xlab("Boroughs") + ylab("Incidents") + ggtitle("Shooting Incidents in NY Borough")
```


**3. Visualize Demographics**

``` {r echo = FALSE}

demographics = ggplot(nypd_shooting) + geom_bar(aes(x = PERP_AGE_GROUP, fill = PERP_SEX)) 
demographics + xlab("AGE GROUP") + ylab("No. of Incident") + ggtitle("Shooting Incidents Demographics")

```


**4. Visualize Shooting Timings**

``` {r echo = FALSE}

weekOrder = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
nypd_shooting$WEEKDAY = factor(nypd_shooting$WEEKDAY, ordered = TRUE, levels = weekOrder)
table(nypd_shooting$WEEKDAY, nypd_shooting$HOUR)
DayHourCounts = as.data.frame(table(nypd_shooting$WEEKDAY, nypd_shooting$HOUR))
str(DayHourCounts)

DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))



ggplot(DayHourCounts, aes(y = Hour, x = Var1)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = "Total Shooting Incidents", low = "orange", high = "red") + 
  theme(axis.title.x = element_blank()) + ggtitle("Heatmap of Shooting Timings")


```

**5. Visualize Incident Geographics**

```{r eval = TRUE, include = FALSE}

api = "AIzaSyB99Xn_RU9qpLEe3wndlh4zbjmbeUDWszQ"
register_google(key = api)

NewYork = get_map(location = "New York", zoom = 11 )



LatLongCounts = as.data.frame(table(round(nypd_shooting$Latitude,2), round(nypd_shooting$Longitude,2)))

str(LatLongCounts)
LatLongCounts$Lat = as.numeric(as.character(LatLongCounts$Var1))
LatLongCounts$Lon = as.numeric(as.character(LatLongCounts$Var2))
```




```{r eval = TRUE, include = FALSE}
na.omit(LatLongCounts)
```


```{r eval = TRUE, warning = FALSE}
ggmap(NewYork) + geom_tile(data = LatLongCounts, aes(x = Lon, y = Lat, alpha = Freq), fill = "red") + 
scale_color_gradient(low = "yellow", high = "red") + 
  ggtitle("GeoModeling of Shooting Incidents in New York")

```


## Analysis

**1.  Year on year incidents of shooting has sharply decreased from 2006 to 2019 by almost 50%.  In the year 2020, shooting incidents increased to almost 100% comparatively to the level when it was 10 years ago.** 

**2.  Shooting incidents in New York happen in the boroughs of Brooklyn and Bronx which contributes to 70% of all shooting incidents.**

**3.  Demographics show that the perpetrators in the shooting incident are predomininantly male in the age of 18 to 44 years old** 

**4.  The heatmap suggests that shooting incidents occur frequently during the weekends beginning midnight until dawn.** 

**5.  Geographical profiling shows that majority of the shooting incidents occur on the northern and southern part of NYC.**   

##  Modeling

Predicting PERPETRATOR AGE GROUP by using CLASSIFICATION AND REGRESSION TREE 

``` {r eval = TRUE}

nypd_shooting_new = subset(nypd_shooting, PERP_AGE_GROUP != "UNKNOWN" )
set.seed(2888)

spl = sample.split(nypd_shooting_new$PERP_AGE_GROUP, SplitRatio = 0.70)
Train = subset(nypd_shooting_new, spl == TRUE)
Test = subset(nypd_shooting_new, spl == FALSE)

nypdTree = rpart(PERP_AGE_GROUP ~ WEEKDAY  + HOUR + BORO + PERP_SEX, data = Train, method = "class", minbucket = 25)
prp(nypdTree)
```



## Bias Identification and Conclusion


**1.  The data collected that were used in the analysis limits the study into finding out where the shooting incidents have occured and identified the perpetrator's relative demographics and sex.** 

**2.  The location description variable was not explored in detail due to lots of missing values.**  

**3.  The missing values were imputed using simulation which basically relies on the underlying distribution of complete cases.** 

**4.  Seasonality of the incidents is identified using heat map which obscures the fact that there may be some incidents that were under reported.**

**5.  The following study primarily dives on predictive policing where methods of predicting crimes involves approaches in forecasting places and times with an increased risk in shooting incidents.**

**6.  The study does not explore methods of predicting perpetrators which identifies individuals at risk of committing shooting crimes in the future.**

**7.  The study does not explore methods of predicting victims of crimes which identifies individuals who are probably going to become victims in the future.**
