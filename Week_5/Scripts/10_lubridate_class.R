# 10_lubridate_Class --------------------------------------------------
# Created by: Alyssa Cohen 
# Created on: 2021-02-24 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Week 5b
##  Data Wrangling: lubridate dates and times

###   Outline of Class:
###     1.  Converting and manipulating dates and times using {lubridate}

###   Lab:
###     1.  Practice with dates and times

### [cheat sheet](https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries ------------------------------------------
install.packages("lubridate")
library(tidyverse)
library(here)
library(lubridate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# What time is it now? --------------------------------
now()

# * time in other time zones ----
##    east standard time
now(tzone = "EST")
##    greater mountain time
now(tzone = "GMT")

# * date and not time ----
today()

today(tzone = "GMT")

# * ask if it is am or pm ----
am(now())

pm(now())

# * is it a leap year? ----
leap_year(now())
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Date specifications for lubridate -------------------
##  dates must be a character

# * syntax ----
##    Date            |  function
##  _______________________________
##  2021-02-24        | ymd()
##  02/24/2021        | mdy()
##  February 24 2021  | mdy()
##  24/02/2021        | dmy()

##  y = year
##  m = month
##  d = day

# * produce ISO dates ----
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Date and Time specifications with lubridate ---------
##          Time	              |  function
##  _________________________________________
##  2021-02-24 10:22:20 PM	    | ymd_hms()
##  02/24/2021 22:22:20	        | mdy_hms()
##  February 24 2021 10:22 PM   | mdy_hm()

##  y = hour
##  m = minute (1st m)
##  d = second
##  h = hour
##  m = minute (2nd m)
##  s = second

ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extracting specific date or time elements from datetimes ----
# make a character string
datetimes<-c("02/24/2021 22:22:20",
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")

# convert to datetimes
datetimes <- mdy_hms(datetimes)

# * extract months from character string ----
month(datetimes)

#     can also save it as month name
##        abbreviations
month(datetimes, label = TRUE)
##        spelled out
month(datetimes, label = TRUE, abbr = FALSE)

# * extract the days ---
##    save as number of the day
day(datetimes) 

##    day of the week
wday(datetimes, label = TRUE)

# * extract the hour, minute, second ---
hour(datetimes)
minute(datetimes)
second(datetimes)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Adding dates and times -------------------------------
# * Let's add 4 hours to all the datetimes ----
datetimes + hours(4)
# * add 2 days ----
datetimes + days(2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rounding dates --------------------------------------
# * round to nearest minute ----
round_date(datetimes, "minute")

# * round to nearest 5 minute ----
round_date(datetimes, "5 mins")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# T,P,S -----------------------------------------------
##  Read in the conductivity data (CondData.csv) and 
##    convert the date column to a datetime. 
##    Use the %>% to keep everything clean.

##  Data info:
##    This is temperature and salinity data taken at a site with groundwater 
##      while being dragged behind a float. Data were collected every 10 seconds. 
##    You will also notice depth data. That dataset will be used in late during lab. 
##      Those data are taken from a pressure sensor, also collected data every 10 seconds

# * load data ----
CondData <- read_csv(here("Week_5","Data","CondData.csv"))

# * convert date to datetime ----
CondData$date <- ymd_hms("2021-01-15 08:24:42")


CondData <- CondData %>% mutate(date = ymd_hms(CondData$date))

# did this to check to see if it worked
round_date(CondData$date, "minute")



