# Data Cleaning - BCEENET Workshop
## Basics for cleaning specimen data.
## 2020-06-26
## ML Gaynor

# Setup
install.packages(c("dplyr", "lubridate", "CoordinateCleaner"))

# Load Packages
library(dplyr)
library(lubridate)
library(CoordinateCleaner)

# Load raw data
rawdf <- read.csv("data/raw/Shortia_galacifolia_062620.csv")

## Inspect dataframe  
### What columns are included?  
names(rawdf)

### How many observations do we start with?
nrow(rawdf)

# 1. Resolve taxon names  
## Inspect scientific names included in the raw df.  
unique(rawdf$dwc.scientificName)

## Create a list of accepted names based on the dwc.scientificName in your dataframe
acceptednames <-c("Shortia galacifolia Torr. & Gray",
                  "Shortia galacifolia",
                  "Shortia galacifolia Torr. & A.Gray",
                  "Shortia galacifolia var. brevistyla",
                  "Shortia galacifolia Torrey & A. Gray",
                  "Shortia galacifolia var. galacifolia",
                  "Sherwoodia galacifolia",
                  "Shortia galacifolia var. brevistyla P. A. Davies",
                  "Shortia galacifolia Torr. & A. Gray")



## Filter to only include accepted namesUsing the R package dplyr, we:
### 1. filter the dataframe to only include rows with the accepted names
### 2. filter out any rows with NA for dwc.scientificName.
### 3. create a column called name and set it equal to“Shortia galacifolia”
df <- rawdf %>% 
      filter(dwc.scientificName %in% acceptednames) %>% 
      filter(!is.na(dwc.scientificName)) %>%
      mutate(name = "Shortia galacifolia")

## How many observations do we have now?
nrow(df)

# 2. Remove Duplicates
## Subset columns
### Using the R package dplyr, we:
### 1. select and rename columns.

df <- df %>% 
      dplyr::select(ID = coreid,
                    name = name,
                    basis = dwc.basisOfRecord,
                    catalogNumber = dwc.catalogNumber,
                    collectionCode = dwc.collectionCode,
                    collectionID = dwc.collectionID,
                    coordinateUncertaintyInMeters = dwc.coordinateUncertaintyInMeters,
                    lat = dwc.decimalLatitude,
                    long = dwc.decimalLongitude,
                    date = dwc.eventDate)
## Fix dates
### Using the R package lubridate, we first parse the date into the same format.
df$date <- lubridate::ymd(df$date)

### Next you are going to seperate date into year, month, and day - 
#### where every column only contains one set ofinformation.
df <- df %>% 
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    day = lubridate::day(date))

## Remove rows with identical lat, long, year, month, and day
### If a specimen shares lat, long, and event date
### we are assuming that it is identical. Many specimen lack date 
## and lat/long, so this may be getting rid of information you would want to keep.
df <- distinct(df, lat, long, year, month, day, .keep_all = TRUE)

## How many observations do we have now?
nrow(df)

# 3. Location cleaning
## Filter missing lat and long
### Using the R package dplyr, we:
#### 1. filter out(!) any rows where long ‘is.na’
#### 2. filter out(!) any rows where lat ‘is.na’
df <- df %>% 
      filter(!is.na(long)) %>% 
      filter(!is.na(lat))

## How many observations do we have now?
nrow(df)

## Precision
### Using the R base function ‘round’, we round lat and long to two decimal places
df$lat <- round(df$lat, digits = 2)
df$long <- round(df$long, digits = 2)

## Remove unlikely points
### Remove points at 0.00, 0.00
#### Using the R package dplyr, we:
##### 1. filter to retain rows where long is NOT(!) equal to 0.00 
##### 2. filter to retain rows where long is NOT(!) equal to 0.00
df <- df %>% 
      filter(long != 0.00) %>% 
      filter(lat != 0.00)


### Remove coordinates in cultivated zones, botanical gardens, and outside our desired range
#### Using the R package CoordinateCleaner, we first if points are 
#### at biodiversity institutions and remove anypoints that are.
df <- cc_inst(df, 
              lon = "long",
              lat = "lat",
              species = "name")

#### Next, we look for geographic outliers and remove outliers.
df <- cc_outl(df,
              lon = "long",
              lat = "lat",
              species = "name")

## How many observations do we have now?
nrow(df)

# 4. Save Cleaned .csv
write.csv(df, 
          "data/cleaned/Shortia_galacifolia_062620-cleaned.csv",
          row.names = FALSE)

