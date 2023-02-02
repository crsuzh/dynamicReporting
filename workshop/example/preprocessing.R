# Load libraries
library(sf)

# Write some helper functions
capitalize <- function(x) {
    vapply(
        x,
        function(y) {
            ints <- utf8ToInt(y)
            if (ints[1L] < 91L) {
                return(y)
            } else {
                ints[1L] <- ints[1L] - 32L
                return(intToUtf8(ints))
            }
        },
        character(1L),
        USE.NAMES = FALSE
    )
}

# Define where we want to download it from
source <- paste0(
    "https://data.stadt-zuerich.ch/dataset/",
    "sid_dav_strassenverkehrsunfallorte/",
    "download/RoadTrafficAccidentLocations.csv"
)

# Read the data file
data <- read.csv(source, header = TRUE, stringsAsFactors = FALSE)

# Preprocessing
## 1) Filter out translation columns in German, French, Italian
data <- data[!grepl("(_de|_fr|_it)$", names(data))]

## 2) Get rid of non-informative or duplicate columns
non_informative <- c(
    "AccidentUID",
    "MunicipalityCode",
    "CantonCode",
    "AccidentType",
    "AccidentSeverityCategory",
    "RoadType",
    "AccidentHour"
)

data <- data[!(names(data) %in% non_informative)]
names(data)[names(data) == "AccidentMonth_en"] <- "month_name"
data$AccidentWeekDay  <- as.integer(sub(".*(\\d)$", "\\1", data$AccidentWeekDay))
names(data)[names(data) == "AccidentWeekDay"]  <- "weekday"
names(data)[names(data) == "AccidentWeekDay_en"]  <- "weekday_name"


## 4) Simplify column names
names(data) <- gsub("(Accident|Category|Involving|_en|_text)", "", names(data))
names(data) <- sub("^Location_CHLV95", "coord", names(data))
names(data) <- tolower(names(data))

## 5) Convert pedestrian, bicycle and motorcycle variables to proper
## booleans
bool <- c("pedestrian", "bicycle", "motorcycle")
data[bool] <- lapply(data[bool], \(x) ifelse(x == "false", FALSE, TRUE))

## 6) Construct a column with date objects (note that the day is always "01"
## since the day of accidents is not in the data set) and get rid of the
## month and year columns
# data$date <- with(data, as.Date(paste0(year, month, "-01"), format = "%Y%B-%d"))
# data <- data[!(names(data) %in% c("month", "year"))]

## 8)Convert clean data frame to sf object
# data <- sf::st_as_sf(data, coords = c("coord_e", "coord_n"), crs = 2056)

## 9) Convert "type", "severity", "roadtype" to factors and abbreviate
## the factor levels
data$type <- gsub("Accident (with|when) ", "", data$type)
data$type <- capitalize(data$type)
data$type <- factor(
    data$type,
    levels = c(
        "Skidding or self-accident",
        "Overtaking or changing lanes",
        "Rear-end collision",
        "Turning left or right",
        "Turning-into main road",
        "Crossing the lane(s)",
        "Head-on collision",
        "Parking",
        "Accident involving pedestrian(s)",
        "Accident involving animal(s)",
        "Other"
    )
)
data$severity <- gsub("Accident (with|when) ", "", data$severity)
data$severity <- capitalize(data$severity)
data$severity <- factor(
    data$severity,
    levels = c(
        "Fatalities",
        "Severe injuries",
        "Light injuries",
        "Property damage"
    )
)
data$roadtype <- gsub("Accident (with|when) ", "", data$roadtype)
data$roadtype <- factor(
    data$roadtype,
    levels = c(
        "Motorway",
        "Expressway",
        "Principal road",
        "Minor road",
        "Motorway side installation",
        "Other"
    )
)

# Split the pre-processed data and save it into two files
year <- as.integer(format(data$date, "%Y"))
month <- as.integer(format(data$date, "%m"))
idx_first <- year == 2020
idx_second <- year == 2021

write.csv(data[idx_first, ], "accidents_2020-01.csv", row.names = FALSE))
write.csv(data[idx_second, ], "accidents_2020-01_02.csv", row.names = FALSE)
