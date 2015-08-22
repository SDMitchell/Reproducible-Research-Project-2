# NOAA Weather Data Analysis 1950-2011
SDMitchell  
August 15, 2015  

## Synopsis
This report will attempt to analyze event data in the NOAA Storm Data database. This database contains data for the United States from the years 1950 through 2011 inclusive; the analysis will consist of attempting to answer the following two questions:
  
1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
  
2. Across the United States, which types of events have the greatest economic consequences?
  
*** 
Points of note
  
* The code for fetching, extracting and caching relevant portions of the data is all included as part of the analysis;
* It is important to note that all dollar amounts are in US dollars and are *NOT* adjusted for inflation.
  
*** 
  
## Data Processing
The following libraries are needed for this analysis and it is assumed that they are already installed (via install.packages) if any reproduction is being attempted:

```r
library(ggplot2)
library(tools)
library(lubridate)
library(reshape2)
library(plyr)
library(maps)
library(mapproj)

options(digits=2, scipen=1)
```
  
These variables are for our target data on the internet and our starting filenames.
  

```r
cacheFilename <- "repdata_data_StormData.rds"
rawDataFilename <- "repdata_data_StormData.csv.bz2"
targetURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
```
  
We'll start the data cleaning process by abbreviating the data set to one that continas only the features of interest. Once we have this set we store it on disk as a serialized R object for future use (and much shorter startup times). We store this data set in the variable "originalData".
  

```r
acquireData <- function() {
	# Try to download the given file if our input file does not already exist
	if(!file.exists(cacheFilename))
	{
		# If the input data file doesn't exist yet, we need to download it
		if(!file.exists(rawDataFilename))
		{
			download.file(url=targetURL, destfile=rawDataFilename, method="auto", mode="wb")
		}
	
		if(file.exists(rawDataFilename))
		{
			# Record the downloaded file's MD5 sum; this file's creation date can serve as the downloaded date
			write(md5sum(rawDataFilename), paste(rawDataFilename, ".MD5", sep=""))
		}
		else
		{
			stop("There was a problem attempting to download the file from the given URL")
		}
		# We either have the raw data file or it was already on disk. So here we are going to create an abbreviate data set
		# with just the features we want. This allows us to get rid of the noisy (and memory-heavy) REMARKS column. Plus
		# the time columns are an error-filled mess that were obviously done by manual entry and are nigh unusable.
		dataRaw <- read.csv(rawDataFilename, header=TRUE, sep=",", stringsAsFactors=FALSE, na.strings="NA")
		dataSelection <- dataRaw[, c("BGN_DATE", "STATE", "EVTYPE", "F", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP",  "CROPDMG", "CROPDMGEXP", "WFO")]
		saveRDS(dataSelection, cacheFilename)
	}
	else
	{
		dataSelection <- readRDS(cacheFilename)
	}
	
	dataSelection
}
originalData <- acquireData()
```
  
In the interest of starting with a tidy data set, we can compact the property damage and crop damage columns by simply multiplying them by their proper exponent column. There are a lot of problems in the exponents column with many entries not matching the documented accepted values, so some assumptions must be made.
  
Invalid Exponent | Assumed Value
-------- | --------
- | 1000
+ | 1000000
? | 0
h/H | 100
Numeric | equivalent number
empty string | 0


```r
# Create a mapping of data set value to exponent that we are going to assume it represents
exponent <- as.data.frame(c(3, 6, 0, 9, 6, 6, 0, 5, 6, 0, 4, 2, 3, 2, 7, 2, 3, 1, 8, 3))
colnames(exponent) <- c("exponent")
rownames(exponent) <- c("K", "M", "", "B", "m", "+", "0", "5", "6", "?", "4", "2", "3", "h", "7", "H", "-", "1", "8", "k")
originalData[originalData$PROPDMGEXP=="", "PROPDMGEXP"] <- "0"
originalData[originalData$CROPDMGEXP=="", "CROPDMGEXP"] <- "0"

# Compute the actual damage values and bind the new columns to the data frame
originalData$realPropDamage <- originalData$PROPDMG * 10^exponent[originalData$PROPDMGEXP, ]
originalData$realCropDamage <- originalData$CROPDMG * 10^exponent[originalData$CROPDMGEXP, ]

# List of valid states, according to ANSI, here: https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations
validStates <- as.data.frame(c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI", "UM", "FM", "MH", "PW"))
colnames(validStates) <- c("abbrev")
validStates$fullNames <- tolower(c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming","American Samoa","Guam","Northern Mariana Islands","Puerto Rico","Virgin Islands","U.S. Minor Outlying Islands","Federated States of Micronesia","Marshall Islands","Palau"))

# We need to weed out any rows with an invalid STATE. There is likely a better way to do this...
totalCropDamageByState <- aggregate(realCropDamage~STATE, data=originalData, FUN=sum)
invalidRecordsBecauseOfState <- dim(originalData[(originalData$STATE %in% setdiff(totalCropDamageByState$STATE, validStates$abbrev)), ])[1]
percentageInvalidRecordsBecauseOfState <- invalidRecordsBecauseOfState/dim(originalData)[1] * 100

# Replace our current data set with one that has the bad records removed
originalData <- originalData[!(originalData$STATE %in% setdiff(totalCropDamageByState$STATE, validStates$abbrev)),]
```
  
Our data also has a problem with invalid states which we cannot easily work around; we computed the number of invalid rows (14757 rows) and decided that due to them being a small percentage of the overall data set (1.64%) that it was likely valid to remove them altogether.
  
The next issue has to do with the EVTYPE field. There are many unique fields here and most of them are redundant. The strategy will be to organize these into groupings with regular expression matches with an "Other" category to capture everything that doesn't fall within a chosen grouping. To make sure that we catch as much as possible, a new column called eventType will be added to the data frame where the EVTYPE field is upper-cased and trimmed of any leading/trailing spaces. Please note that due to limitations in R Markdown processing, the regular expressions had to be slightly modified in the table to be compliant. Simply replace the phrase " or " with the regex term "|" to get the valid expression (see the code for the real expressions).
  
Category | Expression
--------|--------------
Tornado | TORNADO or FUNNEL
Large summer storm | HURRICANE or TROPICAL STORM or TYPHOON
Tidal / Coastal | TSUNAMI or SURGE or TIDE or SURF or SEAS or MARINE or RIP CURRENT or WAVE or SWELL
Lightning | LIGHTNING or LIGHTING or THUNDER
Wind | WIND or MICROBURST
Flooding | FLOOD or WATER or RAIN or PRECIP
Winter storm / Cold | BLIZZARD or SNOW or COLD or FREEZE or CHILL or ICE or HYPOTHERMIA or SLEET or WINTER or LOW TEMP or RECORD LOW or FROST
Heat / Fire | HEAT or DROUGHT or FIRE or HIGH TEMP or RECORD HIGH or HYPERTHERMIA or HOT
Dust / Fog / Smoke | DUST or FOG or SMOKE
Hail | HAIL
Avalanche / Mudslide | AVALANCHE or (?:LAND or MUD)SLIDE
Other | Everything else


```r
# This takes a minute to run - it has a lot of work to do (regexes are not cheap and this is running over 800K of them).
originalData$eventType <- toupper(mapply(FUN=gsub, originalData$EVTYPE, MoreArgs=list(pattern="^\\s+(.*?)\\s*$", replacement="\\1", perl=TRUE), USE.NAMES=FALSE))

# This is also not going to be cheap, because it is going to run even more regexes. This is the classification function which is the heart of the analysis; there is an appendix at the end of the report if there is any curiousity as to what EVTYPEs fall into the "Other" category.
classifyEventType <- function(s) {
	result <- "Other"
	if(grepl("TORNADO|FUNNEL", s))
	{
		result <- "Tornado"
	}
	else if (grepl("HURRICANE|TROPICAL STORM|TYPHOON", s))
	{
		result <- "Hurricane"
	}
	else if (grepl("TSUNAMI|SURGE|TIDE|SURF|SEAS|MARINE|RIP CURRENT|WAVE|SWELL", s))
	{
		result <- "Tidal / Coastal"
	}
	else if (grepl("LIGHTNING|LIGHTING|THUNDER", s))
	{
		result <- "Lightning"
	}
	else if (grepl("WIND|MICROBURST", s))
	{
		result <- "Wind / Microburst"
	}
	else if (grepl("FLOOD|WATER|RAIN|PRECIP", s))
	{
		result <- "Flooding / Water"
	}
	else if (grepl("BLIZZARD|SNOW|COLD|FREEZE|CHILL|ICE|HYPOTHERMIA|SLEET|WINTER|LOW TEMP|RECORD LOW|FROST", s))
	{
		result <- "Winter Storm / Cold"
	}
	else if (grepl("HEAT|DROUGHT|FIRE|HIGH TEMP|RECORD HIGH|HYPERTHERMIA|HOT", s))
	{
		result <- "Heat / Fire"
	}
	else if (grepl("DUST|FOG|SMOKE", s))
	{
		result <- "Dust / Fog / Smoke"
	}
	else if (grepl("HAIL", s))
	{
		result <- "Hail"
	}
	else if (grepl("AVALANCHE|(?:LAND|MUD)SLIDE", s))
	{
		result <- "Avalanche / Mudslide"
	}
	result
}
originalData$eventClassification <- mapply(FUN=classifyEventType, originalData$eventType, USE.NAMES=FALSE)

# This is used for the population plot
fatalityCount <- aggregate(FATALITIES~eventClassification, data=originalData, FUN=sum)
fatalityCount <- fatalityCount[order(fatalityCount$FATALITIES, decreasing=T), ]
injuryCount <- aggregate(INJURIES~eventClassification, data=originalData, FUN=sum)
injuryCount <- injuryCount[order(injuryCount$INJURIES, decreasing=T), ]
victimCount <- merge(fatalityCount, injuryCount, by= "eventClassification")
populationHealthData <- melt(victimCount, id.vars=c("eventClassification"))
victimCount$total <- victimCount$FATALITIES + victimCount$INJURIES

# This is used for the economic impact plot
propertyDamageAmount <- aggregate(realPropDamage~eventClassification, data=originalData, FUN=sum)
propertyDamageAmount <- propertyDamageAmount[order(propertyDamageAmount$realPropDamage, decreasing=T), ]
cropDamageAmount <- aggregate(realCropDamage~eventClassification, data=originalData, FUN=sum)
cropDamageAmount <- cropDamageAmount[order(cropDamageAmount$realCropDamage, decreasing=T), ]
totalDamageAmount <- merge(propertyDamageAmount, cropDamageAmount, by= "eventClassification")
economicHealthData <-melt(totalDamageAmount, id.vars=c("eventClassification"))
totalDamageAmount$total <- totalDamageAmount$realCropDamage + totalDamageAmount$realPropDamage

# These are for our post-analysis choropleths

# Water damage
waterDamageByState <- by(originalData, originalData$STATE, function(x) sum(x[x$eventClassification=="Flooding / Water", "realCropDamage"] + x[x$eventClassification=="Flooding / Water", "realPropDamage"]))

# We need a data frame to really do anything of use with "by" results
dfWaterDamageByState <- as.data.frame(names(waterDamageByState))
colnames(dfWaterDamageByState) <- c("state")
dfWaterDamageByState$cost <- as.vector(waterDamageByState[names(waterDamageByState)])
dfWaterDamageByState <- merge(dfWaterDamageByState, validStates, by.x="state", by.y="abbrev")
colnames(dfWaterDamageByState) <- c("state", "cost", "stateName")

# Tornado victims
tornadoVictimsByState <- by(originalData, originalData$STATE, function(x) sum(x[x$eventClassification=="Tornado", "FATALITIES"] + x[x$eventClassification=="Tornado", "INJURIES"]))

# We need a data frame to really do anything of use with "by" results
dfTornadoVictimsByState <- as.data.frame(names(tornadoVictimsByState))
colnames(dfTornadoVictimsByState) <- c("state")
dfTornadoVictimsByState$victims <- as.vector(tornadoVictimsByState[names(tornadoVictimsByState)])
dfTornadoVictimsByState <- merge(dfTornadoVictimsByState, validStates, by.x="state", by.y="abbrev")
colnames(dfTornadoVictimsByState) <- c("state", "victims", "stateName")
```
  
*** 
  
## Results
To answer the questions posed in the Synopsis section, we have constructed three plots. 
  
**1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?**
  
After classifying the various types of events and performing some aggregations based thereon, it was very apparent that **tornado activity** was overwhelmingly responsible for causing the most harm to humans - to the point that a bar chart is uninteresting and not presented here (although the code to reproduce it is relevant):
  

```r
# Stacked bar plot fatality/injury
g <- ggplot(populationHealthData, aes(eventClassification, value/1000, order=desc(variable)))
g <- g + geom_bar(stat="identity", aes(fill=variable), colour="black") +
	labs(x = "Event Type", y = "Victims (Thousands)", fill="Incident Type", title="US Total Victims Per Weather Phenomenon 1950 - 2011") +
	scale_fill_discrete(labels=c("Fatalities", "Injuries")) +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), 
		  legend.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  legend.position = c(1,1), legend.justification = c( 1,1),
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.grid.major = element_line(colour="#FFDEAD"))
#g
```
  

```r
victimCount[order(victimCount$total, decreasing=T), ]
```

```
##     eventClassification FATALITIES INJURIES total
## 10              Tornado       5661    91410 97071
## 5           Heat / Fire       3048    10442 13490
## 3      Flooding / Water       1646     8963 10609
## 11    Wind / Microburst       1183     9036 10219
## 7             Lightning       1019     7685  8704
## 12  Winter Storm / Cold        862     6236  7098
## 9       Tidal / Coastal       1054     1400  2454
## 6             Hurricane        201     1716  1917
## 2    Dust / Fog / Smoke        104     1559  1663
## 4                  Hail         15     1371  1386
## 1  Avalanche / Mudslide        268      225   493
## 8                 Other         50      427   477
```
  
The per-state breakdown for tornado harm is interesting but unsurprising, as tornado activity in the United States happens in a rather well-defined area. A choropleth of the victim count shows Texas as the most victimized by injury/fatality:
  

```r
states_map <- map_data("state") 
pal <- colorRampPalette(c("#00FF00", "#FF0000"))(5)

g <- ggplot(dfTornadoVictimsByState, aes(map_id=stateName, fill=victims))
g <- g + geom_map(map=states_map, colour ="black") + 
	scale_fill_gradient2(low=pal[1], mid=pal[3], high=pal[5], midpoint=median(dfTornadoVictimsByState$victims)) +
	labs(x = "Longitude (deg)", y = "Latitude (Deg)", fill="Victims", title="US Per-State Victims Due To Tornado-Related Weather Affects 1950-2011") +
	expand_limits(x = states_map$long, y = states_map$lat) +
	coord_map("polyconic") +
	theme(legend.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.grid.major = element_line(colour="#FFDEAD"))
g
```

<img src="NOAA_files/figure-html/Tornado Victim Count-1.png" title="" alt="" style="display: block; margin: auto;" />
  

```r
head(dfTornadoVictimsByState[order(dfTornadoVictimsByState$victims, decreasing=T), c("stateName", "state", "victims")], 10)
```

```
##      stateName state victims
## 48       texas    TX    8745
## 2      alabama    AL    8546
## 29 mississippi    MS    6696
## 3     arkansas    AR    5495
## 40    oklahoma    OK    5125
## 47   tennessee    TN    5116
## 28    missouri    MO    4718
## 39        ohio    OH    4633
## 18     indiana    IN    4476
## 17    illinois    IL    4348
```
  
**2. Across the United States, which types of events have the greatest economic consequences?**
  
Economic consequences were a bit more evenly spread out than the population harm results, although **flooding and water-related damages** seemed to clearly be the worst for property damage and **heat-related weather conditions** were very bad in terms of crop damage:
  

```r
# Stacked bar plot prop/crop damage
g <- ggplot(economicHealthData, aes(eventClassification, value/1000000000))
g <- g + geom_bar(stat="identity", aes(fill=variable), colour="black") +
	guides(fill = guide_legend(reverse = TRUE)) +
	labs(x = "Event Type", y = "Cost (USD Billions, non-adjusted)", fill="Damage Type", title="US Total Cost Per Weather Phenomenon 1950 - 2011") +
	scale_fill_discrete(labels=c("Property", "Crop")) +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1), 
		  legend.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  legend.position = c(1,1), legend.justification = c( 1,1),
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.grid.major = element_line(colour="#FFDEAD"))
g
```

<img src="NOAA_files/figure-html/Economic Impact Graph-1.png" title="" alt="" style="display: block; margin: auto;" />
  

```r
totalDamageAmount[order(totalDamageAmount$total, decreasing=T), ]
```

```
##     eventClassification realPropDamage realCropDamage   total
## 3      Flooding / Water        1.7e+11        1.3e+10 1.8e+11
## 6             Hurricane        9.3e+10        6.2e+09 9.9e+10
## 10              Tornado        5.9e+10        4.2e+08 5.9e+10
## 9       Tidal / Coastal        4.8e+10        4.8e+07 4.8e+10
## 5           Heat / Fire        9.6e+09        1.5e+10 2.5e+10
## 12  Winter Storm / Cold        1.2e+10        8.7e+09 2.1e+10
## 4                  Hail        1.6e+10        3.0e+09 1.9e+10
## 11    Wind / Microburst        1.1e+10        1.5e+09 1.2e+10
## 7             Lightning        7.6e+09        6.7e+08 8.2e+09
## 1  Avalanche / Mudslide        3.3e+08        2.0e+07 3.5e+08
## 8                 Other        6.8e+07        1.6e+08 2.2e+08
## 2    Dust / Fog / Smoke        3.1e+07        3.1e+06 3.4e+07
```
  
A per-state breakdown of the economic impact of these weather events seems to place California overwhelmingly in the lead in terms of damage. This is likely due to being a coastal area with many expensive properties. California is also responsible for a large amount of food production for the entire United States, leading to an unbalanced amount of damage per square mile.


```r
states_map <- map_data("state") 
pal <- colorRampPalette(c("#00FF00", "#FF0000"))(5)

g <- ggplot(dfWaterDamageByState, aes(map_id=stateName, fill=log(cost)))
g <- g + geom_map(map=states_map, colour ="black") + 
	scale_fill_gradient2(low=pal[1], mid=pal[3], high=pal[5], midpoint=median(log(dfWaterDamageByState$cost))) +
	labs(x = "Longitude (deg)", y = "Latitude (Deg)", fill="Log-Cost", title="US Per-State Cost Due To Water-Related Weather Affects 1950-2011") +
	expand_limits(x = states_map$long, y = states_map$lat) +
	coord_map("polyconic") +
	theme(legend.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  plot.background = element_rect(fill = "#FFFAF0", colour = "#000000"), 
		  panel.grid.major = element_line(colour="#FFDEAD"))
g
```

<img src="NOAA_files/figure-html/Economic Impact choropleth-1.png" title="" alt="" style="display: block; margin: auto;" />
  

```r
head(dfWaterDamageByState[order(dfWaterDamageByState$cost, decreasing=T), c("stateName", "state", "cost")], 10)
```

```
##       stateName state    cost
## 6    california    CA 1.2e+11
## 17     illinois    IL 1.1e+10
## 47    tennessee    TN 4.8e+09
## 32 north dakota    ND 4.2e+09
## 15         iowa    IA 3.4e+09
## 21    louisiana    LA 3.3e+09
## 38     new york    NY 3.2e+09
## 35   new jersey    NJ 2.9e+09
## 42 pennsylvania    PA 2.5e+09
## 11      florida    FL 2.5e+09
```
  
*** 
  
## About the Analysis
  
### Metadata
File Characteristic | Value
----------------- | -------
File Name | repdata_data_StormData.csv.bz2
URL | [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
File Size | 49,177,144 bytes
Compression | bz2
MD5 | df4aa61fff89427db6b7f7b1113b5553

### Environment
**System Information**

```r
sysinf <- Sys.info()
```
Parameter | Value
-------- | --------
Operating System | Windows7 x64
version | build 7601, Service Pack 1
machine arch | x86-64

**Session Information**

```r
sessionInfo()
```

```
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_Canada.1252  LC_CTYPE=English_Canada.1252   
## [3] LC_MONETARY=English_Canada.1252 LC_NUMERIC=C                   
## [5] LC_TIME=English_Canada.1252    
## 
## attached base packages:
## [1] tools     stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] mapproj_1.2-4   maps_2.3-11     plyr_1.8.3      reshape2_1.4.1 
## [5] lubridate_1.3.3 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-6 digest_0.6.8     evaluate_0.7     formatR_1.2     
##  [5] grid_3.1.0       gtable_0.1.2     htmltools_0.2.6  knitr_1.10.5    
##  [9] labeling_0.3     magrittr_1.5     MASS_7.3-42      memoise_0.2.1   
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.6      rmarkdown_0.7   
## [17] scales_0.2.5     stringi_0.5-5    stringr_1.0.0    yaml_2.1.13
```
  
## References
* [Wiki for United States state abbreviations](https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations)
* [R Graphics Cookbook](http://shop.oreilly.com/product/0636920023135.do) - Winston Chang
  
*** 
  
### What is "Other"?
This is just a quickie function to list all of the classifications that fall into the "Other" category. There are many of them, but most are summary data and mis-spellings of terms that would have been caught by the classification algorithm if they were correct. Some are rare enough or non-specific enough to deserve the "Other" classification.
  

```r
whatIsOther <- function(eventTypeData) {
other <- as.data.frame(unique(eventTypeData), stringsAsFactors=FALSE)
colnames(other) <- c("type")
other$classification <- mapply(FUN=classifyEventType, unique(eventTypeData), USE.NAMES=FALSE)
other <- other[other$classification == "Other", "type"]
other
}
whatIsOther(originalData$eventType)
```

```
##   [1] "WALL CLOUD"                    "SEVERE TURBULENCE"            
##   [3] "RECORD WARMTH"                 "APACHE COUNTY"                
##   [5] "URBAN/SMALL"                   "HIGH"                         
##   [7] "URBAN AND SMALL"               "DOWNBURST"                    
##   [9] "GUSTNADO AND"                  "FREEZING DRIZZLE"             
##  [11] "GLAZE"                         "WINTRY MIX"                   
##  [13] "DRY"                           "WAYTERSPOUT"                  
##  [15] "URBAN AND SMALL STREAM"        "MUD SLIDE"                    
##  [17] "LIGNTNING"                     "COOL AND WET"                 
##  [19] "SMALL STREAM AND"              "MUD SLIDES"                   
##  [21] "EXCESSIVE WETNESS"             "ROTATING WALL CLOUD"          
##  [23] "LARGE WALL CLOUD"              "GUSTNADO"                     
##  [25] "WARM DRY CONDITIONS"           "URBAN/SMALL STREAM"           
##  [27] "TORNDAO"                       "AVALANCE"                     
##  [29] "RECORD TEMPERATURES"           "OTHER"                        
##  [31] "ICY ROADS"                     "HEAVY MIX"                    
##  [33] "DAM FAILURE"                   "SOUTHEAST"                    
##  [35] "FREEZING DRIZZLE AND FREEZING" "WET WEATHER"                  
##  [37] "BEACH EROSIN"                  "MUD/ROCK SLIDE"               
##  [39] "FLASH FLOOODING"               "TSTMW"                        
##  [41] "EXCESSIVE"                     "?"                            
##  [43] "DRY PATTERN"                   "MILD/DRY PATTERN"             
##  [45] "MILD PATTERN"                  "HEAVY SHOWERS"                
##  [47] "HEAVY SHOWER"                  "URBAN SMALL"                  
##  [49] "SMALL STREAM"                  "URBAN/SML STREAM FLD"         
##  [51] "RECORD DRY MONTH"              "TEMPERATURE RECORD"           
##  [53] "COASTAL STORM"                 "WET MONTH"                    
##  [55] "WET YEAR"                      "BEACH EROSION"                
##  [57] "LANDSLUMP"                     "RECORD WARM TEMPS."           
##  [59] "RECORD TEMPERATURE"            "FREEZING SPRAY"               
##  [61] "SUMMARY JAN 17"                "SUMMARY OF MARCH 14"          
##  [63] "SUMMARY OF MARCH 23"           "SUMMARY OF MARCH 24"          
##  [65] "SUMMARY OF APRIL 3RD"          "SUMMARY OF APRIL 12"          
##  [67] "SUMMARY OF APRIL 13"           "SUMMARY OF APRIL 21"          
##  [69] "SUMMARY AUGUST 11"             "SUMMARY OF APRIL 27"          
##  [71] "SUMMARY OF MAY 9-10"           "SUMMARY OF MAY 10"            
##  [73] "SUMMARY OF MAY 13"             "SUMMARY OF MAY 14"            
##  [75] "SUMMARY OF MAY 22 AM"          "SUMMARY OF MAY 22 PM"         
##  [77] "SUMMARY OF MAY 26 AM"          "SUMMARY OF MAY 26 PM"         
##  [79] "METRO STORM, MAY 26"           "SUMMARY OF MAY 31 AM"         
##  [81] "SUMMARY OF MAY 31 PM"          "SUMMARY OF JUNE 3"            
##  [83] "SUMMARY OF JUNE 4"             "SUMMARY JUNE 5-6"             
##  [85] "SUMMARY JUNE 6"                "SUMMARY OF JUNE 11"           
##  [87] "SUMMARY OF JUNE 12"            "SUMMARY OF JUNE 13"           
##  [89] "SUMMARY OF JUNE 15"            "SUMMARY OF JUNE 16"           
##  [91] "SUMMARY JUNE 18-19"            "SUMMARY OF JUNE 23"           
##  [93] "SUMMARY OF JUNE 24"            "SUMMARY OF JUNE 30"           
##  [95] "SUMMARY OF JULY 2"             "SUMMARY OF JULY 3"            
##  [97] "SUMMARY OF JULY 11"            "SUMMARY OF JULY 22"           
##  [99] "SUMMARY JULY 23-24"            "SUMMARY OF JULY 26"           
## [101] "SUMMARY OF JULY 29"            "SUMMARY OF AUGUST 1"          
## [103] "SUMMARY AUGUST 2-3"            "SUMMARY AUGUST 7"             
## [105] "SUMMARY AUGUST 9"              "SUMMARY AUGUST 10"            
## [107] "SUMMARY AUGUST 17"             "SUMMARY AUGUST 21"            
## [109] "SUMMARY AUGUST 28"             "SUMMARY SEPTEMBER 4"          
## [111] "SUMMARY SEPTEMBER 20"          "SUMMARY SEPTEMBER 23"         
## [113] "SUMMARY SEPT. 25-26"           "SUMMARY: OCT. 20-21"          
## [115] "SUMMARY: OCTOBER 31"           "SUMMARY: NOV. 6-7"            
## [117] "SUMMARY: NOV. 16"              "WET MICOBURST"                
## [119] "NO SEVERE WEATHER"             "SUMMARY OF MAY 22"            
## [121] "SUMMARY OF JUNE 6"             "SUMMARY AUGUST 4"             
## [123] "SUMMARY OF JUNE 10"            "SUMMARY OF JUNE 18"           
## [125] "SUMMARY SEPTEMBER 3"           "SUMMARY: SEPT. 18"            
## [127] "SML STREAM FLD"                "VOLCANIC ASH"                 
## [129] "VOLCANIC ASH PLUME"            "NONE"                         
## [131] "DAM BREAK"                     "TSTM WND"                     
## [133] "URBAN/SML STREAM FLDG"         "COASTALSTORM"                 
## [135] "SUMMARY OF MARCH 24-25"        "SUMMARY OF MARCH 27"          
## [137] "SUMMARY OF MARCH 29"           "URBAN/SMALL STRM FLDG"        
## [139] "MILD AND DRY PATTERN"          "DRY SPELL"                    
## [141] "DRY WEATHER"                   "ABNORMAL WARMTH"              
## [143] "UNUSUAL WARMTH"                "COASTAL EROSION"              
## [145] "UNUSUAL/RECORD WARMTH"         "SEICHE"                       
## [147] "TSTM"                          "ROCK SLIDE"                   
## [149] "RECORD COOL"                   "RECORD WARM"                  
## [151] "TROPICAL DEPRESSION"           "VOLCANIC ERUPTION"            
## [153] "COOL SPELL"                    "EXCESSIVELY DRY"              
## [155] "VOG"                           "MONTHLY TEMPERATURE"          
## [157] "RECORD DRYNESS"                "DRY CONDITIONS"               
## [159] "REMNANTS OF FLOYD"             "LANDSPOUT"                    
## [161] "DRIEST MONTH"                  "DRYNESS"                      
## [163] "UNUSUALLY WARM"                "WARM WEATHER"                 
## [165] "ABNORMALLY DRY"                "RED FLAG CRITERIA"            
## [167] "WND"                           "EXTREMELY WET"                
## [169] "VERY DRY"                      "PROLONG WARMTH"               
## [171] "NORTHERN LIGHTS"               "VERY WARM"                    
## [173] "ABNORMALLY WET"                "DROWNING"                     
## [175] "VOLCANIC ASHFALL"
```
  
  
