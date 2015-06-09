## Including the required R packages.
packages <- c("ggplot2", "ggthemes", "reshape2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(reshape2)
library(ggthemes)

multiplyDamage <- function(value, multiplier) {
  # Function for calculating true property/crop damage by factoring in multiplier. Example: 1.5 M => 1500000.
  result <- NULL
  
  multiplier <- toupper(multiplier)
  
  if (multiplier == 'K') {
    result <- value * 1000
  }
  else if (multiplier == 'M') {
    result <- value * 1000000
  }
  else if (multiplier == 'B') {
    result <- value * 1000000000
  }
  else {
    result <- value
  }
  
  result
}

orderGroupsByEvent <- function(data, groupColumnName1, groupColumnName2) {
  # Transforms a data.frame into a sorted data.frame, grouped by Event Type (where event types will appear in order). Useful for plotting charts of top 10 event types.

  # Transform data into long-style, for usage with stacked bar chart (injuries + fatalities).
  p <- melt(data, id.vars = 'Event')
  p <- p[p$variable != 'Total',]
  p <- p[order(p$value, decreasing = TRUE), ]
  
  # Split into groups by event name, to allow sorting by event type in order by total count.
  p1 <- split(p, p$Event)
  
  # Reshape the groups to include Index, Event, Total.
  p2 <- sapply(seq_along(p1), function(t) {
    c(Index = t, Event = names(p1[t]), Total = sum(p1[[t]]$value))
  })
  
  # Sort the groups by total in descending order.
  p3 <- p2[, order(as.numeric(p2['Total',]), decreasing=TRUE)]
  
  # Create a new data.frame with the groups as rows (grouped together by event name) in descending order by total count.
  p4 <- data.frame(Event = character(), variable = character(), Total = numeric())
  
  # Loop through set and append rows of groups. This allows us to plot a bar chart in histogram form.
  for (i in 1:ncol(p3)) {
    row <- p3[, i]
    
    originalRow <- p[p$Event == p3[, i]['Event'],]
    
    newRow1 <- data.frame(Event = row[['Event']], variable = groupColumnName1, Total = originalRow[originalRow$variable == groupColumnName1,]['value'])
    names(newRow1) <- c('Event', 'variable', 'Total')
    
    newRow2 <- data.frame(Event = row[['Event']], variable = groupColumnName2, Total = originalRow[originalRow$variable == groupColumnName2,]['value'])
    names(newRow2) <- c('Event', 'variable', 'Total')
    
    p4 <- rbind(p4, newRow2)
    p4 <- rbind(p4, newRow1)
  }
  
  p4
}

# Download dataset, if it does not exist.
fileName <- 'repdata-data-StormData.csv.bz2';
if (!file.exists(fileName)) {
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', fileName, method="auto")
}

# Read csv file.
data <- read.csv(fileName)

# EVTYPE contains many variations of the same event name.
# Convert to all upper-case.
data$EVTYPE <- toupper(data$EVTYPE)

# Add a date column by parsing the BGN_DATE field and converting to a date type.
dateFormat <- "%m/%d/%Y %H:%M:%S"
data$date <- as.Date(data$BGN_DATE, dateFormat)

# Add a year column
data$year <- format(data$date, '%Y')

# Add a decade column by rounding the year down to the nearest 10.
data$decade <- floor(as.numeric(format(data$date, "%Y")) / 10) * 10

# Remove event types containing: SUMMARY, TEMPERATURE RECORD.
data <- data[!grepl("SUMMARY", data$EVTYPE), ]
data <- data[!grepl("TEMPERATURE RECORD", data$EVTYPE), ]

# Replace anything other than [A-Z] with space.
data$EVTYPE <- gsub('[^a-zA-Z]', ' ', data$EVTYPE)

# Replace AND with space.
data$EVTYPE <- gsub('AND', ' ', data$EVTYPE)
data$EVTYPE <- gsub('UNSEASONABLE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('UNSEASONABLY', ' ', data$EVTYPE)
data$EVTYPE <- gsub('SEVERE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('PROLONG', ' ', data$EVTYPE)
data$EVTYPE <- gsub('PROLONGED', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EXPOSURE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EXCESSIVE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EXTREME', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EXTREMELY', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EARLY', ' ', data$EVTYPE)
data$EVTYPE <- gsub('DEEP', ' ', data$EVTYPE)
data$EVTYPE <- gsub('ABNORMALLY', ' ', data$EVTYPE)
data$EVTYPE <- gsub('ABNORMAL', ' ', data$EVTYPE)
data$EVTYPE <- gsub('STRONG', ' ', data$EVTYPE)
data$EVTYPE <- gsub('STORM FORCE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('RECORD', ' ', data$EVTYPE)
data$EVTYPE <- gsub('FIRST', ' ', data$EVTYPE)
data$EVTYPE <- gsub('DENSE', ' ', data$EVTYPE)
data$EVTYPE <- gsub('EROSIN', 'EROSION', data$EVTYPE)
data$EVTYPE <- gsub('AVALANCE', 'AVALANCHE', data$EVTYPE)
data$EVTYPE <- gsub('COASTALSTORM', 'COASTAL STORM', data$EVTYPE)

# Remove double-spaces.
data$EVTYPE <- gsub('[ ]{2,}', ' ', data$EVTYPE)

# Rename abbreviations: SML=>SMALL, FLDG=>FLOOD, FLOODING=>FLOOD, FLOODS => FLOOD, FLOODIN => FLOOD, FLD=>FLOOD, WARMTH=>WARM, COOL=>COLD, WND=>WIND, FIRES=>FIRE, WILD FIRES=>WILDFIRE, WILDFIRES=>WILDFIRE, VOG=>FOG
data$EVTYPE <- gsub('SML', 'SMALL', data$EVTYPE)
data$EVTYPE <- gsub('HURRICANE.*', 'HURRICANE', data$EVTYPE)
data$EVTYPE <- gsub('FLDG|FLOODING|FLOODS|FLOODIN|FLD', 'FLOOD', data$EVTYPE)
data$EVTYPE <- gsub('.+FLOOD.*', 'FLOOD', data$EVTYPE)
data$EVTYPE <- gsub('WARMTH', 'WARM', data$EVTYPE)
data$EVTYPE <- gsub('COOL|LOW TEMPERATURE', 'COLD', data$EVTYPE)
data$EVTYPE <- gsub('WND', 'WIND', data$EVTYPE)
data$EVTYPE <- gsub('FIRES|.+FIRE|FIRE WX', 'FIRE', data$EVTYPE)
data$EVTYPE <- gsub('WILD FIRE|WILD FIRES|WILD FIRE|WILDFIRES|WILD FOREST FIRE', 'WILDFIRE', data$EVTYPE)
data$EVTYPE <- gsub('VOG', 'FOG', data$EVTYPE)
data$EVTYPE <- gsub('WATERSPOUT .+|WATERSPOUTS|WAYTERSPOUT|WATER SPOUT', 'WATERSPOUT', data$EVTYPE)
data$EVTYPE <- gsub('WET MONTH|WET YEAR|WET MICROBURST|WET MICOBURST|WET SNOW|WET WEATHER|WETNESS', 'WET', data$EVTYPE)
data$EVTYPE <- gsub('WIND .+|WINDS|HIGH WIND.*', 'WIND', data$EVTYPE)
data$EVTYPE <- gsub('VOLCANIC ASH.+', 'VOLCANIC ASH', data$EVTYPE)
data$EVTYPE <- gsub('THUNDER.+|THUNDEER.+|TUNDER.*|TSTM.*|THUNERSTORM.*|THUNDESTORM.*|THUDERSTORM.*', 'THUNDERSTORM', data$EVTYPE)
data$EVTYPE <- gsub('STORM SURGE.+', 'STORM SURGE', data$EVTYPE)
data$EVTYPE <- gsub('SNOW.+|.+SNOW.*|WINTER.*|WINTRY.*', 'SNOW', data$EVTYPE)
data$EVTYPE <- gsub('SLEET.+', 'SLEET', data$EVTYPE)
data$EVTYPE <- gsub('RIP CURRENT.+', 'RIP CURRENT', data$EVTYPE)
data$EVTYPE <- gsub('RAIN.+', 'RAIN', data$EVTYPE)
data$EVTYPE <- gsub('MUDSLIDE.*|MUD SLIDE.*|MUD ROCK.*', 'MUDSLIDE', data$EVTYPE)
data$EVTYPE <- gsub('MARINE.+', 'MARINE', data$EVTYPE)
data$EVTYPE <- gsub('LIGHTNING.+', 'LIGHTNING', data$EVTYPE)
data$EVTYPE <- gsub('ICE.+|ICY.+|GLAZE.*|BLACK ICE', 'ICE', data$EVTYPE)
data$EVTYPE <- gsub('HOT.*|HEAT.*|WARM.*', 'HEAT', data$EVTYPE)
data$EVTYPE <- gsub('TORNADO.*', 'TORNADO', data$EVTYPE)
data$EVTYPE <- gsub('DRY.*', 'DRY', data$EVTYPE)
data$EVTYPE <- gsub('COLD.*', 'COLD', data$EVTYPE)
data$EVTYPE <- gsub('BLIZZARD.*', 'BLIZZARD', data$EVTYPE)
data$EVTYPE <- gsub('TROPICAL STORM.*', 'TROPICAL STORM', data$EVTYPE)
data$EVTYPE <- gsub('HAIL.*', 'HAIL', data$EVTYPE)

# Trim the event names of whitespace.
data$EVTYPE <- gsub('^\\s+|\\s+$', '', data$EVTYPE)

# Set the EVTYPE to a factor.
data$EVTYPE <- factor(data$EVTYPE)

# Display the unique event types.
levels(data$EVTYPE)

# Filter the dataset to records starting at 1993 and later (due to missing data in prior years, which would skew results towards Tornado and Thunderstorm being over-represented while calculating total numbers across years).
data1993 <- data[data$year >= 1993,]

# Calculate the total fatalities per event type.
fatalities <- aggregate(FATALITIES ~ EVTYPE, data1993, FUN=sum)

# Calculate the total injuries per event type.
injuries <- aggregate(INJURIES ~ EVTYPE, data1993, FUN=sum)

# Create a tidy dataset of just the Event Type, Number of Fatalaties, Number of Injuries, and total number of fatalities + injuries.
personalHarm <- data.frame(Event = fatalities$EVTYPE, Fatalities = fatalities$FATALITIES, Injuries = injuries$INJURIES, Total = fatalities$FATALITIES + injuries$INJURIES)

# Sort dataset by Total, grouped by event type.
personalHarmSorted <- orderGroupsByEvent(personalHarm, 'Fatalities', 'Injuries')

# Draw bar chart.
g <- ggplot(personalHarmSorted[1:20,], aes(x = Event, y = Total, fill = variable))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle('Personal Harm by Event from 1993 to 2011')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('Event')
g <- g + ylab('Total Fatalities and Injuries')
g <- g + scale_fill_manual(values=c('#303030', 'red'))

print(g)

# Calculate the total fatalities per event type and per decade.
fatalitiesByDecade <- aggregate(FATALITIES ~ EVTYPE + decade, data, FUN=sum)

# Calculate the total injuries per event type.
injuriesByDecade <- aggregate(INJURIES ~ EVTYPE + decade, data, FUN=sum)

# Create a tidy dataset of fatalities, injuries, and total by decade.
personalHarmByDecade <- data.frame(Event = fatalitiesByDecade$EVTYPE, Fatalities = fatalitiesByDecade$FATALITIES, Injuries = injuriesByDecade$INJURIES, Decade = fatalitiesByDecade$decade, Total = fatalitiesByDecade$FATALITIES + injuriesByDecade$INJURIES)

# Sort the tidy dataset by total count.
personalHarmByDecadeSorted <- personalHarmByDecade[order(personalHarmByDecade$Total, decreasing = TRUE),]

# Filter list to only those with top 10 highest total count, from previous chart.
personalHarmByDecadeTop <- personalHarmByDecade[personalHarmByDecade$Event %in% personalHarmByDecadeSorted[1:20, 'Event'],]

# Draw time-series chart of fatalities + injuries by decade.
g <- ggplot(personalHarmByDecadeTop, aes(x = Decade, y = Total))
g <- g + geom_line(aes(color = personalHarmByDecadeTop$Event), group = personalHarmByDecadeTop$Event, size=2)
g <- g + ggtitle('Personal Harm by Event Per Decade')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + xlab('Decade')
g <- g + ylab('Total Fatalities and Injuries')
g <- g + scale_x_continuous(breaks=seq(from = 1950, to = 2010, by = 10))
g <- g + guides(colour = guide_legend(override.aes = list(size=4)))
g <- g + scale_colour_colorblind()

print(g)

# Calculate the property damage by multiplying PROPDMG by PROPDMGEXP.
propertyDamage <- aggregate(multiplyDamage(PROPDMG, PROPDMGEXP) ~ EVTYPE, data1993, FUN=sum)
names(propertyDamage)[2] <- 'Cost'

# Calculate the crop damage by multiplying CROPDMG by CROPDMGEXP.
cropDamage <- aggregate(multiplyDamage(CROPDMG, CROPDMGEXP) ~ EVTYPE, data1993, FUN=sum)
names(cropDamage)[2] <- 'Cost'

# Create a tidy dataset of just the Event Type, Property Damage, Crop Damage, and total property + crop damage.
damages <- data.frame(Event = propertyDamage$EVTYPE, Property = propertyDamage$Cost, Crop = cropDamage$Cost, Total = propertyDamage$Cost + cropDamage$Cost)

# Sort by total count in decreasing order.
damages <- damages[order(damages$Total, decreasing = TRUE),]

# Sort dataset by Total, grouped by event type.
damagesSorted <- orderGroupsByEvent(damages, 'Property', 'Crop')

# Draw bar chart.
g <- ggplot(damagesSorted[1:20,], aes(x = Event, y = Total)) #, fill = variable (omit since crop damage is too small compared to property)
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle('Damages by Event from 1993 to 2011')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab('Event')
g <- g + ylab('Total Property and Crop Damage')

print(g)
