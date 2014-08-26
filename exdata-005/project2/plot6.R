library('ggplot2')
library('sqldf')

# Read data files.
NEI <- readRDS('summarySCC_PM25.rds')

# Filter SCC to just those relating to coal combustion.
SCC <- readRDS('Source_Classification_Code.rds')

# Rename columns to remove periods from column names (they interfere with sqldf).
names(SCC) <- gsub("\\.", "", names(SCC))

# Limit data to Baltimore City (fips == 24510).
NEIBaltimoreCity <- NEI[NEI$fips == '24510', ]

# Limit data to Los Angeles County (fips == 06037).
NEILosAngeles <- NEI[NEI$fips == '06037', ]

# Filter SCC to just those relating to coal combustion.
SCCMotor <- sqldf("select * from SCC where SCCLevelTwo LIKE '%Vehicle%'")

# Match source classification codes to NEI rows, dropping NEI rows that do not match (motor vehicle).
NEIBaltimoreCitySource <- merge(NEIBaltimoreCity, SCCMotor, by = 'SCC')
NEILosAngelesSource <- merge(NEILosAngeles, SCCMotor, by = 'SCC')

# Perform a sum of the emissions over each year for each type.
NEIBaltimoreCitySourceByYear <- aggregate(Emissions ~ year, data = NEIBaltimoreCitySource, sum)
NEILosAngelesSourceByYear <- aggregate(Emissions ~ year, data = NEILosAngelesSource, sum)

# Resize Emissions to 10^5 for cleaner display.
#NEIBaltimoreCitySourceByYear$Emissions <- NEIBaltimoreCitySourceByYear$Emissions
#NEILosAngelesSourceByYear$Emissions <- NEILosAngelesSourceByYear$Emissions

# Set year to factor for proper display.
NEIBaltimoreCitySourceByYear$year <- factor(NEIBaltimoreCitySourceByYear$year)
NEILosAngelesSourceByYear$year <- factor(NEILosAngelesSourceByYear$year)

# Add city column.
NEIBaltimoreCitySourceByYear$City <- 'Baltimore City'
NEILosAngelesSourceByYear$City <- 'Los Angeles County'

total <- sum(NEIBaltimoreCitySourceByYear$Emissions)
NEIBaltimoreCitySourceByYear$PercentOfTotal <- (NEIBaltimoreCitySourceByYear$Emissions / total) * 100

total <- sum(NEILosAngelesSourceByYear$Emissions)
NEILosAngelesSourceByYear$PercentOfTotal <- (NEILosAngelesSourceByYear$Emissions / total) * 100

# Combine data sources into one data.frame.
NEISource <- rbind(NEIBaltimoreCitySourceByYear, NEILosAngelesSourceByYear)

# Set device resolution.
dev.new(width = 480, height = 480)

#NEIBaltimoreCitySourceByYear$Emissions <- NEIBaltimoreCitySourceByYear$Emissions

#plot1 <- ggplot(NEIBaltimoreCitySourceByYear, aes(x = year, y = Emissions)) +
#    geom_line(aes(x = year, y = Emissions, group = City))
    
#plot2 <- ggplot(NEILosAngelesSourceByYear, aes(x = year, y = Emissions)) +
#    geom_line(aes(x = year, y = Emissions, group = City)) +
#    scale_y_log10()

#grid.arrange(plot1, plot2, ncol = 2)

# Plot histogram.
#chart <- ggplot(NEISource, aes(x=year, y=Emissions)) +
#    ggtitle('Total PM2.5 Emissions from Motor Vehicles in United States') +
#    xlab('Year') +
#    ylab('PM2.5 Emissions (Tons)') +
#    geom_point(aes(colour = City)) +
#    geom_smooth(method = "lm", se=FALSE, color="darkgray", aes(group=City), lty = 2) +
#    theme_bw()

#chart <- ggplot(NEISource, aes(x = year, y = Emissions, colour = factor(City))) +
#    geom_bar() +
#    geom_smooth(method = "lm", color="darkgray", aes(group = City), se = FALSE, lty = 2) +
#    scale_y_log10()

#chart <- ggplot(NEISource, aes(x = year, y = Emissions, colour = factor(City))) +
#    geom_line(aes(group = City)) +
#    geom_smooth(method = "lm", color="darkgray", aes(group = City), se = FALSE, lty = 2) +
#    scale_y_log10()

chart <- ggplot(NEISource, aes(x = year, y = PercentOfTotal, colour = factor(City))) +
    ggtitle('Change in PM2.5 Emissions from Motor Vehicles') +
    xlab('Year') +
    ylab('Percent of PM2.5 Emissions from 1999 to 2008') +
    guides(color = guide_legend(title='City')) +
    geom_line(aes(group = City), lwd = 1) +
    theme_bw()

print(chart)

# Copy to png file.
dev.copy(png, file = 'plot6.png')
dev.off()