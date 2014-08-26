library('ggplot2')
library('sqldf')

# Read data files.
NEI <- readRDS('summarySCC_PM25.rds')

# Filter SCC to just those relating to coal combustion.
SCC <- readRDS('Source_Classification_Code.rds')

# Rename columns to remove periods from column names (they interfere with sqldf).
names(SCC) <- gsub("\\.", "", names(SCC))

# Filter SCC to just those relating to coal combustion.
SCCCoalComb <- sqldf("select * from SCC where SCCLevelOne LIKE '%Combustion%' AND EISector LIKE '%Coal%'")

# Match source classification codes to NEI rows, dropping NEI rows that do not match (coal combustion).
NEISource <- merge(NEI, SCCCoalComb, by = 'SCC')

# Perform a sum of the emissions over each year for each type.
NEISourceByYear <- aggregate(Emissions ~ year, data = NEISource, sum)

# Resize Emissions to 10^5 for cleaner display.
NEISourceByYear$Emissions <- NEISourceByYear$Emissions / 10^5

# Set year to factor for proper display.
NEISourceByYear$year <- factor(NEISourceByYear$year)

# Set device resolution.
dev.new(width = 480, height = 480)

# Plot histogram.
chart <- ggplot(NEISourceByYear, aes(x=year, y=Emissions)) +
    ggtitle('Total PM2.5 Emissions for Coal Combustion in United States') +
    xlab('Year') +
    ylab('PM2.5 Emissions (10^5 Tons)') +
    geom_bar(stat = 'identity') +
    geom_smooth(method = "lm", se=FALSE, color="darkgray", aes(group=1), lty = 2) +
    theme_bw()

print(chart)

# Copy to png file.
dev.copy(png, file = 'plot4.png')
dev.off()