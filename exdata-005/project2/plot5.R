library('ggplot2')
library('sqldf')

# Read data files.
NEI <- readRDS('summarySCC_PM25.rds')

# Filter SCC to just those relating to coal combustion.
SCC <- readRDS('Source_Classification_Code.rds')

# Rename columns to remove periods from column names (they interfere with sqldf).
names(SCC) <- gsub("\\.", "", names(SCC))

# Limit data to Baltimore City (fips == 24510).
NEICity <- NEI[NEI$fips == 24510, ]

# Filter SCC to just those relating to coal combustion.
SCCMotor <- sqldf("select * from SCC where SCCLevelTwo LIKE '%Vehicle%'")

# Match source classification codes to NEI rows, dropping NEI rows that do not match (motor vehicle).
NEISource <- merge(NEICity, SCCMotor, by = 'SCC')

# Perform a sum of the emissions over each year for each type.
NEISourceByYear <- aggregate(Emissions ~ year, data = NEISource, sum)

# Resize Emissions to 10^5 for cleaner display.
NEISourceByYear$Emissions <- NEISourceByYear$Emissions

# Set year to factor for proper display.
NEISourceByYear$year <- factor(NEISourceByYear$year)

# Set device resolution.
dev.new(width = 480, height = 480)

# Plot histogram.
chart <- ggplot(NEISourceByYear, aes(x=year, y=Emissions)) +
    ggtitle('Total PM2.5 Emissions from Motor Vehicles in Baltimore City, MD') +
    xlab('Year') +
    ylab('PM2.5 Emissions (Tons)') +
    geom_bar(stat = 'identity') +
    geom_smooth(method = "lm", se=FALSE, color="darkgray", aes(group=1), lty = 2) +
    theme_bw()

print(chart)

# Copy to png file.
dev.copy(png, file = 'plot5.png')
dev.off()