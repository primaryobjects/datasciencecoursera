library('ggplot2')

# Read data files.
NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

# Limit data to Baltimore City (fips == 24510).
NEICity <- NEI[NEI$fips == 24510, ]

# Perform a sum of the emissions over each year for each type.
NEICityByYear <- aggregate(Emissions ~ year + type, data = NEICity, sum)

# Set device resolution.
dev.new(width = 480, height = 480)

# Display line chart, grouped by type.
chart <- ggplot(NEICityByYear, aes(x=year, y=Emissions, color=type)) +
    ggtitle('Total PM2.5 Emissions in Baltimore City, MD') +
    xlab('Year') +
    ylab('PM2.5 Emissions (Tons)') +
    guides(color = guide_legend(title='Source Type')) +
    geom_line(lwd=1) +
    theme_bw()

print(chart)

# Copy to png file.
dev.copy(png, file = 'plot3.png')
dev.off()