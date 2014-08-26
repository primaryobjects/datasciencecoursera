# Read data files.
NEI <- readRDS('summarySCC_PM25.rds')
SCC <- readRDS('Source_Classification_Code.rds')

# Split by year.
NEIByYear <- split(NEI, NEI$year)

# Calculate sum of all emissions by year.
totals <- lapply(seq_along(NEIByYear), function(index) {
    sum(NEIByYear[[index]]$Emissions) / 10 ^ 6
})

# Set column names to years.
totals <- unlist(setNames(totals, names(NEIByYear)))

# Set device resolution.
dev.new(width = 480, height = 480)

# Plot histogram.
chart <- barplot(totals, main = 'Total PM2.5 Emissions in United States', xlab = 'Year', ylab = 'PM2.5 Emissions (10^6 Tons)')

# Show trend line.
smoothingSpline <- smooth.spline(seq_along(totals), totals)
lines(smoothingSpline, lwd = 1, lty = 2)

# Copy to png file.
dev.copy(png, file = 'plot1.png')
dev.off()