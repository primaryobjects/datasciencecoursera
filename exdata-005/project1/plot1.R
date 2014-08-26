library(sqldf)

# Read data file, selecting only those rows between 2007-02-01 and 2007-02-02.
data <- read.csv.sql(file = 'household_power_consumption.txt', sep = ';', header = TRUE, sql = "select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")

# Setup date/time format.
dateFormat <- '%d/%m/%Y %H:%M:%S'

# Combine date and time columns into single field and convert to Date object.
data$DateTime <- as.POSIXlt(paste(data$Date, data$Time), format = dateFormat)

# Set margin spacing in plot.
par(mar = c(4, 4, 2, 2))

# Plot histogram.
hist(data$Global_active_power, main = 'Global Active Power', xlab = 'Global Active Power (kilowatts)', col = 'Red', cex.lab=0.75, cex.axis = 0.7, cex.main = 0.9)

# Copy to png file.
dev.copy(png, file = 'plot1.png', width = 480, height = 480)
dev.off()