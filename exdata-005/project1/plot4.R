library(sqldf)

# Read data file, selecting only those rows between 2007-02-01 and 2007-02-02.
data <- read.csv.sql(file = 'household_power_consumption.txt', sep = ';', header = TRUE, sql = "select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")

# Setup date/time format.
dateFormat <- '%d/%m/%Y %H:%M:%S'

# Combine date and time columns into single field and convert to Date object.
data$DateTime <- as.POSIXlt(paste(data$Date, data$Time), format = dateFormat)

# Set margin spacing in plot (mfrow = 2x2 plots, mar = margin on axis labels[left, bottom], mgp = bottom axis margin[bottom label, axis markers])
par(mfrow = c(2,2), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))

# Set device resolution.
dev.new(width = 480, height = 480)

# Plot 1.
plot(data$DateTime, data$Global_active_power, type = 'l', xlab = '', ylab = 'Global Active Power', cex.lab = 0.75, cex.axis = 0.8)



# Plot 2.
plot(data$DateTime, data$Voltage, type = 'l', xlab = 'datetime', ylab = 'Voltage', cex.lab = 0.75, cex.axis = 0.8)



# Plot 3.
plot(data$DateTime, data$Sub_metering_1, type = 'l', xlab = '', ylab = 'Energy sub metering', cex.lab = 0.75, cex.axis = 0.8)

# Plot second line.
lines(data$DateTime, data$Sub_metering_2, col = 'Red')

# Plot third line.
lines(data$DateTime, data$Sub_metering_3, col = 'Blue')

# Draw legend.
legend('topright', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col = c('black', 'red', 'blue'), lty = 1, lwd = 1, bty = 'n', cex = 0.725, text.width = 85000)



# Plot 4.
plot(data$DateTime, data$Global_reactive_power, type = 'l', xlab = 'datetime', ylab = 'Global_reactive_power', cex.lab = 0.75, cex.axis = 0.8)


# Copy to png file.
dev.copy(png, file = 'plot4.png', width = 480, height = 480)
dev.off()