# create vectors
temps <- c(20.8, 19.6, 18.7, 17.8, 17.7, 19.1, 
               21.4, 22.8, 24.1, 26.5, 27.8, 29.4, 
               30.7, 31.8, 32.4, 32.0, 31.8, 31.5, 
               31.2, 29.9, 28.3, 27.1, 26.2)

datetimes <- c("30/06/2025 01:00:00", "30/06/2025 02:00:00", 
               "30/06/2025 03:00:00", "30/06/2025 04:00:00", 
               "30/06/2025 05:00:00", "30/06/2025 06:00:00", 
               "30/06/2025 07:00:00", "30/06/2025 08:00:00", 
               "30/06/2025 09:00:00", "30/06/2025 10:00:00", 
               "30/06/2025 11:00:00", "30/06/2025 12:00:00", 
               "30/06/2025 13:00:00", "30/06/2025 14:00:00", 
               "30/06/2025 15:00:00", "30/06/2025 16:00:00", 
               "30/06/2025 17:00:00", "30/06/2025 18:00:00", 
               "30/06/2025 19:00:00", "30/06/2025 20:00:00", 
               "30/06/2025 21:00:00", "30/06/2025 22:00:00", 
               "30/06/2025 23:00:00")

# extract index
fir_temp <- temps[1]

fir_time <- datetimes[1]

# test
print(fir_temp)
print(fir_time)

# sub-string
spl <- strsplit(fir_time, " ")
date <- spl[[1]][1]
print(date)

# calculate temperatures
low <- min(temps)
high <- max(temps)
avg <- round(mean(temps), digits = 2)

# convert temperatures
low_f <- round((low * 9 / 5) + 32, digits = 2)
high_f <- round((high * 9 / 5) + 32, digits = 2)
avg_f <- round((avg * 9 / 5) + 32, digits = 2)

# print out
cat("Temperature on:", date,
    "\nLow:", paste0(low, "°C (", low_f, "°F) | High: ", 
                     high, "°C (", high_f, "°F)"),
    "\nDaily average:", paste0(avg, "°C (", avg_f, "°F)"),
    sep = "")
