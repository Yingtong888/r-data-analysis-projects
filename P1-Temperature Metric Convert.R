#Celsius
LD_c <- 25
print(LD_c)
cat("The temperature in London is", LD_c, "°C", "\n")


#Fahrenheit to Celsius
LD_f <- round((LD_c * 9 / 5) + 32)
print(LD_f)

#Fahrenheit to Celsius
NY_f <- 80
NY_c <- round((NY_f - 32) *5 / 9, digits = 0)
print(NY_f)
cat("The temperature in NY is", NY_f, "°F", "\n")

#print information
cat("The temperature in London is", LD_c, "°C, which is", LD_f, "°F", "\n")
cat("The temperature in NY is", NY_f, "°F, which is", NY_c, "°C", "\n")

#test
LD_c <- 10
cat("The temperature in London is", LD_c, "°C", "\n")

LD_f <- round((LD_c * 9 / 5) + 32)

NY_f <- 20
cat("The temperature in NY is", NY_f, "°F", "\n")

NY_c <- round((NY_f - 32) *5 / 9, digits = 0)

cat("The temperature in London is", LD_c, "°C, which is", LD_f, "°F", "\n")
cat("The temperature in NY is", NY_f, "°F, which is", NY_c, "°C", "\n")

