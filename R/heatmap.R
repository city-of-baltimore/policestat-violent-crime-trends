install.packages("plyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
library("plyr")
library("lubridate")
library("ggplot2")
library("dplyr")

col1 = "#d8e1cf" 
col2 = "#438484"

#Peek at the data set and attach the column names
head(violent_crime)
attach(violent_crime)
str(violent_crime)

#violent_crime$ymd <-mdy(crimedate)
violent_crime$month <- month(violent_crime$ymd, label = TRUE)
violent_crime$year <- year(violent_crime$ymd)
violent_crime$wday <- wday(violent_crime$ymd, label = TRUE)
violent_crime$hour <- hour(violent_crime$ymd)
attach(violent_crime)
head(violent_crime)

dayHour <- ddply(violent_crime, c( "hour", "wday"), summarise,
                 N    = length(ymd)
)

#reverse order of months for easier graphing
dayHour$wday <- factor(dayHour$wday, levels=rev(levels(dayHour$wday)))
attach(dayHour)

#Create Heatmap
ggplot(dayHour, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="Total Incidents")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Histogram of Seattle Incidents by Day of Week and Hour",
       x = "Incidents Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
