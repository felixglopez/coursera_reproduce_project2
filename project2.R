## Reproducible research - Course Project 2

#required libraries
library(tidyverse)
library(scales)

#Define the URL and destination of the files
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "storm.bz2"

#Download the zip file
download.file(fileUrl, "storm.bz2", method = "curl")

#Read a bz file as data.frame
storm <- read.csv(bzfile("storm.bz2"))
sd <- storm


#cheking file structure and data
dim(storm)
str(sd)
head(sd [, 1:5])

#converting date (month/day/year) and time to respective formats using lubridate

sd <- sd |> 
        mutate(BGN_DATE = mdy_hms(BGN_DATE)) |> 
        mutate(Year = year(BGN_DATE))  # Extract year


#Question 1 - Across the United States, which types of events (as indicated in the EVTYPE
#are most harmful with respect to population health?

#I'm assigning a object to measure the total fatalities and injuries by event type in all years.
summary(sd$FATALITIES )

harmful <- sd |> 
        group_by(EVTYPE) |> 
        summarize(total_harm = sum(FATALITIES+INJURIES)) |> 
        arrange(desc(total_harm))

print(harmful, n=20)


#Create a bar plot to make visualization easier

#selecting only the top 15 events
top_harm <- harmful |> 
        slice_max(total_harm, n = 15) 
        
#creating the bar plot

plot1 <-ggplot(top_harm, aes(x = reorder(EVTYPE, total_harm), y = total_harm)) +
        geom_bar(stat = "identity") +
        coord_flip() +  # Flip coordinates for better readability
        labs(title = "Total Harm by Event Type",
             x = "Event Type",
             y = "Total Harm") +
        theme_minimal()

print(plot1)

#calculating the impact of each event in %

harmful_perc <- harmful |> 
        mutate(percentage = total_harm/sum(total_harm)*100) |> 
        mutate(percentage = scales::percent(percentage/100, accuracy = 0.1))

print(harmful_perc, n=15)



#Lest check the number of harms caused by tornados by year

harm_tornado <-  sd |>
        filter(EVTYPE == "TORNADO") |> 
        group_by(BGN_DATE)|> 
        summarize(harm = sum(FATALITIES+INJURIES))


ggplot(harm_tornado, aes(x = harm)) +
        geom_histogram(binwidth=100, fill = "blue", color = "black") +
        labs(title = "Histogram of Tornado Counts",
             x = "Number of Tornadoes",
             y = "Frequency") +
        theme_minimal()

# Which types of events have the greatest economic consequentes

unique(sd$PROPDMGEXP)

econ_con <- sd |> 
        group_by(EVTYPE) |> 
        summarize(conseq = sum(PROPDMG)) |> 
        arrange(desc(conseq)) 

        print(econ_con, n=20)
        
        
        
        