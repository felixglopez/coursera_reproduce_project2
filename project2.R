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


#Create a bar plot to make visualization easier (total harm)

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


#look at the two separately


#alternative way to look at it
# Reviewing events that cause the most fatalities ( The Top-10 Fatalities by Weather Event )

## Procedure = aggregate the top 10 fatalities by the event type and sort the output in descending order

fatalities <- aggregate(FATALITIES ~ EVTYPE, data = sd, FUN = sum)
Top10_Fatalities <- fatalities[order(-fatalities$FATALITIES), ][1:10, ] 
Top10_Fatalities 


#Now looking at injuries
#Reviewing events that cause the most injuries ( The Top-10 Injuries by Weather Event )

## Procedure = aggregate the top 10 injuries by the event type and sort the output in descending order

Injuries <- aggregate(INJURIES ~ EVTYPE, data = sd, FUN = sum)
Top10_Injuries <- Injuries[order(-Injuries$INJURIES), ][1:10, ] 
Top10_Injuries 




# Plot of Top 10 Fatalities & Injuries for Weather Event Types ( Population Health Impact )

## Proecedure = plot graphs showing the top 10 fatalities and injuries

par(mfrow=c(1,2),mar=c(10,3,3,2))
barplot(Top10_Fatalities$FATALITIES,names.arg=Top10_Fatalities$EVTYPE,las=2,col="sienna",ylab="fatalities",main="Top 10 fatalities")
barplot(Top10_Injuries$INJURIES,names.arg=Top10_Injuries$EVTYPE,las=2,col="sienna",ylab="injuries",main="Top 10 Injuries")


# Which types of events have the greatest economic consequences

unique(sd$PROPDMGEXP)

econ_con <- sd |> 
        group_by(EVTYPE) |> 
        summarize(conseq = sum(PROPDMG)) |> 
        arrange(desc(conseq)) 

        print(econ_con, n=20)

 # Assigning values for the property exponent Storm Data
        
strmdata <- storm
        
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "K"] <- 1000
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "M"] <- 1e+06
        strmdata$PROPEXP[strmdata$PROPDMGEXP == ""] <- 1
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "B"] <- 1e+09
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "m"] <- 1e+06
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "0"] <- 1
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "5"] <- 1e+05
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "6"] <- 1e+06
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "4"] <- 10000
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "2"] <- 100
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "3"] <- 1000
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "h"] <- 100
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "7"] <- 1e+07
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "H"] <- 100
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "1"] <- 10
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "8"] <- 1e+08   
        
#there are some invalid symbols
        
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "+"] <- 0
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "-"] <- 0
        strmdata$PROPEXP[strmdata$PROPDMGEXP == "?"] <- 0
        
        
        
        #Now I calculate the property damage value
        strmdata$PROPDMGVAL <- strmdata$PROPDMG * strmdata$PROPEXP
        
        
        #Doing the same to damage in crops
        
        unique(strmdata$CROPDMGEXP)

        # Assigning values for the crop exponent strmdata 
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "M"] <- 1e+06
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "K"] <- 1000
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "m"] <- 1e+06
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "B"] <- 1e+09
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "0"] <- 1
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "k"] <- 1000
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "2"] <- 100
        strmdata$CROPEXP[strmdata$CROPDMGEXP == ""] <- 1
        
        # Assigning '0' to invalid exponent strmdata
        strmdata$CROPEXP[strmdata$CROPDMGEXP == "?"] <- 0
        
        # calculating the crop damage 
        strmdata$CROPDMGVAL <- strmdata$CROPDMG * strmdata$CROPEXP
        
        
        # Property Damage Summary
        
        ## Procedure = aggregate the property damage by the event type and sort the output it in descending order
        
        prop <- aggregate(PROPDMGVAL~EVTYPE,data=strmdata,FUN=sum,na.rm=TRUE)
        prop <- prop[with(prop,order(-PROPDMGVAL)),]
        prop <- head(prop,10)
        print(prop)
        
        
        #Now crop damage Summary
        # Crop Damage Summary
        
        ## Procedure = aggregate the crop damage by the event type and sort the output it in descending order
        
        crop <- aggregate(CROPDMGVAL~EVTYPE,data=strmdata,FUN=sum,na.rm=TRUE)
        crop <- crop[with(crop,order(-CROPDMGVAL)),]
        crop <- head(crop,10)
        print(crop)
        
        
        #Now plotting the result
        
        # Plot of Top 10 Property & Crop damages by Weather Event Types ( Economic Consequences )
        
        ##plot the graph showing the top 10 property and crop damages
        
        par(mfrow=c(1,2),mar=c(11,3,3,2))
        barplot(prop$PROPDMGVAL/(10^9),names.arg=prop$EVTYPE,las=2,col="blue",ylab="Prop.damage(billions)",main="Top10 Prop.Damages")
        barplot(crop$CROPDMGVAL/(10^9),names.arg=crop$EVTYPE,las=2,col="blue",ylab="Crop damage(billions)",main="Top10 Crop.Damages")
        