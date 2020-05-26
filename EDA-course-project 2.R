
# Peer-graded Assignment: EDA-Course Project 2

# The overall goal of this assignment is to explore the National Emissions Inventory database 
# and see what it say about fine particulate matter pollution in the United states over 
# the 10-year period 1999–2008. You may use any R package you want to support your analysis.

setwd('D:/DS Spec-2020/EDA-project2')
getwd()

#install the necessary packages
install.packages("data.table", "dplyr")
packages<- c("data.table", "dplyr")
lapply(packages, library, character.only = TRUE)


#download the zip file and unzip it
filepath<- getwd()
filename<- "NEI_data.zip"
fileurl<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileurl, destfile = filename, method = "curl")
unzip(filename)

list.files(filepath)

NEI<- readRDS("summarySCC_PM25.rds")
str(NEI)

head(NEI)
tail(NEI)

SCC<- readRDS("Source_Classification_Code.rds")
str(SCC)
head(SCC)
tail(SCC)



# DATASETS OBSERVATIONS DESCRIPTIONS
# 1. fips: A five-digit number (represented as a string) indicating the U.S. county
# 2. SCC: The name of the source as indicated by a digit string (see source code classification table)
# 3. Pollutant: A string indicating the pollutant
# 4. Emissions: Amount of PM2.5 emitted, in tons
# 5. type: The type of source (point, non-point, on-road, or non-road)
# 6. year: The year of emissions recorded


# Questions
# You must address the following questions and tasks in your exploratory analysis. 
# For each question/task you will need to make a single plot. Unless specified, 
# you can use any plotting system in R to make your plot.


# QUESTION 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all
#sources for each of the years 1999, 2002, 2005, and 2008.

#check for NA values
sum(is.na(NEI$Emissions))  
sum(is.na(NEI$year))


AnnualEmissions = aggregate(Emissions ~ year, NEI, sum)

png("Plot1.png")
barplot((AnnualEmissions$Emissions/1000000), names.arg = AnnualEmissions$year, col = "blue",
        xlab ="Years", ylab = "Emissions(millions)",main = "Emissions(PM 2.5) per year")
dev.off()

# QUESTION 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

BaltimoreEmissions<- NEI[NEI$fips == "24510", ]
AnnualBal_Emissions<- aggregate(Emissions ~ year, BaltimoreEmissions, sum)

png("Plot2.png")
barplot(AnnualBal_Emissions$Emissions, names.arg = AnnualBal_Emissions$year,
        col = "red", xlab = "Years", ylab = "Emissions(PM 2.5)",
        main = "Baltimore City Emissions (PM 2.5) per Year")
dev.off()

# QUESTION 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)

BaltimoreEmissions<- NEI[NEI$fips == "24510", ]
Bal_YrTy_Emissions<- aggregate(Emissions ~ year + type, BaltimoreEmissions, sum)

png("plot3.png")
ggplot(BaltimoreEmissions,aes(factor(year),Emissions,fill=type)) + 
    geom_bar(stat="identity") + facet_grid(.~type) + 
    labs(x="Years", y = "Emissions(PM 2.5)", 
         title = "Baltimore City Emissions (PM 2.5) per Year & type")
dev.off()


# QUESTION 4
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999–2008?


# merge the tables
NEISCC <- merge(NEI, SCC, by="SCC")

# search for "coal" and create boolean column

Emit_coal  <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
coal_NEISCC <- NEISCC[Emit_coal, ]

coalEmitperyr <- aggregate(Emissions ~ year, coal_NEISCC, sum)

png('plot4.png')
ggplot(coalEmitperyr, aes(x=factor(year), y=Emissions/1000,fill = year)) +
    geom_bar(stat="identity") + labs(x = "Years", y = "Total PM2.5 emissions(kilotons)", 
                                     title ="Emissions from coal combustion-related sources in USA" ) 
dev.off()   


#QUESTION 5.
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

Bal_veh_NEI <-(NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD", ])

Bal_veh_EmissionsPerYr <- aggregate(Emissions ~ year, Bal_veh_NEI, sum)

png('plot5.png')
ggplot(Bal_veh_EmissionsPerYr, aes(factor(year), Emissions, fill=year)) +
    geom_bar(stat="identity") + labs(x = "Years", y = "PM2.5 Emissions", 
                title = "Total Annual Emissions of ON-ROAD motor vehicle in Baltimore City")
dev.off()



# QUESTION 6. 
# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# table with Motor Vehicle emissions per year of Baltimore
Bal_veh_NEI<- summarize(group_by(filter(NEI, fips=='24510'), year), sum(Emissions)) 
# add place name
Bal_veh_NEI<- mutate(Bal_veh_NEI, Place = 'Baltimore City')

# table with Motor Vehicle emissions per year of LA County
LA_veh_NEI <- summarize(group_by(filter(NEI, fips=='06037'), year), sum(Emissions))
# add place name
LA_veh_NEI<- mutate(LA_veh_NEI, Place = 'Los Angeles County')
# union of the tables
Bal_LA_Emissions <- rbind(Bal_veh_NEI,LA_veh_NEI)
#renamed columns
colnames(Bal_LA_Emissions) <- c('Year', 'Emissions', 'Place')
# year converted to string
Bal_LA_Emissions$Year<- as.character(Bal_LA_Emissions$Year)


#Plot 6
png('plot6.png')
qplot(Year,data=Bal_LA_Emissions, geom="bar", weight=Emissions, facets=.~Place, fill=Year,
      xlab= "Years", ylab = "PM 2.5Emissions",
      main="Annual motor vehicle Emissions in Baltimore City and Los Angeles County")
dev.off()






