setwd("/Users/marysilva/Desktop/AMS204_Project")
library(data.table)
library(lubridate)

#read all data files contained in FlighData folder
temp = list.files(pattern="*.csv")
temp1 <- lapply(temp, fread) #requires library(data.table)

#merge each data file into one single data frame
Airline_Data <- rbindlist(temp1)
Airline_Data = Airline_Data[,1:12]
dim(Airline_Data)
#omit any NA data
Airline_Data = na.omit(Airline_Data)
Airline_Data$ORIGIN = as.factor(Airline_Data$ORIGIN)
head(Origin)
####Split data by Origin Destination####
byOrigin = split(Airline_Data, Airline_Data$ORIGIN)
ARRAY = NULL
length(byOrigin)
for(i in 1:length(byOrigin)){
  ARRAY[i] = dim(byOrigin[[i]])[1]
}

index_cities = which(ARRAY >= 80000)
A = data.frame(summary(byOrigin[index_cities]))
Names = levels(A$Var1)


for(i in 1:length(index_cities)){
  assign(Names[i],data.frame(byOrigin[index_cities[i]]))
}



library(dplyr)
Reformat_Airlines = function(df){
  colnames(df) = colnames(Airline_Data)
  df$ARR_TIME_BLK = format(strptime(df[,10],format = '%H'), "%H:%M")
  df$DATE_TIME = paste(df$FL_DATE, df$ARR_TIME_BLK)
  newDF = df %>% group_by(DATE_TIME, ORIGIN) %>%
    arrange(DATE_TIME) %>%
    summarise(Med_Delay = median(ARR_DELAY_NEW),
              Med_Dist = median(DISTANCE))
  newDF <- data.frame(newDF)
  return(newDF)
  }


ATL_Medians = Reformat_Airlines(ATL)
BOS_Medians = Reformat_Airlines(BOS)
BWI_Medians = Reformat_Airlines(BWI)
CLT_Medians = Reformat_Airlines(CLT)
DEN_Medians = Reformat_Airlines(DEN)
DFW_Medians = Reformat_Airlines(DFW)
DTW_Medians = Reformat_Airlines(DTW)
EWR_Medians = Reformat_Airlines(EWR)
FLL_Medians = Reformat_Airlines(FLL)
IAH_Medians = Reformat_Airlines(IAH)
JFK_Medians = Reformat_Airlines(JFK)
LAS_Medians = Reformat_Airlines(LAS)
LAX_Medians = Reformat_Airlines(LAX)
LGA_Medians = Reformat_Airlines(LGA)
MCO_Medians = Reformat_Airlines(MCO)
MDW_Medians = Reformat_Airlines(MDW)
MSP_Medians = Reformat_Airlines(MSP)
ORD_Medians = Reformat_Airlines(ORD)
PHX_Medians = Reformat_Airlines(PHX)
SFO_Medians = Reformat_Airlines(SFO)
SEA_Medians = Reformat_Airlines(SEA)
SLC_Medians = Reformat_Airlines(SLC)


####################################################################################
########################   WEATHER DATA FOLDER   ###################################
####################################################################################
setwd("/Users/marysilva/Desktop/AMS204_Project/dataset-weather")
#read all data files contained in FlighData folder
tempor = list.files(pattern="*.csv")
tempor1 <- lapply(tempor, fread) #requires library(data.table)

Weather_Data <- rbindlist(tempor1)
#names(Weather_Data)
Weather_Data = Weather_Data[,c(2,6,8,9,11,18,25)]

dim(Weather_Data)

head(Weather_Data)

names(Weather_Data) = c("ORIGIN", "DATE", "SKY_COND", 
                        "VISIBILITY", "TEMPF", "WIND_SP", "RAINFALL")
newDATE = strptime(Weather_Data$DATE, format = "%Y-%m-%d %H:%M")
Weather_Data = data.frame(Origin = Weather_Data$ORIGIN, 
                          newDATE, 
                          SKY_COND = Weather_Data$SKY_COND,
                          VISIBILITY = Weather_Data$VISIBILITY,
                          TEMPF = Weather_Data$TEMPF,
                          WIND_SP = Weather_Data$WIND_SP,
                          RAINFALL = Weather_Data$RAINFALL)

Weather_Data = Weather_Data[,-3]

Weather_Data$VISIBILITY = as.numeric(Weather_Data$VISIBILITY)
Weather_Data$TEMPF = as.numeric(Weather_Data$TEMPF)
Weather_Data$WIND_SP = as.numeric(Weather_Data$WIND_SP)
Weather_Data$RAINFALL = as.numeric(Weather_Data$RAINFALL)
Weather_Data[is.na(Weather_Data)] <- 0

head(Weather_Data)
Weather_Data = na.omit(Weather_Data)
summary(Weather_Data)

HourlyWeather2 <- Weather_Data %>%
  group_by(ROUNDED_DATE = floor_date(newDATE, unit = "hour"), Origin) %>%
  arrange(ROUNDED_DATE) %>%
  summarise(
    VISIBILITY = mean(VISIBILITY),
    TEMPF = mean(TEMPF),
    WIND_SP = mean(WIND_SP),
    RAINFALL = mean(RAINFALL)
  )
head(HourlyWeather2)
HourlyWeather2=data.frame(HourlyWeather2)
HourlyWeather2$Origin = as.factor(HourlyWeather2$Origin)
levels(HourlyWeather2$Origin) = Names[c(1,3:8,11:13,15,17:22)]

summary(HourlyWeather2)


#write.csv(HourlyWeather2, "Weather_Data_Dec2.csv")
