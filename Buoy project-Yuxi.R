library(lubridate)
library(tidyverse)
library(seasonal)
library(fpp2)
library(stringr)

#Step1: Import buoy data and tidy data
#In order to read txt file automatically
# make URLs

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(2000:2019)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

###  Read the data from the website
N <- length(urls)
colname_1 <- colnames(read.table(urls[1],fill=TRUE, header = TRUE))
colname_2 <- colnames(read.table(urls[7],fill=TRUE, header = TRUE))
for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read.table(urls[i],fill=TRUE ,header = TRUE))
  )
  file <- get(filenames[i])
  if(ncol(file)==17){
    colnames(file)=colname_1
  }else{
    colnames(file)=colname_2
  }
  if(i == 1){
    MR <- file
  }else{
    MR <- dplyr::bind_rows(MR, file)
  }
}


# To adjust the date, and deal with outliers in order to get tidy data. 
MR <- data.frame(MR)
time <- MR %>% select(YYYY,MM,DD,hh)
time <- make_datetime(MR$YYYY,time$MM,time$DD,time$hh)
time <- data.frame(time)
MR1 <- dplyr::mutate(time,MR)
MR1 <- MR1[,c(-2,-3,-4,-5)]
MR1$time = as.POSIXct(MR1$time)
MR1 <- MR1[,-c(5,6,7,8,12,13,14,15)]  
# After watching the output we found that:WVHT,DPD,APD,MWD,DEWP,VIS TIDE,MM are all useless.
MR1 <- filter(MR1,MR$WD<999&MR$WSPD<99&MR$GST<99&MR$BAR<9999&MR$ATMP<999&MR$WTMP<999)
summary(MR1)
#sampling the data, reduce data size by using the mean value for each day.
buoy <- MR1%>%
  group_by(date(time))%>%
  summarize(mean(WD),mean(WSPD),mean(GST),mean(BAR),mean(ATMP),mean(WTMP))

# Transfer the data into time series's form
ATMP <- buoy[,6] 
WTMP <- buoy[,7] 
ATMP <- ts(ATMP,frequency =365,start=c(2000,1))
tsdisplay(ATMP)
WTMP <- ts(WTMP,frequency =365,start=c(2000,1))
tsdisplay(WTMP)

# Using ATMP and WTMP to see whether there has a global warming phenomenon.
#Method one

dc1<-decompose(ATMP)
plot(dc1)
dc2<-decompose(WTMP)
plot(dc2)

#Method 2
model1 <- lm(ATMP~time(ATMP))
summary(mode1)
ggplot(data=ATMP, aes(x=time(ATMP), y=ATMP)) + 
  geom_point() + 
  stat_smooth(method="lm", formula=y ~ x, se=TRUE)

model2 <- lm(WTMP~time(WTMP))
summary(model2)
ggplot(data=WTMP, aes(x=time(WTMP), y=WTMP)) + 
  geom_point() + 
  stat_smooth(method="lm", formula=y ~ x, se=TRUE)

buoy1 <- buoy[,-c(1)]
b <- cor(buoy1)
corrplot(b,method="pie")

#Drawing a pie plot of the correlation matrix to see if there are variables related to temperature
buoy1 <- buoy[,-c(1)]
b <- cor(buoy1)
corrplot(b,method="pie")


