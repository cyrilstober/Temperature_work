rm(list = ls())
#-------------------------------------------------------------------------------
## set working directory
setwd("~/Desktop/temp")


#-------------------------------------------------------------------------------


## first of all use install.packages("packageName")
# install.packages('terra', repos='https://rspatial.r-universe.dev')
# install.packages("terra")
# install.packages("rgdal") ## do this for the rest of the packages
# 
# install.packages("raster")
# install.packages("ncdf4")
# install.packages("cython")
library(rgdal)
library(sp)
library(raster)
library(terra)
library(ncdf4)



#-------------------------------------------------------------------------------
#### set file path
d='~/Desktop/temp'


#### create .nc4 list
lf = list.files(d, pattern = ".nc$")

#### creat function for converting .nc4 files to tif
f = function(x) {
  r= rast(paste0('NETCDF4:',x))
  crs(r) <- "+proj=longlat +datum=WGS84"
  writeRaster(r,gsub(".nc$", "_from_nc_.tif", as.character(x)), overwrite=T)
}

#### apply the function and convert .nc4 files to tif
temp_data<- lapply(lf,f)

# system.time(temp_data<- mclapply(lf,f,mc.cores = 7))

#### create a list of .tif files
data_list = list.files(d, pattern = ".tif$") 

#-------------------------------------------------------------------------------
## import shape files of study area

# required package
library(rgdal)
dsn = "~/Desktop/temp"

###
shpfile = readOGR(".","study_area_boundary")

options(stringsAsFactors = FALSE)

#change crs of shapefile to crs of one of the raster

shp2 <- spTransform(shpfile, crs("+proj=longlat +datum=WGS84 +no_defs"))


plot(shp2)

extent(shp2)

# set crop extent

crop_ext = extent(6.3,7,5.4,6.8)








temp_rast <- rast(temp_data)


### crop raster using crop_ext to readuce 
temp_data_crop <- crop(temp_rast, crop_ext)

temp_mask <- mask(brick(temp_data_crop), shp2)

plot(temp_mask)


library(raster)
## increasing raster resolution
# check current resolution
res(temp_mask)

#disaggregate from 0.0417x0.0417 resolution to 0.0035 (factor = 14.65)
## note: aggregate() reduces resolution
temp_resolution <- disaggregate(temp_mask, fact=70)
res(temp_resolution)
#[1] 0.0005 0.0005

plot(temp_resolution)



#lebel for Temperature
celcious=expression("Temperature" (degree*C))


##------------------------------------------------------------------------------
### find mean temperature value

### loop for mean aet
fun_mean <- function(f) {
  #import tif as brick
  r <- brick(f)
  #crop brick
  r2 <- crop(r, crop_ext)
  #change resolution
  r3 <- disaggregate(r2, fact=16.67)
  #mask with polygon but you must have imported polygon already
  r4 = mask(r3, shp2)
  #calculate mean
  r5<- data.frame(cellStats(r4, stat='mean', na.rm=T))
  #print result
  print(r5)
}


#### apply loop function
summary <- data.frame(lapply(data_list, fun_mean))


#### transpose file
temp_mean = data.frame(t(summary))


#-------------------------------------------------------------------------------
#####
######

###if it was a single line data use this to rearrange into a matrix
####temp_mat <- as.data.frame(matrix(unlist(temp_max), ncol = 12,
####nrow = 22, byrow = TRUE))

colnames(temp_mean) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

rownames(temp_mean) <- 1991:2021

dim(temp_mean)

plot(temp_mean)
print(temp_mean[,1])
colnames(temp_mean)
rownames(temp_mean)


# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("forcats")
library(ggplot2)
library(tibble)
library(dplyr)
library(forcats)
# install.packages("tidyr")
library(tidyr)

temp_ts <- temp_mean %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::pivot_longer(cols = -year, names_to = "month",
                      values_to = "tmax") %>%
  mutate(month = as.factor(month) %>%
           forcats::fct_relevel(month.abb))

#########
temp_ts %>%
  ggplot(aes(x = month, y = year, fill = tmax )) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(y = "Year", x = "Month", fill = "Mean temp.")
#########
temp_ts %>%
  ggplot(aes(x = month, y = tmax, fill = year, color = year, group = year)) +
  # geom_line(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_line(se = FALSE, span = 0.9) +
  labs(y = "mean temp", x = "Month")


# groups Nov, Dec, Jan+1, Feb+1
# groups Jun, Jul, Aug, Sep
# groups Feb, Mar, Apr, May

temp_ts %>%
  ggplot(aes(x = year, y = tmax)) +
  geom_line() +
  scale_fill_viridis_c() +
  labs(y = "Temperature (C)", x = "Month", fill = "Mean temp.")







# ndvi_ts
# evi_ts
# kndvi_ts
# tmax_ts
# ppt_ts


# ------------------------------------------------------------------------------
library(scales)
temp_ts
### add data to timeseries data
start_date <- as.Date("1991/01/01")
temp_ts$date <- seq(start_date, by = "month", length.out = 372)

fig45=ggplot(temp_ts, aes(x=date, y=tmax))+geom_point()+geom_line()+ stat_smooth(data = temp_ts, aes(date, tmax),method="loess",span=0.1,se=T)+
  theme_bw()+ ggtitle("")+
  labs(x = "Date", y ="Temperature (*C)")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
                                   labels=date_format("%Y"))

temp_ts= temp_ts %>% 
         mutate(season= ifelse(month=="Apr"|month=="May"|month=="Jun"|month=="Jul"|month=="Aug"|month=="Sep"|month=="Oct","Wet","Dry"))


fig46=ggplot(temp_ts, aes(x=date, y=tmax,fill=season))+geom_bar(stat="identity")+
  theme_bw()+ ggtitle("")+
  labs(x = "Date", y ="Temperature (*C)")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
                                                       labels=date_format("%Y"))



celcious=expression("Temperature" (degree*C))

write.csv(temp_ts,"temp_ts.csv")


#temp_ts2=filter(temp_ts,month %in% c("Jan","Feb","Mar","Apr","May","Jun"))
temp_ts2 %>%
  ggplot(aes(x = date, y = tmax)) +
  geom_line() +
  geom_point() +
  labs(title = "Variable trends", x = "Year", y= "temp (mm)") +
  theme(legend.position="none") +
  geom_smooth()

Fig13=temp_ts %>%
  ggplot(aes(x = month, y = tmax, group=year,color=year)) +
  geom_point() +
  geom_line()+
  labs(title = "Variable trends", x = "Month") +
  facet_wrap(~ year, ncol = 5, scale = "free_y") +
  #scale_color_continous() +
  theme_bw() +
  theme(legend.position='none')

ggsave("Fig13.jpeg",Fig13,dpi = 500)
# excluding some months
temp_ts3=filter(temp_ts, month %in% c("Jul","Aug","Sep","Oct","Nov","Dec"))

Fig14=temp_ts3 %>%
  ggplot(aes(x = month, y = tmax, group=year,color=year)) +
  geom_point() +
  geom_line()+
  labs(title = "Variable trends", x = "Month") +
  facet_wrap(~ year, ncol = 5, scale = "free_y") +
  #scale_color_continous() +
  theme_bw() +
  theme(legend.position='none')

ggsave("Fig14.jpeg",Fig14,dpi = 500)



cor(ppt_ts$ppt,Runoff_ts$q)

length(Runoff_ts$q)
#-------------------------------------------------------------------------------
### find correlation between variables
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

my_data <- data.frame(ppt_ts$ppt,Runoff_ts$q,temp_ts$tmax)

names(my_data) <- c("Precipitation", "Runoff","Temperature")

plot(my_data)
cor(my_data,method="pearson")



Data.num <- my_data[c("Precipitation","Runoff","Temperature")]

chart.Correlation(Data.num,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)



