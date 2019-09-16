library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)

#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')

ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))



##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

#Your code here

#changing from full_long to full_wide and filtering only numeric data, getting rid of NA observations
full_wide <- spread(full_long, key = data, value = value)%>%
  filter_if(is.numeric, all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime),
         year = year(DateTime))

summary(full_wide)

#plotting correlation 
ggplot(data=full_wide)+
  geom_point(mapping = aes(x=ndmi, y=ndvi, color=site))+
  theme_few()+
  theme(legend.position = c(0.4, 0.2))

#filtering summer months from data
summer_only <- filter(full_wide, month %in% c(6,7,8,9))
#graphing only summer month trends
ggplot(data= summer_only)+
  geom_point(mapping = aes(x=ndmi, y=ndvi, color=site))+
  theme_few()+
  theme(legend.position = c(0.8, 0.8))
## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer?

## Your code here

ndvi_long <- gather(ndvi, key='site', value='ndvi', -DateTime) %>%
  filter(!is.na(ndvi))

#filtering to only have months Jan-Apr
winter1 <- filter(full_wide, month %in% c(1,2,3,4)) %>%
  mutate(year = year(DateTime),
         month = month (DateTime)) %>%
  group_by(year, site) %>%
  summarize(mean_ndsi = mean(ndsi))


#filtering for Jun-Aug
summer1 <- filter(full_wide, month %in% c(6,7,8)) %>%
  mutate(year = year(DateTime),
         month = month (DateTime)) %>%
  group_by(year, site) %>%
  summarise(mean_ndvi = mean(ndvi))

#new table combining this data
summer_winter<- inner_join(summer1, winter1, by = c("year", "site")) 

#plotting the trend between ndsi and ndvi

ggplot(summer_winter, aes(y=mean_ndvi, x=mean_ndsi, color = site)) +
  geom_point()+
  theme_few()

  

 ## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here
#filtering for pre and post fire, fire happened in 2002
pre_post <- mutate(summer_winter, condition = if_else(year < 2002, "pre", "post"))

#graphing pre and post burn and burn and unburn
ggplot(data=pre_post, aes(y=mean_ndvi, x=mean_ndsi, color = site))+
  geom_point()+
  theme_few()+
  facet_wrap(~condition)


## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

annual <- ndvi %>%
  filter(!is.na(burned), !is.na(unburned)) %>%
  mutate(month=month(DateTime)) %>%
  mutate(year=year(DateTime)) %>%
  gather(key='site',
         value='value', 
         -DateTime, -month, -data, -year) %>%
  group_by(month)%>%
  summarise('mean_ndvi'= mean(value))

#pre
pre <- ndvi %>%
  filter(!is.na(burned), !is.na(unburned)) %>%
  mutate(month=month(DateTime)) %>%
  mutate(year=year(DateTime)) %>%
  filter(year < 2002) %>%
  gather(key='site',
         value='value', 
         -DateTime, -month, -data, -year) %>%
  group_by(month)%>%
  summarise('mean_ndvi'= mean(value))

#post
  post <- ndvi %>%
    filter(!is.na(burned), !is.na(unburned)) %>%
    mutate(month=month(DateTime)) %>%
    mutate(year=year(DateTime)) %>%
    filter(year >= 2002) %>%
    gather(key='site',
         value='value', 
         -DateTime, -month, -data, -year) %>%
    group_by(month)%>%
    summarise('mean_ndvi'= mean(value))
  
#gathering the data
both <- inner_join(pre, post, by = 'month') %>%
  gather(key='site', value='value', -month)

#plotting data
ggplot(data=both, aes(x = month, y = value, color = site))+
  geom_point()+
  theme_few() +
  theme(legend.position = c(0.7, 0.2)) +
  scale_color_manual(labels = c("Pre-burn", "Post-burn"), values = c("blue", "red")) 

##### Question 5 ####
#What month is the snowiest on average?
annual_snow <- ndsi %>%
  filter(!is.na(burned), !is.na(unburned)) %>%
  mutate(month=month(DateTime)) %>%
  mutate(year=year(DateTime)) %>%
  gather(key='site',
         value='value', 
         -DateTime, -month, -data, -year) %>%
  group_by(month)%>%
  summarise('mean_ndsi'= mean(value))

ggplot(annual_snow, aes(x=month, y=mean_ndsi))+
  theme_few()+
  geom_point()
  
