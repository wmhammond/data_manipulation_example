#### load & explore data ####
library(data.table)
library(purrr)
library(tidyverse)
library(zoo)

gtm <- read.csv("./data/GTM_02.csv")

list.files(path = './data/') #how to see what files are in a folder

gtm.combined <- list.files(pattern = "*.csv", path = './data/', full.names=TRUE) %>%
  map_df(~fread(.))

head(gtm.combined)
ggplot(gtm, aes(x=tmax)) + geom_histogram()
ggplot(gtm.combined, aes(x=tmax)) + geom_histogram() + facet_wrap(~gtm)

ggplot(gtm.combined, aes(x=index, y=tmax)) + geom_line()+ facet_wrap(~gtm)

ggplot(gtm.combined, aes(x=index/12+1958, y=ppt)) + 
  geom_col() + 
  facet_wrap(~gtm) 


#### summarise data ####
head(gtm)

# Question: what was the warmest month on average?

gtm.summarised <- gtm %>%
  group_by(year) %>%
  summarise(annual.tmax = mean(tmax))
  
gtm.summarised.all <- gtm %>%
  group_by(year) %>%
  summarise_all(list(~mean(.), ~min(.), ~max(.)))

ggplot(gtm, aes(x=index/12+1958, y=tmax)) + geom_line()
ggplot(gtm.summarised.all, aes(x=year, y=tmax_mean)) + geom_line() +
  geom_line(aes(x=year, y=tmax_min), color = 'blue') +
  geom_line(aes(x=year, y=tmax_max), color = 'red')


# For multiple sites:
gtm.summarised.combined <- gtm.combined %>%
  group_by(gtm, year) %>%
  summarise(annual.tmax = mean(tmax))

gtm.summarised.all.combined <- gtm.combined %>%
  group_by(gtm, year) %>%
  summarise_all(list(~mean(.), ~min(.), ~max(.))) 

ggplot(gtm.summarised.all.combined, aes(x=year, y=tmax_mean)) + geom_line() +
  geom_line(aes(x=year, y=tmax_min), color = 'blue') +
  geom_line(aes(x=year, y=tmax_max), color = 'red') + facet_wrap(~gtm) +
  theme_bw()

#### calculate a rolling mean, or trend line ####
gtm$tmax_rollmean <- rollmean(gtm$tmax, 6, fill = NA)
ggplot(gtm, aes(x=index/12+1958, y=tmax)) + geom_line() +
  geom_line(aes(x=index/12+1958, y=tmax_rollmean), color = 'red')



