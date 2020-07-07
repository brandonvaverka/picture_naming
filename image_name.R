
# Library Section ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(exiftoolr)

# Temporary read in of dataframe ------------------------------------------

picture_info <- read.table("df_name_time",header = TRUE,sep = ",") %>% 
  select(x.FileName,x.DateTimeOriginal) %>% 
  as_tibble() %>% 
  mutate(.,Date=as.POSIXct(x.DateTimeOriginal, format="%Y-%m-%d %H:%M:%S")) %>% 
  select(-x.DateTimeOriginal)


# Gather Picture Info -----------------------------------------------------

#Create a variable with full path to all images
all_pictures <- list.files("test_photos/",pattern = "*.JPG",full.names = TRUE)

#Use exiftoolr to generate tibble containing picture path, name, and original creation date
picture_info <- exif_read(path = all_pictures) %>% 
  select(SourceFile,FileName,DateTimeOriginal) %>% 
  as_tibble() %>% 
  mutate(.,Date=as.POSIXct(DateTimeOriginal, format="%Y:%m:%d %H:%M:%S")) %>% 
  select(-DateTimeOriginal)


# Group and Rename --------------------------------------------------------

# Create a picture grouping constraint and group pictures for each location by lag difference.
# Works by creating a lag period and calculating the difference between the lag and the origninal
# The return is the difference in minutes, stored in lag_time. Next groups are created using an ifelse
# and a cumulative sum.
picture_groups <- picture_info %>% 
  mutate(.,lag_time = lag(Date,default=Date[1]) %>% 
                      difftime(Date,.,units = "mins"),
           group_num = ifelse(lag_time>15,1,0) %>% 
                    cumsum(.)) %>% 
  group_by(., group_num) %>% 
  mutate(.,new_name =str_c("2020WY629",group_num+800,"-",1:n(),".JPG")) %>% 
  select(.,-lag_time)

# Renaming the files with the new file names
file.rename(picture_groups$FileName,picture_groups$new_name)