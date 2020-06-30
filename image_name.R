library(jpeg)
a <- readJPEG('Me_working')
creation_date <- file.info('Me_working')$ctime
# file.rename("Me_working",paste("working", format(file.info("Me_working")$ctime,
#                                                   "%Y-%m-%d"), "rds", sep = "."))
file.rename("Me_working",paste("working", format(file.info("Me_working")$ctime,
                                                  "%Y-%m-%d"), "rds", sep = "."))

# format=c('y-m-d','h:m:s'))

b <- format(file.info("Me_working")$ctime,"h:m:s")

if (file.info("Me_working")$ctime)
  
  library(tidyverse)    

setwd(choose.dir())

# select the folder with your images in it
d$tc <- cut(d$t, breaks = "6 min")
files <- list.files("test_photos")

library(exif)
read_exif(paste(my.path,vec.jpg[1], sep="/"))$origin_timestamp

# a <- readJPEG(system.file("test_photos","IMG_0301.jpg", package = "jpeg"))

img <- readJPEG("./test_photos/IMG_0301.JPG")

b <- system.file("Me_working", package = "exif")

c <- read_exif(a)

file <- system.file("test_photos")
file_metadata <- read_exif(file)


keep <- files %>% file.info() %>% 
  select(mtime) %>% 
  rownames_to_column("file") %>%
  group_by(mtime) %>% 
  mutate(id = row_number()) %>% 
  filter(id == min(max(id),2)) %>% 
  pull(file)

# at this point `keep` is a list of file names meeting the specified criteria

dir.create("Keep")

file.copy(keep, paste0("./Keep/", keep))



library(tidyverse)  
library(plyr)
library(lubridate)
library(exiftoolr)
library(xts)
pics <- list.files("test_photos")
x <- exiftoolr::exif_read(paste0("./test_photos/",pics))
x$DateTimeOriginal <- ymd_hms(x$DateTimeOriginal)
df <- data.frame(x$FileName,x$DateTimeOriginal)

# a <- data.frame(seq(1:nrow(df)))
# df.a <- data.frame(x$FileName,x$DateTimeOriginal,a)

df$by15 = cut(df$x.DateTimeOriginal, breaks="15 min")
df.a <- df %>% arrange(by15)
df.b <- group_rows(df.a)

file.fun <- function(x){
  file.copy(x, "2020WY629800")
}
lapply(pics[df.b[[1]][2]], file.fun)


  #pick category == 1 rows
  # df2 <- df[df.c$ID.x.Filename==1,]
  # #for each timestamp create two variables start and end
  # #for +10 and -10 minutes
  # #then pick rows between them
  # lapply(df.c$x.DateTimeOriginal, function(time) {
  #   start <- time - minutes(10)
  #   end   <- time + minutes(10)
  #   df.c[between(df$TimeStamp, start, end),]
  # })
  # 
  # test <- within(df,abs(difftime(df$x.DateTimeOriginal, df$x.DateTimeOriginal,units="mins")) <= 10 )
  
df %>%
  mutate(Time = as.POSIXct(Time)) %>%
  group_by(lubridate::hour(Time), Word) %>%
  summarise(count=n())
  

  
  df$x.FileName <- revalue(df$x.FileName,c("IMG_0301.JPG"=paste0("2020WY629800")))
  

count.pics <- function(x,y=0){  
dtime <- sort(x)                           
start_time <- dtime[y+1]                                                        
end_time <- start_time + minutes(10)                                            
n.pics <- sum(dtime >= start_time & dtime <= end_time)
return(n.pics)
}


add.pics <- function(x){
  a <- x + 1
  return(a)
}



p.pics <- function(x){
  s_time <- dtime[x]                                                        
  e_time <- s_time + minutes(10)                                            
  n.pics <- sum(dtime >= s_time & dtime <= end_time)
  a.time <- n.pics + 1
  return(a.time)
}

pics <- function(x){
  a <- c.pics(x)
  b <- p.pics(a)
  return(b)
}

c.pics <- function(x){
  dtime <- sort(df$x.DateTimeOriginal)
  s_time <- dtime[x]                                                        
  e_time <- s_time + minutes(10)                                            
  n.pics <- sum(dtime >= s_time & dtime <= e_time)
  return(n.pics)
}
 
# Try using a Repeat loop

a <- 1:c.pics(1)
b <- length(a)+1:+c.pics(c.pics(1)+1)
even.more.pics <- more.pics + c.pics(more.pics)
