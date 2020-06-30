file.fun <- function(x){
  file.copy(x, "2020WY629800")
}

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
