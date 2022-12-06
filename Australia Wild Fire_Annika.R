# 'fia' - Fires In Australia
fia <- read.csv("fire_nrt_M6_96062.csv", sep=",")
# format date
fia$acq_date <- as.Date(fia$acq_date, format = "%d/%m/%Y")
# take a look at the data
summary(fia)
head(fia)
# a random sample of the data can be used during design and development...
#NumberOfSamples <- 1000
#fia <- fia[ sample(1:dim(fia)[1], NumberOfSamples, replace=F ),  ]
  
# layout
#layout(matrix(c(1,3,5,2,4,5)), 3,2)
dev.new()
par(mfrow = c(2,2), mar = c(1,1,1,1),oma=c(1,1,1,1), bg = rgb(134, 150, 167, max = 255),
    col.axis = rgb(193, 203, 215, max = 255))


# figure 1: burned brightness-pie chart
# par(mfrow = c(1,2), mar = c(2,1,2,1))
library(dplyr)
fia2 <- fia %>% 
  mutate(brightness = case_when(
    brightness < 368 ~ "small",
    brightness >= 368 & brightness < 438 ~ "moderate",
    brightness >= 438 ~ "large"
  ))

fia2 <- fia2 %>% 
  mutate(frp = case_when(
    frp < 30 ~ "small",
    frp >= 30 & frp < 100 ~ "moderate_small",
    frp >= 100 & frp < 1000 ~ "moderate_large",
    frp >= 1000 ~ "large"
  ))

  col_pie = c(rgb(202, 195, 187, max=255),rgb(181, 196, 177, max=255),rgb(193, 203, 215, max=255), 
              rgb(193, 215, 215, max = 255))
  # fire_size0 = tapply(fia2$brightness, fia2$brightness, length)
  fire_size = tapply(fia2$frp, fia2$frp, length)
  pie(fire_size, main = "Fire Radiative Power",col = col_pie)
  legend("bottomleft", c("small", "moderate_small", "moderate_large", "large"), cex =0.7)
  # pie(fire_size0, main = "Fire Brightness",col = col_pie)
  # legend("bottomleft", c("small", "moderate", "large"), cex =0.7)
  
# figure 2: fire happened in day and night-line graph
  fire_day = fia %>% filter(daynight == "D") 
  fire_da = tapply(fire_day$daynight, fire_day$acq_date, length)
  fire_night = fia %>% filter(daynight == "N") 
  fire_ni = tapply(fire_night$daynight, fire_night$acq_date, length)
  
  plot(fire_ni, type="o", xlab = "DATE", ylab = "DAYNIGHT", labels = F,
       main = "Numbers of Day and Night Fires in Australia Between 2019.12.5-2020.1.5",
       axes = F, bty = "L", pch = 16, col = 20, panel.first = grid(7,10))
  lines(fire_da, col = "red", type = "o", pch = 15)
  axis(1, at = c(0, 5, 10, 15, 20, 25, 30, 35), labels = c("2019-12-5", "2019-12-10", "2019-12-15", "2019-12-20",
                          "2019-12-25", "2019-12-30", "2020-1-5", "2020-1-10"), 
              col = rgb(193, 203, 215, max = 255))
  axis(2, at = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000),
       col = rgb(193, 203, 215, max = 255))
  legend("topleft", c("day-fire", "night-fire"),pch = c(15,16), col=c("red",20))
  text(31,4100,labels = "3897", cex = 1, col = "red")
  text(25,5000,labels = "4975", cex = 1, col = 20)
  
  
# figure 3.1:  large map
  library(maps)
  library(mapproj)
  library( mapdata ) # "a cleaned-up version of the CIA World Data Bank II data"

  m = maps::map(regions = "Australia", plot = FALSE)
  maps::map( regions="Australia", fill = TRUE, 
       col = rgb(193, 203, 215, max = 255), bg = rgb(134, 150, 167, max = 255),
       xlim=c(110,160), ylim = c(-45,0))
  map.grid(m, lim = c(110, 160, -45, -10), col="gray86", 
           nx=10, ny=10, label=FALSE, lty=2, pretty = TRUE)
  map.axes(col = "grey86")
  fia3 <- fia %>%
    mutate(satellite = case_when(
      satellite == "Aqua"                ~ 20,# blue
      satellite == "Terra"~2# red
    ))
  text(135,-5, labels="Fire Collected by Different Satellites in Australia", 
       col="black", cex=1)
  points(fia3$longitude, fia3$latitude, pch=24, cex=c(fia3$brightness/500), 
         col=c(fia3$satellite), lwd = 0.5, col.lab = 2)  
  legend("topright", c("Aqua", "Terra"),pch = c(24,24), col=c(20,"red"))
  rect(145,-40,155,-25, col=rgb(0,0,1,0.5) , border=F)
  
  
# figure 4: confidence and brightness
  plot(fia$confidence, fia$brightness, pch=20, col = 20, 
       main = "True Fires' Brightness", bty = 'l')
  fia3 <- fia[fia$confidence>75,] 
  points(fia3$confidence, fia3$brightness, pch=20, col="red")
  legend("topleft", c("confidence>75", "others"),pch = c(20,20), col=c("red",20))
  
  # figure 5-8: bright_t31~latitude-linear regression
  dev.new()
  
  fit = lm(bright_t31~latitude+longitude, fia)
  summary(fit)
  par(mfrow = c(2,2),oma=c(0,0,0,0), bg = rgb(134, 150, 167, max = 255),
      col.axis = rgb(193, 203, 215, max = 255), main = "kiss")
  plot(fit, cex = 0.5, fg = rgb(193, 203, 215, max = 255), 
       col = 20, lwd = 2)
  
  
  # figure 3.2: small map
  m = maps::map(regions = "Australia", plot = FALSE)
  maps::map( regions="Australia", fill = TRUE, 
             col = rgb(193, 203, 215, max = 255), bg = rgb(134, 150, 167, max = 255),
             xlim=c(145,155), ylim = c(-40,-25))
  map.grid(m, lim = c(145, 155, -40, -25), col="gray86", 
           nx=20, ny=20, label=FALSE, lty=1, pretty = TRUE)
  map.axes(col = "grey86")
  fia3 <- fia %>%
    mutate(satellite = case_when(
      satellite == "Aqua"                ~ 20,# blue
      satellite == "Terra"~2# red
    ))
  points(fia3$longitude, fia3$latitude, pch=20, cex=c(fia3$brightness/1000), 
         col=c(fia3$satellite), lwd = 0.05, col.lab = 2)  
  
  
  
  
  
  
  
  
  