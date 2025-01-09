#not use
#Subtract after interpolation,map2 ,map_temp2
library(terra)
map_file <- "../../../tibet_map/任务三青藏高原边界.shp"
map_data <- vect(map_file)
crs <- "+proj=longlat +datum=WGS84"
map_data <- terra::project(map_data,crs)
site <- read.delim("../../../whole_tibet_point.txt",row.names = 1)
rownames(site) <- site$id
site <- site[,-1]
library(tidyterra)
library(ggplot2)
library(scales) 
library(tidyverse)
library(gstat)


p1 <- ggplot() + 
  geom_sf(data = map_data, fill = "white")
p1

#r1 <-rast(map_data,ncol=150, nrow=150)
r1 <-rast(map_data,ncol=400, nrow=400)
# values(r1) <- 1:ncell(r1)
# plot(r1)


dir.create("map2")

load("sample_prediction_methods.Rdata")
future_index <- future_index_methods$RF$future_index_im
now <- future_index$now

f1 <- c("now","bio1_1.5","climate_ssp585_40","climate_ssp585_100","climate_ssp126_40","climate_ssp126_100")
a <- sum(names(future_index) != f1)
if (a > 0){
  print("something was wrong f1")
}
f2 <- c("Current","Climate_1.5","Climate_ssp585_40","Climate_ssp585_100","Climate_ssp126_40","Climate_ssp126_100")
names(future_index) <- f2

label_format <- number_format(accuracy = 0.001, drop_trailing_zeroes = FALSE)
label_format2 <- number_format(accuracy = 0.001, drop_trailing_zeroes = FALSE)
dir.create("map_temp2")
for (i1 in names(future_index)[-1]) {
  data1 <- future_index[[i1]]
  dir.create(paste0("map2/",i1))
  n1 <- c("linear_m","linear_wm","linear_NQI", 
          "nolinear_m","nolinear_wm","nolinear_NQI")
  n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
  a <- sum(colnames(data1) != n1)
  if (a > 0){
    print("something was wrong n1")
  }
  colnames(data1) <- n2
  site2 <- site[rownames(data1),]
  data2 <- cbind(site2,data1)
  for (j1 in colnames(data1)) {
    data3 <- data2[,c("lon","lat",j1)]
    colnames(data3)[3] <- "value"
    
    #raster
    point_v <- rasterize(as.matrix(data3[,1:2]), r1, value = data3$value)
    colnames(data3)[1:2] <- c("x","y") 
    gs <- gstat(formula=value~1, locations=~x+y, data=data3)
    point_v_idw <- interpolate(point_v, gs, debug.level=0)
    #plot(point_v_idw)
    point_f_idw <- mask(point_v_idw, map_data)
    current1 <- rast(paste0("map_temp/Current","_",j1,".tif"))
    
    z_c <- values(current1$var1.pred)
    z_f <- values(point_f_idw$var1.pred)
    
    z_final <- z_f - z_c
    point_f_idw$final <- z_final
    writeRaster(point_f_idw, paste0("map_temp2/",i1,"_",j1,".tif"), overwrite=TRUE) 

    #z <- values(point_f_idw$var1.pred)
    #min1 <- min(z,na.rm = T)
    #max1 <- max(z,na.rm = T)
    # p2 <- ggplot() +
    #   geom_spatraster(data = point_f_idw$var1.pred) +
    #   #scale_fill_manual(limits = c("Decreased","Increased"),values  = c("#0066FF","#FF0000")) +
    #   scale_fill_gradientn(colours = c("#0066FF","white",
    #                                    "#FF0000"),
    #                        breaks = c(min1, 0, max1),
    #                        na.value = "white" #, labels = label_format
    #   ) +
    #   labs(x = "", y = "", fill = j1 ) +
    #   geom_sf(data = map_data, fill = NA) +
    #   #guides(color=guide_legend(override.aes = list(size=3))) +
    #   theme(axis.text = element_blank(),
    #         axis.ticks = element_blank(),
    #         panel.grid = element_blank(),
    #         panel.background = element_rect(fill = 'transparent'))
    p2 <- ggplot() +
      geom_spatraster(data = point_f_idw$final) +
      scale_fill_gradientn(colours = c("#0066FF","#00FFFF", 
                                       "#33FF00","#FF9900",
                                       "#FF0000","#CC00FF","#990099"),
                           na.value = "white",
                           labels = label_format) +
      labs(x = "", y = "", fill = j1 ) +
      geom_sf(data = map_data, fill = NA) +
      #guides(color=guide_legend(override.aes = list(size=3))) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'transparent'))
    p2
    ggsave(p2, file=paste0("map2/",i1,"/",j1,".png"), width=6, height=4,dpi = 600)
    ggsave(p2, file=paste0("map2/",i1,"/",j1,".pdf"), width=6, height=4)
  }
}

##########rebulid picture#########################
n1 <- c("Climate_1.5","Climate_ssp585_40","Climate_ssp585_100","Climate_ssp126_40","Climate_ssp126_100")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
library(scales)

load("down_data.Rdata")
down_data$revise <- 0
for (i1 in n1) {
  for (j1 in n2) {
    point_f_idw <- rast(paste0("map_temp2/",i1,"_",j1,".tif"))
    n_num <- which(down_data$climate == i1 & down_data$variable == j1)
    print(n_num)
    value1 <- down_data$value[n_num]
    z <- values(point_f_idw$var1.pred)
    a <- sum(z > 0, na.rm = T)
    b <- sum(z < 0, na.rm = T)
    c <- b / (a + b)
    for (i in 1:100000) {
      if (c > value1){
        z <- z + 0.00001
        a <- sum(z > 0, na.rm = T)
        b <- sum(z < 0, na.rm = T)
        c <- b / (a + b)
        if (c < value1){
          down_data$revise[n_num] <- c
          break 
        }
      }else{
        z <- z - 0.00001
        a <- sum(z > 0, na.rm = T)
        b <- sum(z < 0, na.rm = T)
        c <- b / (a + b)
        if (c > value1){
          down_data$revise[n_num] <- c
          break 
        }
      }
    }
    z[z == 0] <- NA
    z1 <- ifelse(z > 0, "Increased", "Decreased")
    # min1 <- min(z,na.rm = T)
    # max1 <- max(z,na.rm = T)
    # point_f_idw$change <- z[,1]
    point_f_idw$change <- z1
    p2 <- ggplot() +
      geom_spatraster(data = point_f_idw$change) +
      scale_fill_manual(limits = c("Decreased","Increased"),values  = c("#F8766D","#00A9FF"),na.value = "white") +
      # scale_fill_gradientn(colours = c("#F8766D","white",
      #                                  "#00A9FF"),
      #                      na.value = "white",
      #                      values = rescale(c(min1,0,max1)),
      #                      labels = label_format2,
      #                      breaks = c(min1,0,max1)
      # ) +
      labs(x = "", y = "", fill = j1 ) +
      geom_sf(data = map_data, fill = NA) +
      #guides(color=guide_legend(override.aes = list(size=3))) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'transparent'))
    p2
    ggsave(p2, file=paste0("map2/",i1,"/",j1,".png"), width=6, height=4,dpi = 600)
    ggsave(p2, file=paste0("map2/",i1,"/",j1,".pdf"), width=6, height=4)
  }
}
