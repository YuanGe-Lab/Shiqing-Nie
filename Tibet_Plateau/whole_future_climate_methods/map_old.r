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


dir.create("map")

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

label_format <- number_format(accuracy = 0.01, drop_trailing_zeroes = FALSE)
map_result <- list()
for (i1 in names(future_index)) {
  map_result[[i1]] <- list()
  if (i1 == "Current"){
    data1 <- future_index[[i1]]
  }else{
    data1 <- future_index[[i1]] - now
    #data1[data1 > 0] <- "Increased"
    #data1[data1 < 0] <- "Decreased"
  }
  dir.create(paste0("map/",i1))
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
    if (i1 == "Current"){
      #raster
      point_v <- rasterize(as.matrix(data3[,1:2]), r1, value = data3$value)
      colnames(data3)[1:2] <- c("x","y") 
      gs <- gstat(formula=value~1, locations=~x+y, data=data3)
      point_v_idw <- interpolate(point_v, gs, debug.level=0)
      #plot(point_v_idw)
      point_f_idw <- mask(point_v_idw, map_data)
      map_result[[i1]][[j1]] <- point_f_idw
      #plot(point_f_idw, 1)
      #raster end
      # p2 <- p1 + geom_point(data = data3,aes(x = lon, y = lat, color = value),
      #                       alpha = 1,size = 0.1) +
      #   scale_color_gradientn(colours = c("#0066FF","#00FFFF",  
      #                                     "#33FF00","#FF9900",
      #                                     "#FF0000","#CC00FF","#990099")) +
      #   labs(x = "", y = "", color = j1 ) +
      #   theme(axis.text = element_blank(),
      #         axis.ticks = element_blank(),
      #         panel.grid = element_blank(),
      #         panel.background = element_rect(fill = 'transparent'))
      p2 <- ggplot() +
        geom_spatraster(data = point_f_idw$var1.pred) +
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
    }else{
      data3 <- data3[data3$value != "0",]
      #raster
      # point_v <- rasterize(as.matrix(data3[,1:2]), r1, value = data3$value)
      # chara1 <- unique(data3$value)
      # order1 <- order(chara1)
      # order1 <- order1 - 1
      # temp1 <- as.data.frame(values(point_v$values))
      # temp1$change <- temp1$values
      # for (i2 in 1:nrow(temp1)) {
      #   if (!is.na(temp1$change[i2])){
      #     temp2 <- temp1$change[i2]
      #     temp1$change[i2] <- chara1[temp1$change[i2] == order1]
      #   }
      # }
      # point_v$fi <- temp1$change
      # plot(point_v,"fi")
      # point_v <- point_v$fi
      
      #polygon interpoltion
      #vector
      point_v <- vect(data3,geom= c("lon","lat"),
                    crs = crs)
      point_v_polygon <- voronoi(point_v)
      point_f_polygon <- crop(point_v_polygon, map_data)
      map_result[[i1]][[j1]] <- point_f_polygon
      #plot(point_f_polygon, "value")
      #raster end
      
      #point
      # p2 <- p1 + geom_point(data = data3,aes(x = lon, y = lat, color = value),
      #                       alpha = 1,size = 0.1) +
      #   scale_color_manual(limits = c("Decreased","Increased"),values  = c("#0066FF","#FF0000"),na.value = "white") +
      #   labs(x = "", y = "", color = j1 ) +
      #   guides(color=guide_legend(override.aes = list(size=3))) +
      #   theme(axis.text = element_blank(),
      #         axis.ticks = element_blank(),
      #         panel.grid = element_blank(),
      #         panel.background = element_rect(fill = 'transparent'))
      
      #raster
      # p2 <- ggplot() +
      #   geom_spatraster(data = point_v) +
      #   scale_fill_manual(limits = c("Decreased","Increased"),values  = c("#0066FF","#FF0000"),na.value = "white") +
      #   labs(x = "", y = "", fill = j1 ) +
      #   geom_sf(data = map_data, fill = NA) +
      #   #guides(color=guide_legend(override.aes = list(size=3))) +
      #   theme(axis.text = element_blank(),
      #         axis.ticks = element_blank(),
      #         panel.grid = element_blank(),
      #         panel.background = element_rect(fill = 'transparent'))
      
      #interpolation
      p2 <- ggplot() +
        geom_spatvector(data = point_f_polygon, aes(fill = value)) +
        scale_fill_manual(limits = c("Decreased","Increased"),values  = c("#0066FF","#FF0000")) +
        labs(x = "", y = "", fill = j1 ) +
        geom_sf(data = map_data, fill = NA) +
        #guides(color=guide_legend(override.aes = list(size=3))) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = 'transparent'))
    }
    p2
    ggsave(p2, file=paste0("map/",i1,"/",j1,".png"), width=6, height=4,dpi = 600)
    ggsave(p2, file=paste0("map/",i1,"/",j1,".pdf"), width=6, height=4)
  }
}

save(map_result,file = "map_result.Rdata")
