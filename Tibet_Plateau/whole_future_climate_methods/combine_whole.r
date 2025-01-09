library(terra)
library(tidyterra)
library(ggplot2)
library(scales) 
library(tidyverse)
library(gstat)

n1 <- c("Current","Climate_1.5","Climate_ssp585_40","Climate_ssp585_100","Climate_ssp126_40","Climate_ssp126_100")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
n3 <- c("Current","simple_1.5","ssp585_40","ssp585_100","ssp126_40","ssp126_100")

label_format <- number_format(accuracy = 0.01, drop_trailing_zeroes = FALSE)
label_format2 <- number_format(accuracy = 0.001, drop_trailing_zeroes = FALSE)

map_file <- "../../../tibet_map/任务三青藏高原边界.shp"
map_data <- vect(map_file)
crs <- "+proj=longlat +datum=WGS84"
map_data <- terra::project(map_data,crs)
picture_future <- list()
for (i1 in n1) {
  id1 <- n3[n1 == i1]
  for (j1 in n2) {
    point_f_idw <- rast(paste0("final_future_temp/",i1,"_",j1,".tif"))
    if (i1 == "Current"){
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
      z <- values(point_f_idw$change)
      #z1 <- ifelse(z > 0, "Increased", "Decreased") 
      min1 <- min(z,na.rm = T)
      max1 <- max(z,na.rm = T)
      p2 <- ggplot() +
        geom_spatraster(data = point_f_idw$change) +
        #scale_fill_manual(limits = c("Decreased","Increased"),values  = c("#F8766D","#00A9FF"),na.value = "white") +
        scale_fill_gradientn(colours = c("#F8766D","white",
                                         "#00A9FF"),
                             na.value = "white",
                             values = rescale(c(min1,0,max1)),
                             labels = label_format2,
                             breaks = c(min1,0,max1)
        ) +
        labs(x = "", y = "", fill = id1 ) +
        theme(legend.title = element_text(margin = margin(t = 15, b = 15))) +
        geom_sf(data = map_data, fill = NA) +
        #guides(color=guide_legend(override.aes = list(size=3))) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = 'transparent'))
    }
    p2
    picture_future[[i1]][[j1]] <- p2
  }
}

p1 <- picture_future$Current$`NL-M`
library(ggrastr)
library(patchwork)
load("picture_whole_cor.Rdata")
whole_cor
# layout <- c(
#   area(1,1,1,2),
#   area(1,3,1,3)
# )
# plot(layout)
# p2 <- p1 + whole_cor + plot_layout(design = layout)
# p2
ggsave(p1,file = "current_cor1.pdf",width = 6,height = 4)



p3 <- picture_future$Current$`L-M` + picture_future$Current$`L-WM` + picture_future$Current$`L-NQI` + picture_future$Current$`NL-WM` + picture_future$Current$`NL-NQI` + plot_layout(ncol = 2) +  plot_annotation(tag_levels = "a")
p3
ggsave(p3,file = "current_others.pdf",width = 12,height = 12)

p32 <- picture_future$Current$`L-M` + picture_future$Current$`L-WM` + picture_future$Current$`L-NQI` + picture_future$Current$`NL-W` + picture_future$Current$`NL-WM` + picture_future$Current$`NL-NQI` + plot_layout(ncol = 2) +  plot_annotation(tag_levels = "a")
p32
ggsave(p32,file = "current_six.pdf",width = 12,height = 12)
ggsave(p32,file = "current_six.png",width = 12,height = 12)

load("picture_down.Rdata")
p_down
load("/mnt/data/tibet/dada2/core_asv/heathy_index/asv/core_climate_0.3_add_others/whole_prediction/climate1_mean.Rdata")
climate1_mean <- climate1_mean[-1,]
climate1_mean$x <- c("Climate_1.5","Climate_ssp585_40","Climate_ssp585_100","Climate_ssp126_40","Climate_ssp126_100")
library(ggside)
data1 <- p_down$data

z <- data1 %>% 
  ggplot(aes(x = climate, y = value)) +
  geom_point(aes(color = variable)) +
  geom_line(aes(color = variable,group = variable),linetype = 2) + 
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "red") +
  scale_color_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                     values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  scale_x_discrete(labels = c("simple_1.5","ssp126_40","ssp585_40","ssp126_100","ssp585_100")) +
  labs(x = "Future climate", y= "Decreased propotion", color = "SHI") +
  #facet_wrap(~methods, scales = "fixed") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
z1 <- z + annotate("text",x= "Climate_ssp126_40", y=0.85,label="Mean temprature",hjust=-.3,vjust=0) +#
  geom_xsidecol(data = climate1_mean,aes(x = x ,y = mean),fill = "red") + scale_xsidey_continuous(#expand = expansion(mult = c(0,1)),  #空白空间占比
  #labels = scales::percent,   #百分比显示
  #limits = c(0,0.25/100),   
  breaks = seq(0,6,by=3)) #+ scale_fill_gradient(low = "green", high = "red")
ggsave(z1,file = "future_nl_m_2.pdf",width = 5,height = 3)
id1 <- "NL-M"
p4 <- picture_future$Climate_1.5[[id1]] + picture_future$Climate_ssp126_40[[id1]] + picture_future$Climate_ssp585_40[[id1]] + picture_future$Climate_ssp126_100[[id1]] + picture_future$Climate_ssp585_100[[id1]] + plot_layout(ncol = 2)
p4
ggsave(p4,file = "future_nl_m.pdf",width = 12,height = 12)


n1 <- names(picture_future$Climate_1.5)
n1 <- n1[n1 != "NL-M"]
for (id1 in n1) {
  future_nl <- picture_future$Climate_1.5[[id1]] + picture_future$Climate_ssp126_40[[id1]] + picture_future$Climate_ssp585_40[[id1]] + picture_future$Climate_ssp126_100[[id1]] + picture_future$Climate_ssp585_100[[id1]] + plot_layout(ncol = 2) + plot_annotation(tag_levels = "a") & theme(legend.title = element_text(size = 15),legend.text = element_text(size = 13),plot.tag = element_text(size = 32, face = "bold"))
  future_nl
  ggsave(future_nl,file = paste0("future_",id1,".pdf"),width = 12,height = 12)
}
