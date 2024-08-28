theme_NAG_publication <- function(base_size=10, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(#plot.title = element_text(#face = "bold",
                                      #size = rel(1.2), 
                                      #hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.grid = element_blank(), # no grid
            panel.border = element_rect(colour = NA),
            #axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(size= base_size,
                                        angle=90,
                                        vjust = 0),
            axis.title.x = element_text(size= base_size,
              vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text=element_text(size=base_size),
            legend.title=element_text(size=base_size),
            #legend.key.size= unit(0.2, "cm"),
            #legend.margin = unit(0, "cm"),
            # legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(
              #face="bold"
              )
    ))
  
}

scale_color_oil_gas <- function(){
  library(scales)
  discrete_scale("color","Publication",manual_pal(values = c("GAS" = "#F5797B", "OIL" = "#01A984")))
}

scale_fill_oil_gas <- function(){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("GAS" = "#F5797B", "OIL" = "#01A984")))
}


scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}