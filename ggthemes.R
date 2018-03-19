theme_base <- function(base_size = 12) {
  structure(list(
    axis.line =         element_blank(),
    axis.text.x =       element_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       element_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        element_line(colour = NA, size = 0.2),
    axis.title.x =      element_text(colour = NA,size = base_size, vjust = 1),
    axis.title.y =      element_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
 
    legend.background = element_rect(colour=NA), 
    legend.key =        element_rect(colour = NA, ),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       element_text(colour = NA,size = base_size * 0.8),
    legend.title =      element_text(colour = NA,size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",
 
    panel.background =  element_rect(fill = NA, colour = NA), 
    panel.border =      element_rect(fill = NA, colour=NA), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = NA, size = 0.5),
    panel.margin =      unit(0.25, "lines"),
 
    strip.background =  element_rect(fill = NA, colour = NA), 
    strip.text.x =      element_text(colour = NA,size = base_size * 0.8),
    strip.text.y =      element_text(colour = NA,size = base_size * 0.8, angle = -90),
 
    plot.background =   element_rect(colour = NA),
    plot.title =        element_text(colour = NA,size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}


theme_noGrid <- function (base_size = 12, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(
                axis.line = element_line(colour = "grey60"),     
                axis.text = element_text(size = rel(0.8)), 
                axis.ticks = element_line(colour = "black"), 
                legend.key = element_rect(colour = "grey80"), 
                panel.background = element_rect(fill = "white", colour = NA), 
                panel.border = element_rect(fill = NA,colour = NA),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),                
                panel.grid.minor = element_blank(), 
                strip.background = element_rect(fill = "white", colour = "grey50", size = 0.5)
            )
}


theme_grid_y <- function (base_size = 12, base_family = "") 
{
    theme_noGrid(base_size = base_size, base_family = base_family) %+replace% 
        theme(
                panel.grid.major.y = element_line(colour = "grey78",size = 0.3),
                panel.grid.minor.y = element_line(colour = "grey88",size = 0.25),
            )
}

theme_grid_x <- function (base_size = 12, base_family = "") 
{
    theme_noGrid(base_size = base_size, base_family = base_family) %+replace% 
        theme(
                panel.grid.major.x = element_line(colour = "grey78",size = 0.3),
                panel.grid.minor.x = element_line(colour = "grey88",size = 0.25),
            )
}


theme_fullBlank <- function (base_size = 12, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(
                axis.line = element_blank(),        
                axis.text = element_blank(), 
                axis.ticks = element_blank(), 
                axis.title = element_blank(),               
                legend.key = element_rect(colour = "grey80"), 
                panel.background = element_rect(fill = "white", colour = NA), 
                panel.border = element_rect(fill = NA,colour = NA),
                panel.grid.major = element_blank(),                  
                panel.grid.minor = element_blank(), 
                strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2)
            )
}


