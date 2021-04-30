library(tidyverse)
library(ggtext)
mytheme <- theme(
  panel.background = element_rect(fill = "#272822",colour = "#272822"),
  plot.background = element_rect(fill = "#272822",colour = "#272822"),
)
geom_arrow <- function(xstart,xend, y, width = NULL, label = "",direction = "right", colour = "black", fill = "#E16460", inherit.aes = FALSE) {
  xdelta <- abs(xend-xstart)
  if(is.null(width)){
    width <- xdelta/1.618
    message(paste("width:",width))
  }
  
    spanstart <- xdelta/3*2
  
  
  arrowh <- xstart + spanstart
  halfwidth <- width/2
  b <- width/2*3
  arroww <- b/2
  
  dd <- tibble(
    x = c(xstart,xstart, arrowh, arrowh, xend, arrowh,arrowh,  xstart, xstart),
    y = c(y,y-halfwidth, y-halfwidth, y-arroww,y, y+arroww, y+halfwidth, y+halfwidth, y)
  )
  if(direction == "left"){
    mid <- median(c(xstart,xend))
    mid - dd$x + mid  -> dd$x
  }
  
  list(
    geom_polygon(data = dd, aes(x,y), colour = "black", fill = fill,inherit.aes = inherit.aes),
    geom_richtext(x = xstart+xdelta/2, y = y, hjust = 0.5, label = label, fill = NA, label.color = NA, text.colour = "white")
    )

  }

tribble(
  ~label, ~x, ~y,
  "working<br>directory", 1, 5,
  "staging<br>area", 2, 5,
  "local repo", 3, 5,
  "remote<br>repo", 4, 5
) %>%
  ggplot(aes(x,y,label = label)) +
  geom_segment(aes(x = x,y = 0, xend = x, yend = y))+
  geom_arrow(1,2,3.5,label = "git add") +
  geom_arrow(2,3,2.5,label = "git commit") +
  geom_arrow(3,4,1.5,label = "git push") +
  geom_arrow(3,4,0.5,direction = "left", label = "git pull") +
  
  geom_textbox(fill = "lightgrey", 
               width = unit(2, "cm"),
               height = unit(1, "cm"),
               halign = 0.5,
               valign = 0.5
               ) +
  theme_void() +
  # lims(x = c(0, 6), y = c(0, 6)) +
  mytheme
  