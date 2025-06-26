
source("./dev/useful_functions.R")
use_packages("ggplot2")



#blue<-"#82A9D0"
#lightblue<-"#BACFE4"
#darkblue<-"#6D8CA0"
#darkgreen<-"#336633"
#lightgreen<-"#99CC99"
#lightred<-"#CC0000"
#red <-"#990000"
#darkred<-"#660000"

lightred<-"#FF6B54"
red <-"#C60B04"
darkred<-"#9D2318"
redBlack<-"#280503"
lightgreen<-"#D8F3CF"
green <-"#ADC7A6"
darkgreen<-"#778B71"
greenBlack<-"#283A10"
blue<-"#4878A8"
lightblue<-"#BACFE4"
darkblue<-"#142A69"

cbBlack<-"#000000"
cbGrey<-"#999999"
cbOrange<-"#E69F00"
cbBlue<-"#56B4E9"
cbGreen<-"#009E73"
cbYellow<-"#F0E442"
cbBlueDark<-"#0072B2"
cbOrangeDark<-"#D55E00"
cbPink<-"#CC79A7"
cbRed<-"#DF4738"



greenPalette<-c("#ECFFE6", "#D8F3CF",  "#ADC7A6", "#8EA886", "#778B71")
bluePalette<-c("#C8D9E9",  "#6090C0", "#4878A8", "#184890", "#142A69")
redPalette<-c("#FAC6AE",  "#FF6B54", "#C60B04", "#9D2318", "#280503")
YGBpalette<-c("#edf8b1", "#7fcdbb", "#2c7fb8")
cbPalette<-c(cbBlack, cbGrey, cbOrange, cbBlue, cbGreen, cbYellow, cbBlueDark, cbOrangeDark, cbPink)

cbFill<-c(cbBlueDark, cbOrangeDark, cbGreen, cbYellow, cbBlue, cbOrange)


andy.colour <- scale_colour_manual(values = c(red, darkblue))
red.blue <- scale_colour_manual(values = c(lightred, blue))
red.green <- scale_colour_manual(values = c(lightred, green))
andy.YGB <- scale_colour_manual(values = YGBpalette)
andy.RGB <- scale_colour_manual(values = c("#e31a1c","#33a02c","#1f78b4"))
andy.cb <- scale_colour_manual(values = cbPalette)

andy.fill <- scale_fill_manual(values = c(red, darkblue))
red.blue.fill <- scale_fill_manual(values = c(lightred, blue))
red.green.fill <- scale_fill_manual(values = c(lightred, green))
andy.YGB.fill<-scale_fill_manual(values = YGBpalette)
andy.cb.fill <- scale_fill_manual(values = cbPalette)





green.colour<-scale_colour_manual(values = greenPalette)
green.fill<-scale_fill_manual(values = greenPalette)
blue.colour<-scale_colour_manual(values = bluePalette)
blue.fill<-scale_fill_manual(values = bluePalette)
red.colour<-scale_colour_manual(values = redPalette)
red.colour3<-scale_colour_manual(values = c("#FF6B54", "#C60B04", "#9D2318"))

red.fill<-scale_fill_manual(values = redPalette)




andy.theme <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.title          = element_text(size = rel(1.2)),
      axis.title.y        = element_text(margin = margin(0, 10, 0, 0), angle = 90),
      axis.title.x        = element_text(margin = margin(10, 0, 0, 0)),
      axis.text           = element_text(size = rel(0.75), colour = "grey35"),
      axis.text.x         = element_text(margin = margin(2, 0, 0, 0)),
      axis.text.y         = element_text(margin = margin(0, 2, 0, 0)),
      axis.ticks        = element_line(colour = "grey35"),
      legend.key        = element_rect(colour = "white"),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      strip.background  = element_rect(fill = "grey80", colour = "grey50"),
      strip.text  = element_text(size = rel(1.4))
    )
}


andy.dsus <- function(base_size = 12, base_family = "") {
  andy.theme(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.title          = element_text(size = rel(1.6)),
      axis.text           = element_text(size = rel(1.3), colour = "grey35"),
      strip.background = element_rect(fill = cbBlueDark, colour = "white"),
      strip.text  = element_text(size = rel(1.3), colour = "white")
    )
}

andy.dsus.trans <- function(base_size = 12, base_family = "") {
  andy.dsus(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_rect(fill = "transparent", colour = NA),
      panel.background  = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = cbBlueDark, colour = "white"),
      strip.text  = element_text(size = rel(1.3), colour = "white")
    )
}




andy.blue <-function(base_size = 12, base_family = "") {
  andy.theme(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_rect(fill = lightblue, colour = lightblue),
      strip.text  = element_text(colour = darkblue)
    )
}

andy.minimal <-function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.text = element_text(colour = darkblue),
      strip.text  = element_text(size = rel(1.4)),
      axis.title          = element_text(size = rel(1.2)),
      axis.title.y        = element_text(margin = margin(0, 10, 0, 0), angle = 90),
      axis.title.x        = element_text(margin = margin(10, 0, 0, 0)),
      axis.text           = element_text(size = rel(0.75), colour = "grey35"),
      axis.text.x         = element_text(margin = margin(2, 0, 0, 0)),
      axis.text.y         = element_text(margin = margin(0, 2, 0, 0))
    )
}


andy.cbblue <-function(base_size = 12, base_family = "") {
  andy.theme(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_rect(fill = cbBlueDark, colour = "white"),
      strip.text  = element_text(colour = "white")
    )
}





