# JASP cols
grn <- "#A7D75D"
grn_dk <- "#8CC63E"
blu <- "#50B0E3"
blu_dk <- "#29A1E3"

# old cols
grn <- "#16a085"
grn_dk <- "#1b7742"
blu <- "#5c97bf"
blu_dk <- "#34415e"
mag <- "#b381b3"
mag_dk <- "#886288"
gry <- "#939393"
gry_dk <- "#545454"
ylw <- "#aa8f00"
ylw_dk <- "#634806"
ong <- "#d47500"
ong_dk <- "#d35400"
red <- "#ef4836"
red_dk <- "#b50000"


gn_rd_1 <- "#16a085"
gn_rd_2 <- "#70b8a4"
gn_rd_3 <-"#accfc4"
gn_rd_4 <-"#e5e5e5"
gn_rd_5 <-"#f4b5a8"
gn_rd_6 <-"#f6836e"
gn_rd_7 <-"#ef4836"

bl_ong_1 <- "#346888"
bl_ong_2 <- "#94aabb"
bl_ong_3 <- "#f1f1f1"
bl_ong_4 <- "#ffcb8a"
bl_ong_4 <- "#ffa600"

dust_bwn <- "#555555"
dust_red <-"#db735c"
dust_salmon <- "#EFA86E"
dust_bwn_dk <- "#9A8A76"
dust_beige <- "#F3C57B"
dust_bwn_vdk <- "#7A6752"
dust_vinegar <-"#2A91A2"
dust_lime <-"#87F28A"
dust_blu <- "#6EDCEF"

flat_gry <- "#34495e"
flat_blu <- "#3498db"
flat_grn <- "#2ecc71"
flat_yel <- "#f1c40f"
flat_pnk <- "#e74c3c"
flat_purp <- "#9b59b6"
flat_grn2 <- "#1abc9c"
flat_ong <- "#f39c12"
flat_ong_dk <- "#d35400"

fresh_gry <- "#111111"
fresh_blu <- "#65ADC2"
fresh_gry2 <- "#233B43"
fresh_red <- "#E84646"
fresh_bwn <- "#C29365"
fresh_blk <- "#362C21"
fresh_vin_dk <- "#316675"
fresh_grn_dk <- "#168E7F"
fresh_grn <- "#109B37"



theme_trans <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_rect(fill = "transparent", colour = NA),
      panel.background  = element_rect(fill = "transparent", colour = NA),
      panel.grid = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = blu_dk, colour = "white"),
      strip.text  = element_text(size = rel(1.3), colour = "white")
    )
}


point_size = 6
bar_width = 0.6
line_size = 1
axis_rel = 1.3
title_rel = 1.6
