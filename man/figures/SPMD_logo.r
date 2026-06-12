
library(hexSticker)
library(ggplot2)
library(ggforce)
library(sysfonts)

# custom colors
uvic_blue <- "#005493"    #RGB: 0-84-147
uvic_yellow <- "#F5AA1C"    #RGB: 245-170-28
uvic_red <- "#C63527"    #RGB: 198-53-39
uvic_blue_dark <- "#002754"  #RGB: 0-39-84

sysfonts::font_add_google("Roboto Slab", "roboto_slab")

plotdata <- data.frame(x=c(1, 1, 2, 2),
                       xend=c(3.5, 3, 2.6, 4.1),
                       y=c(5, 4, 2.1, 1.1),
                       yend=c(5, 4, 2.1, 1.1),
                       kind=c("1", "2", "2", "1"))

plotdata <- data.frame(x=c(1, 2, 1, 2),
                       xend=c(1.7, 2.7, 1.7, 2.7),
                       y=c(5, 5, 3, 3),
                       yend=c(5, 5, 3, 3),
                       kind=c("1", "2", "2", "1"))

p <- ggplot(plotdata, aes(x=x, y=y)) +
  geom_segment(aes(xend=xend, yend=yend), color=uvic_red, linewidth=1.2) +
  geom_segment(aes(x=xend, xend=xend, y=y-0.4, yend=y+0.4), color=uvic_red,
               linewidth=1.2) +
  geom_point(aes(x=x, y=y, shape=kind), color=uvic_yellow, size=2) +
  theme_void() +
  theme(legend.position="none") +
  scale_shape_manual(values=c(16, 4)) +
  ylim(c(-4, 5.4))
p

# create sticker
s <- hexSticker::sticker(p,
                         package="SPMD",
                         p_size=31,
                         p_x=1,
                         p_y=0.65,
                         p_color=uvic_yellow,
                         s_x=1,
                         s_y=0.9,
                         s_width=1.3,
                         s_height=1.1,
                         filename="logo.png",
                         h_fill=uvic_blue_dark,
                         h_color=uvic_yellow,
                         p_family="roboto_slab",
                         p_fontface="bold")
plot(s)
