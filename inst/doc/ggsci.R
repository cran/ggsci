## ------------------------------------------------------------------------
library("ggsci")
library("ggplot2")
library("gridExtra")

data("diamonds")

p1 = ggplot(subset(diamonds, carat >= 2.2),
       aes(x = table, y = price, colour = cut)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.05, size = 1, span = 1) +
  theme_bw()

p2 = ggplot(subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
       aes(x = depth, fill = cut)) +
  geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
  theme_bw()

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_npg = p1 + scale_color_npg()
p2_npg = p2 + scale_fill_npg()
grid.arrange(p1_npg, p2_npg, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_aaas = p1 + scale_color_aaas()
p2_aaas = p2 + scale_fill_aaas()
grid.arrange(p1_aaas, p2_aaas, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_nejm = p1 + scale_color_nejm()
p2_nejm = p2 + scale_fill_nejm()
grid.arrange(p1_nejm, p2_nejm, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_lancet = p1 + scale_color_lancet()
p2_lancet = p2 + scale_fill_lancet()
grid.arrange(p1_lancet, p2_lancet, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_jco = p1 + scale_color_jco()
p2_jco = p2 + scale_fill_jco()
grid.arrange(p1_jco, p2_jco, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_ucscgb = p1 + scale_color_ucscgb()
p2_ucscgb = p2 + scale_fill_ucscgb()
grid.arrange(p1_ucscgb, p2_ucscgb, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_d3 = p1 + scale_color_d3()
p2_d3 = p2 + scale_fill_d3()
grid.arrange(p1_d3, p2_d3, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_locuszoom = p1 + scale_color_locuszoom()
p2_locuszoom = p2 + scale_fill_locuszoom()
grid.arrange(p1_locuszoom, p2_locuszoom, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_igv_default = p1 + scale_color_igv()
p2_igv_default = p2 + scale_fill_igv()
grid.arrange(p1_igv_default, p2_igv_default, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_uchicago = p1 + scale_color_uchicago()
p2_uchicago = p2 + scale_fill_uchicago()
grid.arrange(p1_uchicago, p2_uchicago, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_startrek = p1 + scale_color_startrek()
p2_startrek = p2 + scale_fill_startrek()
grid.arrange(p1_startrek, p2_startrek, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_futurama = p1 + scale_color_futurama()
p2_futurama = p2 + scale_fill_futurama()
grid.arrange(p1_futurama, p2_futurama, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_rickandmorty = p1 + scale_color_rickandmorty()
p2_rickandmorty = p2 + scale_fill_rickandmorty()
grid.arrange(p1_rickandmorty, p2_rickandmorty, ncol = 2)

## ---- fig.width = 10.67, fig.height = 4, out.width = 800, out.height = 300, dpi = 150----
p1_simpsons = p1 + scale_color_simpsons()
p2_simpsons = p2 + scale_fill_simpsons()
grid.arrange(p1_simpsons, p2_simpsons, ncol = 2)

## ------------------------------------------------------------------------
library("reshape2")

data("mtcars")
cor = cor(unname(cbind(mtcars, mtcars, mtcars, mtcars)))
cor_melt = melt(cor)

p3 = ggplot(cor_melt,
            aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "black", size = 0.3) +
  theme_bw()

## ---- fig.width = 8, fig.height = 6.67, out.width = 600, out.height = 500, dpi = 150----
p3_gsea = p3 + scale_fill_gsea()
p3_gsea

## ---- fig.width = 6.67, fig.height = 6.67, out.width = 500, out.height = 500, dpi = 150----
mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal

library("scales")
show_col(mypal)

