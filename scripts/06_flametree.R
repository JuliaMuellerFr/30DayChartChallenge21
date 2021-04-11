library(flametree)

dat <- flametree_grow(seed = 8, time = 14)

img_magma <- flametree_plot(tree = dat,
                      background = "black", palette = "viridis::magma")

img_berlin <- flametree_plot(tree = dat,
                      background = "black", palette = "scico::berlin")

img_vik <- flametree_plot(tree = dat,
                             background = "#140d07", palette = "scico::vik")

plot_magma <- plot(img_magma)
plot_berlin <- plot(img_berlin)
plot_vik <- plot(img_vik)

flametree_save(plot_magma, filename = "E:/Github/30DayChartChallenge21/plots/6_ft_magma.png")
flametree_save(plot_berlin, filename = "E:/Github/30DayChartChallenge21/plots/6_ft_berlin.png")
flametree_save(plot_vik, filename = "E:/Github/30DayChartChallenge21/plots/6_ft_vik.png")
