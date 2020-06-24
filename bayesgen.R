# author: Danielle Navarro
# date: 2020-06-23

# packages
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggforce)
set.seed(1)

n <- 50000 # number of consequential regions to sample
r <- 7.5   # range parameter for the plots


# shepard model -----------------------------------------------------------


# construct posterior hypothesis space
hypotheses <- tibble(
  mid_x = runif(n, min = -r, max = r), # prior location parameters are treated as arbitrary
  mid_y = runif(n, min = -r, max = r),
  len_x = rgamma(n, rate = 1.2, shape = 1), # prior size is gamma/erlang per Shepard
  len_y = rgamma(n, rate = 0.5, shape = 1)
) %>%
  mutate( # reparameterize as the edges of the regions
    x_min = mid_x - len_x / 2, 
    x_max = mid_x + len_x / 2,
    y_min = mid_y - len_y / 2,
    y_max = mid_y + len_y / 2
  ) %>%
  filter( # under weak sampling, Bayesian updating is simply falsification/filtering
    x_min < 0, x_max > 0, 
    y_min < 0, y_max > 0
  ) %>%
  filter( # for visual nicety, use the Navarro et al 2012 "bounded" model
    x_min > -r, x_max < r,
    y_min > -r, y_max < r
  )


# convenience function to compute the generalization probability given a 
# stimulus locations and vectors 
within_prob <- function(x, xmin, xmax) {
  within <- function(x) {
    mean(x < xmax & x > xmin)
  }
  unlist(lapply(x, within))
}

# construct generalization gradients
generalise <- tibble(
  x = seq(-r, r, length.out = 1000),
  y = seq(-r, r, length.out = 1000),
  px = within_prob(x, hypotheses$x_min, hypotheses$x_max),
  py = within_prob(y, hypotheses$y_min, hypotheses$y_max)
)


# draw plot ---------------------------------------------------------------


# the central plot showing the regions...
pic <- ggplot(
  data = hypotheses, 
  mapping = aes(xmin = x_min, ymin = y_min, 
                xmax = x_max, ymax = y_max)
) + 
  geom_rect(alpha = .15, color = "white", size = .1) + 
  
  # arrows and text
  annotate("point", x = 0, y = 0, shape = 19, size = 2) + 
  annotate(
    geom = "curve", x = 2.2, y = 6, xend = 0, yend = .6, 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 2.7, y = 6, 
           label = "Known\nConsequential\nStimulus", hjust = "left")  +
  annotate(
    geom = "curve", x = -5.8, y = -6.5, xend = -4.2, yend = 0, 
    curvature = -.2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -7, y = -7.2, 
           label = "Possible Consequential Region", hjust = "left")  +
  
  # stylistic
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  scale_x_continuous("Stimulus Dimension 1", labels = NULL, limits = c(-r, r)) + 
  scale_y_continuous("Stimulus Dimension 2", labels = NULL, limits = c(-r, r))


# the generalisation curve above
above <- ggplot(
  data = generalise,
  mapping = aes(x, px)
) + 
  geom_area() + 
  scale_x_continuous(NULL, labels = NULL, limits = c(-r, r)) +
  scale_y_continuous("Generalization", limits = c(0, 1), labels = NULL) +
  theme_bw()

# the generalisation curve to the right
right <- ggplot(
  data = generalise,
  mapping = aes(py, y)
) + 
  geom_area(orientation = "y") + 
  scale_y_continuous(NULL, labels = NULL, limits = c(-r, r)) +
  scale_x_continuous("Generalization", limits = c(0, 1), labels = NULL) +
  theme_bw()

# spacer plot
blank <- ggplot(data = generalise) + theme_void()

# compose plots using patchwork
figure <- above + blank + pic + right +
  plot_layout(
    heights = c(1, 2), 
    widths = c(2, 1)
  )

# save to file
ggsave(
  filename = here::here("shepardsim.png"), 
  plot = figure, 
  height = 6,
  width = 6
)



# session information -----------------------------------------------------

# devtools::session_info()

# ─ Session info ───────────────────────────────────────────────────────────────────────────
# setting  value                       
# version  R version 4.0.1 (2020-06-06)
# os       Ubuntu 20.04 LTS            
# system   x86_64, linux-gnu           
# ui       RStudio                     
# language en_AU:en                    
# collate  en_AU.UTF-8                 
# ctype    en_AU.UTF-8                 
# tz       Australia/Sydney            
# date     2020-06-23                  
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────
# package     * version  date       lib source        
# assertthat    0.2.1    2019-03-21 [1] CRAN (R 4.0.0)
# backports     1.1.7    2020-05-13 [1] CRAN (R 4.0.0)
# callr         3.4.3    2020-03-28 [1] CRAN (R 4.0.0)
# cli           2.0.2    2020-02-28 [1] CRAN (R 4.0.0)
# colorspace    1.4-1    2019-03-18 [1] CRAN (R 4.0.0)
# crayon        1.3.4    2017-09-16 [1] CRAN (R 4.0.0)
# desc          1.2.0    2018-05-01 [1] CRAN (R 4.0.0)
# devtools    * 2.3.0    2020-04-10 [1] CRAN (R 4.0.0)
# digest        0.6.25   2020-02-23 [1] CRAN (R 4.0.0)
# dplyr       * 0.8.5    2020-03-07 [1] CRAN (R 4.0.0)
# ellipsis      0.3.1    2020-05-15 [1] CRAN (R 4.0.0)
# evaluate      0.14     2019-05-28 [1] CRAN (R 4.0.0)
# fansi         0.4.1    2020-01-08 [1] CRAN (R 4.0.0)
# farver        2.0.3    2020-01-16 [1] CRAN (R 4.0.0)
# fs            1.4.1    2020-04-04 [1] CRAN (R 4.0.0)
# generics      0.0.2    2018-11-29 [1] CRAN (R 4.0.0)
# ggforce     * 0.3.1    2019-08-20 [1] CRAN (R 4.0.0)
# ggplot2     * 3.3.1    2020-05-28 [1] CRAN (R 4.0.0)
# git2r       * 0.27.1   2020-05-03 [1] CRAN (R 4.0.0)
# glue          1.4.1    2020-05-13 [1] CRAN (R 4.0.0)
# gtable        0.3.0    2019-03-25 [1] CRAN (R 4.0.0)
# here          0.1      2017-05-28 [1] CRAN (R 4.0.0)
# htmltools     0.4.0    2019-10-04 [1] CRAN (R 4.0.0)
# knitr         1.28     2020-02-06 [1] CRAN (R 4.0.0)
# labeling      0.3      2014-08-23 [1] CRAN (R 4.0.0)
# lifecycle     0.2.0    2020-03-06 [1] CRAN (R 4.0.0)
# lubridate     1.7.8    2020-04-06 [1] CRAN (R 4.0.0)
# magrittr      1.5      2014-11-22 [1] CRAN (R 4.0.0)
# MASS          7.3-51.6 2020-04-26 [1] CRAN (R 4.0.0)
# memoise       1.1.0    2017-04-21 [1] CRAN (R 4.0.0)
# munsell       0.5.0    2018-06-12 [1] CRAN (R 4.0.0)
# packrat       0.5.0    2018-11-14 [1] CRAN (R 4.0.0)
# patchwork   * 1.0.1    2020-06-22 [1] CRAN (R 4.0.1)
# pillar        1.4.4    2020-05-05 [1] CRAN (R 4.0.0)
# pkgbuild      1.0.8    2020-05-07 [1] CRAN (R 4.0.0)
# pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 4.0.0)
# pkgdown     * 1.5.1    2020-04-09 [1] CRAN (R 4.0.0)
# pkgload       1.1.0    2020-05-29 [1] CRAN (R 4.0.0)
# polyclip      1.10-0   2019-03-14 [1] CRAN (R 4.0.0)
# prettyunits   1.1.1    2020-01-24 [1] CRAN (R 4.0.0)
# processx      3.4.2    2020-02-09 [1] CRAN (R 4.0.0)
# ps            1.3.3    2020-05-08 [1] CRAN (R 4.0.0)
# purrr         0.3.4    2020-04-17 [1] CRAN (R 4.0.0)
# R6            2.4.1    2019-11-12 [1] CRAN (R 4.0.0)
# Rcpp          1.0.4.6  2020-04-09 [1] CRAN (R 4.0.0)
# remotes       2.1.1    2020-02-15 [1] CRAN (R 4.0.0)
# rlang         0.4.6    2020-05-02 [1] CRAN (R 4.0.0)
# rmarkdown     2.2      2020-05-31 [1] CRAN (R 4.0.0)
# roxygen2    * 7.1.0    2020-03-11 [1] CRAN (R 4.0.0)
# rprojroot     1.3-2    2018-01-03 [1] CRAN (R 4.0.0)
# rstudioapi    0.11     2020-02-07 [1] CRAN (R 4.0.0)
# scales        1.1.1    2020-05-11 [1] CRAN (R 4.0.0)
# sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 4.0.0)
# stringi       1.4.6    2020-02-17 [1] CRAN (R 4.0.0)
# stringr       1.4.0    2019-02-10 [1] CRAN (R 4.0.0)
# testthat    * 2.3.2    2020-03-02 [1] CRAN (R 4.0.0)
# tibble        3.0.1    2020-04-20 [1] CRAN (R 4.0.0)
# tidyselect    1.1.0    2020-05-11 [1] CRAN (R 4.0.0)
# tweenr        1.0.1    2018-12-14 [1] CRAN (R 4.0.0)
# usethis     * 1.6.1    2020-04-29 [1] CRAN (R 4.0.0)
# vctrs         0.3.1    2020-06-05 [1] CRAN (R 4.0.0)
# withr         2.2.0    2020-04-20 [1] CRAN (R 4.0.0)
# xfun          0.14     2020-05-20 [1] CRAN (R 4.0.0)
# xml2          1.3.2    2020-04-23 [1] CRAN (R 4.0.0)
# yaml          2.2.1    2020-02-01 [1] CRAN (R 4.0.0)
# 
# [1] /home/danielle/R/x86_64-pc-linux-gnu-library/4.0
# [2] /usr/local/lib/R/site-library
# [3] /usr/lib/R/site-library
# [4] /usr/lib/R/library
