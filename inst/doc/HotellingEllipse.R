## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.retina = 2
)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(dplyr)
library(FactoMineR)
library(tibble)
library(purrr)
library(ggplot2)
library(ggforce)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(HotellingEllipse)

## -----------------------------------------------------------------------------
data("specData")

## -----------------------------------------------------------------------------
set.seed(123)
pca_mod <- specData %>%
  select(where(is.numeric)) %>%
  PCA(scale.unit = FALSE, graph = FALSE)

## -----------------------------------------------------------------------------
pca_scores <- pca_mod %>%
  pluck("ind", "coord") %>%
  as_tibble()

## -----------------------------------------------------------------------------
pca_scores

## -----------------------------------------------------------------------------
res <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)

## -----------------------------------------------------------------------------
str(res)

## -----------------------------------------------------------------------------
a1 <- pluck(res, "Ellipse", "a.99pct")
b1 <- pluck(res, "Ellipse", "b.99pct")

## -----------------------------------------------------------------------------
a2 <- pluck(res, "Ellipse", "a.95pct")
b2 <- pluck(res, "Ellipse", "b.95pct")

## -----------------------------------------------------------------------------
Tsq <- pluck(res, "Tsquare", "value")

## ----eval=FALSE, fig.align='center', fig.height=5, fig.width=7, include=FALSE----
#  pca_scores %>%
#    ggplot(aes(x = Dim.1, y = Dim.2)) +
#    geom_point(aes(fill = Tsq), shape = 21, size = 3, color = "black") +
#    scale_fill_viridis_c(option = "viridis") +
#    geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .5, linetype = "dotted") +
#    geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed") +
#    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
#    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
#    labs(
#      title = "Scatterplot of PCA scores",
#      subtitle = "PC1 vs. PC2",
#      x = "PC1",
#      y = "PC2",
#      fill = "T2 stats",
#      caption = "Figure 1"
#      ) +
#    theme_bw()

## -----------------------------------------------------------------------------
xy_coord <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 3, conf.limit = 0.95, pts = 500)

## -----------------------------------------------------------------------------
str(xy_coord)

## ----eval=FALSE, fig.align='center', fig.height=5, fig.width=7, include=FALSE----
#  pca_scores %>%
#    ggplot() +
#    geom_point(aes(x = Dim.1, y = Dim.3), shape = 21, size = 4, fill = "gold", color = "black") +
#    geom_ellipse(data = xy_coord, aes(x0 = x, y0 = y, a = 1, b = 1, angle = 0), size = .5, linetype = "dashed") +
#    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
#    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
#    labs(
#      title = "Scatterplot of PCA scores",
#      subtitle = "PC1 vs. PC3",
#      x = "PC1",
#      y = "PC3",
#      caption = "Figure 2"
#      ) +
#    theme_bw()

