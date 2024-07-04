## ----include = FALSE----------------------------------------------------------
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

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
requireNamespace("rgl", quietly = TRUE)
requireNamespace("scales", quietly = TRUE)
requireNamespace("viridisLite", quietly = TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(HotellingEllipse)

## -----------------------------------------------------------------------------
data("specData", package = "HotellingEllipse")

## -----------------------------------------------------------------------------
set.seed(002)
pca_mod <- specData %>%
  select(where(is.numeric)) %>%
  PCA(scale.unit = FALSE, graph = FALSE)

## -----------------------------------------------------------------------------
pca_scores <- pca_mod %>%
  pluck("ind", "coord") %>%
  as_tibble() %>%
  print()

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, k = 3)$Tsquare$value

## -----------------------------------------------------------------------------
T2

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, k = 5)$Tsquare$value

## -----------------------------------------------------------------------------
T2

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, threshold = 0.80)$Tsquare$value

## -----------------------------------------------------------------------------
T2

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, threshold = 0.95)$Tsquare$value

## -----------------------------------------------------------------------------
T2

## -----------------------------------------------------------------------------
ellipse_axes <- ellipseParam(pca_scores, pcx = 2, pcy = 4)

## -----------------------------------------------------------------------------
str(ellipse_axes)

## -----------------------------------------------------------------------------
a1 <- ellipse_axes %>% pluck("Ellipse", "a.99pct")
b1 <- ellipse_axes %>% pluck("Ellipse", "b.99pct")

## -----------------------------------------------------------------------------
a2 <- ellipse_axes %>% pluck("Ellipse", "a.95pct")
b2 <- ellipse_axes %>% pluck("Ellipse", "b.95pct")

## -----------------------------------------------------------------------------
Tsq <- ellipse_axes %>% pluck("Tsquare", "value")

## ----message=FALSE, warning=FALSE---------------------------------------------
pca_scores %>%
  ggplot(aes(x = Dim.2, y = Dim.4)) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), linewidth = .5, linetype = "solid", fill = "white") + 
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), linewidth = .5, linetype = "dashed", fill = "white") +
  geom_point(aes(fill = Tsq), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  labs(
    title = "Scatterplot of PCA scores", 
    subtitle = "PC1 vs. PC3", 
    x = "PC2", 
    y = "PC4", 
    fill = "T2", 
    caption = "Figure 1"
    ) +
  theme_grey()

## -----------------------------------------------------------------------------
xy_coord <- ellipseCoord(pca_scores, pcx = 1, pcy = 5, conf.limit = 0.975, pts = 500)

## -----------------------------------------------------------------------------
str(xy_coord)

## ----message=FALSE, warning=FALSE---------------------------------------------
ggplot() +
  geom_polygon(data = xy_coord, aes(x, y), color = "black", fill = "white") +
  geom_point(data = pca_scores, aes(x = Dim.1, y = Dim.5), shape = 21, size = 5, fill = "blue", color = "black", alpha = 1/2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  labs(
    title = "Scatterplot of PCA scores", 
    subtitle = "PC1 vs. PC5", 
    x = "PC1", 
    y = "PC5", 
    caption = "Figure 2"
    ) +
  theme_grey()

## -----------------------------------------------------------------------------
xyz_coord <- ellipseCoord(pca_scores, pcx = 1, pcy = 2, pcz = 3, conf.limit = 0.95, pts = 100)

## -----------------------------------------------------------------------------
str(xyz_coord)

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, k = 3)$Tsquare$value

