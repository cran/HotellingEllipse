## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.retina = 2
)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(dplyr)
library(glue)
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
res <- ellipseParam(pca_scores, k = 3, conf.limit = c(0.975, 0.999))
res$cutoff.97.5pct
res$cutoff.99.9pct

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
ellipse_axes <- ellipseParam(pca_scores, pcx = 1, pcy = 3)

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

## -----------------------------------------------------------------------------
t1 <- round(as.numeric(pca_mod$eig[1,2]), 2)
t2 <- round(as.numeric(pca_mod$eig[2,2]), 2)
t3 <- round(as.numeric(pca_mod$eig[3,2]), 2)

## ----message=FALSE, warning=FALSE---------------------------------------------
pca_scores %>%
  ggplot(aes(x = Dim.1, y = Dim.3)) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), linewidth = .5, linetype = "solid", fill = "white") + 
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), linewidth = .5, linetype = "solid", fill = "white") +
  geom_point(aes(fill = Tsq), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  labs(
    title = "Scatterplot of PCA scores", 
    subtitle = "PC1 vs. PC3", 
    x = glue("PC1 [{t1}%]"), 
    y = glue("PC3 [{t3}%]"), 
    fill = "T2"
    ) +
  theme_grey() +
  theme(
    aspect.ratio = .7,
    panel.grid = element_blank(),
    panel.background = element_rect(
    colour = "black",
    linewidth = .3
    )
  )

## -----------------------------------------------------------------------------
xy_coord <- ellipseCoord(pca_scores, pcx = 2, pcy = 3, conf.limit = 0.975, pts = 500)

## -----------------------------------------------------------------------------
str(xy_coord)

## ----message=FALSE, warning=FALSE---------------------------------------------
ggplot() +
  geom_polygon(data = xy_coord, aes(x, y), color = "black", fill = "white") +
  geom_point(data = pca_scores, aes(x = Dim.2, y = Dim.3), shape = 21, size = 3, fill = "black", color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  labs(
    title = "Scatterplot of PCA scores", 
    subtitle = "PC2 vs. PC3", 
    x = glue("PC2 [{t2}%]"), 
    y = glue("PC3 [{t3}%]")
    ) +
  theme_grey() +
  theme(
    aspect.ratio = .7,
    panel.grid = element_blank(),
    panel.background = element_rect(
    colour = "black",
    linewidth = .3
    )
  )

## -----------------------------------------------------------------------------
xyz_coord <- ellipseCoord(pca_scores, pcx = 1, pcy = 2, pcz = 3, conf.limit = 0.95, pts = 100)

## -----------------------------------------------------------------------------
str(xyz_coord)

## -----------------------------------------------------------------------------
T2 <- ellipseParam(pca_scores, k = 3)$Tsquare$value

