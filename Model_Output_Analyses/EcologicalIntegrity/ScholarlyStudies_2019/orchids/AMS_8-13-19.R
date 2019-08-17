library(ggplot2)
library(gtable)
library(reshape2)
library(RColorBrewer)

# set working directory and load orchind environmental variables .csv
setwd("U:/CLI/Field Surveys/Orchid")
dat <- read.csv("CLI_Orchids_Environmental_8-2-19.csv")

#FOREST ===========================================================================

#data melt
workingdat <- dat[, c(1, 56:59)]
meltdat <- melt(workingdat, id="Site")

#facet_wrap
plots <- ggplot(data = meltdat, aes(x = value, fill = variable)) + 
  geom_histogram(bins = 10, boundary = 0) + 
  facet_wrap(~variable, nrow = 1) +
  scale_fill_brewer(palette = "Greens", 
                    name = "Radius", labels = c("250m", "500m", "1000m", "5000m")) +
  ggtitle("Forest Cover") +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1.0, "lines")
    )

#print product
plots

#save product
ggsave("forest_plot.jpg", plot = plots, width = 7, height = 3, 
       units = "in", dpi = 600)

#DEVELOPMENT ==========================================================================

#data melt
workingdat2 <- dat[, c(1, 52:55)]
meltdat2 <- melt(workingdat2, id="Site")

#facet_wrap
plots2 <- ggplot(data = meltdat2, aes(x = value, fill = variable)) + 
  geom_histogram(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + 
  facet_wrap(~variable, nrow = 1) +
  scale_fill_brewer(palette = "Greys", 
                    name = "Radius", labels = c("250m", "500m", "1000m", "5000m")) +
  ggtitle("Development Cover") +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1.0, "lines")
  )

plots2

ggsave("development_plot.jpg", plot = plots2, width = 7, height = 3, 
       units = "in", dpi = 600)

#GRASS================================================================================

#data melt
workingdat3 <- dat[, c(1, 60:63)]
meltdat3 <- melt(workingdat3, id="Site")

#facet_wrap
plots3 <- ggplot(data = meltdat3, aes(x = value, fill = variable)) + 
  geom_histogram(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + 
  facet_wrap(~variable, nrow = 1) +
  scale_fill_brewer(palette = "YlOrBr", 
                    name = "Radius", labels = c("250m", "500m", "1000m", "5000m")) +
  ggtitle("Grass Cover") +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1.0, "lines")
  )

plots3

ggsave("grass_plot.jpg", plot = plots3, width = 7, height = 3, 
       units = "in", dpi = 600)

#CROPS================================================================================

#data melt
workingdat4 <- dat[, c(1, 48:51)]
meltdat4 <- melt(workingdat4, id="Site")

#facet_wrap
plots4 <- ggplot(data = meltdat4, aes(x = value, fill = variable)) + 
  geom_histogram(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + 
  facet_wrap(~variable, nrow = 1) +
  scale_fill_brewer(palette = "BuPu", 
                    name = "Radius", labels = c("250m", "500m", "1000m", "5000m")) +
  ggtitle("Crop Cover") +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(1.0, "lines")
  )

plots4

ggsave("crops_plot.jpg", plot = plots4, width = 7, height = 3, 
       units = "in", dpi = 600)