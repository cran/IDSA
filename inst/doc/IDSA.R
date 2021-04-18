## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- read.table("table1.txt", header = TRUE, sep = "\t")
kable(dt, caption = "A brief summary of spatial stratified heterogeneity models and R packages in this document.", col.names = c("Model", "R package and primary functions")) %>%
  kable_styling(c("striped", "bordered")) %>% 
  column_spec(1, width = "20em") %>% 
  column_spec(2, width = "20em")

## ---- eval = FALSE------------------------------------------------------------
#  ## install and library pacakges
#  install.packages("GD")
#  install.packages("IDSA")
#  library("GD")
#  library("IDSA")

## ---- eval = FALSE------------------------------------------------------------
#  library("IDSA")
#  data(sim)
#  head(sim)
#  ## visualize simulation data
#  library(ggplot2)
#  library(RColorBrewer)
#  library(wesanderson)
#  # plot y
#  ggplot(sim, aes(x = lo, y = la, fill = y)) +
#    geom_tile() +
#    scale_fill_gradientn(colours = wes_palette("Zissou1", 100,
#                         type = "continuous")) +
#    scale_x_discrete(expand = c(0, 0)) +
#    scale_y_discrete(expand = c(0, 0)) +
#    coord_equal()
#  # plot xa
#  ggplot(sim, aes(x = lo, y = la, fill = xa)) +
#    geom_tile() +
#    scale_fill_gradientn(colours = brewer.pal(n = 8, name = "YlGn")) +
#    scale_x_discrete(expand = c(0, 0)) +
#    scale_y_discrete(expand = c(0, 0)) +
#    coord_equal()

## ---- eval = FALSE------------------------------------------------------------
#  ## spatial discretization for variables
#  library(IDSA)
#  sim.disc <- do.call(cbind, lapply(1:3, function(x){
#    data.frame(discretize(sim[,x+3], 3, method = "quantile"))
#  }))
#  names(sim.disc) <- paste("h", 1:3, sep = "")
#  sim <- cbind(sim, sim.disc)
#  
#  ## GD model
#  library(GD)
#  ## factor detector
#  g1 <- gd(y ~ h1 + h2 + h3, data = sim)
#  g1
#  plot(g1)
#  ## risk detector: risk mean and detector
#  rm1 <- riskmean(y ~ h1 + h2 + h3, data = sim)
#  rm1
#  plot(rm1)
#  gr1 <- gdrisk(y ~ h1 + h2 + h3, data = sim)
#  gr1
#  plot(gr1)
#  ## interaction detector
#  gi1 <- gdinteract(y ~ h1 + h2 + h3, data = sim)
#  gi1
#  plot(gi1)
#  ## ecological detector
#  gd1 <- gdeco(y ~ h1 + h2 + h3, data = sim)
#  gd1
#  plot(gd1)

## ---- eval = FALSE------------------------------------------------------------
#  ## optional methods: equal, natural, quantile, geometric, sd and manual
#  discmethod <- c("equal","quantile")
#  discitv <- c(3:6)
#  ## "gdm" function
#  gdm1 <- gdm(y ~ xa + xb + xc,
#              continuous_variable = c("xa", "xb", "xc"),
#              data = sim,
#              discmethod = discmethod, discitv = discitv)
#  gdm1
#  plot(gdm1)

## ---- eval = FALSE------------------------------------------------------------
#  ## set optional discretization methods and numbers of intervals
#  discmethod <- c("equal","quantile")
#  discitv <- c(3:6)
#  ## optimal discretization
#  odc1 <- optidisc(y ~ xa + xb + xc, data = sim,
#                   discmethod, discitv)
#  odc1
#  plot(odc1)

## ---- eval = FALSE------------------------------------------------------------
#  library("IDSA")
#  data(sim)
#  ## SPADE-based PD
#  q.spade <- spade(formula = y ~ xa + xb, location = c("lo", "la"),
#                   data = sim, ndisc = c(4, 4), methoddisc = "quantile")
#  q.spade

## ---- eval = FALSE------------------------------------------------------------
#  idsa.ab <- idsa(y ~ xa + xb, location = c("la", "lo"), data = sim, c(4, 4),
#                  methoddisc = "quantile", methodoverlay = "fuzzyAND")
#  idsa.ab$qs.interaction

## ---- eval = FALSE------------------------------------------------------------
#  r3 <- gdrisk(y ~ overlay, data = idsa.ab$data)
#  plot(r3) # Figure 4(c) in Song et al (2021)

## ---- eval = FALSE------------------------------------------------------------
#  # interaction variable derived from GD-ID
#  sim$xa2 <- discretize(sim$xa, 5, method = "quantile")
#  sim$xb2 <- discretize(sim$xb, 4, method = "quantile")
#  sim$xid.gd <- do.call(paste, c(sim[, c("xa2", "xb2")], sep = "_"))
#  # significant difference test
#  level1 <- expand.grid(levels(sim$xb2), levels(sim$xa2))
#  level1 <- do.call(paste, c(level1[,c(2:1)], sep = "_"))
#  sim$xid.gd <- factor(sim$xid.gd, levels = level1)
#  r1 <- gdrisk(y ~ xid.gd, data = sim)
#  plot(r1) # Figure 4(a) in Song et al (2021)
#  
#  # interaction variable derived from SPADE-ID
#  xid.spade <- idsa(y ~ xa + xb, location = c("la", "lo"), data = sim, c(4, 4),
#                    methoddisc = "quantile", methodoverlay = "intersection")
#  xid.spade$qs.interaction
#  # significant difference test
#  r2 <- gdrisk(y ~ overlay, data = xid.spade$data)
#  plot(r2)  # Figure 4(b) in Song et al (2021)
#  
#  # plot cumulative significance distributions: Figure 4(d) in Song et al (2021)
#  sigratio <- rbind(data.frame(method = rep("idsa", nrow(r3$overlay)), sig = r3$overlay$sig),
#                    data.frame(method = rep("spadeid", nrow(r2$overlay)), sig = r2$overlay$sig),
#                    data.frame(method = rep("gdid", nrow(r1$xid.gd)), sig = r1$xid.gd$sig))
#  sigratio$method <- factor(sigratio$method, levels = c("idsa", "spadeid", "gdid"))
#  library(ggplot2)
#  ggplot(sigratio, aes(sig, color = method)) + stat_ecdf(geom = "step") +
#    scale_x_sqrt(breaks = c(0, 0.01, 0.05, 0.1, 0.5, 1)) +
#    geom_vline(xintercept = 0.05) +
#    theme_bw()

