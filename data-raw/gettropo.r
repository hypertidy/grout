dp <- "/rsdi/PRIVATE/data_local/acecrc.org.au/tropo"
#dir.create("dp")

library(raadtools)
library(dplyr)

## ibcso not really integer, but whatevs
## extract(readtopo("ibcso"), sample(1e9, 20)

fs <- c("etopo2", "etopo1", "gebco_14", "ibcso")
for (i in seq_along(fs)) {
  topo <- readAll(raadtools::readtopo(fs[i]))
  
  tab <- data_frame(topo = as.integer(values(topo)))
  
  template <- data_frame(nrow = nrow(topo), ncol = ncol(topo), xmin = xmin(topo), ymin = ymin(topo), 
                         xmax = xmax(topo), ymax = ymax(topo), crs = projection(topo))
  
  topo <- list(topo = tab, template = template)
  saveRDS(topo, file = file.path(dp, sprintf("%s.rds", fs[i])), compress = "xz")
}

