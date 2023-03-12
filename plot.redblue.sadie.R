# This is an edited version of plot.sadie within the epiphy function
# which allows plotting of red-blue figures.
# See epiphy:::plot.sadie for original function

plot.redblue.sadie<- function (x, ..., index = c("Perry", "Li-Madden-Xu"), isoclines = FALSE,
                               
                               resolution = 100, bins = 5, thresholds = c(-1.5, 1.5), conf.level = 0.95,
                               
                               point_size = c("radius", "area"))
  
{
  
  index <- match.arg(index)
  
  point_size <- match.arg(point_size)
  
  data_clust <- x$info_clust
  
  idx <- switch(index, Perry = "idx_P", `Li-Madden-Xu` = "idx_LMX")
  
  data_clust$col <- switch(index, Perry = {
    
    vapply(data_clust[[idx]], function(x) {
      
      if (x < thresholds[1L]) "blue" else if (x <= thresholds[2L]) "white" else "red"
        
    }, character(1L))
    
  }, `Li-Madden-Xu` = {
    
    vapply(seq_len(nrow(data_clust)), function(i1) {
      
      if (data_clust[["prob"]][i1] <= (1 - conf.level)) {
        
        if (data_clust[[idx]][i1] < 0) "blue" else if (data_clust[[idx]][i1] >=
                                                       
                                                       0) "red"
          
      } else {
        
        "white"
        
      }
      
    }, character(1L))
    
  })
  
  gg <- ggplot()
  
  if (isoclines) {
    
    data_loess <- stats::loess(as.formula(paste0(idx, " ~ x * y")),
                               
                               data = data_clust, degree = 2, span = 0.2)
    
    input_val <- expand.grid(x = seq(min(data_clust$x), max(data_clust$x),
                                     
                                     length = resolution), y = seq(min(data_clust$y),
                                                                   
                                                                   max(data_clust$y), length = resolution))
    
    interpolated <- predict(data_loess, input_val)
    
    data_landscape <- data.frame(input_val, z = as.vector(interpolated))
    
    gg <- gg + geom_raster(data = data_landscape, aes(x,
                                                      
                                                      y, fill = z))
    
    gg <- gg + geom_contour(data = data_landscape, aes(x,
                                                       
                                                       y, z = z), bins = bins, size = 0.6, color = "black")
    
    gg <- gg + geom_point(data = data_clust, aes_(quote(x),
                                                  
                                                  quote(y), size = call("abs", as.name(idx))), colour = "black",
                          
                          fill = "black", pch = 21)
    
    switch(point_size, area = {
      
      gg <- gg + scale_size("Absolute\nindex", range = c(0,
                                                         
                                                         10))
      
    }, radius = {
      
      gg <- gg + scale_radius("Absolute\nindex", range = c(0,
                                                           
                                                           10))
      
    })
    
    gg <- gg + scale_fill_gradient2("Interpolated\nindex",
                                    
                                    low = "#67a9cf",
                                    
                                    mid = "#f7f7f7",
                                    
                                    high = "#ef8a62",
                                    
                                    midpoint = 0)
    
  }
  
  else {
    
    gg <- gg + geom_point(data = data_clust, aes_(quote(x),
                                                  
                                                  quote(y), fill = quote(col), size = call("abs", as.name(idx))),
                          
                          colour = "black", pch = 21)
    
    switch(point_size, area = {
      
      gg <- gg + scale_size("Absolute\nindex", range = c(0,
                                                         
                                                         10))
      
    }, radius = {
      
      gg <- gg + scale_radius("Absolute\nindex", range = c(0,
                                                           
                                                           10))
      
    })
    
    switch(index, Perry = {
      
      gg <- gg + scale_fill_manual("Index\nthreshold",
                                   
                                   breaks = c("red", "white", "blue"), labels = c(red = paste0("> ",
                                                                                               
                                                                                               thresholds[2L]), white = paste0("Between ",
                                                                                                                               
                                                                                                                               thresholds[1L], "\nand ", thresholds[2L]),
                                                                                  
                                                                                  blue = paste0("< ", thresholds[1L])), values = c(red = "red",
                                                                                                                                   
                                                                                                                                   white = "white", blue = "blue"))
      
    }, `Li-Madden-Xu` = {
      
      gg <- gg + scale_fill_manual("Index\nthreshold",
                                   
                                   breaks = c("red", "white", "blue"), labels = c(red = "Sign. > 0",
                                                                                  
                                                                                  white = "No sign.\ndifferent\nfrom 0", blue = "Sign. < 0"),
                                   
                                   values = c(red = "red", white = "white", blue = "blue"))
      
    })
    
  }
  
  gg <- gg + theme_bw()
  
  print(gg)
  
  invisible(NULL)
  
}