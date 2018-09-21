


imagePlot <- function(m) {
  print(m)
  print(colnames(m))
  image(1:ncol(m), 1:nrow(m), t(m), col = terrain.colors(60), 
        axes = FALSE, main='PanCan Cluster Label Check (percent samples)', 
        xlab = 'Reported Cluster Labels', ylab='New Cluster Calls')
  axis(1, 1:ncol(m), colnames(m))
  axis(2, 1:nrow(m), rownames(m))
  for (x in 1:ncol(m))
    for (y in 1:nrow(m))
      text(x, y, m[y,x])
}


