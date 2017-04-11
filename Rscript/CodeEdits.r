# OLD LINE OF CODE:
#dat.avg.this.species <- rbind(dat.avg.this.species[1:(which.row-1),],
#                              new.row,
#                              dat.avg.this.species[-(1:(which.row-1)),])

# NEW CODE:
dat.avg.as.mat <- cbind(dat.avg.this.species[,1], dat.avg.this.species[,2],
                        dat.avg.this.species[,3])
dat.avg.as.mat <- rbind(dat.avg.as.mat[1:(which.row-1),],
                        new.row,
                        dat.avg.as.mat[-(1:(which.row-1)),])
row.names(dat.avg.as.mat) <- c(1:8)
dat.avg.this.species <- dat.avg.as.mat