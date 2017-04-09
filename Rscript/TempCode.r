# Find the correlation between MWD and each species' cover value.
a <- group_by(soil, Block, Treatment) %>%
  summarise(mean(MWD))
b <- a$`mean(MWD)`
mean.MWD <- c(b[1], b[2], b[3], b[5], b[6], b[7], b[8], b[4])
cor.vec <- rep(NA, times=length(species.vec))
for (ii in 1:length(species.vec))
{
  this.dat.avg <- dat.avg.list[[ii]]
  this.mean.cv <- as.numeric(as.character(this.dat.avg$y.avg))
  cor.vec[ii] <- cor(mean.MWD, this.mean.cv)
}