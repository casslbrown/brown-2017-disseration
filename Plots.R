source("./mplus.R")
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
n
library(rhdf5)
mplus.plot.sample_and_estimated_means('Immediate word recall plots.gh5','wrectoti')
rhdf5::mpl