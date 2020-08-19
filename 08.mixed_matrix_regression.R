library(reshape2)

## get veg data from lin 87 of veg script
load("data/veg.dissimilarity.df.Rdata")
colnames(veg.dissimilarity.df) = c("A", "B", "dissim")
## get psi.data
file.to.load1= list.files("data/")[grep("all.psi", list.files("data/"))]
load(paste0("data/", file.to.load1))
rm(file.to.load1)

### load variance matrices
file.to.load2= list.files("data/")[grep("var.04r", list.files("data/"))]
load(paste0("data/", file.to.load2))
rm(file.to.load2)

all.psi = merge(all.psi, veg.dissimilarity.df, by = c("A", "B"))


plot(all.psi$dissim ~ all.psi$psi.30cm)
mod1 = lm(all.psi$dissim ~ all.psi$psi.30cm)
summary(mod1)
abline(mod1)

plot(all.psi$dissim ~ all.psi$psi.all)
mod1 = lm(all.psi$dissim ~ all.psi$psi.all)
summary(mod1)
abline(mod1)

plot(all.psi$dissim ~ all.psi$psi.growing)
mod1 = lm(all.psi$dissim ~ all.psi$psi.growing)
summary(mod1)
abline(mod1)


veg.dissimilarity.df$psi = NA

for(i in 1:nrow(veg.dissimilarity.df)){
  s1 = veg.dissimilarity.df[i, "A"]
  s2 = veg.dissimilarity.df[i, "B"]
  
  val = all.psi[all.psi$A %in% s1 & all.psi$B %in% s2, "psi.all"]
  if (length(val) == 0){
    val = all.psi[all.psi$A %in% s2 & all.psi$B %in% s1, "psi.all"]
  }
  
  if(length(val) == 0){
    val = 0
  }
  
  veg.dissimilarity.df$psi[i] = val
  
}

veg.dissimilarity.df = veg.dissimilarity.df[veg.dissimilarity.df$A != 15,]
veg.dissimilarity.df = veg.dissimilarity.df[veg.dissimilarity.df$B != 15,]



## psi.all matrix
psi.matrix = acast(veg.dissimilarity.df, A ~ B, value.var = "psi")
veg.matrix = acast(veg.dissimilarity.df, A ~ B, value.var = "dissim")

vegan::mantel(psi.matrix, veg.matrix)

Xmats = list(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5)
Xmats = list(ecology = psi.matrix)

MMRR(veg.matrix, Xmats, nperm = 10000)

### load mean matrices
## this overwrites the variance objects
file.to.load3= list.files("data/")[grep("mean.04r", list.files("data/"))]
load(paste0("data/", file.to.load3))
rm(file.to.load3)

Xmats = list(sma30, sma5, smb30, smb5, smc30, smc5, st30, st5)
MMRR(veg.matrix, Xmats, nperm = 10000)

## R2 is 0.24 p = 0.087
