cs <- readRDS("stouffer_cs.rds")
summary(cs)

summary(cs[, 58:61])
names(cs)[58:61]
which(names(cs) =="v58")
# so v58 is indexed at 90
names(cs)[93]
# and v61 is indexed at 93, so now...
summary(cs[, 90:93])
save.image(file = "saveattempt")
