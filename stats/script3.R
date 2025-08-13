sink("rlog2.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2214<- xtabs(~ v22 + v14, data= cs)
fcs2214<- cs2214[-c(4,5),-5]
# dimnames(fcs2214) <- list("v22 Cut Aid to Allies Now"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2214) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v14 cs")
fcs2214
chisq.test(fcs2214)

ls2214 <- xtabs(~v22 + v14, data= ls)
fls2214 <- ls2214[-c(4,5),-5]
dimnames(fls2214) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v14 ls")
fls2214
chisq.test(fls2214)

sink()
