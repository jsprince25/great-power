sink("rlog2.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2214<- xtabs(~ v22 + v14, data= cs)
fcs2214<- cs2214[-c(4,5),-5]
# dimnames(fcs2214) <- list("v22 Cut Aid to Allies Now"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2214) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v14 cs\n")
fcs2214
chisq.test(fcs2214)

ls2214 <- xtabs(~v22 + v14, data= ls)
fls2214 <- ls2214[-c(4,5),-5]
dimnames(fls2214) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v14 ls\n")
fls2214
chisq.test(fls2214)

cs2215<- xtabs(~ v22 + v15, data= cs)
fcs2215<- cs2215[-c(4,5),-5]
dimnames(fcs2215) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v15 cs\n")
fcs2215
chisq.test(fcs2214)

ls2215 <- xtabs(~v22 + v15, data= ls)
fls2215 <- ls2215[-c(4,5),-5]
dimnames(fls2215) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v22 and v15 ls\n")
fls2215
chisq.test(fls2215)

cs2235<- xtabs(~ v22 + v35, data= cs)
fcs2235<- cs2235[-c(4,5),-3]
dimnames(fcs2235) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v22 and v35 cs\n")
fcs2235
chisq.test(fcs2235)

ls2235 <- xtabs(~v22 + v35, data= ls)
fls2235 <- ls2235[-c(4,5),-3]
dimnames(fls2235) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v22 and v35 ls\n")
fls2235
chisq.test(fls2235)

cs2236<- xtabs(~ v22 + v36, data= cs)
fcs2236<- cs2236[-c(4,5),-3]
dimnames(fcs2236) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v22 and v36 cs\n")
fcs2236
chisq.test(fcs2236)

ls2236 <- xtabs(~v22 + v36, data= ls)
fls2236 <- ls2236[-c(4,5),-3]
dimnames(fls2236) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v22 and v36 ls\n")
fls2236
chisq.test(fls2236)

cs2237<- xtabs(~ v22 + v37, data= cs)
fcs2237<- cs2237[-c(4,5),-3]
dimnames(fcs2237) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v22 and v37 cs\n")
fcs2237
chisq.test(fcs2237)

ls2237 <- xtabs(~v22 + v37, data= ls)
fls2237 <- ls2237[-c(4,5),-3]
dimnames(fls2237) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v22 and v37 ls\n")
fls2237
chisq.test(fls2237)

cs2242<- xtabs(~ v22 + v42, data= cs)
fcs2242<- cs2242[-c(4,5),-6]
dimnames(fcs2242) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v22 and v42 cs\n")
fcs2242
chisq.test(fcs2242)

ls2242 <- xtabs(~v22 + v42, data= ls)
fls2242 <- ls2242[-c(4,5),-6]
dimnames(fls2242) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v22 and v42 ls\n")
fls2242
chisq.test(fls2242)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v22 = rep(cs$v22, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2243<- xtabs(~ v22 + v43, data = cs_long)

# cs2243<- xtabs(~ v22 + v43a, data= cs)
fcs2243<- cs2243[-c(4,5),-c(1,9,10)]
dimnames(fcs2243) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v22 and v43 cs\n")
fcs2243
chisq.test(fcs2243)

ls_long <- data.frame(
  v22 = rep(ls$v22, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2243<- xtabs(~ v22 + v43, data = ls_long)
fls2243 <- ls2243[-c(4,5),-c(1,9,10)]
dimnames(fls2243) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v22 and v43 ls\n")
fls2243
chisq.test(fls2243)

cs2244<- xtabs(~ v22 + v44, data= cs)
fcs2244<- cs2244[-c(4,5),-c(4)]
dimnames(fcs2244) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v22 and v44 cs\n")
fcs2244
chisq.test(fcs2244)

ls2244 <- xtabs(~v22 + v44, data= ls)
fls2244 <- ls2244[-c(4,5),-c(4)]
dimnames(fls2244) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v22 and v44 ls\n")
fls2244
chisq.test(fls2244)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc" )

#messy again

csv46 <- data.frame(
  v22 = rep(cs$v22, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2246<- xtabs(~ v22 + v46, data= csv46)
fcs2246<- cs2246[-c(4,5),-c(1,10)]
dimnames(fcs2246) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v22 and v46 cs\n")
fcs2246
chisq.test(fcs2246)

lsv46 <- data.frame(
  v22 = rep(ls$v22, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2246 <- xtabs(~v22 + v46, data= lsv46)
fls2246 <- ls2246[-c(4,5),-c(1,10)]
dimnames(fls2246) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v22 and v46 ls\n")
fls2246
chisq.test(fls2246)

cs2247<- xtabs(~ v22 + v47, data= cs)
fcs2247<- cs2247[-c(4,5),-c(3)]
dimnames(fcs2247) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v22 and v47 cs\n")
fcs2247
chisq.test(fcs2247)

ls2247 <- xtabs(~v22 + v47, data= ls)
fls2247 <- ls2247[-c(4,5),-c(3)]
dimnames(fls2247) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v22 and v47 ls\n")
fls2247
chisq.test(fls2247)

#messy again

csv48 <- data.frame(
  v22 = rep(cs$v22, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2248<- xtabs(~ v22 + v48, data= csv48)
fcs2248<- cs2248[-c(4,5),-c(1,6)]
dimnames(fcs2248) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v22 and v48 cs\n")
fcs2248
chisq.test(fcs2248)

lsv48 <- data.frame(
  v22 = rep(ls$v22, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2248 <- xtabs(~v22 + v48, data= lsv48)
fls2248 <- ls2248[-c(4,5),-c(1,6)]
dimnames(fls2248) <- list("v22 Cut Aid"= c("Yes","Qualified","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v22 and v48 ls\n")
fls2248
chisq.test(fls2248)

sink()
