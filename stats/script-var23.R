sink("report-var23.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
Ocs <- readRDS("stouffer_cs.rds")
Ols <- readRDS("stouffer_l.rds")



#v23 will use messy method throughout

# csv23 <- data.frame(
#  v23 = rep(cs$v23, 3),
#  v46 = c(cs$v46a, cs$v46b, cs$v46c)
#)

# For Ocs
other_cols_cs <- setdiff(names(Ocs), c("v23a", "v23b"))

cs <- data.frame(
  v23 = c(Ocs$v23a, Ocs$v23b),
  lapply(Ocs[other_cols_cs], function(col) rep(col, 2))
)

names(cs) <- c("v23", other_cols_cs)

# For Ols
other_cols_ls <- setdiff(names(Ols), c("v23a", "v23b"))

ls <- data.frame(
  v23 = c(Ols$v23a, Ols$v23b),
  lapply(Ols[other_cols_ls], function(col) rep(col, 2))
)

names(ls) <- c("v23", other_cols_ls)


cs2314<- xtabs(~ v23 + v14, data= cs)
fcs2314<- cs2314[-c(1,7),-5]
# dimnames(fcs2214) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2314) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v23 and v14 cs\n")
fcs2314
chisq.test(fcs2314)

ls2314 <- xtabs(~v23 + v14, data= ls)
fls2314 <- ls2314[-c(1,7),-5]
dimnames(fls2314) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v23 and v14 ls\n")
fls2314
chisq.test(fls2314)

cs2215<- xtabs(~ v23 + v15, data= cs)
fcs2215<- cs2215[-c(1,7),-5]
dimnames(fcs2215) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v23 and v15 cs\n")
fcs2215
chisq.test(fcs2214)

ls2215 <- xtabs(~v23 + v15, data= ls)
fls2215 <- ls2215[-c(1,7),-5]
dimnames(fls2215) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v23 and v15 ls\n")
fls2215
chisq.test(fls2215)

cs2235<- xtabs(~ v23 + v35, data= cs)
fcs2235<- cs2235[-c(1,7),-3]
dimnames(fcs2235) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v23 and v35 cs\n")
fcs2235
chisq.test(fcs2235)

ls2235 <- xtabs(~v23 + v35, data= ls)
fls2235 <- ls2235[-c(1,7),-3]
dimnames(fls2235) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v23 and v35 ls\n")
fls2235
chisq.test(fls2235)

cs2236<- xtabs(~ v23 + v36, data= cs)
fcs2236<- cs2236[-c(1,7),-3]
dimnames(fcs2236) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v23 and v36 cs\n")
fcs2236
chisq.test(fcs2236)

ls2236 <- xtabs(~v23 + v36, data= ls)
fls2236 <- ls2236[-c(1,7),-3]
dimnames(fls2236) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v23 and v36 ls\n")
fls2236
chisq.test(fls2236)

cs2237<- xtabs(~ v23 + v37, data= cs)
fcs2237<- cs2237[-c(1,7),-3]
dimnames(fcs2237) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v23 and v37 cs\n")
fcs2237
chisq.test(fcs2237)

ls2237 <- xtabs(~v23 + v37, data= ls)
fls2237 <- ls2237[-c(1,7),-3]
dimnames(fls2237) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v23 and v37 ls\n")
fls2237
chisq.test(fls2237)

cs2242<- xtabs(~ v23 + v42, data= cs)
fcs2242<- cs2242[-c(1,7),-6]
dimnames(fcs2242) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v23 and v42 cs\n")
fcs2242
chisq.test(fcs2242)

ls2242 <- xtabs(~v23 + v42, data= ls)
fls2242 <- ls2242[-c(1,7),-6]
dimnames(fls2242) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v23 and v42 ls\n")
fls2242
chisq.test(fls2242)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v23 = rep(cs$v23, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2243<- xtabs(~ v23 + v43, data = cs_long)

# cs2243<- xtabs(~ v23 + v43a, data= cs)
fcs2243<- cs2243[-c(1,7),-c(1,9,10)]
dimnames(fcs2243) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v23 and v43 cs\n")
fcs2243
chisq.test(fcs2243)

ls_long <- data.frame(
  v23 = rep(ls$v23, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2243<- xtabs(~ v23 + v43, data = ls_long)
fls2243 <- ls2243[-c(1,7),-c(1,9,10)]
dimnames(fls2243) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v23 and v43 ls\n")
fls2243
chisq.test(fls2243)

cs2244<- xtabs(~ v23 + v44, data= cs)
fcs2244<- cs2244[-c(1,7),-c(4)]
dimnames(fcs2244) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v23 and v44 cs\n")
fcs2244
chisq.test(fcs2244)

ls2244 <- xtabs(~v23 + v44, data= ls)
fls2244 <- ls2244[-c(1,7),-c(4)]
dimnames(fls2244) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v23 and v44 ls\n")
fls2244
chisq.test(fls2244)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v23 = rep(cs$v23, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2246<- xtabs(~ v23 + v46, data= csv46)
fcs2246<- cs2246[-c(1,7),-c(1,10)]
dimnames(fcs2246) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v23 and v46 cs\n")
fcs2246
chisq.test(fcs2246)

lsv46 <- data.frame(
  v23 = rep(ls$v23, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2246 <- xtabs(~v23 + v46, data= lsv46)
fls2246 <- ls2246[-c(1,7),-c(1,10)]
dimnames(fls2246) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v23 and v46 ls\n")
fls2246
chisq.test(fls2246)

cs2247<- xtabs(~ v23 + v47, data= cs)
fcs2247<- cs2247[-c(1,7),-c(3)]
dimnames(fcs2247) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v23 and v47 cs\n")
fcs2247
chisq.test(fcs2247)

ls2247 <- xtabs(~v23 + v47, data= ls)
fls2247 <- ls2247[-c(1,7),-c(3)]
dimnames(fls2247) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v23 and v47 ls\n")
fls2247
chisq.test(fls2247)

#messy again

csv48 <- data.frame(
  v23 = rep(cs$v23, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2248<- xtabs(~ v23 + v48, data= csv48)
fcs2248<- cs2248[-c(1,7),-c(1,6)]
dimnames(fcs2248) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v23 and v48 cs\n")
fcs2248
chisq.test(fcs2248)

lsv48 <- data.frame(
  v23 = rep(ls$v23, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2248 <- xtabs(~v23 + v48, data= lsv48)
fls2248 <- ls2248[-c(1,7),-c(1,6)]
dimnames(fls2248) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v23 and v48 ls\n")
fls2248
chisq.test(fls2248)

cs2249<- xtabs(~ v23 + v49, data= cs)
fcs2249<- cs2249[-c(1,7),-c(3,4)]
dimnames(fcs2249) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v23 and v49 cs\n")
fcs2249
chisq.test(fcs2249)

ls2249 <- xtabs(~v23 + v49, data= ls)
fls2249 <- ls2247[-c(1,7),-c(3,4)]
dimnames(fls2249) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v23 and v49 ls\n")
fls2249
chisq.test(fls2249)

cs2250<- xtabs(~ v23 + v50, data= cs)
fcs2250<- cs2250[-c(1,7),-c(3,4,5)]
dimnames(fcs2250) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v23 and v50 cs\n")
fcs2250
chisq.test(fcs2250)

ls2250 <- xtabs(~v23 + v50, data= ls)
fls2250 <- ls2250[-c(1,7),-c(3,4,5)]
dimnames(fls2250) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v23 and v50 ls\n")
fls2250
chisq.test(fls2250)

cs2251<- xtabs(~ v23 + v51, data= cs)
fcs2251<- cs2251[-c(1,7),-c(3,4,5)]
dimnames(fcs2251) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v23 and v51 cs\n")
fcs2251
chisq.test(fcs2251)

ls2251 <- xtabs(~v23 + v51, data= ls)
fls2251 <- ls2251[-c(1,7),-c(3,4,5)]
dimnames(fls2251) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v23 and v51 ls\n")
fls2251
chisq.test(fls2251)

cs2252<- xtabs(~ v23 + v52, data= cs)
fcs2252<- cs2252[-c(1,7),-c(3,4)]
dimnames(fcs2252) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v23 and v52 cs\n")
fcs2252
chisq.test(fcs2252)

ls2252 <- xtabs(~v23 + v52, data= ls)
fls2252 <- ls2252[-c(1,7),-c(3,4)]
dimnames(fls2252) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v23 and v52 ls\n")
fls2252
chisq.test(fls2252)


#messy again

csv53 <- data.frame(
  v23 = rep(cs$v23, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs2253<- xtabs(~ v23 + v53, data= csv53)
fcs2253<- cs2253[-c(1,7),-c(9,10,11)]
dimnames(fcs2253) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v23 and v53 cs\n")
fcs2253
chisq.test(fcs2253)

lsv53 <- data.frame(
  v23 = rep(ls$v23, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls2253 <- xtabs(~v23 + v53, data= lsv53)
fls2253 <- ls2253[-c(1,7),-c(9,10,11)]
dimnames(fls2253) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v23 and v53 ls\n")
fls2253
chisq.test(fls2253)

#messy again
csv55 <- data.frame(
  v23 = rep(cs$v23, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs2255<- xtabs(~ v23 + v55, data= csv55)
fcs2255<- cs2255[-c(1,7),-c(1,9)]
dimnames(fcs2255) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v23 and v55 cs\n")
fcs2255
chisq.test(fcs2255)

lsv55 <- data.frame(
  v23 = rep(ls$v23, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls2255 <- xtabs(~v23 + v55, data= lsv55)
fls2255 <- ls2255[-c(1,7),-c(1,9)]
dimnames(fls2255) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v23 and v55 ls\n")
fls2255
chisq.test(fls2255)

cs2257<- xtabs(~ v23 + v57, data= cs)
fcs2257<- cs2257[-c(1,7),-c(3,4)]
dimnames(fcs2257) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v23 and v57 cs\n")
fcs2257
chisq.test(fcs2257)

ls2257 <- xtabs(~v23 + v57, data= ls)
fls2257 <- ls2257[-c(1,7),-c(3,4)]
dimnames(fls2257) <- list("v23 Why or Why not Cut Aid"= c("Yes, Money for home","Yes, Waste/Ally Hostility","No, Altruistic","No, Helps Us","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v23 and v57 ls\n")
fls2257
chisq.test(fls2257)

sink()
