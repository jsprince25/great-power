sink("report-var24.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2414<- xtabs(~ v24 + v14, data= cs)
fcs2414<- cs2414[-c(3),-5]
# dimnames(fcs2414) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2414) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v24 and v14 cs\n")
fcs2414
chisq.test(fcs2414)

ls2414 <- xtabs(~v24 + v14, data= ls)
fls2414 <- ls2414[-c(3),-5]
dimnames(fls2414) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v24 and v14 ls\n")
fls2414
chisq.test(fls2414)

cs2415<- xtabs(~ v24 + v15, data= cs)
fcs2415<- cs2415[-c(3),-5]
dimnames(fcs2415) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v24 and v15 cs\n")
fcs2415
chisq.test(fcs2414)

ls2415 <- xtabs(~v24 + v15, data= ls)
fls2415 <- ls2415[-c(3),-5]
dimnames(fls2415) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v24 and v15 ls\n")
fls2415
chisq.test(fls2415)

cs2435<- xtabs(~ v24 + v35, data= cs)
fcs2435<- cs2435[-c(3),-3]
dimnames(fcs2435) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v24 and v35 cs\n")
fcs2435
chisq.test(fcs2435)

ls2435 <- xtabs(~v24 + v35, data= ls)
fls2435 <- ls2435[-c(3),-3]
dimnames(fls2435) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v24 and v35 ls\n")
fls2435
chisq.test(fls2435)

cs2436<- xtabs(~ v24 + v36, data= cs)
fcs2436<- cs2436[-c(3),-3]
dimnames(fcs2436) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v24 and v36 cs\n")
fcs2436
chisq.test(fcs2436)

ls2436 <- xtabs(~v24 + v36, data= ls)
fls2436 <- ls2436[-c(3),-3]
dimnames(fls2436) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v24 and v36 ls\n")
fls2436
chisq.test(fls2436)

cs2437<- xtabs(~ v24 + v37, data= cs)
fcs2437<- cs2437[-c(3),-3]
dimnames(fcs2437) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v24 and v37 cs\n")
fcs2437
chisq.test(fcs2437)

ls2437 <- xtabs(~v24 + v37, data= ls)
fls2437 <- ls2437[-c(3),-3]
dimnames(fls2437) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v24 and v37 ls\n")
fls2437
chisq.test(fls2437)

cs2442<- xtabs(~ v24 + v42, data= cs)
fcs2442<- cs2442[-c(3),-6]
dimnames(fcs2442) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v24 and v42 cs\n")
fcs2442
chisq.test(fcs2442)

ls2442 <- xtabs(~v24 + v42, data= ls)
fls2442 <- ls2442[-c(3),-6]
dimnames(fls2442) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v24 and v42 ls\n")
fls2442
chisq.test(fls2442)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v24 = rep(cs$v24, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2443<- xtabs(~ v24 + v43, data = cs_long)

# cs2443<- xtabs(~ v24 + v43a, data= cs)
fcs2443<- cs2443[-c(3),-c(1,9,10)]
dimnames(fcs2443) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v24 and v43 cs\n")
fcs2443
chisq.test(fcs2443)

ls_long <- data.frame(
  v24 = rep(ls$v24, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2443<- xtabs(~ v24 + v43, data = ls_long)
fls2443 <- ls2443[-c(3),-c(1,9,10)]
dimnames(fls2443) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v24 and v43 ls\n")
fls2443
chisq.test(fls2443)

cs2444<- xtabs(~ v24 + v44, data= cs)
fcs2444<- cs2444[-c(3),-c(4)]
dimnames(fcs2444) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v24 and v44 cs\n")
fcs2444
chisq.test(fcs2444)

ls2444 <- xtabs(~v24 + v44, data= ls)
fls2444 <- ls2444[-c(3),-c(4)]
dimnames(fls2444) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v24 and v44 ls\n")
fls2444
chisq.test(fls2444)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v24 = rep(cs$v24, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2446<- xtabs(~ v24 + v46, data= csv46)
fcs2446<- cs2446[-c(3),-c(1,10)]
dimnames(fcs2446) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v24 and v46 cs\n")
fcs2446
chisq.test(fcs2446)

lsv46 <- data.frame(
  v24 = rep(ls$v24, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2446 <- xtabs(~v24 + v46, data= lsv46)
fls2446 <- ls2446[-c(3),-c(1,10)]
dimnames(fls2446) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v24 and v46 ls\n")
fls2446
chisq.test(fls2446)

cs2447<- xtabs(~ v24 + v47, data= cs)
fcs2447<- cs2447[-c(3),-c(3)]
dimnames(fcs2447) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v24 and v47 cs\n")
fcs2447
chisq.test(fcs2447)

ls2447 <- xtabs(~v24 + v47, data= ls)
fls2447 <- ls2447[-c(3),-c(3)]
dimnames(fls2447) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v24 and v47 ls\n")
fls2447
chisq.test(fls2447)

#messy again

csv48 <- data.frame(
  v24 = rep(cs$v24, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2448<- xtabs(~ v24 + v48, data= csv48)
fcs2448<- cs2448[-c(3),-c(1,6)]
dimnames(fcs2448) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v24 and v48 cs\n")
fcs2448
chisq.test(fcs2448)

lsv48 <- data.frame(
  v24 = rep(ls$v24, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2448 <- xtabs(~v24 + v48, data= lsv48)
fls2448 <- ls2448[-c(3),-c(1,6)]
dimnames(fls2448) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v24 and v48 ls\n")
fls2448
chisq.test(fls2448)

cs2449<- xtabs(~ v24 + v49, data= cs)
fcs2449<- cs2449[-c(3),-c(3,4)]
dimnames(fcs2449) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v24 and v49 cs\n")
fcs2449
chisq.test(fcs2449)

ls2449 <- xtabs(~v24 + v49, data= ls)
fls2449 <- ls2447[-c(3),-c(3,4)]
dimnames(fls2449) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v24 and v49 ls\n")
fls2449
chisq.test(fls2449)

cs2450<- xtabs(~ v24 + v50, data= cs)
fcs2450<- cs2450[-c(3),-c(3,4,5)]
dimnames(fcs2450) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v24 and v50 cs\n")
fcs2450
chisq.test(fcs2450)

ls2450 <- xtabs(~v24 + v50, data= ls)
fls2450 <- ls2450[-c(3),-c(3,4,5)]
dimnames(fls2450) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v24 and v50 ls\n")
fls2450
chisq.test(fls2450)

cs2451<- xtabs(~ v24 + v51, data= cs)
fcs2451<- cs2451[-c(3),-c(3,4,5)]
dimnames(fcs2451) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v24 and v51 cs\n")
fcs2451
chisq.test(fcs2451)

ls2451 <- xtabs(~v24 + v51, data= ls)
fls2451 <- ls2451[-c(3),-c(3,4,5)]
dimnames(fls2451) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v24 and v51 ls\n")
fls2451
chisq.test(fls2451)

cs2452<- xtabs(~ v24 + v52, data= cs)
fcs2452<- cs2452[-c(3),-c(3,4)]
dimnames(fcs2452) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v24 and v52 cs\n")
fcs2452
chisq.test(fcs2452)

ls2452 <- xtabs(~v24 + v52, data= ls)
fls2452 <- ls2452[-c(3),-c(3,4)]
dimnames(fls2452) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v24 and v52 ls\n")
fls2452
chisq.test(fls2452)


#messy again

csv53 <- data.frame(
  v24 = rep(cs$v24, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs2453<- xtabs(~ v24 + v53, data= csv53)
fcs2453<- cs2453[-c(3),-c(9,10,11)]
dimnames(fcs2453) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v24 and v53 cs\n")
fcs2453
chisq.test(fcs2453)

lsv53 <- data.frame(
  v24 = rep(ls$v24, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls2453 <- xtabs(~v24 + v53, data= lsv53)
fls2453 <- ls2453[-c(3),-c(9,10,11)]
dimnames(fls2453) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v24 and v53 ls\n")
fls2453
chisq.test(fls2453)

#messy again
csv55 <- data.frame(
  v24 = rep(cs$v24, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs2455<- xtabs(~ v24 + v55, data= csv55)
fcs2455<- cs2455[-c(3),-c(1,9)]
dimnames(fcs2455) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v24 and v55 cs\n")
fcs2455
chisq.test(fcs2455)

lsv55 <- data.frame(
  v24 = rep(ls$v24, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls2455 <- xtabs(~v24 + v55, data= lsv55)
fls2455 <- ls2455[-c(3),-c(1,9)]
dimnames(fls2455) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v24 and v55 ls\n")
fls2455
chisq.test(fls2455)

cs2457<- xtabs(~ v24 + v57, data= cs)
fcs2457<- cs2457[-c(3),-c(3,4)]
dimnames(fcs2457) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v24 and v57 cs\n")
fcs2457
chisq.test(fcs2457)

ls2457 <- xtabs(~v24 + v57, data= ls)
fls2457 <- ls2457[-c(3),-c(3,4)]
dimnames(fls2457) <- list("v24 Save Taxes, Cut Air Force Now"= c("Yes","No"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v24 and v57 ls\n")
fls2457
chisq.test(fls2457)

sink()
