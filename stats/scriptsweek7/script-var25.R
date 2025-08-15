sink("report-var25.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2514<- xtabs(~ v25 + v14, data= cs)
fcs2514<- cs2514[-c(3),-5]
# dimnames(fcs2514) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2514) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v25 and v14 cs\n")
fcs2514
chisq.test(fcs2514)

ls2514 <- xtabs(~v25 + v14, data= ls)
fls2514 <- ls2514[-c(3),-5]
dimnames(fls2514) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v25 and v14 ls\n")
fls2514
chisq.test(fls2514)

cs2515<- xtabs(~ v25 + v15, data= cs)
fcs2515<- cs2515[-c(3),-5]
dimnames(fcs2515) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v25 and v15 cs\n")
fcs2515
chisq.test(fcs2514)

ls2515 <- xtabs(~v25 + v15, data= ls)
fls2515 <- ls2515[-c(3),-5]
dimnames(fls2515) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v25 and v15 ls\n")
fls2515
chisq.test(fls2515)

cs2535<- xtabs(~ v25 + v35, data= cs)
fcs2535<- cs2535[-c(3),-3]
dimnames(fcs2535) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v25 and v35 cs\n")
fcs2535
chisq.test(fcs2535)

ls2535 <- xtabs(~v25 + v35, data= ls)
fls2535 <- ls2535[-c(3),-3]
dimnames(fls2535) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v25 and v35 ls\n")
fls2535
chisq.test(fls2535)

cs2536<- xtabs(~ v25 + v36, data= cs)
fcs2536<- cs2536[-c(3),-3]
dimnames(fcs2536) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v25 and v36 cs\n")
fcs2536
chisq.test(fcs2536)

ls2536 <- xtabs(~v25 + v36, data= ls)
fls2536 <- ls2536[-c(3),-3]
dimnames(fls2536) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v25 and v36 ls\n")
fls2536
chisq.test(fls2536)

cs2537<- xtabs(~ v25 + v37, data= cs)
fcs2537<- cs2537[-c(3),-3]
dimnames(fcs2537) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v25 and v37 cs\n")
fcs2537
chisq.test(fcs2537)

ls2537 <- xtabs(~v25 + v37, data= ls)
fls2537 <- ls2537[-c(3),-3]
dimnames(fls2537) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v25 and v37 ls\n")
fls2537
chisq.test(fls2537)

cs2542<- xtabs(~ v25 + v42, data= cs)
fcs2542<- cs2542[-c(3),-6]
dimnames(fcs2542) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v25 and v42 cs\n")
fcs2542
chisq.test(fcs2542)

ls2542 <- xtabs(~v25 + v42, data= ls)
fls2542 <- ls2542[-c(3),-6]
dimnames(fls2542) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v25 and v42 ls\n")
fls2542
chisq.test(fls2542)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v25 = rep(cs$v25, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2543<- xtabs(~ v25 + v43, data = cs_long)

# cs2543<- xtabs(~ v25 + v43a, data= cs)
fcs2543<- cs2543[-c(3),-c(1,9,10)]
dimnames(fcs2543) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v25 and v43 cs\n")
fcs2543
chisq.test(fcs2543)

ls_long <- data.frame(
  v25 = rep(ls$v25, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2543<- xtabs(~ v25 + v43, data = ls_long)
fls2543 <- ls2543[-c(3),-c(1,9,10)]
dimnames(fls2543) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v25 and v43 ls\n")
fls2543
chisq.test(fls2543)

cs2544<- xtabs(~ v25 + v44, data= cs)
fcs2544<- cs2544[-c(3),-c(4)]
dimnames(fcs2544) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v25 and v44 cs\n")
fcs2544
chisq.test(fcs2544)

ls2544 <- xtabs(~v25 + v44, data= ls)
fls2544 <- ls2544[-c(3),-c(4)]
dimnames(fls2544) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v25 and v44 ls\n")
fls2544
chisq.test(fls2544)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v25 = rep(cs$v25, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2546<- xtabs(~ v25 + v46, data= csv46)
fcs2546<- cs2546[-c(3),-c(1,10)]
dimnames(fcs2546) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v25 and v46 cs\n")
fcs2546
chisq.test(fcs2546)

lsv46 <- data.frame(
  v25 = rep(ls$v25, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2546 <- xtabs(~v25 + v46, data= lsv46)
fls2546 <- ls2546[-c(3),-c(1,10)]
dimnames(fls2546) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v25 and v46 ls\n")
fls2546
chisq.test(fls2546)

cs2547<- xtabs(~ v25 + v47, data= cs)
fcs2547<- cs2547[-c(3),-c(3)]
dimnames(fcs2547) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v25 and v47 cs\n")
fcs2547
chisq.test(fcs2547)

ls2547 <- xtabs(~v25 + v47, data= ls)
fls2547 <- ls2547[-c(3),-c(3)]
dimnames(fls2547) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v25 and v47 ls\n")
fls2547
chisq.test(fls2547)

#messy again

csv48 <- data.frame(
  v25 = rep(cs$v25, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2548<- xtabs(~ v25 + v48, data= csv48)
fcs2548<- cs2548[-c(3),-c(1,6)]
dimnames(fcs2548) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v25 and v48 cs\n")
fcs2548
chisq.test(fcs2548)

lsv48 <- data.frame(
  v25 = rep(ls$v25, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2548 <- xtabs(~v25 + v48, data= lsv48)
fls2548 <- ls2548[-c(3),-c(1,6)]
dimnames(fls2548) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v25 and v48 ls\n")
fls2548
chisq.test(fls2548)

cs2549<- xtabs(~ v25 + v49, data= cs)
fcs2549<- cs2549[-c(3),-c(3,4)]
dimnames(fcs2549) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v25 and v49 cs\n")
fcs2549
chisq.test(fcs2549)

ls2549 <- xtabs(~v25 + v49, data= ls)
fls2549 <- ls2547[-c(3),-c(3,4)]
dimnames(fls2549) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v25 and v49 ls\n")
fls2549
chisq.test(fls2549)

cs2550<- xtabs(~ v25 + v50, data= cs)
fcs2550<- cs2550[-c(3),-c(3,4,5)]
dimnames(fcs2550) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v25 and v50 cs\n")
fcs2550
chisq.test(fcs2550)

ls2550 <- xtabs(~v25 + v50, data= ls)
fls2550 <- ls2550[-c(3),-c(3,4,5)]
dimnames(fls2550) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v25 and v50 ls\n")
fls2550
chisq.test(fls2550)

cs2551<- xtabs(~ v25 + v51, data= cs)
fcs2551<- cs2551[-c(3),-c(3,4,5)]
dimnames(fcs2551) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v25 and v51 cs\n")
fcs2551
chisq.test(fcs2551)

ls2551 <- xtabs(~v25 + v51, data= ls)
fls2551 <- ls2551[-c(3),-c(3,4,5)]
dimnames(fls2551) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v25 and v51 ls\n")
fls2551
chisq.test(fls2551)

cs2552<- xtabs(~ v25 + v52, data= cs)
fcs2552<- cs2552[-c(3),-c(3,4)]
dimnames(fcs2552) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v25 and v52 cs\n")
fcs2552
chisq.test(fcs2552)

ls2552 <- xtabs(~v25 + v52, data= ls)
fls2552 <- ls2552[-c(3),-c(3,4)]
dimnames(fls2552) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v25 and v52 ls\n")
fls2552
chisq.test(fls2552)


#messy again

csv53 <- data.frame(
  v25 = rep(cs$v25, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs2553<- xtabs(~ v25 + v53, data= csv53)
fcs2553<- cs2553[-c(3),-c(9,10,11)]
dimnames(fcs2553) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v25 and v53 cs\n")
fcs2553
chisq.test(fcs2553)

lsv53 <- data.frame(
  v25 = rep(ls$v25, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls2553 <- xtabs(~v25 + v53, data= lsv53)
fls2553 <- ls2553[-c(3),-c(9,10,11)]
dimnames(fls2553) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v25 and v53 ls\n")
fls2553
chisq.test(fls2553)

#messy again
csv55 <- data.frame(
  v25 = rep(cs$v25, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs2555<- xtabs(~ v25 + v55, data= csv55)
fcs2555<- cs2555[-c(3),-c(1,9)]
dimnames(fcs2555) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v25 and v55 cs\n")
fcs2555
chisq.test(fcs2555)

lsv55 <- data.frame(
  v25 = rep(ls$v25, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls2555 <- xtabs(~v25 + v55, data= lsv55)
fls2555 <- ls2555[-c(3),-c(1,9)]
dimnames(fls2555) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v25 and v55 ls\n")
fls2555
chisq.test(fls2555)

cs2557<- xtabs(~ v25 + v57, data= cs)
fcs2557<- cs2557[-c(3),-c(3,4)]
dimnames(fcs2557) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v25 and v57 cs\n")
fcs2557
chisq.test(fcs2557)

ls2557 <- xtabs(~v25 + v57, data= ls)
fls2557 <- ls2557[-c(3),-c(3,4)]
dimnames(fls2557) <- list("v25 Save Taxes, Cut Down ABomb Production"= c("Yes","No"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v25 and v57 ls\n")
fls2557
chisq.test(fls2557)

sink()
