sink("report-var17.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs1714<- xtabs(~ v17 + v14, data= cs)
fcs1714<- cs1714[-c(5,6),-5]
# dimnames(fcs1714) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs1714) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v17 and v14 cs\n")
fcs1714
chisq.test(fcs1714)

ls1714 <- xtabs(~v17 + v14, data= ls)
fls1714 <- ls1714[-c(5,6),-5]
dimnames(fls1714) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v17 and v14 ls\n")
fls1714
chisq.test(fls1714)

cs1715<- xtabs(~ v17 + v15, data= cs)
fcs1715<- cs1715[-c(5,6),-5]
dimnames(fcs1715) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v17 and v15 cs\n")
fcs1715
chisq.test(fcs1714)

ls1715 <- xtabs(~v17 + v15, data= ls)
fls1715 <- ls1715[-c(5,6),-5]
dimnames(fls1715) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v17 and v15 ls\n")
fls1715
chisq.test(fls1715)

cs1735<- xtabs(~ v17 + v35, data= cs)
fcs1735<- cs1735[-c(5,6),-3]
dimnames(fcs1735) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v17 and v35 cs\n")
fcs1735
chisq.test(fcs1735)

ls1735 <- xtabs(~v17 + v35, data= ls)
fls1735 <- ls1735[-c(5,6),-3]
dimnames(fls1735) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v17 and v35 ls\n")
fls1735
chisq.test(fls1735)

cs1736<- xtabs(~ v17 + v36, data= cs)
fcs1736<- cs1736[-c(5,6),-3]
dimnames(fcs1736) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v17 and v36 cs\n")
fcs1736
chisq.test(fcs1736)

ls1736 <- xtabs(~v17 + v36, data= ls)
fls1736 <- ls1736[-c(5,6),-3]
dimnames(fls1736) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v17 and v36 ls\n")
fls1736
chisq.test(fls1736)

cs1737<- xtabs(~ v17 + v37, data= cs)
fcs1737<- cs1737[-c(5,6),-3]
dimnames(fcs1737) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v17 and v37 cs\n")
fcs1737
chisq.test(fcs1737)

ls1737 <- xtabs(~v17 + v37, data= ls)
fls1737 <- ls1737[-c(5,6),-3]
dimnames(fls1737) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v17 and v37 ls\n")
fls1737
chisq.test(fls1737)

cs1742<- xtabs(~ v17 + v42, data= cs)
fcs1742<- cs1742[-c(5,6),-6]
dimnames(fcs1742) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v17 and v42 cs\n")
fcs1742
chisq.test(fcs1742)

ls1742 <- xtabs(~v17 + v42, data= ls)
fls1742 <- ls1742[-c(5,6),-6]
dimnames(fls1742) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v17 and v42 ls\n")
fls1742
chisq.test(fls1742)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v17 = rep(cs$v17, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs1743<- xtabs(~ v17 + v43, data = cs_long)

# cs1743<- xtabs(~ v17 + v43a, data= cs)
fcs1743<- cs1743[-c(5,6),-c(1,9,10)]
dimnames(fcs1743) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v17 and v43 cs\n")
fcs1743
chisq.test(fcs1743)

ls_long <- data.frame(
  v17 = rep(ls$v17, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls1743<- xtabs(~ v17 + v43, data = ls_long)
fls1743 <- ls1743[-c(5,6),-c(1,9,10)]
dimnames(fls1743) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v17 and v43 ls\n")
fls1743
chisq.test(fls1743)

cs1744<- xtabs(~ v17 + v44, data= cs)
fcs1744<- cs1744[-c(5,6),-c(4)]
dimnames(fcs1744) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v17 and v44 cs\n")
fcs1744
chisq.test(fcs1744)

ls1744 <- xtabs(~v17 + v44, data= ls)
fls1744 <- ls1744[-c(5,6),-c(4)]
dimnames(fls1744) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v17 and v44 ls\n")
fls1744
chisq.test(fls1744)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v17 = rep(cs$v17, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs1746<- xtabs(~ v17 + v46, data= csv46)
fcs1746<- cs1746[-c(5,6),-c(1,10)]
dimnames(fcs1746) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v17 and v46 cs\n")
fcs1746
chisq.test(fcs1746)

lsv46 <- data.frame(
  v17 = rep(ls$v17, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls1746 <- xtabs(~v17 + v46, data= lsv46)
fls1746 <- ls1746[-c(5,6),-c(1,10)]
dimnames(fls1746) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v17 and v46 ls\n")
fls1746
chisq.test(fls1746)

cs1747<- xtabs(~ v17 + v47, data= cs)
fcs1747<- cs1747[-c(5,6),-3]
dimnames(fcs1747) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v17 and v47 cs\n")
fcs1747
chisq.test(fcs1747)

ls1747 <- xtabs(~v17 + v47, data= ls)
fls1747 <- ls1747[-c(5,6),-3]
dimnames(fls1747) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v17 and v47 ls\n")
fls1747
chisq.test(fls1747)

#messy again

csv48 <- data.frame(
  v17 = rep(cs$v17, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs1748<- xtabs(~ v17 + v48, data= csv48)
fcs1748<- cs1748[-c(5,6),-c(1,6)]
dimnames(fcs1748) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v17 and v48 cs\n")
fcs1748
chisq.test(fcs1748)

lsv48 <- data.frame(
  v17 = rep(ls$v17, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls1748 <- xtabs(~v17 + v48, data= lsv48)
fls1748 <- ls1748[-c(5,6),-c(1,6)]
dimnames(fls1748) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v17 and v48 ls\n")
fls1748
chisq.test(fls1748)

cs1749<- xtabs(~ v17 + v49, data= cs)
fcs1749<- cs1749[-c(5,6),-c(3,4)]
dimnames(fcs1749) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v17 and v49 cs\n")
fcs1749
chisq.test(fcs1749)

ls1749 <- xtabs(~v17 + v49, data= ls)
fls1749 <- ls1747[-c(5,6),-c(3,4)]
dimnames(fls1749) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v17 and v49 ls\n")
fls1749
chisq.test(fls1749)

cs1750<- xtabs(~ v17 + v50, data= cs)
fcs1750<- cs1750[-c(5,6),-c(3,4,5)]
dimnames(fcs1750) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v17 and v50 cs\n")
fcs1750
chisq.test(fcs1750)

ls1750 <- xtabs(~v17 + v50, data= ls)
fls1750 <- ls1750[-c(5,6),-c(3,4,5)]
dimnames(fls1750) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v17 and v50 ls\n")
fls1750
chisq.test(fls1750)

cs1751<- xtabs(~ v17 + v51, data= cs)
fcs1751<- cs1751[-c(5,6),-c(3,4,5)]
dimnames(fcs1751) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v17 and v51 cs\n")
fcs1751
chisq.test(fcs1751)

ls1751 <- xtabs(~v17 + v51, data= ls)
fls1751 <- ls1751[-c(5,6),-c(3,4,5)]
dimnames(fls1751) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v17 and v51 ls\n")
fls1751
chisq.test(fls1751)

cs1752<- xtabs(~ v17 + v52, data= cs)
fcs1752<- cs1752[-c(5,6),-c(3,4)]
dimnames(fcs1752) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v17 and v52 cs\n")
fcs1752
chisq.test(fcs1752)

ls1752 <- xtabs(~v17 + v52, data= ls)
fls1752 <- ls1752[-c(5,6),-c(3,4)]
dimnames(fls1752) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v17 and v52 ls\n")
fls1752
chisq.test(fls1752)


#messy again

csv53 <- data.frame(
  v17 = rep(cs$v17, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs1753<- xtabs(~ v17 + v53, data= csv53)
fcs1753<- cs1753[-c(5,6),-c(9,10,11)]
dimnames(fcs1753) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v17 and v53 cs\n")
fcs1753
chisq.test(fcs1753)

lsv53 <- data.frame(
  v17 = rep(ls$v17, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls1753 <- xtabs(~v17 + v53, data= lsv53)
fls1753 <- ls1753[-c(5,6),-c(9,10,11)]
dimnames(fls1753) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v17 and v53 ls\n")
fls1753
chisq.test(fls1753)

#messy again
csv55 <- data.frame(
  v17 = rep(cs$v17, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs1755<- xtabs(~ v17 + v55, data= csv55)
fcs1755<- cs1755[-c(5,6),-c(1,9)]
dimnames(fcs1755) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v17 and v55 cs\n")
fcs1755
chisq.test(fcs1755)

lsv55 <- data.frame(
  v17 = rep(ls$v17, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls1755 <- xtabs(~v17 + v55, data= lsv55)
fls1755 <- ls1755[-c(5,6),-c(1,9)]
dimnames(fls1755) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v17 and v55 ls\n")
fls1755
chisq.test(fls1755)

cs1757<- xtabs(~ v17 + v57, data= cs)
fcs1757<- cs1757[-c(5,6),-c(3,4)]
dimnames(fcs1757) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v17 and v57 cs\n")
fcs1757
chisq.test(fcs1757)

ls1757 <- xtabs(~v17 + v57, data= ls)
fls1757 <- ls1757[-c(5,6),-c(3,4)]
dimnames(fls1757) <- list("v17 Comm Stopped in Euro w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v17 and v57 ls\n")
fls1757
chisq.test(fls1757)

sink()
