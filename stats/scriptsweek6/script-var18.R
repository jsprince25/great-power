sink("report-var18.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs1814<- xtabs(~ v18 + v14, data= cs)
fcs1814<- cs1814[-c(5,6),-5]
# dimnames(fcs1814) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia iff Allies help","Other"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs1814) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v18 and v14 cs\n")
fcs1814
chisq.test(fcs1814)

ls1814 <- xtabs(~v18 + v14, data= ls)
fls1814 <- ls1814[-c(5,6),-5]
dimnames(fls1814) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v18 and v14 ls\n")
fls1814
chisq.test(fls1814)

cs1815<- xtabs(~ v18 + v15, data= cs)
fcs1815<- cs1815[-c(5,6),-5]
dimnames(fcs1815) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v18 and v15 cs\n")
fcs1815
chisq.test(fcs1814)

ls1815 <- xtabs(~v18 + v15, data= ls)
fls1815 <- ls1815[-c(5,6),-5]
dimnames(fls1815) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v18 and v15 ls\n")
fls1815
chisq.test(fls1815)

cs1835<- xtabs(~ v18 + v35, data= cs)
fcs1835<- cs1835[-c(5,6),-3]
dimnames(fcs1835) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v18 and v35 cs\n")
fcs1835
chisq.test(fcs1835)

ls1835 <- xtabs(~v18 + v35, data= ls)
fls1835 <- ls1835[-c(5,6),-3]
dimnames(fls1835) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v18 and v35 ls\n")
fls1835
chisq.test(fls1835)

cs1836<- xtabs(~ v18 + v36, data= cs)
fcs1836<- cs1836[-c(5,6),-3]
dimnames(fcs1836) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v18 and v36 cs\n")
fcs1836
chisq.test(fcs1836)

ls1836 <- xtabs(~v18 + v36, data= ls)
fls1836 <- ls1836[-c(5,6),-3]
dimnames(fls1836) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v18 and v36 ls\n")
fls1836
chisq.test(fls1836)

cs1837<- xtabs(~ v18 + v37, data= cs)
fcs1837<- cs1837[-c(5,6),-3]
dimnames(fcs1837) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v18 and v37 cs\n")
fcs1837
chisq.test(fcs1837)

ls1837 <- xtabs(~v18 + v37, data= ls)
fls1837 <- ls1837[-c(5,6),-3]
dimnames(fls1837) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v18 and v37 ls\n")
fls1837
chisq.test(fls1837)

cs1842<- xtabs(~ v18 + v42, data= cs)
fcs1842<- cs1842[-c(5,6),-6]
dimnames(fcs1842) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v18 and v42 cs\n")
fcs1842
chisq.test(fcs1842)

ls1842 <- xtabs(~v18 + v42, data= ls)
fls1842 <- ls1842[-c(5,6),-6]
dimnames(fls1842) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v18 and v42 ls\n")
fls1842
chisq.test(fls1842)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v18 = rep(cs$v18, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs1843<- xtabs(~ v18 + v43, data = cs_long)

# cs1843<- xtabs(~ v18 + v43a, data= cs)
fcs1843<- cs1843[-c(5,6),-c(1,9,10)]
dimnames(fcs1843) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v18 and v43 cs\n")
fcs1843
chisq.test(fcs1843)

ls_long <- data.frame(
  v18 = rep(ls$v18, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls1843<- xtabs(~ v18 + v43, data = ls_long)
fls1843 <- ls1843[-c(5,6),-c(1,9,10)]
dimnames(fls1843) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v18 and v43 ls\n")
fls1843
chisq.test(fls1843)

cs1844<- xtabs(~ v18 + v44, data= cs)
fcs1844<- cs1844[-c(5,6),-c(4)]
dimnames(fcs1844) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v18 and v44 cs\n")
fcs1844
chisq.test(fcs1844)

ls1844 <- xtabs(~v18 + v44, data= ls)
fls1844 <- ls1844[-c(5,6),-c(4)]
dimnames(fls1844) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v18 and v44 ls\n")
fls1844
chisq.test(fls1844)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v18 = rep(cs$v18, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs1846<- xtabs(~ v18 + v46, data= csv46)
fcs1846<- cs1846[-c(5,6),-c(1,10)]
dimnames(fcs1846) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v18 and v46 cs\n")
fcs1846
chisq.test(fcs1846)

lsv46 <- data.frame(
  v18 = rep(ls$v18, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls1846 <- xtabs(~v18 + v46, data= lsv46)
fls1846 <- ls1846[-c(5,6),-c(1,10)]
dimnames(fls1846) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v18 and v46 ls\n")
fls1846
chisq.test(fls1846)

cs1847<- xtabs(~ v18 + v47, data= cs)
fcs1847<- cs1847[-c(5,6),-3]
dimnames(fcs1847) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v18 and v47 cs\n")
fcs1847
chisq.test(fcs1847)

ls1847 <- xtabs(~v18 + v47, data= ls)
fls1847 <- ls1847[-c(5,6),-3]
dimnames(fls1847) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v18 and v47 ls\n")
fls1847
chisq.test(fls1847)

#messy again

csv48 <- data.frame(
  v18 = rep(cs$v18, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs1848<- xtabs(~ v18 + v48, data= csv48)
fcs1848<- cs1848[-c(5,6),-c(1,6)]
dimnames(fcs1848) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v18 and v48 cs\n")
fcs1848
chisq.test(fcs1848)

lsv48 <- data.frame(
  v18 = rep(ls$v18, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls1848 <- xtabs(~v18 + v48, data= lsv48)
fls1848 <- ls1848[-c(5,6),-c(1,6)]
dimnames(fls1848) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v18 and v48 ls\n")
fls1848
chisq.test(fls1848)

cs1849<- xtabs(~ v18 + v49, data= cs)
fcs1849<- cs1849[-c(5,6),-c(3,4)]
dimnames(fcs1849) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v18 and v49 cs\n")
fcs1849
chisq.test(fcs1849)

ls1849 <- xtabs(~v18 + v49, data= ls)
fls1849 <- ls1847[-c(5,6),-c(3,4)]
dimnames(fls1849) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v18 and v49 ls\n")
fls1849
chisq.test(fls1849)

cs1850<- xtabs(~ v18 + v50, data= cs)
fcs1850<- cs1850[-c(5,6),-c(3,4,5)]
dimnames(fcs1850) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v18 and v50 cs\n")
fcs1850
chisq.test(fcs1850)

ls1850 <- xtabs(~v18 + v50, data= ls)
fls1850 <- ls1850[-c(5,6),-c(3,4,5)]
dimnames(fls1850) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v18 and v50 ls\n")
fls1850
chisq.test(fls1850)

cs1851<- xtabs(~ v18 + v51, data= cs)
fcs1851<- cs1851[-c(5,6),-c(3,4,5)]
dimnames(fcs1851) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v18 and v51 cs\n")
fcs1851
chisq.test(fcs1851)

ls1851 <- xtabs(~v18 + v51, data= ls)
fls1851 <- ls1851[-c(5,6),-c(3,4,5)]
dimnames(fls1851) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v18 and v51 ls\n")
fls1851
chisq.test(fls1851)

cs1852<- xtabs(~ v18 + v52, data= cs)
fcs1852<- cs1852[-c(5,6),-c(3,4)]
dimnames(fcs1852) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v18 and v52 cs\n")
fcs1852
chisq.test(fcs1852)

ls1852 <- xtabs(~v18 + v52, data= ls)
fls1852 <- ls1852[-c(5,6),-c(3,4)]
dimnames(fls1852) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v18 and v52 ls\n")
fls1852
chisq.test(fls1852)


#messy again

csv53 <- data.frame(
  v18 = rep(cs$v18, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs1853<- xtabs(~ v18 + v53, data= csv53)
fcs1853<- cs1853[-c(5,6),-c(9,10,11)]
dimnames(fcs1853) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v18 and v53 cs\n")
fcs1853
chisq.test(fcs1853)

lsv53 <- data.frame(
  v18 = rep(ls$v18, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls1853 <- xtabs(~v18 + v53, data= lsv53)
fls1853 <- ls1853[-c(5,6),-c(9,10,11)]
dimnames(fls1853) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v18 and v53 ls\n")
fls1853
chisq.test(fls1853)

#messy again
csv55 <- data.frame(
  v18 = rep(cs$v18, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs1855<- xtabs(~ v18 + v55, data= csv55)
fcs1855<- cs1855[-c(5,6),-c(1,9)]
dimnames(fcs1855) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v18 and v55 cs\n")
fcs1855
chisq.test(fcs1855)

lsv55 <- data.frame(
  v18 = rep(ls$v18, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls1855 <- xtabs(~v18 + v55, data= lsv55)
fls1855 <- ls1855[-c(5,6),-c(1,9)]
dimnames(fls1855) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v18 and v55 ls\n")
fls1855
chisq.test(fls1855)

cs1857<- xtabs(~ v18 + v57, data= cs)
fcs1857<- cs1857[-c(5,6),-c(3,4)]
dimnames(fcs1857) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v18 and v57 cs\n")
fcs1857
chisq.test(fcs1857)

ls1857 <- xtabs(~v18 + v57, data= ls)
fls1857 <- ls1857[-c(5,6),-c(3,4)]
dimnames(fls1857) <- list("v18 To Save Euro from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v18 and v57 ls\n")
fls1857
chisq.test(fls1857)

sink()
