sink("rlog1.txt", split = TRUE)

cs <- readRDS("stouffer_cs.rds")

which(names(cs) =="v58")
# so v58 is indexed at 90

names(cs)[93]
# and v61 is indexed at 93, so now...

summary(cs[, 90:93])

library(haven)

for (var in c("v58", "v59", "v60", "v61")) {
  val <- cs[[var]]
  var_label <- attr(val, "label")  # Get the Stata-style variable label
  lbl <- as_factor(val)            # Apply value labels to get "YES", "NO", etc.
  
  # Filter based on numeric value <= 5
  lbl_filtered <- lbl[val <= 5]
  
  freq <- table(lbl_filtered, useNA = "ifany")
  pct <- prop.table(freq) * 100
  cum_pct <- cumsum(pct)
  
  tab <- data.frame(
    Category = names(freq),
    Frequency = as.vector(freq),
    Percent = round(pct, 2),
    Cumulative = round(cum_pct, 2)
  )
  
  # Print variable label like in Stata
  cat("\n->", var, ":", var_label, "\n")
  print(tab, row.names = FALSE)
}

# now I just need to do the same thing but for v35-v37

library(haven)

for (var in c("v35", "v36", "v37")) {
  val <- cs[[var]]
  var_label <- attr(val, "label")  # Get the Stata-style variable label
  lbl <- as_factor(val)            # Apply value labels to get "YES", "NO", etc.
  
  # Filter based on numeric value <= 5
  lbl_filtered <- lbl[val <= 5]
  
  freq <- table(lbl_filtered, useNA = "ifany")
  pct <- prop.table(freq) * 100
  cum_pct <- cumsum(pct)
  
  tab <- data.frame(
    Category = names(freq),
    Frequency = as.vector(freq),
    Percent = round(pct, 2),
    Cumulative = round(cum_pct, 2)
  )
  
  # Print variable label like in Stata
  cat("\n->", var, ":", var_label, "\n")
  print(tab, row.names = FALSE)
}

# chi squared test attempt

library(haven)

xvars <- c("v35", "v36", "v37", "v59")
yvar <- "v138"

for (xvar in xvars) {
  cat("\n================================================\n")
  cat("Crosstab:", yvar, "vs", xvar, "\n")
  
  # Original numeric values for filtering
  x_raw <- cs[[xvar]]
  y_raw <- cs[[yvar]]
  
  # Labels for display
  x_fact <- as_factor(x_raw)
  y_fact <- as_factor(y_raw)
  
  # Filter to value <= 5 AND drop "DONT KNOW"/"NA"
  keep <- (!is.na(x_raw) & !is.na(y_raw)) & (x_raw <= 5 & y_raw <= 5)
  
  x_use <- x_fact[keep]
  y_use <- y_fact[keep]
  
  # Further filter out specific labels by name
  valid_labels <- !(x_use %in% c("DONT KNOW", "NA") | y_use %in% c("DONT KNOW", "NA"))
  x_use <- x_use[valid_labels]
  y_use <- y_use[valid_labels]
  
  # Drop unused factor levels (critical fix)
  x_use <- droplevels(x_use)
  y_use <- droplevels(y_use)
  
  # Crosstab
  tbl <- table(y_use, x_use)
  
  
  cat("\nContingency table (counts):\n")
  print(tbl)
  
  row_pct <- prop.table(tbl, margin = 1) * 100
  cat("\nRow percentages (%):\n")
  print(round(row_pct, 2))
  
  # Chi-squared test only if table is valid
  if (nrow(tbl) >= 2 && ncol(tbl) >= 2) {
    cat("\nChi-squared test:\n")
    print(chisq.test(tbl))
  } else {
    cat("\nChi-squared test: Not enough valid data.\n")
  }
}

sink()

