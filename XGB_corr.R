#----------------------------------------------------------------------------------------------------------
# NOTES: The following code will provide a few options for evaluating variables that might be predictive
# of a company that will ultimately IPO or be acquired
#
# You should start with a dataset that has been summarized down to one row per company, and integrated
# in acquisition and IPO data.  You will also need to address character variables as these techniques will 
# only work on numeric / integer / factor variables.
#----------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
##--CHANGE THIS: IDENTIFY DATA AND TARGET VARIABLE ---------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

#- Assign your data frame to the object 'data'----------------
data <- org_full_data.3

#- Identify your target variable -----------------------------
target_var = "acq_ipo"

#----------------------------------------------------------------------------------------------------------
##--LOAD PACKAGES                         -----------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
packages <- c("dplyr", "stringr", "readr", "lubridate", "xgboost")

for (package in packages) {
  if (!package %in% row.names(installed.packages())) {
    install.packages(package, repos = "https://cran.rstudio.com")}}
rm(packages)

library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(xgboost)

#----------------------------------------------------------------------------------------------------------
##-- BASIC CORRELATION (Pearson)                              ---------------------------------------------
#----------------------------------------------------------------------------------------------------------

#-- REMOVE CHARACTER & DATE VARIABLES STILL REMAINING IN YOUR DATA ----------------------------------------
data.c <- data %>% 
  select(which(!sapply(.,is.character))) %>% 
  select(which(!sapply(.,is.Date)))

#-- CHANGE THIS: REMOVE VARIABLES YOU DON'T WANT TO RUN CORR ON --------
VAR_LIST <- names(data.c)[ - which(names(data.c) == target_var)] #<--TARGET VAR
remove <- c("org_permalink",
            "acquired",
            "ipo")
VAR_LIST <- setdiff(VAR_LIST, remove)

#-- SET UP DF TO POPULATE ----------------------------------------------
col_names <- c("Variable", "Corr", "P.Value", "abs.Corr")
list <- data.frame(matrix(nrow = length(VAR_LIST), ncol = length(col_names)))
names(list) <- col_names
n <- 1

#-- RUN CORRELATION FOR EACH VARIABLE w/TARGET VARIABLE ----------------
for (i in 1:(length(VAR_LIST))) {
  p <- cor.test(data.c[[VAR_LIST[[i]]]], data.c[[target_var]])
  list[n,"Variable"] <- VAR_LIST[[i]]
  list[n,"Corr"] <- p$estimate
  list[n,"P.Value"] <- p$p.value
  list[n,"abs.Corr"] <- abs(p$estimate)
  n <- n + 1
}

#-- SUMMARIZE AND PRINT ------------------------------------------------
list <- list[order(-list$abs.Corr),]
head(list, 50)


#----------------------------------------------------------------------------------------------------------
##-- XGBOOST                              -----------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

#-- DEFINE RESPONSE VECTOR ---------------------------------------------------
response <- data[[target_var]]

#-- THIS REMOVES ANY CHARACTER VARIABLES LEFT IN YTOUR DATA (except org_permalink) -----------------
data.xgb <- data %>% 
  select(which(!sapply(.,is.character)), org_permalink)

#-- CHANGE THIS: REMOVE VARIABLES YOU DON'T WANT TO USE AS PREDICTORS --------
feature.names <- names(data.xgb)[ - which(names(data.xgb) == target_var)] # <--- TARGET VARIABLE
remove <- c("org_permalink",
            "ipo_went_public_on"	,
            "ipo_shares_sold"	,
            "acq_announced_on"	,
            "acquired"	,
            "ipo_opening_share_price_usd"	,
            "ipo_opening_valuation_usd"	,
            "acq_price_usd"	,
            "ipo",
            "acq_type",
            "ipo_stock_exchange_symbol",
            "acq_status",
            "org_rank",
            "org_stock_exchange"
)
feature.names <- setdiff(feature.names, remove)
rm(remove)

#-- RUNS XGBOOST MODEL (you can change the params if you are adventurous) --------------------------
xgb.model <- xgboost(
  data             = data.matrix(data.xgb[,feature.names]),
  label            = response,
  objective        = "binary:logistic",
  nrounds          = 10,
  gamma            = 1,
  max.depth        = 8,
  eta              = .3,
  booster          = "gbtree")

#-- PRINT OUT THE MOST IMPORTANT VARIABLES ----------------------------------------------------------
important <- xgb.importance(feature.names, model = xgb.model)
head(important,50)
