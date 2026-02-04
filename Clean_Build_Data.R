library(data.table)
library(lubridate)

rm(list = ls())

# save the data files to ESG and return based
esg_clean_file <- "ESG_Refinitiv_2024_clean.csv"
crsp_clean_file <- "ESG_CRSP_d_2024_clean.csv"


############################################################################

# let's keep the most important ones
esg_scores <- c("ESGScore","EnvironmentPillarScore",
                "GovernancePillarScore","SocialPillarScore")

CLEAN_DATA <- TRUE # <- run for first time

if(CLEAN_DATA){
  
  ########################
  ######### ESG ##########
  ########################
  
  ESG_file <- "ESG_Refinitiv_2024_full.csv"
  SG <- fread(ESG_file)
  SG$Cusip <- SG$cusip;   SG$cusip <- NULL
  # ADJUST THE CUSIPS TO MERGE WITH CRSP
  SG$Cusip <- substr(SG$Cusip,1,8)
  
  keep_cusips <- unique(SG$Cusip)
  # all cusips follow 8 charachters except one
  keep_cusips <- keep_cusips[nchar(keep_cusips) != 0]
  
  # focus on the same time series
  SG <- SG[SG$Cusip %in% keep_cusips,]
  
  # data has fiscal year and actual value date  
  SG$valuedate <- ceiling_date(SG$valuedate,"y") - 1
  range(SG$valuedate)
  SG$date <- ymd(SG$year*10000 + 101)
  SG$date <- ceiling_date(SG$date,"y") - 1
  SG <- SG[order(SG$Cusip,SG$date,SG$pillar),]
  
  N_sum <- SG[,.N,by = "date"]
  N_sum <- N_sum[order(N_sum$date),]
  range(SG$date)
  
  SG <- SG[,list(Cusip,date,pillar,fieldname,valuescore)]
  
  # let's deal with duplicates and cast the data
  SG$pillar <- NULL
  SG <- SG[,list(valuescore = mean(valuescore,na.rm = TRUE)), by= c("Cusip","date","fieldname") ]
  
  SG_cast <- data.table()
  for (i in 1:length(keep_cusips)) {
    cat("This stock i = ",i,"\n")
    cusip_i <- keep_cusips[i]
    SG_cast_i <- dcast(SG[SG$Cusip %in% cusip_i,], date ~ fieldname, value.var = "valuescore")
    SG_cast_i$Cusip <- cusip_i
    SG_cast <- rbind(SG_cast,SG_cast_i)
  }
  
  
  SG_cast <- SG_cast[order(SG_cast$Cusip,SG_cast$date),]
  SG_cast <- SG_cast[,c("Cusip","date",esg_scores),with = FALSE]
  SG <- SG_cast; rm(SG_cast); gc()
  fwrite(SG,esg_clean_file)
  
  #########################
  ######### CRSP ##########
  #########################
  
  crsp_file <- "CRSP_1960_2023_d.csv" # daily CRSP file
  select.var <- c("PERMNO","date","EXCHCD","SHRCD","RET","PRC","CUSIP","SICCD","SHROUT")
  DT  <- fread(crsp_file,select = select.var)
  
  crsp_cusips <- unique(DT$CUSIP)
  table(nchar(crsp_cusips))
  
  # There seems to be 75% of the ESG data in the CRSP, which has to do with data as well
  mean(keep_cusips %in% crsp_cusips)
  
  DT <- DT[DT$CUSIP %in% keep_cusips,]
  
  # keep common shares
  DT <- DT[DT$SHRCD %in% 10:11,]
  # major exchanges
  # 1	New York Stock Exchange
  # 2	American Stock Exchange
  # 3	The Nasdaq Stock Market(SM)
  DT <- DT[DT$EXCHCD %in% 1:3,]
  DT$RET <- as.numeric(DT$RET)
  
  DT <- DT[!is.na(DT$RET),] # drop missing returns
  
  # drop NAs from price
  DT <- DT[!is.na(DT$PRC),]
  DT$PRC <- abs(DT$PRC)
  DT$MKTCAP <- DT$PRC*DT$SHROUT
  DT <- unique(DT)
  
  ##############
  # check for duplicate returns
  dim(DT)
  DT[ , `:=`( duplicate_N = .N   ) , by= list(CUSIP,date)]
  
  # check for any duplicates - SEEMS AL GOOD!
  table(DT$duplicate_N)
  
  # format date
  DT$date <- ymd(DT$date)
  DT$duplicate_N <- NULL
  
  # adjust SICCD and drop all NAs for SIC
  DT$SICCD <- as.integer(DT$SICCD)
  DT <- DT[!is.na(DT$SICCD),]
  DT$PERMNO <- DT$EXCHCD <- DT$SHRCD <- DT$SHROUT <- DT$PRC <- NULL
  DT <- unique(DT)
  
  
  # The minimum data for the ESG data is 2002-12-31
  EG_min_year <- year(min(SG$date)) 
  keep_years <- (EG_min_year - 5 + 1):year(max(SG$date)) 
  
  # keep the same years for CRSP
  DT <- DT[year(DT$date) %in% keep_years,]
  fwrite(DT,crsp_clean_file)
  gc()
  
}
 

if(!CLEAN_DATA) {
  SG <- fread(esg_clean_file)
  DT <- fread(crsp_clean_file)
  
  # The minimum data for the ESG data is 2002-12-31
  EG_min_year <- year(min(SG$date)) 
  keep_years <- (EG_min_year - 5 + 1):year(max(SG$date)) 
  
}

