library(data.table)
library(lubridate)
library(stargazer)
library(ggplot2)
library(ggcorrplot)
library(RiskPortfolios)
library(moments)
library(plyr)
library(xtable)
library(parallel)
library(plm)
library(panelvar)
library(tikzDevice)
library(gtools)

rm(list = ls())

# LaTeX folder for output

output_dir <- "/ESG_shrink_draft"
tex_dir <- paste(output_dir,"/TeX/Main_2026_01_16",sep = "")
data_dir <- paste(output_dir,"/Data",sep = "")
fig_dir <- paste(tex_dir,"/Figures",sep = "")
tables_dir <- paste(tex_dir,"/Tables",sep = "")

# RUN SPECIAL TESTS/ANALYSIS
run_boot <- FALSE # to run bootstrap
run_factor <-  FALSE # <- Needs to run once, and then it's saved to the hard drive
USE_SR <- FALSE # sensitivity analysis to run for Sharpe portfolio instead of GMVP

# save the data files to ESG and return based
esg_clean_file <- "ESG_Refinitiv_2024_clean.csv" # provided on Google Drive
crsp_clean_file <- "ESG_CRSP_d_2024_clean.csv" # provided on Google Drive

############################################################################

# let's keep the most important ones
esg_scores <- c("ESGScore","EnvironmentPillarScore",
                "GovernancePillarScore","SocialPillarScore")

SG <- fread(esg_clean_file) 
DT <- fread(crsp_clean_file)

# The minimum data for the ESG data is 2002-12-31
EG_min_year <- year(min(SG$date)) 
keep_years <- (EG_min_year - 5 + 1):year(max(SG$date)) 

############################################################################

##################################################
######### FINALIZE STOCK SELECTION DATA ##########

{
  
  # load the data
  DT_choose <- DT
  SG_choose <- SG
  # these are fiscal years. Make sure the data is available on time
  range(SG_choose$date)
  SG_choose$date <- date(SG_choose$date)
  SG_choose$date <- ceiling_date(SG_choose$date + 1,"y")-1 # <---- shift data by on year
  range(SG_choose$date)
  
  # let's number of observations to DT
  DT_choose <- DT_choose[order(DT_choose$CUSIP,DT_choose$date),]
  DT_choose[,N_roll := 1:.N, by = "CUSIP"  ]
  DT_choose$date_y <- ceiling_date(DT_choose$date,"y") - 1
  DT_choose_y <- DT_choose[,.SD[.N], by = c("CUSIP","date_y") ]
  DT_choose_y$date_y <- ymd(DT_choose_y$date_y)
  DT_choose_y$date <- ymd(DT_choose_y$date)
  DT_choose_y$Date_Diff <-  as.numeric(DT_choose_y$date_y - DT_choose_y$date)
  quantile(DT_choose_y$Date_Diff,0.95)
  DT_choose_y <- DT_choose_y[DT_choose_y$Date_Diff <= 2,]
  
  
  
  # let's start with the ESG data first
  esg_dates <- as.character(sort(unique(SG_choose$date)))
  DT_keep_data_roll <- data.table()
  for (date_i in esg_dates) {
    SG_choose_i <- na.omit(SG_choose[SG_choose$date %in% date(date_i),])
    
    DT_choose_y_i <- DT_choose_y[DT_choose_y$date_y %in% date(date_i),]
    DT_choose_y_i[,TOT_MKTCAP := MKTCAP/sum(MKTCAP),by = "date_y"]
    # keep stocks that have at least five years of data by the end of that year
    DT_choose_y_i <- DT_choose_y_i[DT_choose_y_i$N_roll >= 250*5,] 
    # match with the ESG data available for that year
    DT_choose_y_i <- DT_choose_y_i[DT_choose_y_i$CUSIP %in% SG_choose_i$Cusip, ]
    DT_keep_data_roll <- rbind(DT_keep_data_roll,DT_choose_y_i)
  }
  
  # report summary for total stocks and market share
  {
    tot_stocks <- DT_keep_data_roll[,.N,by = "date_y"]
    # plot(tot_stocks$N ~ tot_stocks$date_y)
    tot_market_share <- DT_keep_data_roll[,sum(TOT_MKTCAP), by ="date_y"]
    tot_market_share
    
    X <- merge(tot_stocks,tot_market_share)
    X$date_y <- year(X$date_y)
    X <- print(xtable(X, digits = c(0,0,0,4)),include.rownames = FALSE)
    X <- strsplit(X,"\n")[[1]]
    X <- X[9:30]
    file.i <- paste(tables_dir,"/total_stocks.txt",sep = "")
    cat(X,file = file.i,sep = "\n")
  }
  rm(DT_choose_y)
  
  
  # keep the daily stock returns that correspond to that
  DT_choose_A <- merge(DT_choose,DT_keep_data_roll[,list(CUSIP,date_y),])
  DT_choose_A[,RET_A :=  mean(RET)*252, by = list(CUSIP,date_y) ]
  DT_choose_A[,RV_A :=  sd(RET)*sqrt(252), by = list(CUSIP,date_y) ]
  
  DT_choose_A <- DT_choose_A[,list(CUSIP,date_y,RET_A,RV_A)]
  DT_choose_A$Cusip <- DT_choose_A$CUSIP
  DT_choose_A$date <- DT_choose_A$date_y
  DT_choose_A$CUSIP <- DT_choose_A$date_y <- NULL
  DT_choose_A <- unique(DT_choose_A)
  # let's keep the DT_keep_data_roll for the final panel
  
  DT_keep_data_roll$Cusip <- DT_keep_data_roll$CUSIP
  DT_keep_data_roll$date <- DT_keep_data_roll$date_y
  DT_keep_data_roll <- DT_keep_data_roll[,list(Cusip,date,MKTCAP)]
  
  # final panel
  DT_panel <- merge(DT_choose_A,DT_keep_data_roll)
  DT_panel <- merge(DT_panel,SG_choose)
  
  # make sure to keep year for panel
  DT_panel$year <- year(DT_panel$date)
  # add size as log MKTAP
  DT_panel$size <- log(DT_panel$MKTCAP)
  X <- DT_panel[,c("year","RET_A","RV_A","size",esg_scores),with = FALSE]
  X <- stargazer(X)
  X <- unlist(strsplit(X,"\n"))
  X <- X[11:18]
  file.i <- paste(tables_dir,"/sum_stats.txt",sep = "")
  cat(X,file = file.i, sep = "\n")
  
  # Compute a matrix of correlation p-values
  corr <- round(cor(DT_panel[,-c(1:2)],use = "pairwise"),2)
  # p.values
  p.mat <- cor_pmat(DT_panel[,-c(1:2)])
  
  corr.plot <- ggcorrplot(
    corr, hc.order = TRUE, type = "lower", outline.col = "white",
    p.mat = p.mat
  )
  
  corr.plot <- corr.plot + ggtitle("Correlation Matrix")
  file.i <- paste(fig_dir,"/cor_heat.pdf",sep = "")
  pdf(file.i)
  print(corr.plot)
  dev.off()
  
  
}

fwrite(DT_panel,"DT_Panel.csv") # save final panel

###############################################################################

############################################################
######### EXPLORING MECHANISMS THROUGH VAR #################


RUN_PVAR <- FALSE # <----- portfolio results do not depend on this one

if(RUN_PVAR){
  choose_ret_var <- "RV_A"
  {
    choose_score <- esg_scores[1]
    var_panel <- pvargmm(dependent_vars = c(choose_score,"RV_A"),
                         lags = 1,
                         data = data.frame(DT_panel),
                         panel_identifier=c("Cusip", "year"),
                         steps = c("onestep"),
                         system_instruments = FALSE,
                         max_instr_dependent_vars = 99,
                         max_instr_predet_vars = 99,
                         min_instr_dependent_vars = 2L,
                         min_instr_predet_vars = 1L,
                         collapse = TRUE)
  }
  
  N_steps <- 25
  
  IRF <- girf(var_panel,N_steps,N_steps)
  y_limit <- (lapply(IRF, function(x) range(x[,c(1,2)]) ))
  IRF <- lapply(IRF,function(x) data.frame(Steps = 1:N_steps, x) )
  
  for (irf_i in 1:length(IRF)) {
    file.i <- paste(fig_dir,"/IRF_",irf_i,".pdf",sep = "")
    pdf(file.i)
    ds_plot <- IRF[[irf_i]]
    ds_plot$score <- ds_plot[,choose_score]
    plot(score ~ Steps,data = ds_plot,ylim = y_limit[[irf_i]],type = "l",ylab= "",xlab = "Years")
    points(score~Steps,data = ds_plot,pch = 20)
    lines(RV_A~Steps,data = ds_plot,type = "l",col = 2)
    points(RV_A~Steps,data = ds_plot,pch = 20,col = 2)
    abline(h = 0,lty = 2)
    legend("top",c("Rating","Volatility"), col = 1:2, lwd = 1)
    grid(10)
    dev.off()
  }
  
}



gc()
###############################################################################


####################################
########## PORTFOLIO RULES #########
####################################

# load functions and portfolio rules
{
  
  ################
  ## NOTE <---------------------
  ## COMMENT: when adding models, we need to
  ########### 1. change names(m_list)  
  ########### 2. update indexing in backtest function
  ########### 3. update X extract values for cat function
  m_1 <- list(type = "naive")
  m_2 <- list(type = "lw")
  m_3 <- list(type = "factor")
  m_4 <- list(type = "const")
  m_5 <- list(type = "cor")
  m_6 <- list(type = "oneparm")
  m_7 <- list(type = "diag")
  m_8 <- list(type = "large")
  m_9 <- list(type = "overall")
  m_10 <- list(type = "envrn")
  m_11 <- list(type = "corp")
  m_12 <- list(type = "soc")
  m_13 <- list(type = "overall_full")
  m_14 <- list(type = "envrn_full")
  m_15 <- list(type = "corp_full")
  m_16 <- list(type = "soc_full")
  m_17 <- list(type = "naive")
  m_18 <- list(type = "mktcap")
  
  m_list <- list(m_1,m_2,m_3,
                 m_4,m_5,m_6,
                 m_7,m_8, # Market 1:8
                 m_9,m_10,m_11,m_12, # ESG shrink 9:12
                 m_13,m_14,m_15,m_16, # ESG full 13:16
                 m_17,m_18)
  
  # create indices to keep track
  cutoff1 <- 8
  cutoff2 <- 12
  cutoff3 <- 16
  
  
  
  # for further information, see discussion by LW MATLAB here https://www.mathworks.com/matlabcentral/fileexchange/106240-covshrinkage?s_tid=FX_rc1_behav
  
  names(m_list) <- c("Sample","LW",
                     "Factor","Constant","Cor",
                     "OnePar","Diagonal","Large",
                     "ESG","Envrn","Corp","Social",
                     "ESG_F","Envrn_F","Corp_F","Social_F",
                     "Naive","Market")
  
  
  
  
  GMVP_fun <- function(S_i) {
    S_i_inv <- solve(S_i)
    d <- nrow(S_i)
    v_ones <- rep(1,d)
    w_hat <- S_i_inv%*%v_ones/(sum(S_i_inv))
    return(w_hat)
  }
  
  SRP_fun <- function(S_i,M_i) {
    S_i_inv <- solve(S_i)
    num <-  S_i_inv%*%M_i
    denom <- sum(num)
    w_hat <- num/denom
    return(w_hat)
  }
  
  # performance summary
  my_sum <- function(x) {
    m <- mean(x)
    m_neg <- mean(x[x < 0])
    s <- sd(x)
    s_neg <- sd(x[x < 0])
    SR <- m/s
    Sort <- m/s_neg
    skew <- skewness(x)
    kurt <- kurtosis(x)
    VaR <- mean(x) - quantile(x,0.05)
    # results <- c(m,m_neg,s,s_neg,SR,Sort,skew,kurt,VaR)
    results <- c(m,s,SR,Sort,skew,kurt,VaR)
    
    return(results)
  }
  
  
  
}

#### LET'S ADD OUR LW ADJUSTED FUNCTION TO SHRINK TOWARDS ESG

# the function was translated from MATLAB code of LW 
# for the record, I asked chatgpt to do so, and it nailed it!

diag_shrink_fun <- function(Y,target,shrink_fully = FALSE) {
  n <-nrow(Y)
  sample <- t(Y) %*% Y /n
  
  # estimate the parameter that we call pi in Ledoit and Wolf (2003, JEF)
  Y2 <- Y^2
  sample2 <- t(Y2) %*% Y2 / n
  piMat <- sample2 - sample^2
  pihat <- sum(piMat)
  
  # estimate the parameter that we call gamma in Ledoit and Wolf (2003, JEF)
  gammahat <- norm(sample - target, "F")^2
  
  # diagonal part of the parameter that we call rho 
  rho_diag <- sum(diag(piMat))
  
  # off-diagonal part of the parameter that we call rho 
  rho_off <- 0
  
  # compute shrinkage intensity
  rhohat <- rho_diag + rho_off
  kappahat <- (pihat - rhohat) / gammahat
  shrinkage <- max(0, min(1, kappahat / n))
  
  # compute shrinkage estimator
  if(shrink_fully) 
    shrinkage <- 1
  sigmahat <- shrinkage * target + (1 - shrinkage) * sample
  list(sigmahat,shrinkage)
}


{
  # perspective on ESG volatility
  
  for (i in 1:length(esg_scores)) {
    choose_score <- esg_scores[i]
    
    DT_panel_i <- DT_panel[,c("Cusip","date",choose_score,"RV_A"),with = FALSE] 
    S <- DT_panel_i[,get(choose_score)]
    S[S ==0] <- 0.001
    DT_panel_i$esg_vol <- log(1/S)
    G <- 10
    DT_panel_i[,group := dplyr::ntile(esg_vol,G), by = "date"]
    DT_panel_i <- DT_panel_i[,lapply(.SD,mean), by  = "group",.SDcols = c("esg_vol","RV_A")]
    ds_vol <- data.frame(DT_panel_i)
    ds_vol$stock_vol <- ds_vol$RV_A
    ds_vol$RV_A <- NULL
    ds_vol <- ds_vol[order(ds_vol$group),]
    
    ds_vol$y_hat <- predict(loess(ds_vol$stock_vol ~ ds_vol$esg_vol,span = 1),ds_vol$esg_vol)
    
    file.i <- paste(fig_dir,"/stock_vol_",choose_score,".pdf",sep = "")
    pdf(file.i)
    plot(stock_vol ~ esg_vol, data = ds_vol,lwd = 2,ylab = "Stock Volatility",xlab = "ESG Penalty"  )
    points(stock_vol ~ esg_vol, data = ds_vol,pch = 20,col =2)
    lines(y_hat ~ esg_vol, data = ds_vol,lty = 2)
    grid(10)
    dev.off()
    
  }
  
}



# report additional summary stats
DT_panel[,esg_scores,with = FALSE]



################################################################################3

####################################
##### BEGINNING OF EXPERIMENT ######

final_years <- sort(unique(year(DT_panel$date)))
# keep years fixed
T_years <- 5
back_test_years <- (final_years[1] - T_years):(max(final_years))
TC <- 20/(100^2) # 20 bps in line with KWZ

# to avoid repetitions, make sure we have a year for DT_choose
DT_choose$year <- year(DT_choose$date)

# put altogether in a single function
main_backtest_function <- function(seed = 1,N_sample = 200,TC) {
  
  main_results <- data.frame()
  
  for (model_index in 1:length(m_list)) {
    cat("-----> THIS IS MODEL ",model_index,"\n")
    
    m_i <- m_list[[model_index]]
    
    W_mat <- W_mat_next <- data.frame(); 
    port_RET_next_mat <- c()
    OOS_ESG_scores_mat <- data.frame()    
    for (y_i in final_years[1]:final_years[length(final_years)-1]) {
      
      {
        cat("This is year ",y_i,"\n")
        
        # let's pull the data from the final panel
        train_years <- (y_i-T_years+1):y_i
        sub_DT <- DT_panel[DT_panel$year %in% y_i,]
        select_cusips <- sub_DT$Cusip
        
        if(seed == 0) {
          N_sample <- min(1000,length(select_cusips))
          sample_cusips <- sort(sample(select_cusips,N_sample))
        }
        
        
        {
          set.seed(seed)
          sample_cusips <- sort(sample(select_cusips,N_sample))
        }
        
        # now that we identified the cusips for that year, let's cast the returns and ESG scores
        next_year <- y_i+1
        
        # keep the training and next year's for computational efficiency
        RET_sample <- DT_choose[DT_choose$year %in% c(train_years,next_year),]
        RET_sample <- RET_sample[RET_sample$CUSIP %in% sample_cusips,]
        RET_sample <- dcast.data.table(RET_sample,date~CUSIP, value.var = "RET")
        
        # let's do the same for ESG
        ESG_sample <- DT_panel[DT_panel$year %in% c(y_i,next_year),]
        ESG_sample <- ESG_sample[ESG_sample$Cusip %in% sample_cusips,]
        
        ESG_now <- ESG_sample[ESG_sample$year %in% y_i,]
        ESG_next <- ESG_sample[ESG_sample$year %in% next_year,]
        
        
        R_sub <- RET_sample[year(RET_sample$date) %in% train_years,]
        R_sub <- data.frame(R_sub[,-1]) # drop the date
        sum_na <- apply(R_sub,2,function(x) sum(is.na(x)) )
        
        # there were some cases with few variables are missing since we asked for 1250 
        R_sub <- R_sub[,sum_na <= 100]
        R_sub <- na.omit(as.matrix(R_sub))
        N <- ncol(R_sub)
        
        SG_list_y_i <- SG_list_y_next <- list()
        for(score_i in esg_scores ) {
          ESG_now_i <- dcast.data.table(ESG_now,date~Cusip, value.var = score_i)
          
          S <- as.numeric(ESG_now_i[,-1])
          names(S) <- names(data.frame(ESG_now_i))[-1]
          S <- S[colnames(R_sub)]
          S[S == 0] <- 0.001
          SG_list_i <- solve(diag(S))
          SG_list_y_i <- c(SG_list_y_i,list(SG_list_i))
        }
        
        names(SG_list_y_i) <- esg_scores
        
        
        # get the MKTCAP for market portfolio
        MKTCAP_sample <- DT_panel[DT_panel$year %in% y_i,]
        MKTCAP_sample <- MKTCAP_sample[MKTCAP_sample$Cusip %in% sample_cusips,]
        MKTCAP_sample <- dcast.data.table(MKTCAP_sample,date~Cusip, value.var = "MKTCAP")
        
        
        
        
        if(model_index <= cutoff1) {
          S_i <- covEstimation(R_sub,control = m_i)
          if(!USE_SR) {
            w_i <- GMVP_fun(S_i)
          }
          
          if(USE_SR) {
            M_i <- apply(R_sub,2,mean)
            w_i <- SRP_fun(S_i,M_i)
          }
          
        }
        
        if(model_index %in% (cutoff1 + 1):cutoff2) {
          S_sample <- diag(var(R_sub)) 
          target <- SG_list_y_i[[model_index - cutoff1]]
          # normalize the values so it has same sd as sample
          # target <- target*sd(S_sample)/sd(target)
          # LW 2004 suggest using mean
          target <- target*mean(S_sample)/mean(as.numeric(diag(target)))
          S_i <- diag_shrink_fun(R_sub,target)[[1]]
          
          if(!USE_SR) {
            w_i <- GMVP_fun(S_i)
          }
          
          if(USE_SR) {
            M_i <- apply(R_sub,2,mean)
            w_i <- SRP_fun(S_i,M_i)
          }
          
        }
        
        if(model_index %in% (cutoff2 + 1):cutoff3) {
          S_sample <- diag(var(R_sub))
          target <- SG_list_y_i[[model_index - cutoff2]] 
          # do the same as above
          target <- target*mean(S_sample)/mean(as.numeric(diag(target)))
          S_i <- diag_shrink_fun(R_sub,target,shrink_fully = TRUE)[[1]]
          
          if(!USE_SR) {
            w_i <- GMVP_fun(S_i)
          }
          
          if(USE_SR) {
            M_i <- apply(R_sub,2,mean)
            w_i <- SRP_fun(S_i,M_i)
          }
          
        }
        
        # for naive portfolio
        if(model_index == (cutoff3+1))
          w_i <- rep(1/N,N)
        
        # for market portfolio
        if(model_index == (cutoff3+2)) {
          w_i <- data.frame(MKTCAP_sample[,-1])
          w_i <- w_i[,colnames(R_sub)]
          w_i <- as.numeric(w_i)
          w_i <- w_i/sum(w_i)
        }
        
        # make w_i is matrix for merging
        w_i <- as.numeric(w_i)
        names(w_i) <-  colnames(R_sub)
        W_mat <- smartbind(W_mat,t(w_i))
        
        # let's look at the next period return
        RET_next <- RET_sample[year(RET_sample$date) %in% next_year,]
        # adjust in case NA were dropped
        RET_next[is.na(RET_next)] <- 0
        RET_next <- data.frame(RET_next[,-1])
        RET_next <- RET_next[,colnames(R_sub)]
        # put as matrix
        RET_next <- as.matrix(RET_next)
        
        # keep track of changes in positions
        w_next <- apply(RET_next,2,function(x) prod(1+x)  )*as.numeric(w_i)
        
        # what is the value of the portfolio over the next year
        port_RET_next <- sum(w_next) - 1
        port_RET_next_mat <- c(port_RET_next_mat,port_RET_next)
        # normalize the next portfolio weight is it sums to 1
        w_next <- w_next/sum(w_next)
        W_mat_next <- smartbind(W_mat_next,t(w_next))
        
        
        OOS_ESG_scores <- c()
        for(score_i in esg_scores ) {
          
          ESG_next_i <- data.frame(dcast.data.table(ESG_next,date~Cusip, value.var = score_i)[,-1])
          ESG_next_i2 <- data.frame(Cusip = names(ESG_next_i),Value = as.numeric(ESG_next_i))
          w_i_ds <- data.frame(Cusip = names(w_i),Weights = w_i)
          w_i_ds[is.na(w_i_ds)] <- 0
          w_i_ds <- merge(w_i_ds,ESG_next_i2,all = TRUE)
          OOS_ESG_score_i <- sum(w_i_ds$Weights*w_i_ds$Value,na.rm = TRUE)
          OOS_ESG_scores <-  c(OOS_ESG_scores,OOS_ESG_score_i)
        }
        
        
        
        OOS_ESG_scores_mat <- rbind(OOS_ESG_scores_mat,OOS_ESG_scores)
      }
      
    }
    
    colnames(OOS_ESG_scores_mat) <- esg_scores
    ESG_out <- apply(OOS_ESG_scores_mat,2,mean)
    
    
    W1 <- data.table(W_mat_next)
    W0 <- data.table(W_mat)
    W1[is.na(W1)] <- 0
    W0[is.na(W0)] <- 0
    
    TO <- W1 - W0
    TO <- apply(TO,1,function(x) sum(abs(x)) )
    port_RET <- port_RET_next_mat
    
    # let's adjust for TO and TC
    port_RET <- port_RET - TC*TO # last position is 100% TO
    sum_perf <- my_sum(port_RET)
    # names(sum_perf) <- c("Mean","Mean_Neg","Stdev","Stdev_Neg",
    #                      "Sharpe","Sortino", 
    #                      "Skewness","Kurtosis","VaR")
    names(sum_perf) <- c("Mean","Stdev",
                         "Sharpe","Sortino", 
                         "Skewness","Kurtosis","VaR")
    # performance_summary
    sum_perf <- c(Model = model_index,TC = TC,sum_perf,TO = mean(TO) )
    
    
    
    sum_perf <- c(sum_perf,ESG_out)
    
    main_results <- rbind(main_results,t(sum_perf))
  }
  
  # adjust the names to reflect so
  main_results$Model <- mapvalues(main_results$Model,1:length(m_list), names(m_list) )
  return(main_results)
}

# For this experiment, N_sample does not matter
run_baseline <- TRUE
file.i.full <- paste(data_dir,"/main_results_sample_list_full.RDS",sep = "")
if(USE_SR) {
  file.i.full <- paste(data_dir,"/main_results_sample_list_full_SR.RDS",sep = "")
} 

if(run_baseline){
  main_results_full <- main_backtest_function(seed = 0,N_sample = 200,TC)
  saveRDS(main_results_full,file.i.full)
}
if(!run_baseline) {
  main_results_full <- readRDS(file.i.full)
}


# add average results
main_results_full1 <- data.table(main_results_full[main_results_full$Model%in% names(m_list)[1:cutoff1],])
main_results_full2 <- data.table(main_results_full[main_results_full$Model%in% names(m_list)[(cutoff1 + 1):cutoff2],])
main_results_full3 <- data.table(main_results_full[main_results_full$Model%in% names(m_list)[(cutoff2 + 1):cutoff3],])

main_results_full1$Model <- "Agg_Return"
main_results_full2$Model <- "Agg_ESG"
main_results_full3$Model <- "Agg_ESG_F"

main_results_full1 <- main_results_full1[,lapply(.SD,mean), by = "Model"]
main_results_full2 <- main_results_full2[,lapply(.SD,mean), by = "Model"]
main_results_full3 <- main_results_full3[,lapply(.SD,mean), by = "Model"]

main_results_full <- rbind(main_results_full,main_results_full1,main_results_full2,main_results_full3)
# summarize results
{
  X <- print(xtable(main_results_full,digits = 3),include.rownames = FALSE)
  X <- strsplit(X,"\n")[[1]]
  X1 <- X[9:(9+cutoff1 - 1)]
  X2 <- X[(9+cutoff1):(9 + cutoff3 - 1) ]
  X3 <- X[(9 + cutoff3):(9 + cutoff3 + 2 - 1) ]
  X4 <- X[(9 + cutoff3 + 2):(9 + cutoff3 + 2 + 2) ]
  
  file.1 <- paste(tables_dir,"/table_full_ret.txt",sep = "")
  file.2 <- paste(tables_dir,"/table_full_esg.txt",sep = "")
  file.3 <- paste(tables_dir,"/table_full_other.txt",sep = "")
  file.4 <- paste(tables_dir,"/table_full_median.txt",sep = "")
  
  if(USE_SR) {
    file.1 <- paste(tables_dir,"/table_full_ret_SR.txt",sep = "")
    file.2 <- paste(tables_dir,"/table_full_esg_SR.txt",sep = "")
    file.3 <- paste(tables_dir,"/table_full_other_SR.txt",sep = "")
    file.4 <- paste(tables_dir,"/table_full_median_SR.txt",sep = "")
    
  }
  
  
  cat(X1,file = file.1, sep = "\n")
  cat(X2,file = file.2, sep = "\n")
  cat(X3,file = file.3, sep = "\n")
  cat(X4,file = file.4, sep = "\n")
  
  
  
}


################################################################################



#####################################
########## BOOTSTRAP ################


# The boot only runs on the stock data and not the Industry by design of the above loops
file.i.50 <- paste(data_dir,"/main_results_sample_list_50.RDS",sep = "")
file.i.100 <- paste(data_dir,"/main_results_sample_list_100.RDS",sep = "")
file.i.200 <- paste(data_dir,"/main_results_sample_list_200.RDS",sep = "")

if(USE_SR) {
  file.i.50 <- paste(data_dir,"/main_results_sample_list_50_SR.RDS",sep = "")
  file.i.100 <- paste(data_dir,"/main_results_sample_list_100_SR.RDS",sep = "")
  file.i.200 <- paste(data_dir,"/main_results_sample_list_200_SR.RDS",sep = "")
}


if(run_boot) {
  cat("Running boot 50 \n")
  main_results_sample_list_50 <- mclapply(1:1000, function(s)
    main_backtest_function(seed = s,N_sample = 50,TC),mc.cores = 20)
  saveRDS(main_results_sample_list_50,file.i.50)
  cat("Running boot 100 \n")
  main_results_sample_list_100 <- mclapply(1:1000, function(s)
    main_backtest_function(seed = s,N_sample = 100,TC),mc.cores = 20)
  saveRDS(main_results_sample_list_100,file.i.100)
  cat("Running boot 200 \n")
  main_results_sample_list_200 <- mclapply(1:1000, function(s)
    main_backtest_function(seed = s,N_sample = 200,TC),mc.cores = 20)
  saveRDS(main_results_sample_list_200,file.i.200)
}

if(!run_boot) {
  main_results_sample_list_50 <- readRDS(file.i.50)
  main_results_sample_list_100 <- readRDS(file.i.100)
  main_results_sample_list_200 <- readRDS(file.i.200)
}


for(boot_i in c(50,100,200)){
  # summarize results as we did before
  
  if(boot_i == 50)
    main_results_sample_DT <- rbindlist(main_results_sample_list_50)
  
  if(boot_i == 100)
    main_results_sample_DT <- rbindlist(main_results_sample_list_100)
  
  if(boot_i == 200)
    main_results_sample_DT <- rbindlist(main_results_sample_list_200)
  
  
  main_results_sample_DT <- main_results_sample_DT[,lapply(.SD,mean,na.rm = TRUE), by ="Model" ]
  
  # add average results
  main_results_full1 <- data.table(main_results_sample_DT[main_results_sample_DT$Model%in% names(m_list)[1:cutoff1],])
  main_results_full2 <- data.table(main_results_sample_DT[main_results_sample_DT$Model%in% names(m_list)[(cutoff1+1):cutoff2],])
  main_results_full3 <- data.table(main_results_sample_DT[main_results_sample_DT$Model%in% names(m_list)[(cutoff2 + 1):cutoff3],])
  
  main_results_full1$Model <- "Agg_Return"
  main_results_full2$Model <- "Agg_ESG"
  main_results_full3$Model <- "Agg_ESG_F"
  
  main_results_full1 <- main_results_full1[,lapply(.SD,mean), by = "Model"]
  main_results_full2 <- main_results_full2[,lapply(.SD,mean), by = "Model"]
  main_results_full3 <- main_results_full3[,lapply(.SD,mean), by = "Model"]
  
  main_results_sample_DT <- rbind(main_results_sample_DT,main_results_full1,main_results_full2,main_results_full3)
  # summarize results
  
  X <- print(xtable(main_results_sample_DT,digits = 3),include.rownames = FALSE)
  X <- strsplit(X,"\n")[[1]]
  X1 <- X[9:(9+cutoff1 - 1)]
  X2 <- X[(9+cutoff1):(9 + cutoff3 - 1) ]
  X3 <- X[(9 + cutoff3):(9 + cutoff3 + 2 - 1) ]
  X4 <- X[(9 + cutoff3 + 2):(9 + cutoff3 + 2 + 2) ]
  
  file.1 <- paste(tables_dir,"/table_boot_ret_",boot_i,".txt",sep = "")
  file.2 <- paste(tables_dir,"/table_boot_esg_",boot_i,".txt",sep = "")
  file.3 <- paste(tables_dir,"/table_boot_other_",boot_i,".txt",sep = "")
  file.4 <- paste(tables_dir,"/table_boot_full_median_",boot_i,".txt",sep = "")
  
  if(USE_SR) {
    file.1 <- paste(tables_dir,"/table_boot_ret_SR_",boot_i,".txt",sep = "")
    file.2 <- paste(tables_dir,"/table_boot_esg_SR_",boot_i,".txt",sep = "")
    file.3 <- paste(tables_dir,"/table_boot_other_SR_",boot_i,".txt",sep = "")
    file.4 <- paste(tables_dir,"/table_boot_full_median_SR_",boot_i,".txt",sep = "")
  }
  
  cat(X1,file = file.1, sep = "\n")
  cat(X2,file = file.2, sep = "\n")
  cat(X3,file = file.3, sep = "\n")
  cat(X4,file = file.4, sep = "\n")
  
}


###################################################
########## STATISITICAL SIGNIFICANCE ##############


for(boot_i in c(50,100,200)){
  
  if(boot_i == 50)
    main_results_sample_DT <- rbindlist(main_results_sample_list_50)
  
  if(boot_i == 100)
    main_results_sample_DT <- rbindlist(main_results_sample_list_100)
  
  if(boot_i == 200)
    main_results_sample_DT <- rbindlist(main_results_sample_list_200)
  
  
  main_results_sample_DT$Experiment <- sort(rep(1:1000,uniqueN(main_results_sample_DT$Model)))
  
  rules_list <- c(list(names(m_list)[(cutoff1+1):cutoff2]),
                  list(names(m_list)[(cutoff2+1):cutoff3]),
                  list("Naive"),list("Market"))
  
  DIFF_mat <- data.frame()
  for (i in 1:length(rules_list)) {
    
    Z1 <- main_results_sample_DT[main_results_sample_DT$Model %in% names(m_list)[1:cutoff1],]
    Z1$Model <- NULL
    Z1 <- Z1[,lapply(.SD,mean), by = "Experiment"]
    Z1$Experiment <- NULL
    Z2 <- main_results_sample_DT[main_results_sample_DT$Model %in% rules_list[[i]],]
    
    Z2$Model <- NULL
    Z2 <- Z2[,lapply(.SD,mean), by = "Experiment"]
    Z2$Experiment <- NULL
    Z1 <- as.matrix(Z1)
    Z2 <- as.matrix(Z2)
    
    Z12 <- Z1 - Z2
    Z_labels <- colnames(Z12)
    # function to get the significance based on boot p. values
    get.asterisk <- function(pv) {
      star <- rep("",length(pv))
      star[pv < 0.1] <- "*"
      star[pv < 0.05] <- "**"
      star[pv < 0.01] <- "***"
      return(star)
    }
    
    # Calculate the mean difference for each metric
    mean_diff <- apply(Z12, 2,function(x) mean(x,na.rm=TRUE))
    Z12 <- apply(Z12,2,function(x) x -mean(x,na.rm = TRUE) )
    
    # Compute the empirical p-values for each metric using the bootstrapped data in Z12
    p_values <- sapply(1:ncol(Z12), function(i) 2*(1 - ecdf(Z12[,i])(abs(mean_diff[i]))))
    
    mean_diff <- sprintf("%.3f", round(mean_diff,4))
    mean_diff <- paste(mean_diff,get.asterisk(p_values),sep = "")
    DIFF_mat <- rbind(DIFF_mat,matrix(mean_diff,1))
  }
  
  names(DIFF_mat) <- Z_labels
  rownames(DIFF_mat) <- c("ESG","ESG_F","Naive","Market") 
  X <- print(xtable(DIFF_mat))
  X <- unlist(strsplit(X,"\n"))
  X <- X[9:12]
  
  file.i <- paste(tables_dir,"/table_boot_pv_",boot_i,".txt",sep = "")
  
  if(USE_SR) {
    file.i <- paste(tables_dir,"/table_boot_pv_SR_",boot_i,".txt",sep = "")
  }
  
  cat(X,file = file.i,sep = "\n")
  
}


# Repeat the above for the LW models

for(boot_i in c(50,100,200)){
  
  if(boot_i == 50)
    main_results_sample_DT <- rbindlist(main_results_sample_list_50)
  
  if(boot_i == 100)
    main_results_sample_DT <- rbindlist(main_results_sample_list_100)
  
  if(boot_i == 200)
    main_results_sample_DT <- rbindlist(main_results_sample_list_200)
  
  
  main_results_sample_DT$Experiment <- sort(rep(1:1000,uniqueN(main_results_sample_DT$Model)))
  
  rules_list <- c(list(names(m_list)[(cutoff1+1):cutoff2]),
                  list(names(m_list)[(cutoff2+1):cutoff3]),
                  list("Naive"),list("Market"))
  
  DIFF_mat <- data.frame()
  for (i in 1:length(rules_list)) {
    
    Z1 <- main_results_sample_DT[main_results_sample_DT$Model %in% c("Factor"),]
    Z1$Model <- NULL
    Z1 <- Z1[,lapply(.SD,mean), by = "Experiment"]
    Z1$Experiment <- NULL
    Z2 <- main_results_sample_DT[main_results_sample_DT$Model %in% rules_list[[i]],]
    
    Z2$Model <- NULL
    Z2 <- Z2[,lapply(.SD,mean), by = "Experiment"]
    Z2$Experiment <- NULL
    Z1 <- as.matrix(Z1)
    Z2 <- as.matrix(Z2)
    
    Z12 <- Z1 - Z2
    Z_labels <- colnames(Z12)
    # function to get the significance based on boot p. values
    get.asterisk <- function(pv) {
      star <- rep("",length(pv))
      star[pv < 0.1] <- "*"
      star[pv < 0.05] <- "**"
      star[pv < 0.01] <- "***"
      return(star)
    }
    
    # Calculate the mean difference for each metric
    mean_diff <- apply(Z12, 2,function(x) mean(x,na.rm=TRUE))
    Z12 <- apply(Z12,2,function(x) x -mean(x,na.rm = TRUE) )
    
    # Compute the empirical p-values for each metric using the bootstrapped data in Z12
    p_values <- sapply(1:ncol(Z12), function(i) 2*(1 - ecdf(Z12[,i])(abs(mean_diff[i]))))
    
    mean_diff <- sprintf("%.3f", round(mean_diff,4))
    mean_diff <- paste(mean_diff,get.asterisk(p_values),sep = "")
    DIFF_mat <- rbind(DIFF_mat,matrix(mean_diff,1))
  }
  
  names(DIFF_mat) <- Z_labels
  rownames(DIFF_mat) <- c("ESG","ESG_F","Naive","Market") 
  X <- print(xtable(DIFF_mat))
  X <- unlist(strsplit(X,"\n"))
  X <- X[9:12]
  
  file.i <- paste(tables_dir,"/table_boot_pv_LW_",boot_i,".txt",sep = "")
  if(USE_SR) {
    file.i <- paste(tables_dir,"/table_boot_pv_LW_SR_",boot_i,".txt",sep = "")
  }
  cat(X,file = file.i,sep = "\n")
  
}

################################################################################################


#################################
##### PANEL REGRESSION ##########
#################################


for (run in 0:2) {
  
  main_results_sample_DT <- rbindlist(main_results_sample_list_200)
  # add an index for the experiment
  main_results_sample_DT$Experiment <- sort(rep(1:1000,uniqueN(main_results_sample_DT$Model)))
  flag_ESG_models <- names(m_list)[(cutoff1+1):cutoff3]
  
  if(run == 1) {
    main_results_sample_DT <- main_results_sample_DT[!main_results_sample_DT$Model %in% flag_ESG_models,]
  }
  
  if(run == 2) {
    main_results_sample_DT <- main_results_sample_DT[main_results_sample_DT$Model %in% flag_ESG_models,]
  }
  main_results_sample_DT <- data.frame(main_results_sample_DT)
  
  for(score_i in esg_scores){
    plm1 <- plm(Mean~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm2 <- plm(Stdev~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm3 <- plm(Sharpe~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm4 <- plm(Sortino~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm5 <- plm(VaR~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm6 <- plm(TO~get(score_i), index = c("Experiment","Model"),model = "within", data = main_results_sample_DT)
    plm_list <- list(plm1,plm2,plm3,plm4,plm5,plm6)
    X <- stargazer(plm_list,covariate.labels = score_i)
    X <- unlist(strsplit(X,"\n"))
    X <- X[c(14,15,18,19)]
    file.i <- paste(tables_dir,"/panel_",score_i,"_",run,".txt",sep = "")
    if(USE_SR) {
      file.i <- paste(tables_dir,"/panel_SR_",score_i,"_",run,".txt",sep = "")
    }
    cat(X,file = file.i,sep = "\n")
    
  }
  
}

