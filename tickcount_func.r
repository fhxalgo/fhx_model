
load_tick_data <- function(sym_list) {
  for (n in 1:length(sym_list)) {
    tick_file <- paste(DATA_DIR,"/",sym_list[n],"_",date_str,"_tick.csv",sep="")
    if (!file.exists(tick_file)) {
    cat("cannot find tick data file : ", tick_file, "...skipping it.\n")  
      cat("cannot find tick data file for symbol: ", sym_list[n], "...skipping it.\n")  
      # remove it from sym_list
      next
    }
  
    input_data <<- read.csv(tick_file)
    cat("loading tick data from file: ",tick_file,", size: ",nrow(input_data),"\n")
    
  #  if (nrow(input_data) < 23401)
  #    next ;
    
    # update global var  
    sym_data[[n]] <<- (input_data$Bid + input_data$Ask) * 0.5
    sym_trading_list <<- c(sym_trading_list, sym_list[n])
    
  }
}

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

tick_counter <- function(cvec) {
    score <- length(cvec[cvec >0]) - length(cvec[cvec <0])
    # Note: normalize the score by dividing it by length(cvex)
    #score <- (length(cvec[cvec >0]) - length(cvec[cvec <0]))/length(cvec)
}

ma <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}

# use apply function
update_sw <- function(newbw, bwnum)
{
  for(j in 1:ncol(newbw)) {
    bwdat <- newbw[,j]
    rw <- as.numeric(bwdat)
    
    if (bwnum == 1) {
      chopChunk[[j]] <<- rw
    }
    else if (bwnum <= (sw/bw)) {
      chopChunk[[j]] <<- c(chopChunk[[j]], rw)  # append to existing basicwin
    }
    else {
      # override the oldest bw : do a shift update: should use a shift function
      chopChunk[[j]] <<- c(chopChunk[[j]][(bw+1):sw], rw)
    }
  }
  
  #return(chopChunk)
}

trading_rules1 <- function() {
  # return an order obj
  
}

gen_pos_list <- function() {
  # method 1: if 3 consecutive buy signals are formed, long index
  #           if 3 consecutive sell signals are formed, short index 
  # EXIT: close when 3 or 5 zeros are formed 
  if (  !is.null(signalList[[bwnum]]) 
    &&  !is.null(signalList[[bwnum-2]])
    &&  !is.null(signalList[[bwnum-3]])
    && signalList[[bwnum]]$t == "buy" 
    && signalList[[bwnum-1]]$t == "buy" 
    && signalList[[bwnum-1]]$t == "buy" ) {
  
    ord <- list(bwnum=bwnum, pos="long")
    posList[[bwnum]] <<- ord  
  }

  if (  !is.null(signalList[[bwnum]]) 
    &&  !is.null(signalList[[bwnum-2]])
    &&  !is.null(signalList[[bwnum-3]])
    && signalList[[bwnum]]$t == "sell" 
    && signalList[[bwnum-1]]$t == "sell" 
    && signalList[[bwnum-1]]$t == "sell" ) {
  
    ord <- list(bwnum=bwnum, pos="shortsell")
    posList[[bwnum]] <<- ord  
  }
 
}

gen_close_list <- function() {
  # 1. close based on "zero" signal
  # 2. close based on eod trading
   
}

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

process_bw_data <- function(bwdat, bwnum) {
    cat("\n++++++BEGIN BASIC WINDOW [",bwnum,"] ++++++++++++++++++++++\n")

    # get new basic window data for all stream
    #bwdat <- tickstream[readpointer:(bwnum*bw*n_stream), ]
    # use time as index: start at 09:30, offset 120 seconds.

    
    cat("processing bwnum: ",bwnum, " \n")
    cat(" time begin: ", rownames(bwdat)[1], "\n")
    cat(" time   end: ", rownames(bwdat)[nrow(bwdat)], " \n")
    idx_time <<- c(idx_time, rownames(bwdat)[nrow(bwdat)])

    # update raw data for sw
    #chopChunk <- update_sw(chopChunk, bwdat, bwnum)
    update_sw(bwdat, bwnum)

    # now build each basic win tick count stats: use apply func
    logret <- diff(log(bwdat))

    #ret_rowsum <- apply(logret, 1, sum) # by row
    #ret_colsum <- apply(logret, 2, sum) # by col
    # sum of ret_rowsum should be equal to ret_colsum

    # score of each basic win
    
    bw_score <- apply(logret, 2, tick_counter) # 2: by column vector
    bw_score_sum <- sum(bw_score[-which(names(bw_score)==sector)])

    bw_score_list <<- c(bw_score_list, bw_score_sum) # global var
    index_px_vec <- as.numeric(bwdat[,which(names(bw_score)==sector)])
    index_px_list <<- c(index_px_list, index_px_vec[length(index_px_vec)])
    
    index_bwret <- log(index_px_vec[nrow(bwdat)]/index_px_vec[1])
    index_bwret_list <<- c(index_bwret_list, index_bwret)
    
    # now process sliding window stats
    if ( bwnum >= sw/bw) {
      # lazy way: should do a 1-step update
      sw_score_list <<- ma(bw_score_list)
      
      # compute sw index return, assume index 1 is the ETF sector
      sid <- which(names(bw_score)==sector)
      index_swpx <- chopChunk[[sid]]
      index_swret <- log(index_swpx[length(index_swpx)]/index_swpx[1])
      index_swret_list <<- c(index_swret_list, index_swret)

      # compute swStats: bw stats, current sw
      bw_score_sd <- sd(bw_score_list)
      swStats <- sum(bw_score_list[(bwnum-sw/bw):bwnum])
      
      if (swStats > 2*bw_score_sd) {
        # buy
        cat("XXXX: buy signal...bwnum=",bwnum)
        ord <- list(bwnum=bwnum, t="buy")
        signalList[[bwnum]] <<- ord
      }
      else if (swStats < -2*bw_score_sd) {
        # sell
        cat("XXXX: sell signal...bwnum=",bwnum)
        ord <- list(bwnum=bwnum, t="sell")
        signalList[[bwnum]] <<- ord
      }
      else if (abs(swStats/bw_score_sd) < 0.5) {
        # recommond no positions held, 
        # use this signal to close the position (long/short) if any
        # or, only set this when holding index position
        
        cat("XXXX: zero signal...bwnum=",bwnum)
        ord <- list(bwnum=bwnum,t="zero")
        signalList[[bwnum]] <<- ord
      }
      else {
        signalList[[bwnum]] <<- NULL
      }
      
      # we need a list bw scores and current sw score to make a trading decision

    }

    cat("\n++++++END BASIC WINDOW [",bwnum,"] ++++++++++++++++++++++++\n")

}

gen_report_and_plot <- function() {
  cat("EOD reached.  Generating trading reports and PnL plot...\n")
  
  # compute sw score and ret
  #sw_score_list <- ma(bw_score_list) # * (sw/bw)
  
  score_list <- cbind(idx_time, bw_score_list, sw_score_list, index_px_list, index_bwret_list, index_swret_list)
  colnames(score_list, c("time", "bw_score", "sw_score", "index_px", "bw_index_ret", "sw_index_ret"))
  
  out_report <- paste("/export/data/",date_str,"/",sector,"_score_list_",date_str,".csv",sep="")
  write.csv(score_list, out_report)
  
  cat("=========== corr summary ==================\n")
  me <- read.csv(out_report)
  print(summary(me))
  
  # corr analysis
  cat("===========",sector,",",date_str,"==================\n")
  cat("#> cor(me$bw_score_list, me$index_bwret_list) \n ")
  cat(cor(me$bw_score_list, me$index_bwret_list))
  cat("\n#> cor(me$bw_score_list[1:189], me$index_bwret_list[2:190]) \n")
  cat(cor(me$bw_score_list[1:189], me$index_bwret_list[2:190]))
  
  # sw score and ret, should be highly correlated
  cat("\n#> cor(me$sw_score_list[5:190], me$index_swret_list[5:190]) \n")
  cat(cor(me$sw_score_list[5:190], me$index_swret_list[5:190]))
  # corr between current sw_score and sw ret
  # note: 4 (sw-1) basic window data overlap
  # so likely the 1 bw prediction time is not so useful, BUT TRY it!
  cat("\n#> cor(me$sw_score_list[5:189], me$index_swret_list[6:190]) \n")
  cat(cor(me$sw_score_list[5:189], me$index_swret_list[6:190]))
  cat("\n=========== END ==================\n")
  
  # trading rules: use all bw scores stats, if current sw_score is 2 outside of 2*sd (bw_score),
  # make an entry position and exit when it's reverting back to mean.
  
  signalDf <- as.data.frame(do.call(rbind,signalList))
  buyIdx <- as.numeric(signalDf[signalDf$t=="buy",1] )
  sellIdx <- as.numeric(signalDf[signalDf$t=="sell",1] )
  zeroIdx <- as.numeric(signalDf[signalDf$t=="zero",1] )
  
  #png(filename=paste("F:/DEV/Robmind/",sector,"_",dateStr,"_figure.png",sep=""), height=295, width=600, bg="white")
  #par(mfrow=c(2,1)))
  #par(mfcol=c(2,1)))
  ptitle <- paste(sector,", ",date_str,sep="")
  #plot(me$index_px_list, type='o', ylim=range(me$index_px_list), axes=F, ann=T, xlab="", ylab="px")
  plot(me$index_px_list, type='o', ylim=range(me$index_px_list), axes=FALSE, ann=FALSE, xlab="", ylab="px")
  grid()
  abline(v=buyIdx,col='green')
  abline(v=sellIdx,col='red')
  abline(v=zeroIdx,col='yellow')
  atidx <- seq(1,length(idx_time), 5)
  timelabel <- strptime(idx_time, "%Y-%m-%d %H:%M:%OS")
  xlabel <- format(timelabel, "%H:%M")
  #axis(1,at=atidx, lab=substring(idx_time[atidx],1,5), las=2)
  axis(1,at=atidx, lab=xlabel[atidx], las=2)
  #axis(2, las=1, at=range(me$index_px_list))
  axis(2, las=1, at=seq(min(me$index_px_list), max(me$index_px_list), 0.10))
  box()
  title(main=ptitle)
  #dev.off()
  
  #plot(me$sw_score_list, type='o')
  #axis(1, at=1:5, lab=c("Mon","Tue","Wed","Thu","Fri"))
  #abline(v=buyIdx,col='green')
  #abline(v=sellIdx,col='red')
  
  
  cat(paste("DONE. ", format(Sys.time(),format="%Y-%m-%d %H:%M:%S"), "\n"),sep="")
}


## plot price time series 
plotPriceSeries <- function(X, label="px") {
  x <- 1:NROW(X)                        # simple index 
  plot.new()                            # empty plot 
  oldpar <- par(mar=c(0,4,2,4),         # no bottom spacing 
                ylog=FALSE,              # plot on log(price) axis
                lend="square")          # square line ends

  ## set up coordinates
  plot.window(range(x), range(X, na.rm=TRUE), xaxs="i")
  grid()                                # dashed grid 

  lines(x, X, col='black')

  axis(2) 
  #axis(4, pos=par("usr")[1], line=0.5)  # this would plot them 'inside'
  title(ylab=label)              # y-axis label 

  box()                                 # outer box 
  par(oldpar) 
}

gen_plot <- function() {
  cat("EOD reached.  Generating trading reports and PnL plot...\n")
  
  score_list <- cbind(idx_time, bw_score_list, sw_score_list, index_px_list, index_bwret_list, index_swret_list)
  colnames(score_list, c("time", "bw_score", "sw_score", "index_px", "bw_index_ret", "sw_index_ret"))
  
  out_report <- paste("/export/data/",date_str,"/",sector,"_score_list_",date_str,".csv",sep="")
  write.csv(score_list, out_report)
  
  cat("=========== corr summary ==================\n")
  me <- read.csv(out_report)
  print(summary(me))
  
  # trading rules: use all bw scores stats, if current sw_score is 2 outside of 2*sd (bw_score),
  # make an entry position and exit when it's reverting back to mean.
  
  signalDf <- as.data.frame(do.call(rbind,signalList))
  buyIdx <- as.numeric(signalDf[signalDf$t=="buy",1] )
  sellIdx <- as.numeric(signalDf[signalDf$t=="sell",1] )
  zeroIdx <- as.numeric(signalDf[signalDf$t=="zero",1] )
  
  # test plot
  layout(matrix(c(1,2,3),3,1,byrow=TRUE), 
         height=c(0.5,0.2,0.3), width=1)

  ## set 'global' plot parameters: horizontal y-axis labels, tighter spacing
  ## and no outer spacing
oldpar <- par(las=1, mar=c(2,4,2,4), oma=c(2.5,0.5,1.5,0.5)) 


  plotPriceSeries(me$index_px_list, "index px")

  ptitle <- paste(sector,", ",date_str,sep="")
  #plot(me$index_px_list, type='o', ylim=range(me$index_px_list), axes=F, ann=T, xlab="", ylab="px")
  #plot(me$index_px_list, type='o', ylim=range(me$index_px_list), axes=FALSE, ann=FALSE, xlab="", ylab="px")
  grid()
  abline(v=buyIdx,col='green')
  abline(v=sellIdx,col='red')
  #abline(v=zeroIdx,col='yellow')
  title(main=ptitle)

  plotPriceSeries(me$sw_score_list, "sliding win score")
  #plotSignalsSeries(me$sw_score_list)
  plotPriceSeries(me$bw_score_list, "basic win score")
  
  atidx <- seq(1,length(idx_time), 5)
  timelabel <- strptime(idx_time, "%Y-%m-%d %H:%M:%OS")
  xlabel <- format(timelabel, "%H:%M")
  #axis(1,at=atidx, lab=substring(idx_time[atidx],1,5), las=2)
  axis(1,at=atidx, lab=xlabel[atidx], las=2)
  #axis(2, las=1, at=range(me$index_px_list))
  axis(2, las=1, at=seq(min(me$index_px_list), max(me$index_px_list), 0.10))
  
  #hist(sprd, main="", col="lightblue")
  par(oldpar)      

  cat(paste("DONE. ", format(Sys.time(),format="%Y-%m-%d %H:%M:%S"), "\n"),sep="")
}
