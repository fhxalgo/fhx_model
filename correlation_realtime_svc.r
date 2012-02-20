#library(zoo)
library(xts) 

# start time of the program
startTime <- format(Sys.time(),format="%Y-%m-%d %H:%M:%S")
##
## Global variables 
##
corr_report <- data.frame()  # correlation report
rdx <- 1 # data.frame index

streamData <<- data.frame()

statstream <- list() # digest /all the digests of the data streams, using rotating windows

#chopChunk <- matrix(data=0, nrow=n_stream, ncol=sw) # all data points in the sliding window
chopChunk <- list()  # raw data for each stream to compute real correlation of sw

corr_pairs <<- list() 		# list of all the correlation stats calculated on 
# each bw. keys are the bw number, values are data frames

open_positions <<- data.frame() 		# open positions at any given moment
# added when trading signal is generated, remove when closing position
# TODO: this can be linked with actual ors trade executions

all_trade_signals <<- data.frame()	# data frame to hold all of todays trading signal

sym_data <- list()
tick_data <<- data.frame()

# set up
bw <- 2
sw <- 8 
threshold <- 0.8  # correlation threshold for outputs

bwdat <- data.frame()

date_str <- Sys.Date()
trade_period <- paste(date_str, " 09:30:00", "::", date_str, " 16:00:00", sep="")
trading_end_time <- paste(date_str, " 15:45:00", sep="")

#sym_list <- c("DIA",
#		"AA","AXP","BAC","BA","CAT","CSCO","CVX","DD","DIS","GE",
#		"HD","HPQ","IBM","INTC","JNJ","JPM","KFT","KO","MCD","MMM",
#		"MRK","MSFT","PFE","PG","TRV","T","UTX","VZ","WMT","XOM")
sym_list <- c("EUR", "GBP", "JPY")
n_stream <- length(sym_list)
sym_index <- sym_list[1] # to be used everywhere

corr_matrix <- data.frame(ncol=length(sym_list))
corr_matrix[1,1] <- 0
cor_idx <- 1
vol_matrix  <- data.frame(ncol=length(sym_list))
vol_matrix[1,1] <- 0

order_list <- data.frame() # sym, shares, price, type(open/close), bwnum
ord_idx <- 0
position_list <- list() # sym, qty, px, index_px, index_qty

bwnum <- 0
swnum <- 0
timepointer <- c() # use timestamp as data frame rownames

# update each stream's digest for each new basic window
# Note: here the stream data are column based, i.e. AA, IBM, ...etc.
UpdateDigest3 <- function(statstream, chopChunk, newdat, bwnum)
{
	# first 2 cols are timestamp and basicwindownumber
	for(j in 3:ncol(newdat)) {
		
		#get new bw for each stream
		sid <- paste("s",j,sep="")
		bwdat <- newdat[,j]
		rw <- as.numeric(bwdat)
		
		if (bwnum == 1) {
			chopChunk[[j]] <- rw
		}
		else if (bwnum <= (sw/bw)) {                               
			chopChunk[[j]] <- c(chopChunk[[j]], rw)  # append to existing basicwin
		}
		else {
			# override the oldest bw : do a shift update: should use a shift function 
			chopChunk[[j]] <- c(chopChunk[[j]][(bw+1):sw], rw)
		}      
		
		#computing digest
		#statstream[[index]] <- batchDigestUpdate(statstream[[index]], rw)
		
	} # end of n_stream 
	
	# return data    
	retList <- list()
	retList$chopChunk <- chopChunk
	retList$statstream <- statstream
	
	retList 
}

add_orders <- function(order_list, sym_y, order_type, sym_y_qty, sym_px_list, bwnum, curTime) 
{
	
	# sym position
	ord_idx <- length(order_list) + 1
	order_list[ord_idx, 1] <- sym_y # sym
	order_list[ord_idx, 2] <- order_type
	order_list[ord_idx, 3] <- - sym_y_qty                              
	order_list[ord_idx, 4] <- sym_px_list[sw]
	order_list[ord_idx, 5] <- bwnum
	order_list[ord_idx, 6] <- curTime
	order_list[ord_idx, 7] <- 0  # for new orders
	
	if (order_type == "CLOSE") {
		order_list[ord_idx, 7] <- -0.99  # closing a position
	}
	
# index position
#ord_idx <- ord_idx + 1
#order_list[ord_idx, 1] <- sym_index # sym
#order_list[ord_idx, 2] <- "CLOSE" 
#order_list[ord_idx, 3] <- -sym_y_pos$index_qty
#order_list[ord_idx, 4] <- index_px[sw]
#order_list[ord_idx, 5] <- bwnum
#order_list[ord_idx, 6] <- as.character(end(bwdat))
#order_list[ord_idx, 7] <- index_pnl
	
	order_list 
}

add_new_order <- function(sym_y, order_type, sym_y_qty, sym_px_list, bwnum, curTime) 
{
	# Symbol OrderType Quantity  Price BasicWinNum  Time  PnL
	new_order <- list()
	# sym position
	
	new_order$Symbol <- sym_y # sym
	new_order$OrderType <- order_type
	new_order$Quantity <- - sym_y_qty                              
	new_order$Price <- sym_px_list[sw]
	new_order$BasicWinNum <- bwnum
	new_order$Time <- curTime
	new_order$PnL <- 0  # for new orders
	
	if (order_type == "CLOSE") {
		new_order$PnL <- -0.99  # closing a position
	}
	
	new_order 
}

#
# newdata is a data frame row with format
# timestamp, bwnum, midTick1, midTick2, midTick3, ...
#
process_basic_window <- function(newdat) 
{		
	tick_data <- newdat
	colnames(tick_data) <- c("timeStamp", "bwnum", sym_list)
	
	# create a xts object
	#tick_xts <- as.xts(tick_data)
	#X <- tick_xts   #[trade_period]  # 5 seconds time series streams
	
	X <- tick_data
	bw <- 2
	corr_matrix <- data.frame(ncol=length(sym_list))
	corr_matrix[1,1] <- 0
	cor_idx <- 1
	vol_matrix  <- data.frame(ncol=length(sym_list))
	vol_matrix[1,1] <- 0
	
	order_list <- data.frame() # sym, shares, price, type(open/close), bwnum
	ord_idx <- 0
	position_list <- list() # sym, qty, px, index_px, index_qty
	bwnum <- 0
	swnum <- 0
	timepointer <- c() # use timestamp as data frame rownames
	
	m <- floor(nrow(X) / bw)
	
	cat("looping",m,"times","\n")
	
	# m is for each bw update 
	for (i in 1:m) {
		#for (i in 1:10) {
		cat("m=",m,"\n")
		cat("\n++++++++++++NEW BASIC WINDOW BEGIN+++++++++++++++++++++++++++++++\n")
		
		#i <- 1
		bwnum <- i
		
		# get new basic window data for all stream  
		#bwdat <- tickstream[readpointer:(bwnum*bw*n_stream), ]
		# use time as index: start at 09:30, offset 120 seconds.
		
		x_start <- (bwnum - 1) * bw + 1
		x_end   <- (bwnum * bw)
		cat("x_start=",x_start,"\n")
		cat("x_end=",x_end,"\n")
		cat("here",nrow(X))
		
		bwdat <- X[x_start:x_end, ]
		cat("processing bwnum...",bwnum," \n")
		
		# Note: bwdat is now column based
		newdigest <- UpdateDigest3(statstream, chopChunk, bwdat, bwnum)
		cat("xxxx newdigest <- UpdateDigest3() \n")
		# update chopChunk and statstream
		statstream <- newdigest$statstream 
		chopChunk <- newdigest$chopChunk
		
		# computing correlation    
		if (bwnum >= sw/bw) {    
			swnum <- swnum + 1
			cat("sliding window num ", swnum, " start computing correlations \n")
			
			pos_signal_list <- list()
			neg_signal_list <- list()
			
			index_px <- chopChunk[[1]] # index is always the first one
			
			if (sd(index_px) == 0) {
				# need to do nothing as index prices don't change         
				corr_matrix[cor_idx, 1:length(sym_list)] <- 0  # this may not work
				cat("xxx Warning: index_px for ", sym_index, " didn't change. All corr set to 0. \n ") 
				
				# do nothing until the next basic window update
				next 
			}
			else {
				corr_matrix[cor_idx, 1] <- 1
				vol_matrix[cor_idx, 1] <- sd(index_px)
				
				# now compute the correlations of each symbol with ETF 
				for (j in 2:n_stream) {
					sym_j <- sym_list[j]
					sym_px_list <- chopChunk[[j]]
					
					if (sd(sym_px_list) == 0) {
						cat("xxxx got a constant price for ", sym_j)
						cat(" bwnum=", bwnum, "\n")
						cat("Time: ")
						print(start(bwdat))
						cat(" - ")
						print(end(bwdat))
						cat(" \n ")
						
						corr_matrix[cor_idx, j] <- 0  # prices don't change over a sliding window, ignore
					}
					else {
						sym_cor_j <- cor(chopChunk[[1]], chopChunk[[j]])        
						
						if (sym_cor_j > threshold) {
							pos_signal_list[[sym_j]] <- sym_cor_j
						}
						
						if (sym_cor_j < -threshold) {
							neg_signal_list[[sym_j]] <- sym_cor_j            
						}
						
						corr_matrix[cor_idx, j] <- sym_cor_j            
					}
					
					# log each symbol's volatility
					vol_matrix[cor_idx, j] <- sd(sym_px_list)  
					
				} # end of for(j)
				
				#cat("$$$ pos_signal_list: ") 
				#print(pos_signal_list)
				#cat("\n")
				#cat("$$$ neg_signal_list: ") 
				#print(neg_signal_list)
				#cat("\n")
				
				# now rank the correlations and create two lists: positive/negative corr
				#cor_vec <- as.numeric(corr_matrix[5,-1])  # exclude index column
				#cor_vec <- as.numeric(corr_matrix[189,])  
				#cor_sorted <- sort(cor_vec, index.return = TRUE)   # index.return = FALSE
				
				# close position first 
				if (length(position_list) >0) {                
					close_signals <- names(pos_signal_list)          
					index_close_qty <- 0 # aggregate all shares for index so we don't send mutitple orders
					
					for (y in 1:length(close_signals)) {
						# close all positions that are highly correlated with index again
						# note: use order_list to find the px of OPEN position
						sym_y <- close_signals[y]
						sym_y_idx <- which(names(position_list) == sym_y)
						
						if (length(sym_y_idx) > 0) {
							# close the position: compute pnl
							cat("$$$$ CLOSING position for ", sym_y, " \n")
							
							sym_y_pos <- position_list[[sym_y]]  
							#index_open_px <- as.numeric(position_list[[sym_list[1]]][2])  # not correct here, fix it later
							index_open_px <- as.numeric(sym_y_pos$index_px)  # fixed
							index_close_px <- index_px[sw] 
							
							sym_open_px <- as.numeric(sym_y_pos$px)
							sym_id <- which(sym_list == sym_y)
							sym_close_px <- chopChunk[[sym_id]][sw]
							
							# compute pnl
							sym_pnl <-  (sym_close_px - sym_open_px) * sym_y_pos$qty 
							index_pnl <- (index_close_px - index_open_px) * sym_y_pos$index_qty
							
							# sym position
							ord_idx <- ord_idx + 1
							order_list[ord_idx, 1] <- sym_y # sym
							order_list[ord_idx, 2] <- "CLOSE" 
							order_list[ord_idx, 3] <- - sym_y_pos$qty                              
							order_list[ord_idx, 4] <- sym_close_px
							order_list[ord_idx, 5] <- bwnum
							order_list[ord_idx, 6] <- as.character(end(bwdat))
							order_list[ord_idx, 7] <- sym_pnl
							
							# index position
							ord_idx <- ord_idx + 1
							order_list[ord_idx, 1] <- sym_index # sym
							order_list[ord_idx, 2] <- "CLOSE" 
							order_list[ord_idx, 3] <- -sym_y_pos$index_qty
							order_list[ord_idx, 4] <- index_px[sw]
							order_list[ord_idx, 5] <- bwnum
							order_list[ord_idx, 6] <- as.character(end(bwdat))
							order_list[ord_idx, 7] <- index_pnl
							
							index_close_qty <- index_close_qty - sym_y_pos$index_qty
							
							position_list[[sym_y]] <- NULL # remove the position
							#position_list[[sym_list[1]]] <- NULL # remove the position        
							
						} # end of if (y_idx)
					} # end of for(y)        
					
					# upadte position_list[[sym_index]]$qty  
					position_list[[sym_index]]$qty <- position_list[[sym_index]]$qty + index_close_qty
					position_list[[sym_index]]$px <- 0
					# position_list should be flat after the Open positions are closed
					
					# close all positions after 15:45, and turn trading flag to false
					#if (as.character(end(bwdat)) > trading_end_time && length(position_list) >0) {
					if (as.character(format(end(bwdat), format="%Y%m%d %H:%M:%S")) > trading_end_time ) {
						cat("$$$$ ending the auto trading session, liquiditing all open positions: \n");
						print(names(position_list)) 
						
						for (p in 1:length(position_list)) {
							sym <- names(position_list)[p]
							pos <- position_list[[sym]]
							cat(" sym: ", sym, " qty: ", pos$qty, " \n")
							
							position_list[[sym]] <- NULL
						}
						cat("$$$$ \n")
						stop
					}
					
				} # end of if close position
				
				# index return: used by every neg_signal_list to open a position 
				index_ret <- log(index_px[sw]/index_px[1])
				
				# go through the neg_signal_list:  names(pos_signal_list[pos_signal_list<0])          
				if (length(neg_signal_list) >0 ) {
					open_signals <- names(neg_signal_list)
					index_open_qty <- 0
					
					for (x in 1:length(open_signals)) {
						sym_x <- open_signals[x]
						sym_x_pos <- which(names(position_list) == sym_x)
						
						if (length(sym_x_pos) !=0)
							next # skip it as position has been held for sym_x 
						
						# get sym_id from sym_list
						sym_id <- which(sym_list == sym_x)
						
						if (length(sym_id)) {
							sym_px <- chopChunk[[sym_id]]  # raw price data 
							# log return in the current sliding window
							sym_ret <- log(sym_px[sw]/sym_px[1])  
							
							cat("$$$$ OPENING position for ", sym_x, " \n") 
							# now create the order based on index and sym returns
							# rule is simple: 
							# if (ret_index > sym_ret)  short index, long sym
							# else  long index, short sym
							
							# add to position_list
							if ( sym_ret < index_ret ) {
								position_list[[sym_x]]$qty <- 100 # long
								position_list[[sym_x]]$px <- sym_px[sw]
								position_list[[sym_x]]$index_px <- index_px[sw]
								index_qty <- floor(-100 * sym_px[sw]/index_px[sw])
								position_list[[sym_x]]$index_qty <- index_qty
								# notice: we want to offset the quantity for index symbol, $$$
								index_open_qty <- index_open_qty + index_qty
								
								# create order
								ord_idx <- ord_idx + 1
								order_list[ord_idx, 1] <- sym_x
								order_list[ord_idx, 2] <- "Buy" 
								order_list[ord_idx, 3] <- 100  # long
								order_list[ord_idx, 4] <- sym_px[sw]
								order_list[ord_idx, 5] <- bwnum
								order_list[ord_idx, 6] <- as.character(end(bwdat)) 
								order_list[ord_idx, 7] <- 0 
								
								#new_order <- add_new_order(sym_x, "Buy", 100, sym_px, bwnum, as.character(end(bwdat)) ) 
								#order_list[ord_idx,] <- new_order
								
							}
							else {
								position_list[[sym_x]]$qty <- -100 # short sell
								position_list[[sym_x]]$px <- sym_px[sw]
								position_list[[sym_x]]$index_px <- index_px[sw]
								index_qty <- floor(100 * sym_px[sw]/index_px[sw])
								position_list[[sym_x]]$index_qty <- index_qty
								# notice: we want to offset the quantity for index symbol, $$$
								index_open_qty <- index_open_qty + index_qty
								
								ord_idx <- ord_idx + 1
								order_list[ord_idx, 1] <- sym_x
								order_list[ord_idx, 2] <- "ShortSell" 
								order_list[ord_idx, 3] <- -100  # short sale
								order_list[ord_idx, 4] <- sym_px[sw]
								order_list[ord_idx, 5] <- bwnum
								order_list[ord_idx, 6] <- as.character(end(bwdat))                
								order_list[ord_idx, 7] <- 0
								
								#new_order <- add_new_order(sym_x, "ShortShell", -100, sym_px, bwnum, as.character(end(bwdat)) ) 
								#order_list[ord_idx,] <- new_order  
							}                 
						}
					} # end of for (x)
					
					# set index_open_qty
					if (index_open_qty != 0) {
						cat("$$$ open position_list on index is: ", index_open_qty, "\n")
						position_list[[sym_index]]$qty <- index_open_qty # this is hedged position for open positions    
						position_list[[sym_index]]$px <- index_px[sw]
						
						ord_idx <- ord_idx + 1
						#      order_list[ord_idx, 1] <- sym_index # sym
						#      order_list[ord_idx, 2] <- "Open" 
						#      order_list[ord_idx, 3] <- index_open_qty  
						#      order_list[ord_idx, 4] <- index_px[sw]
						#      order_list[ord_idx, 5] <- bwnum
						#      order_list[ord_idx, 6] <- as.character(end(bwdat))
						#      order_list[ord_idx, 7] <- 0
						
						#order_list <- add_orders(order_list, sym_index, "Open", index_open_qty, index_px, bwnum, as.character(end(bwdat)))
						new_order <- add_new_order(sym_index, "Open", index_open_qty, index_px, bwnum, as.character(end(bwdat)) ) 
						order_list[ord_idx,] <- new_order
						
					} # end if index_position 
					
				} # end of if (length(neg_signal_list)
				
			}
		} # end of bwnum computing correlation  
		
		cor_idx <- cor_idx + 1
		
		cat("------------NEW BASIC WINDOW END---------------------------------\n")
	} # end of basic window loop
	
	nrow(tick_data)
}

test <- function(bw_tick, bw_num)
{
	chopChunk[[bw_num]] <- bw_tick
}

process_basic_window2 <- function(newdat) 
{		
	tick_data <- newdat
	colnames(tick_data) <- c("timeStamp", "bwnum", sym_list)
	
	# create a xts object
	#tick_xts <- as.xts(tick_data)
	#X <- tick_xts   #[trade_period]  # 5 seconds time series streams
	
	X <- tick_data
	
	m <- floor(nrow(X) / bw)
	
	cat("looping",m,"times","\n")
	
	# m is for each bw update 
	for (i in 1:m) {
		
		#for (i in 1:10) {
		cat("mXX=",m,"\n")
		cat("\n++++++++++++NEW BASIC WINDOW BEGIN+++++++++++++++++++++++++++++++\n")
		
		#i <- 1
		bwnum <- i
		
		# get new basic window data for all stream  
		#bwdat <- tickstream[readpointer:(bwnum*bw*n_stream), ]
		# use time as index: start at 09:30, offset 120 seconds.
		bw <- 2
		x_start <- (bwnum - 1) * bw + 1
		x_end   <- (bwnum * bw)
		cat("x_start=",x_start,"\n")
		cat("x_end=",x_end,"\n")
		cat("herehaha",nrow(X),"\n")
		
		bwdat <- X[x_start:x_end, ]
		cat("processing bwnum...",bwnum," \n")
		print(bwdat)
		
		chopChunk <- UpdateDigest2(chopChunk, bwdat, bwnum)
		
		
		cat("xxxx newdigest <- UpdateDigest3() \n")
		# update chopChunk and statstream
		#statstream <- newdigest$statstream 
		#chopChunk <- newdigest$chopChunk
		
	}
	
	nrow(tick_data)
}

UpdateDigest2 <- function(chopChunk, bw_tick, bw_num)
{
	for(j in 1:length(sym_list)) {
		
		bwdat <- bw_tick[,j+2]
		rw <- as.numeric(bwdat)
		
		if (bw_num == 1) {
			chopChunk[[j]] <- rw
		}
		else if (bw_num <= (sw/bw)) {
			chopChunk[[j]] <- c(chopChunk[[j]], rw)  # append to existing basicwin
		}
		else {
			# override the oldest bw : do a shift update: should use a shift function
			chopChunk[[j]] <- c(chopChunk[[j]][(bw+1):sw], rw)
		}
		
	} # end of n_stream
	
	chopChunk
}

handle_position_closing <- function()
{
	# close position first 
	if (length(position_list) >0) {                
		close_signals <- names(pos_signal_list)          
		index_close_qty <- 0 # aggregate all shares for index so we don't send mutitple orders
		
		for (y in 1:length(close_signals)) {
			# close all positions that are highly correlated with index again
			# note: use order_list to find the px of OPEN position
			sym_y <- close_signals[y]
			sym_y_idx <- which(names(position_list) == sym_y)
			
			if (length(sym_y_idx) > 0) {
				# close the position: compute pnl
				cat("$$$$ CLOSING position for ", sym_y, " \n")
				
				sym_y_pos <- position_list[[sym_y]]  
				#index_open_px <- as.numeric(position_list[[sym_list[1]]][2])  # not correct here, fix it later
				index_open_px <- as.numeric(sym_y_pos$index_px)  # fixed
				index_close_px <- index_px[sw] 
				
				sym_open_px <- as.numeric(sym_y_pos$px)
				sym_id <- which(sym_list == sym_y)
				sym_close_px <- chopChunk[[sym_id]][sw]
				
				# compute pnl
				sym_pnl <-  (sym_close_px - sym_open_px) * sym_y_pos$qty 
				index_pnl <- (index_close_px - index_open_px) * sym_y_pos$index_qty
				
				# sym position
				ord_idx <- ord_idx + 1
				order_list[ord_idx, 1] <- sym_y # sym
				order_list[ord_idx, 2] <- "CLOSE" 
				order_list[ord_idx, 3] <- - sym_y_pos$qty                              
				order_list[ord_idx, 4] <- sym_close_px
				order_list[ord_idx, 5] <- bwnum
				order_list[ord_idx, 6] <- as.character(end(bwdat))
				order_list[ord_idx, 7] <- sym_pnl
				
				# index position
				ord_idx <- ord_idx + 1
				order_list[ord_idx, 1] <- sym_index # sym
				order_list[ord_idx, 2] <- "CLOSE" 
				order_list[ord_idx, 3] <- -sym_y_pos$index_qty
				order_list[ord_idx, 4] <- index_px[sw]
				order_list[ord_idx, 5] <- bwnum
				order_list[ord_idx, 6] <- as.character(end(bwdat))
				order_list[ord_idx, 7] <- index_pnl
				
				index_close_qty <- index_close_qty - sym_y_pos$index_qty
				
				position_list[[sym_y]] <- NULL # remove the position
				#position_list[[sym_list[1]]] <- NULL # remove the position        
				
			} # end of if (y_idx)
		} # end of for(y)        
		
		# upadte position_list[[sym_index]]$qty  
		position_list[[sym_index]]$qty <- position_list[[sym_index]]$qty + index_close_qty
		position_list[[sym_index]]$px <- 0
		# position_list should be flat after the Open positions are closed
		
		# close all positions after 15:45, and turn trading flag to false
		#if (as.character(end(bwdat)) > trading_end_time && length(position_list) >0) {
		if (as.character(format(end(bwdat), format="%Y%m%d %H:%M:%S")) > trading_end_time ) {
			cat("$$$$ ending the auto trading session, liquiditing all open positions: \n");
			print(names(position_list)) 
			
			for (p in 1:length(position_list)) {
				sym <- names(position_list)[p]
				pos <- position_list[[sym]]
				cat(" sym: ", sym, " qty: ", pos$qty, " \n")
				
				position_list[[sym]] <- NULL
			}
			cat("$$$$ \n")
			stop
		}
		
	} # end of if close position
	
	1
}

process_basic_window3 <- function(newdat) 
{		
	tick_data <- newdat
	colnames(tick_data) <- c("timeStamp", "bwNum", sym_list)
	
	# create a xts object
	#tick_xts <- as.xts(tick_data)
	#X <- tick_xts[trade_period]  # 5 seconds time series streams
	
	# number of bw from input data files
	m <- floor(nrow(tick_data) / bw)
	
	# m is for each bw update 
	for (i in 1:m) {
		#for (i in 1:10) {
		cat("\n++++++++++++NEW BASIC WINDOW BEGIN+++++++++++++++++++++++++++++++\n")
		#i <- 1
		bwnum <- i
		
		# get new basic window data for all stream  
		#bwdat <- tickstream[readpointer:(bwnum*bw*n_stream), ]
		# use time as index: start at 09:30, offset 120 seconds.
		
		x_start <- (bwnum - 1) * bw + 1
		x_end   <- (bwnum * bw)
		cat("x_start: ",x_start, " ")
		cat("x_end  : ",x_end, " \n")
		
		bwdat <- tick_data[x_start:x_end, ]
		print(bwdat)
		cat("processing bwnum...",bwnum, " time: ",bwdat$timeStamp[i], " \n")
		
		# Note: bwdat is now column based
		chopChunk <- UpdateDigest2(chopChunk, bwdat, bwnum)
		cat("done updating chopChunk... \n")
		print(chopChunk)
		
		# computing correlation    
		if (bwnum >= sw/bw) {    
			# cache bwwin timestamp
			timepointer <- c(timepointer, bwnum)
			
			swnum <- swnum + 1
			cat("sliding window num ", swnum, " start computing correlations \n")
			
			pos_signal_list <- list()
			neg_signal_list <- list()
			
			index_px <- chopChunk[[1]] # index is always the first one
			
			if (sd(index_px) == 0) {
				# need to do nothing as index prices don't change
				cat("xxx Warning: index_px for ", sym_index, " didn't change. All corr set to 0. \n ")
				corr_matrix[cor_idx, ] <- 0  # this may not work
				
				# do nothing until the next basic window update
				next 
			}
			else {
				corr_matrix[cor_idx, 1] <- 1
				vol_matrix[cor_idx, 1] <- sd(index_px)
				
				# now compute the correlations of each symbol with ETF 
				for (j in 2:length(sym_list)) {
					sym_j <- sym_list[j]
					sym_px_list <- chopChunk[[j]]
					
					cat("symbol px ->\n")
					print(chopChunk[[j]])
					
					if (sd(sym_px_list) == 0) {
						cat("xxxx got a constant price for ", sym_j)
						cat(" bwnum=", bwnum, "\n")
						cat("Time: ")
						print(start(bwdat))
						cat(" - ")
						print(end(bwdat))
						cat(" \n ")
						
						corr_matrix[cor_idx, j] <- 0  # prices don't change over a sliding window, ignore
					}
					else {
						sym_cor_j <- cor(chopChunk[[1]], chopChunk[[j]])        
						
						if (sym_cor_j > threshold) {
							pos_signal_list[[sym_j]] <- sym_cor_j
						}
						
						if (sym_cor_j < -threshold) {
							neg_signal_list[[sym_j]] <- sym_cor_j            
						}
						
						cat("correlation(1&",j,") = ",sym_cor_j,"\n")
						corr_matrix[cor_idx, j] <- sym_cor_j            
					}
					
					# log each symbol's volatility
					vol_matrix[cor_idx, j] <- sd(sym_px_list)  
					
				} # end of for(j)
			} # end of if(sd_index_px)==0)
			
			#
			# properly close the open positions from previous windows
			#
			handle_position_closing()
			
			#
			# generate open signals
			#
			# index return: used by every neg_signal_list to open a position 
			index_ret <- log(index_px[sw]/index_px[1])
			
			# go through the neg_signal_list:  names(pos_signal_list[pos_signal_list<0])          
			if (length(neg_signal_list) >0 ) {
				open_signals <- names(neg_signal_list)
				index_open_qty <- 0
				
				for (x in 1:length(open_signals)) {
					sym_x <- open_signals[x]
					sym_x_pos <- which(names(position_list) == sym_x)
					if (length(sym_x_pos) !=0)
						next # skip it as position has been held for sym_x 
					
					# get sym_id from sym_list
					sym_id <- which(sym_list == sym_x)
					
					if (length(sym_id)) {
						sym_px <- chopChunk[[sym_id]]  # raw price data 
						# log return in the current sliding window
						sym_ret <- log(sym_px[sw]/sym_px[1])  
						
						cat("$$$$ OPENING position for ", sym_x, " \n") 
						# now create the order based on index and sym returns
						# rule is simple: 
						# if (ret_index > sym_ret)  short index, long sym
						# else  long index, short sym
						
						# add to position_list
						if ( sym_ret < index_ret ) {
							position_list[[sym_x]]$qty <- 100 # long
							position_list[[sym_x]]$px <- sym_px[sw]
							position_list[[sym_x]]$index_px <- index_px[sw]
							index_qty <- floor(-100 * sym_px[sw]/index_px[sw])
							position_list[[sym_x]]$index_qty <- index_qty
							# notice: we want to offset the quantity for index symbol, $$$
							index_open_qty <- index_open_qty + index_qty
							
							# create order
							ord_idx <- ord_idx + 1
							order_list[ord_idx, 1] <- sym_x
							order_list[ord_idx, 2] <- "Buy" 
							order_list[ord_idx, 3] <- 100  # long
							order_list[ord_idx, 4] <- sym_px[sw]
							order_list[ord_idx, 5] <- bwnum
							order_list[ord_idx, 6] <- as.character(end(bwdat)) 
							order_list[ord_idx, 7] <- 0 
							
							#new_order <- add_new_order(sym_x, "Buy", 100, sym_px, bwnum, as.character(end(bwdat)) ) 
							#order_list[ord_idx,] <- new_order
							
						}
						else {
							position_list[[sym_x]]$qty <- -100 # short sell
							position_list[[sym_x]]$px <- sym_px[sw]
							position_list[[sym_x]]$index_px <- index_px[sw]
							index_qty <- floor(100 * sym_px[sw]/index_px[sw])
							position_list[[sym_x]]$index_qty <- index_qty
							# notice: we want to offset the quantity for index symbol, $$$
							index_open_qty <- index_open_qty + index_qty
							
							ord_idx <- ord_idx + 1
							order_list[ord_idx, 1] <- sym_x
							order_list[ord_idx, 2] <- "ShortSell" 
							order_list[ord_idx, 3] <- -100  # short sale
							order_list[ord_idx, 4] <- sym_px[sw]
							order_list[ord_idx, 5] <- bwnum
							order_list[ord_idx, 6] <- as.character(end(bwdat))                
							order_list[ord_idx, 7] <- 0
							
							#new_order <- add_new_order(sym_x, "ShortShell", -100, sym_px, bwnum, as.character(end(bwdat)) ) 
							#order_list[ord_idx,] <- new_order  
						}                 
					}
				} # end of for (x)
				
				# set index_open_qty
				if (index_open_qty != 0) {
					cat("$$$ open position_list on index is: ", index_open_qty, "\n")
					position_list[[sym_index]]$qty <- index_open_qty # this is hedged position for open positions    
					position_list[[sym_index]]$px <- index_px[sw]
					
					ord_idx <- ord_idx + 1
					#      order_list[ord_idx, 1] <- sym_index # sym
					#      order_list[ord_idx, 2] <- "Open" 
					#      order_list[ord_idx, 3] <- index_open_qty  
					#      order_list[ord_idx, 4] <- index_px[sw]
					#      order_list[ord_idx, 5] <- bwnum
					#      order_list[ord_idx, 6] <- as.character(end(bwdat))
					#      order_list[ord_idx, 7] <- 0
					
					#order_list <- add_orders(order_list, sym_index, "Open", index_open_qty, index_px, bwnum, as.character(end(bwdat)))
					new_order <- add_new_order(sym_index, "Open", index_open_qty, index_px, bwnum, as.character(end(bwdat)) ) 
					order_list[ord_idx,] <- new_order
					
				} # end if index_position 
				
			} # end of if (length(neg_signal_list)
			
			cor_idx <- cor_idx + 1
			
			colnames(corr_matrix) <- sym_list
			rownames(corr_matrix) <- timepointer 
			corr_matrix[is.na(corr_matrix)] <- 0
			colnames(vol_matrix) <- sym_list
			rownames(vol_matrix) <- timepointer 
			vol_matrix[is.na(vol_matrix)] <- 0
			
			cat("corr_matrix ->\n")
			print(corr_matrix)
			cat("vol_matrix ->\n")
			print(vol_matrix)
			
		} #end of if (bwnum >= sw/bw)
		
		cat("------------NEW BASIC WINDOW END---------------------------------\n")
	} # end of for(i)
	
	
	
	# return
	nrow(tick_data)
}