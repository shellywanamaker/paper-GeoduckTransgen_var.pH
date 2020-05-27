## load libraries
library(readxl)
library(devtools)
library(LoLinR)

## read in data


file.name <- dir(paste(data_paths, folder.list[3], sub.folder.list[1], sep = "/"), pattern = "OXY.xlsx", full.names = TRUE)
Resp.Data <- data.frame(read_xlsx(file.name, skip = 12)) # read in data
Resp.Data$Time_Min <- seq(0.25, (nrow(Resp.Data))*0.25, by=0.25)
Resp.Data$Date.Time <- as.POSIXct(strptime(Resp.Data$Date.Time, format ="%d.%m.%y %H:%M:%S"))
a <- 0.2

resp_total <- data.frame() # create dataframe to save cumunalitively during for loop
resp.table <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save each trial data during for loop
colnames(resp.table)<-c('Date', 'Run', 'SDR_position', 'Lpc', 'Leq' , 'Lz', 'alpha') # names for columns in the for loop

for(k in 3:26){
  model <- rankLocReg(
    xall=Resp.Data$Time_Min, yall=Resp.Data[, k],
    alpha=a, method="pc", verbose=TRUE) # run the LoLin script
  sum.table<-summary(model)
  resp.table$Date <- unique(format(as.POSIXct(Resp.Data$Date.Time,format="%m.%d.%y %H:%M:%S"),format="%m.%d.%y")) # all files have date in the form of yyyymmdd at the start of each csv name
  resp.table$Run <- gsub("run","",sub.folder.list[i])
  resp.table$SDR_position <- colnames(Resp.Data[k]) # assign the vial position - this will be related to contents (blank or individuals) later in script
  resp.table$alpha <- a # set at start of script - reresents the proportion of data for final estimate of slopes (Lpc, Leq, Lz)
  resp.table$Lpc <-sum.table$Lcompare[3,6] # Lpc slope 
  resp.table$Leq <-sum.table$Lcompare[2,6] # Leq slope 
  resp.table$Lz <-sum.table$Lcompare[1,6]  # Lz slope 
  #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
  #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
  #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]
  # save plots every inside loop and name by date_run_vialposition
  pdf(paste0("~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/plots_alpha0.2/",resp.table$Date[1],"_",sub.folder.list[i],"_",colnames(Resp.Data[k]),"_regression.pdf"))
  plot(model)
  dev.off()
  resp_total <- rbind(resp_total,resp.table) #bind to a cumulative list dataframe
} # end of inside for loop


