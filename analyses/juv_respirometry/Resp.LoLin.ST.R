## load libraries
library(readxl)
library(devtools)
library(LoLinR)

## read in data

data_paths <- "~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR"
folder.list <- dir(data_paths)
df_total <- data.frame()
for(j in 1:length(folder.list)){
  sub.folder.list <- dir(paste(data_paths, folder.list[j], sep = "/"))
  runs_total <- data.frame()
  for(i in 1:length(sub.folder.list)){
    file.name <- dir(paste(data_paths, folder.list[j], sub.folder.list[i], sep = "/"), pattern = "OXY.xlsx", full.names = TRUE)
    Resp.Data <- data.frame(read_xlsx(file.name, skip = 12)) # read in data
    Resp.Data$Time_Min <- seq(0.25, (nrow(Resp.Data))*0.25, by=0.25)
    Resp.Data$Date.Time <- as.POSIXct(strptime(Resp.Data$Date.Time, format ="%d.%m.%y %H:%M:%S"))
    a <- 0.4
    
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
      pdf(paste0("~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/plots_alpha0.4/",resp.table$Date[1],"_",sub.folder.list[i],"_",colnames(Resp.Data[k]),"_regression.pdf"))
      plot(model)
      dev.off()
      
      resp_total <- rbind(resp_total,resp.table) #bind to a cumulative list dataframe
    } # end of inside for loop
    runs_total <- rbind(runs_total, resp_total)
  }
  df_total <- rbind(df_total, runs_total)
  print(df_total[nrow(df_total),]) # print last row to monitor progress
}

ouputNAME<-"~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/Cumulative_resp_alpha0.4.csv" 

write.table(df_total,ouputNAME,sep=",", row.names=FALSE) # write out to the path names outputNAME




Resp.Data <- data.frame(read_xlsx("~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/20190905_SDR/round1/round1_710_OXY.xlsx", skip = 12))





Resp.Data$Time_Min <- seq(0.25, (nrow(Resp.Data))*0.25, by=0.25)
Resp.Data$Date.Time <- as.POSIXct(strptime(Resp.Data$Date.Time, format ="%m.%d.%y %H:%M:%S"))




a <- 0.4

df_total <- data.frame()
resp.table <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'Lpc', 'Leq' , 'Lz', 'alpha') # names for comuns in the for loop

for(j in 3:26){
  model <- rankLocReg(
    xall=Resp.Data$Time_Min, yall=Resp.Data[, j],
    alpha=a, method="pc", verbose=TRUE) # run the LoLin script
  sum.table<-summary(model)
  resp.table$Date <- unique(format(as.POSIXct(Resp.Data$Date.Time,format="%m.%d.%y %H:%M:%S"),format="%m.%d.%y")) # all files have date in the form of yyyymmdd at the start of each csv name
  resp.table$RUN <- NA
  #resp.table$RUN <- substr(file.names[i], 15,15) # assign the run to the number in the title for the trials completed that day
  resp.table$SDR_position <- colnames(Resp.Data[j]) # assign the vial position - this will be related to contents (blank or individuals) later in script
  resp.table$alpha <- a # set at start of script - reresents the proportion of data for final estimate of slopes (Lpc, Leq, Lz)
  resp.table$Lpc <-sum.table$Lcompare[3,6] # Lpc slope 
  resp.table$Leq <-sum.table$Lcompare[2,6] # Leq slope 
  resp.table$Lz <-sum.table$Lcompare[1,6]  # Lz slope 
  #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
  #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
  #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]
  
  df_total <- rbind(df_total,resp.table) #bind to a cumulative list dataframe
  print(df_total) # print to monitor progress
  
  # save plots every inside loop and name by date_run_vialposition
  pdf(paste0("C:/Users/samjg/Documents/My_Projects/Inragenerational_thresholds_OA/RAnalysis/Data/SDR_data/All_resp_data/plots_alpha0.4/",substr(file.names[i], 1,8),"_", "RUN",substr(file.names[i], 15,15),"_",colnames(Resp.Data[j]),"_regression.pdf"))
  #plot(model)
  #dev.off()
} # end of inside for loop