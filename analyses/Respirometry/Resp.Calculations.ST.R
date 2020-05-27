## load libraries
library(readxl)
library(devtools)
library(dplyr)
library(ggplot2)

#Load Size Info
Size.Info <- read_xlsx("~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/Shell_length_data.xlsx") #read Size.info data
Size.Info$Date <- format(as.POSIXct(strptime(Size.Info$Date, format ="%Y%m%d")),format="%m.%d.%y")

# call the cumulative resp table of Lolin raw outputs
cumulative_resp_table <- read.csv(file="~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/Cumulative_resp_alpha0.4.csv", header=T, stringsAsFactors = FALSE) #read Size.info data
#cumulative_resp_table <- na.omit(cumulative_resp_table)
cumulative_resp_table$Date <- format(as.POSIXct(cumulative_resp_table$Date,format="%m.%d.%y"),format="%m.%d.%y")


# call the Size info of shell length and number of individuals per trial ( Size info called above)
x <- merge(cumulative_resp_table, Size.Info, by=c("Date","SDR_position", "Run"))


########################################### #
# SECTION II - analyze blanks  ############ #
########################################### #


blanks_total <- x[which(x$Type == "blank"),]

blanks_total$ID <- paste(blanks_total$Date,blanks_total$Run, blanks_total$SDR_position, sep = ".")

ggplot(blanks_total, aes(x = ID, y = Lpc, group = Date)) + geom_boxplot() + facet_wrap(~Sw.Condition)
ggplot(blanks_total[which(blanks_total$SDR_position != "A1"),], aes(x = ID, y = Lpc, group = Date)) + geom_boxplot() + facet_wrap(~Sw.Condition)

test <- blanks_total %>% group_by(Date,Run,Sw.Condition) %>% summarise(n = n(), mean.lpc = mean(Lpc), mean.leq = mean(Leq), mean.lz = mean(Lz), sd.lpc = sd(Lpc), sd.leq = sd(Leq), sd.lz = sd(Lz))

#exclude blanks that seem weird. These are values that are throwing off the standard deviations and have plots that look pretty different than the other blanks

blanks_total_filtered <- blanks_total[which(blanks_total$ID != "09.05.19.1.C5" & blanks_total$ID != "09.05.19.1.B5" & blanks_total$ID != "09.05.19.2.B5" & blanks_total$ID != "09.06.19.1.A6" & blanks_total$ID != "09.19.19.1.A1"),]

avg.blank <- blanks_total_filtered %>% group_by(Date,Run) %>% summarise(n = n(), mean.lpc = mean(Lpc), mean.leq = mean(Leq), mean.lz = mean(Lz), sd.lpc = sd(Lpc), sd.leq = sd(Leq), sd.lz = sd(Lz))


##################################################### #
# SECTION IV - calc respiration rate  ############### #
##################################################### #

#----------------------------Respiration rate calculation -------------------------------------------

CALC.table <- merge(avg.blank[,c("Date","Run", "mean.lpc")],x, by=c("Date","Run")) # NOTE: this repeats for every distinct length value
#remove lines with NA
CALC.table <- CALC.table[which(CALC.table$Wet.weight.g != "NA"),]
#convert length and wet weight fields to numeric
CALC.table$Length <- as.numeric(CALC.table$Length)
CALC.table$Wet.weight.g <- as.numeric(CALC.table$Wet.weight.g)

# NOTE: the raw data is in umol L-1 4ml-1
# "resp.RAW.µgLhr" calculation = ((((((abs(Lpc)) - (BLANK.mean.Lpc))*(4/1000))*(60))*31.998))
# (1) corrects for blank (2) converts to Liters (3) converts to hours (4) converts moles to grams; final unit = µg O2 L-1 h-1


for(i in 1:nrow(CALC.table)) {
  CALC.table$resp.RAW.µgLhr[i] <-((((((abs(CALC.table[i,"Lpc"])) - (CALC.table[i,"mean.lpc"]))*(4/1000))*(60))*31.998))
  CALC.table$resp.shell[i] <- CALC.table$resp.RAW.µgLhr[i]/CALC.table[i,"Length"] #per cm shell length
  CALC.table$resp.weight[i] <- CALC.table$resp.RAW.µgLhr[i]/CALC.table[i,"Wet.weight.g"] #per gram wet weight
}


#plot raw data

#create a group category
CALC.table$hist.curr.treat <- paste(CALC.table$Parental.history,CALC.table$Sw.Condition, sep = "_")

ggplot(CALC.table, aes(x = Parental.history,y = resp.RAW.µgLhr, fill = Sw.Condition)) + geom_boxplot() +facet_wrap(~Date) + theme_bw()
ggplot(CALC.table, aes(x = Parental.history,y = resp.shell, fill = Sw.Condition)) + geom_boxplot() +facet_wrap(~Date) + theme_bw()
ggplot(CALC.table, aes(x = Parental.history,y = resp.weight, fill = Sw.Condition)) + geom_boxplot() +facet_wrap(~Date) + theme_bw()


ggplot(CALC.table, aes(x = hist.curr.treat,y = resp.RAW.µgLhr, fill = Sw.Condition)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.15) + geom_jitter(shape =16, size = 2,position= position_jitter(0.05)) +facet_wrap(~Date) + theme_bw() + ggtitle("Respirometry data") + ylab("ug O2/L*hr") + xlab("parental_current treatment")

ggplot(CALC.table, aes(x = hist.curr.treat,y = resp.shell, fill = Sw.Condition)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.15) + geom_jitter(shape =16, size = 2,position= position_jitter(0.05)) +facet_wrap(~Date) + theme_bw() + ggtitle("Respirometry data normalized by shell length") + ylab("ug O2/L*hr*shell length") + xlab("parental_current treatment")

ggplot(CALC.table, aes(x = hist.curr.treat,y = resp.weight, fill = Sw.Condition)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.15) + geom_jitter(shape =16, size = 2,position= position_jitter(0.05)) +facet_wrap(~Date) + theme_bw() + ggtitle("Respirometry data normalized by wet weight") + ylab("ug O2/L*hr*gram.wet.weight") + xlab("parental_current treatment")



### anova

#run anova for non-normalized data
a <- aov(resp.RAW.µgLhr ~ Parental.history, data = CALC.table[which(CALC.table$Date == "09.05.19"),])
summary(a)

b <- aov(resp.RAW.µgLhr ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.06.19"),])
summary(b)

c <- aov(resp.RAW.µgLhr ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.19.19"),])
summary(c)

#run anova for data normalized to shell length

a <- aov(resp.shell ~ Parental.history, data = CALC.table[which(CALC.table$Date == "09.05.19"),])
summary(a)

b <- aov(resp.shell ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.06.19"),])
summary(b)

c <- aov(resp.shell ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.19.19"),])
summary(c)

#run anova for data normalized to wet weight
a <- aov(resp.weight ~ Parental.history, data = CALC.table[which(CALC.table$Date == "09.05.19"),])
summary(a)

b <- aov(resp.weight ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.06.19"),])
summary(b)

c <- aov(resp.weight ~ Sw.Condition*Parental.history, data = CALC.table[which(CALC.table$Date == "09.19.19"),])
summary(c)

TukeyHSD(c)




#----------------------OUTPUT - save plots and cumulative tables-----------------------------------------

write.csv(blanks_total,"~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/All.blank.resp.rates.csv", row.names=FALSE, quote = FALSE)
write.csv(CALC.table,"~/Documents/GitHub/P_generosa/amb_v_varlowpH_juvis/data/SDR/Final.resp.rates.csv", row.names=FALSE, quote = FALSE) 