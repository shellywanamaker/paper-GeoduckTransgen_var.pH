install.packages("readxl")
library(readxl)
fileName <- "20191018_ShellLength.xlsx"
#code from: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
#define the function for reading in the excel file with multiple tabs
read_excel_allsheets <- function(fileName, tibble = FALSE) {
  sheets <- readxl::excel_sheets(fileName)
  x <- lapply(sheets, function(X) readxl::read_excel(fileName, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
# create R object for excel file 
sheets <- read_excel_allsheets("20191018_ShellLength.xlsx")
list2env(sheets ,.GlobalEnv)
#create data frame from excel datafile
dt <- data.frame()
for(i in 1:length(sheets)){
  dtemp <- data.frame(sheets[i])
  dtemp$Image <- names(sheets)[i]
  colnames(dtemp) <- c("Area","Mean", "Min", "Max", "Angle", "Length (inch)", "Length (cm)", "Image")
  dt <- rbind(dt, dtemp)
}
# add Silo Information 
dt$Silo <- gsub(".*_","",dt$Image)
#Read in the parental history and treatment conditions
meta_data <- read_excel("Fall2019_AmbxVar.Low_juv_MetaData.xlsx")
colnames(meta_data)[1] <- "Silo"
dt <- merge(dt,meta_data, by = "Silo")
dt$parental_currenttreatment <- paste(dt$Parental.history,dt$Sw.Condition,sep = "_")
# installing ggplot2
install.packages("ggplot2")
library(ggplot2)
# creating violin and box plot to show size distributions across different treatments and histories
ggplot(dt,aes(x = parental_currenttreatment, y = Length (cm), fill = Sw.Condition)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.15) + geom_jitter(shape = 16, size = 2, position = position_jitter(0.2)) + theme_bw()
#Seems like R doesn't know that "Length (cm)" is numeric data, so check data structure
str(dt)
#change column name so there aren't problems
colnames(dt)[8] <- "Length.cm"
#plot with simplified length measurement column name
ggplot(dt,aes(x = parental_currenttreatment, y = Length.cm, fill = Sw.Condition)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.15) + geom_jitter(shape = 16, size = 2, position = position_jitter(0.2)) + theme_bw()
# test code - seeing if Sw.Condition and Parental.history have an affect on length 
a <- aov(Length.cm ~ Sw.Condition*Parental.history, data = dt)
summary(a)
# comparing each combination, and seeing which combination is significantly different 
a_tuk <- TukeyHSD(aov(Length.cm ~ Sw.Condition*Parental.history, data = dt))
summary(a_tuk)
a_tuk
jpeg("Density Plot of Experimental Groups.jpeg", width = 8, height = 8, units = "in", res = 300)
# plot density graph
ggplot(dt) + geom_density(aes(Length.cm, group = parental_currenttreatment, color = parental_currenttreatment, fill = parental_currenttreatment), alpha = 0.2) + theme_bw() + ggtitle("Size Distribution of Juvenile Geoduck Treatment Groups")
dev.off()
# plot histogram
ggplot(dt) + geom_histogram(aes(Length.cm, group = parental_currenttreatment, color = parental_currenttreatment, fill = parental_currenttreatment), bins = 10, position = "identity", alpha = 0.2) + theme_bw()
