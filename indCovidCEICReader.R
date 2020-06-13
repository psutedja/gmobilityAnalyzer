library(tidyverse)
library(readxl)

ma <- function(x,n=7){
  as.numeric(stats::filter(x,rep(1/n,n), sides=1))
} #define moving average function

covid19ind <- read_excel("./rdata/Covid_indonesia.xlsx") %>% as.data.frame() #read raw data, return data frame
covid19ind <- covid19ind[-seq(1,10,1),] #delete row 1 - 10

#convert all but first column to numeric
date <- as.Date(as.numeric(covid19ind[[1]]),origin="1900-01-01")
tempNum <- apply(covid19ind[,-1],2,as.numeric) %>% as.data.frame() 
covid19ind <- bind_cols(as.data.frame(date),tempNum)

#some data wrangling
theNames <- names(covid19ind)
theNames <- str_replace(theNames,"Ministry of Health: COVID-19: Number of Cases: To-Date: ","")
theNames <- str_replace(theNames,"Negative Specimen Twice Follow-ups","Recovered")
names(covid19ind) <- theNames
  
names(covid19ind)[1] <- "date" #set first colum name as date
covid19ind <- covid19ind %>% gather("data","number_of_cases",-date,na.rm=TRUE) %>% mutate(data = as.factor(data)) #convert into tidy data
covid19ind <- covid19ind %>% separate(data,c("category","region"),":\\s",fill="right") # split region from data
covid19ind <- covid19ind %>% mutate(region = replace_na(region,"National")) #replace all NA with National

#function to print table
printByRegion <- function(whichReg = "National") {
  covid19ind %>% 
    filter(region == whichReg & category != "Checking In Progress") %>% 
    group_by(category) %>% 
    mutate(addition=number_of_cases - lag(number_of_cases),MA = ma(addition)) %>% 
    ggplot(aes(x=date)) + ggtitle(whichReg) +
    geom_line(aes(y=addition)) + 
    geom_line(aes(y=MA),color="red") + 
    facet_grid(category ~ .,scales="free",labeller = labeller(category = label_wrap_gen(10))) +
    ylab("Addition per day") + 
    xlab("Date") +
    scale_x_date(date_breaks = "4 weeks ", date_minor_breaks = "1 week", date_labels = "%d-%b",limits = c(min(covid19ind$date),max(covid19ind$date)))
  
  ggsave(paste("./data/covid19cases/",str_replace_all(tolower(whichReg),"\\s",""),".png",sep="")) #save it by region name
}

#test using all regions in Indonesia, including national figures
indonesiaRegions <- covid19ind %>% .$region %>% unique()
suppressWarnings(sapply(indonesiaRegions,printByRegion))

# covid19ind %>% group_by(region,category) %>% filter(date==max(date)) %>% filter(category=="Confirmed Cases" | category=="Deceased") %>% spread(category,number_of_cases) %>% summarize("Case Fatality Rate" = Deceased/ `Confirmed Cases`,`Confirmed Cases`) %>% ggplot(aes(x=reorder(region,-`Case Fatality Rate`),y=`Case Fatality Rate`)) + geom_col() + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_y_continuous(labels=scales::percent)