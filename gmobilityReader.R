library(tidyverse)
library(lubridate)
library(ggrepel)

#read csv mobility data
gmobilityReport <- read.csv("./rdata/Global_Mobility_Report.csv",stringsAsFactors = FALSE) %>% 
  mutate(date = ymd(date)) #read the mobility data

retrieveByRegion <- function(country,subregion1 = "") { #select mobility data based on country and subregion1
  #select data based on provided country and subregion
  gmobilityReport$sub_region_1[gmobilityReport$sub_region_1 == ''] <- country
  
  #filter and clean the resulting dataset
  gmobilityReport %>% 
    filter(country_region == country,sub_region_1 == subregion1 | sub_region_1 == country) %>% 
    setNames(c("code","country","subregion","subregion2","date","retail_and_recreation","grocery_and_pharmacy","parks","transit_stations","workplaces","residential")) %>% 
    select(-subregion2) %>%
    gather("location","vs_baseline",c(retail_and_recreation,grocery_and_pharmacy,parks,transit_stations,workplaces,residential)) %>% 
    mutate(as.factor(location))
}

plotAndSave <- function(country,subregion = "") { #retrieve and save the data
  regionToPlot <- retrieveByRegion(country,subregion) #retrieve the regions within a country
  colorToPlot <- c("darkred","#008B8B") %>% setNames(c(subregion,country)) #set color matrix with complementary color scheme
  regionToPlot %>% group_by(location) %>% #plot the charts
    ggplot(aes(x=date,y=vs_baseline,color=subregion)) + 
    theme(legend.position="bottom") + 
    labs(color = "Region") +
    geom_line() +
    scale_y_continuous(breaks=seq(-100,50,25),limits=(c(-100,50))) + 
    scale_color_manual(values = colorToPlot,limits=names(colorToPlot)) +
    geom_vline(xintercept=ymd("2020-03-02"),linetype=4, colour="black",alpha=0.3) + #first Covid-19 case found in Indonesia
    geom_vline(xintercept=ymd("2020-04-24"),linetype=4, colour="black",alpha=0.3) + #first day of Muslim fasting period
    geom_vline(xintercept=ymd("2020-05-23"),linetype=4, colour="black",alpha=0.3) + #Eid-Al-Fitr day
    facet_wrap(~location,ncol=3,nrow=2) + xlab("Date") + ylab("% vs baseline")
  ggsave(paste("./data/",str_replace_all(tolower(subregion),"\\s",""),".png",sep="")) #save it by region name
}

#test using provinces in Indonesia
indonesiaRegions <- gmobilityReport %>% filter(country_region == "Indonesia") %>% .$sub_region_1 %>% unique() %>% .[-1]
mapply(plotAndSave,"Indonesia",indonesiaRegions)