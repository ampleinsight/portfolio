#####Data Wrangling with the tidyverse

### install the tidyverse if you don't have it installed. You only have to do this once.
#install.packages("tidyverse")

###load the tidyverse functions #### Do this everytime you want to use tidyverse commands
library(tidyverse)
library(dplyr)
library(scales)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Documents")


FAO = read_csv("UNFAO_by_Year.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")

#Fix year format
FAO$year = as.numeric(FAO$Year)

#Filter to get countries
FAO_Pop = filter(FAO, Indicator ==  "Total population"  & IsCountry == "Y")

#Get latest value available for each country
FAO_Pop_Max = FAO_Pop %>% group_by(Regions.Subregions.Countries) %>%
  slice(which.max(Year)) %>%
  select(Region, Regions.Subregions.Countries, Value)


#Ungroup then regroup at Region level for totals
FAO_Pop_Max_Ungp = FAO_Pop_Max %>% ungroup()
FAO_Pop_Max_Gp_Region = FAO_Pop_Max_Ungp %>% group_by(Region)
FAO_Pop_Max_Sum = summarise(FAO_Pop_Max_Gp_Region, Total_Pop = sum(Value, na.rm = TRUE))
FAO_Pop_Max_Sum = FAO_Pop_Max_Sum %>% mutate(Total_Pop2 = comma(Total_Pop))
        
####make a lollipop plot -- Then, edit in Inkspace to add commas and move labels to right of lollipop



lol_plot= 
  ggplot(FAO_Pop_Max_Sum,aes(x=Region,y = Total_Pop, fill=Region))+
  geom_segment(aes(x=Region, xend = Region, y=0, yend = Total_Pop), color = "gray")+
  geom_point(size = 3, color = "#69b3a2")+
  geom_text(aes(label = Total_Pop),nudge_x = -0.2, size = 3)+
  coord_flip()+
  theme_classic() +
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = "null", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.subtitle = element_text(size = 8))+
  scale_fill_brewer(palette="Set1")+
  labs(x="Population",y="Region",
       title="Population Distribution by World Region (2017)",caption="Source: UN Food and Agriculture Organization")

lol_plot

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("myplot.svg", plot=lol_plot)

#Make a column chart to show pop change by region over select years

#Get latest value available for each country
FAO_Pop_Years = FAO_Pop %>% 
  filter(Year == 2000 | Year == 2010 | Year == 2017)%>%
  select(Region, Regions.Subregions.Countries, Year, Value)%>%
  group_by(Region, Year)%>%
  summarise(Total_Pop = sum(Value, na.rm = TRUE))
  #not working right...comes out as Character format: mutate(Total_Pop2 = comma_format()(Total_Pop))


#Make bar chart showing pop growth over time
bar_plot=
ggplot(FAO_Pop_Years, aes(x=Region, y = Total_Pop, width=0.45, fill = Year))+
  geom_bar(stat="sum", position="dodge")+
  coord_flip()+
  theme_classic()+
  scale_fill_brewer(palette = "PuBuGn")
  #scale_fill_manual(values=c("#abb0af","#98b3ad","#69b3a2"))+
  theme(axis.line.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_text(hjust = 0), axis.title.y = element_blank())+
  labs(y="Population (M)",
       title="Population Growth by World Region (2017)",caption="Source: UN Food and Agriculture Organization")

bar_plot

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("mybar2.svg", plot=bar_plot)

#Make a Heat Map for access to clean water by region
#Get values available for each country

#Filter to get countries
FAO_Water = filter(FAO, Indicator ==  "Percentage of people using safely managed drinking water services"  & IsCountry == "Y")

FAO_Water_Dat = FAO_Water %>% 
  select(Region, Regions.Subregions.Countries, Year, Value)


#Ungroup then regroup at Region level for totals by region
FAO_Water_Dat_Ungp = FAO_Water_Dat %>% ungroup()
FAO_Water_Dat_Gp_Region = FAO_Water_Dat_Ungp %>% group_by(Region, Year)
FAO_Water_Dat_Sum = summarise(FAO_Water_Dat_Gp_Region, Avg_H20_Access = median(Value, na.rm = TRUE))

#Recode Region Names for easier reading
FAO_Water_Dat_Sum$Region_Recode <- recode(FAO_Water_Dat_Sum$Region,"Latin America And The Caribbean"="Latin & Caribbean","Northern America And Europe"="N. America & Europe", "Africa" = "Africa", "Asia" = "Asia", "Oceania"="Oceania")


####make a heat map
library(RColorBrewer)

plot2 = 
  ggplot(FAO_Water_Dat_Sum, aes(x=Year, y=Region_Recode, fill= Avg_H20_Access)) + 
  geom_tile()+
  #coord_flip()+
  scale_fill_distiller('Percent Access', palette = "RdBu", direction = 1)+
  # Paired Accent Blues
  theme_classic()+
  theme(axis.title.y = element_blank(), axis.line = element_blank(), plot.caption = element_text(hjust = -1.75, vjust = -3), axis.text.x = element_text(angle = 90, hjust = 0, size = 11), axis.title.x = element_blank(), axis.text.y = element_text(size=11))+
  labs(title = "Chart B. Percent of People by Region with Access to Clean Water", subtitle = "Clean water access has improved, but variation by region persists", caption = "Source: UN Food and Agriculture Organization. Tiles are median values.")

plot2

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("myplot3.svg", plot=plot2)


