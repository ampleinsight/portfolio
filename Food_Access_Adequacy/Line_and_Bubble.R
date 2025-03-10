library(tidyverse)
library(scales)

setwd("/Users/ahmedkhan/Documents")

#NILS = read_csv("WHO_NILS_ALL.csv")
FAO = read_csv("UNFAO_by_Year.csv")
#GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")

#Drop weird column "X23"
#FAO = select (FAO, -X23)

#Fix year format
#FAO$year = as.numeric(FAO$year)

#head(FAO)
#head(NILS)
#head(GFSI)

#unique(FAO$Indicator)



#Global Level
FAO_Avg_Diet_Adeq = filter(FAO, Indicator =="Average dietary energy supply adequacy" & Regions.Subregions.Countries == "WORLD")

      #Get GDP by region for later chart below
      FAO_GDP = filter(FAO, Indicator == "Gross domestic product per capita - in purchasing power equivalent"
                       &
                         (
                           Regions.Subregions.Countries == "NORTHERN AMERICA AND EUROPE"|
                             Regions.Subregions.Countries == "LATIN AMERICA AND THE CARIBBEAN"|
                             Regions.Subregions.Countries == "OCEANIA"|
                             Regions.Subregions.Countries == "ASIA"|
                             Regions.Subregions.Countries == "AFRICA"|
                             Regions.Subregions.Countries == "WORLD"
                         )
      )

#Fix formats
fig_data_FAO_Diet_Adeq = tibble("Year"=as.numeric(FAO_Avg_Diet_Adeq$Year), "Value" = FAO_Avg_Diet_Adeq$Value)

fig_data_FAO_GDP_Latest = tibble("Region" =FAO_GDP$Regions.Subregions.Countries, "Year"=as.numeric(FAO_GDP$Year), "GDP" = FAO_GDP$Value) %>%
  group_by(Region) %>%
  #slice(which.max(Year)) %>%
  select(Region, Year, GDP)


#str(fig_data_FAO_Diet_Adeq)

ggplot(fig_data_FAO_Diet_Adeq, aes(x=Year, y=Value)) + 
  geom_line(color = "orange", size = 0.5) +
  theme_classic() +
  theme(plot.title = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
  xlim(2000,2020) + #ylim(110,120) +
  #scale_y_continuous(labels = percent_format(accuracy = 1))
  labs(x="Year",y="% of Dietary Need Met",title="World Food Adeqancy Rate Trends (2000-2017)", subtitle = "Percent of Overall Dietary Need Met at the Global Level", caption = "Source: UN Food and Agriculture Organization")



##Regional Level

FAO_Avg_Diet_Adeq_Reg = 
  filter(FAO, Indicator =="Average dietary energy supply adequacy" & 
           (
             Regions.Subregions.Countries == "NORTHERN AMERICA AND EUROPE"|
               Regions.Subregions.Countries == "LATIN AMERICA AND THE CARIBBEAN"|
               Regions.Subregions.Countries == "OCEANIA"|
               Regions.Subregions.Countries == "ASIA"|
               Regions.Subregions.Countries == "AFRICA" |
               Regions.Subregions.Countries == "WORLD"
           )
  )

fig_data_FAO_Diet_Adeq_Reg = tibble("Year"=as.numeric(FAO_Avg_Diet_Adeq_Reg$Year), "Value" = FAO_Avg_Diet_Adeq_Reg$Value, "Region" = FAO_Avg_Diet_Adeq_Reg$Regions.Subregions.Countries)

#Bring in GDP as well
fig_data_FAO_Diet_Adeq_Reg_GDP = inner_join(fig_data_FAO_Diet_Adeq_Reg, fig_data_FAO_GDP_Latest, by = c("Region", "Year"), copy = FALSE)

#Goal: Color or size by GDP -- or at least add text for GDP

#Recode Region Names for easier reading
fig_data_FAO_Diet_Adeq_Reg_GDP$Region_Recode <- recode(fig_data_FAO_Diet_Adeq_Reg_GDP$Region,"LATIN AMERICA AND THE CARIBBEAN"="Latin/Caribbean","NORTHERN AMERICA AND EUROPE"="N.America/Europe", "AFRICA" = "Africa", "ASIA" = "Asia", "OCEANIA"="Oceania", "WORLD" = "World")

library(RColorBrewer)
ggplot(fig_data_FAO_Diet_Adeq_Reg_GDP, aes(x=Year, y=Value, color = Region_Recode)) + 
  geom_line() + 
  theme_classic() +
  theme(axis.line = element_line(size = 0.3), legend.title = element_blank(), plot.title = element_text(face="bold", size = 12), axis.text = element_text(size = 10), legend.position="bottom")+
  xlim(2000,2020) +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) Just divide Y aesthetic by 100 to use this line
  labs(x="Year",y="% of Dietary Need Met",title="Food Adeqancy Rate Trends by Region (2000-2017)", subtitle = "Percent of Dietary Need Met by Regional Level") +
  scale_color_brewer(palette = "Dark2")


library(viridis)
minemap = 
  ggplot(fig_data_FAO_Diet_Adeq_Reg_GDP, aes(x=Year, y=Value, size=GDP, color=Region)) +
  geom_point(alpha=0.5)+
  #geom_smooth(method = "loess", se= FALSE, size = 0.25)+
  theme_classic() +
  theme(axis.line = element_line(size = 0.3), legend.title = element_blank(), plot.title = element_text(face="bold", size = 12), axis.text = element_text(size = 10), legend.position="bottom")+
  xlim(2000,2017) +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) Just divide Y aesthetic by 100 to use this line
  labs(x="Year",y="% of Dietary Need Met",title="Food Adeqancy Rate Trends by Region (2000-2017)", subtitle = "Percent of Dietary Need Met by Regional Level") +
  scale_fill_viridis(guide=FALSE)+
  scale_size(name="GDP")
  
minemap

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("Week11_LinearTrendsGDP2.svg", plot=minemap, width=8, height=6)

