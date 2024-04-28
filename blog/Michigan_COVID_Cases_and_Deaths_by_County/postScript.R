
# Load packages -----------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(plotly)
library(RColorBrewer)

# Make the base state map -------------------------------------------------
counties <- map_data("county")
mi_county <- subset(counties, region == "michigan")


# Data prep ---------------------------------------------------------------

micovid <- read.xlsx("https://www.michigan.gov/coronavirus/-/media/Project/Websites/coronavirus/Michigan-Data/10-18-2022/Cases-and-Deaths-by-County-2022-10-18.xlsx?rev=fef915b1c77e4fda8885f51bf9aeb9b3&hash=21A85A12F8370622EE29ECBD20378569")

micovid <- micovid %>%
  group_by(COUNTY) %>%
  summarise(total_cases = sum(Cases),
            total_deaths = sum(Deaths)) %>%
  ungroup() %>%
  mutate(subregion = tolower(COUNTY))

cases_and_county <- inner_join(mi_county, micovid, by = "subregion")
cases_and_county <- cases_and_county %>%
  rename(county = COUNTY)



cases_and_county<- cases_and_county %>%
  mutate(Category = case_when(total_cases < 1000 ~ '0-999', 
                              total_cases < 5000 ~ '1000-4999',
                              total_cases <15000 ~ '5000-14999',
                              total_cases < 30000 ~ '15000-29999',
                              total_cases < 100000~ '30000-99999',
                              total_cases < 1000000 ~ '100000+',
                              TRUE ~ 'NA'))

cases_and_county$Category <- as.factor(cases_and_county$Category)

lvls <- c('0-999', 
          '1000-4999',
          '5000-14999',
          '15000-29999',
          '30000-99999',
          '100000+')

cases_and_county$Category <- fct_relevel(cases_and_county$Category, lvls)

# Making the map ----------------------------------------------------------

p <- c("#ACD1E7", "#82BADC", "#59A1CF", "#236893","#174562", "#122548")

label <- list(
  bgcolor = "#EEEEEE",
  font = list(color = "black")
)

noax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

g <- cases_and_county %>%
  ggplot(aes(long, lat, 
             group = group,
             text = paste('</br>County:', county, '</br>Category:', Category,
                          '</br>Cases:', total_cases)))+
  geom_polygon(colour = alpha("black", 1/2), fill = NA) +
  geom_polygon(data = cases_and_county, colour = "black", aes(fill = Category))+
  theme_void() +
  scale_fill_manual(values = p) 

ggplotly(g, tooltip = c("text"), width = 700, height = 600) %>%
  layout(xaxis = noax,
         yaxis = noax) %>%
  style(hoverlabel = label) %>%
  config(displayModeBar = FALSE)



# leaflet -----------------------------------------------------------------


library(sp)
library(raster)
library(rgdal)
library(leaflet)

miCounties <- readOGR("posts/Michigan COVID Cases and Deaths by County/Counties_(v17a)", "Counties_(v17a)")

micovid <- micovid %>%
  mutate(NAME = case_when(
    COUNTY == "St Clair" ~ "St. Clair",
    COUNTY == "St Joseph" ~ "St. Joseph",
    TRUE ~ COUNTY
  ))

combined <- merge(miCounties, micovid)


# pals and labels for each map -------------------------------------------------------

case_bins <- c(0, 1000, 5000, 15000, 30000, 100000, Inf)
case_pal <- colorBin("Blues", domain = combined$total_cases, bins = case_bins)

case_labels <- sprintf(
  "<strong>%s</strong><br/>Cases: %g",
  combined$NAME, combined$total_cases
) %>% lapply(htmltools::HTML)

death_bins <- c(0, 50, 100, 150, 500, 1500, 3000, Inf)

death_pal <- colorBin("Reds", domain = combined$total_deaths, bins = death_bins)

death_labels  <- sprintf(
  "<strong>%s</strong><br/>Deaths: %g",
  combined$NAME, combined$total_deaths
) %>% lapply(htmltools::HTML)

leaflet() %>% 
  addTiles(group = "base") %>%
  addPolygons(data = combined,
              group = "Cases",
              fillColor = ~case_pal(total_cases),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = case_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(data = combined,
            title = "Cases",
            pal = case_pal, values = ~total_cases, opacity = 0.7,
            position = "bottomright", group = "Cases") %>%
  addPolygons(data = combined,
              group = "Deaths",
              fillColor = ~death_pal(total_deaths),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = death_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(data = combined, 
            title = "Deaths",
            pal = death_pal, values = ~total_deaths, opacity = 0.7,
            position = "bottomright", group = "Deaths") %>%
  addLayersControl(overlayGroups = c("Cases", "Deaths"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Deaths")


