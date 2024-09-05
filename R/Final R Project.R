#Input Data
library(readxl)
df <- read_excel("D:/2023/School/St Johns/Fall 2023/Data Visualization/R Project/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
View(df)

#Load Packages
library(ggplot2)
library(plotly)
library(dplyr)

#Normalization

keeps <- c("Race", "Sex","Location", "InjuryPlace", "DeathCounty", 
           "Age", "MannerofDeath", "AnyOpioid")
subset <- df[ , keeps, drop = FALSE]


#Dyplyr
df2 <- subset %>% mutate(age_group = dplyr::case_when(
  Age >= 10 & Age < 20  ~ "10-19",
  Age >= 20 & Age < 30 ~ "20-29",
  Age >= 30 & Age < 40 ~ "30-39",
  Age >= 40 & Age < 50 ~ "40-49",
  Age >= 50 & Age < 60 ~ "50-59",
  Age >= 60 & Age < 70 ~ "60-69",
  Age >= 70 & Age < 80 ~ "70-79",
  Age >= 80 & Age < 90 ~ "80-89"))

df2 <- df2 %>% mutate(Race = as.factor(Race),
                      Sex = as.factor(Sex),
                      Location = as.factor(Location),
                      InjuryPlace = as.factor(InjuryPlace),
                      DeathCounty = as.factor(DeathCounty),
                      MannerofDeath = as.factor(MannerofDeath),
                      AnyOpioid = as.factor(AnyOpioid),
                      age_group = as.factor(age_group)
                      
) 
summary(df2)

#Main Graphs
#Sex
df2_filterSex <- df2[!is.na(df2$Sex), ]

colorsSex <- c("#443c68", "#576cbc", "#cfe2f3")

figSex <- plot_ly() %>%
  add_pie(data = df2_filterSex, labels = ~Sex, values = ~n, name = "Sex", 
          textinfo='label+value+percent', marker = list(colors = colorsSex)) %>%
  layout(title = "Accidental Drug Related Deaths by Sex", showlegend = TRUE)

figSex

#Location
figLoc <- plot_ly(df2, y = ~Location, type = "histogram",
                   color = ~Location, 
                   colors =c("black", "#393646", "#443c68", 
                             "#576cbc", "#9bc5ef", 
                             "#cfe2f3", "#d0e0e3", "#bcbcbc",)) %>%
  layout(title = "Accidental Drug Related Deaths by Location", showlegend = TRUE,
         xaxis = list(title = "", showgrid = TRUE), yaxis = list(title = "",
                                                                 showgrid = FALSE))

figLoc


df2_filterSex2 <- df2[df2$Sex != "Unknown", ]
figLocSex <- plot_ly(df2_filterSex2, x = ~Sex, type = "histogram", 
                   color = ~Location,
                   colors =c("black", "#393646", "#443c68", 
                             "#576cbc", "#9bc5ef", 
                             "#cfe2f3", "#d0e0e3", "#bcbcbc")) %>%
  layout(title = "Accidental Drug Related Death Locations by Sex", 
         showlegend = TRUE)

figLocSex

figLocMaleRace <- plot_ly(df2[df2$Sex == "Male", ], y = ~Race, type = "histogram", 
                       color = ~Location, 
                       colors = c("black", "#393646", "#443c68", 
                                  "#576cbc", "#9bc5ef", 
                                  "#cfe2f3", "#d0e0e3", "#bcbcbc")) %>%
  layout(title = "Male Accidental Drug Related Death Locations by Race", 
         showlegend = TRUE)

figLocMaleRace

figLocFemaleRace <- plot_ly(df2[df2$Sex == "Female", ], y = ~Race, type = "histogram", 
                         color = ~Location,
                         colors = c("black", "#393646", "#576cbc", 
                                    "#576cbc", "#d0e0e3", "#bcbcbc"))%>%
  layout(title = "Female Accidental Drug Related Death Locations by Race", 
         showlegend = TRUE)
figLocFemaleRace

#Age
figAge <- plot_ly(df2, x = ~age_group, type = 'histogram', 
                  color = ~age_group, 
                  colors =c("black", "#393646", "#443c68", "blue2",
                            "#576cbc", "#9bc5ef", "#cfe2f3", "cyan"))%>%
  layout(title = "Accidental Drug Related Death Locations by Age (10 Year Segments)",
         xaxis = list(title = "Age Groups", showgrid = FALSE), yaxis = list(title = "",
         showgrid = TRUE), showlegend = TRUE)

figAge

figAgeSex <- plot_ly(df2, x = ~age_group, type = 'histogram',
                     color = ~Sex, colors = c("#cfe2f3", "#443c68"))%>%
  layout(title = "Age of Accidental Drug Related Death Locations by Sex", 
         showlegend = TRUE, xaxis = list(title = "Age Groups", showgrid = FALSE), 
         yaxis = list(title = "", showgrid = TRUE), showlegend = TRUE)


figAgeSex

figAgeRace <- plot_ly(df2, y = ~Race, type = 'histogram',
                      color = ~age_group,
                      colors = c("#eeeeee", "#d0e0e3", "#443c68", 
                                 "#576cbc", "#9bc5ef", 
                                 "#cfe2f3", "blue2", "#bcbcbc"))%>%
  layout(title = "Race of Accidental Drug Related Death Locations by Age Groups", 
         showlegend = TRUE, xaxis = list(title = "", showgrid = FALSE), 
         yaxis = list(title = "", showgrid = TRUE), showlegend = TRUE)

figAgeRace

#Injury Place

countsInjPlace <- df2 %>%
  group_by(InjuryPlace) %>%
  summarize(count = n()) %>%
  filter(count > 40) %>%
  select(InjuryPlace, count)

countsInjPlace <- countsInjPlace[!is.na(countsInjPlace$InjuryPlace), ]

colorsPlace <- c("black", "#393646", "#443c68", "blue2",
"#576cbc", "#9bc5ef", "#cfe2f3", "cyan", "#7661cc")
figInjPlace <- plot_ly() %>%
  add_pie(data = countsInjPlace, labels = ~InjuryPlace, values = ~count, 
          name = "Injury Place", marker = list(colors = colorsPlace)) %>% 
  layout(title = "Accidental Drug Related Deaths by Injury Location", 
         showlegend = TRUE)

figInjPlace

countsInjPlaceSR <- df2 %>%
  group_by(InjuryPlace, Sex, Race) %>%
  summarize(count = n())

countsInjPlaceSR <- countsInjPlaceSR %>%
  group_by(InjuryPlace) %>%
  filter(sum(count) > 100)
colorsPlaceSex <- c("blue2",
"#576cbc", "#9bc5ef", "#cfe2f3", "cyan")

figPlaceSex <- plot_ly()
figPlaceSex <- figPlaceSex %>% add_pie(data = countsInjPlaceSR %>%
                         filter(Sex == "Male"), labels = ~InjuryPlace, 
                       values = ~count, textinfo='percent', 
                       showlegend=T, title="Male", name = "Male", 
                       domain = list(x = c(0, 0.4), y = c(0.5, 0.2),
                       marker = list(colors = colorsPlaceSex)))
figPlaceSex <- figPlaceSex %>% add_pie(data = countsInjPlaceSR %>%
                         filter(Sex == "Female"), labels = ~InjuryPlace, 
                                values = ~count, textinfo='percent', 
                                showlegend=T, title="Female",
                       name = "Color", 
                       domain = list(x = c(0.6, 1), y = c(0.5, 0.2),
                          marker = list(colors = colorsPlaceSex)))
figPlaceSex <- figPlaceSex %>% layout(
  title = "Top 5 Accidental Drug Related Death Injury Places by Sex"
)
figPlaceSex

figPlaceRaceMale <- plot_ly(data = countsInjPlaceSR %>%
                         filter(Sex == "Male"), y = ~Race, x=~count, type = "bar", 
                       color = ~InjuryPlace, 
                       colors = c("#393646", "#443c68", "blue2",
                                  "white", "#9bc5ef", "black")) %>%
  layout(title = "Top 5 Male Accidental Drug Related Injury Places by Race", 
         showlegend = TRUE, xaxis = list(title = "", showgrid = TRUE), 
         yaxis = list(title = "", showgrid = FALSE))
figPlaceRaceMale

figPlaceRaceFemale <- plot_ly(data = countsInjPlaceSR %>%
                              filter(Sex == "Female"), y = ~Race, x=~count, 
                              type = "bar", color = ~InjuryPlace, 
                            colors = c("#393646", "#443c68", "blue2",
                                       "white", "#9bc5ef", "black")) %>%
  layout(title = "Top 5 Female Accidental Drug Related Death Locations by Race",
         showlegend = TRUE, xaxis = list(title = "", showgrid = TRUE), 
         yaxis = list(title = "", showgrid = FALSE))
figPlaceRaceFemale

#Death County
df2_deathCounty <- df2[df2$DeathCounty != "USA", ]
figDeathCounty <- plot_ly(df2, y = ~DeathCounty, type = 'histogram', 
                  color = ~DeathCounty, 
                  colors =c("#d0e0e3", "#b88b58", "#443c68", #d0e0e3
                            "#576cbc", "#9bc5ef", 
                            "#f1c232", "#eeeeee", "#bcbcbc")) %>%
  layout(title = "Accidental Drug Related Deaths by County", 
         showlegend = FALSE, xaxis = list(title = "", showgrid = TRUE), 
         yaxis = list(title = "", showgrid = FALSE))%>%
  add_annotations(
    x=max(table(df2$DeathCounty)),
    y=as.numeric(df2$DeathCounty == levels(df2$DeathCounty)["Hartford"]),
                      text = "1,233 Deaths")
figDeathCounty

figDeathCountySex <- plot_ly(df2, y = ~DeathCounty, type = 'histogram', 
                          color = ~Sex, 
                          colors =c("#f1c232", "#b88b58", "#443c68", 
                                    "#576cbc", "#9bc5ef", 
                                    "#f1c232", "#d0e0e3", "#bcbcbc")) %>%
  layout(title = "Accidental Drug Related Deaths by County and Sex", 
         showlegend = TRUE, xaxis = list(title = "", showgrid = TRUE), 
         yaxis = list(title = "", showgrid = FALSE))
figDeathCountySex

figDeathCountyRace <- plot_ly(df2, y = ~DeathCounty, type = 'histogram', 
                             color = ~Race, 
                             colors =c("#999999", "#E69F00", "#b88b58", 
                                       "#009E73", "#F0E442", "#0072B2", 
                                       "#D55E00", "#CC79A7", "#443c68",
                                       "#56B4E9")) %>%
  layout(title = "Accidental Drug Related Deaths by Race and County", 
         showlegend = TRUE, xaxis = list(title = "", showgrid = TRUE), 
         yaxis = list(title = "", showgrid = FALSE))
figDeathCountyRace

#Manner of Death
df2$MannerofDeath[df2$MannerofDeath == "accident"] = "Accident"
df2$MannerofDeath[df2$MannerofDeath == "ACCIDENT"] = "Accident"

countsMOD <- df2 %>%
  group_by(MannerofDeath) %>%
  filter(MannerofDeath %in% c("Accident", "Natural", "Pending")) %>%
  count(MannerofDeath, name = "Count")

colorsMOD <- c("#9bc5ef","#576cbc", "cyan")

figMOD <- plot_ly(data = df2 %>%
                    filter(!is.na(MannerofDeath))) 
figMOD <- figMOD %>% add_pie(data = count(df2, MannerofDeath) %>%
                      filter(MannerofDeath %in% c("Accident", "Natural", "Pending")), 
                             labels = ~MannerofDeath, values = ~n, 
                             textinfo='label+value+percent', showlegend=T,
                             rotation=90, name = "Color",
                      marker = list(colors = colorsMOD)) %>% 
  layout(title = "Manner of Accidental Drug Related Deaths")
figMOD

dfSexMod <- df2 %>%
  filter(MannerofDeath %in% c("Accident", "Natural", "Pending"))
dfSexMod <- dfSexMod %>%
  filter(!is.na(MannerofDeath))
dfSexModMale <- dfSexMod %>%
  filter(Sex == "Male")
dfSexModFemale <- dfSexMod %>%
  filter(Sex == "Female")


figSexMOD <- plot_ly()
figSexMOD <- figSexMOD %>% add_pie(data = count(dfSexModMale, MannerofDeath
                                                ), labels = ~MannerofDeath, values = ~n, 
                                   textinfo='label+value+percent', showlegend=T,
                                   rotation=90, title = "Male", name = "MaleMOD",
                                   domain = list(x = c(0, 0.4), y = c(0.4, 0)))
figSexMOD <- figSexMOD %>% add_pie(data = count(dfSexModFemale, 
                                                MannerofDeath), labels = ~MannerofDeath, values = ~n, 
                                   textinfo='label+value+percent', showlegend=T,
                                   rotation=90, title = "Female", name = "FemaleMOD",
                                   domain = list(x = c(0.6, 1), y = c(0.4, 0))) 
figSexMOD <- figSexMOD %>% 
  layout(title = "Manner of Accidental Drug Related Deaths by Sex")
figSexMOD


dfSexModRace <- df2 %>%
  filter(Race %in% c("White", "Hispanic, White", "Black", "Hispanic, Black", " Other"))
dfSexModRaceA <- dfSexModRace %>%
  filter(MannerofDeath == 'Accident')



figSexModAccident <- plot_ly()
figSexModAccident <- figSexModAccident %>% add_pie(data = count(dfSexModRaceA, Race),
                                   labels = ~Race, values = ~n, 
                                   textinfo='label+value+percent', showlegend=T,
                                   rotation=110, name = "MaleMOD",
                                   domain = list(x = c(1, 1), y = c(0.4, 0)))
figSexModAccident <- figSexModAccident %>% 
  layout(title = "Accident Drug Related Deaths by Race") %>%  
  add_annotations(
    xref="paper",
    yref="paper",
    x=1.2, #Left most
    y=-.03, #Upper most
    text='*Only Represents "Accident" Manner of Death because other manners are signifcantly lower',
    showarrow = F
  )
figSexModAccident

#Any Opioids

figOp <- plot_ly(count(df2, AnyOpioid),
  x = ~AnyOpioid,
  y = ~n, text = ~n, textposition = 'auto',
  type = "bar", marker = list(color = c("#cfe2f3", "#443c68"))
)
figOp <- figOp %>%
  layout(title = "Opioid in Accidental Drug Related Deaths?",
         xaxis = list(title = "Any Opiods?"),
         yaxis = list(title = ""))
figOp

figOpSex <- plot_ly(count(df2, AnyOpioid, Sex),
                     x = ~AnyOpioid,
                     y = ~n,
                    type = "bar", color = ~Sex, 
                     colors =c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
)
figOpSex <- figOpSex %>%
  layout(title = "Opioid in Accidental Drug Related Deaths by Sex",
         xaxis = list(title = "Any Opiods?"),
         yaxis = list(title = ""))
figOpSex

figOpRace <- plot_ly(count(df2, AnyOpioid, Race),
                 x = ~AnyOpioid,
                 y = ~n,
                 type = "bar", color = ~Race, 
                 colors =c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
)
figOpRace <- figOpRace %>%
  layout(title = "Opioid in Accidental Drug Related Deaths by Race",
         xaxis = list(title = "Any Opiods?"),
         yaxis = list(title = ""))
figOpRace




