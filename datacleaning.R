library(tidyverse)
library(ggplot2)
library(highcharter)
library(dplyr)
library(purrr)
library(gganimate)
library(waffle)
library(readxl)
library(extrafont)

ncsis_registeredguns <- read_csv("/Users/aliciaortiz/Documents/capstone_ortiz/data/nics-firearm-background-checks.csv")
school_safety_audits <- read_csv("/Users/aliciaortiz/Downloads/School Safety Audits.csv")
school_safety_drills <- read_csv('/Users/aliciaortiz/Downloads/School Safety Drills.csv')
school_safety_plans <-read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolsafetyplans.csv')
school_resource_officers <- read_csv('/Users/aliciaortiz/Downloads/School Resource Officers.csv')
weapons_in_schools <- read_csv('/Users/aliciaortiz/Downloads/Weapons in Schools.csv')
securitymeasures <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/')
florida_data <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/fl_bills.csv')
colorodo_data <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/co_bills.csv')
school_shooting <- readRDS('/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolshootings.RDS')



weapons_in_schools <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/weaponsinschools.RDS')
florida_data <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/florida_bills.RDS')
colorodo_data <- read_csv('/Users/aliciaortiz/Documents/capstone_ortiz/data/colorodo_bills.RDS')
mental_health <- read_excel('/Users/aliciaortiz/Documents/capstone_ortiz/data/mental_health.xlsx')

saveRDS(mental_health, '/Users/aliciaortiz/Documents/capstone_ortiz/data/mental_health.RDS')







#cleansecuritymeasures

securitymeasures <- securitymeasures %>% select(`Security measure`, "1999", "2001", "2003", "2005","2007", "2009","2011", "2013")
securitymeasures <- securitymeasures %>% gather(type, rate, c(2:9))



saveRDS(school_shooting, 'Documents/capstone_ortiz/data/schoolshootings.RDS')
saveRDS(k127018, '/Users/aliciaortiz/Documents/capstone_ortiz/data/k12schoolshooting.RDS')
saveRDS(school_safety, 'data/schoolsafety.RDS')
saveRDS(school_safety_audits, '/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolsafetyaudits.RDS')
saveRDS(school_safety_drills, '/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolsafetydrills.RDS')
saveRDS(school_safety_plans, '/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolsafetyplans.RDS')
saveRDS(school_resource_officers, '/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolresourceofficers.RDS')
saveRDS(weapons_in_schools, '/Users/aliciaortiz/Documents/capstone_ortiz/data/weaponsinschools.RDS')
saveRDS(colorodo_data, '/Users/aliciaortiz/Documents/capstone_ortiz/data/colorodo_bills.RDS')
saveRDS(florida_data, '/Users/aliciaortiz/Documents/capstone_ortiz/data/florida_bills.RDS')

#count which day of the week it occured the most 
dayofweek <-ggplot(data=school_shooting, aes(x=day_of_week)) +
a

#Casulties over the years 



#shootingdaysofthe week
daysofweek <- ggplot(schoolshooting_timeoverview, aes(x =Day.of.week..formula., y=Total.Injured.Killed.Victims, colour = Category))+
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )


daysofweek + transition_states(Day.of.week..formula., wrap = FALSE) +
  shadow_mark()



#reviewing Florida data 
school_shooting <- readRDS('/Users/aliciaortiz/Documents/capstone_ortiz/data/schoolshootings.RDS')
florida <- filter(school_shooting, state == "Florida")
florida$date<- as.Date(florida$date, "%m/%d/%Y")



#test out map 
school_shooting$date<- as.Date(school_shooting$date, "%m/%d/%Y")
set.seed(1234)

n <- 238
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y,0) })
df2 <- data_frame(
  lat = school_shooting$lat,
  lon = school_shooting$long,
  z = school_shooting$date,
  color = colorize(z),
  sequence = sequences,
  name = school_shooting$school_name
)

us_map <- hcmap("countries/us/us-all") %>% 
  hc_add_series(data = df2, type = "mapbubble", name = "schools", 
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, 
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))
us_map <- us_map %>% hc_add_theme(hc_theme_chalk())

#schoolshooting overview
schoolshooting_timeoverview$Date<- as.Date(schoolshooting_timeoverview$Date, "%m/%d/%y")
timeline_shooting <- schoolshooting_timeoverview %>% hchart(type = "line", hcaes(x=Date, y= Total.Injured.Killed.Victims, group = Firearm.Type))
timeline_shooting%>% hc_add_theme(hc_theme_chalk())




#data of time florida
daysofweek <- ggplot(florida, aes(x =day_of_week, y=casualties, colour = shooting_type))+
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
daysofweek + transition_states(day_of_week, wrap = FALSE) +
  shadow_mark()

#florida legislation

gun_control <- grep("gun", florida_data$title)
gun_control <-subset(florida_data, grepl("Gun|gun|gun control|Gun Control", florida_data$title))
mental_health <-subset(florida_data, grepl("Mental Health|mental health|mental", florida_data$title))
school_safety <-subset(florida_data, grepl("School Safety|school safety|School safety", florida_data$title))
all <- subset(florida_data, grepl("School Safety|school safety|School safety| Mental Health|mental health|Gun|gun|gun control|Gun Control", florida_data$title))
fl_legislation <- ggplot(all, aes(session)) + geom_bar(stat="count")

#floridamap 
set.seed(1234)

n <- 18
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y, 0) })
df <- data_frame(
  lat = florida$lat,
  lon = florida$long,
  z = florida$date,
  color = colorize(z),
  sequence = sequences,
  name = florida$school_name
)

fl_map <- hcmap("countries/us/us-fl-all") %>% 
  hc_add_series(data = df, type = "mapbubble", name = "schools", 
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))
fl_map <- fl_map %>% hc_add_theme(hc_theme_handdrawn())


#add in legislation
target <- c("SPB 7026","SB 196", "SB 1940", "HB 997", "SB 1368", "SB 180", "SB 968")
fl_legis <- filter(all, bill_id %in% target)
fl_legis <- fl_legis %>% 
  add_row( updated_at = "03/09/2018", .before = 7)


#reviewing Colorado data 
colorado <- filter(school_shooting, state == "Colorado")
colorado$date<- as.Date(colorado$date, "%m/%d/%Y")



#data of time Colorodo 
#notesonshinyapp
daysofweekco <- ggplot(colorado, aes(x =day_of_week, y=casualties, colour = shooting_type))+
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
daysofweekco + transition_states(day_of_week, wrap = FALSE) +
  shadow_mark()

#coloradomap 
set.seed(1234)

n <- 5
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y, 0) })
df <- data_frame(
  lat = colorado$lat,
  lon = colorado$long,
  z = colorado$date,
  color = colorize(z),
  sequence = sequences,
  name = colorado$school_name
)

co_map <- hcmap("countries/us/us-co-all") %>% 
  hc_add_series(data = df, type = "mapbubble", name = "schools", 
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))
co_map <- co_map %>% hc_add_theme(hc_theme_handdrawn())







#colorado guns
co_guns <- ncsis_registeredguns %>% filter(state == "Colorado")
co_guns$permit <- as.integer(co_guns$totals)
coguns_timeline <- co_guns %>% hchart(type = "line", hcaes(x=month, y= totals))
coguns_timeline %>% hc_add_theme(hc_theme_chalk())
#florida guns 
fl_guns <- ncsis_registeredguns %>% filter(state == "Florida")
fl_guns$permit <- as.integer(fl_guns$totals)
flguns_timeline <- fl_guns %>% hchart(type = "line", hcaes(x=month, y= totals))
flguns_timeline %>% hc_add_theme(hc_theme_chalk())


#################################
######## School Safety Page #####
#################################

#weaponsinschools
weapons_in_schools <- weapons_in_schools %>% separate('Concealed Carry Permit Holders Are Explicitly&lt;/BR&gt; Authorized in  State Statute or Regulation&lt;/BR&gt; to Possess Weapons In Schools',c("carry_with_concealed","non_tested"))
weapons_in_schools <- select(weapons_in_schools, "State", "tested")

t <-waffle(c(no = 40, yes = 11), rows = 5,  
       colors = c("#c7d4b6", "#a3aabd"), title = "Concealed Carry Permit ")

#school safety plan 
table(school_safety_plans$`School Safety&lt;/br&gt; Plans Required` )
l <-waffle(c(no = 7, yes = 44), rows = 5,  
           colors = c("#c7d4b6", "#a3aabd"), title = "Safety School Plans ")

#school resource officers 
table(school_safety_plans$`School Safety&lt;/br&gt; Plans Required` )
y <-waffle(c(no = 35, yes = 16), rows = 5,  
           colors = c("#c7d4b6", "#a3aabd"), title = "Certification Requirements for School Resource Officers")

#mental health 
mental_health <- mental_health %>% hchart(type = "bar", hcaes(x =school_characteristics, y = public_schools)) %>%
       hc_exporting(enabled = TRUE)%>%
       hc_add_theme(hc_theme_chalk())


#weaponsinschools
concealedcarry <- weapons_in_schools %>%
separate(`Concealed Carry Permit Holders Are Explicitly&lt;/BR&gt; Authorized in  State Statute or Regulation&lt;/BR&gt; to Possess Weapons In Schools`,c("concealedcarry", "concealedcarryanswer"), ",")
concealedcarry <- concealedcarry %>% select('State','concealedcarry', 'concealedcarryanswer')
#create an elseif statement to create a binary yes or no 
test <- mutate(concealedcarry, position = ifelse(concealedcarry == "Yes", 1, 0))




#school resources officers 
certification_req<- school_resource_officers
certification_req <- rename(certification_req, cert_req = "Certification Requirements for&lt;/br&gt; School Resource Officers/&lt;/br&gt;School Security Officers")
certification_req <-  mutate(certification_req, position = ifelse(cert_req == "<NA>", "0" ,"1"))
certification_req$position[is.na(certification_req$position)] <- 0 
library(tigris)
library(dplyr)
library(leaflet)

states <- states(cb= T)

states %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup=~NAME)

#clean up 
sro <- certification_req%>% rename(state=State)
sro <- select(sro, position, state)
sro$position <- as.numeric(sro$position)
states_merged_sro <- geo_join(states, sro, "STUSPS", "state")
pal <- colorNumeric("Greens", domain=sro$position)
states_merged_sro <- subset(certification_req, !is.na(`Definition of School &lt;/br&gt;Resource Officer/&lt;/br&gt;School Security Officer`))
popup_sb <- paste0((certification_req$`Definition of School &lt;/br&gt;Resource Officer/&lt;/br&gt;School Security Officer`))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sro, 
              fillColor = 
   ~pal(sro$position), 
              fillOpacity = .2, 
              weight = 0.2, 
              smoothFactor = 0.2) %>%
  addLegend( pal = pal, 
             values = sro$position,
             position = "bottomright",
             title = "School Resource Officer Certification Requirements")


#Mental Health Data

#gun laws 
gun_laws <- read_csv('/Users/aliciaortiz/Documents/capstone/data/gun_laws_bystate.csv')
gun_laws <- gun_laws[1:19,]

fl_gun_laws <- gun_laws %>% hchart(type = "line", hcaes(x =year, y = fl_gun_laws)) %>%
  hc_exporting(enabled = TRUE)%>%
  hc_add_theme(hc_theme_darkunica())


#Florida Safety Data 
fl_safety_data <- read_excel('/Users/aliciaortiz/Documents/capstone/data/floridasafetydata.xls')
fl_safety_data <- fl_safety_data[1:26,]
fl_safety_data <- fl_safety_data %>% hchart(type = "bar", hcaes(x = `Type of Incident`, y = `Total Incidents`, group = `Type of Incident`)) %>%
  hc_exporting(enabled = TRUE)%>%
  hc_add_theme(hc_theme_darkunica())




laws <-read_excel('/Users/aliciaortiz/Documents/capstone/data/laws.xls')
laws <- laws %>% filter(!is.na(`Signed into law`))
laws$`Signed into law`<- as.Date(laws$`Signed into law`, "%Y/%m/%d")


#florida timeline 
timeline_florida <- florida %>% hchart(type = "stock", hcaes(x=date, y= casualties, name = florida$school_name) )

data_flags <- data_frame(
  date = sample((laws$`Signed into law`), size = 26),
  title = sprintf("Policy #%s", seq_along(date)),
  text = sprintf("Policy: #%s in %s", laws$Description, date)
)

fl_policy_timeseries <- timeline_florida %>% 
  hc_add_series(data_flags, hcaes(date),
                type = "flags", onSeries = "usdjpy") 






#florida spliced up dat a
fl_0005 <- florida[1:3,]
n <- 3
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y, 0) })

df <- data_frame(
  lat = fl_0005$lat,
  lon = fl_0005$long,
  z = fl_0005$date,
  color = colorize(z),
  sequence = sequences,
  names = fl_0005$school_name
)

df2 <- data_frame(
  lat = fl_0005$lat,
  lon = fl_0005$long,
  z = fl_0005$date,
  name = fl_0005$school_name
)


fl_05 <- hcmap("countries/us/us-fl-all") %>% 
  hc_add_series(data = df, type = "mapbubble",
                minSize = 0, maxSize = 30, name = "Shootings") %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))


#Colorado laws 
colorado <- filter(school_shooting, state == "Colorado")
colorado$date<- as.Date(colorado$date, "%m/%d/%Y")

colaws <- read_excel("Documents/capstone/data/laws.xls", sheet = "Colorado ")
colaws <- colaws %>% filter(!is.na(`Signed into law`))
timeline_colorado <- colorado %>% hchart(type = "stock", hcaes(x=date, y= casualties, name = colorado$school_name) )

data_flags <- data_frame(
  date = sample((colaws$`Signed into law`), size = 39),
  title = sprintf("Policy #%s", seq_along(date)),
  text = sprintf("Policy: #%s in %s", colaws$Description, date)
)

co_policy_timeseries <- timeline_colorado %>% 
  hc_add_series(data_flags, hcaes(date),
                type = "flags", onSeries = "usdjpy") 
