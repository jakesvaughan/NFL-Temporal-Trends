library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)

#Data Scraping 
library(robotstxt)
library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

#Table iterations 
#/html/body/div[4]/table[1]
#/html/body/div[4]/table[1]
#/html/body/div[4]/table[1]
#Same Table HTML code for every table 

#Web page 
#https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=525
#https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=550
#https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=575
#https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=600
#Web pages go up by 25


#Multiple pages: 
url_base <- "https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=%d"

map_df(seq(0,100, 25), function(i) {
  print(sprintf(url_base, i))
  data.frame(html_nodes(read_html(sprintf(url_base, i)), xpath = '/html/body/div[4]/table[1]') %>% 
               html_table(header = T))
  
}) -> testing 

#works 

testing <- testing %>% 
  mutate(
    Acquired = substring(Acquired, 3),
    Relinquished = substring(Relinquished, 3)
  )


#Baseball 
url_base <- "https://www.prosportstransactions.com/baseball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=%d"

map_df(seq(0,19475, 25), function(i) {
  print(i)
  data.frame(html_nodes(read_html(sprintf(url_base, i)), xpath = '/html/body/div[4]/table[1]') %>% 
               html_table(header = T))
  
}) -> baseball_injury 


baseball_injury <- baseball_injury %>% 
  mutate(
    Acquired = substring(Acquired, 3),
    Relinquished = substring(Relinquished, 3)
  )

write.csv(baseball_injury, "/Users/student/Desktop/baseball_injury.csv")


#Basketball 
url_basket <- "https://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&Submit=Search&start=%d"

map_df(seq(0,27900, 25), function(i) {
  print(i)
  data.frame(html_nodes(read_html(sprintf(url_basket, i)), xpath = '/html/body/div[4]/table[1]') %>% 
               html_table(header = T))
  
}) -> basketball_injury 


basketball_injury <- basketball_injury %>% 
  mutate(
    Acquired = substring(Acquired, 3),
    Relinquished = substring(Relinquished, 3)
  )

write.csv(basketball_injury, "/Users/student/Desktop/basketball_injury.csv")

#Football 
url_football <- "https://www.prosportstransactions.com/football/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=%d"

map_df(seq(0,12175, 25), function(i) {
  print(i)
  data.frame(html_nodes(read_html(sprintf(url_football, i)), xpath = '/html/body/div[4]/table[1]') %>% 
               html_table(header = T))
  
}) -> football_injury 


football_injury <- football_injury %>% 
  mutate(
    Acquired = substring(Acquired, 3),
    Relinquished = substring(Relinquished, 3)
  )

write.csv(football_injury, "/Users/student/Desktop/football_injury.csv")


#Hockey 
url_hockey <- "https://www.prosportstransactions.com/hockey/Search/SearchResults.php?Player=&Team=&BeginDate=&EndDate=&InjuriesChkBx=yes&submit=Search&start=%d"

map_df(seq(0,21850, 25), function(i) {
  print(i)
  data.frame(html_nodes(read_html(sprintf(url_hockey, i)), xpath = '/html/body/div[4]/table[1]') %>% 
               html_table(header = T))
  
}) -> hockey_injury 


hockey_injury <- hockey_injury %>% 
  mutate(
    Acquired = substring(Acquired, 3),
    Relinquished = substring(Relinquished, 3)
  )


write.csv(hockey_injury, "/Users/student/Desktop/hockey_injury.csv")

#combining sports 

#Split By Surgery, FX, and Pure Injury, no incidence. 
#Baseball DF
inj_base_2
#Basketball DF
inj_basket_2
#Football DF 
inj_foot_2
#Hockey DF 
inj_hock_2

#Combining Baseball and Basketball 
base_basket <- rbind(inj_base_2, inj_basket_2)

#Combining basketball/basketball with football 
base_basket_foot <- rbind(base_basket, inj_foot_2)

#All 4 major sports combined 
all_sports <- rbind(base_basket_foot, inj_hock_2)

write.csv(all_sports, "/Users/student/Desktop/all_sports_injury_data.csv")


#All sports for without duplicates
all_sports_precise <- all_sports %>% 
  distinct(Relinquished, body_part_2, season, sport, .keep_all = T)

all_sports_precise <- all_sports_precise %>% 
  filter(season != 0)

all_sports_precise <- all_sports_precise %>% 
  filter(Date_2 > "2007-3-1")

all_sports_precise <- all_sports_precise %>% 
  filter(season != "2006-2007")

write.csv(all_sports_precise, "/Users/student/Desktop/all_sports_precise.csv")


#split by football

foot_precise <- all_sports_precise %>% 
  filter(sport == "Football")

foot_precise$season2 <- 0

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2007-2008",
                          2007, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2008-2009",
                          2008, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2009-2010",
                          2009, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2010-2011",
                          2010, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2011-2012",
                          2011, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2012-2013",
                          2012, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2013-2014",
                          2013, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2014-2015",
                          2014, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2015-2016",
                          2015, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2016-2017",
                          2016, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2017-2018",
                          2017, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2018-2019",
                          2018, season2))

foot_precise <- foot_precise %>% 
  mutate(season2 = ifelse(season == "2019-2020",
                          2019, season2))

body_count_year_foot <- foot_precise %>%
  group_by(season2) %>% 
  count(body_part_2)


body_count_year_foot <- body_count_year_foot %>% 
  filter(season2 != 0)

body_count_year_foot$Incidence <- 0 

body_count_year_foot$sport <- "Football"

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2005", 
                            round((n/1320) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2006", 
                            round((n/1298) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2007", 
                            round((n/1324) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2008", 
                            round((n/1334) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2009", 
                            round((n/1343) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2010", 
                            round((n/1379) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2011", 
                            round((n/1313) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2012", 
                            round((n/1290) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2013", 
                            round((n/1312) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2014", 
                            round((n/1337) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2015", 
                            round((n/1315) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2016", 
                            round((n/1327) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2017", 
                            round((n/1323) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2018", 
                            round((n/1347) *100, digits = 2),
                            Incidence))

body_count_year_foot <- body_count_year_foot %>% 
  mutate(Incidence = ifelse(season2 == "2019", 
                            round((n/1107) *100, digits = 2),
                            Incidence))



tf <- body_count_year_foot %>% 
  filter(Incidence > 4)

tf1 <- body_count_year_foot %>% 
  filter(Incidence > 1)

tf2 <- body_count_year_foot %>% 
  filter(Incidence > 2)

tf_back <- body_count_year_foot %>%
  filter(body_part_2 == "back")

tf_achilles <- body_count_year_foot %>%
  filter(body_part_2 == "Achilles")

mean(tf_achilles$Incidence, na.rm = T) 0.175

tf_ankle <- body_count_year_foot %>% 
  filter(body_part_2 == "ankle" )

mean(tf_ankle$Incidence, na.rm = T)     4.44

tf_knee <- body_count_year_foot %>% 
  filter(body_part_2 == "knee")

tf_conc <- body_count_year_foot %>% 
  filter(body_part_2 == "concussion")

tf_foot <- body_count_year_foot %>% 
  filter(body_part_2 == "foot")

tf_adductor <- body_count_year_foot %>% 
  filter(body_part_2 == "adductor")

tf_shoulder <- body_count_year_foot %>% 
  filter(body_part_2 == "shoulder")

tf_hamstring <- body_count_year_foot %>%
  filter(body_part_2 == "hamstring")
  
mean(tf_knee$Incidence, na.rm = T) 


#Code for one injury over all seasons

ggplot(tf_shoulder,aes(season2, Incidence)) + 
  geom_line() + 
  geom_point() +
  labs(x = "Season", y = "Shoulder Injury Incidence per 100 Players") +
  ggtitle("Shoulder Time Loss Injury by Season") +
  scale_color_discrete(name = "Sport") + 
  theme(plot.title = element_text(hjust = 0.5))


#Code for injuries >1 per 100 players over seasons 

tf1 <- tf1 %>%
  filter(body_part_2 == "knee" | body_part_2 == "ankle" |
                   body_part_2 == "concussion" | body_part_2 == "hamstring" | body_part_2 == "shoulder" | 
                   body_part_2 == "adductor" | body_part_2 == "foot")

ggplot(tf1,aes(season, Incidence, color = body_part_2)) + 
  geom_line(aes(group = body_part_2)) + 
  geom_point() +
  labs(x = "Season", y = "Injury Incidence per 100 Players") +
  ggtitle("Time Loss Injury by Season") +
  scale_color_discrete(name = "Injury") + 
  theme(plot.title = element_text(hjust = 0.5))



#Total injury count per season

season_injury_rate_foot <- body_count_year_foot %>%
  group_by(season2) %>%
  count(sum(n))

names(season_injury_rate_foot)[2] <- "total_injury"

season_injury_rate_foot <-season_injury_rate_foot %>% 
  filter(season2 != 0)

season_injury_rate_foot$Incidence <- 0 

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2007", 
                            round((total_injury/1324) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2008", 
                            round((total_injury/1334) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2009", 
                            round((total_injury/1343) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2010", 
                            round((total_injury/1379) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2011", 
                            round((total_injury/1313) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2012", 
                            round((total_injury/1290) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2013", 
                            round((total_injury/1312) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2014", 
                            round((total_injury/1337) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2015", 
                            round((total_injury/1315) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2016", 
                            round((total_injury/1327) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2017", 
                            round((total_injury/1323) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2018", 
                            round((total_injury/1347) *100, digits = 2),
                            Incidence))

season_injury_rate_foot <- season_injury_rate_foot %>% 
  mutate(Incidence = ifelse(season2 == "2019", 
                            round((total_injury/1107) *100, digits = 2),
                            Incidence))

mean(season_injury_rate_foot$Incidence, na.rm = T) 

season_injury_rate_foot$sport <- "Football"

ggplot(season_injury_rate_foot,aes(season2, Incidence, sport)) + 
  geom_line(aes(group=sport)) + 
  geom_point() +
  labs(x = "Season", y = "Injury Incidence per 100 Players") +
  ggtitle("Time Loss Injury by Season") +
  scale_color_discrete(name = "sport") + 
  theme(plot.title = element_text(hjust = 0.5))

write.table(season_injury_rate_foot, file="seasonalinjuryrate.csv", sep=",")

#monthly injury rate 

install.packages("lubridate")

library(lubridate)

foot_precise$MonthInjured <- 0

monthly_injury <- foot_precise %>%
  mutate(MonthInjured = Date %>% month()
   ) %>%
  group_by(MonthInjured) %>% 
   count(body_part_2) 

monthly_injury <- monthly_injury %>%
  filter( body_part_2 == "knee" | body_part_2 == "ankle" |
            body_part_2 == "concussion" | body_part_2 == "hamstring" | body_part_2 == "shoulder")

monthly_injury$month_injured_2 <- 0

monthly_injury <- monthly_injury %>%
  ungroup(MonthInjured) %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==1,
                                "January", 
                                month_injured_2)) 

monthly_injury <- monthly_injury %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==2,
                                "February",
                                month_injured_2)) 

monthly_injury <- monthly_injury %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==9,
                                  "September",
                                  month_injured_2)) 

monthly_injury <- monthly_injury %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==10,
                                  "October",
                                  month_injured_2)) 

monthly_injury <- monthly_injury %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==11,
                                  "November",
                                  month_injured_2)) 

monthly_injury <- monthly_injury %>%
  mutate(month_injured_2 = ifelse(MonthInjured ==12,
                                  "December",
                                  month_injured_2)) 

monthly_injury$Incidence <- 0  

monthly_injury <- monthly_injury %>%
  mutate(Incidence = ifelse(body_part_2 =="knee",
                            round((n/17051) *100, digits = 2),
                            Incidence))

monthly_injury <- monthly_injury %>%
  mutate(Incidence = ifelse(body_part_2 =="ankle",
                                    round((n/17051) *100, digits = 2),
                                    Incidence))

monthly_injury <- monthly_injury %>%
  mutate(Incidence = ifelse(body_part_2 =="concussion",
                                    round((n/17051) *100, digits = 2),
                                    Incidence))

monthly_injury <- monthly_injury %>%
  mutate(Incidence = ifelse(body_part_2 =="hamstring",
                                    round((n/17051) *100, digits = 2),
                                    Incidence))

monthly_injury <- monthly_injury %>%
  mutate(Incidence = ifelse(body_part_2 =="shoulder",
                                    round((n/17051) *100, digits = 2),
                                    Incidence))

ggplot(monthly_injury, aes(month_injured_2, Incidence, color = body_part_2)) + 
  scale_x_discrete(limits=c("September", "October", "November", "December", "January", "February")) + 
  geom_line(aes(group = body_part_2)) + 
  geom_point() +
  labs(x = "Month", y = "Monthly Injury Incidence per 100 players") +
  ggtitle("Injury Incidence Throughout the Season") +
  scale_color_discrete(name = "Body Part") + 
  theme(plot.title = element_text(hjust = 0.5))

monthly_knee <- monthly_injury %>%
  filter(body_part_2 == "knee")

monthly_ankle <- monthly_injury %>%
  filter(body_part_2 == "ankle")

monthly_conc <- monthly_injury %>%
  filter(body_part_2 == "concussion")    

monthly_hamstring <- monthly_injury %>%
  filter(body_part_2 == "hamstring")

monthly_shoulder <- monthly_injury %>%
  filter(body_part_2 == "shoulder")


#Normality testing, Histograms, Kernel density plot & Shap-wilk

plot(ankledensity)

hist(tf_knee$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Knee Injury Incidence") 

lines(density(tf_knee$Incidence), col="Red")

sqrt(var(tf_knee$Incidence)) 0.72

ggqqplot(tf_knee$Incidence)


sqrt(var(tf_foot$Incidence))


hist(tf_ankle$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Ankle Injury Incidence")

lines(density(tf_ankle$Incidence), col="red")    

sqrt(var(tf_ankle$Incidence)) 0.91

     
hist(tf_conc$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Concussion Incidence")

lines(density(tf_conc$Incidence), col="red") 

sqrt(var(tf_conc$Incidence)) 2.02

ggqqplot(tf_conc$Incidence)


hist(tf_hamstring$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Hamstring Injury Incidence")

lines(density(tf_hamstring$Incidence), col="red") 

sqrt(var(tf_hamstring$Incidence)) 0.99


hist(tf_shoulder$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Shoulder Injury Incidence")

lines(density(tf_shoulder$Incidence), col="red") 

sqrt(var(tf_shoulder$Incidence)) 0.73



hist(season_injury_rate_foot$Incidence, freq = FALSE, xlab = "Incidence", ylab = "Density", main = "Distribution of Seasonal Injury Incidence")

lines(density(season_injury_rate_foot$Incidence), col="red") 

sqrt(var(season_injury_rate_foot$Incidence)) 5.75

body_count_year_foot %>% 
  group_by(body_part_2) %>%
  shapiro_test(Incidence)

shapiro_test(tf_knee$Incidence) 0.512

ggqqplot(tf_hamstring$Incidence)

shapiro_test(tf_ankle$Incidence) 0.459

shapiro_test(tf_hamstring$Incidence) 0.218

shapiro_test(tf_conc$Incidence) 0.0724

shapiro_test(tf_shoulder$Incidence) 0.750

shapiro_test(tf2$Incidence) 0.118

shapiro_test(season_injury_rate_foot$Incidence) 0.278

ggplot(tf_knee, aes(x = season, y = Incidence)) +
  geom_point() +
  labs(x = "Season", y = "Injuries per 100 players per season") +
  geom_smooth(method = 'lm', se = FALSE)

#Significance

five_inj_data <- body_count_year_foot %>%
  filter(body_part_2 == "knee" | body_part_2 == "ankle" |
           body_part_2 == "concussion" | body_part_2 == "hamstring" | body_part_2 == "shoulder")

ggplot(five_inj_data, aes(x = body_part_2, y = Incidence, fill = body_part_2)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position = "none") +
  xlab("Body part") +
  ylab("Injury Incidence")

write.csv(five_inj_data, "five_inj_data.csv")

five_inj_data$season2 <- as.factor(five_inj_data$season2)

res.aov_inj <- aov(n ~ body_part_2, data = five_inj_data)

summary(res.aov_inj)

TukeyHSD(res.aov_inj) 

Injury_posthoc <- TukeyHSD(res.aov_inj)

Injury_posthoc <- as.data.frame(Injury_posthoc[1:1])

Injury_posthoc <- round(Injury_posthoc, 3)

knee_nine_twelve <- tf_knee %>% 
  filter(season2 == "2008" | season2 == "2011")

##rename the variable names to identify the regions

tf_ankle$Ankle <- tf_ankle$Incidence
tf_knee$Knee <- tf_knee$Incidence
tf_hamstring$Hamstring <- tf_hamstring$Incidence
tf_shoulder$Shoulder <- tf_shoulder$Incidence
tf_conc$Concussion <- tf_conc$Incidence
season_injury_rate_foot$all <- season_injury_rate_foot$Incidence

###combining the data
data3 <- cbind(tf_ankle, tf_knee, tf_hamstring, tf_shoulder, tf_conc, season_injury_rate_foot)

##trimming the data and keep the required
data4 <- data3[,c(1,5,10,15,20,25,31)]


### change the variable names

data4$"Total Injury" <- data4$all

data4$all <- NULL

names(data4)[1] <- "Year"

write.csv(data4, "data4.csv")

#Use data4 in Joinpoint Software 

#Sensitivity analysis 

foot_precise$fiveyr <- 0

foot_precise <- as.character(foot_precise$season)

foot_precise <- foot_precise %>% 
  mutate(fiveyr = ifelse((season == "2007-2008") | (season == "2008-2009") | (season == "2009-2010") | (season == "2010-2011") | (season =="2011-2012"),
                          "2007-2012",
                          ifelse((season == "2012-2013") | (season == "2013-2014") | (season == "2014-2015") | (season == "2015-2016") | (season =="2016-2017"),
                                 "2012-2017",
                                 ifelse((season == "2017-2018") | (season == "2018-2019") | (season == "2019-2020"),
                                        "2017-2020", !is.na(fiveyr)))))


sens_analy <- foot_precise %>%
  group_by(fiveyr) %>%
  count(body_part_2)
  
sens_analy <- sens_analy %>%
  filter( body_part_2 == "knee" | body_part_2 == "ankle" |
             body_part_2 == "concussion" | body_part_2 == "hamstring" | body_part_2 == "shoulder")

sens_analy$Incidence <- 0 

sens_analy <- sens_analy %>% 
  mutate(Incidence = ifelse(fiveyr == "2007-2012", 
                            round((n/6693) *100, digits = 2),
                            Incidence))

sens_analy <- sens_analy %>% 
  mutate(Incidence = ifelse(fiveyr == "2012-2017", 
                            round((n/6581) *100, digits = 2),
                            Incidence))

sens_analy <- sens_analy %>% 
  mutate(Incidence = ifelse(fiveyr == "2017-2020", 
                            round((n/3777) *100, digits = 2),
                            Incidence))

ggplot(sens_analy,aes(fiveyr, Incidence, color =body_part_2)) + 
  geom_line(aes(group=body_part_2)) + 
  geom_point() +
  labs(x = "Seasons", y = "Injury Incidence") +
  ggtitle("Time Loss Injury by 5 seasons") +
  scale_color_discrete(name = "Injury") + 
  theme(plot.title = element_text(hjust = 0.5))

#team analysis 

team_injury <- foot_precise %>%
  group_by(Team) %>% 
  count(sport)

mean(team_injury$n, na.rm = T)

sqrt(var(team_injury$n)) 32.2

team_injury <- foot_precise %>%
  group_by(Team) %>% 
  count(body_part_2)

write.csv(team_injury, "team_injury.csv")

team_injury$season2 <- as.factor(team_injury$season2)

res.aov <- aov(n~ season2, data = team_injury)

summary(res.aov)

TukeyHSD(res.aov)

year_ph <- TukeyHSD(res.aov)

year_ph <- as.data.frame(year_ph[1:1])

year_ph <- round(year_ph,3)

year_ph$P_val <- year_ph$season2.p.adj

year_ph <- year_ph[which(year_ph$P_val < 0.05),]

write_csv(year_ph, "year_ph.csv")

res.aov2 <- aov(n~Team, data = team_injury)

summary(res.aov2)

TukeyHSD(res.aov2)

team_ph <- TukeyHSD(res.aov2)

team_ph <- as.data.frame(team_ph[1:1])

team_ph <- round(team_ph, 3)

team_ph$P_val <- team_ph$Team.p.adj

team_ph <- team_ph[which(team_ph$P_val < 0.05),]

write.csv(team_ph, "team_ph.csv")


#player analysis 

player_injury <- foot_precise %>%
  group_by(Relinquished) %>%
  count(sport)


