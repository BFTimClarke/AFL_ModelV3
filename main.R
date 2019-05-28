library("tidyverse")
library("lubridate")
library("BradleyTerryScalable")
library("elo")

data <- read_csv("afl_results_data_raw.csv")
odds <- read_csv("afl_odds.csv")


#Weighting constants
home_mod <-1
margin_mod <- 0.5
k<- 20



#initialising team/elo array (Alphabetical)
teams <- data %>%
 distinct(Home.Team) %>%
  arrange(Home.Team) %>%
  mutate(Rating = 1200)
dig <- 3
nteams <- nrow(teams)


#Init Team.ID columns.  Will be used to reference team elos
data <- data%>%
  filter(Round.Type == "Regular") %>%
  mutate(Home.Team.ID = NA,Away.Team.ID = NA)

  ngames <- nrow(data)
  x = 1:ngames
count = 1
  for(val in x){
 count1 = 1
   t = 1:nteams
   for (val in t){
        if(data$Home.Team[count] == teams[count1,1]){
     data$Home.Team.ID[count] = count1
        }
        if(data$Away.Team[count] == teams[count1,1]){
       data$Away.Team.ID[count] = count1
        }
     count1 = count1+1  
   }
  count = count+1
  }
  

#Creating a win modifier for each game based on the margin of victory.  Diminishing returns for greater margins.  Margin.Mod is affected by margin_mod constant. 
#margin_mod determines what proportion should automatically go to the winning team; the rest of the winshare is split as below
data <- data%>%
  mutate(Margin.Mod = round(0.5+0.5*sin((Home.Points-Away.Points)/180*pi),digits = dig)) %>%
  mutate(Date = dmy(Date)) 

#DaysSince - used backend to determine rest between teams.  probably redundant
data <- data %>%
  mutate(DaysSince = data$Date[ngames]-Date)

#Win Share - 1 for win, 0 for a loss. will be >1 for a draw as a workaround for case example of Margin.Mod
data <- data %>%
  mutate(Home.Win = ifelse(Margin>=0,1,0)) %>%
  mutate(Away.Win = 1-Home.Win) %>%
  mutate(Away.Win = ifelse(Margin == 0,1,Away.Win))
  

#tally Home Vanue Adventage.  total home wins / sum of games
Home.Venue.Adv <- sum(data$Home.Win)/(sum(data$Home.Win)+sum(data$Away.Win))
#advantage can be affected by home_mod (component difference of p = 0.5 is scaled by home_mod)
Home.Venue.Adv <- 0.5+(Home.Venue.Adv-0.5)*home_mod  


#Determine amount of rest had by a team before game.  Compiled on a team by team basis, bind_rows to combine.
x = 1:nteams
count = 1
new <- data[0,]

for(val in x){
  team <- data %>%
  filter(Home.Team == as.character(teams[count,1])|Away.Team == as.character(teams[count,1])) %>%
  arrange(Date) %>%
  mutate(Home.Rest = ifelse(Home.Team == as.character(teams[count,1]),Date - lag(Date,1), 0)) %>%
  mutate(Away.Rest = ifelse(Away.Team == as.character(teams[count,1]),Date - lag(Date,1), 0)) 

  count = count+1
 new <- bind_rows(new,team)
}


new <- new %>%
  group_by(Game) %>% 
  summarise(Home.Rest = sum(Home.Rest),Away.Rest = sum(Away.Rest))

#categorise Rest into more meaningful groups. over 2 weeks = 15, 12-14 =12.  less than 5 is 5.  Helps to clean up cases for Bradley Terry (paired comparison) matrix
new <- new%>% 
  mutate(Home.Rest = ifelse(is.na(Home.Rest),15, ifelse(Home.Rest>15,15, ifelse(Home.Rest>12,12,ifelse(Home.Rest<5,5,Home.Rest))))) %>%
  mutate(Away.Rest = ifelse(is.na(Away.Rest),15, ifelse(Away.Rest>15,15, ifelse(Away.Rest>12,12,ifelse(Away.Rest<5,5,Away.Rest)))))

#join Home.Rest and Away.Rest statistics with main data table
data <- left_join(data,new,by = 'Game')

#create 4 column win matrix for Bradley Terry Analysis colnames "Home.Rest" "Away.Rest" "Home.Win" "Away.Win"
rest_4col <- data %>%
  select(Home.Rest,Away.Rest,Home.Win,Away.Win) %>%
  mutate(Home.Rest = as.factor(Home.Rest),Away.Rest = as.factor(Away.Rest))

#Nested BT analysis to create lookup matrix for rest weighting
rest_btp <- btprob(btfit(btdata(rest_4col,return_graph = TRUE),1))

#Fill diagonal with 0.5 (i.e. 5 days rest vs 5 days rest -> no advantage)
x<- 1:nrow(rest_btp)
count= 1
for(val in x){
  rest_btp[count,count] <- 0.5
  count <- count+1
}

#Fill in Rest adv/disadv row by row based on rest lookup in BT Rest Table
data <- data%>%rowwise%>%
  mutate(Home.Rest.Adv = as.double(rest_btp[as.character(Home.Rest),as.character(Away.Rest)])) %>%
  mutate(Away.Rest.Adv = 1-Home.Rest.Adv)


#Home.Win.Modified is Win Share which has been adjusted by margin, home/away advantage and rest advantage
#In each case, the home v away advantage is keep proportional

data <- data %>%
  mutate(Home.Win.Modified = Home.Win *(1-margin_mod) + margin_mod * Home.Win * Margin.Mod) %>%
  mutate(Away.Win.Modified = Away.Win *(1-margin_mod) + margin_mod * Away.Win * (1-Margin.Mod)) %>%

  mutate(Home.Win.Modified = if_else(Home.Win.Modified == 0,1-Away.Win.Modified,Home.Win.Modified)) %>%
  mutate(Away.Win.Modified = if_else(Away.Win.Modified == 0,1-Home.Win.Modified,Away.Win.Modified)) %>%

  mutate(Home.Win.Proportion = Home.Venue.Adv/(1-Home.Venue.Adv) *  Home.Rest.Adv/Away.Rest.Adv) %>%
  mutate(Home.Win.Proportion = Home.Win.Proportion/(1+Home.Win.Proportion)) %>%
  
  mutate(Home.Win.Modified = min(1,Home.Win.Modified *0.5/Home.Win.Proportion))%>%
  mutate(Away.Win.Modified = 1-Home.Win.Modified)



data <- data %>% mutate(Home.Elo = 1200,Away.Elo = 1200)



#ELO CALCS
#Game by game, calcs are only performed up to specified season, otherwise most recent elo is filled.  This is equivalent to making all of a seasons predictions at the start of the season


count = 1
x = 1:nrow(data)

for(val in x){
  
    home.elo <-  teams[data$Home.Team.ID[count],2]
  away.elo <-  teams[data$Away.Team.ID[count],2]
  
  home.win <- data$Home.Win.Modified[count]
  
  data$Home.Elo[count] <- as.numeric(home.elo)
  data$Away.Elo[count] <- as.numeric(away.elo)
  
  if(count < nrow(data%>% filter(Season <2018))){
 elo_hold <- elo.calc(home.win,home.elo,away.elo,k)

  home.elo <- elo_hold[1,1]
  away.elo <- elo_hold[1,2]
  }
  teams[data$Home.Team.ID[count],2] <- home.elo
  teams[data$Away.Team.ID[count],2] <- away.elo
  
  count = count+1
}


#creating team by team data for elo changes over time - used to plot
x = 1:nteams
count = 1
for(val in x){
  teamstr <- teams[count,1]
  teamplot<- data %>% filter(Home.Team == teamstr | Away.Team == teamstr) %>%
    mutate(team.elo = ifelse(Home.Team == teamstr, Home.Elo,Away.Elo )) %>%
    mutate(elo.track = count)

  if(count == 1){compplot = teamplot}
  
  compplot <- bind_rows(compplot,teamplot)
  count = count+1
}

x = 1:nrow(compplot)
count = 1
for(val in x){
compplot$elo.track[count] = as.character(teams[as.integer(compplot$elo.track[count]),1])
count = count+1
}

comp_ELO_plot <- compplot %>%
  group_by(elo.track) %>%
  ggplot() + geom_line(aes(x = Date,y = team.elo,group = elo.track,colour = elo.track)) +
  labs(x = "Date", y = "Elo Rating (1200 base)")



#BEGIN TO CLEANUP ODDS DATA

#convert colnames to shared colnames
colnames(odds) <- c("Date","Kickoff","Home.Team","Away.Team","Venue","Home.Points","Away.Points","Poff","Home.Goals","Home.Behinds","Away.Goals","Away.Behinds","Home.Odds","Away.Odds")

#date cleaned via lubridate
odds <- odds%>%
  mutate(Date = ymd(Date)) 

#Difference in team names fixed
odds <- odds %>%
  mutate(Home.Team = if_else(Home.Team == "GWS Giants","GWS",Home.Team)) %>%
  mutate(Home.Team = if_else(Home.Team == "Brisbane","Brisbane Lions",Home.Team)) %>%
  mutate(Home.Team = if_else(Home.Team == "Western Bulldogs","Footscray",Home.Team)) %>%
  mutate(Away.Team = if_else(Away.Team == "GWS Giants","GWS",Away.Team)) %>%
  mutate(Away.Team = if_else(Away.Team == "Brisbane","Brisbane Lions",Away.Team)) %>%
  mutate(Away.Team = if_else(Away.Team == "Western Bulldogs","Footscray",Away.Team))
  
#join odds and results data
betting <- left_join(data,odds,by = c("Date","Home.Team","Away.Team"))


#
betreport <- betting %>%
 mutate(Home.MOdds = 1/elo.prob(Home.Elo,Away.Elo)) %>%
mutate(Away.MOdds = 1/elo.prob(Away.Elo,Home.Elo)) %>%
  mutate(PL = 0)

betreport <- betreport %>%
  select(Game,Date,Season,Home.Team,Away.Team,Home.Win,Away.Win,Home.Odds,Away.Odds,Home.MOdds,Away.MOdds) %>%
  mutate(Bet = ifelse(Home.Odds>Home.MOdds,Home.Team,ifelse(Away.Odds>Away.MOdds,Away.Team,NA))) %>%
  mutate(PL = ifelse(Bet == Home.Team,1*Home.Odds*Home.Win-1,ifelse(Bet == Away.Team,1*Away.Odds*Away.Win-1,0))) %>%
  mutate(PL = ifelse(is.na(PL),0,PL))%>%
  filter(Season ==2018) %>%
  mutate(cumPL = 0)

x = 1:nrow(betreport)
count = 1
cumPL = 0
for(val in x){
  cumPL =cumPL + betreport$PL[count]
  betreport$cumPL[count] = cumPL 
count = count+1
}


ggplot(compplot) + geom_line(aes(x = Date,y = team.elo,group = elo.track,colour = elo.track)) +
  labs(x = "Date", y = "Elo Rating (1200 base)")

ggplot(betreport) + geom_line(aes(x = Game,y = cumPL))




