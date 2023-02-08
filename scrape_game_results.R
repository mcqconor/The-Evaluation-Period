#install necessary packages
#install.packages('tidyverse')
#install.packages('rvest')
#install.packages('data.table')

#load in the necessary packages
library(tidyverse)
library(rvest)
library(data.table)

#A note on using data.table: If you are new to R I do not recommend it. I use it here because it is much faster, but the syntax is much more
#difficult to parse, especially if you're new to this. If you are using this as a learning exercise, I would encourage you to use my comments
#as notes to translate this into dplyr

#I will go extremely in depth on how I scraped the post merger (save the strike seasons of 1982 and 1987) game results
#Every other section is scraped in the same way, just separated either for readability/to keep me sane, or because
#in some cases PFR will do something odd such as skip a random week (say Week 12) and I find it easier to just make 
#Exceptions in those cases which follow the same basic structure

#list out the years we would like to scrape
merger_games <- c(1970:1981, 1983:1986, 1988:2022) %>% 
  #make list to input into pmap_dfr
  list() %>% 
  #this defines a temporary function (lambda function) that scrapes every week in a year, wrangles the tables, and spits them all out
  #the benefit of these maps is they logically work like for loops while being much faster and pmap_dfr in particular delivers everything
  #binded together at the end to a single data frame
  pmap_dfr(function(x){
    
    #Just because I like to know progress
    print(x)
    
    #the length of the NFL season varies over time. So I just went ahead and listed them out here because it made the logic of determining 
    #playoffs easier later, but if you are a real go getter I believe there is a way to go ahead and grab the weeks in a season right from PFR
    #Consider this homework if you want to improve on my code
    max_week = case_when(
      x %in% c(1970:1977) ~ 17,
      x %in% c(1981) ~ 19,
      x %in% c(1978:1980, 1983:1986, 1988:1989) ~ 20,
      x %in% c(1990:1992, 1994:2020, 2022) ~ 21,
      x %in% c(1993, 2021) ~ 22
    )
    
    #This is where things get a little messy, I am nesting for loops which is generally slow. Luckily the processes we're doing here are
    #fast bc of data.table, but if you switch to dplyr you will see performance drops
    #This just loops over every week in the given season from 1 to the max_week determined earlier. We feed it into a pmap_dfr the same way
    #as we did earlier
    games <- c(1:max_week) %>%
      list() %>% 
      pmap_dfr(function(y){
        
        #Sports Reference allows you to access 20 pages a minutes, and if you do any more they'll lock you out for an hour
        #even on my suboptimal machine, this code easily clears 20 a minute, so we make the machine rest the minimum amount
        #of time to not get locked out
        Sys.sleep(3.1)
        
        #This is the actual scraping. We go to the URL of the week in the season we are looking at
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',x,'/week_',y,'.htm')) %>%
          #We grab all the tables in the specific section of the webpage we want
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          #We tell R that they are tables, and it returns us a list with every element being a table for a game in a week
          html_table()
        
        #Each game in a week on PFR actually has 2 tables 
        #The first table is the actual result with away score on top and home score on the bottom
        #The second table says the passing, rushing, and receiving leader of the game (where applicable)
        #we only care about the score result of the game, so we only get the tables at odd indexes
        
        #To clarify, this means if there are 4 games in a week, we will have 8 tables. Tables 1, 3, 5, 7 
        #will have the final score and team names, while tables 2, 4, 6, 8 have the statistical leaders.
        #All this line does is give us tables 1, 3, 5, 7 such that we don't have to say explicitly how many
        #games are in each week
        cc <- cc[seq(1,length(cc), 2)]
        
        
        #the final score tables are in a list, so we want to apply the same set of functions on 
        #every dataframe in the list at the same time to maximize efficiency. That's what this does
        
        #in case you're unfamiliar with data.table syntax, you refer to elements via table[row,column]
        #the goal of this is a table which has one row for every game in a week
        lapply(cc, function(z){
          
          #turn each element to a data.table, then remove the first row (only the game's date)
          #and the 3rd column (which only has the string "final")
          data.table(z)[-1,][,-3][
            
            #now we define a bunch of columns to manipulate the data into the form we want
            ,`:=` (
              season = c(x,x), #input the season
              week = c(y,0), #input the week
              away = c(X1[1],'b'), #The away team is the first element in the first column
              away_score = c(as.numeric(X2[1]), 0), #The Away score is the first element in the second column
              home = c(X1[2],'a'), #The home team is the second element in the first column
              home_score = c(as.numeric(X2[2]), 0), #The home score is the second element in the second column
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0), #as an arbitrary convention, I chose to represent the margin as away - home
              neutral_field = c(ifelse(y == max_week, 1, 0),0), #for simplicity, I assume only the SB is on a neutral field
              playoffs = c( #because each season has a different number of weeks, the playoffs start at diferent times
                case_when(
                  x %in% c(1970:1977) & y >= 15 ~  1,
                  x %in% c(1978:1980, 1983:1986, 1988:1989) & y >= 17 ~ 1,
                  x %in% c(1990:1992, 1994:2021) & y >= 18 ~ 1,
                  x %in% c(1993, 2021, 2021, 1981) & y >= 19 ~ 1,
                  T ~ 0
                ),
                0),
              championship = c(ifelse(y == max_week, 1, 0), 0) #the last week of the season is the championship
            )
            #we remove the first and second columns (only had team names and scores) and only keep the 1st row for 
            #1 row per game. Sometimes a game isn't played or we don't have the result recorded, and in those cases
            #the scores are NA so the margin is noted as NA. We won't fuss over these, so we'll just remove them
          ][,c(-1,-2)][1,][!is.na(margin),] 
        }) %>% 
          #we bind all the tables in the list to a single table
          bind_rows()
        
      })
    
  })

#AFPA GAMES
#The only thing to keep in mind with these years is that if a league
#is listed under a non-NFL name in PFR then you have to put a _[league abbr]
#in the URL after the year
apfa_check <- c(1920:1921) %>% 
  list() %>% 
  pmap_dfr(function(x){
    
    print(x)
    
    games <- c(1:13) %>%
      list() %>% 
      pmap_dfr(function(y){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',x,'_APFA/week_',y,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(x,0),
              week = c(y,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(0, 0),
              championship = c(0, 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
    
  })

#1922 to 1949
#this is a wide range, basically from when the NFL was called the NFL to when some
#AAFC teams joined the NFL
#since only 2 AAFC teams joined (Browns, 49ers) I don't include them in this count
#Fun fact, the NFL didn't have a championship game until 1933!
pre_aafc <- c(1922:1949) %>% 
  list() %>% 
  pmap_dfr(function(x){
    
    print(x)
    
    max_week <- case_when(
      x %in% c(1922, 1923) ~ 11,
      x %in% c(1924) ~ 10,
      x %in% c(1946, 1949, 1927:1929 ,1945) ~ 13,
      x %in% c(1925, 1926, 1930:1936, 1938:1940, 1942, 1944, 1948) ~ 14,
      x %in% c(1937, 1943, 1947) ~ 15,
      x %in% c(1941) ~ 16
    )
    
    games <- c(1:max_week) %>%
      list() %>% 
      pmap_dfr(function(y){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',x,'/week_',y,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(x,0),
              week = c(y,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(case_when(
                x >= 1932 & y == max_week ~ 1,
                x == 1939 & y >= 13 ~ 1,
                x == 1941 & y >= 15 ~ 1,
                x %in% c(1943,1947,1937) & y >= 14 ~ 1,
                T ~ 0
              ), 0),
              championship = c(ifelse(x >= 1932 & y == max_week, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
    
  })

#1953, 1955, 1959 all skip Week 13 for some reason
exp_1953_59 <- c(1953, 1955, 1959) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:12,14) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(0, 0),
              championship = c(ifelse(x == 14, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#1961-1964 all skip week 15 for some reason
exp_1961_64 <- c(1961:1964) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:14,16) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(0, 0),
              championship = c(ifelse(x == 16, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#1966 skips week 16
exp_1966 <- c(1966) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:15,17:18) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(ifelse(x >= 17, 1, 0), 0),
              championship = c(ifelse(x == 18, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#1950-1952, 1954, 1956-1960, 1965, 1967-1969
#This is the time period pre-SB but post-AAFC, the NFL is the only player in town
#for most of this era but doesn't have an extremely solid footing
pre_sb_nfl <- c(1950:1952, 1954, 1956:1958, 1960, 1967:1969) %>% 
  list() %>% 
  pmap_dfr(function(x){
    
    print(x)
    
    max_week <- case_when(
      x %in% c(1950) ~ 15,
      x %in% c(1951, 1955, 1959) ~ 13,
      x %in% c(1952, 1954, 1956:1958, 1960) ~ 14,
      x %in% c(1965) ~ 16,
      x %in% c(1963, 1967, 1968) ~ 17,
      x %in% c(1966, 1969) ~ 18
    )
    
    games <- c(1:max_week) %>%
      list() %>% 
      pmap_dfr(function(y){
        
        # print(y)
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',x,'/week_',y,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(x,x),
              week = c(y,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(ifelse(y == max_week & x >= 1966, 1, 0),0),
              playoffs = c(case_when(
                x == 1950 & y >= 14 ~ 1,
                x %in% c(1957, 1952, 1958) & y >= 13 ~ 1,
                x %in% c(1965, 1967, 1968, 1969) & y >= 15 ~ 1,
                x == 1966 & y >= 17 ~ 1,
                T ~ 0
              ), 0),
              championship = c(ifelse(y == max_week, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
    
  })

#1961 - 1965, 1967-1968
#The entire AFL would go on to join the NFL, and the NFL would go on to adopt
#some AFL rules in its future, so I go ahead and include them in this tally
pre_merger_afl <- c(1961:1962, 1964:1965, 1967:1968) %>% 
  list() %>% 
  pmap_dfr(function(x){
    
    print(x)
    
    max_week <- case_when(
      x %in% c(1961, 1962, 1964, 1965) ~ 16,
      x %in% c(1967, 1968) ~ 17,
      x %in% c(1963, 1969) ~ 18
    )
    
    games <- c(1:max_week) %>%
      list() %>% 
      pmap_dfr(function(y){
        
        # print(y)
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',x,'_AFL/week_',y,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(x,x),
              week = c(y,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]), 0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]), 0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(ifelse(y == max_week & x >= 1966, 1, 0),0),
              playoffs = c(case_when(
                x == 1963 & y >= 17 ~ 1,
                x == 1968 & y >= 16 ~ 1,
                T ~ 0
              ), 0),
              championship = c(ifelse(y == max_week, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
    
    
  })

#AFL 1960 skips Week 16
afl_1960 <- c(1960) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:15,17) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'_AFL/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(0, 0),
              championship = c(ifelse(x == 17, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#1963 AFL skips Week 12
afl_1963 <- c(1963) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:11,13:18) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'_AFL/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(0,0),
              playoffs = c(ifelse(x >= 17, 1, 0), 0),
              championship = c(ifelse(x == 18, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#1966 AFL skips Week 17
afl_1966 <- c(1966) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:16,18) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'_AFL/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(ifelse(x == 19, 1, 0),0),
              playoffs = c(0, 0),
              championship = c(ifelse(x == 18, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#AFL 1969 skips Week 16
afl_1969 <- c(1969) %>% 
  list() %>% 
  pmap_dfr(function(y){
    print(y)
    
    c(1:15,17,18) %>% 
      list() %>% 
      pmap_dfr(function(x){
        
        Sys.sleep(3.1)
        
        cc <- read_html(paste0('https://www.pro-football-reference.com/years/',y,'_AFL/week_',x,'.htm')) %>% 
          html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
          html_table()
        
        cc <- cc[seq(1,length(cc), 2)]
        
        lapply(cc, function(z){
          data.table(z)[-1,][,-3][
            ,`:=` (
              season = c(y,0),
              week = c(x,0),
              away = c(X1[1],'b'),
              away_score = c(as.numeric(X2[1]),0),
              home = c(X1[2],'a'),
              home_score = c(as.numeric(X2[2]),0),
              margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
              total = c(as.numeric(X2[1])+as.numeric(X2[2]), 0),
              neutral_field = c(ifelse(x == 18, 1, 0),0),
              playoffs = c(ifelse(x >= 15, 1, 0), 0),
              championship = c(ifelse(x == 18, 1, 0), 0)
            )
          ][,c(-1,-2)][1,][!is.na(margin),]
        }) %>% 
          bind_rows()
        
      })
  })

#The NFL Went on strike in 1982 and canceled Weeks 3-10
#Strikes at times in history have accomplished some goals but they mess with my code!
games_1982 <- c(1,2,11:21) %>%
  list() %>% 
  pmap_dfr(function(y){
    
    Sys.sleep(3.1)
    
    cc <- read_html(paste0('https://www.pro-football-reference.com/years/1982/week_',y,'.htm')) %>% 
      html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
      html_table()
    
    cc <- cc[seq(1,length(cc), 2)]
    
    lapply(cc, function(z){
      data.table(z)[-1,][,-3][
        ,`:=` (
          season = c(1982,0),
          week = c(y,0),
          away = c(X1[1],'b'),
          away_score = c(as.numeric(X2[1]), 0),
          home = c(X1[2],'a'),
          home_score = c(as.numeric(X2[2]), 0),
          margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
          neutral_field = c(ifelse(y == 21, 1, 0),0),
          playoffs = c(
            case_when(
              y >= 18 ~ 1,
              T ~ 0
            ),
            0),
          championship = c(ifelse(y == 21, 1, 0), 0)
        )
      ][,c(-1,-2)][1,][!is.na(margin),]
    }) %>% 
      bind_rows()
    
  })

#The NFL again went on strike in 1987, but the NFL only canceled one game (Week 3) and played
#a lot of games with scabs or players who were blackmailed into crossing the picket line.
#Could make a reasonable argument to not include this season, but it is still history
games_1987 <- c(1,2,4:20) %>%
  list() %>% 
  pmap_dfr(function(y){
    
    Sys.sleep(3.1)
    
    cc <- read_html(paste0('https://www.pro-football-reference.com/years/1987/week_',y,'.htm')) %>% 
      html_nodes(xpath = '//*[@id="content"]/div[4]/div/table') %>% 
      html_table()
    
    cc <- cc[seq(1,length(cc), 2)]
    
    lapply(cc, function(z){
      data.table(z)[-1,][,-3][
        ,`:=` (
          season = c(1987,0),
          week = c(y,0),
          away = c(X1[1],'b'),
          away_score = c(as.numeric(X2[1]), 0),
          home = c(X1[2],'a'),
          home_score = c(as.numeric(X2[2]), 0),
          margin = c(as.numeric(X2[1])-as.numeric(X2[2]), 0),
          neutral_field = c(ifelse(y == 20, 1, 0),0),
          playoffs = c(
            case_when(
              y >= 17 ~ 1,
              T ~ 0
            ),
            0),
          championship = c(ifelse(y == 20, 1, 0), 0)
        )
      ][,c(-1,-2)][1,][!is.na(margin),]
    }) %>% 
      bind_rows()
    
  })

#We smush them all together so we can do some analysis!
all_games <- bind_rows(apfa_check, pre_aafc) %>% 
  bind_rows(exp_1953_59) %>% 
  bind_rows(exp_1961_64) %>% 
  bind_rows(exp_1966) %>% 
  bind_rows(pre_sb_nfl) %>% 
  bind_rows(afl_1960) %>% 
  bind_rows(afl_1963) %>% 
  bind_rows(afl_1966) %>% 
  bind_rows(afl_1969) %>% 
  bind_rows(pre_merger_afl) %>% 
  bind_rows(merger_games) %>% 
  bind_rows(games_1982) %>% 
  bind_rows(games_1987) %>% 
  unique()