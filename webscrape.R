#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

standardize_name = function(player_name){
  #remove periods
  name = gsub("\\.", "", player_name)
  #remove apostrophe
  name = gsub("'", "", name)
  #remove dashes
  name = gsub("-", "", name)
  #remove *
  name = gsub("\\*", "", name)
  #remove spaces and any third or more name element
  name = paste0(strsplit(name, split = " ")[[1]][1], strsplit(name, split = " ")[[1]][2])
  #upcase all letters
  name = toupper(name)
  return(name)
}
standardize_name = Vectorize(standardize_name)

library(pacman)
p_load(tidyverse, rvest, janitor)

years = 2013:2022

fa_contract_data = data.frame()
sr_season_stats = data.frame()

url = paste0('https://www.spotrac.com/nba/free-agents/2016')

#sign into spotrac
session = session(url)
form = html_form(session)[[3]]
form = form %>% html_form_set(email = Sys.getenv("SPOTRAC_EMAIL"), password = Sys.getenv("SPOTRAC_PW"))

session = session_submit(session, form)


#scrape spotrac FA list
for(y in years){
urls_fa = paste0('https://www.spotrac.com/nba/free-agents/', y)

df = session %>% session_jump_to(urls_fa) %>% read_html() %>%
  html_element(xpath = '//*[(@id = "main")]') %>%
  html_table() %>% 
  row_to_names(row_number = 1) %>%
  rename(Player = 1,
         AAV = 'Avg. Salary',
         MaxVal = 'Max Value') %>%
  filter(Player != "",
         Player != "Totals", 
         To != "TBD") %>%
  mutate(Yrs = as.numeric(Yrs),
         Age = as.numeric(Age),
         Age = ifelse(Age > 60, NA, Age), #for a couple players, age is the offseason year and not their age
         Value = gsub("[^0-9]", "", Value) %>% as.numeric(),
         AAV = gsub("[^0-9]", "", AAV) %>% as.numeric(),
         MaxVal = gsub("[^0-9]", "", MaxVal) %>% as.numeric()) %>%
  filter(Value != 0,
         Type != "",
         Yrs != 10) %>% #remove a row that is a 10 day contract miscoded as 10 years
  select(1:10) %>%
  mutate(offseason = y) %>%
  mutate(Player_join = stringi::stri_trans_general(Player, "Latin-ASCII")) %>%
  mutate(Player_join = standardize_name(Player_join))
  

Sys.sleep(20)

fa_contract_data = rbind(fa_contract_data, df)
}

rm(df, form, session, url, urls_fa, y)

#scrape sr stats
years = 2012:2022
for(y in years){
sr_total_url = paste0('https://www.basketball-reference.com/leagues/NBA_', y ,'_totals.html')

totals = read_html(sr_total_url) %>%
  html_table() %>% .[[1]] %>%
  filter(Player != "Player")

change_tm = totals %>%
  filter(Tm == 'TOT') %>%
  select(Player) %>%
  as.vector() %>%
  .[[1]]

totals_new = totals %>%
  filter(Tm == 'TOT' | !Player %in% change_tm)

Sys.sleep(20)

sr_advanced_url = paste0('https://www.basketball-reference.com/leagues/NBA_', y ,'_advanced.html')

advanced = read_html(sr_advanced_url) %>%
  html_table() %>% .[[1]] %>%
  select(-c(20,25)) %>%
  filter(Player != "Player") %>%
  filter(Tm == 'TOT' | !Player %in% change_tm)

Sys.sleep(20)

sr_pbp_url = paste0('https://www.basketball-reference.com/leagues/NBA_', y ,'_play-by-play.html')

pbp = read_html(sr_pbp_url) %>%
  html_table() %>% .[[1]] %>%
  row_to_names(row_number = 1) %>%
  rename(FC_Shoot = 17,
         FC_Off = 18,
         FD_Shoot = 19,
         FD_Off = 20) %>%
  filter(Player != "Player") %>%
  filter(Tm == 'TOT' | !Player %in% change_tm)

Sys.sleep(20)

sr_shoot_url = paste0('https://www.basketball-reference.com/leagues/NBA_', y ,'_shooting.html')

shoot = read_html(sr_shoot_url) %>%
  html_table() %>% .[[1]] %>%
  row_to_names(row_number = 1) %>%
  select(c(2, 5, 28, 29, 31, 32)) %>%
  filter(Player != "Player") %>%
  filter(Tm == 'TOT' | !Player %in% change_tm) %>%
  rename('DunkA/FGA' = 3,
         MadeDunks = 4,
         'Corner3/3PA' = 5,
         'Corner3FG%' = 6)

Sys.sleep(20)

SR_stats_yr = totals_new %>%
  left_join(advanced) %>%
  left_join(pbp, by=c("Player", "Pos", "Age", "Tm", "MP")) %>%
  left_join(shoot) %>%
  #can't let it join on all common vars because one player has conflicting info on common var in seperate tables
  select(-c(Rk.x, Rk.y, G.y)) %>%
  rename(G = G.x) %>%
  mutate(season = y)

sr_season_stats = rbind(sr_season_stats, SR_stats_yr)

}

rm(advanced, pbp, shoot, SR_stats_yr, totals, totals_new, change_tm, sr_advanced_url, sr_pbp_url, sr_shoot_url, sr_total_url, y)

sr_season_stats= sr_season_stats %>%
  mutate_at(vars('PG%':'C%'), ~ str_remove(., pattern = "%")) %>%
  mutate_at(vars(G:season), as.numeric) %>%
  clean_names() %>%
  mutate_at(vars(c(fg_percent, x3p_percent, x2p_percent, e_fg_percent, ft_percent, ts_percent, x3p_ar, f_tr,
                   tov_percent, pg_percent, sg_percent, sf_percent, pf_percent, c_percent,
                   dunk_a_fga, corner3_3pa, corner3fg_percent)), ~ifelse(is.na(.), 0, .))

#for each offseason year, create a data set for the prior two regular seasons
years = 2013:2022
sr_twoyr_dataset = data.frame()
for(y in years){
  player_twoyr_stats_totals = sr_season_stats %>%
    filter(season == y | season == y-1) %>%
    mutate_at(vars(pg_percent:c_percent), ~(./100) * mp) %>%
    mutate(dunk_att = dunk_a_fga*fga) %>%
    mutate(corner3_att = corner3_3pa*x3pa) %>%
    mutate(corner3_made = corner3fg_percent*corner3_att) %>%
    group_by(player) %>%
    summarise_at(vars(c(g, gs, mp, fg, fga, x3p, x3pa, x2p, x2pa, ft, fta, orb, drb, trb, ast, stl, blk, tov, pf, pts, ows, dws, ws, 
                       bad_pass, lost_ball, fc_shoot, fc_off, fd_shoot, fd_off, pga, and1, blkd, made_dunks, pg_percent, sg_percent,
                       sf_percent, pf_percent, c_percent, dunk_att, corner3_att, corner3_made)), 
              ~ sum(.)) %>%
    mutate_at(vars(pg_percent:c_percent), ~./mp)
  
  player_twoyr_stats_pct = player_twoyr_stats_totals %>% 
    mutate(fg_pct = fg/fga,
           x3p_pct = x3p/x3pa,
           x2p_pct = x2p/x2pa,
           efg_pct = (fg + .5*x3p)/fga,
           ft_pct = ft/fta,
           ts_pct = pts/(2*(fga + .44*fta)),
           x3par = x3pa/fga,
           ftr = fta/fga,
           tov_pct = (100*tov)/(.44*fta + fga + tov),
           ws_48 = (ws*48)/mp,
           dunkA_fga = dunk_att / fga,
           corner3a_3pa = corner3_att/x3pa,
           corner3_pct = corner3_made / corner3_att) %>%
    select(c(player, fg_pct:corner3_pct)) %>%
    mutate_at(vars(c(2:14)), ~ ifelse(is.na(.), 0, .))
  
  player_twoyr_stats_adv = sr_season_stats %>%
    filter(season == y | season == y-1) %>%
    group_by(player) %>%
    summarise_at(vars(c(per, orb_percent, drb_percent, trb_percent, ast_percent, stl_percent, blk_percent, usg_percent,
                        obpm, dbpm, bpm, vorp, on_court, on_off)), 
                 ~ mean(.))
  
  temp = player_twoyr_stats_totals %>%
    left_join(player_twoyr_stats_pct) %>%
    left_join(player_twoyr_stats_adv) %>%
    mutate(offseason = y) %>%
    mutate(player_join = stringi::stri_trans_general(player, "Latin-ASCII")) %>%
    mutate(player_join = standardize_name(player_join)) %>%
    mutate(player_join = ifelse(player_join=="NICCLAXTON", "NICOLASCLAXTON", player_join),
           player_join = ifelse(player_join=="MOBAMBA", "MOHAMEDBAMBA", player_join),
           player_join = ifelse(player_join=="SVIMYKHAILIUK", "SVIATOSLAVMYKHAILIUK", player_join),
           player_join = ifelse(player_join=="LOUWILLIAMS", "LOUISWILLIAMS", player_join),
           player_join = ifelse(player_join=="ISHSMITH", "ISHMAELSMITH", player_join),
           player_join = ifelse(player_join=="JJBAREA", "JOSEBAREA", player_join),
           player_join = ifelse(player_join=="JOSHGRAY", "JOSHIAGRAY", player_join),
           player_join = ifelse(player_join=="NENENA", "NENEHILARIO", player_join),
           player_join = ifelse(player_join=="LUCMBAH", "LUCRICHARD", player_join),
           player_join = ifelse(player_join=="JAMESMICHAEL", "JAMESMCADOO", player_join),
           player_join = ifelse(player_join=="LOUAMUNDSON", "LOUISAMUNDSON", player_join),
           player_join = ifelse(player_join=="DEJUANBLAIR", "DEJAUNBLAIR", player_join),
           player_join = ifelse(player_join=="BYRONMULLENS", "BJMULLENS", player_join))
  
  sr_twoyr_dataset = rbind(sr_twoyr_dataset, temp)
}

rm(player_twoyr_stats_adv, player_twoyr_stats_pct, player_twoyr_stats_totals, temp, y, years)

free_agents_stats = fa_contract_data %>%
  left_join(sr_twoyr_dataset, by=c('Player_join' = 'player_join', 'offseason')) %>%
  filter(!is.na(g)) %>%
  select(-c(Player_join, player))
