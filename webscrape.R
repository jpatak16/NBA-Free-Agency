library(pacman)
p_load(tidyverse, rvest, janitor)

years = 2013:2022

fa_contract_data = data.frame()

url = paste0('https://www.spotrac.com/nba/free-agents/2016')

session = session(url)
form = html_form(session)[[3]]
form = form %>% html_form_set(email = Sys.getenv("SPOTRAC_EMAIL"), password = Sys.getenv("SPOTRAC_PW"))

session = session_submit(session, form)



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
  mutate(offseason = y)
  

Sys.sleep(20)

fa_contract_data = rbind(fa_contract_data, df)
}

rm(df, form, session, url, urls_fa, y)