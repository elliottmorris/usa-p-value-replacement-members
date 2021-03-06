library(tidyverse)
library(zoo)

# senate data -------------------------------------------------------------
senate <- read_csv('data/1964-2020-election-results.csv') %>%
  filter(contested_election == TRUE,
         !is.na(winning_party),
         election_year >= 1980) %>%
  select(year = election_year,
         state_abbv = state,
         district_id = district_id,
         dem = dem_2_party_pct,
         winning_party = winning_party,
         dem_last_name, 
         rep_last_name)  %>%
  mutate(dem_margin = (dem - 0.5)*2) %>%
  arrange(year)


# presidential results ----------------------------------------------------
# read file
pres <- read_csv('data/1976-2020-president.csv')

# clean up party code , then set to two-party %
pres <- pres %>%
  mutate(party_simplified = case_when(party_simplified == 'DEMOCRAT' ~ "dem",
                                      party_simplified == 'REPUBLICAN' ~ "rep",
                                      TRUE ~ 'other')) %>%
  filter(party_simplified %in% c('dem','rep'))

# first, national results
pres_national <- pres %>%
  group_by(year,
           party = party_simplified) %>%
  summarise(pct = sum(candidatevotes, na.rm=T)) %>%
  mutate(pct = pct / sum(pct, na.rm=T))  %>%
  spread(party,pct)  %>%
  mutate(dem_national_margin = dem - rep) %>%
  select(-c(dem,rep))

# now sum up state results
pres <- pres %>%
  group_by(year,
           state_abbv = state_po,
           party = party_simplified) %>%
  summarise(pct = sum(candidatevotes, na.rm=T)) %>%
  group_by(year, state_abbv) %>%
  mutate(pct = pct / sum(pct, na.rm=T))  %>%
  spread(party,pct) 


# find optimal pvi blend for predicting senate results --------------------
# get margins in each year
pvi <- pres %>%
  group_by(state_abbv) %>%
  mutate(dem_margin = round(dem - rep, 4),
         lag_dem_margin = round(lag(dem,1) - lag(rep,1),4)) %>% 
  select(-c(dem,rep)) %>%
  left_join(pres_national) %>%
  mutate(dem_margin_lean = dem_margin - dem_national_margin,
         lag_dem_margin_lean = lag_dem_margin - lag(dem_national_margin,1)) %>%
  na.omit %>%
  select(-c(dem_margin,lag_dem_margin,dem_national_margin))

# merge margins, carrying forward from last pres elec for midterm elecs
senate <- senate %>% 
  left_join(pvi)

senate <- senate %>%
  filter(!is.na(dem_margin_lean)) %>%
  bind_rows(
    senate %>% 
      filter(is.na(dem_margin_lean)) %>%
      select(-c(dem_margin_lean, lag_dem_margin_lean)) %>%
      mutate(year = year - 2) %>%
      left_join(pvi, by = c("year","state_abbv")) %>%
      mutate(year = year + 2)
  )

# if any stragglers, na.locf
senate <- senate %>%
  group_by(state_abbv) %>%
  mutate(dem_margin_lean = na.locf(dem_margin_lean,na.rm=T),
         lag_dem_margin_lean = na.locf(lag_dem_margin_lean,na.rm=T))

unique(senate$year)

# loss function, lets pvi change as linear function of time
root_mean_sq_error <- function(par, dat){
  print("#############")
  err <- dat %>% mutate(year_pvi_coef = NA)

  err$year_pvi_coef = 
    par[1] +
    (par[2]*(err$year-1900))
  
  print(c(par[1],par[2]))
  print(range(err$year_pvi_coef))
  
  err$year_pvi_coef <- case_when(err$year_pvi_coef < 0.6 ~ 0.6,
                                 err$year_pvi_coef > 0.9 ~ 0.9,
                                 TRUE ~ err$year_pvi_coef)
  
  print(range(err$year_pvi_coef))
  
  err$pvi = ((err$dem_margin_lean * err$year_pvi_coef) + 
               (err$lag_dem_margin_lean * (1 - err$year_pvi_coef)))
  
  err$error = err$dem_margin - err$pvi
  
  print(sqrt(mean(err$error^2)))
  
  return(sqrt(mean(err$error^2)))
}

# optimize it
optimize_pvi <- optim(par = c(0.2, 0.005), # starting values
      fn = root_mean_sq_error,
      dat = senate,
      control = list(maxit = 1000000))

optimize_pvi

# add the coef from the optimizer to the df
senate$year_pvi_coef <- NA_real_

senate$year_pvi_coef = 
  optimize_pvi$par[1] + 
  (optimize_pvi$par[2]*((senate$year-1900))) 

senate$year_pvi_coef <- case_when(senate$year_pvi_coef < 0.6 ~ 0.6,
                                  senate$year_pvi_coef > 0.9 ~ 0.9,
                                  TRUE ~ senate$year_pvi_coef)


senate$pvi = ((senate$dem_margin_lean * senate$year_pvi_coef) + 
                  (senate$lag_dem_margin_lean * (1 - senate$year_pvi_coef)))

sqrt(mean( ((senate[senate$year>2009,]$pvi) - 
              (senate[senate$year>2009,]$dem_margin))^2 ) )

# what is the yearly coef
senate %>% 
  select(year,year_pvi_coef) %>% distinct()  %>% as.data.frame

senate %>% 
  select(year,year_pvi_coef) %>% distinct() %>%
  ggplot(., aes(year,year_pvi_coef)) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1))

# what do the predictions look like?
cor(senate$dem_margin_lean, senate$dem_margin)
ggplot(senate, aes(x = lag_dem_margin_lean, y = dem_margin,col = year)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm',aes(group = year),se=F) +
  theme_minimal() +
  geom_abline() +
  labs('col' = '')

cor(senate$lag_dem_margin_lean, senate$dem_margin)
ggplot(senate, aes(x = lag_dem_margin_lean, y = dem_margin,col = year)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm',aes(group = year),se=F) +
  theme_minimal() +
  geom_abline() +
  labs('col' = '')

cor(senate$pvi, senate$dem_margin)
ggplot(senate, aes(x = pvi, y = dem_margin,col = year)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method = 'lm',aes(group = year),se=F) +
  theme_minimal() +
  geom_abline() +
  labs('col' = '')

# write
write_csv(senate, 'output/senate_results_with_pvi.csv')
