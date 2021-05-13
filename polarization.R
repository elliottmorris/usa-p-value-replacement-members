library(tidyverse)

results <- politicaldata::house_results
# results <- politicaldata::pres_results_by_cd %>%
#   group_by(year,state_abb) %>%
#   mutate(num=n(),
#          district = dplyr::case_when(num == 1 ~ "AL",
#                                      district < 10 ~ paste0("0",district),
#                                      TRUE ~ as.character(district)),
#          district = sprintf('%s-%s',state_abb,district)) %>%
#   ungroup()

house <- read_csv('data/Hall_members.csv')

house <- house %>% 
  dplyr::filter(chamber=='House') %>% 
  mutate(year = congress*2 + 1786) %>%
  group_by(year,state_abbrev) %>%
  mutate(num=n(),
         district = dplyr::case_when(num == 1 ~ "AL",
                                     district_code < 10 ~ paste0("0",district_code),
                                     TRUE ~ as.character(district_code)),
         district = sprintf('%s-%s',state_abbrev,district),
         party_code = ifelse(party_code == 100, 'Dem','Rep')) %>%
  select(year,state_abbrev,district,bioname,party=party_code,d1= nominate_dim1)

house <- results %>% 
  left_join(house,by=c('year','district')) %>% 
  filter(total_votes>1,!is.na(dem),!is.na(rep),!is.na(party),!is.na(dem),!is.na(d1))  %>%
  ungroup() %>%
  # normalize measures
  #mutate(dem = (dem - mean(dem))/sd(dem), d1 = (d1 - mean(d1))/sd(d1))
  # two-party share
  mutate(dem = (dem / (dem + rep))*100) %>%
  filter(!(party=='Rep'&d1<0.05),!(party=="Rep"&dem>50)) %>%
  filter(!(party=='Dem'&d1>0.05),!(party=="Dem"&dem<50)) 



# charts
start_year <- 1996
end_year <- 2018

house %>% 
  filter(year %in% c(end_year,start_year)) %>%
  mutate(group = paste0(year,party),
         year = as.character(year)) %>%
  ggplot(.,aes(x=dem, d1,col=party,group=group,linetype=year)) +
  geom_point(alpha=0.5,aes(shape=year)) +
  geom_smooth(method='lm') +
  theme_minimal() +
  labs(x='Democratic two-party house vote in district',
       y='DW Nominate Dimension 1') +
  scale_color_manual(values=c("Dem"="blue","Rep"="red")) +
  scale_x_continuous(breaks=seq(0,100,10))

house %>% 
  filter(year %in% c(end_year,start_year)) %>%
  filter(!(party=='Rep'&d1<0)) %>%
  mutate(group = paste0(year,party),
         year = as.character(year)) %>%
  group_by(group) %>%
  mutate(median = median(dem)) %>%
  ggplot(.,aes(x=dem,col=party,group=group,linetype=year)) +
  geom_density() +
  #geom_vline(aes(xintercept=median*100)) +
  theme_minimal() +
  labs(x='Democratic two-party house vote in district',
       y='Density') +
  scale_color_manual(values=c("Dem"="blue","Rep"="red")) +
  theme(axis.text.y = element_blank())+
  scale_x_continuous(breaks=seq(0,100,10))


# table
house %>% 
  filter(year %in% c(start_year,end_year)) %>%
  filter(!(party=='Rep'&d1<0)) %>%
  mutate(group = paste0(year,party),
         year = as.character(year)) %>%
  group_by(year,party) %>%
  summarise(median_dem_share = median(dem),
            median_d1 = median(d1))  %>%
  group_by(party) %>%
  arrange(party,year) %>%
  mutate(pct_chg_dem_share = median_dem_share/lag(median_dem_share),
         pct_chg_d1 = median_d1/lag(median_d1))


models <- house %>% 
  group_by(party,year) %>%
  do(model = lm(d1 ~ dem,data=.))

models$coef = lapply(1:nrow(models),
      function(x){coef(models[x,]$model[[1]])[1]}
      ) %>% unlist


ggplot(models,aes(x=year,y=coef,col=party)) + 
  geom_line() +
  geom_smooth(method='lm',linetype=2,se=F,size=0.5) +
  theme_minimal() +
  scale_color_manual(values=c("Dem"="blue","Rep"="red")) + 
  labs(subtitle='Intercept of model dwnom dim 1 ~ dem vote, grouped by party x year')+
  scale_x_continuous(breaks=seq(0,3000,4))
  
     