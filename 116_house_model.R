library(tidyverse)
library(ggrepel)

# data wrangling ----------------------------------------------------------
# clean house results
cong <- read_csv('data/2020_house_data - Sheet5.csv') %>%
  mutate(last_name = toupper(last_name),
         state_abbv = toupper(gsub("-.*","",Code)))
  
cong <- cong %>% 
  select(state_abbv, code = Code,
         last_name,
         biden_2020,trump_2020,
         dem_house_2020,rep_house_2020) %>%
  # remove uncontested races
  filter(dem_house_2020 != 0 & rep_house_2020 != 0)


# clean voteview data
h116 <- read_csv('data/H116_members.csv') 

h116 <- h116 %>% 
  select(state_abbv = state_abbrev, 
         district = district_code,
         party = party_code,
         bioname,
         nominate_dim1, nominate_dim2) %>%
  mutate(last_name = toupper(gsub(",.*","",bioname)),
         district = case_when(state_abbv %in% c("WY","VT","ND","SD","DE","MT") ~ "AL",
                              district < 10 ~ paste0(0,district),
                              TRUE ~ as.character(district)),
         code = sprintf("%s-%s",state_abbv,district),
         party = case_when(party == 100 ~ "Democratic",
                           party == 200 ~ "Republican",
                           last_name == "AMASH" ~ "Republican"))

# join
house <- left_join(h116, cong, by = c("state_abbv", "last_name","code")) %>%
  mutate(house_dem_beat_pres_margin = (dem_house_2020 - rep_house_2020) - (biden_2020 - trump_2020))

# remove districts with now nominate data
house <- house %>% filter(!is.na(nominate_dim1))  

# average dim 1 and 2
house <- house %>% mutate(nominate_mean = (nominate_dim1 + nominate_dim2)/2)


highlight_list <- c("OMAR","KATKO","UPTON","OCASIO-CORTEZ","MASSIE","GAETZ","LEVIN","KIND","CARTWRIGHT","GOLDEN",
  "CASE","HIGGINS","CUELLA","CORREA","CARTER","GARCIA","CRENSHAW","KATKO","BALDERSON",
  "NADLER","PHILLIPS","SMITH","BACON","VELA","DOYLE","GOSAR","PELOSI","KILDEE")
 
# for fun, look at ideology v Biden vote
ggplot(house, aes(x = biden_2020 - trump_2020, y = nominate_dim1, col = party)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(last_name %in% toupper(highlight_list)),
                  aes(label = last_name),min.segment.length = 0.01,show.legend = F) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',size=0.8,linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=1,linetype=2) +
  theme_minimal() +
  scale_color_manual(values=c("Democratic"="#3498DB","Republican"="#E74C3C")) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x="Biden margin 2020",
       y='DW-NOMINATE 1st Dimension',
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.8,0.9))

ggplot(house, aes(x = biden_2020 - trump_2020, y = nominate_dim2, col = party)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(last_name %in% toupper(highlight_list)),
                  aes(label = last_name),min.segment.length = 0.01,show.legend = F) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',size=0.8,linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=1,linetype=2) +
  theme_minimal() +
  scale_color_manual(values=c("Democratic"="#3498DB","Republican"="#E74C3C")) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x="Biden margin 2020",
       y='DW-NOMINATE 2nd Dimension',
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.8,0.9))

ggplot(house, aes(x = biden_2020 - trump_2020, y = nominate_mean, col = party)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(last_name %in% toupper(highlight_list)),
                  aes(label = last_name),min.segment.length = 0.01,show.legend = F) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',size=0.8,linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=1,linetype=2) +
  theme_minimal() +
  scale_color_manual(values=c("Democratic"="#3498DB","Republican"="#E74C3C")) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x="Biden margin 2020",
       y='DW-NOMINATE "ideology" scores\nAverage of first two dimensions',
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.8,0.9))

ggsave('house_pvi_nominate_scatter.png',width=8,height=6)


# house member margins less biden margins 2020 ----------------------------
# remove NA and stray uncontested contests
# house <- house %>% na.omit %>% filter(abs(house_dem_beat_pres_margin) < 20)

# plot
ggplot(house, aes(x = nominate_mean, y = house_dem_beat_pres_margin, col = party)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(last_name %in% toupper(highlight_list)),
                  aes(label = last_name),min.segment.length = 0.01,show.legend = F) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',size=0.8,linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=1,linetype=2) +
  theme_minimal() +
  scale_color_manual(values=c("Democratic"="#3498DB","Republican"="#E74C3C")) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x='DW-NOMINATE "ideology" scores\nAverage of first two dimensions',
       y="Democratic House margin minus Biden margin",
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.85,0.1))



# value over replacement member -------------------------------------------
win_model <- glm(I(dem_house_2020 > rep_house_2020) ~ I(biden_2020 - trump_2020), 
    house %>% na.omit,
    family = binomial(link = 'logit'))

dem_ideology_model <- lm(nominate_mean ~ I(biden_2020 - trump_2020), 
                          house %>% na.omit %>% filter(party == "Democratic"))

rep_ideology_model <- lm(nominate_mean ~ I(biden_2020 - trump_2020), 
                         house %>% na.omit %>% filter(party == "Republican"))

house$probability_dem = predict(win_model,newdata = house,type='response') 

house <- house %>%
  mutate(expected_ideology = 
           (predict(dem_ideology_model, .)*probability_dem) + 
           (predict(rep_ideology_model, .)*(1-probability_dem)) ,
         VALUE = nominate_mean - expected_ideology)


house %>%
  filter(party == "Democratic") %>%
  ggplot(., aes(x = house_dem_beat_pres_margin, y = -VALUE, col = biden_2020 - trump_2020)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(last_name %in% toupper(highlight_list)),
                  aes(label = last_name),min.segment.length = 0.01,show.legend = F) +
  theme_minimal() +
  scale_color_gradient2(high="#3498DB",low="#E74C3C",mid='#8E44AD',midpoint=0,
                        limits = c(-40,40),
                        labels = c("R+ 40+","20","0","20","D+ 40+"),
                        oob = scales::squish) +
  labs(title = "Political Value above replacement member (P-VALUE) for Democratic House Reps",
       x="Democratic House margin minus Biden margin",
       y='DW-NOMINATE "ideology" scores over replacement member\nAverage of first two dimensions\nHigher is more liberal than expected',
       col='Home district\n2020 Biden margin') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.15,0.25))


ggsave('p_value_house.png',width=8,height=6)



