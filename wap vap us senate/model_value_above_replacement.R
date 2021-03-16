library(tidyverse)
library(ggrepel)
library(lme4)

# an improvement upon https://fivethirtyeight.com/features/bayh-like-other-moderates-was-valuable/
# using geographic and other ancillary information to improve ideology estimates


# electoral value over replacement ----------------------------------------
# read data
senate <- read_csv('output/senate_results_with_pvi.csv') %>%
  mutate(dem_last_name = gsub("MASTO","CORTEZ MASTO",dem_last_name))

# vote_model to predict dem share
vote_model <- lm(dem_margin ~ 0 + pvi, data = senate)
summary(vote_model)

# predict dem margin in each state and calculate overperformance
senate <- senate %>%
  mutate(pred_dem_margin = predict(vote_model, .),
         actual_dem_margin = dem_margin,
         actual_v_prediction = actual_dem_margin - pred_dem_margin ) %>%
  arrange(desc(actual_v_prediction))

# then residualize by year
senate <- senate %>%
  group_by(year) %>%
  mutate(actual_v_prediction_resid = actual_v_prediction - median(actual_v_prediction)) %>%
  ungroup()

# and by region over time
senate <- senate %>%
  left_join(read_csv('data/state_region_crosswalk.csv') %>% 
              select(state_abbv,region = region_computed))

senate <- senate %>%
  group_by(region) %>%
  mutate(actual_v_prediction_resid = actual_v_prediction - median(actual_v_prediction)) %>%
  ungroup()

# now, view
senators <- senate %>%
  arrange(desc(year)) %>%
  group_by(district_id)  %>%
  mutate(last_elec = row_number()) %>%
  # code in winner direction
  mutate(incumbent = ifelse(winning_party == 'D', dem_last_name, rep_last_name),
         winner_v_expectations_residualized = ifelse(winning_party == 'D', actual_v_prediction_resid, -1*actual_v_prediction_resid),
         winner_expected = ifelse(winning_party == 'D', pred_dem_margin, -1*pred_dem_margin),
         winner_actual = ifelse(winning_party == 'D', actual_dem_margin, -1*actual_dem_margin)) %>%
  select(seat = district_id, 
         incumbent, 
         party = winning_party,
         pvi,
         winner_v_expectations_residualized,
         winner_expected, winner_actual,
         year,
         last_elec) %>% 
  arrange(desc(winner_v_expectations_residualized)) 

senators %>% filter(last_elec == 1) %>% head(10)


# policy value over replacement -------------------------------------------
# read in the voteview nominate numbers
voteview <- read_csv('data/Sall_members.csv') %>% 
  filter(congress >= 94)

# join senate reuslts with next-session scores, by last name
voteview <- voteview %>% 
  mutate(year = congress*2 + 1786,
         last_name = toupper(gsub(",.*","",bioname))) %>%
  mutate(last_name = gsub("LUJÁN","LUJAN",last_name),
         last_name = gsub("ROCKEFELLER","ROCKEFELLER IV",last_name))

# do the join
senators_vv <- senators %>%
  left_join(voteview %>% select(year, incumbent=last_name, 
                                nokken_poole_dim1, nominate_dim1), 
            by = c("year","incumbent"))



# plot ideology by pvi ----------------------------------------------------

highlight_list <- c('warren', 'booker', 'manchin', 'sinema', 'tester', 'capito', 
                    'romney', 'collins', 'lee', 'sanders', 'ossoff', 'gillibrand', 
                    'schatz', 'murkowski','baldwin','leahy','cruz','blackburn',
                    'scott','hawley','cornyn','young','coons','murphy')

senators_vv %>%
  filter(last_elec == 1) %>%
  ggplot(., aes(x = pvi, y = nominate_dim1, col = party)) +
  geom_point(data = . %>% filter(!incumbent %in% toupper(highlight_list)), 
             alpha=0.5,show.legend = F) +
  geom_text_repel(data = . %>% filter(incumbent %in% toupper(highlight_list)),
            aes(label = incumbent),show.legend = F) +
  scale_color_manual(values=c("D"="#3498DB","R"="#E74C3C")) +
  scale_x_continuous(breaks = seq(-0.5,0.5,00.1), 
                     labels = function(x){round(x*100)}) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x="Home state's federal partisan voting index*",
       y='DW-NOMINATE "ideology" scores',
       caption="*At the senator's most recent election",
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.8,0.8))



# calculate estimated ideology --------------------------------------------
# first, model D and R win rates by geography
dem_win_model <- glmer(I(party == "D" ~ pvi + (1 | )))

# then, model ideology conditional on winning
dem_ideology_model <- lm(nominate_dim1 ~ pvi, 
                         data = senators_vv %>% filter(party == 'D'))
rep_ideology_model <- lm(nominate_dim1 ~ pvi, 
                         data = senators_vv %>% filter(party == 'R'))


