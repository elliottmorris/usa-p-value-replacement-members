library(tidyverse)
library(ggrepel)
library(blme)
library(caret)
library(caretEnsemble)

source("calculate_optimal_pvi.R")

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

# then residualize by year (shrunk toward 0)
senate <- senate %>%
  group_by(year) %>%
  mutate(actual_v_prediction_resid = actual_v_prediction - (median(actual_v_prediction)/2)) %>%
  ungroup()

# and by region over time (shrunk toward 0)
senate <- senate %>%
  left_join(read_csv('data/state_region_crosswalk.csv') %>% 
              select(state_abbv,region = region_computed))

senate <- senate %>%
  group_by(region) %>%
  mutate(actual_v_prediction_resid = actual_v_prediction_resid - (median(actual_v_prediction)/2)) %>%
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
         last_elec, region) %>% 
  arrange(desc(winner_v_expectations_residualized)) 

senators %>% filter(last_elec == 1) %>% head(10)


# policy value over replacement -------------------------------------------
# read in the voteview nominate numbers
voteview <- read_csv('data/Sall_members.csv') %>% 
  filter(congress >= 94, 
         state_abbrev != "USA",
         !is.na(nominate_number_of_votes)) 

# join senate reuslts with next-session scores, by last name
voteview <- voteview %>% 
  mutate(year = congress*2 + 1786,
         last_name = toupper(gsub(",.*","",bioname))) %>%
  mutate(last_name = gsub("LUJÁN","LUJAN",last_name),
         last_name = gsub("ROCKEFELLER","ROCKEFELLER IV",last_name))

# do the join
senators_vv <- senators %>%
  left_join(voteview %>% select(year, incumbent=last_name, 
                                nokken_poole_dim1, nominate_dim1, nominate_number_of_votes), 
            by = c("year","incumbent")) %>%
  ungroup()

# for senators without a lot of votes, puch ideology scores back to party average
senators_vv <- senators_vv %>% 
  group_by(incumbent,party,state=substr(seat,1,2)) %>%
  mutate(lifetime_votes = sum(nominate_number_of_votes, na.rm=T)) %>%
  mutate(ideology_weight = pmin(1, lifetime_votes / 200)) %>%
  group_by(party,year) %>%
  mutate(nokken_poole_dim1 = (nokken_poole_dim1*ideology_weight) + 
           (mean(nokken_poole_dim1, na.rm=T)*(1-ideology_weight)),
         nominate_dim1 = (nominate_dim1*ideology_weight) + 
           (mean(nominate_dim1, na.rm=T)*(1-ideology_weight))) %>%
  select(-c(state,lifetime_votes)) %>%
  filter(!is.na(nominate_dim1)) %>%
  ungroup()

# plot ideology by pvi ----------------------------------------------------
highlight_list <- c('warren', 'booker', 'manchin', 'sinema', 'tester', 'capito', 
                    'romney', 'collins', 'lee', 'sanders', 'ossoff', 'gillibrand', 
                    'schatz', 'murkowski','baldwin','leahy','cruz','blackburn',
                    'scott','hawley','cornyn','young','coons','murphy','warnock',
                    'klobuchar','shaheen','hassan','brown')

senators_vv %>%
  filter(last_elec == 1) %>%
  ggplot(., aes(x = pvi, y = nominate_dim1, col = party)) +
  geom_point(alpha=0.5,show.legend = F) +
  geom_text_repel(data = . %>% filter(incumbent %in% toupper(highlight_list)),
                  aes(label = incumbent),show.legend = F,min.segment.length = 0.1) +
  scale_color_manual(values=c("D"="#3498DB","R"="#E74C3C")) +
  scale_x_continuous(breaks = seq(-0.5,0.5,00.1), 
                     labels = function(x){round(x*100)}) +
  geom_smooth(method='lm',se=F,aes(group=1),col='black',linetype=2) +
  geom_smooth(method='lm',se=F,alpha=0.5,size=0.8,linetype=2) +
  labs(x="Home state's federal partisan voting index*",
       y='DW-NOMINATE "ideology" scores',
       caption="*At the time of the senator's most recent election",
       col='Party') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.8,0.8))

ggsave("pvi_nominate_scatter.png",width=8,height=6)


# run the models for vote margin and ideology -----------------------------
# pick the ideology measure we'll use from hereon down
senators_vv <- senators_vv %>% mutate(ideology_score = nominate_dim1)

# first, model D and R win rates by geography and time
dem_win_model <- bglmer(I(party == "D")  ~ pvi + region +
                          (1 + pvi | region) + 
                          pvi*I(year/1976) + pvi*I(year/1976)^2,
                        data = senators_vv, 
                        family = binomial(link = 'logit'))

# then, model ideology conditional on party
ideology_model <- lm(ideology_score ~ pvi + pvi*region + pvi*party +
                       pvi*I(year/1976) + pvi*I(year/1976)^2,
                     data = senators_vv)

# center, scale and perform a YeoJohnson transformation
# identify and remove variables with near zero variance
# perform pca
senators_train <- senators_vv %>% select(ideology_score, pvi, party, year)
senators_train <- model.matrix(ideology_score ~ ., data = senators_train)  %>%
  as.data.frame %>% bind_cols(select(senators_train,ideology_score))

# specify that the resampling method is 
fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  verbose = TRUE,
  savePredictions="final",
  index = createResample(senators_train$ideology_score, 10))

# fit a ranger model
ranger_fit <- train(ideology_score ~ .*.,
                  data = senators_train,
                  method = "ranger",
                  preProcess = c('center','scale','nzv'),
                  trControl = fit_control,
                  tuneGrid = expand.grid(mtry = seq(1,dim(senators_train)[2]-1,2), min.node.size=5,
                                         splitrule=c('extratrees','variance')))

# boosted linear regression
linear_fit <- train(ideology_score ~ .*.,
                 data = senators_train,
                 method = "BstLm",
                 preProcess = c('center','scale','nzv'),
                 trControl = fit_control,
                 tuneLength = 20)

# fit a neural net 
nnet_fit <- train(ideology_score ~ .*.,
                 data = senators_train,
                 method = "neuralnet",
                 preProcess = c('center','scale','nzv'),
                 trControl = fit_control)

# dimensionality reduction
pcr_fit <- train(ideology_score ~ .*.,
                  data = senators_train,
                  method = "pcr",
                  preProcess = c('center','scale','nzv'),
                  trControl = fit_control,
                 tuneLength = 10)

# stepwise aic
aic_fit <- train(ideology_score ~ .*.,
             data = senators_train,
             method = "lmStepAIC",
             preProcess = c('center','scale','nzv'),
             trControl = fit_control,
             tuneLength = 20)

# glmnet
glmnet_fit <- train(ideology_score ~ .*.,
                 data = senators_train,
                 method = "glmnet",
                 preProcess = c('center','scale','nzv'),
                 trControl = fit_control,
                 tuneLength = 20)

# ensemble model, a boosted glm
caret_ensemble <- caretStack(
  all.models = c(ranger_fit, linear_fit, nnet_fit, 
                 pcr_fit, aic_fit, glmnet_fit),
  method="glmboost",
  metric="RMSE",
  trControl=trainControl(
    method="cv",
    number=10,
    savePredictions="final",
  ),
  tuneLength = 20
)

# check predictions v fitted
tibble(yhat = predict(caret_ensemble, senators_train), y=senators_train$ideology_score) %>%
  ggplot(., aes(x=yhat, y=y)) +
  geom_point() +
  geom_abline()


# generate ideology above replacement and make a graph --------------------


senators_vv$expected_dem_win_rate = predict(dem_win_model, newdata=senators_vv, type='response')

senators_vv$expected_dem_ideology = predict(linear_fit,
                                            newdata=senators_train %>% mutate(partyR = 0),
                                            type='raw')
senators_vv$expected_rep_ideology = predict(linear_fit,
                                            newdata=senators_train %>% mutate(partyR = 1),
                                            type='raw')

# senators_vv$expected_dem_ideology = predict(ideology_model, 
#                                             newdata=senators_vv %>% mutate(party = "D"), 
#                                             type='response')
# senators_vv$expected_rep_ideology = predict(ideology_model, 
#                                             newdata=senators_vv %>% mutate(party = "R"), 
#                                             type='response')

senators_vv$expected_senator_ideology = 
  (senators_vv$expected_dem_win_rate * senators_vv$expected_dem_ideology) + 
  ((1 - senators_vv$expected_dem_win_rate) * senators_vv$expected_rep_ideology) 


# look at expected v senator values
ggplot(senators_vv, aes(expected_senator_ideology, ideology_score, col = party)) +
  geom_point() + 
  geom_abline()  +
  labs(subtitle='actual v expeted nominate scores for the average d or r rep in a given state')


senators_vv <- senators_vv %>%
  mutate(ideology_over_replacement_senator = ideology_score - expected_senator_ideology) %>%
  arrange(desc(ideology_over_replacement_senator))

# most valuable dems
senators_vv %>%
  filter(last_elec == 1, party == "D") %>%
  group_by(incumbent,pvi=pvi*100) %>%
  summarize(average_VARS = mean(ideology_over_replacement_senator),
            winner_v_expectations_residualized) %>%
  arrange(average_VARS) %>%
  ggplot(., aes(x=winner_v_expectations_residualized, y=-average_VARS,
                col=pvi)) +
  scale_color_gradient2(high="#3498DB",low="#E74C3C",mid='#6D6D6D',midpoint=0) +
  geom_point(alpha = 0.5) +
  geom_text_repel(data = . %>% filter(incumbent %in% toupper(highlight_list)),
                  aes(label = incumbent),min.segment.length = 0.01) +
  labs(x = 'Most recent vote margin over expectations',
       y = 'DW-NOMINATE score for ideology over replacement senator',
       title = "Value Above Replacement Senators (VARS) for Democrats",
       col='Home state \npartisan lean*',
       caption="*At the time of the senator's most recent election") +
  scale_x_continuous(breaks = seq(-0.5,0.5,00.1), 
                     labels = function(x){round(x*100)}) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=1),
        legend.position = c(0.85,0.4))

ggsave("VARS.png",width=8,height=6)
