## ----global-options, include=FALSE-------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)


## ----pkgs, warning = F, message = F, include = F----
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(knitr)
library(kableExtra)


## ----data, warning = F, message = F, include = F, cache = T----
options(dplyr.summarise.inform = FALSE)
Playoffs <- readRDS(file = "../data_2/playoffs_sims.rds")
all_1819 <- readRDS(file = "../data_2/all_1819.rds")
#qbr_sims <- readRDS(file = "../data/QBR_sims.rds")
plays <- readRDS(file = "../data_2/pbp-data-1819.rds")
qbr_thirds_sims <- readRDS(file = "../data_2/QBR_thirds_sims.rds")
fourths_sims <- readRDS(file = "../data_2/fourth_down_sims.rds")
fourths_sims$Scenario <- as.factor(fourths_sims$Scenario)
yds_less_than <- readRDS(file = "../data_2/yds_less_than_sims.rds")
pass_run <- plays %>% 
  dplyr::filter(., play_type %in% c("run", "pass"), down %in% 1:3) %>% 
  dplyr::mutate(., year = lubridate::year(game_date)) %>% 
  dplyr::group_by(., year, play_type) %>%
  dplyr::summarize(., n = n()) %>% 
  dplyr::mutate(prop = n / sum(n))
# Group by drives and caculate percent score and average pts per drive
scores <- Playoffs %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(., score = ifelse(points > 0, 1, 0),
                score_type = ifelse(points == 3, "FG",
                                    ifelse(points == 7, "TD", "NoScore"))) %>% 
  dplyr::group_by(., teams, proportion, score_type) %>%
  dplyr::summarize(., score_ct = n()) %>% 
  dplyr::group_by(., teams, proportion) %>%
  dplyr::mutate(p = score_ct/sum(score_ct)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., upper = p + 2*sqrt(p*(1 - p)/10000),
                lower = p - 2*sqrt(p*(1 - p)/10000),
                year = ifelse(grepl("2018", teams), "2018", "2019"),
                playoffs = ifelse(grepl("Non", teams), "No", "Yes"))

all_scores <- all_1819  %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(., score = ifelse(points > 0, 1, 0)) %>% 
  dplyr::group_by(., year, proportion) %>%
  dplyr::summarize(., p = sum(score)/n(),
                   score_avg = sum(points)/n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., upper = p + 2*sqrt(p*(1 - p)/10000),
                lower = p - 2*sqrt(p*(1 - p)/10000))
all_scores <- all_scores %>% 
  dplyr::mutate(full_year = if_else(year == 18,2018,2019))
all_scores_by_type <- all_1819  %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(., score = ifelse(points > 0, 1, 0),
                full_year = ifelse(year == 18, 2018, 2019),
                score_type = ifelse(points == 3, "FG",
                                    ifelse(points == 7, "TD", "NoScore"))) %>% 
  dplyr::group_by(., full_year, proportion, score_type) %>%
  dplyr::summarize(., score_ct = n()) %>% 
  dplyr::group_by(., full_year, proportion) %>%
  dplyr::mutate(p = score_ct/sum(score_ct)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., upper = p + 2*sqrt(p*(1 - p)/10000),
                lower = p - 2*sqrt(p*(1 - p)/10000))

qbr_scores <- qbr_thirds_sims %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(., score = ifelse(points > 0, 1, 0),
                full_year = ifelse(year == 18, 2018, 2019),
                qbr_short = ifelse(grepl("High", RTG), "High",
                                   ifelse(grepl("Low", RTG), "Low", "Mid")),
                score_type = ifelse(points == 3, "FG",
                                    ifelse(points == 7, "TD", "NoScore"))) %>% 
  dplyr::group_by(., full_year, proportion, qbr_short, score_type) %>%
  dplyr::summarize(., score_ct = n()) %>% 
  dplyr::group_by(., full_year, proportion, qbr_short) %>%
  dplyr::mutate(., p = score_ct / sum(score_ct)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(., upper = p + 2*sqrt(p*(1 - p)/10000),
                lower = p - 2*sqrt(p*(1 - p)/10000))


## ---- eval = F---------------------------
## ## From CRAN
## install.packages("NFLSimulatoR")
## ## From Github
## install.packages("remotes")
## remotes::install_github("rtelmore/NFLSimulatoR")


## ----fourth-down-perc-score, fig.width = 4.75, fig.height=2.75, fig.cap = "\\label{fig:fourth-down-perc-score}The percentage of simulated drives that resulted in no score (green), a field goal (orange), or a touchdown (purple) in 2018 and 2019, for the fourth-down sub-strategies", echo = F----

# scores <- fourths_sims %>% 
#   dplyr::filter(., end_drive == TRUE) %>%
#   dplyr::mutate(score = ifelse(points > 0, 1, 0)) %>% 
#   dplyr::group_by(.,Scenario) %>%
#   dplyr::summarize(.,score_pct = sum(score)/n(),
#                    score_avg = sum(points)/n(),.groups = "keep")
pct_type <- fourths_sims %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(score = ifelse(points > 0, 1, 0),
                score_type = ifelse(points == 3,"FG",ifelse(points == 7,"TD","NoScore"))) %>%
  dplyr::group_by(.,Scenario,score_type) %>%
  dplyr::summarize(.,score_ct = n(),.groups = "keep") %>% 
  dplyr::group_by(.,Scenario) %>%
  dplyr::mutate(score_pct = score_ct/sum(score_ct),.groups = "keep")

# Percent Score Figure
p_score <- ggplot(data = subset(pct_type, score_type %in% c("FG", "TD")),
                  aes(x = Scenario, y = score_pct,fill = score_type)) +
  geom_bar(stat = 'identity', position = "stack") +  
 ylab("Percent Score") +
  scale_x_discrete(breaks = c("always_go_for_it","empirical","exp_pts",
                            "never_go_for_it","yds_less_than_5"),
                 labels = c("Always\n go for it", "Empirical", "Expected\n points",
                            "Never\n go for it","Yards smaller\n than 5")) + 
  scale_fill_brewer("Score Type",
                      breaks = c("FG","TD"),
                      labels = c("FG","TD"),
                    palette = "Dark2") +
  theme_bw() +
  theme(legend.text = element_text(size = 8),
        legend.title = element_blank())

p_score <-  fourths_sims %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  count(Scenario,points) %>% 
  mutate(pct = n/10000) %>% 
  mutate(points = factor(points)) %>%
  ggplot(aes(x = factor(Scenario,levels = c("always_go_for_it","empirical","yds_less_than_5","exp_pts",
                            "never_go_for_it")), y = pct,fill = points),
         show.legend = F) +
  geom_col(position = "stack",show.legend = F) +
  ylab("Percent of Drives") + 
  xlab(NULL) +
scale_x_discrete(breaks = c("always_go_for_it","empirical","exp_pts",
                            "never_go_for_it","yds_less_than_5"),
                 labels = c("Always\n go for it", "Empirical", "Expected\n points",
                            "Never\n go for it","Yards smaller\n than 5")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0.0, 1, by = .1),
                     #labels = scales::percent,
                     labels = percent_format(accuracy = 1)) +
    scale_fill_brewer("Score Type",
                      breaks = c("0","3","7"),
                      labels = c("0","FG","TD"),
                    palette = "Dark2") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.75,"line"),
        plot.title = element_text(size = 10))
p_score


## ----table1, full_width = FALSE, echo=F----
fourths_table <- fourths_sims %>% 
  mutate(Scenario = recode(Scenario,
                               "always_go_for_it" = "Always go for it",
                               "empirical" = "Empirical",
                               "exp_pts" = "Expected points",
                               "never_go_for_it" = "Never go for it",
                               "yds_less_than_5" = "Yards less than 5")) %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(score = ifelse(points > 0, 1, 0)) %>% 
  dplyr::group_by(.,Scenario) %>%
  dplyr::summarize(.,
                   #len = n(),
                   pct_fg = paste0(as.character(round(100*sum(is_field_goal)/10000),2),"%"),
                   pct_td = paste0(as.character(round(100*sum(is_td_offense)/10000),2),"%"),
                   mean_score = round(mean(points),2),
                   lower_ci = round(mean(points) - (2*sd(points)/sqrt(n())),2),
                   upper_ci = round(mean(points) + (2*sd(points)/sqrt(n())),2),
                   .groups = "keep") %>% 
  ungroup() %>% 
  rename("Sub-strategy" = "Scenario") %>% 
  rename("Mean Score" = "mean_score",
         "% of Drives Ending in FG" = "pct_fg",
         "% of Drives Ending in TD" = "pct_td",
         "Lower 95% CI for Score" = "lower_ci",
         "Upper 95% CI for Score" = "upper_ci") 
fourths_table %>%
  knitr::kable(booktabs = T,escape = F,format = "markdown",caption ="\\label{tab:tab1} Description of 10000 simulations (2018 and 2019 data) for each fourth down sub-strategy") %>%
  row_spec(0, align = "r")


## ----yds-less-than, fig.width = 5, fig.height=3, fig.cap = "\\label{fig:yds-less-than} 2.1: The percentage of simulated drives that resulted in a field goal (green) ora touchdown (orange); 2.2: Average score per drive for the *yardage less than Y yards* sub-strategy as a function of $Y$; 2.3: Average turnover yardline resulting from the *yardage less than Y yards* sub-strategy as a function of $Y$", echo = F,warning = FALSE,message=FALSE----

yds_strats <- yds_less_than %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(score = ifelse(points > 0, 1, 0),
                score_type = ifelse(points == 3,"FG",ifelse(points == 7,"TD","NoScore"))) %>%
  dplyr::group_by(.,Scenario,score_type) %>%
  dplyr::summarize(.,score_ct = n(),.groups = "keep") %>% 
  dplyr::group_by(.,Scenario) %>%
  dplyr::mutate(score_pct = score_ct/sum(score_ct))

yds_strats_ci <- yds_less_than %>% 
  dplyr::filter(., end_drive == TRUE) %>%
  dplyr::mutate(score = ifelse(points > 0, 1, 0)) %>% 
  dplyr::group_by(.,Scenario) %>%
  dplyr::summarize(.,len = n(),
                   mean_score = mean(points),
                   lower_ci = mean(points) - (2*sd(points)/sqrt(n())),
                   upper_ci = mean(points) + (2*sd(points)/sqrt(n())),
                   .groups = "keep")
yds_strats_ci$Scenario <- as.numeric(substr(yds_strats_ci$Scenario,15,nchar(yds_strats_ci$Scenario)-2))
yds_strats$Scenario <- as.numeric(substr(yds_strats$Scenario,15,nchar(yds_strats$Scenario)-2))
# Percent Score Figure
yds_score <- ggplot(data = subset(yds_strats,score_type %in% c("FG", "TD")),
                  aes(x = reorder(Scenario,Scenario), y = score_pct,fill = score_type)) +
  geom_bar(stat = 'identity', position = "stack") +  
  scale_fill_brewer("Score Type",
                      breaks = c("FG","TD"),
                      labels = c("FG","TD"),
                    palette = "Dark2") +
  xlab("Yards less than") + 
  ylab("Percent Score") +
  scale_y_continuous(limits = c(0, .4), breaks = seq(0.0, .4, by = .1),
                     #labels = scales::percent,
                     labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.75,"line"),
        plot.title = element_text(size = 10))


yds_perc <- ggplot(data = yds_strats_ci,
       aes(x = reorder(Scenario,Scenario), y = mean_score),show.legend = F) +
  geom_point(position = position_dodge(width = .05),show.legend = F) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(.05),
                show.legend = F) +
  ylab("Average Score") +
  xlab("Yards less than") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 10))+
  theme_bw()

no_scores_ci <- yds_less_than %>% 
  dplyr::filter(., points == 0, end_drive == TRUE) %>% 
  dplyr::group_by(.,Scenario) %>%
  dplyr::summarize(.,len = n(),
                   mean_yds = mean(new_yfog),
                   lower_ci = mean(new_yfog) - (2*sd(new_yfog)/sqrt(n())),
                   upper_ci = mean(new_yfog) + (2*sd(new_yfog)/sqrt(n())),
                   .groups = "keep")
#substring extract number
no_scores_ci$Scenario <-as.numeric(substr(no_scores_ci$Scenario,15,nchar(no_scores_ci$Scenario)-2))

turn_yds <- ggplot(data = no_scores_ci,
       aes(x = reorder(Scenario,Scenario), y = mean_yds),show.legend = F) +
  geom_point(position = position_dodge(width = .05),show.legend = F) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(.05),
                show.legend = F) +
  ylab("Average\n Turnover Yardline") +
  xlab("Yards less than") +
  guides(fill = F) +
  theme(plot.title = element_text(size = 10))+
  theme_bw()

plot_grid(plot_grid(NULL,yds_score,NULL,ncol=3,
                    rel_widths=c(0.2,0.7,0.1),labels =c(NULL,"2.1",NULL),label_size = 10,hjust = -3),
          plot_grid(yds_perc,turn_yds,ncol =2,labels = c("2.2","2.3"),label_size = 10,vjust = 0.2,hjust = -2.5),
          ncol = 1)


## ----pass-rush-all-facet, fig.width = 4, fig.height=3, fig.cap = "\\label{fig:pass-rush-all-facet}The percentage of simulated drives that resulted in a score (touchdown or field goal) in 2018 and 2019. The dashed line represents the actual proportion of passing plays on first, second, and third downs in both years.", echo = F----
p <- ggplot(data = all_scores,
            aes(x = proportion, y = p))
p + geom_point(alpha = .75, size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = .75) +
  facet_grid(rows = "full_year") +
  geom_smooth(color = "black",method = "loess") +
  geom_vline(aes(xintercept = .59), col = "grey50", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  scale_y_continuous(limits = c(.3, .6), breaks = seq(.35, .6, by = .1),
                     #labels = scales::percent,
                     labels = percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Proportion Pass", y = "Percent Score") +
  guides(fill = F) +
  theme_bw()


## ---- pass-rush-by-type, fig.width = 4.2, fig.height=3, fig.cap = "\\label{fig:pass-rush-by-type}The percentage of simulated drives that resulted in either a touchdown (orange) or a field goal (green) in 2018 and 2019. The dashed line represents the actual proportion of passing plays on first, second, and third downs in both years.", echo = F----
p <- ggplot(data = subset(all_scores_by_type, 
                          score_type %in% c("FG", "TD")),
            aes(x = proportion, y = p, col = score_type, group = score_type))
# suppressWarnings(p + geom_point(position = position_dodge(width = .05)) +
p + geom_point(position = position_dodge(width = .05), alpha = .5, size = 1) +
#  geom_smooth(aes(col = score_type),method = "loess") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = .05), alpha = .5) +
  facet_grid(rows = "full_year") +
  geom_vline(aes(xintercept = .59), col = "grey50", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  scale_y_continuous(limits = c(0.05, .5), breaks = seq(0, .5, by = .1),
                     #labels = scales::percent,
                     labels = percent_format(accuracy = 1)) +
  scale_color_brewer("type", palette = "Dark2") +
  labs(x = "Proportion Pass", y = "Percent Score") +
  guides(col = F) +
  theme_bw() +
  theme(legend.title = element_blank())


## ----pass-rush-facet, fig.width = 4.25, fig.height=3.25, fig.cap = "\\label{fig:pass-rush-facet} The percentage of simulated drives that resulted in a score by type (touchdown or field goal) in 2018 and 2019 colored by playoff teams (orange) versus non-playoff teams (purple).", echo = F----
p <- ggplot(data = subset(scores, score_type %in% c("FG", "TD")),
            aes(x = proportion, y = p, col = playoffs))
p + geom_point(position = position_dodge(width = .05), alpha = .5, size = 1) +
#  geom_smooth(aes(col = playoffs),method = "loess",show.legend = T) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.05),
                alpha = .5) +
  facet_grid(year ~ score_type) +
  geom_vline(aes(xintercept = .59), col = "grey50", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  scale_y_continuous(#labels = scales::percent,
                      labels = percent_format(accuracy = 1)) +
  #scale_color_brewer(palette = "Set1") +
  scale_colour_manual(values = c("No" = "#984EA3" ,"Yes"="#FF7F00"))+
  labs(x = "Proportion Pass", y = "Percent Score") +
  guides(col = F) +
  theme_bw()


## ----pass-rush-qbr, fig.width = 4, fig.height=3, fig.cap = "\\label{fig:pass-rush-qbr}The percentage of simulated drives that resulted in a score by type (touchdown or field goal) in 2018 and 2019 colored by overall team passer rating classification: High (green), Medium (purple), and Low (orange).", echo = F----
p <- ggplot(data = subset(qbr_scores, score_type %in% c("FG", "TD")),
                  aes(x = proportion, y = p, color = qbr_short)) 
p + geom_point(position = position_dodge(width = .05), alpha = .5, size = 1) +
#  geom_smooth(aes(col = qbr_short),method = "loess") +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.05), 
                alpha = .5) +
  facet_grid(full_year ~ score_type) + 
  geom_vline(aes(xintercept = .59), col = "grey50", linetype = "dashed") +
  scale_color_brewer("RTG", palette = "Dark2") +
  labs(x = "Proportion Pass", y = "Percent Score") +
  scale_x_continuous(breaks = c(seq(0, 1, by = .2))) + 
  scale_y_continuous(breaks = seq(0.0, .5, by = .1), limits = c(0.05, .5),
                     #labels = scales::percent,
                     labels = percent_format(accuracy = 1)) + 
  guides(col = F) +
  theme_bw()


