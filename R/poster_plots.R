library(tidyverse)
library(directlabels)
library(tidyboot)
library(lme4)
library(lmerTest)

## TTR plot
ttrs <- read_csv("../MethodsResults/Notebook/meanttrs.csv")

#pdf("ttrs.pdf", width = 6, height = 4)
ggplot(ttrs, aes(x = tokens, y = types, color = source, label = source)) + 
  #geom_smooth() +
  ggtitle('Average Type Token Ratios') + 
  geom_point(size = .2) + 
  theme_classic(base_size = 18) + 
  theme(plot.title = element_text(size = 15)) + 
  geom_dl(method = list(dl.trans(x=x +.2), "last.qp", cex=1)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 36000))
dev.off()
ggsave('ttrs.pdf', width=6, height = 4)

## Complexity plot

complexity <- read_csv("../MethodsResults/Notebook/complexitydf.csv")

complexity_data_long <- complexity %>%
  filter(book == "False")%>%
  rename(id = X1, sent_complexity = sent_complexities) %>%
  mutate(sent_complexity = gsub("\\[", "", sent_complexity),
         sent_complexity = gsub("\\]", "", sent_complexity),
         sent_complexity = str_split(sent_complexity, ", ") %>% as.list) %>%
  unnest(sent_complexity) %>%
  mutate_at(c("age", "sent_complexity"), as.numeric) %>%
  mutate(source = factor(source, levels = c("Low SES", "High SES")))


base_model <- lmer(sent_complexity ~ 1 + (1|id), 
                         data = complexity_data_long)

age_model <- lmer(sent_complexity ~ age + (1|id), 
                   data = complexity_data_long)

ses_model <- lmer(sent_complexity~ age + source + (1|id), 
                  data = complexity_data_long)


anova(ses_model, age_model)

avg_complexity <- complexity %>%
  mutate(age = (if_else(age != 'book', (as.character(as.numeric(age) * 4 + 10)), 'book'))) %>%
  group_by(age, source) %>%
  tidyboot_mean(avg_sent_complex) %>%
  ungroup() %>%
  mutate(age = factor(age, levels = c(seq(14,58,4),'book')))


ggplot(avg_complexity, aes(x = age, y = empirical_stat,
                       ymin = ci_lower, ymax = ci_upper,
                       color = source, fill = source,
                       group = source, label = source)) +
  geom_pointrange(position = position_dodge(.5)) + 
  geom_smooth(se = F, position = position_dodge(.5)) + 
  theme_classic(base_size = 17) +
  ggtitle('Average Sentence Complexities') + 
  ylab('') +
  xlab('Age (months)') +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(size=13)) +
  #scale_x_discrete(expand = c(0, 5)) +
  expand_limits(x=15) +
  theme(legend.position = "none") +
  geom_dl(method = list(dl.trans(x=x +.2), "last.points", cex=1)) 
ggsave('sentcomplex.pdf', width=6, height = 4)

