library(reshape2)

# Single

importance <- readxl::read_excel('importance.xlsx', sheet = 1)

mean_bra <- importance %>%
  filter(State %in% c('AM','CE', 'PE', 'RJ', 'SP')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(mean(.,na.rm = TRUE)))

mean_usa <- importance %>%
  filter(State %in% c('CA','IL','MA','NJ','NY')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(mean(.,na.rm = TRUE)))

sd_bra <- importance %>%
  filter(State %in% c('AM','CE', 'PE', 'RJ', 'SP')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(sd(.,na.rm = TRUE)))

sd_usa <- importance %>%
  filter(State %in% c('CA','IL','MA','NJ','NY')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(sd(.,na.rm = TRUE)))

mean_bra <- mean_bra %>% 
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

sd_bra <- sd_bra %>% 
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

brazil_single <- data.frame(
  Variable = mean_bra$Variable,
  Model = mean_bra$variable,
  Mean = mean_bra$value,
  SD = sd_bra$value
)

mean_usa <- mean_usa %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')
  
sd_usa <- sd_usa %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

usa_single <- data.frame(
  Variable = mean_usa$Variable,
  Model = mean_usa$variable,
  Mean = mean_usa$value,
  SD = sd_usa$value
)


# VMD
importance <- readxl::read_excel('importance.xlsx', sheet = 2)

mean_bra <- importance %>%
  filter(State %in% c('AM','CE', 'PE', 'RJ', 'SP')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(mean(.,na.rm = TRUE)))

mean_usa <- importance %>%
  filter(State %in% c('CA','IL','MA','NJ','NY')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(mean(.,na.rm = TRUE)))

sd_bra <- importance %>%
  filter(State %in% c('AM','CE', 'PE', 'RJ', 'SP')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(sd(.,na.rm = TRUE)))

sd_usa <- importance %>%
  filter(State %in% c('CA','IL','MA','NJ','NY')) %>% 
  group_by(Variable) %>%
  summarise_at(vars(-State), funs(sd(.,na.rm = TRUE)))

mean_bra <- mean_bra[,-c(4,9,14,19,24)] %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

sd_bra <- sd_bra[,-c(4,9,14,19,24)] %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

brazil_vmd <- data.frame(
  Variable = mean_bra$Variable,
  Model = mean_bra$variable,
  Mean = mean_bra$value,
  SD = sd_bra$value
)

mean_usa <- mean_usa[,-c(4,9,14,19,24)] %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

sd_usa <- sd_usa[,-c(4,9,14,19,24)] %>%
  melt(id.vars = 1) %>% 
  filter(variable != 'cubist')

usa_vmd <- data.frame(
  Variable = mean_usa$Variable,
  Model = mean_usa$variable,
  Mean = mean_usa$value,
  SD = sd_usa$value
)

# merge

brazil <- data.frame(
  Country = rep('BRA'),
  rbind(brazil_single, brazil_vmd) %>% 
  group_by(Variable) %>% 
  summarise_at(vars(-Model), funs(mean(., na.rm = TRUE)))
)

usa <- data.frame(
  Country = rep('USA'),
  rbind(usa_single, usa_vmd) %>% 
    group_by(Variable) %>% 
    summarise_at(vars(-Model), funs(mean(., na.rm = TRUE)))
)

importance_final <- rbind(brazil, usa) 


### plot

library(extrafont)

plot <- importance_final %>% 
  ggplot(aes(x = Variable, y = Mean, fill = Country)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           color = 'black') +
  geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD), width=.2,
                position=position_dodge(.9)) +
  ylab('Variable Importance') +
  scale_fill_manual(values = c('#009c3b','#3C3B6E')) +
  scale_x_discrete(labels = c(expression(Lag[1]),expression(Lag[2]),
                              'Precipitation',
                              'Max. Temperature', 'Min. Temperature')) +
  theme_bw() +
  theme(text = element_text(family = "CM Roman", size = 12))

plot %>% 
  ggsave(
    filename = "importance.pdf",
    device = 'pdf',
    width = 8,
    height = 4.5,
    units = "in",
    dpi = 300
  ) 




















