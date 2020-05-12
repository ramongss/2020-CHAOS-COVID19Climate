setwd(ResultsDir)
require(scales)

error <- read.csv('error_test_SDA.csv')
error$Error <- abs(error$Error)
error$State <- factor(error$State, levels = c('AM', 'CE', 'PE', 'RJ', 'SP',
                                              'CA', 'IL', 'MA', 'NJ', 'NY'))

plot <- error %>% 
  ggplot(aes(x = Model)) +
  geom_boxplot(aes(y = Error, fill = Country) ) +
  facet_wrap(~State, scales = 'free', ncol = 5) +
  ylab('Absolute error') +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1, size = 8),
    text = element_text(family = 'CM Roman', size = 15),
    # axis.text.y = element_text(angle = 20),
    legend.position = 'bottom'
  ) +
  scale_fill_manual(values = c('#009c3b','#3C3B6E')) +
  scale_y_continuous(labels = comma)

# # remove outliers
# stat <- tapply(error$Error,list(error$Model,error$State),function(x) boxplot.stats(x))
# stats <- unlist(tapply(error$Error,list(error$Model,error$State),function(x) boxplot.stats(x)$stats))
# 
# df <- data.frame(
#   Country = rep(c('BRA', 'USA'), each=250),
#   Model = rep(rep(unlist(dimnames(stat)[1]),each=5),length(unlist(dimnames(stat)[2]))),
#   State = rep(unlist(dimnames(stat)[2]),each=50),
#   Error = stats
# )
# 
# df$State <- factor(df$State, levels = c('AM', 'CE', 'PE', 'RJ', 'SP',
#                                         'CA', 'IL', 'MA', 'NJ', 'NY'))
# 
# no_outliers <- df %>% 
#   ggplot(aes(x = Model)) +
#   geom_boxplot(aes(y = Error, fill = Country) ) +
#   facet_wrap(~State, scales = 'free', ncol = 5) +
#   ylab('Absolute error') +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust=1, size = 8),
#     # axis.text.x.top = element_text(angle = 90),
#     text = element_text(family = 'CM Roman', size = 15)
#   ) +
#   scale_fill_manual(values = c('#009c3b','#3C3B6E'))


setwd(FiguresDir)

plot %>% 
  ggsave(
    filename = "error.pdf",
    device = 'pdf',
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
  ) 



