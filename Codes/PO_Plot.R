## BRA ----
# AM
AM <- PREDICTION$bra_vmd_AM
AM <- data.frame(
  'State'    = rep('AM'),
  'Observed' = AM[[1]][,1],
  'ODA-VMD-BRNN' = AM[[1]][,5],
  'TDA-VMD-BRNN' = AM[[2]][,5],
  'SDA-VMD-BRNN' = AM[[3]][,5]
)
AM <- AM %>% filter(Observed > 0) %>% melt(id.vars = 1)

# CE
CE <- PREDICTION$bra_vmd_CE
CE <- data.frame(
  'State'    = rep('CE'),
  'Observed' = CE[[1]][,1],
  'ODA-VMD-CUBIST' = CE[[1]][,4],
  'TDA-VMD-CUBIST' = CE[[2]][,4],
  'SDA-VMD-CUBIST' = CE[[3]][,4]
)
CE <- CE %>% filter(Observed > 0) %>% melt(id.vars = 1)

# PE
PE <- PREDICTION$bra_single_PE
PE <- data.frame(
  'State'    = rep('PE'),
  'Observed' = PE[[1]][,1],
  'ODA-CUBIST' = PE[[1]][,4],
  'TDA-CUBIST' = PE[[2]][,4],
  'SDA-SVR' = PE[[3]][,3]
)
PE <- PE %>% filter(Observed > 0) %>% melt(id.vars = 1)

# RJ
RJ <- PREDICTION$bra_vmd_RJ
RJ <- data.frame(
  'State'    = rep('RJ'),
  'Observed' = RJ[[1]][,1],
  'ODA-VMD-CUBIST' = RJ[[1]][,4],
  'TDA-VMD-CUBIST' = RJ[[2]][,4],
  'SDA-VMD-CUBIST' = RJ[[3]][,4]
)
RJ <- RJ %>% filter(Observed > 0) %>% melt(id.vars = 1)

# SP
SP <- PREDICTION$bra_vmd_SP
SP <- data.frame(
  'State'    = rep('SP'),
  'Observed' = SP[[1]][,1],
  'ODA-VMD-CUBIST' = SP[[1]][,4],
  'TDA-VMD-CUBIST' = SP[[2]][,4],
  'SDA-VMD-CUBIST' = SP[[3]][,4]
)
SP <- SP %>% filter(Observed > 0) %>% melt(id.vars = 1)

## USA ----
# CA
CA <- PREDICTION$usa_single_California
CA <- data.frame(
  'State'    = rep('CA'),
  'Observed' = CA[[1]][,1],
  'ODA-BRNN' = CA[[1]][,5],
  'TDA-BRNN' = CA[[2]][,5],
  'SDA-BRNN' = CA[[3]][,5]
)
CA <- CA %>% filter(Observed > 0) %>% melt(id.vars = 1)

# IL
IL <- data.frame(
  'State'          = rep('IL'),
  'Observed'       = PREDICTION[["usa_single_Illinois"]][[1]][,1],
  'ODA-CUBIST'     = PREDICTION[["usa_single_Illinois"]][[1]][,4],
  'TDA-CUBIST'     = PREDICTION[["usa_single_Illinois"]][[2]][,4],
  'SDA-VMD-CUBIST' = PREDICTION[["usa_vmd_Illinois"]][[3]][,4]
) %>% filter(Observed > 0) %>% melt(id.vars = 1)

# MA
MA <- data.frame(
  'State'          = rep('MA'),
  'Observed'       = PREDICTION[["usa_single_Massachusetts"]][[1]][,1],
  'ODA-BRNN'     = PREDICTION[["usa_single_Massachusetts"]][[1]][,5],
  'TDA-BRNN'       = PREDICTION[["usa_single_Massachusetts"]][[2]][,5],
  'SDA-VMD-CUBIST' = PREDICTION[["usa_vmd_Massachusetts"]][[3]][,4]
) %>% filter(Observed > 0) %>% melt(id.vars = 1)

# NJ
NJ <- data.frame(
  'State'          = rep('NJ'),
  'Observed'       = PREDICTION[["usa_single_New Jersey"]][[1]][,1],
  'ODA-CUBIST'     = PREDICTION[["usa_single_New Jersey"]][[1]][,4],
  'TDA-BRNN'       = PREDICTION[["usa_single_New Jersey"]][[2]][,5],
  'SDA-VMD-CUBIST' = PREDICTION[["usa_vmd_New Jersey"]][[3]][,4]
) %>% filter(Observed > 0) %>% melt(id.vars = 1)

# NY
NY <- data.frame(
  'State'          = rep('NY'),
  'Observed'       = PREDICTION[["usa_single_New York"]][[1]][,1],
  'ODA-VDM-CUBIST' = PREDICTION[["usa_vmd_New York"]][[1]][,4],
  'TDA-SVR'        = PREDICTION[["usa_single_New York"]][[2]][,3],
  'SDA-SVR'        = PREDICTION[["usa_single_New York"]][[3]][,3]
) %>% filter(Observed > 0) %>% melt(id.vars = 1)

# PA
PA <- data.frame(
  'State'          = rep('PA'),
  'Observed'       = PREDICTION[["usa_vmd_Pennsylvania"]][[1]][,1],
  'ODA-VDM-CUBIST' = PREDICTION[["usa_vmd_Pennsylvania"]][[1]][,4],
  'TDA-VDM-CUBIST' = PREDICTION[["usa_vmd_Pennsylvania"]][[2]][,4],
  'SDA-VDM-CUBIST' = PREDICTION[["usa_vmd_Pennsylvania"]][[3]][,4]
) %>% filter(Observed > 0) %>% melt(id.vars = 1)


## Plots ----
# list of states
states <- c('AM','CE','PE','RJ','SP',
            'CA','IL','MA','NJ','NY')

pred_states <- list()

for (state in states) {
  pred_states[[state]] <- get(state)
}

setwd(FiguresDir)
require(scales)

for (state in states) {
  n <- table(pred_states[[state]]$variable)[1]
  plot <- pred_states[[state]] %>% as.data.frame %>% 
    ggplot(aes(x = rep(seq(n), times = 4), y = value, group = variable)) +
    geom_line(aes(linetype = variable, color = variable), size = 1) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.2, 0.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 20),
          text = element_text(family = "CM Roman", size = 20),
          axis.text.y = element_text(angle = 90)) +
    ylab('Cumulative confirmed cases') + xlab('Day') +
    geom_vline(xintercept = n - 6, color = 'black', size = 0.5) +
    annotate(geom = 'text', x = n*.2, y = max(pred_states[[state]]$value)*.2, 
             label = 'Training', color = 'black', family = 'CM Roman', size = 7) +
    annotate(geom = 'text', x = n, y = max(pred_states[[state]]$value)*.2, 
             label = 'Test', color = 'black', family = 'CM Roman', size = 7) +
    scale_y_continuous(labels = comma)
    
  plot %>% 
    ggsave(
      filename = paste0('PO_',state,'.pdf'),
      device = 'pdf',
      width = 9,
      height = 6.75,
      units = "in",
      dpi = 300
    ) 
}
