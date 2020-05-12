setwd(ResultsDir)

list_files <- list.files(pattern = ".RData")
list_files <- list_files[-c(16,22)]
RESULTS <- list()
PREDICTION <- list()
METRICS <- list()
IMPORTANCE <- list()
HYPER <- list()
ERRORS <- list()
for (file in seq(list_files)) {
  RESULTS[[file]] <- get(load(list_files[file]))
  # assign(paste0(sub(".RDa.*", "", list_files[file])), RESULTS)
  PREDICTION[[file]] <- RESULTS[[file]]$Predictions
  METRICS[[file]] <- RESULTS[[file]]$Metrics
  IMPORTANCE[[file]] <- RESULTS[[file]]$Var_Importance
  HYPER[[file]] <- RESULTS[[file]]$Hyperparameters
  ERRORS[[file]] <- RESULTS[[file]]$Errors
  
  rm(
     bra_single_results,
     usa_single_results,
     bra_vmd_results,
     usa_vmd_results
  )
}

names(RESULTS) <- paste0(sub(".RDa.*", "", list_files))
names(PREDICTION) <- names(RESULTS)
names(METRICS) <- names(RESULTS)
names(IMPORTANCE) <- names(RESULTS)
names(HYPER) <- names(RESULTS)
names(ERRORS) <- names(RESULTS)

# xlsx::write.xlsx(IMPORTANCE, file = 'importance.xlsx')

erro_SDA <- list()

states <- c('AM', 'CE', 'PE', 'RJ', 'SP',
            'CA', 'IL', 'MA', 'NJ', 'NY')

for (ii in seq(ERRORS)) {
  erro_SDA[[ii]] <- ERRORS[[ii]][[3]] 
  
  if(ii %in% c(1:10)) {
    if(ii %in% c(1:5)) {
      erro_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_SDA[[ii]])),
        Country = "BRA",
        State = states[ii],
        erro_SDA[[ii]]
      )
      colnames(erro_SDA[[ii]]) <- c(colnames(erro_SDA[[ii]])[1:3], 'KNN', 'SVR',
                                    'CUBIST', 'BRNN', 'QRF')
    } else {
      erro_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_SDA[[ii]])),
        Country = "BRA",
        State = states[ii-5],
        erro_SDA[[ii]]
      )
      colnames(erro_SDA[[ii]]) <- c(colnames(erro_SDA[[ii]])[1:3], 'VMD-KNN', 'VMD-SVR',
                                    'VMD-CUBIST', 'VMD-BRNN', 'VMR-QRF')
    }
  } else {
    if(ii %in% c(11:15)) {
      erro_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_SDA[[ii]])),
        Country = "USA",
        State = states[ii-5],
        erro_SDA[[ii]]
      )
      colnames(erro_SDA[[ii]]) <- c(colnames(erro_SDA[[ii]])[1:3], 'KNN', 'SVR',
                                    'CUBIST', 'BRNN', 'QRF')
    } else {
      erro_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_SDA[[ii]])),
        Country = "USA",
        State = states[ii-10],
        erro_SDA[[ii]]
      )
      colnames(erro_SDA[[ii]]) <- c(colnames(erro_SDA[[ii]])[1:3], 'VMD-KNN', 'VMD-SVR',
                                    'VMD-CUBIST', 'VMD-BRNN', 'VMR-QRF')
    }
  }
  
  erro_SDA[[ii]] <- erro_SDA[[ii]] %>% melt(id.vars = c(1:3))
  colnames(erro_SDA[[ii]]) <- c(colnames(erro_SDA[[ii]])[1:3], 'Model', 'Error')
}

# create a dataframe from the list
do.call(rbind.data.frame, erro_SDA) %>% 
  write.csv('error_SDA.csv', row.names = FALSE)


erro_test_SDA <- list()

states <- c('AM', 'CE', 'PE', 'RJ', 'SP',
            'CA', 'IL', 'MA', 'NJ', 'NY')

for (ii in seq(ERRORS)) {
  erro_test_SDA[[ii]] <- tail(ERRORS[[ii]][[3]],6)
  
  if(ii %in% c(1:10)) {
    if(ii %in% c(1:5)) {
      erro_test_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_test_SDA[[ii]])),
        Country = "BRA",
        State = states[ii],
        erro_test_SDA[[ii]]
      )
      colnames(erro_test_SDA[[ii]]) <- c(colnames(erro_test_SDA[[ii]])[1:3], 'KNN', 'SVR',
                                         'CUBIST', 'BRNN', 'QRF')
    } else {
      erro_test_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_test_SDA[[ii]])),
        Country = "BRA",
        State = states[ii-5],
        erro_test_SDA[[ii]]
      )
      colnames(erro_test_SDA[[ii]]) <- c(colnames(erro_test_SDA[[ii]])[1:3], 'VMD-KNN', 'VMD-SVR',
                                         'VMD-CUBIST', 'VMD-BRNN', 'VMR-QRF')
    }
  } else {
    if(ii %in% c(11:15)) {
      erro_test_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_test_SDA[[ii]])),
        Country = "USA",
        State = states[ii-5],
        erro_test_SDA[[ii]]
      )
      colnames(erro_test_SDA[[ii]]) <- c(colnames(erro_test_SDA[[ii]])[1:3], 'KNN', 'SVR',
                                         'CUBIST', 'BRNN', 'QRF')
    } else {
      erro_test_SDA[[ii]] <- data.frame(
        n = seq(nrow(erro_test_SDA[[ii]])),
        Country = "USA",
        State = states[ii-10],
        erro_test_SDA[[ii]]
      )
      colnames(erro_test_SDA[[ii]]) <- c(colnames(erro_test_SDA[[ii]])[1:3], 'VMD-KNN', 'VMD-SVR',
                                         'VMD-CUBIST', 'VMD-BRNN', 'VMR-QRF')
    }
  }
  
  erro_test_SDA[[ii]] <- erro_test_SDA[[ii]] %>% melt(id.vars = c(1:3))
  colnames(erro_test_SDA[[ii]]) <- c(colnames(erro_test_SDA[[ii]])[1:3], 'Model', 'Error')
}

# create a dataframe from the list
do.call(rbind.data.frame, erro_test_SDA) %>% 
  write.csv('error_test_SDA.csv', row.names = FALSE)


