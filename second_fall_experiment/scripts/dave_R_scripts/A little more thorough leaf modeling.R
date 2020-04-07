library(pacman)
p_load(tidyverse, here, randomForest, reshape2, magrittr, data.table, caret, tictoc, tdr)

# data import
iData <- fread('/home/wmsru/Documents/Clay/greenhouse_2019/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/Harvest1_leafMeasurements.csv')


# model is far more predictive if leaf length is included, but leaf length sucks to measure
# can it be modeled decently enough to use a modeled estimate and improve leaf area predictions?
# KEY CODE FOR LEAF AREA MODEL below
rf2 <- randomForest(leaf_length_cm~leaf_width_cm + leaf_id, 
                    data=na.omit(iData),
                    mtry=1, ntrees=500)

summary(lm(leaf_length_cm~leaf_width_cm + leaf_id, data=iData))

varImpPlot(rf2)


iData$predLength <- predict(rf2, iData, type='response')



# subset data to pertinent columns
widthData <- iData %>%
  subset(., select=c(pot_id, leaf_width_cm, predLength, leaf_area_cm2)) %>%
  na.omit


# this function takes random subsets of the dataset and characterizes the distribution of error terms for a lot of models   
rfFunc <- function(x){
    testPlantID <- sample(unique(widthData$pot_id),1)
    trainPlants <- subset(widthData, pot_id!=testPlantID) %>%
      sample_n(., 50)
    testPlant <- subset(widthData, pot_id==testPlantID) %>%
      sample_n(., 6)
    
    mod <- randomForest(leaf_area_cm2~leaf_width_cm, #+ predLength,
           data=trainPlants, mtry=1, num.trees=100)
    testPlant$modelPredictions <- predict(mod, newdata=testPlant, type='response')
    
    tdStats(m=testPlant$modelPredictions, o=testPlant$leaf_area_cm2,
            functions=c('mbe', 'mae', 'r2')) %>%
      melt %>%
      rownames_to_column(., 'stat')
}


modelRuns2 <- bind_rows(lapply(1:1000, rfFunc)) 

modelRuns2 %>%
  group_by(stat) %>%
  summarise(med=median(value))

ggplot(modelRuns2, aes(x=value)) + theme_bw(base_size=15) +
  geom_density(fill='dodgerblue3', alpha=0.75) +
  facet_wrap(~stat, scales='free')




       