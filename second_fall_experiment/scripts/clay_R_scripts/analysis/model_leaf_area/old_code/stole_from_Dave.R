library(pacman)
p_load(tidyverse, here, randomForest, reshape2, magrittr, data.table, caret, tictoc, tdr)

# data import
iData <- fread('/home/sean/github/2020_greenhouse/second_fall_experiment/scripts/clay_R_scripts/analysis/model_leaf_area/Harvest1_leafMeasurements.csv')


# model is far more predictive if leaf length is included, but leaf length sucks to measure
# can it be modeled decently enough to use a modeled estimate and improve leaf area predictions?
# KEY CODE FOR LEAF AREA MODEL below
rf2 <- randomForest(leaf_length_cm~leaf_width_cm + leaf_id, 
                    data=na.omit(iData),
                    mtry=1, ntrees=500)

summary(lm(leaf_length_cm~leaf_width_cm + leaf_id, data=iData))
summary(lm(leaf_length_cm~leaf_width_cm, data=iData))

varImpPlot(rf2)


iData$predLength <- predict(rf2, iData, type='response')



# subset data to pertinent columns
widthData <- iData %>%
  subset(., select=c(pot_id, leaf_width_cm, predLength, leaf_area_cm2)) %>%
  na.omit


widthData <- harvest

# this function takes random subsets of the dataset and characterizes the distribution of error terms for a lot of models   
rfFunc <- function(x){
    # print(x)
    # sample 25% of plants for testing
    testPlantID <- sample(unique(widthData$pot_id), 7) 
    # sample 75% for training
    trainPlants <- subset(widthData, !pot_id %in% testPlantID) 
    testPlant <- subset(widthData, pot_id %in% testPlantID) 
    
    mod <- randomForest(leaf_area_cm2 ~ leaf_width_cm + mean_width_cm + leaf_order_reverse +
                          days_since_planting + predLength,
           data=trainPlants, mtry=2, num.trees=500)
    testPlant$modelPredictions <- predict(mod, newdata=testPlant, type='response')
    
    tdStats(m=testPlant$modelPredictions, o=testPlant$leaf_area_cm2,
            functions=c('mbe', 'mae', 'r2')) %>%
      melt %>%
      rownames_to_column(., 'stat')
}


modelRuns <- bind_rows(lapply(1:1000, rfFunc)) 

modelRuns %>%
  group_by(stat) %>%
  summarise(med=median(value))

ggplot(modelRuns, aes(x=value)) + theme_bw(base_size=15) +
  geom_density(fill='dodgerblue3', alpha=0.75) +
  facet_wrap(~stat, scales='free')



### Result (using 75/25 split by pot_id)
### Other vars inlcuded: leaf_order_reverse, days_since_planting
# Vars tested but not kept: mean_width_cm

# Results (MAE, MBE, R2):
# Without leaf length: 67, -2.6, 0.88
# With leaf length (measured): 41, -2.6, 0.93
# With leaf length (predicted by separate RF): 50, -2.1, 0.91

summary(lm(leaf_area_cm2 ~ leaf_width_cm + leaf_length_cm + leaf_order_reverse +
             days_since_planting, data = harvest))


       