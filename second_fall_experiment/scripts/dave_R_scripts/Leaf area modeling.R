library(pacman)
p_load(tidyverse, here, randomForest, reshape2, cowplot, magrittr, data.table, broom)

iData <- fread(here::here('Data', 'Harvest1_leafMeasurements.csv'))

# calculate area
modData <- iData %>%
  mutate(elipseArea=pi*leaf_length_cm*leaf_width_cm,
         isoTriArea=2*(leaf_width_cm*0.5*leaf_length_cm)) %>%
  mutate(elipseIsoMean=(elipseArea + isoTriArea)/2) %>%
  subset(., select=-c(pot_id, `measure-er`, percent_dead, notes, treatment))


# first random forest with just leaf width, number and percent expanded
rf1 <- randomForest(leaf_area_cm2 ~ percent_expanded + leaf_width_cm,
                    data=modData, mtry=2, ntree=10000)
varImpPlot(rf1)

    # explains ~ 87% of variance in leaf area


rf2 <- randomForest(leaf_area_cm2 ~ leaf_width_cm + leaf_length_cm,
                    data=modData, mtry=2, ntree=1000)

varImpPlot(rf2)
    # explains ~ 94% of variance


# test model on withheld subset
trainSub <- sample(1:72, 50)
train <- modData[trainSub,]
test <- modData[-trainSub,]


rfTest <- randomForest(leaf_area_cm2 ~ leaf_width_cm + leaf_length_cm,
                        data=train, mtry=1, ntree=1000) 

test$Pred <- predict(rfTest, newdata=test, type='response')

ggplot(test, aes(x=leaf_area_cm2, y=Pred)) + theme_bw(base_size=15) +
  geom_abline(slope=1, size=1.5) +
  geom_smooth(method='lm', color='red') +
  geom_point(size=6, shape=21, fill='grey50') +
  labs(x='Measured leaf area', y='Modeled leaf area')

