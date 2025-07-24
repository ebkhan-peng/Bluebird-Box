library(janitor)
library(tidyverse)
library(here)
library(car)
library(patchwork)
###################################################

#clean table
egg_spat <- read.csv(here('data', 'BB Egg and Spatial Data.csv'))

egg_spat_name <- clean_names(egg_spat)

egg_spat_clean <- select(egg_spat_name, forest_meters1, developed_meters_1, 
                         river_distance, box_code, years_with_nest_material,
                         species, eggs,chicks_hatched, fledglings_per_eggs_laid, distance)

write.csv(egg_spat_clean, here('data', 'Egg + Spatial Data Clean'))
######################################################################

#separate successes and failures
egg_succ <- filter(egg_spat_clean, box_code == 'B3' | box_code == 'B4' | 
                     box_code == 'C2' | box_code == 'C3')

egg_fail <- filter(egg_spat_clean, box_code != 'B3' & box_code != 'B4' &
                     box_code != 'C2' & box_code != 'C3')

#Mann Whitney U Test for Forest Distance
wilcox.test(egg_succ$forest_meters1, egg_fail$forest_meters1, paierd = FALSE,
            exact = FALSE)

#Developed Distance
wilcox.test(egg_succ$developed_meters_1, egg_fail$developed_meters_1, paierd = FALSE,
            exact = FALSE)

#River Distance
wilcox.test(egg_succ$river_distance, egg_fail$river_distance, paierd = FALSE,
            exact = FALSE)

#Box Spacing
wilcox.test(egg_succ$distance, egg_fail$distance, paierd = FALSE,
            exact = FALSE)

################################################
#Summary Stats

egg_succ_forest <-summary(egg_succ$forest_meters1)
egg_fail_forest <-summary(egg_fail$forest_meters1)

egg_succ_dev <-summary(egg_succ$developed_meters_1)
egg_fail_dev <-summary(egg_fail$developed_meters_1)

egg_succ_river <-summary(egg_succ$river_distance)
egg_fail_river <-summary(egg_fail$river_distance)

egg_succ_spacing <-summary(egg_succ$distance)
egg_fail_spacing <-summary(egg_fail$distance)

################################################
#put summary stats in tables
no_egg_group <- data.frame(forest_m = unclass(egg_fail_forest),
                           developed_m = unclass(egg_fail_dev),
                           river_m = unclass(egg_fail_river),
                           spacing_m = unclass(egg_fail_spacing))

egg_group <- data.frame(forest_m = unclass(egg_succ_forest),
                           developed_m = unclass(egg_succ_dev),
                           river_m = unclass(egg_succ_river),
                           spacing_m = unclass(egg_succ_spacing))

#############################################################
#make boxplots

#Comparison of distance from forest
egg_forest_box <- ggplot(data = egg_succ, aes(y=forest_meters1))+
                           geom_boxplot()+
  ylim(270,710)+
  xlab('Egg Group')+ ylab("Distance from Forest (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
  axis.ticks.x = element_blank())
egg_forest_box

no_egg_forest_box <- ggplot(data = egg_fail, aes(y=forest_meters1))+ 
  geom_boxplot()+
  ylim(270,710)+
  xlab('No Egg Group')+ ylab("Distance from Forest (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
no_egg_forest_box 

forest_box <-no_egg_forest_box + egg_forest_box
forest_box

ggsave(here("Forest_Comparison_Plot.jpg"), forest_box, 
       dpi=300, unit="in")


#Comparison of distance from developed land
egg_dev_box <- ggplot(data = egg_succ, aes(y=developed_meters_1))+
  geom_boxplot()+
  ylim(0, 550)+
  xlab('Egg Group')+ ylab("Distance from Developed (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
egg_dev_box

no_egg_dev_box <- ggplot(data = egg_fail, aes(y=developed_meters_1))+ 
  geom_boxplot()+
  ylim(0,550)+
  xlab('No Egg Group')+ ylab("Distance from Developed (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
no_egg_dev_box 

developed_box <-no_egg_dev_box + egg_dev_box
developed_box

ggsave(here("Developed_Comparison_Plot.jpg"), developed_box, 
       dpi=300, unit="in")

#Comparison of distance from rivers
egg_river_box <- ggplot(data = egg_succ, aes(y=river_distance))+
  geom_boxplot()+
  ylim(100, 550)+
  xlab('Egg Group')+ ylab("Distance from Rivers (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
egg_river_box

no_egg_river_box <- ggplot(data = egg_fail, aes(y=river_distance))+ 
  geom_boxplot()+
  ylim(100,550)+
  xlab('No Egg Group')+ ylab("Distance from Rivers (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
no_egg_river_box 

river_box <-no_egg_river_box + egg_river_box
river_box

ggsave(here("River_Comparison_Plot.jpg"), river_box, 
       dpi=300, unit="in")


#Comparison of distance from rivers
egg_space_box <- ggplot(data = egg_succ, aes(y=distance))+
  geom_boxplot()+
  ylim(0, 50)+
  xlab('Egg Group')+ ylab("Distance from Nearest Box (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
egg_space_box

no_egg_space_box <- ggplot(data = egg_fail, aes(y=distance))+ 
  geom_boxplot()+
  ylim(0,50)+
  xlab('No Egg Group')+ ylab("Distance from Nearest Box (m)")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
no_egg_space_box 

space_box <-no_egg_space_box + egg_space_box
space_box

ggsave(here("Spacing_Comparison_Plot.jpg"), space_box, 
       dpi=300, unit="in")

#combine all box plots
all_box <- (forest_box + developed_box) / (river_box + space_box) 
all_box

ggsave(here("All_Variable_Comparison.jpg"), all_box, height = 10,
       width = 10, dpi=300, unit = "in")
