# Script for Chapter 2 plot generation: Main Figures and Supplemental Figures
# Compiled from other code files on 7/30/24

#import libraries
library(ggOceanMaps) #used version 2.1.17
library(tidyverse) 
library(marmap) 
library(cowplot) 
library(FSA) 
library(vegan) 
library(ggh4x) 
library(ggbreak) 
library(ggsci)
library(patchwork)
library(gridExtra)


#theme for plots
plot_theme <- theme_bw()+
  theme(axis.title.y = element_text(size = 14,  face = "bold"),
        axis.text.x = element_text(size = 12),                                
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 20))+
  theme(legend.key = element_rect(fill = "white"),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 12, face = "bold"))+
  theme(panel.background = element_rect(fill = "white", colour="black"), panel.grid.major = element_line(colour = "white", linewidth=.5),
        panel.grid.minor = element_line(colour = "white", linewidth=.5),
        strip.background = element_rect(fill = "white", color = "black", size = .5))


######### FIGURE 1: MAP OF STUDY REGION ######
options(ggOceanMaps.userpath = "Shapefiles/gebco_2023/GEBCO_2023.nc")

#import all stations: just using en668 as example dataset
samples <- read.csv('https://nes-lter-data.whoi.edu/api/stations/en668.csv')

samples_filtered <- samples %>%
  filter(name %in% c("L1", "L2", "L4", "L6", "L8", "L10"))
#get noaa bathymetry for the contour lines
b = getNOAA.bathy(-74, -69,39, 42.5, resolution = 1)
# convert bathymetry to data frame
bf = fortify.bathy(b)

#make map
zoomed_in <- basemap(limits = c(-74, -69,39, 42.5), bathy.style = "rub")+
  geom_contour(data = bf, aes(x = x, y = y, z = z), color = "black", breaks = c(-50, -100, -200, -500,-1000, -2000))+
  theme(legend.position = "none")+
  geom_point(data = samples_filtered, aes(x = longitude, y = latitude), size = 3)+
  annotate("rect", xmin = -71.5, xmax = -70.25, ymin = 39.85, ymax = 41.25, fill = "transparent", color = "black", linetype = "dashed")
zoomed_in 

ggsave( "zoomed_in_map.pdf", plot = zoomed_in, device = "pdf", scale = 1, width =16, height = 14, units ="cm",
        dpi = 300)
#Then import into affinity designer and add the zoomed out map to the lower left corner.
######### END OF FIGURE 1 ############


######### FIGURE 2: WHOLE SHELF TOTAL INTEGRATED NPP & DISCRETE ##########


#read in pp_int_figs dataframe
pp_int_figs <- read_csv("Processed_data_outputs/int_pp_corrected_011924.csv")

pp_int_figs <- pp_int_figs %>%
  filter(!(cruise == "EN668" & cast == "20")) %>% #removing this cruise/cast combo because this was d6a and not included in this paper
  filter(nearest_station != "L11") #not including L11 in this paper

#### Count up how many stations per cruise for SUPPLEMENTAL TABLE 1 #####
stn_cruise_count <- pp_int_figs %>%
  select(cruise, nearest_station, cast) %>%
  unique() %>%
  group_by(cruise) %>%
  tally()

pp_int_figs$season <- factor(pp_int_figs$season , levels = c("Winter", "Summer"))


#summarize by season to get the average for summer and winter
pp_int_figs_summarized_season <- pp_int_figs%>%
  filter(cruise != "EN644") %>%
  filter(size == "pp_GFF") %>% #getting just the total dataframe
  dplyr::group_by(season) %>%
  dplyr::summarise(mean= mean(int_pp), sd = sd(int_pp))

#calculate NPP without the L2 outliers to determine percent difference for paper

#without outlier, average shelf NPP is 442, not 572 in winter, 23%
#without outlier, average shelf NPP is 708, not 832 in summer, 16% increase
pp_int_inner_shelf_not_outliers <-  pp_int_figs%>%
  filter(cruise != "EN644") %>%
  filter(size == "pp_GFF") %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(nearest_station != "L11") %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(mean= mean(int_pp), sd = sd(int_pp))
#then do the % change between the with and without outlier averages

#Now finally getting into the actual plot for figure 2
pp_int_figs_summarized_season$season <- factor(pp_int_figs_summarized_season $season, levels = c("Winter", "Summer"))

#test for seasonal sig within LTER
pp_int_figs_total <- pp_int_figs %>%
  filter(size  == "pp_GFF")

pp_int_figs_total $season <- factor(pp_int_figs_total  $season, levels = c("Winter", "Summer"))

#test for difference in integrated NPP by season
wilcox.test(int_pp~season, data = pp_int_figs_total) # p value is 0.01

#PLot of Just LTER season data
seasonal_lter <- ggplot(pp_int_figs_total, aes(x = factor(season), y = int_pp, fill = season))+
  plot_theme+
  geom_boxplot(aes(fill = season),color = "black", size = 0.6, width = 0.7, outlier.shape = 16, outlier.size = 2)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width = 0.2, position = position_dodge(0.7)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width = 0.2, position = position_dodge(0.7)) +
  ggplot2::stat_summary(fun = base::mean, geom="point", shape=21, size=2, position = position_dodge(0.7))+
  ylab(expression(atop(bold("Integrated NPP"), paste(~mgC~m^-2~d^-1)))) +  # Two lines in ylab
  xlab("Season")+
  scale_y_continuous(breaks = seq(0, 2600, 300),
    minor_breaks = seq(0, 2600, 150),
    limits = c(0,2600),
    guide = "axis_minor",expand = c(0,0))+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  annotate(geom = "text", x = "Winter", y = 1000, label = "a", size = 3)+
  annotate(geom = "text", x = "Summer", y = 1100, label = "b", size = 3)+
  theme(legend.position = "none")+
  coord_cartesian(ylim = c(0, 1200))
seasonal_lter

#read in discrete data for discrete plot which is panel "B" in the paper
#read in pp data
pp_discrete_plots <- read_csv("Processed_data_outputs/pp_discrete_averaged_temp_filter_011924.csv")

#need to fix EN661 C14 -- should be cast15
pp_discrete_plots[261, 3] <- 15
pp_discrete_plots[262, 3] <- 15
pp_discrete_plots[263, 3] <- 15
pp_discrete_plots[264, 3] <- 15

#These are the casts that are being excluded
pp_discrete_plots <- pp_discrete_plots %>%
  filter(!(cruise == "EN668" & cast == "20")) %>% #d6a %>%
  filter(station != "L11")

pp_discrete_plots <- pp_discrete_plots %>%
  filter(cruise %in% c("AT46", "EN649", "EN661", "EN655", "EN687", "EN668"))

#add region 
pp_discrete_plots$region <- with(pp_discrete_plots,
                                 ifelse(station %in% c("L1", "L2", "L1.2"), "Inner-shelf",
                                        ifelse(station %in% c("L4", "L6", "d6a"), "Mid-shelf",
                                               ifelse(station %in%  c('L8', "L10", "L11"), "Outer-shelf", NA)))) 

#add season
pp_discrete_plots$season <- with(pp_discrete_plots,
                                 ifelse(cruise == "EN644", "Summer",
                                        ifelse(cruise == "EN655", "Summer",
                                               ifelse(cruise == "EN649", "Winter", 
                                                      ifelse(cruise == "EN661", "Winter",
                                                             ifelse(cruise == "EN668", "Summer",
                                                                    ifelse(cruise == "AT46", "Winter",
                                                                           ifelse(cruise == "EN687", "Summer",NA))))))))

#### Count up the number of discrete depths for SUPPLEMENTAL TABLE 1 #####
pp_discrete_depths <-  pp_discrete_plots %>%
  select(cruise,cast, depth_grp) %>%
  unique() %>%
  group_by(cruise) %>%
  tally()

#Read in depth information for the discrete plot
pp_depths <- read_csv("pp_depths.csv")


#Examining the depth labels
pp_depth_check <- pp_discrete_plots %>%
  select(cruise, cast, station, depth, depth_category, size) %>%
  filter(size == "total") %>%
  unique() %>%
  left_join(pp_depths, by = c("cruise", "cast", c("depth_category" = "depth_cat")))

pp_depths$depth_ID[pp_depths$depth_ID == "surf"] <- "Surface"
pp_depths$depth_ID[pp_depths$depth_ID == "CM"] <- "Chl-a\nMax."
pp_depths$depth_ID[pp_depths$depth_ID == "below max"] <- "Below\nChl-a Max."

pp_depths$depth_ID <- factor(pp_depths$depth_ID, levels = c( "Below\nChl-a Max.", "Chl-a\nMax.", "Surface"))

#Add depth category to pp data
total_discrete <- pp_discrete_plots %>%
  filter(temp_check == "Good") %>% #only include the samples that didn't have a temp excursion issue
  filter(size == "total") %>%
  left_join(pp_depths, by = c("cruise", "cast","niskin")) %>%
  filter(!is.na(depth_ID)) %>%
  filter(depth_ID != "extra depth")

#calculate average
total_discrete_avg <- total_discrete  %>%
  group_by(season,depth_ID) %>%
  summarise(mean =  mean(mean_npp), sd = sd(mean_npp), n = n()) 

#sig testing for npp by depth
total_discrete_winter <- total_discrete %>%
  filter(season == "Winter") 

kruskal.test(mean_npp~depth_ID, data = total_discrete_winter) #p < 0.05

dunnTest(mean_npp ~ depth_ID,
         data=total_discrete_winter,
         method="bonferroni")  #all depths are different from each other except the chla max and below CM. BUT WITH UPDATED TEMP EXCURSION RULE, THE STATS ARE DIFFERENT, SO FIX THIS IN RE-SUBMISSION

total_discrete_summer <- total_discrete %>%
  filter(season == "Summer") 

kruskal.test(mean_npp~depth_ID, data = total_discrete_summer) #p = 0.009

dunnTest(mean_npp ~ depth_ID,
         data=total_discrete_summer,
         method="bonferroni") #Only CM and BCM are different from each other

#Now plot these averages
total_discrete_avg_plot <- ggplot(total_discrete, aes(y = depth_ID, x = mean_npp, fill = season))+
  plot_theme +
  geom_boxplot(aes(fill = season),color = "black", size = 0.6, width = 0.7, outlier.shape = 16, outlier.size = 2)+
 stat_boxplot(geom = "errorbar", aes(xmin = ..xmax..), width = 0.2, position = position_dodge(0.7)) +
  stat_boxplot(geom = "errorbar", aes(xmax = ..xmin..), width = 0.2, position = position_dodge(0.7)) +
  ggplot2::stat_summary(fun = base::mean, geom="point", shape=21, size=2, position = position_dodge(0.7))+
  xlab(bquote(bold("NPP") ~ (mgC ~ m^-2 ~ d^-1)))+
  ylab("Depth Type")+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  theme(legend.position = "none")+
  coord_cartesian(xlim = c(0, 70))
total_discrete_avg_plot 

#join integrated and discrete plots
whole_shelf_total_pp <- plot_grid(seasonal_lter, total_discrete_avg_plot , ncol=1, align="v", labels = c("A", "B"), label_size = 10 )
#  <- ggarrange(seasonal_lter, total_discrete_avg_plot ,nrow = 1, labels = c("a)", "b)"),common.legend = FALSE, font.label = list(size = 8))
whole_shelf_total_pp 
ggsave('Manuscript/Submission/Revisions/FIGURE2.pdf', plot = whole_shelf_total_pp , scale = 1, width = 3.5, height = 7 , units ="in", dpi = 300)
######### END OF FIGURE 2 ############

######### FIGURE 3: INTEGRATED & DISCRETE DATA FOR SIZE-FRACTIONATED NPP ON WHOLE SHELF ######
pp_int_sizes_summarized_season <- pp_int_figs %>%
  filter(size != "pp_GFF") %>% #removing these sizes because just need the <5, 5-20, and >20
  filter(size != "pp_greater5") %>%
  dplyr::group_by(season, size) %>%
  dplyr::summarise(mean_pp = mean(int_pp, na.rm = TRUE), sd = sd(int_pp, na.rm = TRUE), n = n())

pp_int_sizes <- pp_int_figs %>%
  filter(size != "pp_GFF") %>% #removing these sizes because just need the <5, 5-20, and >20
  filter(size != "pp_greater5")

pp_int_sizes$size <- factor(pp_int_sizes$size , levels = c("pp_less5","pp_between5_20", "pp_greater_20"))

levels(pp_int_sizes$size) <- c("< 5", "5-20", "> 20")
pp_int_sizes$season <- factor(pp_int_sizes$season , levels = c("Winter", "Summer"))

#Make the plot
pp_int_season_plot <- ggplot(pp_int_sizes, aes(x = factor(size), y = int_pp, fill = season, alpha = size))+
  plot_theme+
  facet_wrap(~season)+
  geom_boxplot(aes(fill = season),color = "black", size = 0.6, width = 0.7, outlier.shape = 16, outlier.size = 2)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width = 0.2, position = position_dodge(0.7)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width = 0.2, position = position_dodge(0.7)) +
  ggplot2::stat_summary(fun = base::mean, geom="point", shape=21, size=2, position = position_dodge(0.7))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  ylab(expression(atop(bold("Integrated NPP"), paste(~mgC~m^-2~d^-1)))) +  # Two lines in ylab
  xlab("Size Class (µm)")+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0, 8000, 200),
                   minor_breaks = seq(0, 800, 100),
                      limits = c(0,800),
                      guide = "axis_minor")+
coord_cartesian(ylim = c(0, 800))
pp_int_season_plot

#Testing for significant differences
pp_int_sizes_tests  <- pp_int_figs %>%
  filter(size != "pp_GFF") %>%
  filter(size != "pp_greater5")

pp_int_sizes_tests_winter <- pp_int_sizes_tests %>%
  filter(season == "Winter")

pp_int_sizes_tests_summer <- pp_int_sizes_tests %>%
  filter(season == "Summer")

#test for winter size differences 
kruskal.test(int_pp~size, data = pp_int_sizes_tests_winter) #yes sig dif by size in winter (p = 0.009)
kruskal.test(int_pp~size, data = pp_int_sizes_tests_summer) #no sig dif in summer by size class (p=0.24)

#for winter
dunnTest(int_pp ~ size,
         data=pp_int_sizes_tests_winter,
         method="bonferroni") #only difference is between >20 and <5: p = 0.007

#plot of sizes with depth
#now do the same for the sizes
sizes_discrete <- pp_discrete_plots %>%
  filter(size != "total") %>%
  filter(size != "less20") %>%
  filter(temp_check == "Good") %>%
  left_join(pp_depths, by = c("cruise", "cast","niskin")) %>%
  filter(!is.na(depth_ID)) %>%
  filter(depth_ID != "extra depth")

sizes_discrete_avg <- sizes_discrete  %>%
 group_by(season,depth_ID, size) %>%
 summarise(mean =  mean(mean_npp), sd = sd(mean_npp), n = n()) 

sizes_discrete$size <- factor(sizes_discrete$size, levels = c("less5", "between520", "greater20"))
sizes_discrete$season <- factor(sizes_discrete$season, levels = c("Winter", "Summer"))

#Now plot these averages
sizes_discrete_plot <- ggplot(sizes_discrete, aes(y = depth_ID, x = mean_npp, fill = season, alpha = size))+
  facet_wrap(~season)+
  geom_boxplot(aes(fill = season),color = "black", size = 0.6, width = 0.7, outlier.shape = 16, outlier.size = 2)+
  stat_boxplot(geom = "errorbar", aes(xmin = ..xmax..), width = 0.2, position = position_dodge(0.7)) +
  stat_boxplot(geom = "errorbar", aes(xmax = ..xmin..), width = 0.2, position = position_dodge(0.7)) +
  ggplot2::stat_summary(fun = base::mean, geom="point", shape=21, size=2, position = position_dodge(0.7))+
  plot_theme +
  theme(strip.text.x = element_text(size = 12))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  xlab(bquote(bold("NPP") ~ (mgC ~ m^-2 ~ d^-1)))+
  ylab("Depth Type")+
  theme(legend.position = "none")+
coord_cartesian(xlim = c(0, 40))
sizes_discrete_plot 

whole_shelf_sizes <- plot_grid(pp_int_season_plot, sizes_discrete_plot , ncol=1, align="v", labels = c("A", "B"), label_size = 10 )
whole_shelf_sizes 
ggsave('Manuscript/Submission/Revisions/FIGURE3.pdf', plot = whole_shelf_sizes  , scale = 1, width = 7, height = 6, units ="in", dpi = 300)
#Then adjust the error bars to be all black in affinity designer, as well as fix the panel labels and un-bold the discrete NPP units

######### END OF FIGURE 3 ############

######### FIGURE 4: TWO PANEL PLOT A)BOXPLOT OF TOTAL INTEGRATED NPP ACROSS THE SHELF AND B) %CV FOR EACH REGION/SEASON #########

pp_int_season_regions <- pp_int_figs %>%
  filter(size == "pp_GFF")

pp_int_season_regions$region[pp_int_season_regions$region == "Inner-shelf"] <-  "Inner"
pp_int_season_regions$region[pp_int_season_regions$region == "Mid-shelf"] <-  "Mid"
pp_int_season_regions$region[pp_int_season_regions$region == "Outer-shelf"] <-  "Outer"

pp_int_season_regions$season <- factor(pp_int_season_regions$season, levels = c("Winter", "Summer"))
pp_int_season_regions$region <- factor(pp_int_season_regions$region, levels = c("Inner", "Mid", "Outer"))


pp_int_season_regions_avg <- pp_int_season_regions %>%
  group_by(season,region) %>%
  summarise(mean =  mean(int_pp), sd = sd(int_pp), n = n()) 

#The stat_boxplot option enables me to add little whiskers to the boxplot
seasonal_region <- ggplot(pp_int_season_regions, aes(x = factor(region), y = int_pp, fill= season))+
  plot_theme+
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.2)+
  geom_boxplot(position = position_dodge(preserve = "single", width = 0.9), outlier.color = "black",outlier.shape = 16, outlier.size = 2)+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  xlab("Shelf Region")+
  ylab(bquote(bold("Integrated NPP") ~ (mgC ~ m^-2 ~ d^-1)))+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())+
  coord_cartesian(ylim = c(0, 1000))
seasonal_region

#get the average values for use in the ms
pp_int_season_region_avg <- pp_int_season_regions %>%
  group_by(season, region) %>%
  summarise(mean = mean(int_pp), sd = sd(int_pp))

### Now do panel B, which is the %CV for the region
CV_region <-  pp_int_figs %>%
  filter(nearest_station != "L11") %>%
  #filter(!(cruise == "AT46" & cast == "17")) %>%
  #filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(size == "pp_GFF") %>%
  group_by(season, region) %>%
  summarise(mean= mean(int_pp, na.rm = TRUE), sd = sd(int_pp), CV = (sd/mean)*100)

#change shelf levels
CV_region $region[CV_region $region == "Inner-shelf"] <-  "Inner"
CV_region $region[CV_region $region == "Mid-shelf"] <-  "Mid"
CV_region $region[CV_region $region == "Outer-shelf"] <-  "Outer"

CV_region$season <- factor(CV_region$season, levels = c("Winter", "Summer"))

#this is the plot
CV_total_region_plot <- ggplot(CV_region, aes(x = factor(region), y = CV, fill= season))+
  plot_theme+
  geom_bar(color = "black", stat = "identity", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  xlab("Shelf Region")+
  ylab("% Coefficient of Variation")+
  scale_y_continuous(limits = c(0,130), breaks = c(0,30,60,90,120), expand = c(0,0))+
  theme(legend.position = c(0.8,0.85))
CV_total_region_plot

#Join together the integrated PP across the shelf and the % CV
total_npp_crossshelf <- plot_grid(seasonal_region, CV_total_region_plot , ncol=2, align="h", labels = c("A", "B"), label_size = 10 )
total_npp_crossshelf
ggsave('Manuscript/Submission/Revisions/FIGURE4.pdf', plot =  total_npp_crossshelf, scale = 1, width = 7, height = 4, units ="in", dpi = 300)

#Test for significance differences within each region by season
pp_int_IS <- pp_int_season_regions %>%
  filter(region == "Inner")

wilcox.test(int_pp~season, data = pp_int_IS) #p value is 0.29 so NO Dif

pp_int_MS <- pp_int_season_regions %>% 
  filter(region == "Mid")

wilcox.test(int_pp~season, data = pp_int_MS)#Value is 0.017 so yes, sig

pp_int_OS <- pp_int_season_regions %>%
  filter(region == "Outer")

wilcox.test(int_pp~season, data = pp_int_OS) #0.7 so non-sig


pp_int_winter <- pp_int_season_regions %>%
  filter(season == "Winter")
pp_int_summer <- pp_int_season_regions %>%
  filter(season == "Summer")

kruskal.test(int_pp~region, data = pp_int_winter) #no dif across shelf in winter (0.69)
kruskal.test(int_pp~region, data = pp_int_summer) #no dif across shelf in summer (0.67)

############# END OF FIGURE 4A/B #####################

##### SUPPLEMENTAL TABLE 4: INNER-SHELF AVERAGE WITH AND WITHOUT THE L2 OUTLIERS ####
pp_int_figs_inner_outliers <- pp_int_figs%>%
  filter(region == "Inner-shelf") %>%
  filter(cruise != "EN644") %>%
  filter(size == "pp_GFF") %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(mean= mean(int_pp), sd = sd(int_pp))

# no outliers
pp_int_figs_inner_nooutliers <- pp_int_figs%>%
  filter(region == "Inner-shelf") %>%
  filter(cruise != "EN644") %>%
  filter(size == "pp_GFF") %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(mean= mean(int_pp), sd = sd(int_pp))

##### END OF SUPPLEMENTAL TABLE 4 ##########

##### SUPPLEMENTAL FIGURE 7: DISCRETE NPP PLOT BY SIZE TO HIGHLIGHT L2 #########
#make anamaly plot for size data
#plot of discrete data -- just the inner-shelf
winter_inner_size <- pp_discrete_plots %>%
  filter(season == "Winter") %>%
  filter(region == "Inner-shelf") %>%
  filter(size != "total") %>%
  filter(size != "less20")


summer_inner_size <- pp_discrete_plots %>%
  filter(season == "Summer") %>%
  filter(region == "Inner-shelf") %>%
  filter(size != "total") %>%
  filter(size != "less20")


#Add a depth group info
winter_inner_size <- winter_inner_size %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup()

#add depth group for summer
#Add a depth group info
summer_inner_size <- summer_inner_size %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup()

#need to fix the L1.2 data to be D2 instead of D1
winter_inner_size  <- winter_inner_size %>%
  mutate(depth_ID = ifelse(cruise == "EN661" & station == "L1.2" & depth >2, "2", depth_ID))


winter_inner_avg_size <- winter_inner_size %>%
  filter(!(cruise == "AT46"& cast == "17")) %>%
  group_by(depth_ID, size) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal Average") %>%
  mutate(season = "Winter")

winter_inner_avg_size$depth_ID <- as.numeric(winter_inner_avg_size$depth_ID)

winter_inner_anam_size <-  winter_inner_size %>%
  filter(cruise == "AT46"& cast == "17") %>%
  mutate(type = "L2 in 2022") %>%
  select(depth_ID, mean_npp, size, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Winter")

winter_inner_anam_size$depth_ID <- as.numeric(winter_inner_anam_size$depth_ID )

#now for summer
summer_inner_avg_size <- summer_inner_size %>%
  filter(!(cruise == "EN687"& cast == "2")) %>%
  group_by(depth_ID,size) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal Average") %>%
  mutate(season = "Summer")

summer_inner_anam_size <-  summer_inner_size %>%
  filter(cruise == "EN687"& cast == "2") %>%
  mutate(type = "L2 in 2022") %>%
  select(depth_ID, mean_npp, size, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Summer")

anam_joined_size <- rbind(winter_inner_avg_size,winter_inner_anam_size, summer_inner_avg_size ,summer_inner_anam_size)

anam_joined_size$size <-  factor(anam_joined_size$size, levels = c("less5", "between520", "greater20"))

anam_joined_size$depth_ID <- factor(anam_joined_size$depth_ID, levels = c(3, 2, 1), labels = c("Surface", "Mid", "Deep"))

anam_joined_size$season <- factor(anam_joined_size$season, levels = c("Winter", "Summer"))
anam_joined_size$type <- factor(anam_joined_size$type, levels = c( "Seasonal Average", "L2 in 2022"))

anam_joined_size <- anam_joined_size %>%
  arrange(type, season, size, depth_ID)

#save anam_joined_size as csv
write_csv(anam_joined_size, "Manuscript/Supplemental_Table5.csv")


anam_joined_size$season <- factor(anam_joined_size$season, levels = c("Winter", "Summer"))

anomaly_discrete_sizes <- ggplot(anam_joined_size, aes(y = type, x = mean, fill = season, alpha = interaction(as.factor(size), season))) +
  geom_bar(color = "black", stat = "identity", position = "stack") +
  plot_theme +
  xlab(expression(bold("NPP"~(mgC~m^-3~d^-1)))) +  # Two lines in ylab
  scale_fill_manual(name = "Season", values = c("#0072B5", "#E18726")) +
  scale_alpha_manual(
    name = "Size",
    values = c("less5.Summer" = 0.1, "between520.Summer" = 0.55, "greater20.Summer" = 1,
               "less5.Winter" = 0.1, "between520.Winter" = 0.55, "greater20.Winter" = 1),
    labels = c("<5 µm, Winter", "5-20 µm, Winter", ">20 µm, Winter","<5 µm, Summer", "5-20 µm, Summer", ">20 µm, Summer")
  ) +
  facet_grid(depth_ID ~ season) +
  scale_x_continuous(limits = c(0, 220), expand = c(0,0) ) +
  theme(strip.text.x = element_text(size = 10)) +
  theme(strip.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_blank())+
  theme(legend.position = "top") +  # Placing legend on top
  guides(
    fill = guide_legend(title = "Season"),
    alpha = guide_legend(
      title = "Size",
      override.aes = list(fill = c("#0072B5", "#E18726"),
                          linetype =rep("solid", 6))
    )
  )

anomaly_discrete_sizes

#save size pot
ggsave('Manuscript/anomaly_discrete_sizes.pdf', plot =anomaly_discrete_sizes , width = 5, height =4, units ="in", dpi = 300)

### SUPPLEMENTAL TABLE 10 #####

anom_total_winter <- pp_discrete_plots %>%
  filter(season == "Winter") %>%
  filter(region == "Inner-shelf") %>%
  filter(size == "total")


anom_total_summer <- pp_discrete_plots %>%
  filter(season == "Summer") %>%
  filter(region == "Inner-shelf") %>%
  filter(size == "total")

#Add a depth group info
anom_total_winter  <- anom_total_winter  %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup()

#add depth group for summer
#Add a depth group info
anom_total_summer <- anom_total_summer  %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup()

#need to fix the L1.2 data to be D2 instead of D1
anom_total_winter  <-anom_total_winter %>%
  mutate(depth_ID = ifelse(cruise == "EN661" & station == "L1.2" & depth >2, "2", depth_ID))


winter_inner_avg_total <-anom_total_winter   %>%
  filter(!(cruise == "AT46"& cast == "17")) %>%
  group_by(depth_ID, size) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal Average") %>%
  mutate(season = "Winter")

winter_inner_avg_total$depth_ID <- as.numeric(winter_inner_avg_total$depth_ID)

winter_inner_anam_total <- anom_total_winter  %>%
  filter(cruise == "AT46"& cast == "17") %>%
  mutate(type = "L2 in 2022") %>%
  select(depth_ID, mean_npp, size, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Winter")

winter_inner_anam_total $depth_ID <- as.numeric(winter_inner_anam_total $depth_ID )

#now for summer
summer_inner_avg_total <- anom_total_summer %>%
  filter(!(cruise == "EN687"& cast == "2")) %>%
  group_by(depth_ID,size) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal Average") %>%
  mutate(season = "Summer")

summer_inner_anam_total <-   anom_total_summer %>%
  filter(cruise == "EN687"& cast == "2") %>%
  mutate(type = "L2 in 2022") %>%
  select(depth_ID, mean_npp, size, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Summer")

anam_joined_total <- rbind(winter_inner_avg_total,winter_inner_anam_total, summer_inner_avg_total ,summer_inner_anam_total)



#Now do the significant tests
winter_inner <- pp_discrete_plots %>%
  filter(season == "Winter") %>%
  filter(region == "Inner-shelf") %>%
  filter(size == "total") 


summer_inner <- pp_discrete_plots %>%
  filter(season == "Summer") %>%
  filter(region == "Inner-shelf") %>%
  filter(size == "total")

#Add a depth group info
winter_inner <- winter_inner %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup() %>%
  mutate(depth_ID = ifelse(cruise == "EN661" & station == "L1.2" & depth >2, "2", depth_ID))

shapiro.test(log(winter_inner$mean_npp))
shapiro.test(log(summer_inner$mean_npp))

#add depth group for summer
#Add a depth group info
summer_inner <- summer_inner %>%
  group_by(cruise, cast, station) %>%
  mutate(depth_ID = case_when(
    depth == max(depth) ~ 1,
    depth == median(depth) ~ 2,
    depth == min(depth) ~ 3,
    TRUE ~ NA_integer_
  )) %>%
  ungroup()

winter_inner_avg <- winter_inner %>%
  filter(!(cruise == "AT46"& cast == "17")) %>%
  group_by(depth_ID) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal average") %>%
  mutate(season = "Winter")


winter_inner_anam <-  winter_inner %>%
  filter(cruise == "AT46"& cast == "17") %>%
  mutate(type = "Winter Anomaly") %>%
  select(depth_ID, mean_npp, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Winter")

#now for summer
summer_inner_avg <- summer_inner %>%
  filter(!(cruise == "EN687"& cast == "2")) %>%
  group_by(depth_ID) %>%
  summarise(mean = mean(mean_npp)) %>%
  mutate(type = "Seasonal average") %>%
  mutate(season = "Summer")


summer_inner_anam <-  summer_inner %>%
  filter(cruise == "EN687"& cast == "2") %>%
  mutate(type = "Summer Anomaly") %>%
  select(depth_ID, mean_npp, type ) %>%
  rename("mean" = mean_npp) %>%
  mutate(season = "Summer")


#test for significant differences in NPP rates
#first up is winter deep
winter_test_deep_noamon <- winter_inner %>%
  filter(depth_ID == "1") %>%
  filter(!(cruise == "AT46"& cast == "17"))

winter_test_deep <- winter_inner %>%
  filter(cruise == "AT46"& cast == "17") %>%
  filter(depth_ID == "1")

t.test(log(winter_test_deep_noamon$mean_npp+1),mu = log(winter_test_deep$mean_npp+1), alternative = "less") #p value is 0.1073


#then winter mid
winter_test_mid_noamon <- winter_inner %>%
  filter(depth_ID == "2") %>%
  filter(!(cruise == "AT46"& cast == "17"))

winter_test_mid <- winter_inner %>%
  filter(cruise == "AT46"& cast == "17") %>%
  filter(depth_ID == "2")

t.test(log10(winter_test_mid_noamon$mean_npp+1),mu = log10(winter_test_mid$mean_npp), alternative = "less") #p value is 0.0072 for winter mid

#then winter surf
winter_test_surf_noamon <- winter_inner %>%
  filter(depth_ID == "3") %>%
  filter(!(cruise == "AT46"& cast == "17"))

winter_test_surf <- winter_inner %>%
  filter(cruise == "AT46"& cast == "17") %>%
  filter(depth_ID == "3")

t.test(log10(winter_test_surf_noamon$mean_npp+1),mu = log10(winter_test_surf$mean_npp), alternative = "less") #p value is 0.0006088


#summer differences
summer_test_deep_noamon <- summer_inner %>%
  filter(depth_ID == "1") %>%
  filter(!(cruise == "EN687"& cast == "2"))

summer_test_deep <- summer_inner %>%
  filter(cruise == "EN687"& cast == "2") %>%
  filter(depth_ID == "1")

t.test(log10(summer_test_deep_noamon$mean_npp+1),mu = log10(summer_test_deep$mean_npp), alternative = "less") #p value is 0.989


#then summer mid
summer_test_mid_noamon <- summer_inner %>%
  filter(depth_ID == "2") %>%
  filter(!(cruise == "EN687"& cast == "2"))

summer_test_mid <- summer_inner %>%
  filter(cruise == "EN687"& cast == "2") %>%
  filter(depth_ID == "2")

t.test(log10(summer_test_mid_noamon$mean_npp+1),mu = log10(summer_test_mid$mean_npp), alternative = "less") #p value is 0.0000839

#then summer surf
summer_test_surf_noamon <- summer_inner %>%
  filter(depth_ID == "3") %>%
  filter(!(cruise == "EN687"& cast == "2"))

summer_test_surf <- summer_inner %>%
  filter(cruise == "EN687"& cast == "2") %>%
  filter(depth_ID == "3")

t.test(log10(summer_test_surf_noamon$mean_npp+1),mu = log10(summer_test_surf$mean_npp), alternative = "less") #p value is 0.1434


#Now need to calculate the % difference for anomalously high NPP depths: surf and mid in winter and mid in summer
anom_depths <- anam_joined_size %>%
  filter(type == "L2 in 2022") %>%
  filter(depth_ID == 'Mid' & season == "Winter"| depth_ID == "Surface" & season == "Winter"| depth_ID == "Mid" & season == "Summer")

#>20 and <5
#74.453 & 0.221 -- dif = 198.8%
#100.34 & 3.94 -- dif = 185%
#102.86 & 30.92 -- dif = 108%

#Avereage
mean(c(198, 185, 108)) #164%


# >20 and 5-20
#74.453 & 18.794 -- dif = 119%
#100.34 & 30.548 -- dif = 107%
#102.86 & 56.029 -- dif = 59%

#Average
mean(c(119, 107, 59))

######## END OF SUPPLEMENTAL FIGURE 7 ##########

######### FIGURE 5: SIZE FRACTIONATED % NPP and %CHLA ACROSS THE SHELF #########
#first change the size descriptions
pp_int_figs$size[pp_int_figs$size=="pp_GFF"] <- "total"
pp_int_figs$size[pp_int_figs$size=="pp_less5"] <- "less5"
pp_int_figs$size[pp_int_figs$size=="pp_between5_20"] <- "between520"
pp_int_figs$size[pp_int_figs$size=="pp_greater_20"] <- "greater20"

pp_int_sizes <- pp_int_figs %>%
  filter(size != "total") %>%
  filter(!(cruise == "EN668" & cast == "20"))

pp_int_sizes_sum <- pp_int_sizes %>%
  select(cruise, cast,size, int_pp) %>%
  pivot_wider(id_cols = cruise:cast,
              values_from = int_pp,
              names_from = size) %>%
  #mutate(total_pp = less5+greater20+between520) %>%
  mutate(total_pp = less5+greater20+between520) %>%
  select(cruise, cast,total_pp)

pp_int_sizes_percent <- pp_int_sizes %>%
  left_join(pp_int_sizes_sum, by = c("cruise", "cast")) 

#Calculate %s
pp_int_sizes_percent<- pp_int_sizes_percent %>%
  mutate(percent_pp = (int_pp/total_pp)*100)

#calculate the percent contribution
pp_int_percent <- pp_int_figs%>%
  filter(size != "total") %>%
  filter(size != "greater5") %>%
  select(cruise, cast, size, season, year, region, nearest_station, int_pp) %>%
  pivot_wider(id_cols = cruise:nearest_station,
              names_from = "size",
              values_from = "int_pp") %>%
  mutate(total = less5+between520+greater20) %>%
  mutate(per_less5 = less5/total*100) %>%
  mutate(per_bet5_20 = between520/total*100) %>%
  mutate(per_great20 = greater20/total*100) %>%
  mutate(per_check = per_less5+per_bet5_20+per_great20) %>%
  select(-less5, -greater20, -between520) %>%
  pivot_longer(cols = per_less5:per_great20,
               values_to = "percent_pp",
               names_to = "size") %>%
  filter(nearest_station != "d6a") %>%
  filter(nearest_station != "L11")

#check that size %s match 
pp_int_sizes_percent_totalcheck <- pp_int_percent %>%
  group_by(cruise, cast) %>%
  summarise(sum = sum(percent_pp))

#summarize by percent pp
pp_percent_summarised <-  pp_int_percent %>%
  dplyr::group_by(season, size, region) %>%
  dplyr::summarise(mean_pp_percent = mean(percent_pp, na.rm = TRUE), sd_pp = sd(percent_pp, na.rm = TRUE))

#calculate avg of middle size class
percent5_20 <- pp_int_percent %>%
  filter(size == "per_bet5_20")
mean(percent5_20 $percent_pp)


#make the percents a factor
pp_int_percent $size <- factor(pp_int_percent $size, levels = c("per_less5","per_bet5_20", "per_great20"))


pp_int_percent$region[pp_int_percent $region == "Inner-shelf"] <-  "Inner"
pp_int_percent $region[pp_int_percent $region == "Mid-shelf"] <-  "Mid"
pp_int_percent $region[pp_int_percent $region == "Outer-shelf"] <-  "Outer"

levels(pp_int_percent $size) <- c("less5", "between520","greater20")

pp_percent_summarised$season <- factor(pp_percent_summarised$season, levels = c("Winter", "Summer"))
pp_int_percent$season <- factor(pp_int_percent$season, levels = c("Winter", "Summer"))

#pp by size and season and region
pp_int_season_sizes<- ggplot(pp_int_percent, aes(x = factor(region), y =percent_pp, fill = season, alpha = interaction(as.factor(size), season)))+
  geom_boxplot(color = "black", position = position_dodge(width = 0.9))+
  # geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.3)+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  facet_wrap(~season, nrow = 2)+
  plot_theme+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12))+
  scale_y_continuous(limits = c(0,100))+
  theme(strip.text.x = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))+
  ylab("Contribution to NPP (%)")+
  xlab("Shelf Region")+
  scale_alpha_manual(
    name = "Size",
    values = c("less5.Summer" = 0.1, "between520.Summer" = 0.55, "greater20.Summer" = 1,
               "less5.Winter" = 0.1, "between520.Winter" = 0.55, "greater20.Winter" = 1))  +
  guides(
    fill = guide_legend(title = "Season"),
    alpha = guide_legend(
      title = "Size",
      override.aes = list(fill = c("#0072B5", "#E18726"),
                          linetype =rep("solid", 6))
    ))
pp_int_season_sizes

#Now need to get percent chl! 
chl_int <- read_csv("Processed_data_outputs/integrated_chl_fromEDI_0524.csv")
stn_info <- read_csv("Processed_data_outputs/station_info.csv")
chl_int <- chl_int%>%
  left_join(stn_info, by = c("cruise", "cast"))

chl_int_sizes <- chl_int %>%
  filter(size != "total") %>%
  filter(!(cruise == "EN668" & cast == "20")) %>%
  #filter(!(cruise == "EN661" & cast == "13")) %>%
  #filter(!(cruise == "AT46" & cast == "15")) %>%
  filter(nearest_station != "L11")


chl_int_sizes_sum <- chl_int_sizes %>%
  select(cruise, cast,size, int_chl) %>%
  pivot_wider(id_cols = cruise:cast,
              values_from = int_chl,
              names_from = size) %>%
  mutate(total_chl = less5+greater20+between520) %>%
  select(cruise, cast,total_chl)

chl_int_sizes_percent <- chl_int_sizes %>%
  left_join(chl_int_sizes_sum, by = c("cruise", "cast")) 

#Calculate %s
chl_int_sizes_percent<- chl_int_sizes_percent %>%
  mutate(percent_chl = (int_chl/total_chl)*100)

chl_int_sizes_percent$season <-  with(chl_int_sizes_percent,
                                      ifelse(cruise == "EN644", "Summer",
                                             ifelse(cruise == "EN655", "Summer",
                                                    ifelse(cruise == "EN649", "Winter", 
                                                           ifelse(cruise == "EN661", "Winter",
                                                                  ifelse(cruise == "EN668", "Summer",
                                                                         ifelse(cruise == "AT46", "Winter", ifelse(cruise == "EN687", "Summer",NA))))))))

###Plot of percent contribution across the shelf (with distances)
chl_int_sizes_percent$size <- factor(chl_int_sizes_percent$size, levels = c("less5", "between520" , "greater20"))

chl_int_sizes_percent$season <- factor(chl_int_sizes_percent$season, levels = c("Winter", "Summer"))

chl_int_sizes_percent <- chl_int_sizes_percent %>%
  inner_join(pp_stns, by = c("cruise","cast"))

chl_int_sizes_percent$region <- with(chl_int_sizes_percent,
                                     ifelse(nearest_station %in% c("L1", "L2", "L1.2"), "Inner-shelf",
                                            ifelse(nearest_station %in% c("L4", "L6", "d6a"), "Mid-shelf",
                                                   ifelse(nearest_station %in%  c('L8', "L10", "L11"), "Outer-shelf", NA)))) 

#do same summry for chl
# chl_int_sizes_percent_region <- chl_int_sizes_percent %>%
#   group_by(season, size, region) %>%
#   dplyr::summarise(mean_percent_chl= mean(percent_chl), sd_chl = sd(percent_chl))

chl_int_sizes_percent$region[chl_int_sizes_percent$region == "Inner-shelf"] <- "Inner"
chl_int_sizes_percent$region[chl_int_sizes_percent$region == "Mid-shelf"] <- "Mid"
chl_int_sizes_percent$region[chl_int_sizes_percent$region == "Outer-shelf"] <- "Outer"

percent_chl_region <- ggplot(chl_int_sizes_percent , aes(x = factor(region), y =percent_chl, fill = season, alpha = interaction(as.factor(size), season)))+
  plot_theme+
  geom_boxplot(color = "black", position = position_dodge(width = 0.9))+
  # geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.3)+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12))+
  facet_wrap(~season, nrow = 2)+
  #geom_bar(color = "black", stat = "identity", position = position_dodge(width = 0.9))+
  ylab("Contribution to Chl (%)")+
  xlab("Shelf Region")+
  #geom_errorbar(aes(ymin=mean_percent_chl, ymax=mean_percent_chl+sd), width = 0.2, position = position_dodge(width = 0.9))+
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0,100))+
  theme(strip.text.x = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))+
  scale_alpha_manual(
    name = "Size",
    values = c("less5.Summer" = 0.1, "between520.Summer" = 0.55, "greater20.Summer" = 1,
               "less5.Winter" = 0.1, "between520.Winter" = 0.55, "greater20.Winter" = 1)) +
  guides(
    fill = guide_legend(title = "Season"),
    alpha = guide_legend(
      title = "Size",
      override.aes = list(fill = c("#0072B5", "#E18726"),
                          linetype =rep("solid", 6))
    ))
percent_chl_region

#Now join two plots together
percent_sizes_cross <- pp_int_season_sizes + percent_chl_region +
  plot_layout(guides = "collect") & theme(legend.position = 'top')
percent_sizes_cross

ggsave('Manuscript/percentages_across.pdf', plot =  percent_sizes_cross, scale = 1, width = 4, height = 5, units ="in", dpi = 300)

#Now need to do significance testing!
#Do the stats for % NPP first across the shelf
### winter <5
pp_int_percent_winter_5 <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(size == "less5")
kruskal.test(percent_pp~region, data = pp_int_percent_winter_5 ) #no, p = 0.06

#winter 5-20
pp_int_percent_winter_520 <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(size == "between520")

kruskal.test(percent_pp~region, data = pp_int_percent_winter_520) #no, p = 0.39

#winter >20
pp_int_percent_winter_20 <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(size == "greater20")

kruskal.test(percent_pp~region, data = pp_int_percent_winter_20) #no, p = 0.19


#Summer <5
pp_int_percent_summer_5 <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(size == "less5")

kruskal.test(percent_pp~region, data = pp_int_percent_summer_5) #no, p = 0.060

#summer 5-20
pp_int_percent_summer_520 <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(size == "between520")

kruskal.test(percent_pp~region, data = pp_int_percent_summer_520) #no, p = 0.57

#summer >20
pp_int_percent_summer_20 <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(size == "greater20")

kruskal.test(percent_pp~region, data = pp_int_percent_summer_20) #no, p = 0.50

#no difference across the shelf in either size/season combo
#So now test by size within a given region/season
# ### winter inner
pp_int_percent_winter_inner <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Inner-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_winter_inner) #yes, p = 0.00038

dunnTest(percent_pp~ size,
         data=pp_int_percent_winter_inner   ,
         method="bonferroni") #Only greater 20 and less 5 are dif

#winter mid
pp_int_percent_winter_mid <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Mid-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_winter_mid) #yes, p = 0.007

dunnTest(percent_pp~ size,
         data=pp_int_percent_winter_mid   ,
         method="bonferroni") #greater than 5 and > 20 = dif (0.011) and >20 and 5-20 (0.045

#winter outer
pp_int_percent_winter_outer <- pp_int_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Outer-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_winter_outer) #no, p = 0.95

##Now summer within region
#summer inner
pp_int_percent_summer_inner <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Inner-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_summer_inner) #no, p = 0.53

#summer mid
pp_int_percent_summer_mid <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Mid-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_summer_mid) #no, p = 0.16

#summer outer
pp_int_percent_summer_outer <- pp_int_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Outer-shelf")

kruskal.test(percent_pp~size, data = pp_int_percent_summer_outer) #no, p = 0.25

### stat testing for chl-a
### across shelf first
### winter <5
chl_int_sizes_percent_winter_5 <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(size == "less5")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_winter_5) #no, p = 0.09

#winter 5-20
chl_int_sizes_percent_winter_520 <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(size == "between520")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_winter_520) #no, p = 0.5092

#winter >20
chl_int_sizes_percent_winter_20 <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(size == "greater20")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_winter_20) #no, p = 0.0814

#Summer <5
chl_int_sizes_percent_summer_5 <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(size == "less5")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_summer_5) #no, p = 0.15

#summer 5-20
chl_int_sizes_percent_summer_520 <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(size == "between520")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_summer_520) #no, p = 0.57

#summer >20
chl_int_sizes_percent_summer_20 <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(size == "greater20")

kruskal.test(percent_chl~region, data = chl_int_sizes_percent_summer_20) #no, p = 0.53

#no difference across the shelf in either size/season combo
#So now test by size within a given region/season
# ### winter inner
chl_int_sizes_percent_winter_inner <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Inner")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_winter_inner) #yes, p = 0.003321

dunnTest(percent_chl~ size,
         data=chl_int_sizes_percent_winter_inner  ,
         method="bonferroni") #between520 and greater20 are dif (0.007) and greater20 and less 5 = dif (0.014)

#winter mid
chl_int_sizes_percent_winter_mid <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Mid")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_winter_mid) #yes, p = 0.003

dunnTest(percent_chl~ size,
         data=chl_int_sizes_percent_winter_mid  ,
         method="bonferroni") #between520 and greater20 are dif (0.04) and greater20 and less 5 = dif (0.01)



#winter outer
chl_int_sizes_percent_winter_outer <- chl_int_sizes_percent %>%
  filter(season == "Winter") %>%
  filter(region == "Outer-shelf")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_winter_outer) #no, p = 0.099

##Now summer within region
#summer inner
chl_int_sizes_percent_summer_inner <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Inner")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_summer_inner) #no, p = 0.2199

#summer mid
chl_int_sizes_percent_summer_mid <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Mid")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_summer_mid) #yes, p = 0.0086


dunnTest(percent_chl~ size,
         data=chl_int_sizes_percent_summer_mid ,
         method="bonferroni") #between520 and less 5 are diff (0.008)
#summer outer
chl_int_sizes_percent_summer_outer <- chl_int_sizes_percent %>%
  filter(season == "Summer") %>%
  filter(region == "Outer")

kruskal.test(percent_chl~size, data = chl_int_sizes_percent_summer_outer) #no, p = 0.06646

#Plotting %NPP and %Chl-a
pp_int_percent$size <- factor(pp_int_percent$size, levels = c("per_less5", "per_bet5_20", "per_great20"))
levels(pp_int_percent$size)
levels(pp_int_percent$size) <- c("less5", "between520", "greater20")

chl_pp_percent <- pp_int_percent %>%
  select(cruise, cast, size, percent_pp) %>%
  left_join(chl_int_sizes_percent, by = c("cruise", "cast", "size"))


chl_pp_percent$season <- factor(chl_pp_percent$season, levels = c("Winter", "Summer"))

#graph of %NPP vs. %chl without summarized
chl_pp_percent_all <- ggplot(chl_pp_percent , aes(x = percent_chl, y =percent_pp, fill = size ))+
  #geom_errorbar(aes(xmax=mean_percent_chl+sd_chl, xmin = mean_percent_chl-sd_chl),width = 0.2)+
  # geom_errorbar(aes(ymax=mean_pp_percent+sd_pp, ymin = mean_pp_percent-sd_pp),width = 0.2)+
  geom_smooth(color = "black", method = "lm", formula = y ~ x + 0, se = FALSE, aes(group = size, linetype = size), size = 1) +
  geom_abline(slope = 1, color = "red3", size = 0.8)+
  geom_point(color = "black", size = 3, shape = 21) +  # Fill points by season
    # Add separate regression lines for each size
  # scale_shape_manual(values = c(1,2,3))+
  #facet_wrap(~season)+
  plot_theme+
  ylim(c(0,100))+
  xlim(c(0,100))+
  guides(shape = guide_legend(title = "Size Class")) +
  scale_fill_manual(name = "Size Class", values = c("white", "grey", "black"))+
  theme(legend.position = c(0.1,0.9))+
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12))+
  #scale_shape_manual(values = c(21,22,23))+
  #scale_fill_manual(name = "Season", values = c("skyblue4","sienna4"))+
  scale_linetype_manual(name = "Size", values = c( "dotted", "twodash", "solid")) +  # Set line types for each size
  xlab("% Chlorophyll-a")+
  ylab("% Net Primary Production")+
  theme(legend.position = "top")
chl_pp_percent_all


# Combine the first two plots in a row
top_row <- pp_int_season_sizes + percent_chl_region

# Arrange top_row and chl_pp_percent_all vertically
percent_sizes_cross <- top_row / chl_pp_percent_all +
  plot_layout(guides = "collect") & theme(legend.position = 'top')

# Display the combined plot
percent_sizes_cross


ggsave('Manuscript/percentages_across.pdf', plot =  percent_sizes_cross, scale = 1, width = 3.5, height = 8, units ="in", dpi = 300)

ggsave('Manuscript/Submission/Revisions/FIGURE5.pdf', plot =chl_pp_percent_all , scale = 1, width =3.5, height = 3.5, units ="in", dpi = 300)
#Then copy and past the "chl_pp_percent_all" plot to the "percent_sizes_cross" plot so that the %NPP and %Chla are side by side and then the %NPP and the %chl plot is underneath

#linear regressions
greater20_per <- chl_pp_percent %>%
  filter(size == "greater20")

mod_20 <- lm(percent_pp~0+percent_chl, data =greater20_per )
summary(mod_20)

less5_per <- chl_pp_percent %>%
  filter(size == "less5")

mod_5 <- lm(percent_pp~0+percent_chl, data =less5_per)
summary(mod_5)

bet520_per <- chl_pp_percent %>%
  filter(size == "between520")

mod_520 <- lm(percent_pp~0+percent_chl, data =bet520_per)

summary(mod_520)

########## END OF FIGURE 5 ################# 

######### FIGURE 6: ASSIMILATION EFFICIENCY #########
#now join
int_pp_chl <- pp_int_figs %>%
  left_join(chl_int_plots, by = c("cruise", "cast", "size")) %>%
  filter(int_chl > 0) %>%
  mutate(Carbon = 50) %>%
  mutate(Carbon_W = 25) %>%
  mutate(Carbon_S = 75) %>%
  mutate(int_assim = int_pp/int_chl) %>%
  mutate(int_assim_50 = int_pp/(int_chl*Carbon)) %>%
  mutate(int_assim_25 = int_pp/(int_chl*Carbon_W)) %>%
  mutate(int_assim_75 = int_pp/(int_chl*Carbon_S)) %>%
  filter(nearest_station != "L11")


int_pp_chl$season <- factor(int_pp_chl$season, levels = c("Winter", "Summer"))
int_pp_chl$size <- factor(int_pp_chl$size, levels = c("total", "less5","between520", "greater20" ))

levels(int_pp_chl$size) <- c("total", "< 5","5-20", "> 20")

int_pp_chl$region[int_pp_chl$region == "Inner-shelf"] <-  "Inner"
int_pp_chl$region[int_pp_chl$region == "Mid-shelf"] <-  "Mid"
int_pp_chl$region[int_pp_chl$region == "Outer-shelf"] <-  "Outer"

int_assim_sizes <- int_pp_chl %>%
  filter(size != "total") 

#both seasons together
int_assim_plot<- ggplot(int_assim_sizes, aes(x = factor(size), y = int_assim, fill = season, alpha = size))+
  facet_wrap(~season)+
  plot_theme+
  geom_boxplot(color = "black", position = position_dodge(width = 0.9))+
  # geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.1)+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  ylab(expression(atop(bold("Assimilation Efficiency"), paste(~mgC~mgchl^-1~d^-1))))+
  theme(strip.text.x = element_text(size = 12))+
  coord_cartesian(ylim = c(0, 100))+
  xlab("Size Class (µm)")+
  theme(legend.position = "none")
int_assim_plot

ggsave('Manuscript/assim_efficiency.pdf', plot =int_assim_plot , scale = 1, width =5, height = 4, units ="in", dpi = 300)
#Then edit the points to be the right color  (and error bars) in affinity designer

#Now do significance testing 
#just the sizes for assim
assim_sizes_winter <-  int_pp_chl %>%
  filter(size != "total") %>%
  filter(season == "Winter")

#test for sig differences in assim in winter
kruskal.test(int_assim~size, data = assim_sizes_winter) ### sig at p = 0.0091

dunnTest(int_assim~ size,
         data=assim_sizes_winter,
         method="bonferroni") # < 5 and 5-20 = 0.01; 5-20 and >20 = 0.04

#test for sig differences in assim in summer
assim_sizes_summer <-  int_pp_chl %>%
  filter(size != "total") %>%
  filter(season == "Summer")

kruskal.test(int_assim~size, data = assim_sizes_summer) #p = 0.0079

dunnTest(int_assim~ size,
         data=assim_sizes_summer,
         method="bonferroni") # only the 5-20 and < 5 size class are different in summer

####### END OF FIGURE 6 #######

########## SUPPLEMENTAL FIGURE 10: CARBON-SPECIFIC NPP (USING SEASON-SPECIFIC C:CHL RATIOS) ###########
assim_winter <- int_pp_chl %>%
  filter(size != "total") %>%
  filter(season == "Winter") %>%
  select(season, size, int_assim_25) %>%
  rename(int_assim = int_assim_25)

assim_summer <-  int_pp_chl %>%
  filter(size != "total") %>%
  filter(season == "Summer") %>%
  select(season, size, int_assim_75) %>%
  rename(int_assim = int_assim_75)

assim_sizes_C_chl <- rbind(assim_winter,assim_summer) 

#plot of integrated assim effeciency for whole shelf by season
int_assim_plot_C_chl <- ggplot(assim_sizes_C_chl, aes(x = factor(size), y = int_assim, fill = season, alpha = size))+
  facet_wrap(~season)+
  plot_theme+
  geom_boxplot(color = "black", position = position_dodge(width = 0.9))+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.1)+
  #ylim(c(0,50))+
  # geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  ylab(expression(bold("Carbon-specific NPP"), paste("(", d^-1, ")")))+
  theme(strip.text.x = element_text(size = 12))+
  xlab("Size Class (µm)")+
  #coord_cartesian(ylim = c(0, 2.1))+
  theme(legend.position = "none")
#theme(axis.title.y = element_blank())
int_assim_plot_C_chl

ggsave('Manuscript/assim_efficiency_C_chl_season.pdf', plot =int_assim_plot_C_chl , scale = 1, width =5, height = 4, units ="in", dpi = 300)
#Then adjust error bars to be black in affinity designer

assim_sizes_C_chl_5 <- assim_sizes_C_chl %>%
  filter(size == "< 5")

kruskal.test(int_assim~season, data = assim_sizes_C_chl_5)

assim_sizes_C_chl_520 <- assim_sizes_C_chl %>%
  filter(size == "5-20")

kruskal.test(int_assim~season, data = assim_sizes_C_chl_520)

assim_sizes_C_chl_20 <- assim_sizes_C_chl %>%
  filter(size == "> 20")

kruskal.test(int_assim~season, data = assim_sizes_C_chl_20)

########## END OF SUPPLEMENTAL FIGURE 10 ###########

########## SUPPLEMENTAL FIGURE 11: CHL-SPECIFIC NPP ACROSS THE SHELF ###########
int_assim_plot_season_region <- ggplot(int_assim_sizes, aes(x = factor(region), y = int_assim, fill = season, alpha = size))+
  facet_wrap(~season)+
  plot_theme+
  #ylim(c(0,200))+
  geom_boxplot(color = "black", position = position_dodge(width = 0.9))+
  stat_boxplot(geom='errorbar', position = position_dodge(preserve = "single", width = 0.9), width = 0.2)+
  geom_point(stat = "summary", fun = "mean", shape = 21, size = 2, color = "black", position = position_dodge(width = 0.9)) +
  # geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  ylab(expression(atop(bold("Chl-specific NPP"), paste(~mgC~mgchl^-1~d^-1))))+
  theme(strip.text.x = element_text(size = 12))+
  xlab("Shelf Region")+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position = "top")
int_assim_plot_season_region

ggsave('Manuscript/SUPP_assim_efficiency_sizes_across.pdf', plot =int_assim_plot_season_region , scale = 1, width =6, height = 4, units ="in", dpi = 300)
#Then adjust error bar color in affinity designer

#test for sig differences in assim efficiency across the shelf
assim_sizes_winter_5 <- assim_sizes_winter %>%
  filter(size == "< 5")
kruskal.test(int_assim~region, data = assim_sizes_winter_5) #yes, p = 0.03

dunnTest(int_assim ~ region,
         data=assim_sizes_winter_5,
         method="bonferroni") #only dif is between inner and mid: 0.0264

# mod <- aov(int_assim~region, data = assim_sizes_winter_5)
# summary(mod)
assim_sizes_winter_520 <- assim_sizes_winter %>%
  filter(size == "5-20")
kruskal.test(int_assim~region, data = assim_sizes_winter_520) #nope, p = 0.4826

assim_sizes_winter_20 <- assim_sizes_winter %>%
  filter(size == "> 20")
kruskal.test(int_assim~region, data = assim_sizes_winter_20) #nope, p = 0.20


#Now tests for summer
assim_sizes_summer_5 <- assim_sizes_summer %>%
  filter(size == "< 5")
kruskal.test(int_assim~region, data = assim_sizes_summer_5) #nope, p = 0.76

assim_sizes_summer_520 <- assim_sizes_summer %>%
  filter(size == "5-20")
kruskal.test(int_assim~region, data = assim_sizes_summer_520) #nope, p = 0.53

assim_sizes_summer_20 <- assim_sizes_summer%>%
  filter(size == "> 20")
kruskal.test(int_assim~region, data = assim_sizes_summer_20) #nope, p = 0.3326
######## END OF SUPPLEMENTAL FIGURE 11 ##########

######### FIGURE 7: RDA OF ENVIRONMENTAL DATA AND NPP ON THE SHELF  ######
#read in nut file 
nut_int <- read_csv("Processed_data_outputs/integrated_nutrients_033024_euphotic.csv")

#need to get temperature data
temp_data <- read_csv("Processed_data_outputs/discrete_envi_data_all_bottles.csv")
temp_data <- temp_data %>%
  ungroup() %>%
  select(cruise,cast,niskin, mean_temp, mean_sal, depth)


#only want to average the temp in the euphotic zone (where we have the integrated data, so pull in cast, niskin)
depths_temp <-read_csv("Processed_data_outputs/pp_discrete_averaged_temp_filter_011924.csv")

depths_temp <- depths_temp %>%
  filter(station != "L11") 

depths_temp <- depths_temp %>%
  ungroup() %>%
  select(cruise, cast, station, niskin, depth) %>%
  unique()

euphotic_depth <- depths_temp %>%
  group_by(cruise, cast, station) %>%
  summarise(Zeu = max(depth))

temp_data_difparam <- depths_temp %>%
  left_join(temp_data,by = c("cruise", "cast", "niskin")) %>%
  group_by(cruise, cast) %>%
  summarize(temp_diff = diff(mean_temp[order(niskin)][c(1, length(niskin))]))

#Read in MLD info
MLD <- read_csv("npp_mld_stations.csv")
MLD <- MLD %>%
  select(cruise,cast,MLD_dens)

#combine mld, DCM and temp paramdataframe
temp_data_difparam <- temp_data_difparam %>%
  left_join(MLD, by = c("cruise","cast")) %>%
  left_join(euphotic_depth, by = c("cruise", "cast"))

#make integrated nutrients wide
nut_int_wide <- nut_int %>%
  pivot_wider(id_cols = 1:2,
              names_from = "nutrient",
              values_from = "int_nut") %>%
  filter(!(cruise == "EN668"&cast == 20))

temp_data_difparam  <- temp_data_difparam   %>%
  left_join(nut_int_wide, by = c("cruise", "cast"))

#We don't have nutrients for AT46 cast 15 so this is the NA value in the nutrient datafrme
temp_data_difparam <- temp_data_difparam %>%
  filter(!is.na(nitrate_nitrite))

#Now time to get the RDA ready to run
pp_int_rda <- pp_int_figs 

pp_int_rda<-pp_int_rda %>%
  filter(cruise %in% c("AT46", "EN649", "EN661", "EN655", "EN687", "EN668")) %>%
  filter(nearest_station != "L11") 

pp_int_rda$nearest_station[pp_int_rda$nearest_station == "L1.2"] <- "L1"

#read in incubation light data
inc_light <- read_csv("incubation_light.csv")

pp_cruise_stn_info <- pp_int_rda %>%
  select(cruise, cast, nearest_station, region, season, year) %>%
  unique()

#add cast info to incubation light
inc_light <- inc_light %>%
  rename(nearest_station = station) %>%
  left_join(pp_cruise_stn_info, by = c("cruise","nearest_station","cast" ))

#read in envi data 
envi_int <- temp_data_difparam %>%
  select(-station) %>%
  left_join(inc_light, by = c("cruise", "cast")) 

#read in buoy
buoy <- read_csv("buoyancy_freq.csv")

envi_int <- envi_int %>%
  left_join(buoy, by = c("cruise","cast"))

########## SUPPLEMENTAL TABLE 7: ENVIRONMENTAL DATA ############
#get just PAR, MLD, and Nitrate for the supplemental table
envi_supp_table <- envi_int %>%
  select(cruise, cast,nearest_station, Avg_PAR_inc, MLD_dens, nitrate_nitrite)

envi_supp_table $region <- with(envi_supp_table ,
                                ifelse(nearest_station %in% c("L1", "L2", "L1.2"), "Inner-shelf",
                                       ifelse(nearest_station %in% c("L4", "L6", "d6a"), "Mid-shelf",
                                              ifelse(nearest_station %in%  c('L8', "L10", "L11"), "Outer-shelf", NA)))) 

envi_supp_table $season <-  with(envi_supp_table ,
                                 ifelse(cruise == "EN644", "Summer",
                                        ifelse(cruise == "EN655", "Summer",
                                               ifelse(cruise == "EN649", "Winter", 
                                                      ifelse(cruise == "EN661", "Winter",
                                                             ifelse(cruise == "EN668", "Summer",
                                                                    ifelse(cruise == "AT46", "Winter", ifelse(cruise == "EN687", "Summer",NA))))))))

envi_supp_table_avg <- envi_supp_table %>%
  pivot_longer(cols = Avg_PAR_inc:nitrate_nitrite,
               names_to = "envi",
               values_to = "value") %>%
  group_by(season,region,envi) %>%
  summarise(min_v = min(value), max_v = max(value), mean_v = mean(value), sd_v = sd(value))

###### END OF SUPPLEMENTAL TABLE 7 #########

#join envi and pp int
pp_int_rda <- pp_int_rda %>%
  select(-region,-nearest_station,-year, -season) %>%
  left_join(envi_int, by = c("cruise", "cast")) %>%
  mutate(zeu_PAR = Avg_PAR_inc*Zeu)

#use this for the correlation matrices in supplement (SEE BELOW)
pp_int_cor_matrix <- pp_int_rda 

#make wide format so sizes are wide
pp_int_rda_wide <- pp_int_rda %>%
  filter(cruise != "EN644") %>%
  #filter(!(cruise == "EN661" & cast == 13)) %>%
  select(cruise, cast, region, season, year, nearest_station, MLD_dens,Avg_PAR_inc, nitrate_nitrite,phosphate,silicate,ammonium,temp_diff, Zeu, zeu_PAR,max_N2, size,int_pp) %>%
  pivot_wider(id_cols = cruise:max_N2,
              values_from = "int_pp",
              names_from = "size") %>%
  filter(!is.na(Avg_PAR_inc))


#function for normalizing env data
z_score_norm <- function(x) {
  (x - mean(x)) / sd(x)
}
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#noralize environmental data
pp_int_envi <- as.data.frame(lapply(pp_int_rda_wide[7:16],z_score_norm ))

#get station info
pp_cruise_stn_info <- pp_int_rda_wide %>%
  select(cruise, cast, nearest_station, region, season, year) %>%
  unique()

pp_cruise_stn_info <- pp_cruise_stn_info %>%
  unite(col = "group_ID", cruise:year, sep = "_")


#testing the correlations to make sure envi data aren't correlated
temp_light <- cor.test(pp_int_envi$temp_diff,pp_int_envi$Avg_PAR_inc) #0.70, so yes
temp_lightmetric <- cor.test(pp_int_envi$temp_diff,pp_int_envi$zeu_PAR) #0.50, so no
light_phos <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$phosphate) #-0.31
light_nit <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$nitrate_nitrite) #-0.29
light_sil <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$silicate) #-0.13
light_ammon <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$ammonium) #-0.37
nit_ammon <- cor.test(pp_int_envi$nitrate_nitrite,pp_int_envi$ammonium) #0.465 
nit_sil <- cor.test(pp_int_envi$nitrate_nitrite,pp_int_envi$silicate) #0.84 use nitrate instead 
nit_phos <- cor.test(pp_int_envi$nitrate_nitrite,pp_int_envi$phosphate) #0.90 use nitrate instead
MLD_dens_lightmetric <- cor.test(pp_int_envi$zeu_PAR,pp_int_envi$MLD_dens) #-0.166 so no
MLD_dens_light <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$MLD_dens) #-0.48 so no 
MLD_dens_temp <- cor.test(pp_int_envi$temp_diff,pp_int_envi$MLD_dens) #no, -0.53
DCM_lightmetric <- cor.test(pp_int_envi$zeu_PAR,pp_int_envi$DCM)  #0.41 so no
buoy_light <- cor.test(pp_int_envi$Avg_PAR_inc ,pp_int_envi$max_N2)  #0.86 so yes
zeu_light <- cor.test(pp_int_envi$Avg_PAR_inc ,pp_int_envi$Zeu)  #-0.05
zeu_nitrate <- cor.test(pp_int_envi$nitrate_nitrite ,pp_int_envi$Zeu)  #0.769 usse nitrate instead
nitrate_MLDdens <- cor.test(pp_int_envi$MLD_dens,pp_int_envi$nitrate_nitrite)  #0.56 so no
DCM_light <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$DCM)  #0.186
DCM_Zeu <- cor.test(pp_int_envi$Zeu,pp_int_envi$DCM) 
MLD_Zeu <- cor.test(pp_int_envi$Zeu,pp_int_envi$MLD)  #0.38
tempdiff_Zeu <- cor.test(pp_int_envi$temp_diff,pp_int_envi$Zeu)  
nitrate_Zeu <- cor.test(pp_int_envi$Zeu,pp_int_envi$nitrate_nitrite)  #0.73
nitrate_DCM <- cor.test(pp_int_envi$DCM,pp_int_envi$nitrate_nitrite)  #0.27
nitrate_lightmetric <- cor.test(pp_int_envi$zeu_PAR,pp_int_envi$nitrate_nitrite)  #0.31
light_lightmetric <- cor.test(pp_int_envi$Avg_PAR_inc,pp_int_envi$zeu_PAR)  #0.58


#Based on correlations of envi data: use nittrate, light and MLD, in RDA. Did not use DCM as this isn't really an environmental parameter

#Normalize env data
pp_int_envi$group_ID <- pp_cruise_stn_info$group_ID
pp_int_rda_wide$group_ID <-  pp_cruise_stn_info$group_ID

#make group_ID the rowname
pp_int_envi <- column_to_rownames(pp_int_envi, var = "group_ID")
pp_int_rda_wide <- column_to_rownames(pp_int_rda_wide, var = "group_ID")

#NPP rates go from 16-18
species_all <- pp_int_rda_wide[,18:20] #size rates go from 18-20

#log-transform values
species_all$less5 <- log(species_all$less5+1)
species_all$between520 <- log(species_all$between520+1)
species_all$greater20 <- log(species_all$greater20+1)

#run the rda
set.seed(123)
dbRDA_all <- rda(species_all ~nitrate_nitrite+ Avg_PAR_inc+ MLD_dens, pp_int_envi, scaling = 2)

#look at screeplot
screeplot(dbRDA_all)
#Get R2 values
dbRDA_R2_all <- RsquareAdj(dbRDA_all)$r.squared


plot(dbRDA_all, scaling = 2)

summary(dbRDA_all)

#Overall test of the significance of the analysis
anova(dbRDA_all, permutations = 999) 

#test axes for sig
anova(dbRDA_all, by = "axis", perm.max = 5000) 
#test for significance of env. variables
anova(dbRDA_all, by = "terms", permu = 5000) 

scores(dbRDA_all) #Getting the scores
scores_dbRDA_all <- scores(dbRDA_all, scaling = 2)

site_scores_all <- data.frame(scores_dbRDA_all$sites)

envi_scores <- data.frame(scores_dbRDA_all$biplot, scaling = 2)

site_scores_all$group_ID <- pp_cruise_stn_info$group_ID

site_scores_all <- site_scores_all %>%
  separate(group_ID, into = c("cruise", "cast", "station", "region", "season", "year"), sep = "_")

species_scores_forname_all <- as.data.frame(scores_dbRDA_all$species)

site_scores_all$season <- factor(site_scores_all$season, levels = c("Winter", "Summer"))

#rda with the segments
rda.plot_segment <- ggplot()+
  geom_segment(data = envi_scores, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),color = "black", arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data = species_scores_forname_all , aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "seagreen", arrow = arrow(length = unit(0.01, "npc")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  #coord_fixed(ratio = variance_percentage[2] / variance_percentage[1]) 
rda.plot_segment

#the complete plot
complete_rda<- ggplot()+
  geom_point(data = site_scores_all, aes(x=RDA1, y=RDA2, fill = season), pch = 21,size = 3)+
  plot_theme+
  geom_segment(data = envi_scores, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),color = "black", arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data = species_scores_forname_all , aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "seagreen", arrow = arrow(length = unit(0.01, "npc")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  labs(x = "RDA1 [39%]", y = "RDA2 [1.5%]")+
  # scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  theme(legend.position = c(0.885,0.9), legend.box.background = element_rect(colour = "black"))+
  geom_vline(xintercept=c(-0,0), linetype="dotted")+
  geom_hline(yintercept=c(-0,0), linetype="dotted")+
  annotate(geom = "text", x = -1.1, y = 0.35, label = "PAR", )+
  annotate(geom = "text", x = -0.55, y = -0.75, label = "NO[3] + NO[2]", parse = TRUE)+
  annotate(geom = "text", x = 0.4, y = -0.57, label = "MLD")+
  annotate(geom = "text", x = -1.8, y = -0.4, label = "< 5", color = "seagreen" , size = 3)+
  annotate(geom = "text", x = -0.33, y = 0.35, label = "5-20", color = "seagreen", size = 3 )+
  annotate(geom = "text", x = 0.001, y = 0.5, label = "> 20", color = "seagreen", size = 3 )+
  theme(axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 14))+
  theme(aspect.ratio=1)
complete_rda



ggsave("Manuscript/Submission/Revisions/FIGURE7.pdf", plot = complete_rda, width = 5, height =5, units = c("in"),
       dpi = 600)

######### END OF FIGURE 7 ############

######## SUPPLEMENTAL FIGURE 12: CORRELATION PLOTS #######
library(corrplot)
library(ggcorrplot)
library(rstatix)
pp_int_corr_wide <- pp_int_cor_matrix%>%
  mutate(log_pp = log10(int_pp)) %>%
  select(cruise, cast,region, season, year, temp_diff,MLD_dens,Zeu, ammonium, silicate, phosphate, nitrate_nitrite,Avg_PAR_inc, zeu_PAR,max_N2,  size, log_pp) %>%
  pivot_wider(id_cols = cruise:max_N2,
              names_from = size,
              values_from = log_pp)  %>%
  filter(!(is.na(temp_diff))) %>%
  select(-zeu_PAR)

colnames(pp_int_corr_wide )[6] <- "TDiff"
colnames(pp_int_corr_wide )[7] <- "MLD"
colnames(pp_int_corr_wide )[8] <- "Zeu"
colnames(pp_int_corr_wide )[9] <-  "Ammonium"
colnames(pp_int_corr_wide )[10] <- "Silicate"
colnames(pp_int_corr_wide )[11] <- "Phosphate"
colnames(pp_int_corr_wide )[12] <-"NO3+NO2"
colnames(pp_int_corr_wide )[13] <- "PAR"
colnames(pp_int_corr_wide )[14] <- "Buoyancy Frequency"
colnames(pp_int_corr_wide )[15] <- "Total NPP"
colnames(pp_int_corr_wide )[16] <- ">20 NPP"
colnames(pp_int_corr_wide )[17] <- "5-20 NPP"
colnames(pp_int_corr_wide )[18] <- "<5 NPP"

#Make correlation matrices matrices 
pp_int_corr_wide_winter <- pp_int_corr_wide %>%
  select(-TDiff) %>%
  filter(season == "Winter")

int_corr_winter_env<- as.data.frame(lapply( pp_int_corr_wide_winter[6:13], z_score_norm))
int_corr_winter_rates<- as.data.frame(  pp_int_corr_wide_winter [14:17])
int_corr_winter <- cbind( int_corr_winter_env,  int_corr_winter_rates)

mydata.cor_winter <- cor(int_corr_winter)

p.mat_winter <- cor_pmat(int_corr_winter)
p.mat_winter <- column_to_rownames(p.mat_winter, var = "rowname")
p.mat_winter <- as.matrix(p.mat_winter)


winter_corr_plot <- ggcorrplot(  mydata.cor_winter, type = "upper",  p.mat = p.mat_winter, lab = TRUE, insig = "pch", pch.col = "white", pch.cex = 2)

ggsave("Manuscript/winter_correlation.pdf", plot = winter_corr_plot, device = "pdf", width = 22, height = 17, units = c("cm"),
       dpi = 600)

#Now for summer
pp_int_corr_wide_summer <- pp_int_corr_wide %>%
  select(-TDiff) %>%
  filter(season == "Summer")

int_corr_summer_env<- as.data.frame(lapply( pp_int_corr_wide_summer[6:13], z_score_norm))
int_corr_summer_rates<- as.data.frame(  pp_int_corr_wide_summer [14:17])
int_corr_summer <- cbind( int_corr_summer_env,  int_corr_summer_rates)

mydata.cor_summer <- cor(int_corr_summer)

p.mat_summer <- cor_pmat(int_corr_summer)
p.mat_summer <- column_to_rownames(p.mat_summer, var = "rowname")
p.mat_summer <- as.matrix(p.mat_summer)

summer_corr_plot <- ggcorrplot(  mydata.cor_summer, type = "upper",  p.mat = p.mat_summer, lab = TRUE, insig = "pch", pch.col = "white", pch.cex = 2)

ggsave("Manuscript/summer_correlation.pdf", plot = summer_corr_plot, device = "pdf", width = 22, height = 17, units = c("cm"),
       dpi = 600)
#then in affinity designer, you need to copy and paste them into the same document and fix the correlation bar to put it at the top

######## END OF SUPPLEMENTAL FIGURE 12 ############

# NOW THE REMAINDER OF THE SUPPLEMENTAL FIGURES
#### SUPPLEMENTAL FIGURE 1: DARK CORRECTED NPP VS UNCORRECTED
pp_1 <- read_csv("NPP_data_package/For_EDI/npp_discrete_version1.csv")
pp_2 <- read_csv("NPP_data_package/For_EDI/npp_discrete_version2.csv")

pp_all <- rbind(pp_1, pp_2)

#select just "ambient_temp", not the bottles that were incubated as "experimental temps" on EN687
pp_all <- pp_all %>%
  filter(incub_type == "ambient_temp")

#Just need GFF, Dark sample dataframe
pp_dark <- pp_all %>%
  filter(depth_category == "Dark") 

#make wide to get the siz
pp_dark <- pp_dark %>%
  rename(dark_pp = 'npp_rate') %>%
  dplyr::select(-date_time_utc, -latitude, -longitude)

#join dark samples to pp_all to get dark as it's own column
pp_dark_GFF <- pp_all %>%
  dplyr::select(cruise, depth, cast, niskin, depth_category, filter_size, station, npp_rate) %>%
  filter(depth_category != "Dark") %>%
  left_join(pp_dark, by = c("cruise", "cast","station", "niskin", "filter_size", "depth")) %>%
  filter(!is.na(alternate_sample_category))

#Calculate corrected production
pp_dark_GFF <- pp_dark_GFF %>%
  mutate(pp_gff_corr = npp_rate - dark_pp) %>%
  mutate(ratio_dark_cor = pp_gff_corr/npp_rate) %>%
  filter(cruise != "EN644") %>% #not including these cruises in this paper
  filter(cruise != "AR39B")


dark_model <- lm(pp_gff_corr ~ npp_rate, data = pp_dark_GFF)
summary(dark_model)

#do a wilcox test between the dif rates (un-corrected and dark-corrected) because rates are not normally dist
dark_samples_stat <- pp_dark_GFF %>%
  dplyr::select(npp_rate, pp_gff_corr) %>%
  pivot_longer(cols = npp_rate:pp_gff_corr, 
               values_to = "pp",
               names_to = "dark_set")

wilcox.test(pp ~ dark_set, data = dark_samples_stat)

#Graph dark corrected int pp vs. uncorr with 1:1 line
dark_corrected_plot <- ggplot(pp_dark_GFF, aes(npp_rate, pp_gff_corr))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1)+
  plot_theme+
  ylim(c(0,50))+
  xlim(c(0,50))+
  annotate("text", x=7, y=48, label = "y = -0.27x + 0.99",size = 3)+
  annotate("text", x=5, y=45, label = as.character(expression("R^{2} == 0.99")), parse = T, size = 3)+
  annotate("text", x=5, y=42, label = "p < 0.05", size = 3)+
  xlab(expression(bold("Non-corrected NPP"~(mgC~m^-3~d^-1)))) + 
  ylab(expression(bold("Dark-corrected NPP"~(mgC~m^-3~d^-1))))   # Two lines in ylab
dark_corrected_plot

ggsave( "Manuscript/SUPP_dark_corrected_plot.pdf", plot = dark_corrected_plot, device = "pdf", width =10, height = 10, units ="cm",
        dpi = 300)

############ END OF SUPPLEMENAL FIGURE 1 ################

###### SUPPLEMENTAL FIGURE 2: INCUBATION TEMPERATURE DIFFERENCES ########
#Read in no temp filter data so can make the supplemental plot
pp_temp_for_difs <- read_csv("Processed_data_outputs/pp_discrete_averaged_notemp_filter.csv")

pp_temp_for_difs <- pp_temp_for_difs %>%
  filter(cruise %in% c("AT46", "EN649", "EN661", "EN655", "EN687", "EN668"))  %>%
  filter(nearest_station!="L11")


#plot temp dif and depth and fill in color by npp and shape by size --- supplemental figure--
pp_temp_for_difs_plot <- pp_temp_for_difs %>%
  filter(!is.na(mean_npp)) %>%
  filter(cruise != "EN644")

pp_temp_data_supplemental_fig$range <-  cut(pp_temp_data_supplemental_fig$npp_rate, breaks =  c(-Inf, 1, 10, 100, Inf), labels = c("0-1", "1-10", "10-100", "100+"))

temp_difs_plot <- ggplot(pp_temp_data_supplemental_fig, aes(x = depth, y = temp_dif, fill = range))+
  geom_point(shape = 21, size = 3)+
  #scale_fill_viridis()+
  scale_fill_manual(values= c('darkblue', "lightblue", 'white', "grey"))+
  geom_hline(yintercept = 5.3, linetype = "dotted")+  # add a horizontal line at y = 5.1
  geom_hline(yintercept = -2.7, linetype = "dotted")+  # add a horizontal line at y = -3
  # geom_text(aes(label = round(mean_npp, 5)), size = 3, nudge_y = 0.3)+
  xlab("Depth (m)")+
  ylab(expression(bold(Delta* T* (degree*C))))+
  #annotate("text", x=2.5, y=7.5, label = "a)",size = 4, fontface = "bold")+
  plot_theme+
  theme(legend.position = "top")+
  labs(fill = expression(bold(atop(textstyle("NPP Range"),
                                   "(mgC" ~m^-3~ ~d^-1~ ")"))))
temp_difs_plot

# save the plot
ggsave( "Manuscript/SUPP_temp_difference.pdf", plot = temp_difs_plot, device = "pdf", width =12, height = 10, units ="cm", dpi = 300)
############ END OF SUPPLEMENAL FIGURE 2 ################

###### SUPPLEMENTAL FIGURE 3: HISTOGRAM OF INTEGRATED RATES ########
pp_int_figs_supp <- read_csv("Processed_data_outputs/int_pp_corrected_011924.csv")

pp_int_figs_supp <- pp_int_figs_supp %>%
  filter(!(cruise == "EN668" & cast == "20"))  #This is d6a and not using it

pp_int_figs_supp $season <- factor(pp_int_figs_supp $season , levels = c("Winter", "Summer"))

#plot for supplemental -- distribution of total npp data
pp_distribution <- pp_int_figs_supp %>%
  filter(size == "pp_GFF") %>%
  filter(nearest_station != "L11") #not using L11 data

#make histogram
pp_histogram <- ggplot(data = pp_distribution, aes(x = int_pp, fill = season)) +
  geom_histogram(bins = 12, color = "black")+
  plot_theme+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  facet_wrap(~season)+
  scale_x_continuous(breaks = c(0,500,1000,1500, 2000,2500))+
  xlab(bquote(bold("Integrated NPP" ~ (mgC ~ m^-2 ~ day^-1)))) +  
  # xlab(expression(atop(bold("Mean Integrated NPP"), paste(~mgC%*%m^-2%*%day^-1)))) +  # Two lines in ylab
  ylab("Count")+
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,6.3))
pp_histogram


ggsave('Manuscript/SUPP_int_npp_historgram.tiff', plot = pp_histogram , scale = 1, width = 14, height = 7, units ="cm", dpi = 300)

############ END OF SUPPLEMENAL FIGURE 3 ################

###### SUPPLEMENTAL FIGURE 4: HISTORICAL NPP RATES WITH CURRENT RATES ########
#read in marmap data
marmap <- read.table("marmap_data.txt", header = TRUE, sep = "\t")

marmap$LONG <- gsub("N","",as.character(marmap$LONG))
marmap$LONG <- as.numeric(marmap$LONG)

#add season
marmap$season <- with(marmap,
                      ifelse(Month %in% c(12,1,2), "Winter",
                             ifelse(Month %in% c(3,4,5), "Spring",
                                    ifelse(Month %in% c(6,7,8), "Summer",
                                           ifelse(Month %in% c(9,10, 11), "Fall", NA)))))

#get the right area of our transect
marmap <- marmap %>%
  filter(season %in% c("Summer", "Winter")) %>%
  filter(between(LAT, 39.85, 41.25)) %>%
  filter(between(LONG, -71.5, -70.25)) 

marmap_pp <- marmap %>%
  group_by(season) %>%
  summarise(mean = mean(IPP, na.rm = TRUE), sd = sd(IPP, na.rm = TRUE))%>%
  mutate(program = "MARMAP")

marmap_pp$season <- factor(marmap_pp$season, levels = c("Winter", "Summer"))

pp_int_for_MARMAP <- pp_int_figs_summarized_season %>%
  mutate(program = "This Study")

#add lter data to marmap dataframe
marmap_lter <- rbind(marmap_pp, pp_int_for_MARMAP)

marmap_lter$program <- factor(marmap_lter$program, levels = c("This Study", "MARMAP"))
marmap_lter$season <- factor(marmap_lter$season, levels = c("Winter", "Summer"))

marmap_lter$combined <- factor(paste(marmap_lter$season, marmap_lter$program, sep = "_"))

#read in other historical data just to make sure all is accounted for (this is the other historical, non-marmap data)
historical <- read_csv("historical_pp.csv")

historical$season <- with(historical,
                          ifelse(Month %in% c(12,1,2), "Winter",
                                 ifelse(Month %in% c(3,4,5), "Spring",
                                        ifelse(Month %in% c(6,7,8), "Summer",
                                               ifelse(Month %in% c(9,10, 11), "Fall", NA)))))

historical$LONG <- as.numeric(historical$LONG)
historical$LAT <- as.numeric(historical$LAT)

historical <- historical %>%
  filter(season %in% c("Summer", "Winter")) %>%
  filter(between(LAT, 39.85, 41.25)) %>%
  filter(between(LONG, -71.5, -70.25))  %>%
  filter(!is.na(IPP)) %>%
  select(Count, season, IPP, Day, Month,Year)


#join together historical and marmap
marmap_hist <- marmap %>%
  select(Count, season, IPP, Day, Month,Year)

historical <- rbind(historical, marmap_hist)

historical <- historical %>%
  filter(Month != 6) #remove june months b/c only comparing to july/aug cruises

historical <- historical %>%
  unique() %>%
  filter(!(is.na(IPP))) 

historical_summarized <- historical %>%
  unique() %>%
  group_by(season) %>%
  summarise(mean = mean(IPP, na.rm = TRUE), sd = sd(IPP, na.rm = TRUE)) %>%
  mutate(program = "Historical")

supplemental_marmap_plot <- historical_summarized

#isolate just seasonal data to be able to do the ks.test
historical_summer <- historical %>%
  filter(season == "Summer")

historical_winter <- historical %>%
  filter(season == "Winter")

pp_int_summer <- pp_int_figs %>%
  filter(size == "total") %>%
  filter(season == "Summer") 

pp_int_winter <- pp_int_figs %>%
  filter(size == "total") %>%
  filter(season == "Winter") 

#test for differences
ks.test(historical_summer$IPP,pp_int_summer$int_pp)
ks.test(historical_winter$IPP,pp_int_winter$int_pp)

historical_summarized$program <- "Historical"

pp_int_for_MARMAP <- pp_int_for_MARMAP  %>%
  select(-se)

#Using all of the historical data now, not just the marmap data 
#add lter data to marmap dataframe
hist_lter <- rbind(historical_summarized, pp_int_for_MARMAP)

hist_lter$program <- factor(hist_lter$program, levels = c( "Historical", "Current"))
hist_lter$season <- factor(hist_lter$season, levels = c("Winter", "Summer"))

hist_lter[4,4]<- "Current"
hist_lter[3,4]<- "Current"

# #PLot of Historical and LTER data
seasonal_marmap_lter <- ggplot(hist_lter, aes(x = factor(program), y = mean, fill = season))+
  plot_theme+
  facet_wrap(~season)+
  geom_bar(color = "black",stat = "identity", position = position_dodge(width = 0.9))+ 
  ylab(expression(atop(bold("Integrated NPP"), paste(~mgC~m^-2~day^-1)))) +  # Two lines in ylab
  theme(axis.title.x = element_blank())+
  scale_y_continuous(breaks = seq(0, 1900, 400), limits = c(0,1900), expand = c(0,0))+
  scale_fill_manual(name = "Season", values = c("Winter" = "#005E94", "Summer" = "#C86A20")) +
  geom_errorbar(aes(ymax=mean+sd, ymin = mean),width = 0.2, position = position_dodge(width = 0.9))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 12))
seasonal_marmap_lter 

ggsave('Manuscript/SUPP_lter_hist_npp.pdf', plot = seasonal_marmap_lter , scale = 1, width = 5, height = 3, units ="in", dpi = 300)

#### END OF SUPPLEMENTAL FIGURE 4 #######

###### SUPPLEMENTAL FIGURE 5: INTEGRATED NPP BY YEAR AND STATION #######
pp_station <- pp_int_figs %>%
  filter(size == "total")

pp_station$nearest_station[pp_station$nearest_station == "L1.2"] <- "L1"


pp_station$nearest_station <- factor(pp_station$nearest_station, levels = c("L1", "L2", "L4", "L6", "L8", "L10"))

pp_station$year <- factor(pp_station$year)

pp_station_year <- ggplot(pp_station, aes(x = nearest_station,y = int_pp, fill = year))+
  geom_line(aes(color = year,group = year),size = 1)+
  geom_point(size = 2.5, color = "black", pch = 21)+
  facet_wrap(~season)+
  scale_fill_manual(name = "Year", values = c("2020" ="#7E2F8E", "2021" = 	"#77AC30", "2022" ="#4DBEEE"))+
  scale_color_manual(name = "Year", values = c("2020" ="#7E2F8E", "2021" = 	"#77AC30", "2022" ="#4DBEEE"))+
  plot_theme+
  ylab(expression(atop(bold("Integrated NPP"), paste(~mgC~m^-2~d^-1)))) +  # Two lines in ylab
  xlab("Station")+
  scale_y_continuous(breaks = seq(0, 2650, 500 ),
                     minor_breaks = seq(0, 2650, 250),
                     limits = c(0,2650),
                     guide = "axis_minor",expand = c(0,0))+
  theme(strip.text.x = element_text(size = 10))+
  theme(legend.position = "top")
pp_station_year

ggsave('Manuscript/SUPP_int_npp_year.pdf', device = "pdf", plot =pp_station_year , scale = 1, width = 7, height = 4, units ="in", dpi = 300)

###### END OF SUPPLEMENTAL FIGURE 5 ########

####### SUPPLEMENTAL FIGURE 6: STACKED BAR PLOT OF ANOMALOUS INTEGRATED NPP VERSUS SEASONAL AVERAGE ########
#getting the no anom data
pp_int_percent_no_ammon <- pp_int_percent %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(region == "Inner-shelf") %>%
  #filter(nearest_station != "L1.2") %>%
  group_by(season, size) %>%
  summarise(percent = mean(percent_pp)) %>%
  mutate(type = "Average")

#now isolate percent for anomaly
summer_int_anom <- pp_int_percent %>%
  filter(cruise == "EN687" & cast == "2") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer")

#now isolate percent for anomaly -- winter
winter_int_anom <- pp_int_percent %>%
  filter(cruise == "AT46" & cast == "17") %>%
  rename("percent" = percent_pp) %>%
  ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Winter")

int_anom_joined <- rbind(pp_int_percent_no_ammon, summer_int_anom, winter_int_anom)

int_anom_joined <- int_anom_joined %>%
  mutate(size = case_when(
    size == "per_great20" ~ "greater20",
    size == "per_less5" ~ "less5",
    size== "per_bet5_20" ~ "between520",
    TRUE ~ size
  ))

int_anom_joined$season <- factor(int_anom_joined$season, levels = c("Winter", "Summer"))
int_anom_joined$size <- factor(int_anom_joined$size, levels = c("less5", "between520", "greater20"))
int_anom_joined$type <- factor(int_anom_joined$type, levels = c("L2 in 2022", "Average"))


#plot the anomaly
anomaly_integrated <- ggplot(int_anom_joined  , aes(x = type, y = percent, alpha = size, fill= season))+
  geom_bar(color = "black", stat="identity", position = "stack")+
  plot_theme+
  #labs(x = "Net Primary Production (µgC/L/day)", y = "Depth Type")+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
 # scale_fill_manual(name = "Season", values = c("Winter" = "skyblue4", "Summer" = "#F78408"")) +

  theme(strip.text.x = element_text(size = 14))+
  facet_wrap(~season)+
  scale_y_continuous(expand = c(0,0), limits = c(0,102))+
  labs(x = "Type", y = "% Contribution to Integrated NPP")
anomaly_integrated 

anomaly_integrated  <- anomaly_integrated +
  scale_x_discrete(labels = c("L2 in \n2022", "Seasonal\nAverage"))
anomaly_integrated 

ggsave('Manuscript/SUPP_anomaly_integrated.pdf', device = "pdf", plot =anomaly_integrated  , scale = 1, width =6, height = 4, units ="in", dpi = 300)

###Test for sig differences
#first up is the summer 20
pp_int_percent_no_ammon20 <- pp_int_percent %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(region == "Inner-shelf") %>%
  filter(size == "per_great20")


shapiro.test(pp_int_percent_no_ammon20$percent_pp)

summer_noanom20 <- pp_int_percent_no_ammon20  %>%
  filter(season == "Summer")

#now isolate percent for anomaly
summer_int_anom20 <- pp_int_percent %>%
  filter(cruise == "EN687" & cast == "2") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_great20")


t.test(summer_noanom20$percent_pp, mu =summer_int_anom20$percent, alternative = "less" ) #0.02 for summer >20 increase to L2 --- so it's less because the mu is the anom, so we're seeing if the average is less than the mu

#Now for summer 5-20
pp_int_percent_no_ammon520 <- pp_int_percent %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(region == "Inner-shelf") %>%
  # filter(nearest_station != "L1.2") %>%
  filter(size == "per_bet5_20")

summer_noanom520 <- pp_int_percent_no_ammon520  %>%
  filter(season == "Summer")

#now isolate percent for anomaly
summer_int_anom520 <- pp_int_percent %>%
  filter(cruise == "EN687" & cast == "2") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_bet5_20")

t.test(summer_noanom520$percent_pp, mu =summer_int_anom520$percent , alternative = "less") #0.43 for 5-20 

#Now for summer 5
pp_int_percent_no_ammon5 <- pp_int_percent %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(region == "Inner-shelf") %>%
  filter(nearest_station != "L1.2") %>%
  filter(size == "per_less5")

summer_noanom5 <- pp_int_percent_no_ammon5  %>%
  filter(season == "Summer")

#now isolate percent for anomaly
summer_int_anom5 <- pp_int_percent %>%
  filter(cruise == "EN687" & cast == "2") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_less5")

t.test(summer_noanom5$percent_pp, mu =summer_int_anom5$percent, alternative = "greater" ) #0.001 for an increase from average 

#Now for winter5
winter_noanom5 <- pp_int_percent_no_ammon5  %>%
  filter(season == "Winter")

#now isolate percent for anomaly
winter_int_anom5 <- pp_int_percent %>%
  filter(cruise == "AT46" & cast == "17") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_less5")

t.test(winter_noanom5$percent_pp, mu =winter_int_anom5$percent, alternative = "greater" ) #0.004 for a decrease!!!!!

#Now for winter 520
winter_noanom520 <- pp_int_percent_no_ammon520  %>%
  filter(season == "Winter")

#now isolate percent for anomaly
winter_int_anom520 <- pp_int_percent %>%
  filter(cruise == "AT46" & cast == "17") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_bet5_20")

t.test(winter_noanom520$percent_pp, mu =winter_int_anom520$percent, alternative = "greater" ) #0.02 for a decrease!!!!!

#Now for winter 20
winter_noanom20 <- pp_int_percent_no_ammon20  %>%
  filter(season == "Winter")

#now isolate percent for anomaly
winter_int_anom20 <- pp_int_percent %>%
  filter(cruise == "AT46" & cast == "17") %>%
  rename("percent" = percent_pp) %>%
  dplyr::ungroup() %>%
  select(size, percent) %>%
  mutate(type = "L2 in 2022") %>%
  mutate(season = "Summer") %>%
  filter(size == "per_great20")

t.test(winter_noanom20$percent_pp, mu =winter_int_anom20$percent, alternative = "less" ) #0.012 for an increase
############ END OF SUPPLEMENAL FIGURE 6 ################

##### SUPPLEMENTAL FIGURE 8: CHL DATA FOR L2 #########
#Add supplemental plot for chl data for L2
L2_chl <- chl_int
L2_chl$season <-  with(L2_chl,
                       ifelse(cruise == "EN644", "Summer",
                              ifelse(cruise == "EN655", "Summer",
                                     ifelse(cruise == "EN649", "Winter", 
                                            ifelse(cruise == "EN661", "Winter",
                                                   ifelse(cruise == "EN668", "Summer",
                                                          ifelse(cruise == "AT46", "Winter", ifelse(cruise == "EN687", "Summer",NA))))))))


L2_chl$season <- factor(L2_chl$season, levels = c("Winter", "Summer"))


L2_chl$region <- with(L2_chl,
                      ifelse(nearest_station %in% c("L1", "L2", "L1.2"), "Inner-shelf",
                             ifelse(nearest_station %in% c("L4", "L6", "d6a"), "Mid-shelf",
                                    ifelse(nearest_station %in%  c('L8', "L10", "L11"), "Outer-shelf", NA)))) 

chl_inner_total <- L2_chl%>%
  filter(region == "Inner-shelf") %>%
  filter(size == "total") %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) 

shapiro.test(log(chl_inner_total$int_chl+1))

chl_inner_avg <- chl_inner_total %>%
  group_by(season) %>%
  summarise(sd_chl = sd(int_chl), int_chl = mean(int_chl)) %>%
  mutate(Type = "Seasonal Average")


chl_inner_L2 <-  L2_chl %>%
  filter(size == "total") %>%
  filter(cruise == "AT46" & cast == "17" | cruise == "EN687" & cast == "2") %>%
  mutate(Type = "L2 in 2022") %>%
  mutate(sd_chl = NA) %>%
  select(season, int_chl, Type, sd_chl)  


#test for differences
chl_inner_winter <- chl_inner_total %>%
  filter(season == "Winter")
chl_inner_summer <- chl_inner_total %>%
  filter(season == "Summer")

chl_inner_L2_winter <-  L2_chl %>%
  filter(size == "total") %>%
  filter(cruise == "AT46" & cast == "17" | cruise == "EN687" & cast == "2") %>%
  filter(season == "Winter")

chl_inner_L2_summer <-  L2_chl %>%
  filter(size == "total") %>%
  filter(cruise == "AT46" & cast == "17" | cruise == "EN687" & cast == "2") %>%
  filter(season == "Summer")

t.test(log(chl_inner_winter$int_chl+1), mu = log(chl_inner_L2_winter$int_chl+1)) #yes, 0.0008
t.test(log(chl_inner_summer$int_chl+1), mu = log(chl_inner_L2_summer$int_chl+1)) #no, 0.1772

#What about actual values of chl by size
chl_inner_size <- L2_chl%>%
  filter(region == "Inner-shelf") %>%
  filter(size != "total") %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) 

chl_inner_avg_size <- chl_inner_size %>%
  group_by(season,size) %>%
  summarise(sd_chl = sd(int_chl), int_chl = mean(int_chl)) %>%
  mutate(Type = "Seasonal Average")


chl_inner_L2_size <-  L2_chl %>%
  filter(size != "total") %>%
  filter(cruise == "AT46" & cast == "17" | cruise == "EN687" & cast == "2") %>%
  mutate(Type = "L2 in 2022") %>%
  mutate(sd_chl = NA) %>%
  select(season, int_chl, size,Type, sd_chl) 



chl_inner_L2_test <- L2_chl %>%
  filter(size != "total") %>%
  filter(cruise == "AT46" & cast == "17" | cruise == "EN687" & cast == "2") %>%
  mutate(Type = "L2 in 2022") %>%
  filter(size == "greater20") %>%
  filter(season == "Summer")

chl_inner_L2_test_all <-  L2_chl %>%
  filter(region == "Inner-shelf") %>%
  filter(size != "total") %>%
  filter(!(cruise == "AT46" & cast == "17")) %>%
  filter(!(cruise == "EN687" & cast == "2")) %>%
  filter(size == "greater20") %>%
  filter(season == "Summer")

t.test(log(chl_inner_L2_test_all$int_chl+1), mu = log(chl_inner_L2_test$int_chl+1), alternative = "less") #0.2188

joined_anom_size <- rbind(chl_inner_avg_size,chl_inner_L2_size)
joined_anom_size$Type <- factor(joined_anom_size$Type, levels = c("L2 in 2022", "Seasonal Average"))
joined_anom_size$size <- factor(joined_anom_size$size , levels = c("less5","between520", "greater20"))

#plot of size values
anomaly_integrated_size <- ggplot(joined_anom_size , aes(x = Type, y = int_chl, alpha = size, fill= season))+
  geom_bar(color = "black", stat="identity", position = "dodge")+
  plot_theme+
  #labs(x = "Net Primary Production (µgC/L/day)", y = "Depth Type")+
  scale_fill_manual(name = "Season", values = c("#0072B5","#E18726"))+
  theme(strip.text.x = element_text(size = 12))+
  facet_wrap(~season)+
 scale_y_continuous(limits = c(0, 110), expand = c(0,0) ) +
   geom_errorbar(aes(ymax=int_chl+sd_chl, ymin = int_chl),width = 0.2, position = position_dodge(width = 0.9))+
  ylab(expression(bold("Chlorophyll-a"~(mg~m^-2)))) +  # Two lines in ylab
  xlab("Type")
   anomaly_integrated_size
  anomaly_integrated_size<- anomaly_integrated_size+
   scale_x_discrete(labels = c("L2 in \n2022", "Seasonal\nAverage"))
  ggsave('Manuscript/SUPP_chl_anom.pdf', plot =anomaly_integrated_size, scale = 1, width =6, height = 4, units ="in", dpi = 300)
  ######## END OF SUPPLEMENTAL FIGURE 8 ########
                                                
 ###### SUPPLEMENTAL FIGURE 9: NUTRIENT CONCENTRATIONS DURING ANOMALOUS EVENTS ########
nuts_discrete <- read_csv("Processed_data_outputs/nutrient_data_qced_033024.csv")
 bottles_grouped_complete <- read_csv("niskin_groups_fixed.csv")
                                                
 #Making plots for anomalously high NPP
winter_inner <- pp_discrete_plots %>%
 filter(season == "Winter") %>%
filter(region == "Inner-shelf") %>%
filter(size == "total")
                                                
 summer_inner <- pp_discrete_plots %>%
 filter(season == "Summer") %>%
filter(region == "Inner-shelf") %>%
  filter(size == "total")
                                                
 #Add a depth group info
 winter_inner <- winter_inner %>%
   group_by(cruise, cast, station) %>%
   mutate(depth_ID = case_when(
     depth == max(depth) ~ 1,
     depth == median(depth) ~ 2,
     depth == min(depth) ~ 3,
     TRUE ~ NA_integer_
   )) %>%
   ungroup()
 
 #add depth group for summer
 #Add a depth group info
 summer_inner <- summer_inner %>%
   group_by(cruise, cast, station) %>%
   mutate(depth_ID = case_when(
     depth == max(depth) ~ 1,
     depth == median(depth) ~ 2,
     depth == min(depth) ~ 3,
     TRUE ~ NA_integer_
   )) %>%
   ungroup()
 
 depth_info <- rbind(summer_inner, winter_inner)
 depth_info <- depth_info %>%
   select(cruise,cast,depth_ID, depth_grp)
 
 
 pp_info_stn <- pp_int_figs %>%
   select(cruise,cast,region) %>%
   unique()
 
 nuts_discrete <- nuts_discrete %>%
   select(cruise, cast, niskin, depth, contains("mean_"), contains("sd_")) %>%
   pivot_longer(cols = contains("mean_") | contains("sd_"),
                names_to = c(".value", "nutrient_type"),
                names_pattern = "(mean|sd)_(.*)") %>%
   left_join(pp_info_stn, by = c("cruise", "cast")) %>%
   filter(cruise %in% c("AT46", "EN649", "EN661", "EN655", "EN687", "EN668"))
 
 
 #add season
 nuts_discrete$season <- with(nuts_discrete,
                              ifelse(cruise == "EN655", "Summer",
                                     ifelse(cruise == "EN649", "Winter", 
                                            ifelse(cruise == "EN661", "Winter",
                                                   ifelse(cruise == "EN668", "Summer",
                                                          ifelse(cruise == "AT46", "Winter",
                                                                 ifelse(cruise == "EN687", "Summer",NA)))))))
 
 
 nuts_discrete <- nuts_discrete %>%
   left_join(bottles_grouped_complete, by = c("cruise", "cast", "niskin")) %>%
   left_join(depth_info, by = c("cruise", "cast", "depth_grp")) %>%
   rename("concentration" = mean) 
 
 
 winter_inner_nuts <- nuts_discrete %>%
   filter(season == "Winter") %>%
   filter(region == "Inner-shelf") %>%
   filter(!is.na(depth_ID))
 
 summer_inner_nuts <- nuts_discrete %>%
   filter(season == "Summer") %>%
   filter(region == "Inner-shelf")  %>%
   filter(!is.na(depth_ID))
 
 #add depth group for winter
 
 winter_inner_avg_nuts <- winter_inner_nuts %>%
   filter(!(cruise == "AT46"& cast == "17")) %>%
   group_by(depth_ID,nutrient_type) %>%
   summarise(mean = mean(concentration, na.rm = TRUE), sd = sd(concentration, na.rm = TRUE))%>%
   mutate(type = "Average") %>%
   mutate(season = "Winter")
 
 
 winter_inner_anam_nuts <-  winter_inner_nuts %>%
   filter(cruise == "AT46"& cast == "17") %>%
   mutate(type = "L2 in 2022") %>%
   select(depth_ID, nutrient_type,concentration,sd, type ) %>%
   rename("mean" = concentration) %>%
   mutate(season = "Winter")
 
 #now for summer
 summer_inner_avg_nuts <- summer_inner_nuts %>%
   filter(!(cruise == "EN687"& cast == "2")) %>%
   group_by(depth_ID,nutrient_type) %>%
   summarise(mean = mean(concentration, na.rm = TRUE), sd = sd(concentration, na.rm = TRUE))%>%
   mutate(type = "Average") %>%
   mutate(season = "Summer")
 
 
 summer_inner_anam_nuts <-  summer_inner_nuts %>%
   filter(cruise == "EN687"& cast == "2") %>%
   mutate(type = "L2 in 2022") %>%
   select(depth_ID, concentration,sd,nutrient_type, type ) %>%
   rename("mean" = concentration) %>%
   mutate(season = "Summer")
 
 
 anam_joined_nuts <- rbind(winter_inner_avg_nuts,winter_inner_anam_nuts, summer_inner_avg_nuts ,summer_inner_anam_nuts)
 
 anam_joined_nuts$depth_ID <- factor(anam_joined_nuts$depth_ID, levels = c(3, 2, 1), labels = c("Surface", "Mid", "Deep"))
 anam_joined_nuts$season <- factor(anam_joined_nuts$season, levels = c("Winter", "Summer"))
 anam_joined_nuts$type <- factor(anam_joined_nuts$type, levels = c( "L2 in 2022",  "Average"))
 anam_joined_nuts$nutrient_type <- factor(anam_joined_nuts$nutrient_type, levels = c(  "ammon",  "nitr",  "phos","sil"))
 
 
 #plot of nutrients
 anomaly_discrete_nuts <- ggplot(anam_joined_nuts , aes(y = type, x = mean, fill= nutrient_type))+
   #geom_point(color = "black", shape = 21, size = 3, position = position_dodge(width = 0.9))+
   geom_bar(color = "black", stat = "identity", position = position_dodge(width = 0.9))+
   geom_errorbar(aes(xmax=mean+sd, xmin = mean),width = 0.2, position = position_dodge(width = 0.9))+
   plot_theme+
   #scale_x_continuous(expand = c(0.02,0.02))+
   xlab(expression(bold("Concentration"~(µmol~L^-1)))) +  # Two lines in ylab
   #scale_fill_npg(name = "Nutrient", labels = c("Silicate","Phosphate", "Nitrate/Nitrite", "Ammonium"))+
   scale_fill_npg(name = "Nutrient", labels = c("Ammonium",  "Nitrate/Nitrite","Phosphate","Silicate"))+
   theme(legend.key.size = unit(1, "lines"))+  # Adjust the size as needed
   facet_grid(depth_ID~season, scales = "free")+
   theme(strip.text.x = element_text(size = 10))+
   theme(axis.title.y = element_blank())+
   theme(strip.text.y = element_text(size = 10), legend.position = "top")+
   guides(fill = guide_legend(ncol = 2))
 anomaly_discrete_nuts 
 
 ggsave('Manuscript/SUPP_anomaly_nutrients.pdf', plot =anomaly_discrete_nuts, width = 4, height =5, units ="in", dpi = 300)
 
 #test for sig differences
 winter_nitr_test <- winter_inner_nuts %>%
   filter(!(cruise == "AT46" & cast == "17")) %>%
   filter(depth_ID == 1) %>%
   filter(nutrient_type == "phos")
 
 
 anom_nitr_winter <- winter_inner_nuts  %>%
   filter(cruise == "AT46" & cast == "17") %>%
   filter(depth_ID == 1) %>%
   filter(nutrient_type == "phos")
 
 t.test(winter_nitr_test$concentration, mu =anom_nitr_winter $concentration)
 #result for D1 nitrate in ref to average = 0.36, silicate = 0.11 = higher; ammon = 0.03 lower; phos = 0.06
 #result for D2 nitrate in ref to average = 0.37, silicate = 0.584; ammon = 0.048 lower; phos = 0.11
 #result for D3 nitrate in ref to average = 0.36, silicate = 0.17 = lower; ammon = 0.045 lower; phos = 0.02 higher
 
 #now for summer
 summer_nut_test <- summer_inner_nuts %>%
   filter(!(cruise == "EN687" & cast == "2")) %>%
   filter(depth_ID == 1) %>%
   filter(nutrient_type == "nitr")
 
 
 anom_nut_summer <- summer_inner_nuts   %>%
   filter(cruise == "EN687" & cast == "2")%>%
   filter(depth_ID == 1) %>%
   filter(nutrient_type == "nitr")
 
 t.test(summer_nut_test $concentration, mu =anom_nut_summer $concentration)
 #result for D1 nitrate in ref to average = 3.272E-5 higher, silicate = 0.08 = lower; ammon = 0.003 higher; phos = 0.02 higher
 #result for D2 nitrate in ref to average = everything is 0 at CM, silicate = 0.07; ammon = 0.009 higher; phos = 0.01 higher
 #result for D3 nitrate in ref to average = everything is 0, silicate = 0.13 = lower; ammon = 0.13; phos = 0.3 higher
 ############ END OF SUPPLEMENAL FIGURE 9 ################
          
###### Plotting chl per cell for the reviewer 2 comment ########
attune <- read_csv("attune-transect-discrete-samples.csv")
attune <- attune %>%
  filter(cruise %in% c("EN649", "EN655", "EN661", "EN668", "AT46")) %>%
  left_join( bottles_grouped_complete, by = c("cruise", "cast", "niskin")) %>%
  select(cruise, cast, niskin, depth_grp, redeuk_leq_5um_cells_per_ml, redeuk_leq_20um_cells_per_ml)%>%
  mutate(between520 = redeuk_leq_20um_cells_per_ml - redeuk_leq_5um_cells_per_ml) %>%
  rename(less5 = redeuk_leq_5um_cells_per_ml) %>%
  select(-redeuk_leq_20um_cells_per_ml, -niskin) %>%
  pivot_longer(cols = less5:between520, 
               names_to = "size",
               values_to = "cells_ml")

#read chl data
chl_edi <- read_csv("Processed_data_outputs/chl_EDI_discrete_Qced_0524.csv")
chl_edi <- chl_edi %>%
  select(cruise, cast, niskin, depth_grp, size, mean_chl) %>%
  filter(size %in% c("less5", "between520"))

chl_atttune <- attune %>%
  left_join(chl_edi, by= c("cruise", "cast", "depth_grp","size")) %>%
  mutate(chl_cell = (mean_chl/1000)/cells_ml)

chl_atttune $season <- with(chl_atttune ,
                                 ifelse(cruise == "EN644", "Summer",
                                        ifelse(cruise == "EN655", "Summer",
                                               ifelse(cruise == "EN649", "Winter", 
                                                      ifelse(cruise == "EN661", "Winter",
                                                             ifelse(cruise == "EN668", "Summer",
                                                                    ifelse(cruise == "AT46", "Winter",
                                                                           ifelse(cruise == "EN687", "Summer",NA))))))))

#plot the chl per cell as boxplot
chl_cell_boxplot <- ggplot(chl_atttune , aes(x = factor(season), y = log10(chl_cell*1000000), fill = size))+
  plot_theme+
  geom_boxplot(aes(fill = size), color = "black", size = 0.6, width = 0.7, outlier.shape = 16, outlier.size = 2)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width = 0.2, position = position_dodge(0.7)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width = 0.2, position = position_dodge(0.7)) +
  ggplot2::stat_summary(fun = base::mean, geom="point", shape=21, size=2, position = position_dodge(0.7))+
  ylab(expression(atop(bold("Log10 Chl per Cell"), paste(~pgchl~cell^-1)))) +  # Two lines in ylab
  xlab("Season")+
  scale_fill_manual(name = "Size Class", values = c("grey", "white"), labels = c("5-20 µm", "< 5 µm")) +
seasonal_lter




                                                
                                                
                                                
                                                