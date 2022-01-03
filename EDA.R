if(!require(pacman))install.packages("pacman")
pacman::p_load(
  tidyverse,
  dplyr,
  ggplot,
  caret,
  magnittr,
  pacman,
  GGally,
  knitr,
  parallel, 
  rattel,
  tictoc,
  gridExtra,
  kableExtra,
  readr, 
  purrr,
  randomForest,
  pROC,
  fastDummies, 
  rpart.plot,
  data.table, 
  reshape2,
  graphics,
  corrplot,
  latexpdf,
  ReporteRs,
  tinytex, 
  latexdiffr,
  latex2exp,
  ggrepel,
)

# The import the dataset 
filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs")
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")
murder <- read_csv(filename)

# the summary of dataset 
#dimention of dataset:
dim(murder)
# the name of columns
names(murder)
# missing data 
any(is.na(murder)) 
# the first six rows of dataset
head(murder)

#State names by order of their total murders
ind <- order(murder$total)
State_order_murder <- murder$state[ind]

kable(head(State_order_murder),
      "pandoc", 
      caption = "The State names based on order of Their total murder", 
      align = "c", 
      font_size = 5)

# the max and min of total murder 
max_total_murder <- max(murder$total)
min_total_murder <- min(murder$total)

# The state name of min and max of total number 
i_max <- which.max(murder$total)
State_max_total_murdre <- murder$state[i_max]
i_min <- which.min(murder$total)
state_min_total_murdre <- murder$state[i_min]

# the population of max and min of total number state
Population_max_total_murder <- murder$population[i_max]
Population_min_total_murder <- murder$population[i_min]

rbind(max_total_murder,min_total_murder,
      State_max_total_murdre, state_min_total_murdre,
      Population_max_total_murder, Population_min_total_murder)

# Defined the murder rate
murder <-murder %>% mutate(region = factor(region),
                           murder_rate = total / population * 10^5)

# The state name of min and max of murder rate
max_murder_rate <- max(murder$murder_rate)
min_murder_rate <- min(murder$murder_rate)

i_max_murder_rate <- which.max(murder$murder_rate)
State_max_murdre_rate <- murder$state[i_max_murder_rate]
i_min_murder_rate  <- which.min(murder$murder_rate)
state_min_murdre_rate <- murder$state[i_min_murder_rate ]

rbind(max_murder_rate,min_murder_rate,
      State_max_murdre_rate, state_min_murdre_rate)


# col plot of murder rate of each state base on Abb
murder %>% mutate(abb = reorder(abb,murder_rate)) %>%
  ggplot(aes(abb, murder_rate)) +
  geom_col(width = 0.7, color = "pink",fill = "blue") +
  xlab("Abb") +
  ylab("Murder rate per 100000" ) +
  coord_flip() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 5)) +
  ggtitle("Variation of murder rate based on state")

murder %>% ggplot(aes(murder_rate)) + 
  geom_histogram(bins = 7, color = "pink", fill = "blue") +
  xlab("Murder rate") +
  ylab("Frequency") +
  ggtitle ("Distribution of murder rate")

# Boxplot of murder rate versus region
p1_murder_rate <- murder %>% group_by(region) %>% 
  ggplot(aes(region, murder_rate)) +
  geom_boxplot(col = "pink", fill = "blue")+ 
  xlab("Region") + ylab("Murder rate") 
  
p1_population <- murder %>%  
  group_by(region) %>%
  ggplot(aes(region, population/10^6)) +
  geom_bar(aes(fill = region ),stat="identity")+
  xlab("Region") +
  ylab("Population in million")+ 
  theme(legend.position =  "none") +
  scale_y_log10()

grid.arrange(p1_murder_rate,p1_population)


p1 <- murder %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total murders (log scale)") +
  ggtitle("Total Murder versus population in millions") +
  scale_color_discrete(name = "Region")+
  facet_grid(. ~ region) +
  theme(legend.position = "none")

p2 <- murder %>% ggplot(aes(population/10^6, murder_rate, label = abb)) +
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Murder rate (log scale)") +
  ggtitle("Murder rate versus Population in millions") +
  scale_color_discrete(name = "Region")+
  facet_grid(. ~ region) +
  theme(legend.position = "none")

grid.arrange(p1,p2)

