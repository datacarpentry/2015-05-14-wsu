###################################################
####  Script from Data Carpentry WSU Workshop  ####
###################################################

## 2015-05-15

## Notes about how to do arithmetic
2 + 2
5 * 6
10 / 4

## Square root function:
sqrt(16)

## Repeat the number 8 10 times
rep(8, times = 10)

## Help file for function
?rep

## Assign a value to weight_kg
(weight_kg <- 55)
weight_kg

weight_kg + 10
weight_kg * 2.2

weight_lb <- weight_kg * 2.2
weight_lb

weight_kg <- 100
weight_lb

## Exercise: what is the value of each of these after each assignment?
mass <- 47.5           # mass?
age  <- 122            # age?
mass <- mass * 2.0     # mass?
age  <- age - 20       # age?
massIndex <- mass/age  # massIndex?

### Vector
weights <- c(50, 60, 65, 82)
weights

animals <- c("mouse", "rat", "dog")
animals

weights <- c(weights, 90)
weights

## Finding information about objects
length(weights)                         # length
class(weights)                          # class
class(animals)
str(weights)                            # structure
str(animals)

#############################
####  Working with data  ####
#############################

getwd()                                 # find your working directory

## Load the data (will depend on where data is located)
surveys <- read.csv("data/cleaned//20150514_KHW_Portal_joined.csv")
surveys
str(surveys)

## Creating your own data frames
example_data <- data.frame(animal = c("dog", "cat", "sea cucumber"),
                           feel = c("furry", "furry", "squishy"), 
                           weight = c(45, 8, 1.1))

author_book <- data.frame(author_first=c("Charles", "Ernst", "Theodosius"),
                          author_last=c("Darwin", "Mayr", "Dobzhansky"),
                          year=c(1859, 1942, NA))

## What is the class of each column in this data frame?
country_climate <- data.frame(country=c("Canada", "Panama", "South Africa", 
                                        "Australia"),
                              climate=c("cold", "hot", "temperate", "hot/temperate"),
                              temperature=c(10, 30, 18, "15"),
                              north_hemisphere=c(TRUE, TRUE, FALSE, "FALSE"),
                              has_kangaroo=c(FALSE, FALSE, FALSE, 1))

## Indexing vectors
weights
weights[1]
weights[2:4]
weights[c(1, 3, 5)]
animals[3]

## Indexing data frames (two dimensions)
## [rows, columns]
surveys[1, 1]
surveys[5:10, 3:4]

## What are the column names of the surveys data?
names(surveys)

surveys[1:5, "species_id"]
surveys[1:5, c("species_id", "genus")]

## Create a sequence of numbers
seq(from = 1, to = 10, by = 2)
seq(from = 1, to = 8, by = 3)

## Subset the data to have every tenth row, starting from row 10
surveys_by_10 <- surveys[seq(10, nrow(surveys), by =10), ]
str(surveys)
class(surveys)

surveys$plot_type
class(surveys$plot_type)

## Keep all rows, columns 1-3
surveys[, 1:3]

## Summary statistics of each column
summary(surveys)

########################################
####  Manipulating data with dplyr  ####
########################################

## Install package (once)
install.packages("dplyr")

## Load package (each R session when you want to use it)
library("dplyr")

## Select columns
select(surveys, plot_id, species_id, weight)

## Filter rows
yr95 <- filter(surveys, year == 1995)

## Select and filter with pipes
surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

## Using pipes, subset the data to include rows
## before 1995. Retain columns year, sex, and weight.
surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)

## Number of counts for each sex
surveys %>%
  group_by(sex) %>%
  tally()

## With surveys data, for each sex-species_id combination find the mean weight
## and the minimum weight (removing NAs). Then remove rows where mean_weight is
## not a number, and keep rows where sex is M or F
surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight, na.rm = TRUE)) %>%
  filter(!is.nan(mean_weight)) %>%
  filter(sex == "M" | sex == "F")

## How many times was each plot_type surveyed?
surveys %>%
  group_by(plot_type) %>%
  tally()

## Use group_by() and summarize() to find the mean, min, and max hindfoot length
## for each species.
surveys %>%
  group_by(species_id) %>%
  summarize(mean_ln = mean(hindfoot_length, na.rm = TRUE),
            min_ln = min(hindfoot_length, na.rm = TRUE), 
            max_ln = max(hindfoot_length, na.rm = TRUE)) %>%
  filter(!is.nan(mean_ln)) %>%
  nrow()

## Getting help
?data.frame
??cast
??histogram
help.search("kruskal")

myvec <- c(4, 7, 1; 4)

## Install and load ggplot2 package
install.packages("ggplot2")
library("ggplot2")

## Create an empty plot
myplot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))
summary(myplot)

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
## the above is the same as
myplot + geom_point()

## edit settings of geoms:
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(size = 3)

## Exercise
d2 <- diamonds[sample(1:dim(diamonds)[1], 1000), ]

ggplot(d2, aes(x = carat, y = price, color = color)) +
  geom_point()

## Histogram of times between Old Faithful eruptions
h <- ggplot(faithful, aes(x = waiting))
h + geom_histogram(color = "blue")

## Scatterplot of weight vs hindfoot length in the survey data
ggplot(surveys, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

## Challenge: create a subset of the data for one species and create a scatterplot of weight vs hindfoot length
surveys %>%
  filter(species_id == "DM") %>%
  ggplot(aes(x = weight, y = hindfoot_length)) +
  geom_point()

## Boxplot of weights by species
surveys %>%
  filter(weight > 0) %>%
  ggplot(aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot()

## Calculate the number of counts per year for each genus and make a line plot
p <- surveys %>%
  group_by(year, genus) %>%
  tally() %>%
  ggplot(aes(x = year, y = n, group = genus, color = genus)) +
  geom_line()

p

## Save a plot as a .png file
ggsave("output/lines_by_genus.png", p, 
       height = 6, width = 8)

## Save as a PDF instead
ggsave("output/lines_by_genus.pdf", p, 
       height = 6, width = 8)


