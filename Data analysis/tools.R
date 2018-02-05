################################################################################
################################################################################
################################################################################

library(plyr)

library(tidyverse)
library(DT) # dependency
library(ggthemes) # dependency

library('DescTools')

#library(plotly)


################################################################################
################################################################################
################################################################################


# SOURCE: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Misc functions
scenarios_order <- function (scenarios) {
    scenarios[scenarios=='Temperature'] = 1
    scenarios[scenarios=='Rain']        = 2
    scenarios[scenarios=='Sales']       = 3
    scenarios[scenarios=='Gym members'] = 4
    scenarios[scenarios=='Salary']      = 5
    scenarios[scenarios=='FB friends']  = 6

    return(scenarios)
}

kernels_order <- function (kernels) {
    kernels[kernels=='l+p']    = 'a'
    kernels[kernels=='l+r']    = 'b'
    kernels[kernels=='p+r']    = 'c'
    kernels[kernels=='l×r']    = 'd'
    kernels[kernels=='l×p']    = 'e'
    kernels[kernels=='p×r']    = 'f'
    kernels[kernels=='l+p+r']  = 'g'
    kernels[kernels=='l+p×r']  = 'h'
    kernels[kernels=='l×r+p']  = 'i'
    kernels[kernels=='l×p+r']  = 'j'
    kernels[kernels=='l×p×r']  = 'k'

    return(kernels)
}

scenarios <- c("temperature", "rain", "sales", "gym_memberships", "wage", "facebook_friends")
readable_scenarios <- c("Temperature", "Rain", "Sales", "Gym members", "Salary", "FB Friends")
subconditions = c(1, 2, 3)
condition_names = c("Prior", "Posterior-Positive", "Posterior-Stable", "Posterior-Negative")


################################################################################
##############################   DATA   ########################################
################################################################################

# Import data
data <- suppressWarnings(read_csv("data/catmull-rom-dataset.csv"))

# Small modification
data$noiseIndex <- data$noiseIndex+1

# All participants
participants <- data %>%
                select(userId, age, gender) %>%
                unique

data <- data %>% filter(userId != 'a106',userId != 'a118')

# To tidy data
tidy_data <- data %>%
    gather(day, value, starts_with("day"))

# Transform strings to numbers
tidy_data <- tidy_data %>%
    mutate(day = as.numeric(gsub("day", "", day)),
           value = as.numeric(value))

# Changes values to make them readable
tidy_data$subcondition_name <- mapvalues(tidy_data$subcondition,
                                  from = subconditions,
                                  to   = condition_names[2:4])

tidy_data$condition_name <- ifelse(tidy_data$stage==1,
                                   condition_names[1],
                                   "")

tidy_data$condition_name <- ifelse(tidy_data$stage == 2,
                                   tidy_data$subcondition_name,
                                   tidy_data$condition_name)

### Preparing the tidy data for analysis: `dat`
dat <- data.frame(
                   id        = tidy_data$userId,
                   day       = tidy_data$day, #x
                   value     = tidy_data$value, #y
                   condition = tidy_data$condition_name,
                   scenario  = tidy_data$scenario,
                   noise     = tidy_data$noiseIndex
                )

# Change the readable names of the variables
dat$scenario <- mapvalues( dat$scenario,
                          from = scenarios,
                          to = readable_scenarios)


# Order on the plot
dat$condition <- factor( dat$condition,
                        levels = condition_names)

dat$scenario <- factor( dat$scenario,
                       levels = readable_scenarios)

# Subset the analysis of data
dat <- subset(dat, day > 30 & day < 365*4-30)
