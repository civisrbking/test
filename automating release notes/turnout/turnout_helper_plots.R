library(RPostgreSQL)


####################
## Modifying Data ##
####################

create_plots <- function(data,state,state_abbrev) {

  # data : data frame from query
  # state: full state name
  # state_abbrev: two letter abbreviation

  require(ggplot2)
  require(scales)
  require(plyr)
  
  #### Setting a black and white theme for the plots ####
  theme_param <- theme_bw() + theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank()) + 
    theme(plot.title = element_text(size = rel(0.75)),
          axis.title.x = element_text(size = rel(0.75)),
          axis.title.y = element_text(size = rel(0.75)))
  
  #### Simple Histogram of a state's Predicted Turnout in 2016 ####
    # Change "state's" name (just for plot!) if it's national or DC.
    # Gets changed back at end of function for writing the actual document.
  if (state == "all 50 states and DC") {
    state <- "All 50 States and DC" 
    temp.data <- ddply(data, .(turnout_2016_bins), summarize, count = sum(count))
  } else if (state == "the District of Columbia") {
    state <- "District of Columbia"
    temp.data <- ddply(subset(data, data$state == state_abbrev), # Only DC
                       .(turnout_2016_bins), summarize, count = sum(count))
  } else {
    temp.data <- ddply(subset(data, data$state == state_abbrev), # Only this state
                   .(turnout_2016_bins), summarize, count = sum(count))
  }

    # Create Plot
  simple_hist <- ggplot(temp.data, 
                        aes(x = turnout_2016_bins * 100, # Range now 0 - 100
                            y = count / 1000)) +        # Thousands of obs
    geom_bar(fill = "#0194D3", colour = "#4DC0E8",
             stat = "identity", position = "identity", alpha = 0.5) +
    xlab("Turnout Score") + ylab ("Frequency (in thousands)") +
    ggtitle(paste(state,"Turnout Rates")) +
    theme_param
  
  #### Overlapping Histograms for Habitual / Sporadic / Nonvoters ####
    # Collapse Data for Histogram
    # If it's for national, keep all states + dc
  if (state == 'All 50 states and D.C.') {
    temp.data <- ddply(subset(data, data$voter_type != "Other"), 
                       .(turnout_2016_bins, voter_type), summarize, count = sum(count))
  } else {
    temp.data <- ddply(subset(data, data$voter_type != "Other" & 
                                data$state == state_abbrev), # Only this state
                       .(turnout_2016_bins, voter_type), summarize, count = sum(count))
  }
    # Identity, alpha = 0.2, fill = voter_type
  overlapping_hist <- ggplot(temp.data, 
    aes(x = turnout_2016_bins * 100, y = count / 1000, 
      fill = voter_type, group = voter_type)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
    xlab("Turnout Score") + ylab ("Frequency (in thousands)") + 
    ggtitle(paste(state,"Turnout Rates Across Voter Types")) +
    scale_fill_manual(values = c("#f4a800", # Yellow
                                 "#0194d3", # Blue
                                 "#929292"), # Gray
                      name = "Voter Type")

  #### Output Result ####
  result <- list(simple_hist = simple_hist + theme_param, 
                 overlapping_hist = overlapping_hist + theme_param)
  
  #### Changing 'state' back - necessary for writing word doc later
  if (state == 'All 50 States and DC') {
    state <- "all 50 states and DC" 
  } else if (state == 'District of Columbia') {
    state <- 'the District of Columbia'
  }
  
  return(result)
  
}
