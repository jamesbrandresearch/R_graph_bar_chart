#graph making

#use these library packages
library(ggplot2)
library(plyr)

#subset data into different information
#this makes a data frame called 'example_data_subset', which is a subset of the data from 'example_data'.
#the data is subsetted by a specified level ('level_name') from a specified factor ('variable_1')
#make as many subsetted data frames as you need, so in my example I had 'same_data' and 'different_data'

example_data_subset1 <- subset(example_data, variable_1 == "level_name1")

#aggregate the data to get mean, se and other statistics
#ddply gives you statistics of your dependent variable by separate variables
#'variable2' here could be the variable on the x axis
#'variable3' here could be the variable you want the separate bars to represent
#this will give you a data frame that looks something like this

#variable1	variable2	N	mean	sd	se
#level1		level1		x	x		x	x
#level2		level1		x	x		x	x
#level1		level2		x	x		x	x
#level2		level2		x	x		x	x

data_subset1_aggregate <- ddply(example_data_subset1, c("variable2", "variable3"),
                                summarise,
                                N = length(dependent_variable),
                                mean = mean(dependent_variable),
                                sd = sd(dependent_variable),
                                se = sd / sqrt(N))

#make the graph
#this might be a bit complicated but it has a bit of extra formatting
#it uses the aggregated data to give the mean for each level of 'variable2' with different bars for 'variable3'
#main things you might want to change
#coord_cartesian(ylim=c(0.45, 0.85)) this changes the limits of the y axis
#scale_y_continuous(breaks=seq(0.45, 0.80, 0.05)) i think this does the same but specifies the increments, here it is 0.05
#scale_fill_manual(name="variable3",
#breaks=c("c", "i"),
#labels=c("Congruent", "Incongruent"),
#values = c("Grey", "Black"))
#this code gives you a legend that you can edit to describe what 'varaible3' shows, the breaks line is how the data is coded in the file, the labels line is what you want each one to be called
#the annotate lines give you significance indicators


data_subset1_plot <- ggplot(data_subset1_aggregate, aes(x=variable2, y=mean, fill=variable3)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=.3,
                width=.2,
                position=position_dodge(.9)) + #I think this makes them centered
  coord_cartesian(ylim=c(0.45, 0.85)) + #set the y-axis scale
  scale_y_continuous(breaks=seq(0.45, 0.80, 0.05)) + #also set the y-axis scale and specify intervals
  xlab("name of variable2") +
  ylab("mean for dependent variable") +
  scale_fill_manual(name="variable3", #make a legend
                    breaks=c("c", "i"), #this is how the data is coded in the data frame
                    labels=c("Congruent", "Incongruent"), #this is how the legend should be coded
                    values = c("Grey", "Black")) + #specify the colour you want the levels to be
  annotate("text", x = 2, y = 0.75, label = "***") + #significance indicator (text only), you have to specify where on the figure you want it though
  annotate("segment", x = 1.75, xend = 2.25, y = 0.73, yend = 0.73, #this is the line of the significance
           colour = "black") +
  annotate("text", x = 3, y = 0.75, label = "***") +
  annotate("segment", x = 2.75, xend = 3.25, y = 0.73, yend = 0.73,
           colour = "black") +
  theme_bw()

#save the plot
#this gives you a .png file in your working directory

ggsave(plot=name_of_your_plot, file = "chose_a_name_for_your_plot.png", dpi=600)
