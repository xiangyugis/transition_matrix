# Xie_Matrix
# Sam & Tom
# 1 Apr 2022
# pontiPy project
# NOT A WORKING VERSION - WORK IN PROCESS ;)

library(ggplot2)

# ------------------user input------------------------------
input_df = read.csv("data/example_data.csv", header = FALSE, stringsAsFactors = FALSE)
# ----------------------------------------------------------

# info about input_df
# x_categories <- input_df[3:nrow(input_df),'V2']     # number of rows to be looped
categories <- input_df[2,3:ncol(input_df)]        # categories

# create dataframe without text
data_df <- input_df[3:nrow(input_df),3:ncol(input_df)]
v_nc <- c(1:ncol(data_df))
data_df[,v_nc] <- apply(data_df[ ,v_nc],2,function(x) as.numeric(as.character(x)))

# create dataframe to be plotted
basic_df <- data.frame(Time1 = c(''),          # interval start
                       Time2 = c(''),         # interval end
                       persistence = c(''),   # diagonal value (was diagonal_label)
                       percent = c(''), 
                       intensity = c(''),
                       label = c(''))         # interval label
matrix_df <- basic_df[-c(1),]                  # remove first blank line

# populate dataframe to be plotted
for (i in 1:length(input_df[,2])) {
  if (is.element(input_df[i,2], c("")) & !is.element(input_df[i,3], categories)) { #row has Y date
    time_2 <- input_df[i,3]
    time_1 <- input_df[(i+2),1]
    time_gap <- as.numeric(time_2) - as.numeric(time_1)
    next
  }
  if (is.element(input_df[i,2], c("")) & is.element(input_df[i,3], categories)) {  #row has Y categories
    next
  }
  # rows with data
  for (y in 1:length(categories)) {
    # populate category names to var1 & var2 columns
    matrix_df[nrow(matrix_df) + 1,'Time1'] <- input_df[i,2]
    matrix_df[nrow(matrix_df),'Time2'] <- categories[y]
    # populate interval label to time column
    matrix_df[nrow(matrix_df),'label'] <- paste(time_1,time_2, sep="-")
    # calculate & populate percentage column
    matrix_df[nrow(matrix_df),'percent'] <- 
      (input_df[i,y+3]/(time_gap*sum(as.numeric(input_df[i,3:(2+length(categories))]))))*100
    
    
  }    # end of for y loop
}      # end of for i loop

# rest of code through QA is for reference    
    for (y in 1:length(categories)) {
      # populate category names to var1 & var2 columns
      matrix_df[nrow(matrix_df) + 1,'Time1'] <- data_df[i,2]
      matrix_df[nrow(matrix_df),'Time2'] <- categories[y]
        matrix_df[nrow(matrix_df),'percent'] <- 
        (input_df[i,y+1]/(time_gap*sum(input_df[1:length(x_categories),1:length(y_categories)+1])))*100
    }

  
  for (y in 1:length(categories)) {
    # populate category names to var1 & var2 columns
    matrix_df[nrow(matrix_df) + 1,'Time1'] <- x_categories[i]
    matrix_df[nrow(matrix_df),'Time2'] <- y_categories[y]
    # populate interval label to time column
    matrix_df[nrow(matrix_df),'label'] <- paste(time_1,time_2, sep="-")
    # calculate & populate percentage column
    matrix_df[nrow(matrix_df),'percent'] <- 
      (input_df[i,y+1]/(time_gap*sum(input_df[1:length(x_categories),1:length(y_categories)+1])))*100
    # calculate & populate intensity column
    matrix_df[nrow(matrix_df),'intensity'] <- 
      (input_df[i,y+1]/(time_gap*sum(input_df[i,1:length(y_categories)+1]))) - 
      (sum(input_df[y+1]) - input_df[y,y+1])/ (time_gap*(sum(input_df[1:length(x_categories),1:length(y_categories)+1])- sum(input_df[y+1])))
    # re-populate diagonals = 0
    if (matrix_df[nrow(matrix_df),'Time1'] == matrix_df[nrow(matrix_df),'Time2']) {
      matrix_df[nrow(matrix_df),'persistence'] <- 1
      matrix_df[nrow(matrix_df),'percent'] <- 0
      matrix_df[nrow(matrix_df),'intensity'] <- 0
    }
    # calculate intensity
    #v_int <- as.numeric(input_df[1,3]) - as.numeric(input_df[3,1])                                  # number of periods in interval
    # intensity deviation
    
    # QA
    cat('Cell value for row', i, 'and column', y, ': ', input_df[i+2,y+2], '\n')
    cat('Sum of row:', sum(as.numeric(input_df[(i+2),3:nrow(input_df)])),'\n')
    cat('Sum of column:', sum(as.numeric(input_df[3:ncol(input_df),(y+2)])),'\n')
    cat('Persistence:', as.numeric(input_df[(y+2),(y+2)]), '\n')
    #print(v_w1)
    #print(v_w2)
  }
}

# organize data frame formatting
final_df <- matrix_df[1:16,]
sapply(final_df, class)
final_df$percent <- as.numeric(final_df$percent)
final_df$intensity <- as.numeric(final_df$intensity)
final_df$persistence <- as.integer(final_df$persistence)


# breaks for percentage legend
l <- seq(min(final_df$percent[final_df$percent > 0]), max(final_df$percent), by = (max(final_df$percent)-min(final_df$percent[final_df$percent > 0]))/3)
l <- sapply(l,round,2)

# input data to plotting code
g <- ggplot(final_df, aes(Time2,Time1)) + 
  geom_point(aes(size = percent, color = intensity), shape=15) + 
  geom_point(data=final_df, aes(size =persistence), shape=4) + 
  theme_bw() + 
  xlab("Transition to") + 
  ylab("Transition from") + 
  scale_y_discrete(limits=rev) + 
  scale_x_discrete(position = "top") + 
  scale_color_gradient2( low="#91bfdb", mid="#ffffbf", high="#fc8d59", limits=c(-legend_break,legend_break)) + 
  facet_grid(label ~ .) + 
  labs(size = "Annual Percentage", color = "Intensity Deviation") + 
  theme(axis.text.x = element_text(angle = 90), aspect.ratio=1)

g + scale_size_continuous(range=c(0,12), breaks=c(l)) + guides(
  color = guide_colorbar(order = 0),
  size = guide_legend(order = 1)
)

# save the figure
ggsave('Transition_Matrix_figure.png', height = 6, width = 6)
