# Xie_Matrix
# Sam & Tom
# 1 Apr 2022
# pontiPy project
# test git connection on desktop

library(ggplot2)

# ------------------user input------------------------------
input_df = read.csv("data\\raw_data_text.csv")
time_1 = 1998
time_2 = 2000
# ----------------------------------------------------------

# example
# transition_matrix(data.csv,time1,time2,analysis_category)
# transition_matrix(data.csv,1998,2000)

# calculate time interval
time_gap = time_2 - time_1

# populate dataframe to be plotted
for (i in 1:length(x_categories)) {
  for (y in 1:length(y_categories)) {
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
