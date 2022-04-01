# Xie_Matrix
# Tom Francis
# 25 March 2022
# pontiPy project
# test of RStudio connection to GitHub

library(ggplot2)

input_df = read.csv("data/raw_data.csv", header = FALSE, stringsAsFactors = FALSE)

x_categories <- input_df[3:nrow(input_df),'V2']
y_categories <- input_df[2,3:ncol(input_df)]

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
for (i in 1:length(x_categories)) {
  for (y in 1:length(y_categories)) {
    # populate category names to var1 & var2 columns
    matrix_df[nrow(matrix_df) + 1,'Time1'] <- x_categories[i]
    matrix_df[nrow(matrix_df),'Time2'] <- y_categories[y]
    # populate interval label to time column
    matrix_df[nrow(matrix_df),'label'] <- paste(input_df[3,1],input_df[1,3], sep="-")
    # calculate & populate percentage column
    matrix_df[nrow(matrix_df),'percent'] <- as.numeric(input_df[i+2,y+2]) / sum(as.numeric(input_df[i + 2, 3:nrow(input_df)])) 
    # re-populate diagonals = 0
    if (matrix_df[nrow(matrix_df),'Time1'] == matrix_df[nrow(matrix_df),'Time2']) {
      matrix_df[nrow(matrix_df),'persistence'] <- 1
      matrix_df[nrow(matrix_df),'percent'] <- 0
    }
    # calculate intensity
    v_int <- as.numeric(input_df[1,3]) - as.numeric(input_df[3,1])                                  # number of periods in interval
    v_sum <- sum(rowSums(data_df))                                                                  # sum of extent
    v_r <- as.numeric(input_df[i+2,y+2]) / (v_int * sum(as.numeric(input_df[3,3:nrow(input_df)])))  # Rtij   
    
    v_w1 <- as.numeric(input_df[i+2,y+2]) / 
      (v_int * sum(as.numeric(input_df[(i+2),3:nrow(input_df)])))                                   # first term in Wtj       
    
    v_w2 <- (sum(as.numeric(input_df[3:ncol(input_df),(y+2)])) - as.numeric(input_df[(y+2),(y+2)])) /
      (v_int * (sum(as.numeric(input_df[3:ncol(input_df),(y+2)])) - v_sum))                         # second term in Wtj
    
    matrix_df[nrow(matrix_df),'intensity'] <- v_r - (v_w1/v_w2)                                     # intensity deviation
    
    # QA
    cat('Cell value for row', i, 'and column', y, ': ', input_df[i+2,y+2], '\n')
    cat('Sum of row:', sum(as.numeric(input_df[(i+2),3:nrow(input_df)])),'\n')
    cat('Sum of column:', sum(as.numeric(input_df[3:ncol(input_df),(y+2)])),'\n')
    cat('Persistence:', as.numeric(input_df[(y+2),(y+2)]), '\n')
    #print(v_w1)
    #print(v_w2)
  }
}

final_df <- matrix_df[2:18,]
#Plot the Data
max_value<-max(as.numeric(final_df$intensity))

legend_break <- max(abs(max(as.numeric(final_df$intensity))), abs(min(as.numeric(final_df$intensity))))
g <- ggplot(final_df, aes(Var1, Var2)) + 
  geom_point(aes(size = percent, color = intensity), shape=15) + 
  geom_point(data=df_diag, aes(size = value), shape=4) + 
  theme_bw() + 
  xlab("Transition to") + 
  ylab("Transition from") + 
  scale_y_discrete(limits=rev) + 
  scale_x_discrete(position = "top") + 
  scale_color_gradient2( low="#5e3c99", mid="#f7f7f7", high="#e66101", limits=c(-legend_break,legend_break)) + 
  facet_grid(time ~ .) + 
  labs(size = "Annual Percentage", color = "Intensity Deviation") + 
  theme(axis.text.x = element_text(angle = 90), aspect.ratio=1)

g + scale_size_continuous(range=c(0,12)) + 
  guides(
    color = guide_colorbar(order = 0),
    size = guide_legend(order = 1)
  )

