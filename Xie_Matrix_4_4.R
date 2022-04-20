library(ggplot2)
input_df = read.csv("data\\example_data.csv", header = FALSE, stringsAsFactors = FALSE)


# info about input_df
categories <- input_df[2,3:ncol(input_df)]     # categories to be analyzed

# check extent size, category labels, and number of category 
if (length(input_df[,2])%%(length(categories)+2) == 0) {                                                #check number of category
  row_categories <- list()
  col_categories <- list()
  index_row_num <- 3
  index_col_num <- 2
  check_sum <- sum(as.numeric(unname(unlist(input_df[3:(length(categories)+2),3:ncol(input_df)]))))
  for (x in 1:(length(input_df[,2])/(length(categories)+2))){
    row_categories[[x]] <- as.vector(input_df[index_row_num:((length(categories)+index_row_num)-1),2])  #list all row category
    col_categories[[x]] <- unname(unlist(input_df[index_col_num,3:(length(categories)+2)]))             #list all column category
    temp_sum <- sum(as.numeric(unname(unlist(input_df[index_row_num:((length(categories)+index_row_num)-1),3:ncol(input_df)]))))
    if (temp_sum != check_sum){                                                                         #check extent sizes
      stop("Extent sizes not matching")
    }
    index_row_num <- index_row_num+(length(categories)+2)
    index_col_num <- index_col_num+(length(categories)+2)
  }
  if (length(setdiff(row_categories,col_categories))==0 && length(setdiff(col_categories, row_categories))==0){ #check category labels
    print('Format checked')
  }else{
    stop("Categories not matching")
  }
  
} else {
  stop('Spreadsheet tables columns/rows not matching')
}
# create empty dataframe to be used by ggplot 
basic_df <- data.frame(Time1 = c(''),          # interval start
                       Time2 = c(''),          # interval end
                       persistence = c(''),    # diagonal value (used to create X @ persistence)
                       percent = c(''),        # data of interest
                       intensity = c(''),      # data of interest
                       label = c(''),          # interval label
                       weight = c(''))         # weight factor (used to calculate summary)
matrix_df <- basic_df[-c(1),]                  # remove first blank line (dataframe will be empty)

# populate dataframe to be plotted - matrix_df
r_count <- 0                                                                       # initialize counter
time_total <- 0                                                                    # initialize time interval
for (i in 1:length(input_df[,2])) {                                                # loop thru each row of input_df
  if (is.element(input_df[i,2], c("")) & !is.element(input_df[i,3], categories)) { # row has Y date
    time_2 <- input_df[i,3]                                                        # Y date
    time_1 <- input_df[(i+2),1]                                                    # X date
    time_gap <- as.numeric(time_2) - as.numeric(time_1)                            # time interval
    time_total <- time_total + time_gap                                            # increment time_total
    next
  }
  if (is.element(input_df[i,2], c("")) & is.element(input_df[i,3], categories)) {  # if row has Y categories (new matrix)
    temp_df <- input_df[(i+1):(i+length(categories)), c(3:(length(categories)+2))] # temporary df for current matrix
    next                                                                           # skip to next row
  }
  # rows with data
  for (y in 1:length(categories)) {                                                # loop through categories
    if (y == 1) {r_count <- r_count + 1}                                           # increment counter
    # populate category names to var1 & var2 columns
    matrix_df[nrow(matrix_df) + 1,'Time1'] <- input_df[i,2]                        # write Time1 category column 
    matrix_df[nrow(matrix_df),'Time2'] <- categories[y]                            # write Time2 category column
    # populate interval label to time column
    matrix_df[nrow(matrix_df),'label'] <- paste(time_1,time_2, sep="-")            # write matrix label
    # populate weighting factor
    matrix_df[nrow(matrix_df), 'weight'] <- time_gap                               # weight used to calculate summary values
    # calculate & populate percentage column
    matrix_df[nrow(matrix_df),'percent'] <-                                        # write percentage
      ((as.numeric(temp_df[r_count, y])) /                                         # cell
         (time_gap*sum(as.numeric(unlist(temp_df))))) * 100                           # time interval * sum of extent
    matrix_df[nrow(matrix_df),'intensity'] <-                                      # write intensity
      (as.numeric(temp_df[r_count, y])) /                                          # cell
      (time_gap*sum(as.numeric(temp_df[r_count,]))) -                      # time interval * sum of row
      (((sum(as.numeric(temp_df[,y])) -                                            # sum of column
           as.numeric(temp_df[y,y])) /                                                  # diagonal (agreement)
          (time_gap *                                                                  # time interval
             (sum(as.numeric(unlist(temp_df))) -                                          # sum of dataframe (extent)
                sum(as.numeric(temp_df[,y]))))) )                                       # sum of column
    if (matrix_df[nrow(matrix_df),'Time1'] == matrix_df[nrow(matrix_df),'Time2']) {# write diagonal labels
      matrix_df[nrow(matrix_df),'persistence'] <- 1                                # used to create X @ persistence
      matrix_df[nrow(matrix_df),'percent'] <- 0                                    # persistence not visualized
      matrix_df[nrow(matrix_df),'intensity'] <- 0                                  # persistence not visualized
    } # end write diagonal labels 
  }   # end loop y
  if (r_count == length(categories)) {r_count <- 0}                                # reset counter
}     # end loop i

# create summary matrix
n_charts <- (length(matrix_df[,1])/(length(categories)^2) -1)                     # set number of charts (this changes by end of loop)
for (s in 1:(length(categories)^2)) {                                             # loop # of lines for each chart
  matrix_df[nrow(matrix_df) + 1,'Time1'] <- matrix_df[s,'Time1']                  # write Time1 on new line
  matrix_df[nrow(matrix_df),'Time2'] <- matrix_df[s,'Time2']                      # write Time2
  matrix_df[nrow(matrix_df),'label'] <- 'Weighted Average'                            # write label
  pp <- 0                                                                         # sum of percent set to 0
  ii <- 0                                                                         # sum of intensity set to 0
  for (t in 0:n_charts) {                   # loop thru charts
    if (identical(t,0)) {                                                         # check if new set of values
      pp <- 0                                                                     # sum of percent set to 0
      ii <- 0                                                                     # sum of intensity set to 0
    }
    pp <- (as.numeric(matrix_df[(s + (t*(length(categories)^2))),'percent']) *
             (as.numeric(matrix_df[(s + (t*(length(categories)^2))),'weight']) /
                time_total)) + pp                                                      # add weighted value - percent
    
    ii <- (as.numeric(matrix_df[(s + (t*(length(categories)^2))),'intensity']) *
             (as.numeric(matrix_df[(s + (t*(length(categories)^2))),'weight']) /
                time_total)) + ii                                                      # add weighted value - intensity
    matrix_df[nrow(matrix_df),'percent'] <- pp                                   # write summary percentage
    matrix_df[nrow(matrix_df),'intensity'] <- ii                                 # write summary intensity 
    if (matrix_df[nrow(matrix_df),'Time1'] == matrix_df[nrow(matrix_df),'Time2']) {# write diagonal labels to summary
      matrix_df[nrow(matrix_df),'persistence'] <- 1                                # used to create X @ persistence
      matrix_df[nrow(matrix_df),'percent'] <- 0                                    # persistence not visualized
      matrix_df[nrow(matrix_df),'intensity'] <- 0                                  # persistence not visualized
    } # end write diagonal labels 
  }   # end loop s
}     # end loop t


# organize data frame formatting
final_df <- matrix_df[1:64,]
final_df$percent <- as.numeric(final_df$percent)
final_df$intensity <- as.numeric(final_df$intensity)
final_df$persistence <- as.integer(final_df$persistence)
final_df$weight <- as.numeric(final_df$weight)
sapply(final_df, class)
final_df


# breaks for percentage legend
l <- seq(min(final_df$percent[final_df$percent > 0]), max(final_df$percent), by = (max(final_df$percent)-min(final_df$percent[final_df$percent > 0]))/3)
l <- sapply(l,round,2)


legend_break <- max(abs(max(final_df$intensity)), abs(min(final_df$intensity)))
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
ggsave('Transition_Matrix_figure_13.png', height = 10, width = 10)
