# temporary utility for deleting and recreating blank matrix_df
# allows section of code that creates summary chart data
# to run correctly without running portions of main code block

# create empty dataframe to be used by ggplot 
basic_df <- data.frame(Time1 = c(''),          # interval start
                       Time2 = c(''),          # interval end
                       persistence = c(''),    # diagonal value (was diagonal_label)
                       percent = c(''), 
                       intensity = c(''),
                       label = c(''),          # interval label
                       weight = c(''))         # weight factor
matrix_df <- basic_df[-c(1),]                  # remove first blank line

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
  if (is.element(input_df[i,2], c("")) & is.element(input_df[i,3], categories)) {  # row has Y categories
    temp_df <- input_df[(i+1):(i+length(categories)), c(3:(length(categories)+2))] # temporary df for current matrix
    next                                                                           # skip to next row
  }
  # rows with data
  for (y in 1:length(categories)) {                                                # loop through categories
    if (y == 1) {r_count <- r_count + 1}                                           # increment counter
    # populate category names to var1 & var2 columns
    matrix_df[nrow(matrix_df) + 1,'Time1'] <- input_df[i,2]                        # write Time1 column
    matrix_df[nrow(matrix_df),'Time2'] <- categories[y]                            # write Time2 column
    # populate interval label to time column
    matrix_df[nrow(matrix_df),'label'] <- paste(time_1,time_2, sep="-")            # write matrix label
    # populate weighting factor
    matrix_df[nrow(matrix_df), 'weight'] <- time_gap
    # calculate & populate percentage column
    matrix_df[nrow(matrix_df),'percent'] <-                                        # write percentage
      ((as.numeric(temp_df[r_count, y])) /                                         # cell
         (time_gap*sum(as.numeric(unlist(temp_df))))) * 100                           # time interval * sum of extent
    matrix_df[nrow(matrix_df),'intensity'] <-                                      # write intensity
      (as.numeric(temp_df[r_count, y])) /                                          # cell
      ((time_gap*sum(as.numeric(temp_df[r_count,]))) * 100) -                      # time interval * sum of row
      (((sum(as.numeric(temp_df[y,])) -                                            # sum of column
           as.numeric(temp_df[y,y])) /                                                  # diagonal (agreement)
          (time_gap *                                                                  # time interval
             (sum(as.numeric(unlist(temp_df))) -                                          # sum of dataframe (extent)
                sum(as.numeric(temp_df[y,]))))) * 100)                                       # sum of column
    if (matrix_df[nrow(matrix_df),'Time1'] == matrix_df[nrow(matrix_df),'Time2']) {# write diagonal labels
      matrix_df[nrow(matrix_df),'persistence'] <- 1
      matrix_df[nrow(matrix_df),'percent'] <- 0
      matrix_df[nrow(matrix_df),'intensity'] <- 0
    } # end write diagonal labels 
  }   # end loop y
  if (r_count == length(categories)) {r_count <- 0}                                # reset counter
}     # end loop i 
