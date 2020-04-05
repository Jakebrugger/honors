install.packages("xlsx")
install.packages('plm')
install.packages('mice')
library('rJava')
library('xlsxjars')
library('xlsx')
library('plm')

#########################################  FUNCTIONS - MUST BE READ TO MEMORY IN ORDER CUZ IM LAZY ABOUT SUB-FUNCTIONING  ####################################

letter_remover <- function(string)
  #removes letter characters from a string, leaving only numeric characters
  #returns a numeric character string
{
  chars <- strsplit(string, '')
  new_chars<- c()
  for(char in chars[[1]]){
    if(
      char == '1'|
      char == '2'|
      char == '3'|
      char == '4'|
      char == '5'|
      char == '6'|
      char == '7'|
      char == '8'|
      char == '9'|
      char == '.'|
      
      char == '0'  ) {new_chars <- c(new_chars, char)
    }
  }
  return_string <- list()
  for(i in new_chars){
    return_string <- paste(return_string, i, sep = '')
  }
  return(return_string)
}


frame_column_name_cleaner <- function(frame)
  #converts column names from read.xlsx files into integer form for easy reference to their respective NAICS value indicators
  #returns cleaned input frame
{
  for(i in 1:ncol(frame))
  {
    colnames(frame)[i] <- letter_remover(colnames(frame)[i])
  }
  return(frame)
}



frame_divider <- function(frame_1, frame_2) {
  # divide frame1 by frame2 (frame1/frame2)
  # frames must be the same size
  dim1 <- dim(frame_1)
  print(dim1)
  dim2 <- dim(frame_2)
  print(dim2)
  ifelse(dim1 == dim2,  print('check') , print('frame size error'))
  
  output_frame <- NULL
  for(i in 1:nrow(frame_1)){
    for(j in 1:ncol(frame_1)){
      frame_1[i,j] <- frame_1[i,j] / frame_2[i,j]
    }
  }
  return(frame_1)
}


################################### DATA LOADING ##############################################

port_income_panel <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/panels/NAICS_port_income_panel.xlsx', 'expanded') 

profits_panel <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/panels/NAICS_profits_panel.xlsx', 'expanded')

depreciation_panel <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/panels/NAICS_depreciation_panel.xlsx', 'expanded')



profits_panel <- frame_column_name_cleaner(profits_panel)

port_income_panel <- frame_column_name_cleaner(port_income_panel)

depreciation_panel <- frame_column_name_cleaner(depreciation_panel)




final_profits_panel <- profits_panel + depreciation_panel

portfolio_profits_ratio_depreciation <- frame_divider(port_income_panel, final_profits_panel)

portfolio_profits_ratio <- frame_divider(port_income_panel, profits_panel)

