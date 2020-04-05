# convert union density/ coverage data into initial panel format, as with port_income and profits

install.packages("xlsx")
install.packages('plm')
library('rJava')
library('xlsxjars')
library('xlsx')
library('plm')

######### Collapse or Expand Retail Trade and Information Industries


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


industry_stat_get <- function(CIC_get, CIC_list)  
  # CIC_get is a list containining all values of a NAICS-CIC index row in CIC_get
  # CIC_list is a union_stats frame for a given year, MUST BE IN STANDARD FORM
  # returns a frame with the agglomerated union stat values for all CIC in a given NAICS industry index
  
{
  all_CIC <- data.frame("CIC" = NULL, "EMPLYD" = NULL, "MEMBRS" = NULL, "COVRD" = NULL) #define empty data frame with standard union_stats variables
  
  for (row_get in CIC_get) # iterate through CIC_get CIC codes
  {
    for(row_list in 1:nrow(CIC_list) ) #iterate through rows of CIC_list containing union stat variables per CIC
    {
      if(is.na(row_get)!= TRUE & 
         row_get == CIC_list[row_list,"CIC"])
        
      {
        all_CIC <- rbind(all_CIC, CIC_list[row_list,])
      } 
    }
  }
  
  all_CIC <- colSums(all_CIC)
  
  return(all_CIC)
}



year_density_frame_builder <- function(CIC_table, union_stats)
  #returns a frame containing union statistics for all industries in a given year
{
  stats_frame <- data.frame()
  CIC_table <- frame_column_name_cleaner(CIC_table)
  for(NAICS_index in 1:41)
    #ncol(CIC_table))
  {
    # print('union stats')
    #print(union_stats)
    #print(dim(union_stats))
    
    
    industry_stats <- industry_stat_get(CIC_table[,NAICS_index], union_stats)
    #if(industry_stats == 0){ 
    # break }
    
    #DEBUGGING
    #print(industry_stats)
    
    industry_stats <- data.frame("INDSTRY" = industry_stats[[1]], "EMPLYD" = industry_stats[[2]], "MEMBRS" = industry_stats[[3]], "COVRD" = industry_stats[[4]])
    industry_stats$INDSTRY <- colnames(CIC_table[NAICS_index])
    row.names(industry_stats) <- industry_stats$INDSTRY 
    industry_stats$DENSITY <- (industry_stats$MEMBRS / industry_stats$EMPLYD) #change this line to calculate density vs coverage
    industry_stats$INDSTRY<- NULL
    industry_stats$MEMBRS<- NULL
    industry_stats$COVRD <- NULL
    industry_stats$EMPLYD<- NULL
    stats_frame <- rbind(stats_frame, industry_stats)
    # DEBUGGING
    # print(industry_stats)
  }
  return(t(stats_frame))
}




year_coverage_frame_builder <- function(CIC_table, union_stats)
  #returns a frame containing union statistics for all industries in a given year
{
  stats_frame <- data.frame()
  CIC_table <- frame_column_name_cleaner(CIC_table)
  for(NAICS_index in 1:41)
    #ncol(CIC_table))
  {
    # print('union stats')
    #print(union_stats)
    #print(dim(union_stats))
    
    industry_stats <- industry_stat_get(CIC_table[,NAICS_index], union_stats)
    #if(industry_stats == 0){ 
    # break }
    
    #DEBUGGING
    #print(industry_stats)
    
    industry_stats <- data.frame("INDSTRY" = industry_stats[[1]], "EMPLYD" = industry_stats[[2]], "MEMBRS" = industry_stats[[3]], "COVRD" = industry_stats[[4]])
    industry_stats$INDSTRY <- colnames(CIC_table[NAICS_index])
    row.names(industry_stats) <- industry_stats$INDSTRY 
    industry_stats$COVRGE <- (industry_stats$COVRD / industry_stats$EMPLYD) #change this line to calculate density vs coverage
    industry_stats$INDSTRY<- NULL
    industry_stats$MEMBRS<- NULL
    industry_stats$COVRD <- NULL
    industry_stats$EMPLYD<- NULL
    stats_frame <- rbind(stats_frame, industry_stats)
    # DEBUGGING
    # print(industry_stats)
  }
  return(t(stats_frame))
}


union_density_panel_builder_lists_only <- function(list_of_years, CIC_conversion_index)
  # iterates through a list CIC union statistics for years (list indexes) data and append to new data frame that takes the form of the profits and portfolio income frames
  # frames returned from this function must be stacked to form master panel for coverage and density
  # use CIC conversion indexes in cic_naics XLXS file
  
{
  
  union_panel <- year_density_frame_builder(CIC_conversion_index, list_of_years[[1]])
  print('print first density year')
  for(union_year in list_of_years[-1]){
    union_year <- year_density_frame_builder(CIC_conversion_index, union_year)
    print('built density year')
    union_panel <- rbind(union_panel, union_year[[1]])
  }
  
  return(union_panel)
}


union_coverage_panel_builder_lists_only <- function(list_of_years, CIC_conversion_index)
  # iterates through a list CIC union statistics for years (list indexes) data and append to new data frame that takes the form of the profits and portfolio income frames
  # frames returned from this function must be stacked to form master panel for coverage and density
  # use CIC conversion indexes in cic_naics XLXS file
  
{
  
  union_panel <- year_coverage_frame_builder(CIC_conversion_index, list_of_years[[1]])
  print('built first coverage year')
  for(union_year in list_of_years[-1]){
    union_year <- year_coverage_frame_builder(CIC_conversion_index, union_year)
    print('built coverage year')
    union_panel <- rbind(union_panel, union_year[[1]])
    
  }
  
  return(union_panel)
}


################################################   LOAD RAW FRAMES/ LISTS OF FRAMES     #######################################


union__stats_98 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_1998.xls', 'Sheet2') 
union__stats_99 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_1999.xls', 'Sheet1') 
union__stats_00 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2000.xls', 'Sheet1') 
union__stats_01 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2001.xls', 'Sheet1') 
union__stats_02 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2002.xls', 'Sheet1')
union__stats_03 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2003.xls', 'Sheet1') 
union__stats_04 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2004.xls', 'Sheet1') 
union__stats_05 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2005.xls', 'Sheet1') 
union__stats_06 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2006.xls', 'Sheet1') 
union__stats_07 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2007.xls', 'Sheet1') 
union__stats_08 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2008.xls', 'Sheet1') 
union__stats_09 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2009.xls', 'Sheet1') 
union__stats_10 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2010.xlsx', 'Sheet1') 
union__stats_11 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2011.xlsx', 'Sheet1') 
union__stats_12 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2012.xlsx', 'Sheet1') 
union__stats_13 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2013.xlsx', 'Sheet1') 
union__stats_14 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2014.xlsx', 'Sheet1') 
union__stats_15 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2015.xlsx', 'Sheet1') 
union__stats_16 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2016.xlsx', 'Sheet1') 
union__stats_17 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2017.xlsx', 'Sheet1')
union__stats_18 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/union_density/density_2018.xlsx', 'Sheet1') 


union_stats_98_02 <- list(
  union__stats_98 , 
  union__stats_99 ,
  union__stats_00 , 
  union__stats_01 ,
  union__stats_02  )

union_stats_03_10 <- list(
  union__stats_03  ,
  union__stats_04  ,
  union__stats_05  ,
  union__stats_06  ,
  union__stats_07  ,
  union__stats_08 ,
  union__stats_09 ,
  union__stats_10  )

union_stats_11_15 <- list(
  union__stats_11  ,
  union__stats_12  ,
  union__stats_13  )


conversion_table_98_02 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/cic_naics.xlsx', '98-02_collapsed')
conversion_table_03_11 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/cic_naics.xlsx', '03-11_collapsed')
conversion_table_12_13 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/cic_naics.xlsx', '12-17_collapsed')


#conversion_table_00_01 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/cic_naics.xlsx', '00-01')
#conversion_table_02_11 <- read.xlsx('C:/Users/jbwav/Desktop/r_honors/cic_naics.xlsx', '02-11')

################################################   EXECUTION   #################################################################

density_panel_98_02 <- union_density_panel_builder_lists_only(union_stats_98_02, conversion_table_98_02)

density_panel_03_10 <- union_density_panel_builder_lists_only(union_stats_03_10 , conversion_table_03_11)

density_panel_11_13 <- union_density_panel_builder_lists_only(union_stats_11_15 , conversion_table_03_11)


coverage_panel_98_02 <- union_coverage_panel_builder_lists_only(union_stats_98_02, conversion_table_98_02)

coverage_panel_03_10 <- union_coverage_panel_builder_lists_only(union_stats_03_10 , conversion_table_03_11)

coverage_panel_11_13 <- union_coverage_panel_builder_lists_only(union_stats_11_15 , conversion_table_03_11)


new_panel2 <- merge(density_panel_98_02, density_panel_03_10, all = TRUE, sort = FALSE)
density_panel <- merge(new_panel2, density_panel_11_13, all = TRUE, sort = FALSE)
dim(density_panel)

new_panel1 <- merge(coverage_panel_98_02, coverage_panel_03_10, all = TRUE, sort = FALSE)
coverage_panel <- merge(new_panel1, coverage_panel_11_13, all = TRUE, sort = FALSE)


#union_density_panel_builder_single_year(union__stats_03, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_04, conversion_table_02_11)
#union_density_panel_builder_single_year(union__stats_05, conversion_table_02_11)
#union_density_panel_builder_single_year(union__stats_07, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_08, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_09, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_10, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_11, conversion_table_03_11)
#union_density_panel_builder_single_year(union__stats_12, conversion_table_12_17)
#union_density_panel_builder_single_year(union__stats_13, conversion_table_12_17)
#union_density_panel_builder_single_year(union__stats_14, conversion_table_12_17)
#union_density_panel_builder_single_year(union__stats_15, conversion_table_12_17)


