install.packages("xlsx")
install.packages('plm')
library('rJava')
library('xlsxjars')
library('xlsx')
library('plm')


portfolio_profits_ratio_depreciation



t_dum_builder <- function(year){
  # construct a repeating list of dummy variables for each year
  dummy_list <- rep(0, times = 16)
  dummy_list[year] <- 1
  return_list <- rep(dummy_list, times = 41)
  return(return_list)
}


panel_stacker <- function(panel){
  return_frame <- data.frame(stack(panel[1:ncol(panel)]))
  return(return_frame$values)
}




id_list <-  rep(1, times = 16)
for(i in 2:41){
  id_list = c(id_list, rep(i, times = 16))
}


time_list <- rep((1:16), times = 41)
time_list


tdum1 <- t_dum_builder(1)
tdum2 <- t_dum_builder(2)
tdum3 <- t_dum_builder(3)
tdum4<- t_dum_builder(4)
tdum5<- t_dum_builder(5)
tdum6<- t_dum_builder(6)
tdum7<- t_dum_builder(7)
tdum8<- t_dum_builder(8)
tdum9<- t_dum_builder(9)
tdum10<- t_dum_builder(10)
tdum11<- t_dum_builder(11)
tdum12<- t_dum_builder(12)
tdum13<- t_dum_builder(13)
tdum14<- t_dum_builder(14)
tdum15<- t_dum_builder(15)
tdum16<- t_dum_builder(16)


portfolio__depreciation_stack <-panel_stacker(portfolio_profits_ratio_depreciation)
coverage_stack <- panel_stacker(coverage_panel)
density_stack <- panel_stacker(density_panel)
portfolio_profits_stack <- panel_stacker(portfolio_profits_ratio)


final_panel <- data.frame("Coverage" = coverage_stack, "Density" = density_stack , "fin_ratio" = portfolio_profits_stack, 
                          'id' = id_list , 't' = time_list, 'tdum1' = tdum1,
                          'tdum2'= tdum2,
                          'tdum3'= tdum3,
                          'tdum4'= tdum4,
                          'tdum5'= tdum5,
                          'tdum6'= tdum6,
                          'tdum7'= tdum7,
                          'tdum8'= tdum8,
                          'tdum9'= tdum9,
                          'tdum10'= tdum10,
                          'tdum11'= tdum11,
                          'tdum12'= tdum12,
                          'tdum13'= tdum13,
                          'tdum14'= tdum14,
                          'tdum15'= tdum15,
                          'tdum16'= tdum16
)

final_panel_with_depreciation <- data.frame("Coverage" = coverage_stack, "Density" = density_stack , "fin_ratio" = portfolio__depreciation_stack, 
                                            'id' = id_list , 't' = time_list, 'tdum1' = tdum1,
                                            'tdum2'= tdum2,
                                            'tdum3'= tdum3,
                                            'tdum4'= tdum4,
                                            'tdum5'= tdum5,
                                            'tdum6'= tdum6,
                                            'tdum7'= tdum7,
                                            'tdum8'= tdum8,
                                            'tdum9'= tdum9,
                                            'tdum10'= tdum10,
                                            'tdum11'= tdum11,
                                            'tdum12'= tdum12,
                                            'tdum13'= tdum13,
                                            'tdum14'= tdum14,
                                            'tdum15'= tdum15,
                                            'tdum16'= tdum16
)


X<- cbind(final_panel$fin_ratio)
Y<- cbind(final_panel$Density)

pdata <- pdata.frame(final_panel, index = c('id','t'))
pdata_with_dep <- pdata.frame(final_panel_with_depreciation, index = c('id','t'))

fixed_effect <- plm(Coverage ~ fin_ratio, data = pdata, model = 'within', index=c("id","t"))
fixed_effect_with_dep <- plm(Coverage ~ fin_ratio, data = pdata_with_dep, model = 'within', index=c("id","t"))

summary(fixed_effect)
summary(fixed_effect_with_dep)
