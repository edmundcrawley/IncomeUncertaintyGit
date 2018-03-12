# Calculate mean wealth over the period and look at MPX by wealth

col_year   <- 2
liquidasset_col = 8
netwealth_col = 9
coldy_dif  <- 4
coldc_dif  <- 6

# length of panel. Assumes all individuals appear in all years
T  <- max(all_data[,col_year])-min(all_data[,col_year])+1 
y  <-nrow(all_data) 

#make one column for liquidassets and one for netwealth
mean_log_wealth = array(0.0, dim=c(y,1))

#following is very slow - is there a faster way?
for (k in 0:((y/T)-1)){
  i <- k*T
  #mean_log_wealth[(i+1):(i+T)] = sum(all_data[(i+1):(i+T),liquidasset_col][all_data[(i+1):(k+T),coldc_dif]==1])/sum(all_data[(i+1):(i+T),coldc_dif])
  mean_log_wealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),liquidasset_col],na.rm=TRUE)
}

num_quantiles =5
quantiles = seq(0,1.0,length=(num_quantiles+1))
quantile_cutoffs = quantile(mean_log_wealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
wealth_quantile = as.numeric(cut(mean_log_wealth,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))

# Do by wealth quantile 
wealth_quantile_set = as.character(1:num_quantiles)
output =params_by_subset(all_data, wealth_quantile, wealth_quantile_set,age_range)
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var

plot_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealth")


