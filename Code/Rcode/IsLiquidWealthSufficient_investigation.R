

###############################################################################
# load liquid weath DECILE data and create graph
num_quantiles = 10
T=12
category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(moments_dir,'IsLiquidWealthSufficient/moments_by_liquid_wealth_quantile',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(moments_dir,'IsLiquidWealthSufficient/moments_by_liquid_wealth_quantile',i,'_omega','.txt',sep=''), header = FALSE))
  this_CS_output = CS_parameter_estimation(this_c_vector, this_omega,T)
  category_params[i,1] = this_CS_output$var_perm
  category_params[i,2] = this_CS_output$var_tran
  category_params[i,3] = this_CS_output$ins_perm
  category_params[i,4] = this_CS_output$ins_tran
  category_se[i,1] = this_CS_output$var_perm_se
  category_se[i,2] = this_CS_output$var_tran_se
  category_se[i,3] = this_CS_output$ins_perm_se
  category_se[i,4] = this_CS_output$ins_tran_se
}
  
liquid_wealth_decile_params = category_params


category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(moments_dir,'IsLiquidWealthSufficient/moments_by_liquid_to_perm_quantile',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(moments_dir,'IsLiquidWealthSufficient/moments_by_liquid_to_perm_quantile',i,'_omega','.txt',sep=''), header = FALSE))
  this_CS_output = CS_parameter_estimation(this_c_vector, this_omega,T)
  category_params[i,1] = this_CS_output$var_perm
  category_params[i,2] = this_CS_output$var_tran
  category_params[i,3] = this_CS_output$ins_perm
  category_params[i,4] = this_CS_output$ins_tran
  category_se[i,1] = this_CS_output$var_perm_se
  category_se[i,2] = this_CS_output$var_tran_se
  category_se[i,3] = this_CS_output$ins_perm_se
  category_se[i,4] = this_CS_output$ins_tran_se
}

liquid_wealth_to_perm_inc_decile_params = category_params
param_num = 4 #3 permanent, 4 transitory
MPC_tran_predict = approxfun(c(1000,2000,3200,4100,7000,10000,16000,23000, 35000,100000),liquid_wealth_decile_params[,param_num],rule=2)
MPC_tran_predict_lw2perminc = approxfun(c(1000,2000,3200,4100,7000,10000,16000,23000, 35000,100000),liquid_wealth_to_perm_inc_decile_params[,param_num],rule=2)



inc_decile_stats = read.csv(paste(moments_dir,'IsLiquidWealthSufficient/inc_decile_stats1.txt',sep=''))
inc_decile_MPC_predict = MPC_tran_predict(as.matrix(inc_decile_stats['liquidassets_adj_p50'])/6.7)

URE_decile_stats = read.csv(paste(moments_dir,'IsLiquidWealthSufficient/URE_decile_stats1.txt',sep=''))
URE_decile_MPC_predict = MPC_tran_predict(as.matrix(URE_decile_stats['liquidassets_adj_p50'])/6.7)
URE_decile_estimates = URE_quantile_params[,param_num]
colors = c("#91bfdb","#ffffbf")

barCenters <- barplot(height=t(cbind(URE_decile_MPC_predict,URE_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="URE Deciles")
this_legend=c("Based on Liquid Assets","Actual Estimate")
legend(1, 1, legend=this_legend, fill=colors,bty="n")
mean_MPC = mean(URE_decile_estimates)
mean_sq_error = mean((URE_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((URE_decile_estimates-URE_decile_MPC_predict)^2)
R2_URE = 1- mean_sq_prediction_error/mean_sq_error
std_dev_URE = mean_sq_error^0.5
std_dev_pred_URE = mean_sq_prediction_error^0.5


NNP_decile_stats = read.csv(paste(moments_dir,'IsLiquidWealthSufficient/NNP_decile_stats1.txt',sep=''))
NNP_decile_MPC_predict = MPC_tran_predict(as.matrix(NNP_decile_stats['liquidassets_adj_p50'])/6.7)
NNP_decile_estimates = NNP_quantile_params[,param_num]
barCenters <- barplot(height=t(cbind(NNP_decile_MPC_predict,NNP_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="NNP Deciles")
this_legend=c("Based on Liquid Assets","Actual Estimate")
legend(1, 1, legend=this_legend, fill=colors,bty="n")

mean_MPC = mean(NNP_decile_estimates)
mean_sq_error = mean((NNP_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((NNP_decile_estimates-NNP_decile_MPC_predict)^2)
R2_NNP = 1- mean_sq_prediction_error/mean_sq_error
std_dev_NNP = mean_sq_error^0.5
std_dev_pred_NNP = mean_sq_prediction_error^0.5


Inc_decile_stats = read.csv(paste(moments_dir,'IsLiquidWealthSufficient/inc_decile_stats1.txt',sep=''))
Inc_decile_MPC_predict = MPC_tran_predict(as.matrix(Inc_decile_stats['liquidassets_adj_p50'])/6.7)
Inc_decile_estimates = Income_quantile_params[,param_num]
barCenters <- barplot(height=t(cbind(Inc_decile_MPC_predict,Inc_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="Income Deciles")
this_legend=c("Based on Liquid Assets","Actual Estimate")
legend(1, 1, legend=this_legend, fill=colors,bty="n")

mean_MPC = mean(Inc_decile_estimates)
mean_sq_error = mean((Inc_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((Inc_decile_estimates-Inc_decile_MPC_predict)^2)
R2_Inc = 1- mean_sq_prediction_error/mean_sq_error
std_dev_Inc = mean_sq_error^0.5
std_dev_pred_Inc = mean_sq_prediction_error^0.5


Con_decile_stats = read.csv(paste(moments_dir,'IsLiquidWealthSufficient/con_decile_stats1.txt',sep=''))
Con_decile_MPC_predict = MPC_tran_predict(as.matrix(Con_decile_stats['liquidassets_adj_p50'])/6.7)
Con_decile_estimates = MeanCons_quantile_params[,param_num]
barCenters <- barplot(height=t(cbind(Con_decile_MPC_predict,Con_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="Consumption Deciles")
this_legend=c("Based on Liquid Assets","Actual Estimate")
legend(1, 1, legend=this_legend, fill=colors,bty="n")

mean_MPC = mean(Con_decile_estimates)
mean_sq_error = mean((Con_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((Con_decile_estimates-Con_decile_MPC_predict)^2)
R2_Con = 1- mean_sq_prediction_error/mean_sq_error
std_dev_Con = mean_sq_error^0.5
std_dev_pred_Con = mean_sq_prediction_error^0.5



