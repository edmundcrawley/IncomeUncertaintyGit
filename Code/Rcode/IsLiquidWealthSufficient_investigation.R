require(spatstat)
require(zoo)

base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
Rcode_folder = paste(base_dir,"Code/Rcode/",sep='')
moments_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/",sep='')
txt_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/",sep='')
SCF_dir = paste(base_dir,"Code/SCF/",sep='')

source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
###############################################################################


# load liquid wealth DECILE data and create graph
num_quantiles = 10
T=12
category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(txt_dir,'moments_by_liquid_wealth_decile',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(txt_dir,'moments_by_liquid_wealth_decile',i,'_omega','.txt',sep=''), header = FALSE))
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
liquid_wealth_decile_se = category_se


category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(txt_dir,'liquid_to_income_moments_by_liquid_wealth_decile_level_lincome',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(txt_dir,'liquid_to_income_moments_by_liquid_wealth_decile_level_lincome',i,'_omega','.txt',sep=''), header = FALSE))
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

category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(txt_dir,'moments_by_net_wealth_decile',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(txt_dir,'moments_by_net_wealth_decile',i,'_omega','.txt',sep=''), header = FALSE))
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
net_wealth_decile_params = category_params

category_params = array(0, dim=c(num_quantiles,4))
category_se = array(0, dim=c(num_quantiles,4))
for (i in 1:num_quantiles){
  this_c_vector = scan(paste(txt_dir,'moments_by_Income_quantile',i,'c_vector','.txt',sep=''))
  this_omega = as.matrix(read.csv(paste(txt_dir,'moments_by_Income_quantile',i,'_omega','.txt',sep=''), header = FALSE))
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
income_decile_params = category_params



param_num = 4 #3 permanent, 4 transitory
exchange_rate = 6.87
liquid_decile_stats = read.csv(paste(txt_dir,'liquid_decile_stats1.txt',sep=''))
lw2perminc_decile_stats = read.csv(paste(txt_dir,'liquidtoinc_decile_stats1.txt',sep=''))
Inc_decile_stats = read.csv(paste(txt_dir,'inc_decile_stats1.txt',sep=''))

MPC_tran_predict =    approxfun(as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate,liquid_wealth_decile_params[,param_num],rule=2)
MPC_tran_predict_lw2perminc = approxfun(as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']),liquid_wealth_to_perm_inc_decile_params[,param_num],rule=2)
MPC_tran_predict_from_pctile    = approxfun(c(5,15,25,35,45,55,65,75,85,95),liquid_wealth_decile_params[,param_num],rule=2)
MPC_tran_predict_income =    approxfun(as.matrix(Inc_decile_stats['inc_after_tax_p50'])/exchange_rate,income_decile_params[,param_num],rule=2)
MPC_perm_predict = approxfun(as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate,liquid_wealth_decile_params[,3],rule=2)
MPC_perm_predict_lw2perminc = approxfun(as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']),liquid_wealth_to_perm_inc_decile_params[,3],rule=2)
MPC_perm_predict_income = approxfun(as.matrix(Inc_decile_stats['inc_after_tax_p50'])/exchange_rate,income_decile_params[,3],rule=2)



URE_decile_stats = read.csv(paste(txt_dir,'URE_decile_stats1.txt',sep=''))
URE_decile_MPC_predict = MPC_tran_predict(as.matrix(URE_decile_stats['liquidassets_adj_p50'])/exchange_rate)
URE_decile_MPC_predict_ratio = MPC_tran_predict_lw2perminc(as.matrix(URE_decile_stats['liquid_to_perm_p50']))
URE_decile_estimates = URE_quantile_params[,param_num]

colors = c("#fc8d59","#91bfdb")

pdf(paste(figures_dir, "URE_predict_from_Liquid.pdf",sep=""))
barCenters <- barplot(height=t(cbind(URE_decile_MPC_predict,URE_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="Transitory MPX by URE Decile")
this_legend=c("Based on Liquid Wealth","Actual Estimate")
text(x=barCenters[1,]+1, y =-0.05, adj=1, labels=c(1,2,3,4,5,6,7,8,9,10),xpd=TRUE)
legend(1, 1, legend=this_legend, fill=colors,bty="n")
dev.off()

mean_MPC = mean(URE_decile_estimates)
mean_sq_error = mean((URE_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((URE_decile_estimates-URE_decile_MPC_predict)^2)
R2_URE = 1- mean_sq_prediction_error/mean_sq_error
mean_sq_prediction_error_ratio = mean((URE_decile_estimates-URE_decile_MPC_predict_ratio)^2)
R2_URE_ratio = 1- mean_sq_prediction_error_ratio/mean_sq_error
std_dev_URE = mean_sq_error^0.5
std_dev_pred_URE = mean_sq_prediction_error^0.5

URE_decile_estimates_perm = URE_quantile_params[,3]
URE_decile_MPC_predict_perm = MPC_perm_predict(as.matrix(URE_decile_stats['liquidassets_adj_p50'])/exchange_rate)
URE_decile_MPC_predict_ratio_perm = MPC_perm_predict_lw2perminc(as.matrix(URE_decile_stats['liquid_to_perm_p50']))
mean_sq_prediction_error = mean((URE_decile_estimates_perm-URE_decile_MPC_predict_perm)^2)
std_dev_pred_URE_perm = mean_sq_prediction_error^0.5
mean_sq_prediction_error = mean((URE_decile_estimates_perm-URE_decile_MPC_predict_ratio_perm)^2)
std_dev_pred_URE_ratio_perm = mean_sq_prediction_error^0.5
std_dev_pred_URE_ratio = mean_sq_prediction_error_ratio^0.5

mean_abs_pred_error_URE            = mean(abs(URE_decile_estimates     -URE_decile_MPC_predict))
mean_abs_pred_error_ratio_URE      = mean(abs(URE_decile_estimates     -URE_decile_MPC_predict_ratio))
mean_abs_pred_error_URE_perm       = mean(abs(URE_decile_estimates_perm-URE_decile_MPC_predict_perm))
mean_abs_pred_error_URE_ratio_perm = mean(abs(URE_decile_estimates_perm-URE_decile_MPC_predict_ratio_perm))

URE_decile_MPC_predict_income = MPC_tran_predict_income(as.matrix(URE_decile_stats['inc_after_tax_p50'])/exchange_rate)
URE_decile_MPC_predict_perm_income = MPC_tran_predict_income(as.matrix(URE_decile_stats['inc_after_tax_p50'])/exchange_rate)
mean_abs_pred_error_URE_income            = mean(abs(URE_decile_estimates     -URE_decile_MPC_predict_income))
mean_abs_pred_error_URE_perm_income       = mean(abs(URE_decile_estimates_perm-URE_decile_MPC_predict_perm_income))


NNP_decile_stats = read.csv(paste(txt_dir,'NNP_decile_stats1.txt',sep=''))
NNP_decile_MPC_predict = MPC_tran_predict(as.matrix(NNP_decile_stats['liquidassets_adj_p50'])/exchange_rate)
NNP_decile_MPC_predict_ratio = MPC_tran_predict_lw2perminc(as.matrix(NNP_decile_stats['liquid_to_perm_p50']))
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
mean_sq_prediction_error_ratio = mean((NNP_decile_estimates-NNP_decile_MPC_predict_ratio)^2)
R2_NNP_ratio = 1- mean_sq_prediction_error_ratio/mean_sq_error
std_dev_NNP = mean_sq_error^0.5
std_dev_pred_NNP = mean_sq_prediction_error^0.5

NNP_decile_estimates_perm = NNP_quantile_params[,3]
NNP_decile_MPC_predict_perm = MPC_perm_predict(as.matrix(NNP_decile_stats['liquidassets_adj_p50'])/exchange_rate)
NNP_decile_MPC_predict_ratio_perm = MPC_perm_predict_lw2perminc(as.matrix(NNP_decile_stats['liquid_to_perm_p50']))
mean_sq_prediction_error = mean((NNP_decile_estimates_perm-NNP_decile_MPC_predict_perm)^2)
std_dev_pred_NNP_perm = mean_sq_prediction_error^0.5
mean_sq_prediction_error = mean((NNP_decile_estimates_perm-NNP_decile_MPC_predict_ratio_perm)^2)
std_dev_pred_NNP_ratio_perm = mean_sq_prediction_error^0.5
std_dev_pred_NNP_ratio = mean_sq_prediction_error_ratio^0.5

mean_abs_pred_error_NNP            = mean(abs(NNP_decile_estimates     -NNP_decile_MPC_predict))
mean_abs_pred_error_ratio_NNP      = mean(abs(NNP_decile_estimates     -NNP_decile_MPC_predict_ratio))
mean_abs_pred_error_NNP_perm       = mean(abs(NNP_decile_estimates_perm-NNP_decile_MPC_predict_perm))
mean_abs_pred_error_NNP_ratio_perm = mean(abs(NNP_decile_estimates_perm-NNP_decile_MPC_predict_ratio_perm))

NNP_decile_MPC_predict_income = MPC_tran_predict_income(as.matrix(NNP_decile_stats['inc_after_tax_p50'])/exchange_rate)
NNP_decile_MPC_predict_perm_income = MPC_tran_predict_income(as.matrix(NNP_decile_stats['inc_after_tax_p50'])/exchange_rate)
mean_abs_pred_error_NNP_income            = mean(abs(NNP_decile_estimates     -NNP_decile_MPC_predict_income))
mean_abs_pred_error_NNP_perm_income       = mean(abs(NNP_decile_estimates_perm-NNP_decile_MPC_predict_perm_income))


Inc_decile_stats = read.csv(paste(txt_dir,'inc_decile_stats1.txt',sep=''))
Inc_decile_MPC_predict = MPC_tran_predict(as.matrix(Inc_decile_stats['liquidassets_adj_p50'])/exchange_rate)
Inc_decile_MPC_predict_ratio = MPC_tran_predict_lw2perminc(as.matrix(Inc_decile_stats['liquid_to_perm_p50']))

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
mean_sq_prediction_error_ratio = mean((Inc_decile_estimates-Inc_decile_MPC_predict_ratio)^2)
R2_Inc_ratio = 1- mean_sq_prediction_error_ratio/mean_sq_error
std_dev_Inc = mean_sq_error^0.5
std_dev_pred_Inc = mean_sq_prediction_error^0.5

Inc_decile_estimates_perm = Income_quantile_params[,3]
Inc_decile_MPC_predict_perm = MPC_perm_predict(as.matrix(Inc_decile_stats['liquidassets_adj_p50'])/exchange_rate)
Inc_decile_MPC_predict_ratio_perm = MPC_perm_predict_lw2perminc(as.matrix(Inc_decile_stats['liquid_to_perm_p50']))
mean_sq_prediction_error = mean((Inc_decile_estimates_perm-Inc_decile_MPC_predict_perm)^2)
std_dev_pred_Inc_perm = mean_sq_prediction_error^0.5
mean_sq_prediction_error = mean((Inc_decile_estimates_perm-Inc_decile_MPC_predict_ratio_perm)^2)
std_dev_pred_Inc_ratio_perm = mean_sq_prediction_error^0.5
std_dev_pred_Inc_ratio = mean_sq_prediction_error_ratio^0.5

mean_abs_pred_error_Inc            = mean(abs(Inc_decile_estimates     -Inc_decile_MPC_predict))
mean_abs_pred_error_ratio_Inc      = mean(abs(Inc_decile_estimates     -Inc_decile_MPC_predict_ratio))
mean_abs_pred_error_Inc_perm       = mean(abs(Inc_decile_estimates_perm-Inc_decile_MPC_predict_perm))
mean_abs_pred_error_Inc_ratio_perm = mean(abs(Inc_decile_estimates_perm-Inc_decile_MPC_predict_ratio_perm))

Inc_decile_MPC_predict_income             = MPC_tran_predict_income(as.matrix(Inc_decile_stats['inc_after_tax_p50'])/exchange_rate)
Inc_decile_MPC_predict_perm_income        = MPC_perm_predict_income(as.matrix(Inc_decile_stats['inc_after_tax_p50'])/exchange_rate)
mean_abs_pred_error_Inc_income            = mean(abs(Inc_decile_estimates     -Inc_decile_MPC_predict_income))
mean_abs_pred_error_Inc_perm_income       = mean(abs(Inc_decile_estimates_perm-Inc_decile_MPC_predict_perm_income))


Con_decile_stats = read.csv(paste(txt_dir,'con_decile_stats1.txt',sep=''))
Con_decile_MPC_predict = MPC_tran_predict(as.matrix(Con_decile_stats['liquidassets_adj_p50'])/exchange_rate)
Con_decile_MPC_predict_ratio = MPC_tran_predict_lw2perminc(as.matrix(Con_decile_stats['liquid_to_perm_p50']))

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
mean_sq_prediction_error_ratio = mean((Con_decile_estimates-Con_decile_MPC_predict_ratio)^2)
R2_Con_ratio = 1- mean_sq_prediction_error_ratio/mean_sq_error
std_dev_Con = mean_sq_error^0.5
std_dev_pred_Con = mean_sq_prediction_error^0.5

Con_decile_estimates_perm = MeanCons_quantile_params[,3]
Con_decile_MPC_predict_perm = MPC_perm_predict(as.matrix(Con_decile_stats['liquidassets_adj_p50'])/exchange_rate)
Con_decile_MPC_predict_ratio_perm = MPC_perm_predict_lw2perminc(as.matrix(Con_decile_stats['liquid_to_perm_p50']))
mean_sq_prediction_error = mean((Con_decile_estimates_perm-Con_decile_MPC_predict_perm)^2)
std_dev_pred_Con_perm = mean_sq_prediction_error^0.5
mean_sq_prediction_error = mean((Con_decile_estimates_perm-Con_decile_MPC_predict_ratio_perm)^2)
std_dev_pred_Con_ratio_perm = mean_sq_prediction_error^0.5
std_dev_pred_Con_ratio = mean_sq_prediction_error_ratio^0.5

mean_abs_pred_error_Con            = mean(abs(Con_decile_estimates     -Con_decile_MPC_predict))
mean_abs_pred_error_ratio_Con      = mean(abs(Con_decile_estimates     -Con_decile_MPC_predict_ratio))
mean_abs_pred_error_Con_perm       = mean(abs(Con_decile_estimates_perm-Con_decile_MPC_predict_perm))
mean_abs_pred_error_Con_ratio_perm = mean(abs(Con_decile_estimates_perm-Con_decile_MPC_predict_ratio_perm))

Con_decile_MPC_predict_income = MPC_tran_predict_income(as.matrix(Con_decile_stats['inc_after_tax_p50'])/exchange_rate)
Con_decile_MPC_predict_perm_income = MPC_tran_predict_income(as.matrix(Con_decile_stats['inc_after_tax_p50'])/exchange_rate)
mean_abs_pred_error_Con_income            = mean(abs(Con_decile_estimates     -Con_decile_MPC_predict_income))
mean_abs_pred_error_Con_perm_income       = mean(abs(Con_decile_estimates_perm-Con_decile_MPC_predict_perm_income))


NW_decile_stats = read.csv(paste(txt_dir,'netwealth_decile_stats1.txt',sep=''))
NW_decile_MPC_predict = MPC_tran_predict(as.matrix(NW_decile_stats['liquidassets_adj_p50'])/exchange_rate)
NW_decile_MPC_predict_ratio = MPC_tran_predict_lw2perminc(as.matrix(NW_decile_stats['liquid_to_perm_p50']))

NW_decile_estimates = net_wealth_decile_params[,param_num]
pdf(paste(figures_dir, "NW_predict_from_Liquid.pdf",sep=""))
barCenters <- barplot(height=t(cbind(NW_decile_MPC_predict,NW_decile_estimates)), 
                      beside=TRUE, col = colors, ylim = c(0,1),
                      main="Transitory MPX by Net Wealth Decile")
this_legend=c("Based on Liquid Wealth","Actual Estimate")
text(x=barCenters[1,]+1, y =-0.05, adj=1, labels=c(1,2,3,4,5,6,7,8,9,10),xpd=TRUE)
legend(1, 1, legend=this_legend, fill=colors,bty="n")
dev.off()

mean_MPC = mean(NW_decile_estimates)
mean_sq_error = mean((NW_decile_estimates-mean_MPC)^2)
mean_sq_prediction_error = mean((NW_decile_estimates-NW_decile_MPC_predict)^2)
R2_NW = 1- mean_sq_prediction_error/mean_sq_error
mean_sq_prediction_error_ratio = mean((NW_decile_estimates-NW_decile_MPC_predict_ratio)^2)
R2_NW_ratio = 1- mean_sq_prediction_error_ratio/mean_sq_error
std_dev_NW = mean_sq_error^0.5
std_dev_pred_NW = mean_sq_prediction_error^0.5

NW_decile_estimates_perm = net_wealth_decile_params[,3]
NW_decile_MPC_predict_perm = MPC_perm_predict(as.matrix(NW_decile_stats['liquidassets_adj_p50'])/exchange_rate)
NW_decile_MPC_predict_ratio_perm = MPC_perm_predict_lw2perminc(as.matrix(NW_decile_stats['liquid_to_perm_p50']))
mean_sq_prediction_error = mean((NW_decile_estimates_perm-NW_decile_MPC_predict_perm)^2)
std_dev_pred_NW_perm = mean_sq_prediction_error^0.5
mean_sq_prediction_error = mean((NW_decile_estimates_perm-NW_decile_MPC_predict_ratio_perm)^2)
std_dev_pred_NW_ratio_perm = mean_sq_prediction_error^0.5
std_dev_pred_NW_ratio = mean_sq_prediction_error_ratio^0.5

mean_abs_pred_error_NW            = mean(abs(NW_decile_estimates     -NW_decile_MPC_predict))
mean_abs_pred_error_ratio_NW      = mean(abs(NW_decile_estimates     -NW_decile_MPC_predict_ratio))
mean_abs_pred_error_NW_perm       = mean(abs(NW_decile_estimates_perm-NW_decile_MPC_predict_perm))
mean_abs_pred_error_NW_ratio_perm = mean(abs(NW_decile_estimates_perm-NW_decile_MPC_predict_ratio_perm))

NW_decile_MPC_predict_income = MPC_tran_predict_income(as.matrix(NW_decile_stats['inc_after_tax_p50'])/exchange_rate)
NW_decile_MPC_predict_perm_income = MPC_tran_predict_income(as.matrix(NW_decile_stats['inc_after_tax_p50'])/exchange_rate)
mean_abs_pred_error_NW_income            = mean(abs(NW_decile_estimates     -NW_decile_MPC_predict_income))
mean_abs_pred_error_NW_perm_income       = mean(abs(NW_decile_estimates_perm-NW_decile_MPC_predict_perm_income))

###########################################
# Make graphs for prediction using income as well as liquid wealth
three_colors = c('#fb8072','#bebada','#ffffb3')
pdf(paste(figures_dir, "URE_predict_from_HHCharacteristics.pdf",sep=""))
barCenters <- barplot(height=t(cbind(URE_decile_estimates,URE_decile_MPC_predict,URE_decile_MPC_predict_income)), 
                      beside=TRUE, col = three_colors, ylim = c(0,1),
                      main="Transitory MPX by URE Decile")
this_legend=c("Actual Estimate","Based on Liquid Wealth","Based on Income")
text(x=barCenters[1,]+1, y =-0.05, adj=1, labels=c(1,2,3,4,5,6,7,8,9,10),xpd=TRUE)
legend(1, 1, legend=this_legend, fill=three_colors,bty="n")
dev.off()

three_colors = c('#fb8072','#bebada','#ffffb3')
pdf(paste(figures_dir, "NW_predict_from_HHCharacteristics.pdf",sep=""))
barCenters <- barplot(height=t(cbind(NW_decile_estimates,NW_decile_MPC_predict,NW_decile_MPC_predict_income)), 
                      beside=TRUE, col = three_colors, ylim = c(0,1),
                      main="Transitory MPX by Net Wealth Decile")
this_legend=c("Actual Estimate","Based on Liquid Wealth","Based on Income")
text(x=barCenters[1,]+1, y =-0.05, adj=1, labels=c(1,2,3,4,5,6,7,8,9,10),xpd=TRUE)
legend(1, 1, legend=this_legend, fill=three_colors,bty="n")
dev.off()
###########################################

# Write outputs to csv file
output = matrix(NA,nrow=5,ncol=6)
output[1,1] = mean_abs_pred_error_URE
output[1,2] = mean_abs_pred_error_ratio_URE
output[1,3] = mean_abs_pred_error_URE_perm
output[1,4] = mean_abs_pred_error_URE_ratio_perm
output[1,5] = mean_abs_pred_error_URE_income
output[1,6] = mean_abs_pred_error_URE_perm_income
output[2,1] = mean_abs_pred_error_NNP
output[2,2] = mean_abs_pred_error_ratio_NNP
output[2,3] = mean_abs_pred_error_NNP_perm
output[2,4] = mean_abs_pred_error_NNP_ratio_perm
output[2,5] = mean_abs_pred_error_NNP_income
output[2,6] = mean_abs_pred_error_NNP_perm_income
output[3,1] = mean_abs_pred_error_Inc
output[3,2] = mean_abs_pred_error_ratio_Inc
output[3,3] = mean_abs_pred_error_Inc_perm
output[3,4] = mean_abs_pred_error_Inc_ratio_perm
output[3,5] = mean_abs_pred_error_Inc_income
output[3,6] = mean_abs_pred_error_Inc_perm_income
output[4,1] = mean_abs_pred_error_Con
output[4,2] = mean_abs_pred_error_ratio_Con
output[4,3] = mean_abs_pred_error_Con_perm
output[4,4] = mean_abs_pred_error_Con_ratio_perm
output[4,5] = mean_abs_pred_error_Con_income
output[4,6] = mean_abs_pred_error_Con_perm_income
output[5,1] = mean_abs_pred_error_NW
output[5,2] = mean_abs_pred_error_ratio_NW
output[5,3] = mean_abs_pred_error_NW_perm
output[5,4] = mean_abs_pred_error_NW_ratio_perm
output[5,5] = mean_abs_pred_error_NW_income
output[5,6] = mean_abs_pred_error_NW_perm_income

write.table(output, file = paste(tables_dir,"prediction_errors.csv",sep=""),row.names=FALSE, na="",col.names=FALSE, sep=",")


############################## Load SCF data

SCF_data = read.csv(paste(SCF_dir,'SCF_Auclert','.csv',sep=''), header = TRUE)

URE_MPC_predict_by_decile = array(0, dim=c(10))
URE_MPC_predict_by_decile_median = array(0, dim=c(10))
URE_by_decile = array(0, dim=c(10))
liq_by_URE_decile = array(0, dim=c(10))
liq_by_liq_decile = array(0, dim=c(10))
for (i in 1:10){
  URE_by_decile[i] = weighted.mean(SCF_data$URE[SCF_data$URE_decile==i],SCF_data$wgt[SCF_data$URE_decile==i])
  #URE_MPC_predict_by_decile_median[i] =   MPC_tran_predict(weighted.median(SCF_data$liq[SCF_data$URE_decile==i], SCF_data$wgt[SCF_data$URE_decile==i]))
  URE_MPC_predict_by_decile_median[i]    = MPC_tran_predict_from_pctile(weighted.median(SCF_data$liq_pctile[SCF_data$URE_decile==i], SCF_data$wgt[SCF_data$URE_decile==i]))
  liq_by_URE_decile[i] = weighted.median(SCF_data$liq[SCF_data$URE_decile==i], SCF_data$wgt[SCF_data$URE_decile==i])
  liq_by_liq_decile[i] = weighted.median(SCF_data$liq[SCF_data$liq_decile==i], SCF_data$wgt[SCF_data$liq_decile==i])
  }

barCenters <- barplot(height=t(cbind(URE_MPC_predict_by_decile_median,URE_decile_MPC_predict)), 
                      beside=TRUE, col=colors, ylim = c(0,1),
                      main="URE Deciles")


NNP_MPC_predict_by_decile = array(0, dim=c(10))
NNP_MPC_predict_by_decile_median = array(0, dim=c(10))
NNP_by_decile = array(0, dim=c(10))
liq_by_NNP_decile = array(0, dim=c(10))
for (i in 1:10){
  NNP_by_decile[i] = weighted.mean(SCF_data$NNP[SCF_data$NNP_decile==i],SCF_data$wgt[SCF_data$NNP_decile==i])
  #NNP_MPC_predict_by_decile_median[i] = MPC_tran_predict(weighted.median(SCF_data$liq[SCF_data$NNP_decile==i], SCF_data$wgt[SCF_data$NNP_decile==i]))
  NNP_MPC_predict_by_decile_median[i]    = MPC_tran_predict_from_pctile(weighted.median(SCF_data$liq_pctile[SCF_data$NNP_decile==i], SCF_data$wgt[SCF_data$NNP_decile==i]))
  liq_by_NNP_decile[i] = weighted.median(SCF_data$liq[SCF_data$NNP_decile==i], SCF_data$wgt[SCF_data$NNP_decile==i])
}

Inc_MPC_predict_by_decile = array(0, dim=c(10))
Inc_MPC_predict_by_decile_median = array(0, dim=c(10))
Inc_by_decile = array(0, dim=c(10))
liq_by_Inc_decile = array(0, dim=c(10))
for (i in 1:10){
  Inc_by_decile[i] = weighted.mean(SCF_data$income[SCF_data$inc_decile==i],SCF_data$wgt[SCF_data$inc_decile==i])
  Inc_MPC_predict_by_decile_median[i]    = MPC_tran_predict_from_pctile(weighted.median(SCF_data$liq_pctile[SCF_data$inc_decile==i], SCF_data$wgt[SCF_data$inc_decile==i]))
  liq_by_Inc_decile[i] = weighted.median(SCF_data$liq[SCF_data$inc_decile==i], SCF_data$wgt[SCF_data$inc_decile==i])
}

barCenters <- barplot(height=t(cbind(NNP_MPC_predict_by_decile_median,NNP_decile_MPC_predict)), 
                      beside=TRUE, col=colors, ylim = c(0,1),
                      main="NNP Deciles")

total_C = 0.7*18.7*1000000000000 #18.7 trillion GDP, 70% consumption
total_households = sum(SCF_data$wgt)
C_per_HH = total_C/total_households

#####################################
auclert_elasticity<- function(liquid_wealth_decile_MPX, value, decile, wgt = SCF_data$wgt, liq_pctile= SCF_data$liq_pctile, interpolation_points = c(5,15,25,35,45,55,65,75,85,95)) {
  MPC_tran_predict_from_pctile = approxfun(interpolation_points,liquid_wealth_decile_MPX,rule=2)
  MPC_predict_by_decile_median = array(0, dim=c(10))
  value_by_decile = array(0, dim=c(10))
  for (i in 1:10){
    value_by_decile[i] = weighted.mean(value[decile==i],wgt[decile==i])
    MPC_predict_by_decile_median[i]    = MPC_tran_predict_from_pctile(weighted.median(liq_pctile[decile==i], wgt[decile==i]))
  }
  elasticity = sum(value_by_decile/10*MPC_predict_by_decile_median/C_per_HH)
  return (elasticity)
}
auclert_elasticity_se <-function(liquid_wealth_decile_MPX, value, decile) {
  elasticity_0 = auclert_elasticity(liquid_wealth_decile_MPX, value, decile )
  epsilon = 0.000001
  delta = c(1:10 *0)
  for (i in 1:10){
    shock = c(1:10 *0)
    shock[i] = epsilon
    delta[i] = (auclert_elasticity(liquid_wealth_decile_MPX+shock, value, decile ) - elasticity_0)/epsilon
  }
  se = sum( (delta*liquid_wealth_decile_se[,4])^2 )^0.5
  return (se)
}
E_Y_elasticity<- function(liquid_wealth_decile_MPX, value, decile, wgt = SCF_data$wgt, liq_pctile= SCF_data$liq_pctile, interpolation_points = c(5,15,25,35,45,55,65,75,85,95)) {
  MPC_tran_predict_from_pctile = approxfun(interpolation_points,liquid_wealth_decile_MPX,rule=2)
  MPC_predict_by_decile_median = array(0, dim=c(10))
  value_by_decile = array(0, dim=c(10))
  for (i in 1:10){
    value_by_decile[i] = weighted.mean(value[decile==i],wgt[decile==i])
    MPC_predict_by_decile_median[i]    = MPC_tran_predict_from_pctile(weighted.median(liq_pctile[decile==i], wgt[decile==i]))
  }
  elasticity = sum(value_by_decile/10*MPC_predict_by_decile_median/C_per_HH) - mean(MPC_predict_by_decile_median)
  return (elasticity)
}
E_Y_elasticity_se <-function(liquid_wealth_decile_MPX, value, decile) {
  elasticity_0 = E_Y_elasticity(liquid_wealth_decile_MPX, value, decile )
  epsilon = 0.000001
  delta = c(1:10 *0)
  for (i in 1:10){
    shock = c(1:10 *0)
    shock[i] = epsilon
    delta[i] = (E_Y_elasticity(liquid_wealth_decile_MPX+shock, value, decile ) - elasticity_0)/epsilon
  }
  se = sum( (delta*liquid_wealth_decile_se[,4])^2 )^0.5
  return (se)
}

#####################################
E_R_unbalanced = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$URE, SCF_data$URE_decile )
E_R_se = auclert_elasticity_se(liquid_wealth_decile_params[,4], SCF_data$URE, SCF_data$URE_decile )
E_R = E_R_unbalanced -sum(URE_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets


E_P_unbalanced = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$NNP, SCF_data$NNP_decile )
E_P_se = auclert_elasticity_se(liquid_wealth_decile_params[,4], SCF_data$NNP, SCF_data$NNP_decile )
E_P = E_P_unbalanced -sum(NNP_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets

M = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile )
M_se = auclert_elasticity_se(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile )

E_Y = E_Y_elasticity(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile )
E_Y_se = E_Y_elasticity_se(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile )

################################################
# Calc based on different interpolation
E_R_unbalanced_liq = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$URE, SCF_data$URE_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq ,interpolation_points = as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate)
E_R_liq = E_R_unbalanced_liq -sum(URE_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets

E_P_unbalanced_liq = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$NNP, SCF_data$NNP_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq ,interpolation_points = as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate)
E_P_liq = E_P_unbalanced_liq -sum(NNP_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets

M_liq = auclert_elasticity(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq ,interpolation_points = as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate)
E_Y_liq = E_Y_elasticity(liquid_wealth_decile_params[,4], SCF_data$income, SCF_data$inc_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq ,interpolation_points = as.matrix(liquid_decile_stats['liquidassets_adj_p50'])/exchange_rate)

E_R_unbalanced_ratio = auclert_elasticity(liquid_wealth_to_perm_inc_decile_params[,4], SCF_data$URE, SCF_data$URE_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq/SCF_data$income ,interpolation_points = as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']))
E_R_ratio = E_R_unbalanced_ratio -sum(URE_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets

E_P_unbalanced_ratio = auclert_elasticity(liquid_wealth_to_perm_inc_decile_params[,4], SCF_data$NNP, SCF_data$NNP_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq/SCF_data$income ,interpolation_points = as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']))
E_P_ratio = E_P_unbalanced_ratio -sum(NNP_by_decile/10)*0.1/C_per_HH #assume MPC=0.1 for indirectly held assets

M_ratio = auclert_elasticity(liquid_wealth_to_perm_inc_decile_params[,4], SCF_data$income, SCF_data$inc_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq/SCF_data$income ,interpolation_points = as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']))
E_Y_ratio = E_Y_elasticity(liquid_wealth_to_perm_inc_decile_params[,4], SCF_data$income, SCF_data$inc_decile,  wgt = SCF_data$wgt, liq_pctile= SCF_data$liq/SCF_data$income ,interpolation_points = as.matrix(lw2perminc_decile_stats['liquid_to_perm_p50']))


######################################################
# Bootstrap standard errors for SCF data
######################################################
bootstrap_samples = 1000
bootstrap_E_R = c(1:bootstrap_samples *0)
bootstrap_E_P = c(1:bootstrap_samples *0)
bootstrap_M = c(1:bootstrap_samples *0)
bootstrap_E_Y = c(1:bootstrap_samples *0)
set.seed(6)
wgts = SCF_data$prob
for (sample in 1:bootstrap_samples){
  this_sample = 1:length(SCF_data$wgt) #sample.int(length(SCF_data$wgt),replace=TRUE,prob=SCF_data$wgt)
  this_SCF_bootstrap <- list()
  this_SCF_bootstrap$wgt = SCF_data$wgt[this_sample]
  this_SCF_bootstrap$URE_decile = SCF_data$URE_decile[this_sample]
  this_SCF_bootstrap$URE = SCF_data$URE[this_sample]
  this_SCF_bootstrap$NNP_decile = SCF_data$NNP_decile[this_sample]
  this_SCF_bootstrap$NNP = SCF_data$NNP[this_sample]
  this_SCF_bootstrap$inc_decile = SCF_data$inc_decile[this_sample]
  this_SCF_bootstrap$income = SCF_data$income[this_sample]
  this_SCF_bootstrap$liq_pctile = SCF_data$liq_pctile[this_sample]
  
  #Randomize error from Danish data
  this_liquid_wealth_decile_params = liquid_wealth_decile_params[,4] + rnorm(10)*liquid_wealth_decile_se[,4]
  
  this_URE_by_decile = array(0, dim=c(10))
  this_NNP_by_decile = array(0, dim=c(10))
  this_liq_by_liq_decile = array(0, dim=c(10))
  for (i in 1:10){
    this_URE_by_decile[i] = weighted.mean(this_SCF_bootstrap$URE[this_SCF_bootstrap$URE_decile==i],this_SCF_bootstrap$wgt[this_SCF_bootstrap$URE_decile==i])
    this_NNP_by_decile[i] = weighted.mean(this_SCF_bootstrap$NNP[this_SCF_bootstrap$NNP_decile==i],this_SCF_bootstrap$wgt[this_SCF_bootstrap$NNP_decile==i])
  }
  E_R_unbalanced = auclert_elasticity(this_liquid_wealth_decile_params, 
                                             this_SCF_bootstrap$URE, 
                                             this_SCF_bootstrap$URE_decile, 
                                             wgt = this_SCF_bootstrap$wgt, 
                                             liq_pctile= this_SCF_bootstrap$liq_pctile) 
  bootstrap_E_R[sample] = E_R_unbalanced -sum(this_URE_by_decile/10)*0.1/C_per_HH
  E_P_unbalanced = auclert_elasticity(this_liquid_wealth_decile_params, 
                                      this_SCF_bootstrap$NNP, 
                                      this_SCF_bootstrap$NNP_decile, 
                                      wgt = this_SCF_bootstrap$wgt, 
                                      liq_pctile= this_SCF_bootstrap$liq_pctile) 
  bootstrap_E_P[sample] = E_P_unbalanced -sum(this_NNP_by_decile/10)*0.1/C_per_HH
  bootstrap_M[sample] = auclert_elasticity(this_liquid_wealth_decile_params, 
                                      this_SCF_bootstrap$income, 
                                      this_SCF_bootstrap$inc_decile, 
                                      wgt = this_SCF_bootstrap$wgt, 
                                      liq_pctile= this_SCF_bootstrap$liq_pctile) 
  bootstrap_E_Y[sample] = E_Y_elasticity(this_liquid_wealth_decile_params, 
                                           this_SCF_bootstrap$income, 
                                           this_SCF_bootstrap$inc_decile, 
                                           wgt = this_SCF_bootstrap$wgt, 
                                           liq_pctile= this_SCF_bootstrap$liq_pctile)
}
bootstrap_E_R_se = sd(bootstrap_E_R)
bootstrap_E_P_se = sd(bootstrap_E_P)
bootstrap_M_se = sd(bootstrap_M)
bootstrap_E_Y_se = sd(bootstrap_E_Y)

# Write outputs to csv file
output = matrix(NA,nrow=2,ncol=4)
output[1,1] = M
output[1,2] = E_Y
output[1,3] = E_P
output[1,4] = E_R
output[2,1] = bootstrap_M_se
output[2,2] = bootstrap_E_Y_se
output[2,3] = bootstrap_E_P_se
output[2,4] = bootstrap_E_R_se
write.table(output, file = paste(tables_dir,"US_auclert_stats.csv",sep=""),row.names=FALSE, na="",col.names=FALSE, sep=",")

# Write outputs to csv file
output = matrix(NA,nrow=3,ncol=4)
output[1,1] = M
output[1,2] = E_Y
output[1,3] = E_P
output[1,4] = E_R
output[2,1] = M_liq
output[2,2] = E_Y_liq
output[2,3] = E_P_liq
output[2,4] = E_R_liq
output[3,1] = M_ratio
output[3,2] = E_Y_ratio
output[3,3] = E_P_ratio
output[3,4] = E_R_ratio
write.table(output, file = paste(tables_dir,"US_auclert_different_interpolations.csv",sep=""),row.names=FALSE, na="",col.names=FALSE, sep=",")
