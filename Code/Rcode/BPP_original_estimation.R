# This file does the minimum distance optimization for BPP's original method without time aggregation
# Output is for the appendix of the paper which compares BPP to our method with short-lived consumption responses

######################################################################################

tag = ""
title_string = "MPX"
axis_string = "MPX"


base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
R_code_folder =  "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode"
moments_BPP_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/",sep='')
moments_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/",sep='')
figures_dir = paste(base_dir,"Code/Rcode/Figures/AEJ_revision/",sep='')
tables_dir = paste(base_dir,"Code/Rcode/Tables/AEJ_revision/",sep='')

source(paste(R_code_folder,"/min_distance_BPP.r", sep=""))

colors = c("#fc8d59","#91bfdb","#ffffbf")

######################################################################################
# Function to plot shock variances and consumption elasticities
# Mostly copied from MAIN.R - legend is placed in slightly different place
plot_estimataion_output_BPP<- function(params, se, labels, category_for_title, category_for_save,transitory_only=FALSE,category_label="") {
  # First plot the variances
  #dev.new()
  pdf(paste(figures_dir, "VarianceBy",category_for_save,tag,".pdf",sep=""))
  if (transitory_only){
    param_cols=2
    this_colors=c(colors[2])
    this_legend =c( expression(paste(sigma[q]^2," Transitory Var")))
    xlabel_pos = 0
  } else {
    param_cols=1:2
    this_colors=c(colors[1],colors[2])
    this_legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var")))
    xlabel_pos = 1
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  plotTop = max(params[,param_cols])*1.2
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop*1.05), xaxt="n",
                        main=paste("Permanent and Transitory Variance by ",category_for_title),
                        ylab = "Shock Variance\n", border="black", axes=TRUE,xlab=category_label)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
         barCenters,
         t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(2, plotTop, legend=this_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, "VarianceBy",category_for_save,tag,".png",sep=""))
  #dev.copy(pdf, paste(figures_dir, "VarianceBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "VarianceBy",category_for_save,tag,".svg",sep=""))
  dev.off()
  
  # Now plot the Expenditure Elasticities
  #dev.new()
  pdf(paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  if (transitory_only){
    param_cols=4
    this_legend=c(expression(paste(psi," Transitory MPX")))
  } else {
    param_cols=3:4
    this_legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX")))
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=this_colors)
  plotTop = max(max(params[,param_cols]),1.0)
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste(title_string, " by ",category_for_title),
                        ylab = axis_string, border="black", axes=TRUE,xlab=category_label)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
         barCenters,
         t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(7, plotTop, legend=this_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
  #dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
  dev.off()
}
###############################################################################

###############################################################################
# Full sample estimation
#First load the moments
c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/BPP_moments_all_c_vector.txt", sep=""), header=FALSE, sep=",")))
omega =    as.matrix(read.csv(file=paste(moments_BPP_dir,"/BPP_moments_all_omega.txt",    sep=""), header=FALSE, sep=","))
T=12

#Next replicate BPP
BPP_output = BPP_parameter_estimation(c_vector, omega, T, ma=1, taste=1) 
# Do time aggregation WITH BPP's assumption that consumption is a random walk
BPP_with_TimeAgg_output = BPP_with_TimeAgg_parameter_estimation(c_vector, omega, T, ma=0, taste=1) 


###############################################################################
# Function to estimate parameters for each category for which we have moments
estimation_by_category_BPP<- function(moments_BPP_dir,moments_stub,category_set, T=12) {
  category_params = array(0, dim=c(length(category_set),4))
  category_se = array(0, dim=c(length(category_set),4))
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"c_vector.txt", sep=""), header=FALSE, sep=",")))
    this_omega = as.matrix(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"_omega.txt", sep=""), header=FALSE, sep=","))
    this_BPP_output = BPP_parameter_estimation(this_c_vector, this_omega,T,ma=1) 
    category_params[i,1] = mean(this_BPP_output$var_perm)
    category_params[i,2] = mean(this_BPP_output$var_tran)
    category_params[i,3] = this_BPP_output$ins_perm
    category_params[i,4] = this_BPP_output$ins_tran
    category_se[i,1] = mean(this_BPP_output$var_perm_se)
    category_se[i,2] = mean(this_BPP_output$var_tran_se)
    category_se[i,3] = this_BPP_output$ins_perm_se
    category_se[i,4] = this_BPP_output$ins_tran_se
  }
  output = list("category_params"=category_params,"category_se"=category_se)
  return (output)
}



###############################################################################
# load liquid weath quintile data and create graph
moments_stub = "BPP_moments_by_liquid_wealth_quantile"
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(wealth_quantile_set), T=12)
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
# load the new version of the moments as it contains other information, such as quantile cutoffs, that we don't have in BPP moments
load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output_BPP(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealthBPP")
###############################################################################


###############################################################################
# load net weath quintile data and create graph
moments_stub = 'BPP_moments_by_net_wealth_quantile'
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(wealth_quantile_set), T=12)
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
# load the new version of the moments as it contains other information, such as quantile cutoffs, that we don't have in BPP moments
load(paste(moments_dir,'moments_by_net_wealth_quantile',tag,'.RData',sep=''))
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('< ',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output_BPP(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Net Wealth Quantile","NetWealthBPP")
###############################################################################



###########################################################
# Do Adrien Auclert Stuff
#durable_tag ="_head_nodurableproxy"
durable_tag =tag
mean_household_consumption = 328385

###############################################################################
# load URE quintile data and create graph
moments_stub = 'BPP_moments_by_URE_quantile'
num_quantiles = 10
round_digits = 2
URE_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(URE_quantile_set), T=12)
URE_quantile_output=output
URE_quantile_params = output$category_params
URE_quantile_se = output$category_se
load(paste(moments_dir,'moments_by_URE_quantile',tag,'.RData',sep=''))
URE_quantile_set = t(round(moments_by_URE_quantile$quantile_means/mean_household_consumption,round_digits))
plot_estimataion_output_BPP(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","URE_BPP",transitory_only = TRUE, category_label = "URE/Mean Expenditure")
plot_estimataion_output_BPP(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","permURE_BPP",transitory_only = FALSE, category_label = "URE/Mean Expenditure")


#Now calculate the sufficient statistic
elas_URE_NR = mean(URE_quantile_params[,4]*t(moments_by_URE_quantile$quantile_means /mean_household_consumption))
elas_URE = elas_URE_NR - mean(URE_quantile_params[,4])*mean(t(moments_by_URE_quantile$quantile_means /mean_household_consumption))

mean_URE_MPX = mean(URE_quantile_params[,4]*t(moments_by_URE_quantile$quantile_means))
mean_URE_MPX_se = (sum((URE_quantile_se[,4]*t(moments_by_URE_quantile$quantile_means))^2)^0.5)/num_quantiles

###############################################################################
###############################################################################
# load NNP quintile data and create graph
moments_stub = 'BPP_moments_by_NNP_quantile'
load(paste(moments_dir,'moments_by_NNP_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
NNP_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(NNP_quantile_set), T=12)
NNP_quantile_output=output
NNP_quantile_params = output$category_params
NNP_quantile_se = output$category_se
NNP_quantile_set = t(round(moments_by_NNP_quantile$quantile_means /mean_household_consumption,round_digits))
plot_estimataion_output_BPP(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","NNP_BPP",transitory_only = TRUE, category_label = "NNP/Mean Expenditure")
plot_estimataion_output_BPP(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","permNNP_BPP",transitory_only = FALSE, category_label = "NNP/Mean Expenditure")

#Now calculate the sufficient statistic
elas_NNP_NR = mean(NNP_quantile_params[,4]*t(moments_by_NNP_quantile$quantile_means /mean_household_consumption))
elas_NNP = elas_NNP_NR - mean(NNP_quantile_params[,4])*mean(t(moments_by_NNP_quantile$quantile_means /mean_household_consumption))

mean_NNP_MPX = mean(NNP_quantile_params[,4]*t(moments_by_NNP_quantile$quantile_means))
mean_NNP_MPX_se = (sum((NNP_quantile_se[,4]*t(moments_by_NNP_quantile$quantile_means))^2)^0.5)/num_quantiles

###############################################################################
# load Income quintile data and create graph
moments_stub = 'BPP_moments_by_Income_quantile'
load(paste(moments_dir,'moments_by_Income_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
Income_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(Income_quantile_set), T=12)
Income_quantile_output=output
Income_quantile_params = output$category_params
Income_quantile_se = output$category_se
Income_quantile_set = round(t(moments_by_Income_quantile$quantile_means /mean_household_consumption),round_digits)
plot_estimataion_output_BPP(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","Income_BPP",transitory_only = TRUE, category_label = "Income/Mean Expenditure")
plot_estimataion_output_BPP(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","permIncome_BPP",transitory_only = FALSE, category_label = "Income/Mean Expenditure")

#Now calculate the sufficient statistic
elas_Income_NR = mean(Income_quantile_params[,4]*t(moments_by_Income_quantile$quantile_means /mean_household_consumption))
elas_Income = elas_Income_NR - mean(Income_quantile_params[,4])*mean(t(moments_by_Income_quantile$quantile_means /mean_household_consumption))

mean_Income_MPX = mean(Income_quantile_params[,4]*t(moments_by_Income_quantile$quantile_means))
mean_Income_MPX_se = (sum((Income_quantile_se[,4]*t(moments_by_Income_quantile$quantile_means))^2)^0.5)/num_quantiles

cov_Income_MPX = mean_Income_MPX-mean(Income_quantile_params[,4])*mean(t(moments_by_Income_quantile$quantile_means))
mean_MPX = mean(Income_quantile_params[,4])
mean_MPX_se = (sum(Income_quantile_se[,4]^2)^0.5)/num_quantiles
mean_Income = mean(t(moments_by_Income_quantile$quantile_means))
###############################################################################

###############################################################################
# load MeanCons quintile data and create graph
moments_stub = 'BPP_moments_by_MeanCons_quantile'
load(paste(moments_dir,'moments_by_MeanCons_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
MeanCons_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(MeanCons_quantile_set), T=12)
MeanCons_quantile_output=output
MeanCons_quantile_params = output$category_params
MeanCons_quantile_se = output$category_se
MeanCons_quantile_set = t(round(moments_by_MeanCons_quantile$quantile_means/mean_household_consumption,round_digits))
plot_estimataion_output_BPP(MeanCons_quantile_params,MeanCons_quantile_se,MeanCons_quantile_set ,"Consumption Quantile","MeanCons_BPP",transitory_only = TRUE)

#Now calculate the sufficient statistic
elas_MeanCons_NR = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means /mean_household_consumption))
elas_MeanCons = elas_MeanCons_NR - mean(MeanCons_quantile_params[,4])*mean(t(moments_by_MeanCons_quantile$quantile_means /mean_household_consumption))

mean_cons_MPX = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means))
mean_cons_MPX_se = (sum((MeanCons_quantile_se[,4]*t(moments_by_MeanCons_quantile$quantile_means))^2)^0.5)/num_quantiles


cons_weighted_MPC = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means) /mean(t(moments_by_MeanCons_quantile$quantile_means)))

# Take these numbers to Excel:
mean_URE_MPX
mean_NNP_MPX
mean_Income_MPX
mean_cons_MPX
mean_MPX
mean_Income

###############################################################################
###############################################################################

# Order of groups
# Our Sample
# Head <30
# Head >55
# Pensions and Special Saving
# General government (+NPISH)
# Non-financial corporations
# Financial sector (excl. pension funds)
# Rest of the world

# Data below comes from National accounts and Stata files (see URE_NNP_positions_NationalAccounts.xlsx in the Code directory)
NNP_by_group = c(-1402.5259604757, -216.9792779, -161.2326999, 939.6772886, -583.8741698, -333.746706, 1532.6105, 227.0940084)
URE_by_group = c(-421.5768154,  -105.331947, 38.6470548,  257.4458325,  -159.965526,  -91.4374537,  419.8932876,  62.21753654)
MPX_by_group = c(-999, 0.5, 0.5, 0.1, 0.0, 0.1, 0.1, 0.0) #note first category will be replaced with estimations
Total_consumption = 921.3977035
Total_consumption_under_30 = 149.357466144686
Total_consumption_over_55 = 265.951785816733
sample_size = 1289481.85714286 #note not an integer because this is an average over many years
under_30_size = 669565.714285714
over_55_size = 937670.1429
total_size = sample_size+under_30_size+over_55_size

Total_income_under_30 = 138.996539325257
Total_income_over_55 = 251.828251187066

E_R_component = URE_by_group*MPX_by_group/Total_consumption
E_R_component[1] = mean_URE_MPX*sample_size/(Total_consumption*1000000000)
E_R_component_se = mean_URE_MPX_se*sample_size/(Total_consumption*1000000000)

E_P_component = NNP_by_group*MPX_by_group/Total_consumption
E_P_component[1] = mean_NNP_MPX*sample_size/(Total_consumption*1000000000)
E_P_component_se = mean_NNP_MPX_se*sample_size/(Total_consumption*1000000000)


M_component = c(0,0,0)
M_component[1] = mean_Income_MPX*sample_size/(Total_consumption*1000000000)
M_component[2] = Total_income_under_30*MPX_by_group[2]/Total_consumption
M_component[3] = Total_income_over_55*MPX_by_group[3]/Total_consumption
M_component_se = mean_Income_MPX_se*sample_size/(Total_consumption*1000000000)

Mean_MPX_component = c(0,0,0)
Mean_MPX_component[1] = mean_MPX*sample_size/total_size
Mean_MPX_component[2] = MPX_by_group[2]*under_30_size/total_size
Mean_MPX_component[3] = MPX_by_group[3]*over_55_size/total_size
Mean_MPX_component_se = mean_MPX_se*sample_size/total_size

Income_over_C_component = c(0,0,0)
Income_over_C_component[1] = mean_Income*sample_size/(Total_consumption*1000000000)
Income_over_C_component[2] = Total_income_under_30/Total_consumption
Income_over_C_component[3] = Total_income_over_55/Total_consumption

E_R_auclert = sum(E_R_component)
E_R_auclert_se = E_P_component_se
E_P_auclert = sum(E_P_component)
E_P_auclert_se = E_P_component_se
M_auclert = sum(M_component)
M_auclert_se = M_component_se
E_Y_auclert = sum(M_component) - sum(Mean_MPX_component)*sum(Income_over_C_component)
E_Y_auclert_se = (M_component_se^2 + Mean_MPX_component_se^2*sum(Income_over_C_component)^2)^0.5
S_auclert = 1.0 - (mean_cons_MPX*sample_size/(Total_consumption*1000000000) + MPX_by_group[2]*Total_consumption_under_30/Total_consumption + MPX_by_group[2]*Total_consumption_over_55/Total_consumption ) 
S_auclert_se = mean_cons_MPX_se*sample_size/(Total_consumption*1000000000)


# Convert to dollars for the paper
x_rate_2015 = 6.87
NNP_dollar_by_group = NNP_by_group/x_rate_2015
URE_dollar_by_group = URE_by_group/x_rate_2015

# Write outputs to csv file
output = matrix(NA,nrow=9,ncol=7)
output[2:8,1] = MPX_by_group[2:8]
output[1:8,2] = NNP_dollar_by_group
output[9,2] = sum(NNP_dollar_by_group)
output[1:8,3] = URE_dollar_by_group
output[9,3] = sum(URE_dollar_by_group)
output[1:8,4] = E_P_component
output[9,4] = sum(E_P_component)
output[1:8,5] = E_R_component
output[9,5] = sum(E_R_component)
output[1:5,6] = c(M_auclert, E_Y_auclert, E_P_auclert, E_R_auclert, S_auclert)
output[1:5,7] = c(M_auclert_se, E_Y_auclert_se, E_P_auclert_se, E_R_auclert_se, S_auclert_se)

write.table(output, file = paste(tables_dir,"URE_NNP_BPP_positions_text.csv",sep=""),row.names=FALSE, na="",col.names=FALSE, sep=",")
