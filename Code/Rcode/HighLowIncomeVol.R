

###############################################################################
# load liquid weath quintile data and create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealth")
###############################################################################

###############################################################################
# load low income vol liquid weath quintile data and create graph
load(paste(moments_dir,'moments_low_inc_vol_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_low_inc_vol_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile, Low Income Vol","LowIncVolLiquidWealth")
###############################################################################

###############################################################################
# load high income vol liquid weath quintile data and create graph
load(paste(moments_dir,'moments_high_inc_vol_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_high_inc_vol_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile, High Income Vol","HighIncVolLiquidWealth")
###############################################################################

###############################################################################
# load private sector liquid weath quintile data and create graph
load(paste(moments_dir,'moments_pri_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_pri_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile, Private Sector","Private")
###############################################################################

###############################################################################
# load non-private sector liquid weath quintile data and create graph
load(paste(moments_dir,'moments_nonpri_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_nonpri_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile, Non-Private Sector","NonPrivate")
###############################################################################

# Function to plot shock variances and consumption elasticities
vol_plot<- function(tag_list, moments_name, quantile_labels, tag_list_legend, title_string, filename, param_col=4, legend_xpos = NULL, x_label="Quantile") {
  
  if (param_col==4 | param_col==3){
    axis_string = "MPX"
  }else if (param_col==2 | param_col==1){
    axis_string = "Variance"
  }
  
  this_colors = c('#fb8072','#bebada','#ffffb3','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#8dd3c7')
  this_colors = this_colors[1:length(tag_list)]
  num_quantiles = length(quantile_labels)
  xlabel_pos = (length(tag_list)-1)/2
  params = matrix(0.0,num_quantiles,length(tag_list))
  se = matrix(0.0,num_quantiles,length(tag_list))
  for (i in 1:length(tag_list)) {
    this_tag = tag_list[i]
    load(paste(moments_dir,moments_name,this_tag,'.RData',sep=''))
    output =estimation_by_category(eval(parse(text = paste(moments_name,this_tag,sep=''))), make.names(quantile_labels))
    params[,i] = output$category_params[,param_col]
    se[,i] = output$category_se[,param_col]
  }
  #dev.new()
  pdf(paste(figures_dir, filename,".pdf",sep=""))
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  plotTop = max(max(params),1.0)
  if (param_col==1 | param_col==2){
    plotTop = max(params)*1.15
  }
  #plotTop = max(params)
  barCenters <- barplot(height=t(params),
                        names.arg=quantile_labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=title_string,
                        ylab = "",
                        xlab = x_label,
                        border="black",
                        axes=TRUE)
  mtext(text = axis_string,
        side = 2, #side 2 = left
        line = 4,cex=1.5)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=0, adj=1, labels=quantile_labels,xpd=TRUE)
  segments(barCenters, t(params-se*1.96),
           barCenters,
           t(params+se*1.96), lwd=1.5)
  arrows(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  if (is.null(legend_xpos)){
    legend_xpos = 2.5*num_quantiles
  }
  legend(legend_xpos, plotTop, legend=tag_list_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, filename,".png",sep=""))
  #dev.copy(pdf, paste(figures_dir, filename,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, filename,".svg",sep=""))
  dev.off()
  params
}



tag_list = c("_high_inc_vol_by_liquid_wealth_quantile","_by_liquid_wealth_quantile","_low_inc_vol_by_liquid_wealth_quantile")
tag_list_legend = c("High Vol","Baseline","Low Vol" )

#First do liquid wealth
num_quantiles = 5
#transitory
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "HighLowVol_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
#permanent
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "HighLowVol_permMPX_liquidwealth", param_col=3, x_label="Quintile")


tag_list = c("_pri_by_liquid_wealth_quantile","_by_liquid_wealth_quantile","_nonpri_by_liquid_wealth_quantile")
tag_list_legend = c("Private","Baseline","Non-private" )

#First do liquid wealth
num_quantiles = 5
#transitory
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "pri_nonpri_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Transitory Variance by Liquid Wealth Quintile", "pri_nonpri_tranVar_liquidwealth", param_col=2, x_label="Quintile")

#permanent
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "pri_nonpri_permMPX_liquidwealth", param_col=3, x_label="Quintile")
vol_plot(tag_list, "moments", as.character(1:num_quantiles), tag_list_legend, "Permanent Variance by Liquid Wealth Quintile", "pri_nonpri_permVar_liquidwealth", param_col=1, x_label="Quintile")

