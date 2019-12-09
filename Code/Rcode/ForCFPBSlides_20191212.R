
# Function to plot shock variances and consumption elasticities
plot_estimataion_output<- function(params, se, labels, category_for_title, category_for_save,transitory_only=FALSE,category_label="") {
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
    this_legend=c(expression(paste("Transitory MPX")))
  } else {
    param_cols=3:4
    this_legend=c(expression(paste("Permanent MPX")),expression(paste("Transitory MPX")))
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=this_colors)
  plotTop = max(max(params[,param_cols]),1.0)
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        ylab = "MPX", border="black", axes=TRUE,xlab="")
  title(xlab="Liquid Wealth", mgp=c(5.5,1,0))
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
         barCenters,
         t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(8, plotTop, legend=this_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
  #dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
  dev.off()
  
  #print for 508 compliance for FEDS paper
  print(labels)
  print(params)
  
}
###############################################################################

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
