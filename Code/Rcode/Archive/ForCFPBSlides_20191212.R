
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
    this_legend=c(expression(paste("Transitory MPC")))
  } else {
    param_cols=3:4
    this_legend=c(expression(paste("Permanent MPC")),expression(paste("Transitory MPC")))
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=this_colors)
  plotTop = max(max(params[,param_cols]),1.0)
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        ylab = "MPC", border="black", axes=TRUE,xlab="")
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



require(shape)
# Function to plot MPX along with homeownership and liquid wealth
plot_Auclert_details<- function(params, se, labels, home_ownership, liquid_wealth, category_for_title, category_for_save) {
  
  params_input = cbind(params,home_ownership, liquid_wealth/max(liquid_wealth))
  se_input = cbind(se, matrix(0,nrow(se),2))
  category_for_save_input = category_for_save
  param_cols=4:6
  xlabel_pos = 2
  ylabel = "MPC"
  
  # Loop through plots for presentation
  for (i in 0:7){
    if (i==0 | i==1 | i==2){
      this_legend=c(expression(paste("Transitory MPC")))
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-2),matrix(0,nrow(params_input),2))
      se = se_input
      category_for_save = paste(category_for_save_input,"1",sep="")
      right_axis=FALSE
      if (i==0){
        category_for_save = paste(category_for_save_input,"blank",sep="")
      }
      if (i==2){
        category_for_save = paste(category_for_save_input,"1a",sep="")
      }
    }
    if (i==3 | i==4){
      this_legend=c(expression(paste("Transitory MPC")),"Homeownership")
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-1),matrix(0,nrow(params_input),1))
      se = se_input
      category_for_save = paste(category_for_save_input,"2",sep="")
      if (i==4){
        category_for_save = paste(category_for_save_input,"2a",sep="")
      }
      right_axis=FALSE
    }
    if (i==5 | i==6 | i==7){
      this_legend=c(expression(paste("Transitory MPC")),"Homeownership","Liquid Assets (Right Axis)")
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input
      se = se_input
      category_for_save = paste(category_for_save_input,"3",sep="")
      if (i==6){
        category_for_save = paste(category_for_save_input,"3a",sep="")
      }
      if (i==7){
        category_for_save = paste(category_for_save_input,"Paper",sep="")
        ylabel = "MPC or Homeownership rate"
      }
      right_axis=TRUE
    }
    #dev.new()
    pdf(paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
    par(mar=c(8,7,4,5)+1,cex.axis=1.2,cex.lab=1.5)
    #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=colors)
    plotTop = max(max(params[,param_cols]),1.0)
    barCenters <- barplot(height=t(params[,param_cols]),
                          names.arg=labels,
                          cex.names=0.75,
                          beside=TRUE,col=colors,
                          las=2,ylim=c(0,plotTop), xaxt="n",
                          main=paste(title_string, " by ",category_for_title),
                          ylab = ylabel,xlab="URE/Mean Expenditure", border="black", axes=TRUE, width = c(2,1,1))
    text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
    segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
             barCenters,
             t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
    arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
           angle=90,code=3, length=0.05)
    legend(10, plotTop, legend=this_legend, fill=colors,bty="n")
    if (right_axis){
      myRightAxisTics = pretty(seq(0, max(liquid_wealth), length.out = 10))
      myRightAxisAt = myRightAxisTics/max(liquid_wealth)
      myRightAxisLabs = paste('$',formatC(myRightAxisTics,format="d",big.mark=","))
      axis(4, at = myRightAxisAt, labels = myRightAxisLabs,las=1)
    }
    if (i==0){
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border=NA)
    }
    if (i==2){
      #label Medium MPC
      xpos=10
      ypos=0.4
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Medium MPC",col="black")
      #label Renters
      xpos=31.5
      ypos=0.57
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"High MPC",col="black")
      #label Wealth Homeowners
      xpos=48
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Low MPC",col="black")
    }
    if (i==4){
      #label Mortgaged Homeowners
      xpos=10
      ypos=0.7
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Homeowners",col="black")
      #label Renters
      xpos=35
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Renters",col="black")
      #label Wealth Homeowners
      xpos=46
      ypos=0.5
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Homeowners",col="black")
    }
    if (i==6){
      #label wealthy Hand-to-Mouth
      xpos=13
      ypos=0.235
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 12, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy Hand-to-Mouth",col="black")
      #label Poor Hand-to-Mouth
      xpos=35
      ypos=0.135
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 10, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Poor Hand-to-Mouth",col="black")
      #label Wealthy
      xpos=50
      ypos=0.85
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy",col="black")
    }
    if (i==7){
      #label Medium MPC
      xpos=13
      ypos=0.4
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 12, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy Hand-to-Mouth",col="black")
      #label Renters
      xpos=33
      ypos=0.57
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 10, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Poor Hand-to-Mouth",col="black")
      #label Wealth Homeowners
      xpos=48
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy",col="black")
    }
    #dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
    dev.off()
  }
}
###############################################################################


home_ownership_URE  = read.csv(paste(moments_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,1]
liquid_wealth_URE  = read.csv(paste(moments_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,3] #median liquid assets
liquid_wealth_URE = liquid_wealth_URE/(6.87)  #convert to 2015 USD

plot_Auclert_details(URE_quantile_params,URE_quantile_se,URE_quantile_set,home_ownership_URE,liquid_wealth_URE ,"URE Decile","UREdetails")

