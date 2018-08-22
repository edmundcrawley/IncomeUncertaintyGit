
# Function to plot MPX along with home ownership and liquid wealth
plot_Auclert_details<- function(params, se, labels, home_ownership, liquid_wealth, category_for_title, category_for_save) {

  params_input = cbind(params,home_ownership, liquid_wealth/max(liquid_wealth))
  se_input = cbind(se, matrix(0,nrow(se),2))
  category_for_save_input = category_for_save
  param_cols=4:6
  xlabel_pos = 2
  
  # Loop through plots for presentation
  for (i in 1:6){
    if (i==1 | i==2){
      this_legend=c(expression(paste(psi," Transitory MPX")))
      colors = c("#fc8d59","#91bfdb","#ffffbf")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-2),matrix(0,nrow(params_input),2))
      se = se_input
      category_for_save = paste(category_for_save_input,"1")
      right_axis=FALSE
      if (i==2){
        category_for_save = paste(category_for_save_input,"1a")
      }
    }
    if (i==3 | i==4){
      this_legend=c(expression(paste(psi," Transitory MPX")),"Home Ownership")
      colors = c("#fc8d59","#91bfdb","#ffffbf")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-1),matrix(0,nrow(params_input),1))
      se = se_input
      category_for_save = paste(category_for_save_input,"2")
      if (i==4){
        category_for_save = paste(category_for_save_input,"2a")
      }
      right_axis=FALSE
    }
    if (i==5 | i==6){
      this_legend=c(expression(paste(psi," Transitory MPX")),"Home Ownership","Liquid Assets (Right Axis)")
      colors = c("#fc8d59","#91bfdb","#ffffbf")
      params = params_input
      se = se_input
      category_for_save = paste(category_for_save_input,"3")
      if (i==6){
        category_for_save = paste(category_for_save_input,"3a")
      }
      right_axis=TRUE
    }
    dev.new()
    par(mar=c(8,7,4,5)+1,cex.axis=1.2,cex.lab=1.5)
    barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=colors)
    plotTop = max(max(params[,param_cols]),1.0)
    barCenters <- barplot(height=t(params[,param_cols]),
                          names.arg=labels,
                          cex.names=0.75,
                          beside=TRUE,col=colors,
                          las=2,ylim=c(0,plotTop), xaxt="n",
                          main=paste(title_string, " by ",category_for_title),
                          ylab = "MPX",xlab="URE/Mean Expenditure", border="black", axes=TRUE, width = c(2,1,1))
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
    if (i==2){
      #label Medium MPC
      xpos=10
      ypos=0.4
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"Medium MPX",col="black")
      #label Renters
      xpos=31.5
      ypos=0.57
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"High MPX",col="black")
      #label Wealth Homeowners
      xpos=48
      ypos=0.1
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"Low MPX",col="black")
    }
    if (i==4){
      #label Mortgaged Homeowners
      xpos=10
      ypos=0.7
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"Homeowners",col="black")
      #label Renters
      xpos=35
      ypos=0.1
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"Renters",col="black")
      #label Wealth Homeowners
      xpos=46
      ypos=0.5
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos+0.007,"Homeowners",col="black")
    }
    if (i==6){
      #label wealthy Hand-to-Mouth
      xpos=12
      ypos=0.2
      height = 0.05
      width = 12
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 12, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy Hand-to-Mouth",col="black")
      #label Poor Hand-to-Mouth
      xpos=35
      ypos=0.1
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 10, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Poor Hand-to-Mouth",col="black")
      #label Wealthy
      xpos=50
      ypos=0.85
      height = 0.05
      width = 10
      #rect(xpos-width/2,ypos-height/2,xpos+width/2,ypos+height/2 ,col= colors[2],border=NA)
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy",col="black")
    }
    dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
    dev.off()
  }
}
###############################################################################


home_ownership_URE  = read.csv(paste(tables_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,1]
liquid_wealth_URE  = read.csv(paste(tables_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,3] #median liquid assets
liquid_wealth_URE = liquid_wealth_URE/(6.87)  #convert to 2015 USD

plot_Auclert_details(URE_quantile_params,URE_quantile_se,URE_quantile_set,home_ownership_URE,liquid_wealth_URE ,"URE Decile","UREdetails")
