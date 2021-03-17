require(barplot3d)
require(rgl
require(ks)

T=12
liq_wealth_dim = 5
income_dim = 5
var_perm = array(0, dim=c(income_dim,liq_wealth_dim))
var_tran = array(0, dim=c(income_dim,liq_wealth_dim))
ins_perm = array(0, dim=c(income_dim,liq_wealth_dim))
ins_tran = array(0, dim=c(income_dim,liq_wealth_dim))

var_perm_se = array(0, dim=c(income_dim,liq_wealth_dim))
var_tran_se = array(0, dim=c(income_dim,liq_wealth_dim))
ins_perm_se = array(0, dim=c(income_dim,liq_wealth_dim))
ins_tran_se = array(0, dim=c(income_dim,liq_wealth_dim))
for (i in 1:liq_wealth_dim){
  for (j in 1:income_dim){
    this_c_vector = scan(paste(txt_dir,'moments_by_inc_liq_quantile',i,j,'c_vector','.txt',sep=''))
    this_omega = as.matrix(read.csv(paste(txt_dir,'moments_by_inc_liq_quantile',i,j,'_omega','.txt',sep=''), header = FALSE))
    this_CS_output = CS_parameter_estimation(this_c_vector, this_omega,T)
    var_perm[i,j] = this_CS_output$var_perm
    var_tran[i,j] = this_CS_output$var_tran
    ins_perm[i,j] = this_CS_output$ins_perm
    ins_tran[i,j] = this_CS_output$ins_tran
    var_perm_se[i,j] = this_CS_output$var_perm_se
    var_tran_se[i,j] = this_CS_output$var_tran_se
    ins_perm_se[i,j] = this_CS_output$ins_perm_se
    ins_tran_se[i,j] = this_CS_output$ins_tran_se    
  }
}

max.colors <- 1000
cols <- heat.colors(max.colors)
min_MPX = min( t(ins_tran))
max_MPX =  max(t(ins_tran))
min_MPX = 0
max_MPX =  1

par3d(windowRect=c(50,50,1250,1150))
these_colors = cols[ vec(max.colors - floor(max.colors *( t(ins_tran) -min_MPX) /( max_MPX- min_MPX))) ]
barplot3d(rows=liq_wealth_dim ,cols=income_dim,z=vec(t(ins_tran)),scalexy=0.25,alpha=1.0,theta=30,phi=50,
          topcolors =vec(these_colors),sidecolors=these_colors, xlabels = 1:liq_wealth_dim ,ylabels=1:income_dim,
          xsub="Liquid Wealth Quintile",ysub="Income Quintile",zsub="MPX",gap=0)
rgl.snapshot(paste(figures_dir, "TranMPXByIncLiq.png",sep=""))

par3d(windowRect=c(50,50,1250,1150))
these_colors = cols[ vec(max.colors - floor(max.colors *( t(ins_perm) -min_MPX) /( max_MPX- min_MPX))) ]
barplot3d(rows=liq_wealth_dim ,cols=income_dim,z=vec(t(ins_perm)),scalexy=0.25,alpha=1.0,theta=30,phi=50,
          topcolors =vec(these_colors),sidecolors=these_colors, xlabels = 1:liq_wealth_dim ,ylabels=1:income_dim,
          xsub="Liquid Wealth Quintile",ysub="Income Quintile",zsub="MPX",gap=0)
rgl.snapshot(paste(figures_dir, "PermMPXByIncLiq.png",sep=""))



