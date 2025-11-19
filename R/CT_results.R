


CT_tabulate = function(Blist, RT = 0.9, subset = c(1,2,3,4,5)){

  MPs = rownames(Blist[[1]])
  nMPs = length(MPs)
  tests = names(Blist)
  ntests = length(tests)

  tab = array(NA,c(nMPs,ntests))

  for(tt in 1:ntests){ # use linear interpolation to fill table
    Bmat = Blist[[tt]]
    vals = as.numeric(colnames(Bmat))
    for(mm in 1:nMPs){
      tab[mm,tt] = floor(CT_approx(Bmat[mm,],vals,RT)$y)
    }
  }

  rownames(tab) = MPs
  colnames(tab) = tests
  tab[,subset]

}


makeCTtab = function(tab){
  labs = colnames(tab)[1:ncol(tab)]
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Management Procedure'),
        th(colspan = ncol(tab), "Climate Test")
      ),
      tr(
        lapply(labs, th, colspan=1) #function(x)th(colspan=1,x))
      )
    )
  ))

  dt<-datatable(tab,caption=NULL,rownames = T,container=sketch, options = list(
    searching = FALSE,  # Disables the global search box
    lengthChange = FALSE # Disables the "Show X entries" menu
  ))
  nbk <- 1000
  clrs <- rainbow(nbk + 1, start=0.05,end=0.21)
  for(j in 1:ncol(tab)){
    brks <- seq(0,max(tab[,j],na.rm=T),length.out=nbk)
    dt <- formatStyle(dt, columns = j, backgroundColor = DT::styleInterval(brks, clrs))# ,'text-align' = 'center')
  }

  dt
}


CT_proj = function(data, horizon = 30, test = NA, MPs = NA, RT=0.9,
                   rnd=1, denom = 1E3, unit = "kt", fracextra = 0.3,
                   CurYr = 2019, MPlab_adj = 1.2){

  tests = rownames(data$levlist)
  if(is.na(test[1]))test = tests[1]

  allMPs = data$MSEs[[1]][[1]]@MPs
  if(is.na(MPs[1]))MPs = allMPs

  MSEs = data$MSEs[[match(test,tests)]]
  nM = length(MSEs)

  Blist = CT_metrics(data)$SSB_relative
  Blisty = Blist[[match(test,tests)]]
  levs = round(as.numeric(colnames(Blisty)),rnd)

  MPnos = match(MPs, allMPs)
  nMP = length(MPnos)

  par(mfrow=c(nMP,2),mai = c(0.4,0.4,0.4,0.01),omi=c(0.25,0.25,0.01,0.01))
  nextra = ceiling(horizon * fracextra)

  for(mm in MPnos){
    cols = viridis(nM, begin=1, end=0)
    Bio = sapply(MSEs,function(X,mm,rng)c(mean(X@SSB_hist[,X@nyears]),apply(X@SSB[,mm,rng],2,mean)),mm=mm,rng=1:horizon)
    Bio = Bio / denom
    CT_proj_plot(Bio,cols,levs, RT=RT, nextra=nextra, CurYr = CurYr, Horizon = horizon, nyplot = horizon, test)
    mtext(MPs[mm],3,adj=MPlab_adj,outer=F,line=0.5,font=2)
    CT_intplot(Blisty,mm,MPs,test,levs,Bio,cols,horizon = horizon,RT=RT)

  }
  mtext(paste0("Mean Spawning Stock Biomass (",unit,")"),2,line=0.175,outer=T)
  mtext(c("Projection Year",paste0("% Decline in ",test," After ",horizon," Years")),1,adj=c(0.25,0.92),line=0.2,outer=T)

}


CT_proj_plot = function(Bio, cols, levs, RT=0.7,nextra=5, CurYr = 2019, Horizon = 21,nyplot = 20, test){
  endB = Bio[nrow(Bio),]
  Bio2 = rbind(Bio,array(NA,c(nextra,ncol(Bio))))
  Bio2 = Bio2[,ncol(Bio2):1]
  Yrs = (CurYr-1) +(1:nrow(Bio2))
  labx = horizon * 0.3
  matplot(Yrs, Bio2, ylim=c(0,max(Bio,na.rm=T)*1.025),col="white"); grid()
  abline(h=c(1,RT) * Bio[nrow(Bio),1],col=c("black","red"),lwd=1,lty=c(2,1))
  abline(v=c(CurYr+Horizon),lty=2,lwd=1)
  matplot(Yrs,Bio2,type="l",col=cols,add=T,lwd=2,lty=1)
  projx = CurYr+nrow(Bio2)-0.5-nextra/2
  text(projx,endB,paste0(levs,"%",test),col=rev(cols))
  text(CurYr+labx,Bio2[1,1]*0.93,"MP tuning",font=3)
  text(CurYr+labx,(Bio2[1,1]*RT)-Bio2[1,1]*0.07,"Robustness threshold",font=3,col="red")
}

CT_intplot = function(Blisty,mm,MPs,test,levs,Bio,cols,RT=0.7,horizon = 22){
  Bref = Bio[horizon+1,]
  plot(as.numeric(levs),Bref,ylim=c(0,max(Bio,na.rm=T)*1.025),pch=19,col="white")
  grid()
  abline(h=Bref,col=rev(cols),lty=2)
  abline(h=RT*Bref[1],col="red",lty=1)
  out = CT_approx(Bref,as.numeric(levs),Bref[1]*RT)$y
  abline(v=out,col="red")
  text(out-3,Bref[1]*0.05,paste0(round(out,2),"%"),col='red')
  legend('topright',legend=paste0("MP ",MPs[mm]," is '",test,floor(out),"' robust"),text.col='red',bg="#ffffff99",box.col=NA)
  points(as.numeric(levs),Bref,pch=19,col=rev(cols))
}#


glam_proj = function(Bio, cols, levs,RT=0.7,nextra=5, CurYr = 2019, Horizon = 21,nyplot = 20,miny=0.2){
  endB = Bio[nrow(Bio),]
  Bio = rbind(Bio,array(NA,c(nextra,ncol(Bio))))
  Bio = Bio[,ncol(Bio):1]
  Bio = Bio/mean(Bio,na.rm=T)
  Yrs = (CurYr-1) +(1:nrow(Bio))
  matplot(Yrs,Bio,ylim=c(miny,max(Bio,na.rm=T)*1.025),col="black"); grid()
  axis(1,col="white");  axis(1,c(-1000,10000), col="white")
  axis(2, col="white"); axis(2,c(-1000,10000), col="white")
  abline(h=c(1,RT) * Bio[Horizon,ncol(Bio)],col=c("black","red"),lwd=1,lty=c(2,1))
  abline(v=c(CurYr+Horizon-1),lty=2,lwd=1)
  matplot(Yrs,Bio,type="l",col=cols,add=T,lwd=2,lty=1)
  projx = CurYr+nrow(Bio)-0.5-nextra/2
  text(projx,endB,paste0("-",levs,"% K"),col=rev(cols))
  text(CurYr+5,Bio[1,1]*0.93,"MP tuning",font=3)
  text(CurYr+5,(Bio[1,1]*RT)-Bio[1,1]*0.07,"Robustness threshold",font=3,col="red")
}


summary_blank = function(x, y, yall, TT, RT, cols,grid){

  ngrad=length(cols)
  xs = range(x); ys = range(yall)
  plot(xs,ys, ylim = range(yall), col="white",axes=F,xlab="",ylab="")
  rx = xs[2] - xs[1]
  ry = ys[2] - ys[1]
  xp = c(xs[1]-rx, seq(xs[1],xs[2],length.out = ngrad-1), xs[2] + rx)
  yp = c(ys[1]-ry, ys[2]+ry)
  for(i in 1:ngrad) polygon(xp[c(i,i+1,i+1,i)], yp[c(1,1,2,2)],border=NA,col=cols[i])
  if(grid)grid()
  abline(h = c(TT,RT),col=c("black","red"),lty = c(2,2),lwd=2)
}

do_leg = function(cols, mplabcol,MPs){
  nMP = length(mplabcol)
  plot(1,1,col="white",axes=F,xlab="",ylab="")# polygon(c(-1E10,1E10,1E10,-1E10),c(-1E10,-1E10,1E10,1E10),col=cols[floor(length(cols)/1.5)],border=NA)
  lcol = c("black","red",mplabcol)
  legend('center',legend=c("MP tuning","Robustness threshold",MPs),
         text.col=lcol,
         lty=c(2,2,rep(1,nMP)),
         lwd=2,
         pch=c(NA,NA,rep(19,nMP)),
         col=lcol,
         cex=1.2,text.font=2,
         bg= cols[floor(length(cols)/1.01)],
         box.col="grey20")
}

plotedges =function(){
  rng=c(-1E10,1E10)
  for(ax in 1:4) axis(ax,rng,rng)
}

CT_approx = function(x,y,xout){
  ind0 = (1:length(x))[x > xout]
  ind = c(ind0,ind0[length(ind0)]+1)
  approx(x[ind],y[ind],xout)
}



#' Climate Test Step 4: Summarize the results of the marginal climate tests.
#'
#' Produces a figure showing the robustness of the various MPs with increasingly stringent climate tests
#'
#' @param data A hierarchical list produced in the third step by CT_3_test()
#' @param tests A named list of the tests to plot (e.g. c('M', 'K', 'S', 'R', 'C'). Optional, defaults to all tests.
#' @param MPs A named list of the MPs to plot (e.g. c('Ir_CT', 'It_CT'). Optional, defaults to all MPs
#' @param RT Positive real fraction. Robustness threshold - the fraction of the current SSB.  Used for interpolating to obtain the corresponding percentage level for each MP.
#' @param horizon Positive integer. The time horizon (number of projected years) at which the outcome (e.g. SSB) is tuned to be the same as current levels.
#' @param digits Positive integer. The number of significant digits for the robustness percentages for each MP on the plot.
#' @param grid Boolean. Should gridlines be superimposed on the plot?
#' @return A multipanel figure.
#' @examples
#' OM_list = list(BET_1,BET_2)
#' Hist_list = CT_1_prep(OM_list)
#' MPs_tuned = CT_2_tune(Hist_list, c("Ir","It"))
#' data = CT_3_test(Hist_list, MPs_tuned)
#' CT_4_summary(data)
#' @author T. Carruthers
#' @export
CT_4_summary = function(data, tests = NA, MPs = NA, RT = 0.9, horizon = 30, digits = 1, grid=F){

  allTests = rownames(data$levlist)
  if(is.na(tests[1]))tests = allTests
  nT = length(tests)

  allMPs = data$MSEs[[1]][[1]]@MPs
  if(is.na(MPs[1]))MPs = allMPs
  nMP = length(MPs)

  if(length(RT) == 1) RT = rep(RT, nT)

  mpcol = gray.colors(nMP,0,1,alpha=0.8)
  mplabcol = gray.colors(nMP,0,1)

  ncol = max(floor((nT+1)^0.5),2)
  nrow = ceiling((nT+1)/ncol)

  cols = rev(viridis(200, begin=1, end=0.1))
  par(mfrow = c(nrow,ncol),mai = c(0.55,0.4,0.18,0.01),omi=c(0.25,0.25,0.01,0.01))
  Blist = CT_metrics(data, horizon=horizon , 5)$SSB_relative
  yrng = range(unlist(Blist))
  yinc = (yrng[2]-yrng[1])/15
  for(tt in 1:nT){

    Blisty = Blist[[match(tests[tt],allTests)]]
    levs = as.numeric(colnames(Blisty))
    MPnos = match(MPs, allMPs)
    pdat = Blisty[MPnos,]
    summary_blank(x = levs, y=pdat, yall = unlist(Blist), TT = pdat[1,1],RT=RT[tt], cols=cols, grid=grid)
    matplot(levs, t(pdat), pch=19, cex=1.2, type="p",   col=mpcol, add=T)
    matplot(levs, t(pdat), pch=19, lwd=2,   type = "l", lty=1, col=mpcol, add=T)
    plotedges()
    axis(1)
    if(tt %in% ((0:20)*ncol+1)) axis(2)
    ino = match(tests[tt],CT_Interp$Code)
    mtext(paste0("% ",CT_Interp$Direction[ino], " in ", CT_Interp$Label[ino]),1,line=2.2,cex=0.9)
    mtext(paste0("(",tests[tt],")"),3,adj=0.01,line=0.18,font=2,cex=0.8)
    rob=rep(NA,nMP)
    for(mm in 1:nMP)  rob[mm] = CT_approx(x=pdat[mm,], y=levs, xout=RT[tt])$y
    abline(v=rob,col=mpcol,lty=1,lwd=1)
    xrng = range(levs); xinc = (xrng[2]-xrng[1])/10
    for(mm in 1:nMP)text(rob[mm]+xinc, yrng[1]+yinc*mm, paste0(round(rob[mm],digits),"%"), col=mplabcol[mm])

    #legend('bottom',legend=paste0(round(rob,digits),"%"),text.col=mplabcol,bty='n')

  }

  do_leg(cols, mplabcol, MPs)

  mtext(paste0("Relative SSB after ",horizon, " Proj. Yrs."),2,line=0.13,outer=T,font=2)
  mtext("Strength of Climate Test (% change)",1,line=0.2,outer=T,font = 2)

}



