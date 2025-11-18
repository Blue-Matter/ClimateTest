


CT_tabulate = function(Blist, targlev = 0.9, subset = c(1,2,3,4,5)){

  MPs = rownames(Blist[[1]])
  nMPs = length(MPs)
  tests = names(Blist)
  ntests = length(tests)

  tab = array(NA,c(nMPs,ntests))

  for(tt in 1:ntests){ # use linear interpolation to fill table
    Bmat = Blist[[tt]]
    vals = as.numeric(colnames(Bmat))
    for(mm in 1:nMPs){
      tab[mm,tt] = floor(approx(Bmat[mm,],vals,targlev)$y)
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
    brks <- seq(0,max(tab[,j]),length.out=nbk)
    dt <- formatStyle(dt, columns = j, backgroundColor = DT::styleInterval(brks, clrs))# ,'text-align' = 'center')
  }

  dt
}


CT_proj = function(data, test = NA, MPs = NA, targlev=0.9, rnd=1, denom = 1E3, units = "kt"){

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
  mpadj = c(1.6,1.6)

  for(mm in MPnos){
    cols = viridis(nM, begin=1, end=0)
    Bio = sapply(MSEs,function(X,mm,rng)c(mean(X@SSB_hist[,X@proyears]),apply(X@SSB[,mm,rng],2,mean)),mm=mm,rng=1:21)
    Bio = Bio / denom
    CT_proj_plot(Bio,cols,levs, ref=targlev,nextra=6, CurYr = 2019, Horizon = 22,nyplot = 20)
    mtext(paste0(" MP (",MPs[mm],")"),3,adj=mpadj[mm],outer=F,line=0.5)
    CT_intplot(Blisty,mm,MPs,test,levs,Bio,cols,refyr = 22,ref=targlev)
    mtext(paste0("Mean spawning stock biomass (",units,")"),2,line=0.175,outer=T)
    mtext(c("Projection Year","% Decline in K After 20 Years"),1,adj=c(0.25,0.92),line=0.2,outer=T)
  }

}


CT_proj_plot = function(Bio, cols, levs,ref=0.7,nextra=5, CurYr = 2019, Horizon = 21,nyplot = 20){
  endB = Bio[nrow(Bio),]
  Bio = rbind(Bio,array(NA,c(nextra,ncol(Bio))))
  Bio = Bio[,ncol(Bio):1]
  Yrs = (CurYr-1) +(1:nrow(Bio))
  matplot(Yrs,Bio,ylim=c(0,max(Bio,na.rm=T)*1.025),col="white"); grid()
  abline(h=c(1,ref) * Bio[Horizon,ncol(Bio)],col=c("black","red"),lwd=1,lty=c(2,1))
  abline(v=c(CurYr+Horizon-1),lty=2,lwd=1)
  matplot(Yrs,Bio,type="l",col=cols,add=T,lwd=2,lty=1)
  projx = CurYr+nrow(Bio)-0.5-nextra/2
  text(projx,endB,paste0("-",levs,"% K"),col=rev(cols))
  text(CurYr+5,Bio[1,1]*0.93,"MP tuning",font=3)
  text(CurYr+5,(Bio[1,1]*ref)-Bio[1,1]*0.07,"Robustness threshold",font=3,col="red")
}

CT_intplot = function(Blisty,mm,MPs,type,levs,Bio,cols,ref=0.7,refyr = 22){
  Bref = Bio[refyr,]
  plot(as.numeric(levs),Bref,ylim=c(0,max(Bio,na.rm=T)*1.025),pch=19,col="white")
  grid()
  abline(h=Bref,col=rev(cols),lty=2)
  abline(h=ref*Bref[1],col="red",lty=1)
  out = approx(Bref,as.numeric(levs),Bref[1]*ref)$y
  abline(v=out,col="red")
  text(out-3,Bref[1]*0.05,paste0(round(out,2),"%"),col='red')
  legend('topright',legend=paste0("MP ",MPs[mm]," is '",type,floor(out),"' robust"),text.col='red',bg="#ffffff99",box.col=NA)
  points(as.numeric(levs),Bref,pch=19,col=rev(cols))
}#


glam_proj = function(Bio, cols, levs,ref=0.7,nextra=5, CurYr = 2019, Horizon = 21,nyplot = 20,miny=0.2){
  endB = Bio[nrow(Bio),]
  Bio = rbind(Bio,array(NA,c(nextra,ncol(Bio))))
  Bio = Bio[,ncol(Bio):1]
  Bio = Bio/mean(Bio,na.rm=T)
  Yrs = (CurYr-1) +(1:nrow(Bio))
  matplot(Yrs,Bio,ylim=c(miny,max(Bio,na.rm=T)*1.025),col="black"); grid()
  axis(1,col="white");  axis(1,c(-1000,10000), col="white")
  axis(2, col="white"); axis(2,c(-1000,10000), col="white")
  abline(h=c(1,ref) * Bio[Horizon,ncol(Bio)],col=c("black","red"),lwd=1,lty=c(2,1))
  abline(v=c(CurYr+Horizon-1),lty=2,lwd=1)
  matplot(Yrs,Bio,type="l",col=cols,add=T,lwd=2,lty=1)
  projx = CurYr+nrow(Bio)-0.5-nextra/2
  text(projx,endB,paste0("-",levs,"% K"),col=rev(cols))
  text(CurYr+5,Bio[1,1]*0.93,"MP tuning",font=3)
  text(CurYr+5,(Bio[1,1]*ref)-Bio[1,1]*0.07,"Robustness threshold",font=3,col="red")
}

