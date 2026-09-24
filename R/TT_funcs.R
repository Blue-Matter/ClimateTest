
plot_TT_input = function(TTobj, modno = 1, specno = 1, simno = 1, cex = 0.8){
  npar = dim(TTobj)[5]
  parnams = dimnames(TTobj)[[5]]
  ncol = ceiling(npar^0.5); nrow=ceiling(npar/ncol)
  par(mfrow=c(nrow,ncol),mai=c(0.5,0.8,0.1,0.05))
  cols = c("black","green","red","blue")
  for(pp in 1:npar){
    matplot(t(TTobj[modno,specno,,simno,pp,]),lty=1, lwd=1.5, type="l", ylab = parnams[pp], col=cols); grid()
  }  
  legend('topright',legend=dimnames(AO)[[3]], text.col=cols, bty="n",cex=cex)
}
