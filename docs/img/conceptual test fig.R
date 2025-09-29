# make a figure showing conceptual tests

library(plotly)


ny = 40
ylabs = 2025+(1:ny)
Kmu = 0.99^(1:ny)
Kvar = exp(sin(seq(1,8,length.out=ny))*0.1)
plot(Kvar)
Kboth = Kmu*Kvar
plot(Kboth)
rat =
Mboth =1 / Kboth
Mrand = Mboth *  rlnorm(ny,0,0.03)
Krand = Kboth *  rlnorm(ny,0,0.03)
Mrand = Mrand/Mrand[1]
Krand = Krand/Krand[1]

par(mfrow=c(2,2))
plot(Kboth); plot(Krand)
plot(Mboth); plot(Mrand)

data = data.frame(M = Mrand, K=Krand, Year = ylabs, color=rainbow(ny,start=0.2,end=0.6))

plot_ly(data, x = ~K, y = ~Year, z = ~M, type = 'scatter3d', mode = 'lines',
               opacity = 1, line = list(width = 6, color = ~color, reverscale = FALSE))



nsim = 15
ny = 40
Kout = Mout = array(NA,c(ny, nsim))

ylabs = 2025+(1:ny)
Kmu = 0.99^(1:ny)
Kvar = exp(sin(seq(1,8,length.out=ny))*0.1)
#plot(Kvar)
Kboth = Kmu*Kvar
#plot(Kboth)
Mboth =1 / Kboth
for(i in 1:nsim){
  Mrand = Mboth *  rlnorm(ny,0,0.03)
  Krand = Kboth *  rlnorm(ny,0,0.03)
  Mout[,i] = Mrand/Mrand[1]
  Kout[,i] = Krand/Krand[1]
}
#par(mfrow=c(2,2))
#plot(Kboth); plot(Krand)
#plot(Mboth); plot(Mrand)

datas = data.frame(M = as.vector(Mout), K=as.vector(Kout), Year = rep(ylabs,nsim), color=rep(rainbow(ny,start=0.1,end=0.9),nsim))

plot_ly(datas, x = ~K, y = ~Year, z = ~M, type = 'scatter3d', mode = 'markers',
        opacity = 1, marker = list(width = 6, color = ~color, reverscale = FALSE))
        
        
        
# performance

nCT = 40
SST = seq(0,1,length.out=nCT)
col
MP1 = exp(sin(seq(1.5,2.5,length.out=nCT))*0.7)
MP2 = exp(sin(seq(1.5,3.3,length.out=nCT))*0.5)
MP3 = exp(sin(seq(1.5,4.5,length.out=nCT))*0.5)

MP1 = MP1/MP1[1]
MP2 = MP2/MP2[1]
MP3 = MP3/MP3[1]

matplot(SST,cbind(MP1,MP2,MP3),col="white",xlab="Ocean Warming Scenario (SST increase)",
        ylab="Spawning Stock Biomass Relative to 2025"); grid()
#axis(2)
PT = 0.8
R1 = approx(MP1,SST,PT)$y; R2 = approx(MP2,SST,PT)$y; R3 = approx(MP3,SST,PT)$y; 

col2 = rev(rainbow(nCT,start=0,end=0.35))
polygon(c(-10,0,0,-10),c(-1000,-1000,1000,1000),col=col2[1])
polygon(c(10,1,1,10),c(-1000,-1000,1000,1000),col=col2[nCT])

for(i in 1:(nCT-1)){
  loc = i+c(0,1,1,0); ys = c(-1000,-1000,1000,1000)
  polygon(SST[loc],ys,col=col2[i],border=col2[i])
}

MPcols = c("black","blue","darkgrey")
matplot(SST,cbind(MP1,MP2,MP3),xlab="",ylab="",type="l",col=MPcols,add=T,lty=1,lwd=2); grid()
abline(h = 0.8,col='black',lty=2,lwd=2)
abline(v = c(R1,R2,R3),col=MPcols,lty=2,lwd=2)

mtext(paste0("MP",1:3," = ",round(c(R1,R2,R3),2)),adj = c(R1,R2,R3),col=MPcols,line=0.5,cex=1.06)



