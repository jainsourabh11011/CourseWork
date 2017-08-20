###R code for: Slawa's Power calculation Functions
###June 2013
###Slawa Rokicki srokicki@fas.harvard.edu


##Functions that exist: you could use the following code, which is equivalent to 
##>>sampsi .10 .25, power(.9) alpha(.05) nocont
##but can't do different ratios or continuity correction
power.prop.test(n=NULL, .10, .25, .05, .90, alternative="two.sided")


#########################
#####Function 1: Sample size calculation for proportions
#########################
sampsi.prop<-function(p1, p2, ratio=1, power=.90, alpha=.05, cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
     
     effect.size<-abs(p2-p1)
     avg.p<-(p1+ratio*p2)/(ratio+1)
     sd=ifelse(one.sample==FALSE, sqrt(ratio*p1*(1-p1)+p2*(1-p2)), sqrt(p2*(1-p2)))
     
     z.pow<-qt(1-power, df=Inf, lower.tail=FALSE)
     z.alph<-ifelse(two.sided==TRUE, qt(alpha/2, df=Inf, lower.tail=FALSE), qt(alpha, df=Inf, lower.tail=FALSE))
     ct<-(z.pow+z.alph)
     
     n1<-(z.alph*sqrt((ratio+1)*avg.p*(1-avg.p))+z.pow*sd)^2/(effect.size^2*ratio)
     n1.cont<-ifelse(cont.corr==FALSE, n1, (n1/4)*(1+sqrt(1+(2*(ratio+1))/(n1*ratio*effect.size)))^2)
     
     n<-(((z.alph*sqrt(p1*(1-p1)))+z.pow*sd)/effect.size)^2
     
     if(one.sample==FALSE){
          col1<-c("alpha", "power", "p1", "p2", "effect size", "n2/n1", "n1", "n2")
          col2<-c(alpha,  power, p1, p2, effect.size, ratio, ceiling(n1.cont), ceiling(n1.cont*ratio))
     }
     else{
          col1<-c("alpha", "power", "p", "alternative", "n")
          col2<-c(alpha, power, p1, p2, ceiling(n))
     }
     ret<-as.data.frame(cbind(col1, col2))
     ret$col2<-as.numeric(as.character(ret$col2))
     colnames(ret)<-c("Assumptions", "Value")
     
     description<-paste(ifelse(one.sample==FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of proportions", ifelse(cont.corr==FALSE, "without", "with"), "continuity correction")
     
     retlist<-list(description, ret)
     
     return(retlist)
}


##############################
#####Function 2: Sample size calculation for means
##############################

sampsi.means<-function(m1, m2, sd1, sd2=NA, ratio=1, power=.90, alpha=.05, two.sided=TRUE, one.sample=FALSE){
     
     effect.size<-abs(m2-m1)
     sd2<-ifelse(!is.na(sd2), sd2, sd1)
     
     z.pow<-qt(1-power, df=Inf, lower.tail=FALSE)
     z.alph<-ifelse(two.sided==TRUE, qt(alpha/2, df=Inf, lower.tail=FALSE), qt(alpha, df=Inf, lower.tail=FALSE))
     ct<-(z.pow+z.alph)
     
     n1<-(sd1^2+(sd2^2)/ratio)*(ct)^2/(effect.size^2)
     n<-(ct*sd1/effect.size)^2
     
     if(one.sample==FALSE){
          col1<-c("alpha", "power", "m1", "m2", "sd1", "sd2", "effect size", "n2/n1", "n1", "n2")
          col2<-c(alpha,  power, m1, m2, sd1, sd2, effect.size, ratio, ceiling(n1), ceiling(n1*ratio))
     }
     else{
          col1<-c("alpha", "power", "null", "alternative", "n")
          col2<-c(alpha, power, m1, m2, ceiling(n))
     }
     ret<-as.data.frame(cbind(col1, col2))
     ret$col2<-as.numeric(as.character(ret$col2))
     colnames(ret)<-c("Assumptions", "Value")
     
     description<-paste(ifelse(one.sample==FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of means")
     
     retlist<-list(description, ret)
     
     return(retlist)
}




##############################
#####Function : Cluster sample size 
##############################

samp.clus<-function(sampsi.object, rho, num.clus=NA, obs.clus=NA){
     
     if(is.na(num.clus)&is.na(obs.clus)) print("Either num.clus or obs.clus must be identified")
     else{
          
          so<-sampsi.object[[2]]
          n1<-as.numeric(so[so$Assumptions=="n1",2])
          n2<-as.numeric(so[so$Assumptions=="n2",2])
          
          if(!is.na(obs.clus)){
               deff<-1+(obs.clus-1)*rho
               n1.clus<-n1*deff
               n2.clus<-n2*deff
               num.clus<-ceiling((n1.clus+n2.clus)/obs.clus)
          }
          else if(!is.na(num.clus)){
               
               tot<-(n1*(1-rho)+n2*(1-rho))/(1-(n1*rho/num.clus) - (n2*rho/num.clus))
               if(tot<=0) stop("Number of clusters is too small")
               else{
                    obs.clus<-ceiling(tot/num.clus)
                    deff<-1+(obs.clus-1)*rho
                    n1.clus<-n1*deff
                    n2.clus<-n2*deff
               }
          }
          
          col1<-c("n1 uncorrected", "n2 uncorrected", "ICC", "Avg obs/cluster", "Min num clusters", "n1 corrected", "n2 corrected")
          col2<-c(n1, n2, rho, obs.clus, num.clus, ceiling(n1.clus), ceiling(n2.clus))
          ret<-as.data.frame(cbind(col1, col2))
          colnames(ret)<-c("Assumptions", "Value")
          return(ret)
     }
}


##############################
#####Function 4: Graphs power as a function of sample size
##############################

graph.power.prop<-function(from.power, to.power,p1, p2, ratio=1, power=.90, alpha=.05, cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
     
     seq.p<-seq(from.power, to.power, by=.01)
     n<-rep(NA, length(seq.p))
     
     for(i in 1:length(seq.p)){
          ob<-sampsi.prop(p1=p1, p2=p2, power=seq.p[i], alpha=alpha, ratio=ratio, cont.corr=cont.corr, two.sided=two.sided, one.sample=one.sample)[[2]]
          n[i]<-as.numeric(ob[7,2])
     }
     plot(n, seq.p, ylab="Power", xlab="n (in smaller arm)", type="l",  main=paste("Power graph for p1=", p1, "and p2=", p2))
}


graph.power.means<-function(from.power, to.power, m1, m2, sd1, sd2=NA, ratio=1, alpha=.05, cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
     
     seq.p<-seq(from.power, to.power, by=.01)
     n<-rep(NA, length(seq.p))
     
     for(i in 1:length(seq.p)){
          ob<-sampsi.means(m1=m1, m2=m2, sd1=sd1, sd2=sd2, power=seq.p[i], alpha=alpha, ratio=ratio, one.sample=one.sample, two.sided=two.sided)[[2]]
          n[i]<-as.numeric(ob[9,2])
     }
     plot(n, seq.p, ylab="Power", xlab="n (in smaller arm)", type="l",  main=paste("Power graph for m1=", m1, "and m2=", m2))
}


###############
###Examples
###############

##proportions
sampsi.prop(.5, .55, power=.8, one.sample=TRUE)
sampsi.prop(.5, .55, ratio=2, two.sided=FALSE)


##means
sampsi.means(0, 10, sd1=15, power=.8)
sampsi.means(10, 30, sd1=15, sd2=20, alpha=.1, ratio=1)


##clustering
ss<-sampsi.prop(.10, .25, power=.8, two.sided=FALSE)
samp.clus(ss, rho=.05, obs.clus=15)
samp.clus(ss, rho=.05, num.clus=150)

ss2<-sampsi.means(10, 15, sd1=15, power=.8)
samp.clus(ss2, rho=.05, obs.clus=15)
samp.clus(ss2, rho=.05, num.clus=5) 

##graphs
graph.power.prop(.6, 1, .1, .25)
graph.power.means(.7, 1, m1=0, m2=10, sd1=15)
graph.power.means(.5, 1, m1=0, m2=20, sd1=15)





