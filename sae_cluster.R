attach(data_sae)
library(sae)
sae_cluster <- function(data,cluster='km',nclust=3,hc_method='complete',nsample=10) {
  d <- ncol(data)
  if(cluster=='km') {
    clust <- kmeans(data[,-c(1,2)],nclust)$cluster
  }
  else if(cluster=='hierar') {
    clust <- cutree(hclust(dist(data[,-c(1,2)]), method=hc_method),nclust)
  }
  data$cluster <- clust
  set.seed(20)
  sample <- sample(1:nrow(data_clust),nsample,F)
  data_sae <- data[-sample,]
  y_test <- data[sample,1]
  clust_test <- data$cluster[sample]
  data_test <- data[sample,-c(1,d+1)]
  data_test[,1] <- rep(1,nrow(data_test))
  form <- 'data_sae[,1]~'
  for(i in c(3:d)) {
    if(i==d) {
      form <- paste0(form,'data_sae[,',i,']')
    }
    else {
      form <- paste0(form,'data_sae[,',i,']+')
    }
  }
  form <- as.formula(form)
  model <- eblupFH(form,data_sae[,2])
  vi <- EBLUP_FH(form,data_sae[,2])$u_Cap
  viclust <- cbind.data.frame(vi=vi,cluster=clust[-sample])
  vibar <- aggregate(.~cluster,data=viclust,mean)[,2]
  beta <- eblup1$fit$estcoef$beta
  tanpa_vibar <- colSums(t(data_test)*beta)
  dengan_vibar <- colSums(t(data_test)*beta)+vibar[clust_test]
  hasil_est <- cbind.data.frame(without_vbar=tanpa_vibar,with_vbar=dengan_vibar)
  return(list(eblup=model,vi=viclust,vibar=vibar,estimation=hasil_est,final_data=data,no.sample=sample))
}

hasil$eblup$fit

hasil <- sae_cluster(data_clust)
hasil$final_data$LPK[hasil$no.sample]
data_analysis$logpkp[as.numeric(rownames(hasil))]

hasil <- lapply()

hasill<-cbind(hasil$estimation,hasil$final_data$LPK[hasil$no.sample])
#MAPE
#tanpa vibar
mean(abs((hasill[,1]-hasill[,3])/hasill[,3]))
#dengan vibar
mean(abs((hasill[,2]-hasill[,3])/hasill[,3]))

#MSE
#tanpa vibar
mean((hasill[,1]-hasill[,3])^2)
#dengan vibar
mean((hasill[,2]-hasill[,3])^2)
save.image()
