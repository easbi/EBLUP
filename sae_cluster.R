attach(data_sae)
library(sae)
sae_cluster <- function(data,cluster='km',nclust=3,hc_method='complete',m=2,nsample=10) {
  d <- ncol(data)
  if(cluster=='km') {
    clust <- kmeans(data[,-c(1,2)],nclust)$cluster
  }
  else if(cluster=='hc') {
    clust <- cutree(hclust(dist(data[,-c(1,2)]), method=hc_method),nclust)
  }
  if(cluster=='fcm') {
    clust <- fcm(data[,-c(1,2)],"u",nclust,m)$cluster
  }
  data$cluster <- clust
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
  beta <- model$fit$estcoef$beta
  tanpa_vibar <- colSums(t(data_test)*beta)
  dengan_vibar <- colSums(t(data_test)*beta)+vibar[clust_test]
  hasil_est <- cbind.data.frame(without_vbar=tanpa_vibar,with_vbar=dengan_vibar)
  return(list(eblup=model,vi=viclust,vibar=vibar,estimation=hasil_est,final_data=data,no.sample=sample))
}

simulasi <- function(data,cluster='km',nclust=3,hc_method='complete',m=2,nsample=10,b=100) {
  hasil <- lapply(1:b, function(x) sae_cluster(data,cluster,nclust,hc_method,m,nsample))
}

nsample <- 1:10*5
clust_met <- c("hc","km","fcm")
nclust <- 2:5

sim_res <- lapply(nsample, function(x) lapply(clust_met, function(y) lapply(nclust, 
                  function(z) simulasi(data_clust, y, z, 'complete',2,x, 100))))
sim_res[[1]][[1]][[1]][[2]]$estimation

get.mape.mse <- function(result) {
  fin.res <- c()
  for(i in 1:length(result)) {
    res<-cbind(result[[i]]$estimation,result[[i]]$final_data[hasil$no.sample,1])
    #MAPE
    mape1 <- mean(abs((res[,1]-res[,3])/res[,3])) #tanpa vibar
    mape2 <- mean(abs((res[,2]-res[,3])/res[,3])) #dengan vibar
    
    #MSE
    mse1 <- mean((res[,1]-res[,3])^2) #tanpa vibar
    mse2 <- mean((res[,2]-res[,3])^2) #dengan vibar
    fin.res <- rbind(fin.res,c(mape1,mape2,mse1,mse2))
  }
  colnames(fin.res) <- c("mape.no.vibar","mape.vibar","mse.no.vibar","mse.vibar")
  #data.frame(mape.novibar=mape1,mape.vibar=mape2,mse.novibar=mse1,mse.vibar=mse2)
  return(fin.res)
}

(a <- get.mape.mse(sim_res[[1]][[1]][[1]]))

colMeans(a)

hasil <- sae_cluster(data_clust,'fcm',2)

hasil$eblup$fit

hasil <- sae_cluster(data_clust,'fcm',2)
hasil$no.sample
hasil$estimation
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
