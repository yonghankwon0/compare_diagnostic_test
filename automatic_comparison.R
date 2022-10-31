automatic <- function(y = y, X = X, data = data) {
  
  library(geepack); library(doBy); library(tidyverse); library(openxlsx); library(readxl); library(pROC)

  conf_mat<-function(roc.obj, x=0.5){
    coords_1 <- coords(roc.obj, x = x, ret=c("tp","tn","fp","fn"))
    coords_1$pos <- coords_1$tp+coords_1$fn
    coords_1$neg <- coords_1$tn+coords_1$fp
    coords_1$agree <- coords_1$tp+coords_1$tn
    coords_1$n <- coords_1$pos+ coords_1$neg
    
    coords_1$Sensitivity <- paste0(coords_1$tp,"/",coords_1$pos)
    coords_1$Specificity <- paste0(coords_1$tn,"/",coords_1$neg)
    coords_1$Accuracy <- paste0(coords_1$agree,"/",coords_1$n)
    
    return(coords_1)
  }
  
  X_numeric <- X[lapply(apply(data[,X], 2, table),length) > 2]
  X_factor <- na.omit(X[lapply(apply(data[,X], 2, table),length) <= 2])
  
  roc_list <- list()
  cut_list <- list()
  cut_point_list <- list()
  expected_list <- list()
  conf_mat_list <- list()
  
  if (length(X_numeric)<1) {
    for(i in 1:length(X_factor)) {
      expected_list[[X_factor[i]]] <- as.vector(unlist(data[,X_factor][,i]))
      conf_mat_list[[X_factor[i]]] <- cbind(threshold = NA,
                                            conf_mat(roc(as.formula(paste(y,"~",X_factor[i])), data = data)))
    }
    } else {
        for(i in 1:length(X_numeric)){
          roc_list[[X_numeric[i]]] <- roc(as.formula(paste(y,"~",X_numeric[i])), data = data)
          cut_list[[X_numeric[i]]] <- coords(roc_list[[i]], x=seq(0,1,0.05), ret=c("threshold","specificity","sensitivity","youden"))
          cut_point_list[[X_numeric[i]]] <- cut_list[[X_numeric[i]]][which.max(cut_list[[X_numeric[i]]]$youden),]
          expected_list[[X_numeric[i]]] <- c(ifelse(data[X_numeric[i]] > cut_point_list[[X_numeric[i]]]$threshold, 1, 0))
          conf_mat_list[[X_numeric[i]]] <- cbind(threshold = cut_point_list[[X_numeric[i]]]$threshold,
                                                 conf_mat(roc_list[[X_numeric[i]]],
                                                          x = cut_point_list[[X_numeric[i]]]$threshold))
      }
  }

  for(i in 1:length(X_factor)) {
    expected_list[[X_factor[i]]] <- as.vector(unlist(data[,X_factor][,i]))
    conf_mat_list[[X_factor[i]]] <- cbind(threshold = NA,
                                          conf_mat(roc(as.formula(paste(y,"~",X_factor[i])), data = data)))
  }
  expected <- (expected_list %>% unlist() %>% as.vector())
  
  df1 <- data.frame(id = rep(1:dim(data)[1], times = length(X)), 
                    result = expected,
                    y = rep(as.vector(unlist(data[y])), times = length(X)),
                    ag = (expected == rep(as.vector(unlist(data[y])), times = length(X))),
                    test = rep((expected_list %>% names()), each = dim(data)[1]))
  df1 <- arrange(df1, id)
  
  sn <- geeglm(result ~ test, id = id, data=df1, subset=pull(df1,y)==1, family=binomial(link="identity"))
  sp <- geeglm(result == 0 ~ test, id=id, data=df1, subset=pull(df1,y)==0, family=binomial(link="identity"))
  ac <- geeglm(ag ~ test, id=id, data=df1, family=binomial(link="identity"))
  
  L1 <- matrix(0, length(X), length(X))
  L1[,1] <- 1
  diag(L1) <- 1
  
  Ld <- c()
  for (i in 1:(length(X)-1)){
    for (i_1 in (i+1):length(X)){
      Ld <- rbind(Ld,(L1[i,] - L1[i_1,]))
    }
  }
  
  result1<-data.frame(endpoint = rep(c("Accuracy","Sensitivity","Specificity"), each=length(X)),
                      test=sn$xlevels$test,
                      est=rbind(round(esticon(ac, L = L1),4)[,c("estimate","lwr","upr")],
                                round(esticon(sn, L = L1),4)[,c("estimate","lwr","upr")],
                                round(esticon(sp, L = L1),4)[,c("estimate","lwr","upr")])*100, p=NA)
  test <- c()
  i_2 <- 1
  for(i in 1:(length(X)-1)){
    for(i_1 in (i+1):length(X)){
      test[i_2] <- paste0(sn$xlevels$test[i], "-", sn$xlevels$test[i_1]) #%>% print()
      i_2 <- i_2 + 1
    }
  }
  
  result1d <- data.frame(endpoint = rep(c("Accuracy", "Sensitivity","Specificity"),
                                        each = choose(length(X),2)), 
                         test = test,
                         est=rbind(round(esticon(ac, L=Ld),4)[,c("estimate","lwr","upr")],
                                   round(esticon(sn, L=Ld),4)[,c("estimate","lwr","upr")],
                                   round(esticon(sp, L=Ld),4)[,c("estimate","lwr","upr")])*100,
                         p = round(c(esticon(ac, L=Ld)$p.value, 
                                     esticon(sn, L=Ld)$p.value, 
                                     esticon(sp, L=Ld)$p.value),4))
  result<-bind_rows("estimate"=result1, "difference"=result1d, .id="type")
  result$estimates<-paste(result$est.estimate," (",result$est.lwr,", ",result$est.upr,")", sep="")
  result_<-result %>% pivot_wider(id_cols=c(type, test), names_from=endpoint, values_from=c(estimates,p))
  
  conf_table <- bind_rows(conf_mat_list)
  rownames(conf_table) <- names(conf_mat_list)
  conf_table <- conf_table[conf_table %>% rownames() %>% sort(),c("threshold","n","Accuracy","Sensitivity","Specificity")]
  
  return(list(conf_table,result_))
}
