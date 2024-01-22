###############################
### Create OR robust table ###


create_ORrob <- function(postglm,mypath,mystring,mytitle="Adjusted odds",mysubtitle="GPPS"){
  print("robust")
  print(coeftest(postglm, vcov = vcovHC(postglm, type="HC1")))
  
  a <- coeftest(postglm, vcov = vcovHC(postglm, type="HC1"))
  
  # https://github.com/tidymodels/broom/issues/663
  robustse <- function(o_glm, coef = c("logit", "odd.ratio")) {
    
    myvcov = vcovHC(postglm, type="HC1")
    
    a <- coeftest(postglm, vcov = myvcov )
    a<- as.data.frame(unclass(a))
    
    b <- coefci(postglm,vcov=myvcov)
    b <- as.data.frame(b)
    
    df_join <- a
    df_join$LCI95=b[,"2.5 %"]
    df_join$UCI95=b[,"97.5 %"]
    
    if (coef == "logit") {
      
      return(df_join) # return logit with robust SE's
    } else if (coef == "odd.ratio") {
      df_join[, 1] <- exp(df_join[, 1]) # return odd ratios with robust SE's
      df_join[, 2] <- df_join[, 1] * df_join[, 2]
      df_join[,c("LCI95")]=exp(df_join[,c("LCI95")])
      df_join[,c("UCI95")]=exp(df_join[,c("UCI95")])
      return(df_join)
    } 
  }
  
  mf_01 <- robustse(postglm,coef="odd.ratio")
  
  mf_01 <- cbind(Characteristic = rownames(mf_01), mf_01)
  rownames(mf_01) <- 1:nrow(mf_01)
  
  mf_01 <- mf_01 %>% mutate("s.s."=ifelse(`Pr(>|z|)`<0.001,"***",
                                          ifelse(`Pr(>|z|)`<0.01,"**",
                                                 ifelse(`Pr(>|z|)`<0.05,"*",""))))
  
  gt_tbl <- gt(mf_01 %>% select(-c("Std. Error","z value")) %>% mutate_if(is.numeric,~round(.,3))) %>% tab_header(
    title = mytitle,
    subtitle = mysubtitle
  )
  gt_tbl
  
  
  
  # Use function from gt package to save table as neat png, save the original dataframe too
  gt::gtsave(gt_tbl, file = file.path(paste0(mypath,"/",mystring, "_glm_submission_rob.html")))
  write.csv(mf_01,file.path(paste0(mypath,"/",mystring, "_submission_rob.csv")))
  
  return(gt_tbl)
  
}
