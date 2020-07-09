
##### ---- Required packages ---- ##### 

# library(stringr)
# library(ggplot2)
# library(car)

##### --------------------------- ##### 

GxEreg = function(data, pe, smoking, PRS, cov=NA, logistic=F) {
  
    ##### ---- Generate formula  ---- #####
  
    if (smoking == "pack_years_of_smoking_f20161_0_0") {
      f = str_interp("${pe} ~ ${smoking} * ${PRS} + Batch + Batch:${PRS} + Batch:${smoking}")
      pcs = paste0("PC", 1:10)
      for (pc in pcs){
        f_pc = str_interp("+ ${pc} + ${smoking}:${pc} + ${PRS}:${pc}")
        f = paste(f, f_pc)
      }
      
      if(is.na(cov)){
        message("No covariates included")
      }
      else {
        for (c in cov){
          f_cov = str_interp("+ as.factor(${c}) + ${smoking}:as.factor(${c}) + ${PRS}:as.factor(${c})")
          f = paste(f, f_cov)
        }
      }
      
    } else {
      f = str_interp("${pe} ~ as.factor(${smoking}) * ${PRS} + Batch + Batch:${PRS} + Batch:as.factor(${smoking})")
      pcs = paste0("PC", 1:10)
      for (pc in pcs){
        f_pc = str_interp("+ ${pc} + as.factor(${smoking}):${pc} + ${PRS}:${pc}")
        f = paste(f, f_pc)
      }
      
      if(is.na(cov)){
        message("No covariates included")
      }
      else {
        for (c in cov){
          f_cov = str_interp("+ as.factor(${c}) + as.factor(${smoking}):as.factor(${c}) + ${PRS}:as.factor(${c})")
          f = paste(f, f_cov)
        }
      }
    }
    

    
    ##### ---- Generate list of columns to include in regression dataframe ---- #####
  
    if (smoking == "pack_years_of_smoking_f20161_0_0") {
      var = cbind(pe, smoking, PRS, "Batch", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "pack.factor")
    } else {
      var = cbind(pe, smoking, PRS, "Batch", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
    }
    
    if(is.na(cov)){
      message("List of variables to include in regression model")
    }
    else {
      for (c in cov){
        v_cov = str_interp("${c}")
        var = cbind(var, v_cov)
      }
    }
    var = knitr::combine_words(var, sep = ",", and = "") # add commas between variables
    var = unlist(strsplit(var, ",")) # add quotes between variables
    print(var)
    
    ##########################################
    ##### ---- LOGISTIC REGRESSION  ---- #####
    ##########################################
    
    if (logistic == T){
      message("\nRunning GxE logistic regression model:")
      print(f) # Print formula
      gxe = glm(as.formula(f),
                family=binomial(link='logit'),
                data = na.omit(data[,var]))
      
      message("\nSummary of GxE logistic regression model")
      print(summary(gxe))
      
      message("Running analysis-of-variance tables for regression model\n")
      print(Anova(gxe))
      
      message("Calculating OR, this will take a bit!\n")
      OR = exp(cbind(OR = coef(gxe), confint(gxe))) 
      print(OR)
      invisible(gxe) # return model object without printing
    }
    
    ##########################################
    ##### ---- LINEAR REGRESSION  ---- #####
    ##########################################
    
    else{
      message("\nRunning GxE linear regression model:")
      print(f) # Print formula
      gxe = glm(as.formula(f),
                family="gaussian",
                data = na.omit(data[,var]))
      
      message("\nSummary of GxE linear regression model")
      print(summary(gxe))
      
      message("Running analysis-of-variance tables for regression model\n")
      print(Anova(gxe))
      invisible(gxe) # return model object without printing
    }
    
    ##########################################
    ##### ---- CREATE SCATTER PLOT  ---- #####
    ##########################################
    
    if (logistic == T) {
      ylabel = str_interp("Log odds ${pe}")
    } else {
      ylabel = str_interp("Probability of ${pe}")
    }
      
    if (smoking == "smoking_status_f20116_0_0") {
      
      Plotdf = data.frame(PRS = scale(gxe$data$X0.2),
                          linear.predictors = gxe$linear.predictors,
                          Smoking.status = factor(gxe$data$smoking_status_f20116_0_0, labels=c("Never","Former", "Current")))
      
      Line.smoking.never = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$Smoking.status == "Never",])
      Line.smoking.former = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$Smoking.status == "Former",])
      Line.smoking.current = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$Smoking.status == "Current",])
      
      print(qplot(PRS, linear.predictors, colour = Smoking.status, shape = Smoking.status, data = Plotdf) +
              geom_abline(intercept = Line.smoking.never$coefficients[1], 
                          slope = Line.smoking.never$coefficients[2], 
                          colour = "darkgreen") + 
              geom_abline(intercept = Line.smoking.former$coefficients[1], 
                          slope = Line.smoking.former$coefficients[2], 
                          colour = "red") + 
              geom_abline(intercept = Line.smoking.current$coefficients[1], 
                          slope = Line.smoking.current$coefficients[2], 
                          colour = "blue") + 
              theme_classic(base_size = 20) + 
              theme(axis.line = element_line(colour = "black", size=0.5), 
                    axis.text = element_text(colour = "black"), 
                    axis.line.x.top = element_blank(), 
                    axis.line.y.right = element_blank()) + 
              xlab(expression(PRS["(standardized)"])) + 
              ylab(ylabel) + 
              xlim(-5,4) +
              scale_colour_brewer(type="qual", palette = "Set2") + 
              labs(colour="Smoking\nStatus", shape="Smoking\nStatus"))
    } 
    
    else if (smoking == "maternal_smoking_around_birth_f1787_0_0") {
      
      Plotdf = data.frame(PRS = scale(gxe$data$X0.2),
                          fitted.values = gxe$fitted.values,
                          maternal.smoking = factor(gxe$data$maternal_smoking_around_birth_f1787_0_0, labels=c("No","Yes")))
      
      Line.maternal0 = lm(fitted.values ~ PRS, data=Plotdf[Plotdf$maternal.smoking == "No",])
      Line.maternal1 = lm(fitted.values ~ PRS, data=Plotdf[Plotdf$maternal.smoking == "Yes",])
      
      print(qplot(PRS, fitted.values, colour = maternal.smoking, shape = maternal.smoking, data = Plotdf) +
              geom_abline(intercept = Line.maternal0$coefficients[1], 
                          slope = Line.maternal0$coefficients[2], 
                          colour = "orange") + 
              geom_abline(intercept = Line.maternal1$coefficients[1], 
                          slope = Line.maternal1$coefficients[2], 
                          colour = "blue") + 
              theme_classic(base_size = 20) + 
              theme(axis.line = element_line(colour = "black", size=0.5), 
                    axis.text = element_text(colour = "black"), 
                    axis.line.x.top = element_blank(), 
                    axis.line.y.right = element_blank()) + 
              xlab(expression(PRS["(standardized)"])) + 
              ylab(ylabel) + 
              xlim(-5,4) +
              scale_colour_brewer(type="qual", palette = "Set1") + 
              labs(colour="Maternal\nSmoking", shape="Maternal\nSmoking"))
    }
    
    else if (smoking == "pack_years_of_smoking_f20161_0_0") {
      
      Plotdf = data.frame(PRS = scale(gxe$data$X0.2), 
                          linear.predictors = gxe$linear.predictors, 
                          pack.year = factor(gxe$data$pack.factor, labels=c("< 10","> 32","10-32")))
      
      Line.pack.low = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$pack.year == "< 10",])
      Line.pack.med = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$pack.year == "10-32",])
      Line.pack.high = lm(linear.predictors ~ PRS, data=Plotdf[Plotdf$pack.year == "> 32",])
      
      print(qplot(PRS, linear.predictors, colour = pack.year, shape = pack.year, data = Plotdf) +
              geom_abline(intercept = Line.pack.low$coefficients[1], 
                          slope = Line.pack.low$coefficients[2], 
                          colour = "#fa05a0") + 
              geom_abline(intercept = Line.pack.med$coefficients[1], 
                          slope = Line.pack.med$coefficients[2], 
                          colour = "orange") + 
              geom_abline(intercept = Line.pack.high$coefficients[1], 
                          slope = Line.pack.high$coefficients[2], 
                          colour = "blue") + 
              theme_classic(base_size = 20) + 
              theme(axis.line = element_line(colour = "black", size=0.5), 
                    axis.text = element_text(colour = "black"), 
                    axis.line.x.top = element_blank(), 
                    axis.line.y.right = element_blank()) + 
              xlab(expression(PRS["standardized"])) + 
              ylab(ylabel) + 
              xlim(-5,4) +
              scale_color_manual(values = c("#e60099", "#5680e9", "#e6b800")) + 
              labs(colour="Number\npacks/year", shape="Number\npacks/year"))
    }
    
    else {
      print("No code available for this smoking phenotype. Sorry!")
    }
}


