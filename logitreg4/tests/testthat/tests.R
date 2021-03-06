
#read in all trouble datasets and assign them a correct name
for(i in 1:5){
  assign(paste0("trouble", i), 
         readRDS(system.file(paste0("testdata/data-trouble", i, ".rds"), 
                             package="logitreg4")))
}

#missing value(s) in design
expect_equal(logitreg(trouble1$x, trouble1$y)$coefficients,
             unname(glm(trouble1$y ~ trouble1$x[,-1], 
                        family = binomial)$coefficients),
             tolerance = 0.0009)
 
#data is linear seperable, optim algorithm can't converge
expect_warning(logitreg(trouble2$x, trouble2$y))

#Only observations for one class, optim algorithm can't converge
expect_warning(logitreg(trouble3$x, trouble3$y))

#More covariables than observations 
expect_warning(logitreg(trouble4$x, trouble4$y))


#More covariables than observations after missing values are removed
expect_warning(logitreg(trouble5$x, trouble5$y))

