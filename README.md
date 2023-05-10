# Exemplary-Code-Chunk
## Linear Regressions with Survey Data

The `survey` package allows us to analyze complex samples by making the appropriate adjustments so the data fits in the desired model. Here, I used it to conduct linear regression analyses of the the  Special Eurobarometer 96.2 (2022) survey data about Europeans' attitudes towars immigration and integration.

``
eubar2021 <- eubar2021 %>% filter(complete.cases(w22)) #only keeping the complete cases of the weight variable
dclus_eu2021 <- svydesign(id = ~uniqid, #identifies the cluster of units, here we use the respondents'
                          #unique id
                          weights = ~w22, #indicating the sampling weights
                          data = eubar2021, #dataframe
                          nest = TRUE)


#SUCCESSFUL INTEGRATION------- 
#what is important for a successful integration?
#qb11_1: SHARE NAT CULT AND TRADITIONS ------

#NAT IDENTITY
A <- svyglm(qb11_1 ~ qa5_13 #dep. var (how important sharing nat. culture is) ~ indep. var (national identity salience)
            + qb6, #contact with immigrants
            design = dclus_eu2021) #specify the survey design
#EU IDENTITY
B <- svyglm(qb11_1 ~ qa6 + qb6, design = dclus_eu2021) #same but our predictor is national identity

se1 <- SE(A) #standard errors for each model
se2 <- SE(B)

stargazer(A, B, #introduce the models that you wish to appear 
          type = html, #type of output
          out = 'success_intg.html', #how you wish your output to be saved in your wd
          se = c(se1, se2), #list the se corresponding to their models
          )
``

For the remaining of the code and regression coefficients, you can check the `eurobarometers.R` file.
