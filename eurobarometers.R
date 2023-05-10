#BUILD EUROPE-LEVEL DATA (NAT&IDENTITY)
setwd('~/MASTER/NATIONALISM & IDENTITY/Paper/DATA')

library("haven")
library('dplyr')
library('tidyverse')
library('magrittr')
library('devtools')
library(survey)
library('stargazer')

#EUROBAROMETER -----------------
#2021 -----------
eurobarometer2021 <- read_dta('eurobar 2021.dta')
eubar2021 <- eurobarometer2021  %>% select(uniqid, isocntry, w22, w94, starts_with('d11'),
                                           w94, qb6, starts_with('qb13'), starts_with('qb7ext'), 
                                          starts_with('qb7'), qb2, starts_with('q1'),
                                          starts_with('qb11'), starts_with('qb12'), 
                                          starts_with('qb16'), qa5_14,
                                          starts_with('qb15'),qb14, qa6, qa5_13, polintr
                                          )



#CREATE NATIONALITY VARIABLE--------
eubar2021 <- eubar2021 %>% pivot_longer(cols = starts_with('q1'), names_to = 'nationality',
                                        names_transform = list(nationality = as.character), values_to = 'nat')
eubar2021 <- eubar2021 %>% filter(nat == 1)
eubar2021 <- eubar2021 %>% select(-starts_with('q1'), -nat)

#CLEAN MISSINGNESS---------
eubar2021 <- eubar2021 %>% mutate_all(~replace(., . == 11, NA))
eubar2021 <- eubar2021 %>% mutate(qa6 = na_if(qa6, 9),
                                  qb14 = na_if(qb14, 4),
                                  qb15_1 = na_if(qb15_1, 5),
                                  qb6 = na_if(qb6, c(6)),
                                  qb6 = na_if(qb6, c(7)),
                                  qb6 = na_if(qb6, c(8)),
                                  qb7ext_1 = na_if(qb7ext_1, 9),
                                  qb7ext_6 = na_if(qb7ext_6, 9),
                                  qb7ext_3 = na_if(qb7ext_3, 9),
                                  qb7ext_4 = na_if(qb7ext_4, 9),
                                  qb11_7  = na_if(qb11_7, 9),
)


#2018-----------
eurobarometer2018 <- read_dta('eurobar 90.3 2018.dta')
eubar2018 <- eurobarometer2018  %>% select(uniqid, isocntry, w22, w94, 
                                           starts_with('q1'), qb1_1,
                                           qb1_2, qb2, qd3)
eubar2018 <- eubar2018 %>% pivot_longer(cols = starts_with('q1'), names_to = 'nationality',
                                        names_transform = list(nationality = as.character), values_to = 'nat')
eubar2018 <- eubar2018 %>% filter(nat == 1)
eubar2018 <- eubar2018 %>% select(-starts_with('q1'), -nat)
eubar2018 <- eubar2018 %>% mutate_all(~replace(., . == 9, NA),
                                      ~replace(., . == 5, NA))
#REGRESSIONS ---------
#2021 -----------
eubar2021 <- eubar2021 %>% filter(complete.cases(w22))
dclus_eu2021 <- svydesign(id = ~uniqid, weights = ~w22, data = eubar2021, nest = TRUE)


#UNCOMFORTABLE WITH 3+ CATEGORIES IMMIGRANT------
#NAT IDENTITY
allcat <- svyglm(qb7ext_6 ~ qa5_13 + qb6, design = dclus_eu2021, 
                    family = binomial(link = "logit"))
#EU IDENTITY
allcat1 <- svyglm(qb7ext_6 ~ qa6 + qb6, design = dclus_eu2021, 
                    family = binomial(link = "logit"))

se1 <- SE(allcat)
se2 <- SE(allcat1)
stargazer(allcat,allcat1, type = 'text', se = list(se1, se2))
#UNCOMFORTABLE WITH AT LEAST 1 CATEGORY IMMIGRANT------
#NAT IDENTITY
cat1 <- svyglm(qb7ext_4 ~ qa5_13 + qb6, design = dclus_eu2021, 
               family = binomial(link = "logit"))
#EU IDENTITY
cat2 <- svyglm(qb7ext_4 ~ qa6 + qb6, design = dclus_eu2021, 
               family = binomial(link = "logit"))

se3 <- SE(cat1)
se4 <- SE(cat2)
stargazer(cat1,cat2, type = 'text', se = list(se3, se4))

#SAVE TABLE ----------
stargazer(cat1,cat2,allcat,allcat1,
          type = 'html', out = 'unconfortable.html',
          se = list(se1, se2, se3, se4),
          title='Acceptance of Migrants',
          dep.var.caption = "Unconfortable with -categories of- Immigrants", 
          covariate.labels=c("National Identity", "European Identity", 'Intergroup Contact'),
          dep.var.labels = c('At Least One', 'Three or More'),
          align=TRUE, no.space = T)
#SUCCESSFUL INTEGRATION-------
#qb11_1: SHARE NAT CULT AND TRADITIONS ------
#NAT IDENTITY
A <- svyglm(qb11_1 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
B <- svyglm(qb11_1 ~ qa6 + qb6, design = dclus_eu2021)

se1 <- SE(A)
se2 <- SE(B)
#qb11_2: FEEL MEMBER OF NATIONAL SOCIETY ---------
#NAT IDENTITY
C <- svyglm(qb11_2 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
D <- svyglm(qb11_2 ~ qa6 + qb6, design = dclus_eu2021)
se3 <- SE(C)
se4 <- SE(D)
stargazer(A, B, C, D, type = 'html', se = list(se1, se2, se3, se4))
#qb11_3: ABLE SPEAK LANGUAGE--------
#NAT IDENTITY
E <- svyglm(qb11_3 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
G <- svyglm(qb11_3 ~ qa6 + qb6, design = dclus_eu2021)
se5 <- SE(E)
se6 <- SE(G)

#qb11_4: COMMITTED TO WAY OF LIFE IN (COUNTRY) ------
#NAT IDENTITY
H <- svyglm(qb11_4 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
I <- svyglm(qb11_4 ~ qa6 + qb6, design = dclus_eu2021)
se7 <- SE(H)
se8 <- SE(I)
stargazer(E, G, H, I, type = 'text', se = list(se5, se6, se7, se8))

#qb11_5: ACTIVE IN ORGANISATION/VOTING LOCAL ELECTION ------
#NAT IDENTITY
J <- svyglm(qb11_5 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
K <- svyglm(qb11_5 ~ qa6 + qb6, design = dclus_eu2021)
se9 <- SE(J)
se10 <- SE(K)

#qb11_6: CONTRIBUTE TO WELFARE SYSTEM BY TAXES -----------
#NAT IDENTITY
L <- svyglm(qb11_6 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
M <- svyglm(qb11_6 ~ qa6 + qb6, design = dclus_eu2021)
se11 <- SE(L)
se12 <- SE(M)
stargazer(J, K, L, M, type = 'text', se = list(se9, se10, se11, se12))

#qb11_7:  HAVING (NATIONALITY) FRIENDS ---------
#NAT IDENTITY
N <- svyglm(qb11_7 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
O <- svyglm(qb11_7 ~ qa6 + qb6, design = dclus_eu2021)
se13 <- SE(N)
se14 <- SE(O)

#qb11_8: EDUCATION AND SKILLS TO FIND A JOB ------
#NAT IDENTITY
P <- svyglm(qb11_8 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
Q <- svyglm(qb11_8 ~ qa6 + qb6, design = dclus_eu2021)
se15 <- SE(P)
se16 <- SE(Q)

#qb11_9: ACQUIRING (NATIONALITY) CITIZENSHIP--------
R <- svyglm(qb11_9 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
S <- svyglm(qb11_9 ~ qa6 + qb6, design = dclus_eu2021)
se17 <- SE(R)
se18 <- SE(S)
stargazer(N, O, P, Q, R, S, type = 'text', se = list(se13, se14, se15, se16, se17, se18))


#SAVE TABLE ----------
stargazer(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S,
          type = 'html', out = 'success4intg.html',
          se = list(se1, se2, se3, se4, se5, se6, se7, se8, se9, se10, 
                    se11, se12, se13, se14, se15, se16, se17, se18),
          title="Integration Support", 
          dep.var.caption = "Successful Integration", 
          covariate.labels=c("National Identity", "European Identity", 'Intergroup Contact'),
          dep.var.labels = c('Share nat. traditions','Member of nat. soc.', 'Speak language',
                             'Commit nat. way of life','Active in org./vote','Contribute to welfare system',
                             'Have (nat.) friends','Educ./skills to find job','(nat.) citizen'),
          align=TRUE, no.space=T)


#IMPORTANT FOR INTEGRATION --------
#qb13_1: IMMIGRANTS THEMSELVES ------
#NAT IDENTITY
A <- svyglm(qb13_1 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
B <- svyglm(qb13_1 ~ qa6 + qb6, design = dclus_eu2021)

se1 <- SE(A)
se2 <- SE(B)
#qb13_2: (NATIONALITY) CITIZENS ---------
#NAT IDENTITY
C <- svyglm(qb13_2 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
D <- svyglm(qb13_2 ~ qa6 + qb6, design = dclus_eu2021)
se3 <- SE(C)
se4 <- SE(D)
stargazer(A, B, C, D, type = 'text', se = list(se1, se2, se3, se4), out = "IMPORTANT FOR INTEGRATION.csv")
#qb13_3: (NATIONALITY) GOVERNMENT--------
#NAT IDENTITY
E <- svyglm(qb13_3 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
G <- svyglm(qb13_3 ~ qa6 + qb6, design = dclus_eu2021)
se5 <- SE(E)
se6 <- SE(G)

#qb13_4: EU INSTITUTIONS ------
#NAT IDENTITY
H <- svyglm(qb13_4 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
I <- svyglm(qb13_4 ~ qa6 + qb6, design = dclus_eu2021)
se7 <- SE(H)
se8 <- SE(I)
stargazer(E, G, H, I, type = 'text', se = list(se5, se6, se7, se8))

#qb13_5: LOCAL/REGIONAL AUTHORITIES ------
#NAT IDENTITY
J <- svyglm(qb13_5 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
K <- svyglm(qb13_5 ~ qa6 + qb6, design = dclus_eu2021)
se9 <- SE(J)
se10 <- SE(K)

#qb13_6: MEDIA -----------
#NAT IDENTITY
L <- svyglm(qb13_6 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
M <- svyglm(qb13_6 ~ qa6 + qb6, design = dclus_eu2021)
se11 <- SE(L)
se12 <- SE(M)
stargazer(J, K, L, M, type = 'text', se = list(se9, se10, se11, se12))

#qb13_7:  EDUCATION INSTITUTIONS ---------
#NAT IDENTITY
N <- svyglm(qb13_7 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
O <- svyglm(qb13_7 ~ qa6 + qb6, design = dclus_eu2021)
se13 <- SE(N)
se14 <- SE(O)

#qb13_8: CIVIL SOCIETY ACTORS ------
#NAT IDENTITY
P <- svyglm(qb13_8 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
Q <- svyglm(qb13_8 ~ qa6 + qb6, design = dclus_eu2021)
se15 <- SE(P)
se16 <- SE(Q)

#qb13_9: EMPLOYERS--------
R <- svyglm(qb13_9 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
S <- svyglm(qb13_9 ~ qa6 + qb6, design = dclus_eu2021)
se17 <- SE(R)
se18 <- SE(S)
stargazer(N, O, P, Q, R, S, type = 'text', se = list(se13, se14, se15, se16, se17, se18))




#SAVE TABLE -----
stargazer(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S,
          type = 'html', out = 'important4intg.html',
          se = list(se1, se2, se3, se4, se5, se6, se7, se8, se9, se10, 
                    se11, se12, se13, se14, se15, se16, se17, se18),
          title="Integration Support", 
          covariate.labels=c("National Identity", "European Identity", 'Intergroup Contact'),
          dep.var.caption = 'Important for Integration',
          dep.var.labels = c('Immigrants themselves', '(Nationality) citizens',
                             '(Nationality) government', 'EU institutions',
                             'Local/regional authorities', 'Media',
                             'education institutions', 'Civil society actors',
                             'Employers'),
          align=TRUE, no.space=TRUE)


#ACTIVELY PROMOTING INTEGRATION NECESSARY INVESTMENT (1-TOT AGREE, 4-TOT DISAGREE)-----
#NAT IDENTITY
a1 <- svyglm(qb15_1 ~ qa5_13 + qb6, design = dclus_eu2021)
#EU IDENTITY
a2 <- svyglm(qb15_1 ~ qa6 + qb6, design = dclus_eu2021)
a11 <- SE(a1)
a22 <- SE(a2)
stargazer(a1, a2, 
          type = 'html', out = 'intinv.html', 
          se = list(a11,a22),
          title="Integration Support", 
          covariate.labels=c("National Identity", "European Identity", 'Intergroup Contact'),
          dep.var.labels = "Actively promoting integration is a necessary investment",
          align=TRUE, no.space=TRUE)


#RECODE IV AS DUMMIES -------
eubar2018 <- eubar2018 %>% mutate(qd3 = as.character(qd3))
dummies <- model.matrix(~ qd3 - 1, data = eubar2018)
# combine the dummy variables with the original data frame
eubar2018 <- eubar2018 %>% filter(complete.cases(qd3))
eubar2018 <- cbind(eubar2018, dummies)
#RENAMING THE DUMMY VARS
eubar2018 <- eubar2018 %>% rename(nat = qd31,
                                  nat_eu = qd32,
                                  eu_nat = qd33,
                                  eu = qd34) 
eubar2018 <- eubar2018 %>% mutate(nat = as.factor(nat),
                                  nat_eu = as.factor(nat_eu),
                                  eu_nat = as.factor(eu_nat),
                                  eu = as.factor(eu))
#REGRESSIONS 2018-----
eubar2018 <- eubar2018 %>% filter(complete.cases(w22))
dclus_eu2018 <- svydesign(id = ~uniqid, weights = ~w22, data = eubar2018, nest = TRUE)
#FEELINGS TOWARDS IMMIGRATION----------
#qb1_1: FROM EU MEMBERS ---------
m1 <- svyglm(qb1_1 ~ nat, data = eubar2018, design = dclus_eu2018)
se1 <- SE(m1)

m2 <- svyglm(qb1_1 ~ nat_eu, data = eubar2018, design = dclus_eu2018)
se2 <- SE(m2)

m3 <- svyglm(qb1_1 ~ eu_nat, data = eubar2018, design = dclus_eu2018)
se3 <- SE(m3)

m4 <- svyglm(qb1_1 ~ eu, data = eubar2018, design = dclus_eu2018)
se4 <- SE(m4)

# qb1_2: FROM OUTSIDE THE EU-----------
m5 <- svyglm(qb1_2 ~ nat, data = eubar2018, design = dclus_eu2018)
se5 <- SE(m5)

m6 <- svyglm(qb1_2 ~ nat_eu, data = eubar2018, design = dclus_eu2018)
se6 <- SE(m6)

m7 <- svyglm(qb1_2 ~ eu_nat, data = eubar2018, design = dclus_eu2018)
se7 <- SE(m7)

m8 <- svyglm(qb1_2 ~ eu, data = eubar2018, design = dclus_eu2018)
se8 <- SE(m8)


stargazer(m1, m2, m3, m4, m5, m6, m7, m8,
          type = 'html', out = 'immeu2.html',
          se = list(se1, se2, se3, se4, se5, se6, se7, se8),
          title="Favorable Attitudes towards Migration", 
          dep.var.caption = "Feelings towards immigration", 
          covariate.labels=c("National Identity", "National+European Identity", 
                             "European+National Identity", 'European Identity'),
          dep.var.labels = c('From EU Members', 'From Outside the EU'),
          align=TRUE, no.space=TRUE)




