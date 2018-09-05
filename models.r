


m_ml_nb_pc_1<-glmer(deaths>0~
                      scale(sqrt(fines_pc)) + 
                      scale(pop_pct_men_15_34) + 
                      scale(pop_pct_pov) + 
                      scale(pop_pc_income) + 
                      scale(I(violent.yr/pop_tot)) + 
                      # scale(I(ft_sworn/pop_tot)) + 
                      factor(year_range) + 
                      (1|fips_st),
                    family = "binomial",
                    data = muni)

m_ml_nb_pc_2<-glmer.nb(deaths~
                         scale(sqrt(fines_pc)) + 
                         scale(pop_pct_men_15_34) + 
                         scale(pop_pct_pov) + 
                         scale(pop_pc_income) + 
                         scale(I(violent.yr/pop_tot)) + 
                         # scale(I(ft_sworn/pop_tot)) + 
                         factor(year_range) +  
                         offset(log(pop_tot)) + 
                         (1|fips_st),
                       data = muni%>%
                         filter(deaths>0))

summary(m_ml_nb_pc_1)
summary(m_ml_nb_pc_2)




### Neg bin, multilevel, fines per own source revenue



m_ml_nb_pct_1<-glmer(deaths>0~
                       scale(fines_pct_rev) + 
                       scale(pop_pct_men_15_34) + 
                       scale(pop_pct_pov) + 
                       scale(pop_pc_income) + 
                       scale(I(violent.yr/pop_tot)) + 
                       # scale(I(ft_sworn/pop_tot)) + 
                       factor(year_range) + 
                       (1|fips_st),
                     family = "binomial",
                     data = muni)

m_ml_nb_pct_2<-glmer.nb(deaths~
                          scale(fines_pct_rev) + 
                          scale(pop_pct_men_15_34) + 
                          scale(pop_pct_pov) + 
                          scale(pop_pc_income) + 
                          scale(I(violent.yr/pop_tot)) + 
                          # scale(I(ft_sworn/pop_tot)) + 
                          factor(year_range) + 
                          offset(log(pop_tot)) + 
                          (1|fips_st),
                        data = muni%>%
                          filter(deaths>0))

summary(m_ml_nb_pct_1)
summary(m_ml_nb_pct_2)

