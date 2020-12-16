library(tidyverse)
library(broom)
library(glmnet)
library(janitor)
library(caret)

df <- read_csv('./data/datos_finales.csv')
df <- tibble::rowid_to_column(df, "ID")

dip <- df %>%
        filter(votacion_final1 != 'presidente') %>%
        select(ID, genero, edad, provincia, bloque, votacion_final1) %>%
        mutate(
                votacion_final1 = factor(case_when(
                        votacion_final1 == 'negativo' ~  0 ,
                        votacion_final1 == 'abstencion' ~ 0,
                        votacion_final1 == 'afirmativo' ~ 1,
                        votacion_final1 == 'ausente' ~ 0,
                ), levels=c('0', '1'), labels=c('negativo', 'positivo')),
                genero = as.factor(genero),
                provincia = as.factor(provincia),
                bloque = as.factor(bloque)) %>%
        mutate(bloque = as.factor(case_when(
                bloque == 'frente de todos' ~ 'FDT',
                bloque == 'pro' | bloque == 'ucr' | bloque == 'coalicion civica' ~ 'JxC y aliados',
                TRUE ~ 'resto y provinciales')
        ),
        region = as.factor(case_when(
                provincia == 'tucuman' ~ 'NOA', 
                provincia == 'salta' ~ 'NOA',
                provincia == 'misiones' ~ 'NEA', 
                provincia == 'chaco' ~ 'NEA', 
                provincia == 'corrientes' ~ 'NEA', 
                provincia == 'santiago del estero' ~ 'NOA',
                provincia == 'jujuy' ~ 'NOA', 
                provincia == 'formosa' ~ 'NEA', 
                provincia == 'catamarca' ~ 'NOA', 
                provincia == 'la rioja' ~ 'NOA', 
                provincia == 'mendoza' ~ 'Cuyo', 
                provincia == 'san juan' ~ 'Cuyo',
                provincia == 'san luis' ~ 'Cuyo',
                provincia == 'cordoba' ~ 'Centro',
                provincia == 'santa fe' ~ 'Centro',
                provincia == 'entre rios' ~ 'Centro',
                provincia == 'rio negro' ~ 'Patagonia',
                provincia == 'neuquen' ~ 'Patagonia',
                provincia == 'chubut' ~ 'Patagonia',
                provincia == 'la pampa' ~ 'Patagonia',
                provincia == 'santa cruz' ~ 'Patagonia',
                provincia == 'tierra del fuego' ~ 'Patagonia',
                provincia == 'buenos aires' ~ 'Buenos Aires',
                provincia == 'ciudad autonoma de buenos aires' ~ 'CABA')
        )
        )


# Particion para tuneo
set.seed(789654)
cv_tune <- createFolds(y=dip$votacion_final1,
                       k=8,
                       list=TRUE,
                       returnTrain=TRUE)


# Instanciación modelos
## Regresion logistica
glm_control <- trainControl(
        method="cv",
        number = 5,
        index=cv_tune,
        classProbs=TRUE,
        summaryFunction=twoClassSummary
)

glm_mod <- train(
         form = as.factor(votacion_final1) ~ genero + edad + region + bloque + 
                 genero*edad + genero*bloque + genero*region + edad*bloque +
                 region*bloque,
         data = dip %>% select(-ID, -provincia),
         trControl = glm_control,
         preProc = c("center", "scale"),
         method = "glm",
         family = "binomial",
         metric='ROC'
)

cv_errors <- list()
rocs <- NA
it<-0
for (i in cv_tune){
        it<-it+1
        train <- dip %>% slice(i)
        test <- dip %>% slice(-1)
        m1 <-  glm(votacion_final1 ~ genero + edad + region + bloque + 
                           genero*edad + region*bloque,  
                   family = binomial('logit'), maxit = 200, data=train)
        
        y_probs <- predict(m1, test, type='response')
        y_pred <- as.factor(ifelse(y_probs <= 0.5, 'negativo', 'positivo'))
        
        cv_errors[[it]]<-confusionMatrix(y_pred, test$votacion_final1)$byClass
        rocs[it] <- roc(test$votacion_final1, y_probs, auc=TRUE)$auc
}

mean(rocs)
do.call(cbind, cv_errors) %>%
        rowMeans()

m1 <-  glm(votacion_final1 ~ genero + edad + region + bloque + 
                   genero*edad + region*bloque,  
           family = 'binomial', maxit = 200, data=dip)



tidy(m1) %>%
        ggplot() + 
                geom_bar(aes(x=p.value, y=reorder(term, -p.value), fill=p.value), 
                         stat='identity', show.legend = FALSE) +
                scale_fill_viridis_c() +
                geom_vline(xintercept = 0.05) +
                geom_vline(xintercept = 0.10) +
                theme_minimal() +
                labs(x='p-valor',
                     y='Variable')


ggsave('./notebook/img/pvalores.png')

library(margins)
mar_m1 <- margins(m1)
plot(mar_m1)



get_effects<-function(m1, region='buenos aires'){
        test <- data.frame(
                genero=rep(c('Mujer', 'Varón'), 80),
                edad=rep(20:99,2),
                bloque=rep(c('JxC y aliados'),80),
                region=rep(region, 80)
        ) %>%
                rbind(
                        data.frame(
                                genero=rep(c('Mujer', 'Varón'), 80),
                                edad=rep(20:99,2),
                                bloque=rep(c('FDT'),80),
                                region=rep(region, 80)        
                        )
                ) %>%
                rbind(
                        data.frame(
                                genero=rep(c('Mujer', 'Varón'), 80),
                                edad=rep(20:99,2),
                                bloque=rep(c('resto y provinciales'),80),
                                region=rep(region, 80)   
                )
                )
        
        test %>%
                mutate(probs = predict(m1, test, type='response')) %>%
                ggplot() +
                geom_line(aes(x=edad, y=probs, color=genero)) +
                facet_wrap(~bloque, scales = 'fixed') +
                #scale_y_log10() +
                theme_minimal() +
                labs(title = region)
        
        
}

library(patchwork)

get_effects(m1=m1, 'Buenos Aires')
ggsave('./notebook/img/buenos_aires.png')

get_effects(m1=m1, 'CABA')
ggsave('./notebook/img/CABA.png')

get_effects(m1=m1, 'Centro')
ggsave('./notebook/img/centro.png')


get_effects(m1=m1, 'Patagonia')
ggsave('./notebook/img/patagonia.png')

get_effects(m1=m1, 'NEA')
ggsave('./notebook/img/NEA.png')

get_effects(m1=m1, 'NOA')
ggsave('./notebook/img/NOA.png')

        
        

