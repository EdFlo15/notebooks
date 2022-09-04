library(funModeling)
library(VIM)
library(stringr)
library(xtable)
library(ggplot2)
library(directlabels)
library(scales)
library(mice)
library(scales)
library(gridExtra)
library(stargazer)
library(readxl)
library(ggplot2)
library(leaps)
library(sjPlot)
library(broom.mixed)
library(ggstance)
library(olsrr)
library(sjPlot)
library(AER)
library(relaimpo)
library(jtools)
library(car)
library(tseries)
library(lmtest)
library(sandwich) 
library(vars)
library(psych)

data =read.csv("insurance.csv", stringsAsFactors = TRUE ,sep=",")

sapply(data, class)

# analicemos la variables expenses que va a ser la dependiente
summary(data$expenses)
# grafiquemos la variable dependiente.
hist(data$expenses)

# analicemos la correlación de variables
pairs(data[c("age","bmi","children","expenses")])

# analicemos otro tipo de gráfico

pairs.panels(data[c("age","bmi","children","expenses")])

#

table(data$region)

# escribamos el modelo

modelo=lm(expenses ~ ., data = data )
# de los coeficientes podemos decir que por un incremento en la edad de una unidad 
# se espera que el gasto medico aumente 256.8 dolare, asumiendo lo demás constante. Similarmente
# un hijo adicional genera un gasto medico de 475.7 dolare, un jncremento en el indice corporal
# de una unidad se espera que  339.3 dolares de gasto medico.

# importante notar que R le da manejo a las variables dummy

levels(data$sex)
levels(data$smoker)
levels(data$region)

# el sexo femenino tiene 131.4 dolares menos en gasto medicpor año y los fumadores tiene 
# 23847.5 dolares de gasto medico adicional anuales que aquellos que no fuman.

# evaluating model performance

summary(modelo)

# Se observa que existen variables individualmente significativas y el modelo conjuntamente es significativo,
# el R y el R ajustado explica el 75%  de la variación de Y.


# improving model performance
# TODAS LAS relaciones no necesariamente son lineales, sin emabrgo esto no siempre sucede, por ejemplo
# en la edad, a mayor edad el gasto es diferente esto se puede agrgar de la siguinete manera.

data$age2=data$age^2

modelo2=lm(expenses ~ ., data = data)
summary(modelo2)

# Transformation – converting a numeric variable
# to a binary indicator
# si se sospecha que el gasto e mucho mayor cuando la masa corporal es alta, podemos agregar una nueva variable,
# pero esto es intuitivo y depende de la experiencia, tambien se puede reemplazar la varaible BMI o adicionarla
# al modelo, todo depende del caso de estudio.

data$bmi30=ifelse(data$bmi>=30, 1, 0)

modelo3=lm(expenses ~ ., data = data)
summary(modelo3)

# es posible tambien crear interacciones si se sospecha de que dos varaibles están esterchamente relacionadas
# para el caso, es posible relacionar la varaible smoke con la masa corporal binaria creada anteriormente.
# R incluye la intereaccion automaricamnerte (bmi30* smoker)

modelo_mejorado=lm(expenses  ~ age+age2+children + bmi + sex+bmi30*smoker+region, data = data)
summary(modelo_mejorado)


# Making predictions with a regression model
# apliquemos el modelo a los datos originales y agreguemoslos a una nueva variable para propositos
# de comparación.

data$predict=predict(modelo_mejorado, data)

# analicemos la correlación

cor(data$expenses, data$predict)
# grafiquemos esto.
plot(data$predict, data$expenses)
abline(a=0, b=1, col="red", lwd=3, lty=2)

# hagamos lo mismo con ggplot

ggplot(data = data, aes(x=predict, y=expenses))+
   geom_point()+
   geom_smooth(method = lm, color="red", se=FALSE)

# del resultado anterior podemos observar pequeños puntos por encima de lo que el modelo esperada y otros
# por debajo, este resultado tienen una interpretación muy importante que en otros escenarios se puede
# ver en terminos de la eficiencia.

# ahora supongamos que se desea capturar clientes potenciales, podeos armar un CSV o pasar los datos
# en un dataframe para predecir dicho valor.

enrollments_men=data.frame(age=30, age2=30^2, children=2, bmi=30, sex="male", 
                       bmi30=1, smoker="no", region="northeast")

# predicción.

predict(modelo_mejorado, enrollments)
#5973.774

# Using this value, the insurance company might need to set its prices to about $6,000
# per year, or $500 per month in order to break even for this demographic group. To
# compare the rate for a female who is otherwise similar, use the predict() function
# in much the same way:

enrollments_women=data.frame(age=30, age2=30^2, children=2, bmi=30, sex="female", 
                       bmi30=1, smoker="no", region="northeast")

# predicción.

predict(modelo_mejorado, enrollments_women)
# 6470.543 entonces: 5973.774-6470.543= beta de la variable dummy sexo= -496.769

# analicemos el impacto de los hijos

enrollments_soon=data.frame(age=30, age2=30^2, children=0, bmi=30, sex="female", 
                             bmi30=1, smoker="no", region="northeast")

predict(modelo_mejorado, enrollments_soon)
# 6470.543-5113.34




######################################## omitimos esta parte#####################################

max.model=lm(expenses ~ age+sex+bmi+children+smoker+region, data=data)
summary(modelo)

max.model.formula <- formula(max.model)
fwd.model <- regsubsets(x = max.model.formula, y = expenses, data = data, nvmax = 100000, method = "forward")
best.fwd.model <- summary(fwd.model)
best.fwd.model.by.bic <- which.min(best.fwd.model$bic)
vars.fwd.model <- names(coef(fwd.model, best.fwd.model.by.bic))

best.fwd.model=lm(expenses ~ age+bmi+children+smoker, data = data)
summary(best.fwd.model)

back.model <- regsubsets(x = max.model.formula, y = expenses, data = data, nvmax = 100000, method = "backward")
best.back.model <- summary(fwd.model)
best.back.model.by.bic <- which.min(best.fwd.model$bic)
vars.back.model <- names(coef(fwd.model, best.fwd.model.by.bic))

# el mejor modelo:

best.fwd.model=lm(expenses ~ age+bmi+children+smoker, data = data)
summary(best.fwd.model)

# grafiquemos los residuos:

error=residuals(best.fwd.model)
data$error=error

ggplot(data=data, aes(x=age, y=error))+
   geom_point(position = "identity", color="red", size=3)+
   theme()

ggplot(data=data, aes(x=bmi, y=error))+
  geom_point(position = "identity", color="red", size=3)+
  theme()

ggplot(data=data, aes(x=smoker, y=error))+
  geom_point(position = "identity", color="red", size=3)+
  theme()

ggplot(data=data, aes(x=children, y=error))+
  geom_point(position = "identity", color="red", size=3)+
  theme()


res.fwd <- residuals(best.fwd.model)
ols_test_normality(res.fwd)

res.fwd <- residuals(best.fwd.model)
ks.test(res.fwd, y= pnorm)

jarque.bera.test(res.fwd)

# analisis indivudual de variables que causan heterocedasticidad

ols_test_breusch_pagan(best.fwd.model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")

# prueba de hocedaticidad

bptest(best.fwd.model, studentize = TRUE)

coeftest(best.fwd.model, vcov = (vcovHC(best.fwd.model)))  #    HC3 Davidson y MacKinnon (1993) 

######################################## omitimos esta parte#####################################
