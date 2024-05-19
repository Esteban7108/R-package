library(ggplot2)
library(lmtest)

# Función para realizar el análisis
analizar_vida_esperanza <- function(ruta_archivo) {
  # Leer los datos
  Base <- read.table(ruta_archivo, header = TRUE)

  # Calcular la correlación
  print(cor(Base))

# Esp_vida es la variable dependiente

# 1. Variable de relación lineal más fuerte con Esp_vida: Asesinatos

# Variable X: Asesinatos
# Variable Y: Esp_vida
n = nrow(Base)
x = c(Base$asesinatos)
y = c(Base$esp_vida)

x1 <- sum(x)
y1 <- sum(y)
x2 <- sum(x^2)
y2 <- sum(y^2)

sxx <- x2 - (n*(mean(x)^2))

xy <- sum(x*y)

sxy <- xy - n*mean(x)*mean(y)

b1 <- sxy/sxx

b0 <- (y1-(b1*x1))/(n)

# 2A) Interpreta B0 y B1 en el contexto del problema

# B0: Cuando no se presentan asesinatos en el año,
# la esperanza de vida se registra en 27.1829 años

# B1: Por cada asesinato registrado al año, la esperanza de vida
# disminuye 0.2976 años


# 2B) Gráfico de ddispersión

# Calcular valores predichos por el modelo de regresión
y_pred <- b0 + b1 * x

# Calcular los residuos
residuales <- y - y_pred


# Crear un data frame con los datos
df <- data.frame(x, y)
# Añadir los residuales al data frame
df$residuales <- residuales


ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "blue") +  # Agregar puntos
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +  # Agregar la recta de regresión
  geom_line(aes(x = x, y = y_pred), color = "red") +  # Añadir línea de la recta de regresión
  geom_segment(aes(x = x, y = y, xend = x, yend = y_pred),
               color = "blue", alpha = 0.6,
               arrow = arrow(length = unit(0.3, "cm"))) +  # Agregar líneas para los residuales
  labs(title = "Diagrama de Dispersión, Recta de Regresión y Residuales",
       x = "Asesinatos registrados al año", y = "Esperanza de Vida (Años)")
# 2C) Prueba de Significancia

# H0: B1 = 0
# Ha: B1 != 0

# Suma de los Cuadrados del Error

syy = sum(y^2)-n*(mean(y)^2)
sxy = sum(x*y) - n*mean(x)*mean(y)
sxx = x2 - (n*(mean(x)^2))

sce = syy - b1*sxy

# Error Cuadrático medio (E. C. M., M. S. E.)

ecm = sce/(n-2)

# Errores de Estimadores (B0 y B1)

eeb0 = sqrt((ecm)*((1/n)+((mean(x)^2)/(sxx))))
eeb1 = sqrt((ecm)/(sxx))

# P. H. de significancia del modelo

tcal = (b1)/(eeb1)
pt(tcal, n-2, lower.tail = FALSE)

# pt !< 0.5, por lo que pvalor se define así:
pvalor = 2 * pt(tcal, n-2, lower.tail = TRUE)

# Como Pvalor = 0 y es menor que alpha (0.05), se rechaza H0

# CONCLUSIÓN: Existe suficiente información para concluir que la variable
# 'Asesinatos' es significativa para el valor de la variable 'Esp_vida'
#Metodos secuenciales utilizados para encontrar el mejor modelo para k varibles

#Tercer punto

#Punto A
#Modelo vacio que no tiene variables independientes (y=b0)
Base.Vacio=lm(formula=Base$esp_vida~1,Base)
summary(Base.Vacio)
#Modelo completo con todas las variables independientes
Base.Completo=lm(formula=Base$esp_vida~.,Base)
summary(Base.Completo)
#Mejor modelo es el que tiene el menor AIC
#Regresion Backward
Base.Backward=step(Base.Completo,
                   scope =list(lower=Base.Vacio,upper = Base.Completo),
                   direction = 'backward')
summary(Base.Backward)


#Punto B complementar con tabla de excel
#Ho = 0
#Ha ≠ 0
#P_valor = 0 < 0.05
#Se rechaza la Ho significa que por lo menos 1 variable es significativa


#Punto C
#Significancia individual
#Habitantes
#Ho: b1 = 0
#Ha: b1 ≠ 0
#P_valor_habitantes = 0.052 < 0.05
#No se rechaza la Ho habitantes no es significativamente
#Asesinatos
#Ho: b2 = 0
#Ha: b2 ≠ 0
#P_valor_asesinatos = 0 < 0.05
#Se rechaza  la Ho es decir, la variable  asesinatos si es significante
#Universitarios
#Ho: b3 = 0
#Ha: b3 ≠ 0
#P_valor_universitarios = 0.002 < 0.05
#Se rechaza  la Ho es decir, la variable  univesitarios si es significante
#Heladas
#Ho: b4 = 0
#Ha: b4 ≠ 0
#P_valor_universitarios = 0.01 < 0.05
#Se rechaza  la Ho es decir, la variable heladas si es significante
an=anova(Base.Backward)
an
an1=aov(Base.Backward)
summary(an1)

#Punto D
#Intervalo de confianza
alpha = 0.05
valor_critico = qt(alpha/2, 45, lower.tail = F ) #Lower.tail(derecho = F, izquierdo = T)
#y = 71.030 + 0.00005b1 - 0.30010b2 +0.04658b3 - 0.00594b4
#Habitantes: 8
b1 = 8
b1_g = 71.030 + 0.00005*b1
lim_inf_hab= b1_g-(valor_critico*0.00003)
lim_sup_hab = b1_g+(valor_critico*0.00003)
#Por cada 8 habitantes la esperanza de vida esta entre un  71.0303% y 71.0304%
#Asesinatos: 7
b2 = 7
b2_g = 71.030 - 0.30010*b2
lim_inf_ase= b2_g-(valor_critico*0.03661)
lim_sup_ase = b2_g+(valor_critico*0.03661)
#Por cada 7 asesinatos la esperanza de vida esta entre 68.85% y un 69%
#Universitarios: 10
b3 = 10
b3_g = 71.030 +0.04658*b3
lim_inf_uni= b3_g-(valor_critico*0.01483)
lim_sup_uni = b3_g+(valor_critico*0.01483)
#Por cada 10 universitarios la esperanza de vida está entre un 71.46% y un 71.5256%
#Heladas: 2
b4 = 2
b4_g = 71.030 - 0.00594*b4
lim_inf_hel= b4_g-(valor_critico*0.00242)
lim_sup_hel = b4_g+(valor_critico*0.00242)
#Por cada 2 heladas la esperanza de vida está entre 71.0132% y un 71.0229%


#Punto E
#VALIDACIÓN DE SUPUESTOS
#Normalidad, homocedasticidad e independencia
#Normalidad
hist(Base.Backward$residuals,col = "red")
#residuals = erorres
qqnorm(Base.Backward$residuals)
qqline(Base.Backward$residuals, lwd=2, col='blue')
#Si los puntos pasan sobre o estan cercanos de la linea
#Prueba
shapiro.test(Base.Backward$residuals)
#como 0.525<0.05 como es falso, no se rechaza Ho entonces si es normal
#Homocedasticidad
plot(Base.Backward$fitted.values, Base.Backward$residuals)
abline(h=0, lwd=2, col='green')
bptest(Base.Backward)
#0.17 no se rechaza, son homocedasticos
#Independencia
plot(Base.Backward$fitted.values, Base.Backward$residuals)
abline(h=0, lwd=2, col='pink')
#Si hay patron no es independiente, no hay patron es independiente
dwtest(Base.Backward)
}
#0.3854<0.05, no se rechaza, es independiente
#Cumple con todos los supuestos

