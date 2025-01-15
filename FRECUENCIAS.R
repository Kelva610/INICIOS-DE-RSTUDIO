# DISTRIBUCION DE FRECUENCIAS

# PARA VARIABLE CUALITATIVA
#Ingresando los datos de la variable
datos <- c("Pepsi", "Coca-Cola", "Pepsi", "Pepsi", "Pepsi", "Fanta", 
"Coca-Cola", "Coca-Cola", "Fanta", "Guaraná",	"Coca-Cola", "Fanta", 
"Sprite", "Sprite", "Guaraná", "Sprite", "Pepsi", "Sprite", "Guaraná", 
"Pepsi", "Guaraná", 	"Fanta", "Pepsi", "Pepsi", "Guaraná", "Coca-Cola", 
"Fanta", "Pepsi", "Coca-Cola", "Pepsi", "Pepsi", "Fanta", "Coca-Cola", 
"Sprite", "Fanta", "Guaraná", "Sprite", "Pepsi", "Fanta", "Pepsi", 
"Fanta", "Guaraná", "Coca-Cola", 	"Guaraná", "Coca-Cola", "Sprite", 
"Fanta", "Coca-Cola" ,"Fanta", "Pepsi", "Sprite", "Guaraná", "Guaraná", 
"Pepsi", 	"Guaraná", "Sprite", "Pepsi", "Fanta", "Coca-Cola", "Fanta")
datos
#Frecuencias absolutas
fi <- table(datos)
#Frecuencias relativas (redondeadas a 4 decimales)
hi <- round(prop.table(fi),4)
#Frecuencias porcentuales
pi <- hi*100
# Crear la tabla con las columnas fi, hi, pi
tabla <- cbind(fi,hi,pi)
tabla
# Agregar una fila de totales
total <- c(sum(fi), sum(hi), sum(pi))
# Combinar la tabla con los totales
tabla_completa <- rbind(tabla, Total = total)
tabla_completa

# PARA VARIABLE CUANTITATIVA DISCRETA
# Ingresando los datos de la variable
datos <- c(6, 7, 1, 5, 6, 6, 7, 1, 3, 4, 6, 7, 
           5, 1, 6, 3, 7, 6, 4, 2, 7, 4, 6, 2, 
           1, 2, 6, 4, 6, 6, 7, 6, 4, 1, 6, 7, 
           5, 3, 1, 5, 4, 2, 1, 1, 1, 3, 6, 4, 
           3, 4, 7, 3, 7, 5, 6, 3, 7, 3, 5, 1)
Datos
# Frecuencias absolutas
fi <- table(datos)
# Frecuencias relativas (redondeadas a 4 decimales)
hi <- round(prop.table(fi), 4)
# Frecuencias porcentuales
pi <- hi * 100
# Frecuencias acumuladas absolutas
Fi <- cumsum(fi)
# Frecuencias acumuladas relativas
Hi <- cumsum(hi)
# Frecuencias acumuladas porcentuales
Pi <- cumsum(pi)
# Crear la tabla con las columnas fi, hi, pi, Fi, Hi, Pi
tabla <- cbind(fi, hi, pi, Fi, Hi, Pi)
# Mostrar la tabla sin totales (opcional para verificar)
tabla


# PARA VARIABLE CUANTITATIVA CONTINUA
# Ingresando los datos de la variable
datos <- c(19, 29, 11, 13, 13, 28, 11, 22, 
           14, 22, 34, 27, 6, 11, 31, 9, 
           17, 14, 30, 33, 5, 25, 22, 32,
           7, 26, 14, 34, 13, 23, 15, 28, 
           23, 20, 25, 8, 11, 25, 34, 36, 
           33, 31, 30, 34, 17, 29, 32, 31, 
           23, 30, 28, 16, 6, 10, 19, 7, 
           26, 14, 16, 23, 5, 27, 30, 18, 
           22, 6, 16, 22, 13, 10, 31, 25)

# Número de observaciones
n <- length(datos)
# Calcular el rango de los datos
R <- max(datos) - min(datos)
R
# Número de clases usando la regla de Sturges
k <- ceiling(1 + 3.3 * log10(n))
k
# Calcular la amplitud de los intervalos
c <- ceiling(rango / k)
c

# Definir los límites de los intervalos de clase
limite_inferior <- min(datos)
limite_superior <- limite_inferior + c * k
# Crear los intervalos de clase
intervalos <- seq(limite_inferior, limite_superior, by = c)
# Asignar los datos a los intervalos creados
datos_clases <- cut(datos, breaks = intervalos, right = FALSE, include.lowest = TRUE)
fi <- table(datos_clases) # Frecuencia absoluta
hi <- prop.table(fi) # Frecuencia relativa
pi <- hi * 100 # Frecuencia porcentual
Fi <- cumsum(fi)   # Frecuencia acumulada absoluta
Hi <- cumsum(hi) # Frecuencia acumulada relativa
Pi <- cumsum(pi) # Frecuencia acumulada porcentual
# Crear la tabla de frecuencias
tabla_frecuencias <- cbind( "fi" = fi,
                            "hi" = round(hi, 4),
                            "pi" = round(pi, 2),
                            "Fi" = Fi,
                            "Hi" = round(Hi, 4),
                            "Pi" = Pi)
# Mostrar la tabla de frecuencias
tabla_frecuencias
# Agregar una fila de totales (solo para columnas relevantes)
total <- c(sum(fi), sum(hi), sum(pi), NA, NA, NA)
# Combinar la tabla con los totales
tabla_completa <- rbind(tabla_frecuencias, Total = total)
# Mostrar la tabla completa con totales
tabla_completa


