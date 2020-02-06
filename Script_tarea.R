library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

# Medias ------------------------------------------------------------------

mean(conjunto$Altura)
mean(conjunto$Diametro)
mean(conjunto$Vecinos)


# Variable Altura ---------------------------------------------------------

H.media <- subset(conjunto,
                  conjunto$Altura <= 13.9432) 
mean(H.media$Altura)
H.16 <- subset(conjunto,
               conjunto$Altura < 16.5)
mean(H.16$Altura)

# Variable Vecinos --------------------------------------------------------

Vecinos.3 <- subset(conjunto,
                    conjunto$Vecinos <= 3)

Vecinos.4 <- subset(conjunto,
                    conjunto$Vecinos > 4)
mean(Vecinos.3$Vecinos)
mean(Vecinos.4$Vecinos)

# Variable diametro -------------------------------------------------------

DBH.media <- subset(conjunto,
                    conjunto$Diametro < 15.794)
DBH.16 <- subset(conjunto,
                 conjunto$Diametro > 16)
mean(DBH.16$Diametro)
mean(DBH.media$Diametro)

# Variable Especie --------------------------------------------------------

Cedro.rojo <- subset(conjunto,
                     conjunto$Especie == "C" )


Diametro.crojo <-subset(Cedro.rojo,
                        Cedro.rojo$Diametro <= 16.9)

Altura.crojo <- subset(Cedro.rojo,
                       Cedro.rojo$Altura > 18.5)

Tsuga <- subset(conjunto,
                conjunto$Especie == "H")

Diametro.tsuga <- subset(Tsuga, Tsuga$Diametro <= 16.9)

Altura.tsuga <- subset(Tsuga, Tsuga$Altura > 18.5)

Douglasia <- subset(conjunto, 
                    conjunto$Especie == "F")

HyF <- rbind(Tsuga, Douglasia)

ts.dou <- subset(conjunto, conjunto$Especie == "H" | conjunto$Especie == "F")

Diametro.dou <- subset(Douglasia, Douglasia$Diametro <= 16.9)

Altura.dou <- subset(Douglasia, Douglasia$Altura > 18.5)

# Histogramas -------------------------------------------------------------

par(mfrow=c(2,2))
hist(Tsuga$Altura, main = "Altura especie H",
     col="green", xlab= "Alturas", ylab="Frecuencia")
hist(Douglasia$Altura, main= "Altura especie F", 
     col="gray", xlab="Alturas", ylab="Frecuencia")
hist(Tsuga$Diametro, main = "Diametro especie H", 
     col="green", xlab="Diametros", ylab="Frecuencia")
hist(Douglasia$Diametro, main = "Diametro especie F", 
     col="gray", xlab="Diametros", ylab="Frecuencia")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(H.media$Altura, col="blue" , main= "Alturas menores o iguales a la media", xlab="Alturas", ylab = "Frecuencia")

hist(H.16$Altura, col="purple" , main= "Alturas menores a 16.5", xlab="Alturas", ylab = "Frecuencia")

hist(Vecinos.3$Vecinos, col="blue" , main= "Vecinos iguales o menores a 3", xlab="Vecinos", ylab = "Frecuencia")

hist(Vecinos.4$Vecinos, col="purple" , main= "Vecinos mayores a 4", xlab="Vecinos", ylab = "Frecuencia")

hist(DBH.media$Diametro, col="blue" , main= "Diametros menores a la media", xlab="Diametros", ylab = "Frecuencia")

hist(DBH.16$Diametro, col="purple" , main= "Diametros mayores a 16", xlab="Diametros", ylab = "Frecuencia")
par(mfrow=c(3,3))
# Desviación estandar -----------------------------------------------------


sd(conjunto$Altura)
sd(H.media$Altura)
sd(H.16$Altura)
sd(DBH.16$Diametro)
sd(DBH.media$Diametro)
sd(conjunto$Diametro) 
sd(conjunto$Vecinos)
sd(Vecinos.3$Vecinos)
sd(Vecinos.4$Vecinos)

