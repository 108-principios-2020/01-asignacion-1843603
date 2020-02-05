library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

# Medias ------------------------------------------------------------------

mean(conjunto$Altura)
mean(conjunto$Diametro)
mean(conjunto$Vecinos) 

# Variable Altura ---------------------------------------------------------

H.media <- subset(conjunto,
                  conjunto$Altura <= 13.9432) 
H.16 <- subset(conjunto,
               conjunto$Altura < 16.5)

# Variable Vecinos --------------------------------------------------------

Vecinos.3 <- subset(conjunto,
                    conjunto$Vecinos <= 3)

Vecinos.4 <- subset(conjunto,
                    conjunto$Vecinos > 4)

# Variable diametro -------------------------------------------------------

DBH.media <- subset(conjunto,
                    conjunto$Diametro < 15.794)
DBH.16 <- subset(conjunto,
                 conjunto$Diametro > 16)

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

Diametro.dou <- subset(Douglasia, Douglasia$Diametro <= 16.9)

Altura.dou <- subset(Douglasia, Douglasia$Altura > 18.5)

# Histogramas -------------------------------------------------------------

hist(conjunto$Altura, col = "green" , xlab ="Altura", ylab="Frecuencia" , 
     main="Histograma de altura")

hist(conjunto$Vecinos, col="pink" , xlab ="Vecinos" , ylab="Frecuencia" , 
     main="Histograma de vecinos")   

hist(conjunto$Diametro, col="orange" , xlab = "Diámetro", ylab="Frecuencia" ,
     main="Histograma de diámetro")

# Desviación estandar -----------------------------------------------------


sd(conjunto$Altura)
sd(conjunto$Diametro) 
sd(conjunto$Vecinos)

