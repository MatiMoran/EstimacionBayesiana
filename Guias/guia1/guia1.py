import numpy as np
import matplotlib.pyplot as plt
# Ej 8:  Queremos saber la P de que gane A 3 veces (es decir que llegue a 6)
 
# a) y b)
A = 3
B = 5

# Metodo 1: tita = 3/8
tita_1 = 3/8
prob_1 = (3/8)**3
print("prob 1:", prob_1)

# Metodo 2: Buscar la P de que gane cada punto individualmente.
prob_2 = 4/10 * 5/11 * 6/12
print("prob 2:", prob_2)

# c) Metodo 3:  tita aleatorio entre 0 y 1

# c.1


# c.2
datitos = np.array([])
for i in range(1,10000):
    tita = np.random.uniform(low = 0, high = 1)
    A = np.random.binomial(8, tita)
    if A != 3:
        continue 
    prob_3 = tita ** 3
    datitos = np.append(datitos, prob_3)


mean= np.mean(datitos)
print("Esperanza", mean, "------------------")
fig, ax = plt.subplots(1)
ax.hist(datitos, bins = 20)
ax.set_xlim(0,1)
ax.axvline(prob_1, color = "red")
ax.axvline(prob_2, color = "green")
ax.axvline(mean, color ="violet")
plt.show()

# El resultado se parece a lo encontrado en B. Es decir que B tiene un amayor precision que A.

# d) Podriamos encontrarlo utilizando u = tita**3 

# 9. 

# a) Usando Prior beta de la prob que tiene Montiel de convertir un penal, 
# encontrar la distr post para tita y graficartla.

# Usando como prior beta(13, 2) xq nos parece una distr que modela bien la probabilidad de que un jugador convierta un penal.
# Despues de ver los datos de Montiel, nos queda una Posterior beta(25, 2)

tiros = np.array([])
for i in range(1,10000):
    tita = np.random.beta(25, 2)
    tiros = np.append(tiros, tita)

mean= np.mean(tiros)
print("Promedio", mean)
print("Esperanza", 13 / (13+1))
fig, ax = plt.subplots(1)
ax.hist(tiros, bins = 20)
ax.set_xlim(0,1)
ax.axvline(13 / (13+1), color ="red")
ax.axvline(mean, color = "green")
plt.show()

# b) Segun los Bayesianos, tenemos un 92.884% de probabilidades de ver a Montiel meter el 13vo tiro.
# Mientras que los frecuentistas dirian que es el 100%.

# c) Estamos tomando por sentado muchas cosas que deberian influir en su precision, (arquero, pelota, etc).
# Tmb estamos asumiendo que nadie mete el 100% de los tiros.

# d) 

tiros_predichos = np.array([])
for i in range(1,10000):
    tita = np.random.beta(25, 2)
    goles = np.random.binomial(10, tita)
    tiros_predichos = np.append(tiros_predichos, goles)
print(tiros_predichos)

mean= np.mean(tiros_predichos)
print("Promedio predicho", mean)
fig, ax = plt.subplots(1)
ax.hist(tiros_predichos, bins = 15)
ax.axvline(mean, color = "green")
plt.show()