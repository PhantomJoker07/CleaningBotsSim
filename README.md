# CleaningBotsSim
## Proyecto de Simulación - Programación Declarativa, 2022
### `Gabriel Fernando Martín Fernández C-411`

## Ejecución
Para entrar en el entorno se debe ejecutar el fichero Main.hs:
```bash
ghci Main.hs
```
Se inicializa la aplicación llamando a la función main la cual requiere 8 parámetros enteros:

n: tamaño de la primera dimensión del ambiente.

m: tamaño de la segunda dimensión del ambiente.

t: tiempo entre cambios aleatorios del ambiente.

kidsCount: cantidad de niños.

robotCount: cantidad de robots.

robotType: tipo de robot, 0 para el reactivo y 1 para el inteligente.

seed: semilla, si es 0 se utiliza IO unsafe para generar distribuciones aleatorias cada vez
que se llame a la función main de lo contrario se emplea el valor como semilla para ge-
nerar los valores aleatorios del problema, para una misma semilla se obtendrá una misma
distribución

turnsCount: cantidad de turnos a simular.
## Ejemplo:
```bash
main 5 5 1 2 3 1 0 10
```
