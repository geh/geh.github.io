% Programación 1 - Parcial 2: arreglos y funciones
% 2020-06-22

Los ejercicios se deben hacer dentro de [JSLinux](http://tinyurl.com/prog1linux),
y con el editor de texto `vi`.


El código del ejercicio se deberá mandar al link que le pasará el profesor.

En cada ejercicio se debe tener por lo menos 1 punto para regularizar el parcial.

**No** es necesario que los programas compilen sin warnings con `gcc -Wall`, sin embargo
recomiento que prueben de vez en cuando para detectar posibles errores.

# 1. Arreglos (4 puntos)

Hacer este ejercicio en un programa llamado `arreglos.c`.

El archivo debe tener una constante `T` definida, que sirva para especificar
el tamaño de los arreglos:

~~~C
#define T 11
~~~

Usá esta constante `T` cada vez que sea relevante.

Luego, definí la función `main` de este programa de la forma siguiente:

 1. Declara un arreglo `a` de enteros, de tamaño `T`. Luego, con un bucle, llena este arreglo
    con los valores 1, 2, 3, ... hasta `T`.

 2. Solicitá al usuario ingresar dos valores enteros `n` y `m`, luego aplica la transformación
    siguiente al arreglo `a`:

    * al primer elemento le suma `n`
    * al segundo elemento le suma `m`
    * al tercer elemento le suma `n`
    * al cuarto elemento le suma `m`
    * ...

 3. Calculá e imprimí:
    * La suma de todos los elementos del arreglo que sean menores (estrictamente) a 20.
    * Cuántos valores son mayores o iguales a 20.

 4. Declara un segundo arreglo `c` de caracteres, de tamaño `T`. Luego, llena este arreglo de la manera siguiente:

    * para cada elemento entero del arreglo `a`:
        * si el elemento vale entre 0 y 25, colocar en el arreglo `c` el caracter correspondiente
          en el alfabeto minúsculo
        * sino, colocar el caracter `o` ( la "o" minúscula)

    Por ejemplo si el arreglo `a` contiene los valores 1,33,3,34,5 , el arreglo `c` deberá contener
    `b.d.f`.

 5. Imprimir el contenido del arreglo `c` al revés, carácter por carácter, usando un bucle y `printf`.

Para comprobar que tu implementación es correcta, con lo valores `n=-1` y `m=4` deberías ver la palabra `koimgkeicga`.

# 2. Funciones (4 puntos)

(En la segunda parte del parcial).
