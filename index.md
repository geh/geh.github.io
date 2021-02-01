¡Hola! Soy Investigador Asistente del [CONICET](https://es.wikipedia.org/wiki/CONICET) con lugar de trabajo en la
[Universidad Nacional de Cordoba](https://es.wikipedia.org/wiki/Universidad_Nacional_de_C%C3%B3rdoba)
y Profesor Adjunto de la [Universidad Blas Pascal](https://es.wikipedia.org/wiki/Universidad_Blas_Pascal).

# Apuntes de materias

* 2020: [Programacion Declarativa](pd20) (Univ Blas Pascal)
* 2020: [Matematicas Discretas III](md20) (Univ Blas Pascal)
* 2020: [Programacion 1](prog20) (Univ Blas Pascal)
* 2018: [Calculo Numerico/Metodos Numericos (taller)](2018_mn) (Univ Blas Pascal)
* 2018: [Complejidad Computacional](2018_cc) (Univ Nacional de Cordoba)
* 2018: [El sistema de tipos de Haskell](2018_rio_haskell) (Univ Nacional de Rio Cuarto)
* 2017: [Data Structures](2017_ds) (Univ Blas Pascal)
* 2015: [Logica Modal Computacional](2015_logicas_modales) (Univ Nacional de San Juan)
* 2014: [Inteligencia Artificial](2014_ia) (Univ Blas Pascal)
* 2014: [Complejidad Computacional](2014_cc) (Univ Nacional de San Juan)
* 2012: [Complejidad Computacional](2012_cc) (Univ Nacional de Cordoba)

# Investigacion

* [Publicaciones en DBLP](http://dblp.uni-trier.de/pers/hd/h/Hoffmann:Guillaume)
* [Perfil en Google Scholar](https://scholar.google.com.ar/citations?user=d0O_hjsAAAAJ)

# Otras cosas

* [Darcs, un sistema de control de versiones](http://darcs.net)
* [mi blog](http://guiom.tumblr.com/)
* contacto: <guillaumh@gmail.com>


# Posts

<div class="posts">
  {% for post in site.posts %}
    <article class="post">

      <h1><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></h1>

      <div class="entry">
        {{ post.excerpt }}
      </div>

      <a href="{{ site.baseurl }}{{ post.url }}" class="read-more">Read More</a>
    </article>
  {% endfor %}
</div>

