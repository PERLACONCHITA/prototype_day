# Prototype_day. Uso de base de datos de hoteles para la predicción de la cancelación de reservaciones.
## Proyecto [[`Notebook`](https://github.com/PERLACONCHITA/prototype_day/blob/main/demo_ML_5.ipynb)] 
## Dataset [[`Base de datos`](https://github.com/PERLACONCHITA/prototype_day/blob/main/hotel_bookings%20(1).csv)]
## Documentación dataset [[`Hotel booking demand datasets`](https://www.sciencedirect.com/science/article/pii/S2352340918315191)]
## Presentación
## Introducción 
El sector hotelero es un elemento fundamental para la industria hotelera ya que cubre la necesidad de descanso y hospedaje, de acuerdo con (Gónzalez, 2019).

Euromonitor indica que las ventas totales del sector de alojamiento en Portugal alcanzaron los 4 mil 983 millones de euros en el 2018, del cual el 67% proviene de hoteles. Además Portugal ha sido de los países que ha recuperado pese a la pandemina originada por el COVID19.

La mayoría de los que integra este sector son microempresas con una participación del 65%, quedando las grandes empresas con un 9% de la oferta hotelera y el resto por la mediana empresa.

Sin embargo, pese a los esfuerzos de la industria, existen áreas de oportunidad ya que el consumidor de estos servicios es muy sensible al precio, el mercado es muy competitivo ya que las grandes empresas abarcan la mayoría de la demanda; y la fuerte competencia que Airbnb ejerce en el mercado. (Díaz, 2019)

Los hoteles para hacer frente deben mejorar sus procesos de gestión y dentro de ellos encontramos la cancelación de las reservaciones.

La cancelación de reservaciones es un comportamiento común en los consumidores, al igual que el cambio en sus reservaciones (como la duración de su estadía y el número de personas por reservación) y representa un problema para la industria ya que algunos costos aumentan (como adquisición de clientes y distriución). (Nuno, 2019)

Con la finalidad de estudiar lo anterior y proponer una solución, en este proyecto se utiliza una base de datos llamada Hotel booking demand datasets la cual contiene información sobre hoteles de ciudad y tipo resort y la descripción de sus reservaciones.

Por lo tanto el objetivo del presente proyecto es obtener un modelo que ayude a predecir el comportamiento de la cancelaciones.

Para lograrlo se divide en tres secciones:

1. Procesamiento de datos (correspondientes al módulo 3 del programa).
2. Análisis de datos (correspondiente al módulo 4 del programa)
3. Modelos de machine learning (correspondiente al módulo 5 del programa)
## Contenido del dataset
El dataset contiene información de dos hoteles de Portugal, resort (H1) y de ciudad (H2). Cuenta con 31 variables que describen 40,060 observaciones para el de resort y 79,330 observaciones para el de ciudad (H2). De las cuales 20 variables son númericas y 12 categóricas. Comprende información del 01 julio del 2015 al 31 de agosto del 2017.
