# Alternativas y Capacidades: análisis cuantitativo sobre la sociedad civil en México

## Descripción

Este proyecto tiene como objetivo analizar datos públicos sobre las organizaciones de la sociedad civil (OSC) en México, dentro del marco de la consultoría realizada para la organización Alternativas y Capacidades. Los principales hallazgos cuantitativos se pueden encontrar en este [reporte](https://docs.google.com/document/d/1recs3wDHMTqL7HWkmbMSk4KuxGhT-PKp/edit?usp=sharing&ouid=111796747923741384434&rtpof=true&sd=true).

Este repositorio contiene scripts y datos organizados en tres principales directorios: `import_clean`, `eda`, y `descriptives`.

## Directorios principales

* `import_clean`: Importa datos crudos, los limpia y transforma para su posterior análisis y visualización.
* `eda`: Análisis exploratorio de datos (EDA): se calculan figuras y generan visualizaciones para su potencial uso en la salida final. Tiene como objetivo identificar patrones, tendencias y relaciones de interés.
* `descriptives`: Genera visualizaciones y figuras descriptivas finales.

### Organización de los directorios principales

Cada uno de los directorios principales (`import_clean`, `eda`, `descriptives`) puede contener los siguientes subdirectorios para organizar y estandarizar el flujo de datos en este proyecto:

* `src`: scripts utilizados para transformar datos en los resultados de `output`.
* `output`: datos y gráficas generadas por los scripts de `src`.
  * Ejemplo: `descriptives/output` contiene los gráficas generadas por los scripts en `descriptives/src`
  * En el caso de una base de datos no se encuentra en `output`, probablemente se encuentra almacenada en Google Drive, dado su tamaño. Revisa el script en `src` para encontrar el id de la carpeta de Drive donde se encuentra almacenada.
* `hand`: diccionarios o otros archivos utilizados en los scripts de `src`.

## Licencia

Data Cívica 2025 ©

Para dudas sobre el contenido de este reposito, por favor contactar a Sierra Wells en sierra.wells@datacivica.org.

<!-- done -->
