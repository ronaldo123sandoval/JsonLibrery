# Json Librery
# Documentacion 
> autor: Ronaldo Sandoval Zambrana
## Descripción
Este módulo Haskell proporciona funciones para la conversión bidireccional entre strings y valores de datos JsonValue. Permite la serialización (codificación) de un string en formato JSON a un valor de datos JsonValue, y la deserialización (decodificación) de un valor de datos JsonValue a un string en formato JSON.
## Librerias utilizadas 
Algunas librerias que utilice para este proyecto son:
- Data.Char: Se utiliza para acceder a la función isDigit y isSpace, que se utilizan para determinar si un carácter es un dígito o un espacio en blanco, respectivamente.
- GHC.Unicode: Se utiliza para acceder a la función toLower, que se utiliza para convertir un carácter a minúsculas. Se utiliza en la función parseJsonValue para comparar cadenas en minúsculas con las palabras clave "true" y "false".
- Data.List se utiliza en el código para importar la función "intercalate". Esta función se utiliza para concatenar una lista de listas en una sola lista, intercalando un separador entre cada par de listas.
## Instrucciones de instalación:
El código de este repositorio no requiere seguir pasos específicos para la instalación. Para acceder al repositorio y ejecutar el código, simplemente necesitas tener un compilador de Haskell y GHCi (Glasgow Haskell Compiler interactive) instalados en tu sistema. Una vez que los tengas instalados, deberías poder ejecutar el código sin problemas. Puedes cargar el archivo Haskell en GHCi y llamar a las funciones directamente desde la línea de comandos de GHCi para probarlo
## Guía de uso
Para utilizar el código, puedes hacerlo de dos maneras. Una es utilizando la línea de comandos con GHCi (Glasgow Haskell Compiler interactive), y la otra es utilizando la función main proporcionada en el repositorio. Sin embargo, se recomienda utilizar GHCi, ya que te permite interactuar con el código de forma más flexible y realizar pruebas interactivas. Puedes cargar el archivo Haskell en GHCi y llamar a las funciones directamente desde la línea de comandos de GHCi para probar su funcionamiento.
## Estructura del código
# JsonBuilder
La estructura del código se organiza en un módulo llamado "JsonBuilder" que exporta una función llamada "fromJson". El módulo importa otros módulos como "Data.Char" y "GHC.Unicode" para utilizar algunas funciones de manejo de caracteres.
La función "fromJson" toma un String como entrada y devuelve un valor "Maybe JsonValue". Esta función es la entrada principal para el proceso de construcción de un valor "JsonValue" a partir de una cadena de texto en formato JSON.
El código hace uso de varias funciones auxiliares para analizar y procesar la cadena de texto. Algunas de estas funciones son:
- **parseJsonValue**: Analiza una cadena de texto y devuelve un valor "Maybe JsonValue". Esta función es recursiva y se encarga de analizar la estructura básica de un JSON, como objetos, listas, strings, booleanos, nulos y números.

![Imagen](/JsonLibrery/Image/parseJsonValue.PNG)

- **parseJsonObject**: Analiza una cadena de texto que representa un objeto JSON y devuelve un valor "Maybe JsonValue" del tipo "JObject" que contiene una lista de pares clave-valor.

![Imagen](/JsonLibrery/Image/parseJsonObject.PNG)

- **parseJsonArray**: Analiza una cadena de texto que representa una lista JSON y devuelve un valor "Maybe JsonValue" del tipo "JList" que contiene una lista de elementos.

![Imagen](/JsonLibrery/Image/parseJsonArray.PNG)

- **trim**: Elimina espacios en blanco innecesarios al principio y al final de una cadena de texto.

![Imagen](/JsonLibrery/Image/trim.PNG)

- **readValueJson**: Recibe una cadena de texto y devuelve un valor "JsonValue" a partir de la función "parseJsonValue". Si no se puede analizar la cadena de texto, se devuelve un valor "JNull".

![Imagen](/JsonLibrery/Image/readValueJson.PNG)

- **parseJsonString**: Analiza una cadena de texto que representa una cadena JSON y devuelve un valor "Maybe JsonValue" del tipo "JString" que contiene la cadena de texto sin procesar.

![Imagen](/JsonLibrery/Image/parseJsonString.PNG)

- **parseNull**: Analiza una cadena de texto que representa un valor nulo en JSON y devuelve un valor "Maybe JsonValue" del tipo "JNull" si la cadena de texto es igual a "null".

![Imagen](/JsonLibrery/Image/parseNull.PNG)

- **parseJsonNumber**: Analiza una cadena de texto que representa un número JSON y devuelve un valor "Maybe JsonValue" del tipo "JNumber" si se puede analizar correctamente. Se realizan algunas verificaciones en la parte entera y decimal del número para asegurarse de que estén en el formato correcto.

![Imagen](/JsonLibrery/Image/parseJsonNumber.PNG)

# JsonWrite

La estructura del código consta de un módulo llamado "JsonWriter" que exporta la función "writeJson". El módulo importa dos módulos: "Data.List" que proporciona la función "intercalate" para unir elementos de una lista con un separador, y "JsonObject" que define el tipo de dato "JsonValue" utilizado en la función "writeJson".

La función "writeJson" tiene un parámetro de entrada de tipo "JsonValue" y devuelve una cadena de caracteres (String). La función utiliza patrones para hacer coincidir el valor de "JsonValue" con los diferentes constructores del tipo de dato "JsonValue" definidos en el módulo "JsonObject", y genera la representación en formato JSON correspondiente para cada caso.

![Imagen](/JsonLibrery/Image/whiteJson.PNG)

Los patrones utilizados son los siguientes:

- Para el constructor "JString", se agrega el valor de la cadena entre comillas dobles.
- Para el constructor "JBool", se verifica si el valor booleano es verdadero o falso, y se genera la representación "true" o "false" respectivamente.
- Para el constructor "JNumber", se convierte el valor numérico a una cadena de caracteres utilizando la función "show".
Para el constructor "JList", se genera la representación de una lista en formato JSON, utilizando la función "map" para aplicar recursivamente la función "writeJson" a cada elemento de la lista, y la función "intercalate" para unir los elementos con comas y espacios.
- Para el constructor "JObject", se genera la representación de un objeto en formato JSON, utilizando la función "map" para aplicar recursivamente la función "showNode" a cada par clave-valor del objeto, y la función "intercalate" para unir los pares con comas y espacios.
- Para el constructor "JNull", se genera la representación "null" en formato JSON.

Adicionalmente, se define una función auxiliar llamada "showNode" que toma un par clave-valor del objeto (representado como una tupla de dos elementos), y genera la representación en formato JSON de la clave y el valor, separados por dos puntos y espacio. Esta función es utilizada en el patrón correspondiente al constructor "JObject" en la función "writeJson".

![Imagen](/JsonLibrery/Image/showNode.PNG)

# Notas de versión y cambios
El proyecto originalmente se trabajó en la plataforma de GitLab. Este código pasó por varias versiones mientras se trabajaba en GitLab, antes de ser transferido a GitHub como la versión final. Este código no fue creado en un solo día, sino que requirió varias dias y horas de trabajo.

En cada versión se tuvieron en cuenta varios casos diferentes que el string de entrada podría tener, para luego convertirlos en valores JSON.

``` 
Notas: Actualmente, el archivo "Main.hs" presenta un error que genera barras invertidas "\" adicionales al imprimir el resultado. Este error se corregirá en futuras implementaciones de functors. El programa se ejecuta correctamente en GHCi y se han realizado varios tests para contemplar diferentes casos en el código.
```