# Code Inspector SAP

## Introducción
### El Code Inspector SAP es una herramienta entregada por SAP para inspeccionar rápidamente el código ABAP personalizado.

* ¿Por qué utilizar el Code Inspector SAP?
* ¿Cómo configurar el inspector de código SAP?
* ¿Qué controles SCI se recomiendan en general?
* ¿Qué comprobaciones de SCI son imprescindibles para la preparación de S / 4 HANA?

## ¿Por qué utilizar el Code Inspector SAP?

El Code Inspector de SAP puede ser utilizado transversalmente por diversas entidades, es así que los pueden usar los desarrolladores y también las áreas que solicitaron los desarrollos o bien las áreas de auditoria. El inspector de código de SAP comprobará el código ABAP personalizado para:

* Posibles problemas de rendimiento
* Posibles restricciones de usabilidad
* Comprobaciones de programación robustas
* Uso de convenciones de nomenclatura de códigos ABAP
* Búsqueda de declaraciones (si desea que estén o que no)





## ¿Cómo configurar el inspector de código SAP?
La configuración de Code Inspector es bastante sencilla y se inicia mediante la transacción SCI.
 
 

En esta pantalla principal, vaya a la parte de Check Variant. Asígnele un nombre y  cree una nueva variante:
 

Al presionar crear nueva variante, se verá la pantalla de chequeo de variantes vacias.
 

Si presionamos sobre el icono i podemos obtener información del inspector que nos interese.
 

Luego podemos seleccionar los que nos interesen y grabamos.
 

## Establecer la variante como variante SCI POR DEFECTO
Si queremos ejecutar la herramienta SCI desde el editor de código, se utiliza la variante DEFAULT. Esta es una variante diferente a la que acabamos de crear. Para configurar la variante para las herramientas SCI para nuestra propia variante creada, vaya a tcode SE16 y edite el contenido de la tabla SCICHKV_ALTER:
 
 
 
 

## Ejecutando la herramienta SCI
La herramienta SCI se puede ejecutar desde diferentes lugares. Puede ejecutarlo desde tcode SCI ingresando el objeto o el transporte allí. O puede ejecutarlo desde el editor de código y seleccionando el menú Programa / Verificar / Inspector de código.
Una de las más fáciles es creando un nuevo INSPECTION, como sigue.
 
 
 

 
 
El resultado muestra un loop dentor de un looop:
Otra forma de visualizar el resultado:
 

Después de la ejecución, el desarrollador puede reparar lo que sea necesario y volver a ejecutar.

SAP SCI determinará la gravedad del problema encontrado en Crítico (error / rojo), Advertencias (amarillo) e Información (verde).

## Ajuste de la prioridad del mensaje SCI}
Tambien se puede ajustar la prioridad de los mensajes SCI. Algunos cheques que consideras mas o menos importantes se puede customizar.
Como se hace?
Primero, estando en la pantalla principal
 

 


Si se encuentra en la pantalla principal de SCI, elija la entrada del menú Inspector de código / Gestión de / Prioridades de mensajes, llegará a la pantalla para ajustar y afinar las prioridades:
 
Luego de esto podemos modificar el tipo de mensaje.
 

## Algunos  controles SCI a considerar
* 1.	Cumple nomenclatura CyT (Nombre de Programa y/o Transacción)
* 2.	Carátula informativa completa dentro del programa
* 3.	3.	Contiene las sentencias AUTHORITY-CHECK correspondientes
* 4.	Inexistencia parámetro DUMMY en el AUTHORITY-CHECK
* 5.	Inexistencia de sentencias SELECT * en el programa
* 6.	Inexistencia de sentencias SELECT  dentro de ciclos LOOP/ENDLOOP (Trabajar con tablas internas)
* 7.	Revisión de inexistencia de sentencias INSERT, UPDATE, MODIFY y DELETE a tablas estándar dentro del programa
* 8.	No están permitidas las lecturas a tablas internas (READ TABLE) sin la búsqueda binaria (BINARY SEARCH) o INDEX.
* 9.	Análisis de la condición WHERE para SELECT
* 10.	Búsqueda SELECT.. FOR ALL ENTRIES con tabla interna vacía
* 11.	No permitir código muerto en los programas nuevos Convención de Nombres


## Herramienta SCI y migración S/4 HANA
Cuando está en el proceso de migrar o está pensando en migrar a S / 4 HANA, las comprobaciones de la herramienta SCI juegan un papel central en la preparación del código ABAP personalizado.

1912445 - ABAP custom code migration for SAP HANA - recommendations and Code Inspector variants for SAP HANA migration
https://launchpad.support.sap.com/#/notes/1912445


