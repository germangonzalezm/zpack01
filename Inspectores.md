# Inspectores
## 1.	Convención de Nomenclatura de programas
-	Clase Original: CL_CI_TEST_ABAP_NAMING_NEW
-	Clase Destino: ZCL_CI_001
-	Method: Constructor
## 2.	Convención de nombres a los objetos internos.
-	Clase Origen: CL_CI_TEST_ABAP_NAMING
-	Clase destino: ZCL_CI_002
-	Method: Constructor
## 3.	Uso de Authority-check
-	Clase Original: CL_CI_TEST_FREE_SEARCH
-	Clase destino: ZCL_CI_003
## 4.	Uso de objeto especifico en Authority-check
-	Clase Original: CL_CI_TEST_FREE_SEARCH 
-	Clse destino: ZCL_CI_004
## 5.	Uso inseguro de FOR ALL ENTRIES
-	Clase Original: CL_CI_TEST_FOR_ALL_ENTRIES
-	Clase destino: ZCL_CI_005
## 6.	Inexistencia de sentencias SELECT * en el programa
-	Clase Original: CL_CI_TEST_FREE_SEARCH
-	Clase destino: ZCL_CI_006
## 7.	Inexistencia de sentencias SELECT dentro de ciclos LOOP/ENDLOOP (Trabajar con tablas internas).
## 8.	Se prohíbe el uso de la cláusula WHERE en el comando LOOP, ya que causa latencias. En vez de LOOP / WHERE se debe usar el recurso combinado de READ TABLE y LOOP it_table FROM, lo que llamamos de LOOP BINÁRIO
-	Clase original: CL_CI_TEST_ITAB_PERFORMANCE
-	Clase destino: ZCL_CI_008
## 9.	Check Pretty Print State
-	Clase original: CL_CI_TEST_PRETTY_PRINT
-	Clase destino: ZCL_CI_009
## 10.	Procedimientos con código no utilizado
