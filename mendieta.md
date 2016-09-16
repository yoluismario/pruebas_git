
No probé MPI, sólo OpenMP.
Un código con cuello de botella en diagonalización (DFTB+), con ejemplos chicos anda razonablemente bien, después la RAM pasa a ser un inconveniente (Pregunta: swapea? luce como si), a veces muere sin mayor explicación. 
Para esto no sirve, este código tiene pedazos OpenMP pero no los suficientes.
Para nuestro código (kronos) que normalmente usamos en GPU, pero para el cual tenemos una buena versión OpenMP logré sacarles con un caso chico/mediano una buena performance. 
Acá el cuello de botella son DGEMMs pero el resto tiene TODO OpenMP. 
Las Xeon Phi son un 25% más lentas que las C2090 en doble precisión. 
Me sorprendió la verdad ya que compiló cero inconveniente y corrió bien.

Código muy CPU bound, no demasiada RAM: fáciles de usar porque el OpenMP es infinitamente más fácil que CUDA. 

