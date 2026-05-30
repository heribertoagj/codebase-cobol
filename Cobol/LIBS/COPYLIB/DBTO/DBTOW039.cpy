      *---------------------------------------------------------------*         
      *                        D B T O W 0 3 9                        *         
      *---------------------------------------------------------------*         
      *    ANALISTA  : CASSIO UYENO                                   *         
      *    DATA      : 07/2009                                        *         
      *    COMENTARIO: AREA DE COMUNICACAO COM O PROGRAMA DBTO9999    *         
      *---------------------------------------------------------------*         
                                                                                
       01  DBTOW039.                                                            
           03  DBTOW039-PROGRAMA            PIC  X(008).                        
           03  DBTOW039-JOBNAME             PIC  X(008).                        
                                                                                
           03  DBTOW039-DADOS-ARQUIVOS      OCCURS  30  TIMES.                  
               05  DBTOW039-I-O             PIC  X(001).                        
      *            I - INPUT                                                    
      *            O - OUTPUT                                                   
               05  DBTOW039-DDNAME          PIC  X(008).                        
               05  DBTOW039-DESCARQ         PIC  X(027).                        
               05  DBTOW039-QTDEARQ         PIC  9(012).                        
                                                                                
           03  DBTOW039-DADOS-TABELAS       OCCURS  30  TIMES.                  
               05  DBTOW039-COMANDO         PIC  X(001).                        
      *            S - SELECT                                                   
      *            I - INSERT                                                   
      *            D - DELETE                                                   
      *            U - UPDATE                                                   
               05  DBTOW039-TABLENAME       PIC  X(008).                        
               05  DBTOW039-DESCTAB         PIC  X(027).                        
               05  DBTOW039-QTDETAB         PIC  9(012).                        
                                                                                
           03  DBTOW039-DADOS-PARM.                                             
               05  DBTOW039-PARM            PIC  X(057).                        
                                                                                
           03  DBTOW039-DADOS-COMENTARIO    OCCURS  10  TIMES.                  
               05  DBTOW039-COMENTARIO      PIC  X(059).                        
                                                                                
           03  DBTOW039-DADOS-RESTART.                                          
               05  DBTOW039-RESTART         PIC  X(048).                        
                                                                                
           03  DBTOW039-DADOS-COMPILACAO.                                       
               05  DBTOW039-DATA-COMP       PIC  X(010).                        
               05  DBTOW039-HORA-COMP       PIC  X(008).                        
                                                                                
