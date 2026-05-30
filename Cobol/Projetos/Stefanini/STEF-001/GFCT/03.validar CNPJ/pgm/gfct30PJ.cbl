      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*
       PROGRAM-ID. GFCT30PJ.
       AUTHOR.     HERIBERTO GIANNASI.
      *===============================================================*
      *                      S T E F A N I N I                        *
      *---------------------------------------------------------------*
      *    PROGRAMA....:   GFCT30PJ                                   *
      *    ANALISTA....:   STEFANINI                                  *
      *    DATA........:   10/06/2025                                 *
      *---------------------------------------------------------------*
      *    OBJETIVO....:   VALIDAR CNPJ UTILIZANDO O PATTERN          * 
      *                     [A-Z][0-9]{12}                            *
      *---------------------------------------------------------------*
      *===============================================================*
           EJECT
      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(32)        VALUE
                   '* INICIO DA WORKING GFCT30PJ *'.
      *---------------------------------------------------------------*
       
       01  IND-1                       PIC 9(03)        VALUE ZERO.
       01  WRK-ITERACAO                  PIC 9(03)      VALUE ZERO.
       01  WRK-CARACTER                PIC X(01)       VALUE SPACES.
       88  WRK-CARACTER-VALIDO                   VALUE 'A' THRU 'Z',
                                                       '0' THRU '9'
                                                       ' '.
      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY GFCTW3PJ.
      *===============================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *===============================================================*
      *---------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZA.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *---------------------------------------------------------------*
           
           IF GFCTW3PJ-E-CGC-CNPJ      EQUAL LOW-VALUES 
           OR GFCTW3PJ-E-CGC-CNPJ      EQUAL SPACES
              SET GFCTW3PJ-S-INVALIDO  TO TRUE
              PERFORM 3000-FINALIZA
           END-IF.     

          

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           SET GFCTW3PJ-S-VALIDO       TO TRUE.
                                       
           PERFORM 2100-VALIDAR-CGC.
           
           IF  GFCTW3PJ-E-FILIAL        NOT EQUAL LOW-VALUES 
           AND GFCTW3PJ-E-FILIAL        NOT EQUAL SPACES
              PERFORM 2200-VALIDAR-FILIAL
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-VALIDAR-CGC               SECTION.                         
      *----------------------------------------------------------------*
       
           MOVE LENGTH OF GFCTW3PJ-E-CGC-CNPJ
                                       TO  WRK-ITERACAO.

           PERFORM                     VARYING IND-1                    
                                       FROM 1                           
                                       BY 1                             
                                       UNTIL IND-1                      
                                       GREATER WRK-ITERACAO                     
              MOVE GFCTW3PJ-E-CGC-CNPJ(IND-1:1)                          
                                       TO WRK-CARACTER                  
              IF WRK-CARACTER-VALIDO                                    
                 CONTINUE                                               
              ELSE                                                      
                 SET GFCTW3PJ-S-INVALIDO 
                                       TO TRUE
                 PERFORM 3000-FINALIZA                   
              END-IF                                                    
           END-PERFORM.                                                 
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-VALIDAR-FILIAL             SECTION.  
      *----------------------------------------------------------------*
     
           MOVE LENGTH OF GFCTW3PJ-E-FILIAL
                                       TO  WRK-ITERACAO.
 
           PERFORM                     VARYING IND-1                    
                                       FROM 1                           
                                       BY 1                             
                                       UNTIL IND-1                      
                                       GREATER WRK-ITERACAO                     
              MOVE GFCTW3PJ-E-FILIAL(IND-1:1)                            
                                       TO WRK-CARACTER                  
              IF WRK-CARACTER-VALIDO                                    
                CONTINUE                                               
             ELSE                                                      
                SET GFCTW3PJ-S-INVALIDO 
                                       TO TRUE
                PERFORM 3000-FINALIZA                   
             END-IF                                                    
           END-PERFORM.
      *----------------------------------------------------------------*
       3000-FINALIZA                  SECTION.
      *----------------------------------------------------------------*

           IF GFCTW3PJ-S-VALIDO
              MOVE '00'                TO GFCTW3PJ-S-COD-RETORNO
           ELSE 
              MOVE '08'                TO GFCTW3PJ-S-COD-RETORNO
           END-IF.
           
           EXEC      CICS RETURN                      
           END-EXEC.                                  

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
