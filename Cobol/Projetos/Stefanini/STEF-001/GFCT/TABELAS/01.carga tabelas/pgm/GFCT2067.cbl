      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT2067.
       AUTHOR.     STEFANINI.
      *================================================================*
      *                S T E F A N I N I                               *
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT2067                                      *
      *    OBJETIVO....: ATUALIZACAO CAMPOS ALFANUMERICOS NA TABELA:   *
      *                  GSTAO_CLI_DESC (GFCTB067)                     *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS   COMMA.
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       01  FILLER                      PIC X(032)      VALUE
           '*  INICIO DA WORKING GFCT2067  *'.

       01  FILLER                      PIC X(032)      VALUE
           '*  AREA VARIAVEIS AUXILIARES   *'.

       77  WRK-BATCH                   PIC  X(008)         VALUE   
           'BATCH'.                                                

       01  WRK-FIM-CURSOR              PIC X VALUE 'N'.
       01  WRK-PGM                     PIC X(08) VALUE 'GFCT2067'.
       01  ACU-LIDOS                   PIC 9(09) COMP-3    VALUE ZEROS. 
       01  ACU-ATUALIZADOS             PIC 9(09) COMP-3    VALUE ZEROS. 

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE        
           '*** AREA PARA BRAD7100                         ***'.        
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.                                                 

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE        
           '*** AREA PARA CKRS                             ***'.        
      *----------------------------------------------------------------*
       01  WRK-CKRS0105                PIC  X(08)          VALUE        
           'CKRS0105'.                                                  

       COPY 'I#CKRS04'.                                                 
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           '*   AREA PARA INCLUDES DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA 
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB067
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           '* AREA PARA CURSORES           *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01 CURSOR WITH HOLD FOR
               SELECT CCGC_CPF,
                      CFLIAL_CGC,
                      CCTRL_CPF_CGC
                 FROM DB2PRD.GSTAO_CLI_DESC
                 FOR UPDATE OF CCGC_CPF_ST,
                                CFLIAL_CGC_ST,
                                CCTRL_CPF_CGC_ST
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           '*    FIM DA WORKING GFCT2067   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR UNTIL WRK-FIM-CURSOR = 'S'.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-CONECTAR-DB2.

           EXEC SQL
               OPEN CSR01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)      
           OR (SQLWARN0                EQUAL 'W')            
              MOVE 'GSTAO_CLI_DESC'      TO ERR-DBD-TAB        
              MOVE 'OPEN'               TO ERR-FUN-COMANDO    
              MOVE '0010'               TO ERR-LOCAL          
              PERFORM 9100-TRATAR-ERRO-DB2                   
           END-IF.                                    

           MOVE 'N'                    TO WRK-FIM-CURSOR.
           MOVE ZEROS                  TO ACU-LIDOS.
           MOVE ZEROS                  TO ACU-ATUALIZADOS.

      *----------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1100-CONECTAR-DB2               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ERRO-AREA.                                  

           CALL 'CKRS1000'.                                       
           INITIALIZE PARM-CKRS0105.                              
           MOVE 'C'                    TO PARM-OP                 
           MOVE 'DB2'                  TO PARM-SSID               
           MOVE WRK-PGM                TO PARM-PLAN               
           CALL WRK-CKRS0105           USING PARM-CKRS0105.       

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01 INTO
                   :GFCTB067.CCGC-CPF,
                   :GFCTB067.CFLIAL-CGC,
                   :GFCTB067.CCTRL-CPF-CGC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)      
           OR (SQLWARN0                EQUAL 'W')            
              MOVE 'GSTAO_CLI_DESC'      TO ERR-DBD-TAB        
              MOVE 'FETCH'              TO ERR-FUN-COMANDO    
              MOVE '0010'              TO ERR-LOCAL          
              PERFORM 9100-TRATAR-ERRO-DB2                   
           END-IF.                                           

           IF SQLCODE                EQUAL +100
              MOVE 'S'               TO WRK-FIM-CURSOR
              GO TO 2000-99-FIM
           ELSE
              ADD 1                  TO ACU-LIDOS
           END-IF.

      *-- MOVENDO DADOS NUMERICOS PARA CAMPOS ALFANUMERICOS
           MOVE CCGC-CPF             OF GFCTB067
                                     TO CCGC-CPF-ST
                                     OF GFCTB067

           MOVE CFLIAL-CGC           OF GFCTB067
                                     TO CFLIAL-CGC-ST
                                     OF GFCTB067

           MOVE CCTRL-CPF-CGC        OF GFCTB067
                                     TO CCTRL-CPF-CGC-ST
                                     OF GFCTB067.                               

      *-- ATUALIZANDO O REGISTRO
           EXEC SQL
               UPDATE DB2PRD.GSTAO_CLI_DESC
                  SET CCGC_CPF_ST       = :GFCTB067.CCGC-CPF-ST,
                      CFLIAL_CGC_ST     = :GFCTB067.CFLIAL-CGC-ST,
                      CCTRL_CPF_CGC_ST  = :GFCTB067.CCTRL-CPF-CGC-ST
                WHERE CURRENT OF CSR01
           END-EXEC.

           IF SQLCODE                  NOT EQUAL ZEROS 
              DISPLAY '* ERRO NO UPDATE: ' SQLCODE
              MOVE 'S'                 TO WRK-FIM-CURSOR
              GO TO 2000-99-FIM
           END-IF.

           ADD 1                       TO ACU-ATUALIZADOS.
           
           PERFORM 2100-ATUALIZAR-CKRS.

      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-ATUALIZAR-CKRS              SECTION.
      *----------------------------------------------------------------*
           MOVE 'P'                   TO PARM-OP.             
           CALL WRK-CKRS0105          USING PARM-CKRS0105.    

      *----------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)      
           OR (SQLWARN0                EQUAL 'W')            
              MOVE 'GSTAO_CLI_DESC'      TO ERR-DBD-TAB        
              MOVE 'CLOSE'              TO ERR-FUN-COMANDO    
              MOVE '0030'              TO ERR-LOCAL          
              PERFORM 9100-TRATAR-ERRO-DB2                   
           END-IF.                                    

           MOVE 'D'                   TO PARM-OP.             
           CALL WRK-CKRS0105          USING PARM-CKRS0105.    


           DISPLAY '* FIM DO PROGRAMA GFCT2067'.
           DISPLAY '*'
           DISPLAY '* TOTAL DE REGISTROS LIDOS  : ' ACU-LIDOS
           DISPLAY '* TOTAL DE REGISTROS ATUALIZADOS: ' ACU-ATUALIZADOS.

           MOVE ZEROS                 TO RETURN-CODE.
           STOP RUN.
           
      *----------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9100-TRATAR-ERRO-DB2            SECTION.                         
      *----------------------------------------------------------------*
           MOVE 'DB2'                  TO ERR-TIPO-ACESSO               
           MOVE SQLCODE                TO ERR-SQL-CODE                  

           PERFORM 9999-TRATAR-ERRO.                                    

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.                             
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-TRATAR-ERRO                SECTION.                         
      *----------------------------------------------------------------*
           MOVE WRK-PGM                TO ERR-PGM.                      

           IF ERR-TIPO-ACESSO          EQUAL 'DB2'                      
              CALL 'BRAD7100'          USING WRK-BATCH                  
                                             ERRO-AREA                  
                                             SQLCA                      
           ELSE                                                         
              CALL 'BRAD7100'          USING WRK-BATCH                  
                                             ERRO-AREA                  
           END-IF.                                                       
           
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                             
      *----------------------------------------------------------------*
