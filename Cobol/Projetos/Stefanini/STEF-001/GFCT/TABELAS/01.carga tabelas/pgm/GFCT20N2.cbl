      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT20N2.
       AUTHOR.     STEFANINI.
      *================================================================*
      *                S T E F A N I N I                               *
      *----------------------------------------------------------------*
      *    PROGRAMA....: GFCT20N2                                      *
      *    OBJETIVO....: ATUALIZACAO CNPJ ALFANUMERICO NA TABELA:      *
      *                  TCTRL_NGTVC_CLI (GFCTB0N2)                    *
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
           '*  INICIO DA WORKING GFCT20N2  *'.

       01  FILLER                      PIC X(032)      VALUE
           '*  AREA VARIAVEIS AUXILIARES   *'.

       77  WRK-BATCH                   PIC  X(008)         VALUE   
           'BATCH'.                                                

       01  WRK-FIM-CURSOR              PIC X VALUE 'N'.
       01  WRK-PGM                     PIC X(08) VALUE 'GFCT20N2'.
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
               INCLUDE GFCTB0N2
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           '* AREA PARA CURSORES           *'.
      *----------------------------------------------------------------*

           EXEC SQL
               DECLARE CSR01 CURSOR WITH HOLD FOR
               SELECT CCNPJ_CPF_DEB,
                      CFLIAL_CNPJ_DEB,
                      CCTRL_CNPJ_CPF_DEB
                 FROM DB2PRD.TCTRL_NGTVC_CLI
                 FOR UPDATE OF CCNPJ_CPF_DEB_ST,
                                CFLIAL_CNPJ_DEB_ST,
                                 CCTRL_CNPJ_CPF_DEB_ST
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)      VALUE
           '*    FIM DA WORKING GFCT20N2   *'.
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
              MOVE 'TCTRL_NGTVC_CLI'   TO ERR-DBD-TAB        
              MOVE 'OPEN'              TO ERR-FUN-COMANDO    
              MOVE '0010'              TO ERR-LOCAL          
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
                   :GFCTB0N2.CCNPJ-CPF-DEB,
                   :GFCTB0N2.CFLIAL-CNPJ-DEB,
                   :GFCTB0N2.CCTRL-CNPJ-CPF-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)      
           OR (SQLWARN0                EQUAL 'W')            
              MOVE 'TCTRL_NGTVC_CLI'   TO ERR-DBD-TAB        
              MOVE 'FETCH'             TO ERR-FUN-COMANDO    
              MOVE '0010'              TO ERR-LOCAL          
              PERFORM 9100-TRATAR-ERRO-DB2                   
           END-IF.                                           

           IF SQLCODE                EQUAL +100
              MOVE 'S'               TO WRK-FIM-CURSOR
              GO TO 2000-99-FIM
           ELSE
              ADD 1                   TO ACU-LIDOS
           END-IF.

      *-- MOVENDO DADOS NUMERICOS PARA CAMPOS ALFANUMERICOS
           MOVE CCNPJ-CPF-DEB         OF GFCTB0N2
                                      TO CCNPJ-CPF-DEB-ST
                                      OF GFCTB0N2

           MOVE CFLIAL-CNPJ-DEB       OF GFCTB0N2
                                      TO CFLIAL-CNPJ-DEB-ST
                                      OF GFCTB0N2

           MOVE CCTRL-CNPJ-CPF-DEB    OF GFCTB0N2
                                      TO CCTRL-CNPJ-CPF-DEB-ST
                                      OF GFCTB0N2.

      *-- ATUALIZANDO O REGISTRO
           EXEC SQL
               UPDATE DB2PRD.TCTRL_NGTVC_CLI
                  SET CCNPJ_CPF_DEB_ST = :GFCTB0N2.CCNPJ-CPF-DEB-ST,
                      CFLIAL_CNPJ_DEB_ST = 
                                      :GFCTB0N2.CFLIAL-CNPJ-DEB-ST,
                      CCTRL_CNPJ_CPF_DEB_ST = 
                                      :GFCTB0N2.CCTRL-CNPJ-CPF-DEB-ST
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
              MOVE 'TCTRL_NGTVC_CLI'   TO ERR-DBD-TAB        
              MOVE 'CLOSE'             TO ERR-FUN-COMANDO    
              MOVE '0030'              TO ERR-LOCAL          
              PERFORM 9100-TRATAR-ERRO-DB2                   
           END-IF.                                    

           MOVE 'D'                   TO PARM-OP.             
           CALL WRK-CKRS0105          USING PARM-CKRS0105.    

           DISPLAY '* FIM DO PROGRAMA GFCT20N2'.
           DISPLAY '*'
           DISPLAY '* TOTAL DE REGISTROS LIDOS  : ' ACU-LIDOS
           DISPLAY '* TOTAL DE REGISTROS ATUALIZADOS: ' ACU-ATUALIZADOS.

           MOVE ZEROS                  TO RETURN-CODE
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
