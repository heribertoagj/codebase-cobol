      *================================================================*
       IDENTIFICATION                  DIVISION.                        
      *================================================================*
       PROGRAM-ID. DBTO204C.                                            
       AUTHOR.     TIAGO MACEDO.                                        
      *================================================================*
      *                        P R I M E I T                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DBTO204C                                      *
      *    PROGRAMADOR.: TIAGO MACEDO - PRIMEIT                        *
      *    DATA........: 13/09/2022                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: MONTAR LAYOUT CADOC 8013                      *
      *                  TCTRL_ARQ_DVDOR_TARIF - DBTOB0DD              *
      *                  TDETLH_ARQ_DVDOR_TARIF - DBTOB00E             *
      *                                                                *
      *                                                                *
      *      ARQUIVOS:                                                 *
      *         DDNAME                           INCLUDE/BOOK          *
      *         ARQENTRA                           DBTOW24C            *
      *         ARQSAIDA                           DBTOWRE3            *
      *                                                                *
      *    BOOKS........:                                              *
      *    I#FRWKDB - COMMAREA FRWK2999 (LOG DE ERROS DB2)             *
      *    I#FRWKGE - AREA DE COMUNICACAO FRWK2999                     *
      *                                                                *
      *      MODULOS CHAMADOS:                                         *
      *         BRAD7100 - TRATAMENTO DE ERRO                          *
      *         BRAD7600 - OBTER DATA E HORA                           *
      *                                                                *
      *================================================================*
                                                                        
      *================================================================*
       ENVIRONMENT                     DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.                         
      *----------------------------------------------------------------*
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT  ARQENTRA   ASSIGN   TO UT-S-ARQENTRA                 
                       FILE   STATUS   IS WRK-FS-ARQENTRA.              
                                                                        
           SELECT  ARQSAIDA   ASSIGN   TO UT-S-ARQSAIDA                 
                       FILE   STATUS   IS WRK-FS-ARQSAIDA.              
                                                                        
      *================================================================*
       DATA                            DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       FILE                            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  INPUT:       ORG. SEQUENCIAL   -   LRECL = 300                *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQENTRA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQENTRA                 PIC X(0300).                     
      *----------------------------------------------------------------*
      *  OUTPUT:      ORG. SEQUENCIAL   -   LRECL = 097                *
      *----------------------------------------------------------------*
                                                                        
       FD  ARQSAIDA                                                     
           RECORDING MODE IS F                                          
           LABEL RECORD IS STANDARD                                     
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  FD-ARQSAIDA                 PIC X(0097).                     
                                                                        
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.                         
      *----------------------------------------------------------------*
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '* DBTO204C - INICIO DA WORKING *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*         ACUMULADORES         *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-ACUMULADORES.                                            
           03  ACU-LIDOS-ARQENTRA      PIC  9(10) COMP-3 VALUE ZEROS.   
           03  ACU-GRAVA-ARQSAIDA      PIC  9(10) COMP-3 VALUE ZEROS.   
           03  WRK-IND-1               PIC  9(10) COMP-3 VALUE ZEROS.   
           03  WRK-LIDOS               PIC  9(10) COMP-3 VALUE ZEROS.   
                                                                        
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE            
           'AREA PARA VARIAVEIS DE NULIDADE'.                           
      *----------------------------------------------------------------*
       01  WRK-AREA-NULOS.                                              
           05 WRK-CSERVC-FEBRABAN-N    PIC S9(04) COMP VALUE ZEROS.     
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*     VARIAVEIS AUXILIARES     *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-AUXILIARES.                                              
           03  WRK-DISP                PIC  ZZZZ.ZZZ.ZZ9 VALUE SPACES.  
           03  WRK-MASCARA-SQLCODE     PIC  ZZZ.ZZZ.ZZ9-   VALUE ZEROS. 
           03  WRK-FRWK2999            PIC  X(08) VALUE 'FRWK2999'.     
           03  WRK-ABEND-BAT           PIC S9(004)  COMP   VALUE +1111. 
           03  WRK-DUMP-BAT            PIC  X(001)         VALUE 'S'.   
           03  WRK-CSR1-FIM            PIC  X(001)         VALUE 'N'.   
           03  WRK-CKRS0105            PIC X(008) VALUE   'CKRS0105'.   
                                                                        
                                                                        
       01  WRK-ABEND                   PIC  X(001)         VALUE SPACES.
           88  WRK-ABENDAR                                 VALUE 'S'.   
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  AREAS P/ TESTE FILE STATUS  *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       77  WRK-FS-ARQENTRA             PIC  X(02) VALUE SPACES.         
       77  WRK-FS-ARQSAIDA             PIC  X(02) VALUE SPACES.         
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.  
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.   
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO  '.
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*      AREA DE MENSAGENS       *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
       01  WRK-FS-ERRO.                                                 
           03  FILLER                  PIC  X(09) VALUE SPACES.         
           03  FILLER                  PIC  X(05) VALUE 'ERRO'.         
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.         
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '. 
           03  WRK-ARQUIVO             PIC  X(08) VALUE SPACES.         
           03  FILLER                  PIC  X(17) VALUE                 
               ' - FILE STATUS = '.                                     
           03  WRK-FILE-STATUS         PIC  X(02) VALUE SPACES.         
                                                                        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*      AREA DE REDEFINES       *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01 WRK-S13V99                    PIC +9(13)V99   VALUE ZEROS.    
       01 FILLER REDEFINES WRK-S13V99.                                  
          05 FILLER                    PIC X(01).                       
          05 WRK-13V99                 PIC  9(13)V99.                   
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*       AREA DA BRAD7100       *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
           COPY 'I#BRAD7C'.                                       
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA LAYOUT DO ARQUIVO DBTOW24C *'.                  
      *----------------------------------------------------------------*
       01 WRK-DBTOW24C.                                                 
          COPY DBTOW24C.                                                
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA LAYOUT DO ARQUIVO DBTOWRE3 *'.                  
      *----------------------------------------------------------------*
          COPY DBTOWRE3.                                                
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE            
           'AREA DE TRATAMENTO DE ERROS DE DB2'.                        
      *----------------------------------------------------------------*
       01  WRK-FRWKGDB2.                                                
           COPY 'I#FRWKDB'.                                             
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           'AREA DE COMUNICACAO DO PROGRAMA FRWK2999'.                  
      *----------------------------------------------------------------*
                                                                        
       01  WRK-AREA-ERRO.                                               
           COPY 'I#FRWKGE'.                                             
           05  WRK-BLOCO-INFO-ERRO     VALUE SPACES.                    
             10  WRK-CHAR-INFO-ERRO    PIC X(001)                       
                                       OCCURS 0 TO 30000 TIMES          
                                       DEPENDING ON FRWKGHEA-TAM-DADOS. 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE        
           '* AREA PARA CHECKPOINT/RESTART *'.                          
      *----------------------------------------------------------------*
                                                                        
       COPY 'I#CKRS04'.                                                 
                                                                        
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)  VALUE                
           'AREA PARA DB2'.                                             
      *----------------------------------------------------------------*
           EXEC SQL                                                     
               INCLUDE SQLCA                                            
           END-EXEC.                                                    
                                                                        
      *    TABELA TCTRL_ARQ_DVDOR_TARIF                                0
           EXEC SQL                                                     
               INCLUDE DBTOB0DD                                         
           END-EXEC.                                                    
      *                                                                 
      *    TABELA TDETLH_ARQ_DVDOR_TARIF                                
           EXEC SQL                                                     
               INCLUDE DBTOB0DE                                         
           END-EXEC.                                                    
      *                                                                 
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE            
           '* AREA PARA CURSORES *'.                                    
      *----------------------------------------------------------------*
      *                                                                 
      *--> CURSOR DA TABELA TDETLH_ARQ_DVDOR_TARIF                      
      *                                                                 
           EXEC SQL                                                     
            DECLARE CSR01-DBTOB0DE CURSOR WITH HOLD FOR                 
            SELECT  CCTA_COSIF                                          
                      , CCNPJ_PGDOR_FNAL                                
                      , CFLIAL_PGDOR_FNAL                               
                      , CCTRL_PGDOR_FNAL                                
                      , VSDO_TARIF_ANTER                                
                      , VAMOTZ_PG_DVDOR                                 
                      , VLIBRC_TARIF_RECBR                              
                      , VJURO_INC_RECBR                                 
                      , VAJUST_TARIF_RECBR                              
                      , VSDO_TARIF_ATUAL                                
                      , RCOMPL_TARIF_DVDOR                              
                      , CSERVC_FEBRABAN                                 
                  FROM DB2PRD.TDETLH_ARQ_DVDOR_TARIF                    
            WHERE NCTRL_ARQ_DVDOR    = :DBTOB0DE.NCTRL-ARQ-DVDOR        
           END-EXEC.                                                    
      *                                                                 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER PIC X(32) VALUE  '*  DBTO204C - FIM DA WORKING   *'.  
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
                                                                        
      *================================================================*
       PROCEDURE                       DIVISION.                        
      *================================================================*
                                                                        
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1000-INICIALIZAR.                                    
                                                                        
           PERFORM 2000-VERIFICAR-VAZIO.                                
                                                                        
           PERFORM 3200-VALIDAR-ENTRADA.                                
                                                                        
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ARQENTRA EQUAL '10'.     
                                                                        
           PERFORM 6360-CKRS-CHECK.                                     
                                                                        
           PERFORM 5000-FINALIZAR.                                      

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.                         
      *----------------------------------------------------------------*
           CALL 'CKRS1000'.                                             
                                                                        
           MOVE 'C'                    TO PARM-OP.                      
           MOVE 'DB2'                  TO PARM-SSID.                    
           MOVE SPACES                 TO PARM-PLAN.                    
                                                                        
           CALL WRK-CKRS0105           USING PARM-CKRS0105.             
                                                                        
           MOVE '1000-INICIALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.      
           MOVE WRK-ABERTURA           TO  WRK-OPERACAO.                
                                                                        
           OPEN    INPUT   ARQENTRA                                     
                   OUTPUT  ARQSAIDA.                                    
                                                                        
           MOVE    WRK-ABERTURA        TO      WRK-OPERACAO.            
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 1110-TESTAR-FS-ARQENTRA.                             
                                                                        
           PERFORM 1120-TESTAR-FS-ARQSAIDA.                             
                                                                        
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQENTRA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQENTRA         NOT EQUAL '00'                   
               MOVE 'ARQENTRA'         TO WRK-ARQUIVO                   
               MOVE WRK-FS-ARQENTRA    TO WRK-FILE-STATUS               
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE WRK-FS-ERRO        TO ERR-TEXTO                     
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQSAIDA         SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           IF  WRK-FS-ARQSAIDA         NOT EQUAL '00'                   
               MOVE 'ARQSAIDA'         TO WRK-ARQUIVO                   
               MOVE WRK-FS-ARQSAIDA    TO WRK-FILE-STATUS               
               MOVE 'APL'              TO ERR-TIPO-ACESSO               
               MOVE WRK-FS-ERRO        TO ERR-TEXTO                     
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9999-ROTINA-ERRO                                 
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 2100-LER-ARQENTRA.                                   
                                                                        
           IF  ACU-LIDOS-ARQENTRA EQUAL ZEROS                           
               DISPLAY '*********** DBTO204C ***********'               
               DISPLAY '*                              *'               
               DISPLAY '*    ARQUIVO ARQENTRA VAZIO    *'               
               DISPLAY '*                              *'               
               DISPLAY '*********** DBTO204C ***********'               
               DISPLAY ' '                                              
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       2100-LER-ARQENTRA               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           READ ARQENTRA               INTO WRK-DBTOW24C.               
                                                                        
           IF  WRK-FS-ARQENTRA         EQUAL '10'                       
               GO                      TO 2100-99-FIM                   
           END-IF.                                                      
                                                                        
           MOVE    WRK-LEITURA         TO WRK-OPERACAO.                 
           PERFORM 1110-TESTAR-FS-ARQENTRA.                             
           ADD     1                   TO ACU-LIDOS-ARQENTRA.           
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.                         
      *----------------------------------------------------------------*
           PERFORM 6000-OPEN-CSR01                                      

           PERFORM 6100-FETCH-CSR01 
           
           PERFORM                     VARYING WRK-IND-1 
                                       FROM 1
                                       BY 1 
                                       UNTIL WRK-CSR1-FIM                  
                                       EQUAL 'S'                       
              PERFORM 3100-MONTAR-ARQSAIDA                           

              PERFORM 4000-GRAVAR-ARQSAIDA                           
              PERFORM 6100-FETCH-CSR01 
           END-PERFORM.                                                 
           
           PERFORM 6200-CLOSE-CSR1   

           IF ACU-GRAVA-ARQSAIDA       GREATER ZEROS                    
              PERFORM 6300-UPTATE-STATUS                                
           ELSE                                                         
              DISPLAY '***************** DBTO204C ******************'
              DISPLAY '*'
              DISPLAY '*   NENHUM ENTE FOI ENVIADO'
              DISPLAY '*'
              DISPLAY '***************** DBTO204C ******************'
              PERFORM 5000-FINALIZAR                                      
           END-IF.                                                      
                                                                        
           PERFORM 2100-LER-ARQENTRA.                                    

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-MONTAR-ARQSAIDA            SECTION.                         
      *----------------------------------------------------------------*
           MOVE CSERVC-FEBRABAN        TO WRE3-CSERVC-CTRTE             
                                                                        
           MOVE VSDO-TARIF-ANTER       TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VSALDO-ANT               
                                                                        
           MOVE VAMOTZ-PG-DVDOR        TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VAMORT                   
                                                                        
           MOVE VLIBRC-TARIF-RECBR     TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VLIBER                   
                                                                        
           MOVE VJURO-INC-RECBR        TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VJUROS                   
                                                                        
           MOVE VAJUST-TARIF-RECBR     TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VAJUSTE                  
                                                                        
           MOVE VSDO-TARIF-ATUAL       TO WRK-S13V99                    
           MOVE WRK-13V99              TO WRE3-VSALDO-ATUAL             
                                                                        
      * 1 - TRANSMISSAO EFETIVA                                         
      * 2 - TRANSMISSAO PREVIA                                          
      * 3 - TRANSMISSAO RETRANSMITIR                                    
           EVALUATE DBTOW24C-TP-ACIONAMENTO                             
              WHEN '1'                                                   
                 MOVE 2                TO WRE3-INDCD-GERAC              
                 MOVE 'T'              TO WRE3-INDCD-TRANSM             
              WHEN '2'                                                   
                 MOVE 1                TO WRE3-INDCD-GERAC              
                 MOVE 'T'              TO WRE3-INDCD-TRANSM             
              WHEN '3'                                                   
                 MOVE 2                TO WRE3-INDCD-GERAC              
                 MOVE 'R'              TO WRE3-INDCD-TRANSM             
           END-EVALUATE.                                                
                                                                        
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       3200-VALIDAR-ENTRADA            SECTION.                         
      *----------------------------------------------------------------*
           IF DBTOW24C-CHAVE-MAX       NOT NUMERIC                      
              DISPLAY '***************** DBTO204C ******************'
              DISPLAY '*'
              DISPLAY '*   CHAVE NAO INFORMADA'
              DISPLAY '*'
              DISPLAY '***************** DBTO204C ******************'
              PERFORM 5000-FINALIZAR                                      
           END-IF.                                                      
           
           IF DBTOW24C-TP-ACIONAMENTO  NOT EQUAL '1' AND                
              DBTOW24C-TP-ACIONAMENTO  NOT EQUAL '2' AND                
              DBTOW24C-TP-ACIONAMENTO  NOT EQUAL '3'                    
              DISPLAY '***************** DBTO204C ******************'
              DISPLAY '*'
              DISPLAY '*   TIPO DE ACIONAMENTO INVALIDO'
              DISPLAY '*'
              DISPLAY '***************** DBTO204C ******************'
              PERFORM 5000-FINALIZAR                                      
           END-IF.                                                      
                                                                        
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       4000-GRAVAR-ARQSAIDA            SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           MOVE    WRK-GRAVACAO        TO      WRK-OPERACAO.            
                                                                        
           WRITE   FD-ARQSAIDA         FROM    DBTOWRE3-REG.            
                                                                        
           PERFORM 1120-TESTAR-FS-ARQSAIDA.                             
                                                                        
           ADD     1                   TO      ACU-GRAVA-ARQSAIDA.      
                                                                        
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
       5000-FINALIZAR                  SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 5100-EMITIR-DISPLAY.                                 
                                                                        
           CLOSE   ARQENTRA                                             
                   ARQSAIDA.                                            
                                                                        
           MOVE    WRK-FECHAMENTO      TO      WRK-OPERACAO.            
                                                                        
           PERFORM 1100-TESTAR-FILE-STATUS.                             
                                                                        
           PERFORM 9998-STOP-RUN.
                                                                        
      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*        
      *----------------------------------------------------------------*
       5100-EMITIR-DISPLAY             SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '****************** DBTO204C ******************'     
           DISPLAY '*                                            *'     
                                                                        
           MOVE ACU-LIDOS-ARQENTRA     TO WRK-DISP                      
           DISPLAY '* REGISTROS LIDOS ARQENTRA    - ' WRK-DISP ' *'     
                                                                        
           MOVE ACU-GRAVA-ARQSAIDA     TO WRK-DISP                      
           DISPLAY '* REGISTROS GRAVADOS ARQSAIDA - ' WRK-DISP ' *'     
                                                                        
           DISPLAY '*                                            *'     
           DISPLAY '****************** DBTO204C ******************'.    
                                                                        
      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *  ROTINA PARA ABRIR O CURSOR CSR01-RFINB0FB                      
      *----------------------------------------------------------------*
       6000-OPEN-CSR01                 SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           INITIALIZE                  DBTOB0DE                         
      *                                                                 
           MOVE 'N'                    TO WRK-CSR1-FIM                  
           MOVE DBTOW24C-CHAVE-MAX     TO NCTRL-ARQ-DVDOR               
                                          OF DBTOB0DE                   
      *                                                                 
           EXEC SQL                                                     
                OPEN CSR01-DBTOB0DE                                     
           END-EXEC                                                     
      *                                                                 
           IF (SQLCODE                 NOT EQUAL ZEROS)                 
           OR (SQLWARN0                EQUAL 'W')                       
               SET DB2-OPEN            TO TRUE                          
               MOVE '0010'             TO FRWKGDB2-LOCAL                
               MOVE 'TDETLH_ARQ_DVDOR_TARIF'                            
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '6000-OPEN-CSR01'  TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9993-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *  ROTINA PARA LER O CURSOR CSR01-DBTOB0DE                        
      *----------------------------------------------------------------*
       6100-FETCH-CSR01                SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           EXEC SQL                                                     
               FETCH CSR01-DBTOB0DE                                     
                INTO  :DBTOB0DE.CCTA-COSIF                              
                     ,:DBTOB0DE.CCNPJ-PGDOR-FNAL                        
                     ,:DBTOB0DE.CFLIAL-PGDOR-FNAL                       
                     ,:DBTOB0DE.CCTRL-PGDOR-FNAL                        
                     ,:DBTOB0DE.VSDO-TARIF-ANTER                        
                     ,:DBTOB0DE.VAMOTZ-PG-DVDOR                         
                     ,:DBTOB0DE.VLIBRC-TARIF-RECBR                      
                     ,:DBTOB0DE.VJURO-INC-RECBR                         
                     ,:DBTOB0DE.VAJUST-TARIF-RECBR                      
                     ,:DBTOB0DE.VSDO-TARIF-ATUAL                        
                     ,:DBTOB0DE.RCOMPL-TARIF-DVDOR                      
                     ,:DBTOB0DE.CSERVC-FEBRABAN                         
                            :WRK-CSERVC-FEBRABAN-N                      
           END-EXEC                                                     
      *                                                                 
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)        
           OR (SQLWARN0                EQUAL 'W')                       
               SET DB2-FETCH           TO TRUE                          
               MOVE '0020'             TO FRWKGDB2-LOCAL                
               MOVE 'TDETLH_ARQ_DVDOR_TARIF'                            
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '6100-FETCH-CSR01' TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9993-ERRO-DB2                                    
           END-IF                                                       
      *                                                                 
           IF WRK-CSERVC-FEBRABAN-N    LESS ZEROS                       
              MOVE ZEROS               TO CSERVC-FEBRABAN               
           END-IF                                                       
      *                                                                 
           IF (SQLCODE                 EQUAL +100)                      
              MOVE 'S'                 TO WRK-CSR1-FIM                  
           ELSE                                                         
              ADD 1                    TO WRK-LIDOS                     
           END-IF                                                       
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *  ROTINA PARA FECHAR O CURSOR CSR01-DBTOB0DE                     
      *----------------------------------------------------------------*
       6200-CLOSE-CSR1                 SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           EXEC SQL                                                     
               CLOSE CSR01-DBTOB0DE                                     
           END-EXEC                                                     
      *                                                                 
           IF (SQLCODE                 NOT EQUAL ZEROS)                 
           OR (SQLWARN0                EQUAL 'W')                       
               SET DB2-CLOSE           TO TRUE                          
               MOVE '0030'             TO FRWKGDB2-LOCAL                
               MOVE 'TDETLH_ARQ_DVDOR_TARIF'                            
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE '6200-CLOSE-CSR1'  TO FRWKGHEA-IDEN-PARAGRAFO       
               PERFORM 9993-ERRO-DB2                                    
           END-IF                                                       
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       6200-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *  ROTINA PARA ATUALIZAR STATUS DO ARQUIVO                        
      *----------------------------------------------------------------*
       6300-UPTATE-STATUS              SECTION.                         
      *----------------------------------------------------------------*
      
           MOVE DBTOW24C-CHAVE-MAX     TO NCTRL-ARQ-DVDOR               
                                       OF DBTOB0DD                   
           
      * TP-ACIONAMENTO                                                  
      * 1 - TRANSMISSAO EFETIVA                                         
      * 2 - TRANSMISSAO PREVIA                                          
      * 3 - TRANSMISSAO RETRANSMITIR                                    
      * CSIT-PROCM-ARQ                                                  
      * 2 - TRANS PREVIA                                                
      * 3 - TRANS EFETIVA                                               
           EVALUATE DBTOW24C-TP-ACIONAMENTO                             
             WHEN '1'                                                   
                MOVE 3                TO CSIT-PROCM-ARQ                
             WHEN '2'                                                   
                MOVE 2                TO CSIT-PROCM-ARQ                
           END-EVALUATE.                                                
      *                                                                 
           EXEC SQL                                                     
            UPDATE DB2PRD.TCTRL_ARQ_DVDOR_TARIF SET                     
              CSIT_PROCM_ARQ     = :DBTOB0DD.CSIT-PROCM-ARQ             
            WHERE NCTRL_ARQ_DVDOR    = :DBTOB0DD.NCTRL-ARQ-DVDOR        
           END-EXEC.                                                    
      *                                                                 
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)        
           OR (SQLWARN0                EQUAL 'W')                       
               SET DB2-SELECT          TO TRUE                          
               MOVE '0010'             TO FRWKGDB2-LOCAL                
               MOVE 'DB2PRD.TCTRL_ARQ_DVDOR_TARIF'                      
                                       TO FRWKGDB2-NOME-TABELA          
               MOVE 'UPDATE CSIT_PROCM_ARQ'                             
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
               MOVE SQLWARN0           TO FRWKGDB2-SQLWARN0             
               SET WRK-ABENDAR         TO TRUE                          
               PERFORM 9993-ERRO-DB2                                    
           END-IF.                                                      
      *                                                                 
           PERFORM 6350-CKRS-CHECK.                                     
      *                                                                 
      *----------------------------------------------------------------*
       6300-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *  ROTINA PARA CHAMAR MODULO PARA EFETIVAR ATUALIZACAO DB2       *
      *----------------------------------------------------------------*
       6350-CKRS-CHECK                 SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE '6350-CKRS-CHECK'                                       
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
           INITIALIZE PARM-CKRS0105                                     
      *                                                                 
           MOVE 'P'                    TO PARM-OP                       
      *                                                                 
           MOVE 'DB2 '                 TO PARM-SSID                     
           MOVE 'DBOT204C'             TO PARM-PLAN                     
      *                                                                 
           CALL WRK-CKRS0105           USING PARM-CKRS0105              
      *                                                                 
           IF (RETURN-CODE             NOT EQUAL ZEROS)                 
               PERFORM 5000-FINALIZAR                                   
           END-IF                                                       
      *                                                                 
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       6350-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *  ROTINA PARA CHAMAR MODULO PARA DESCONECTAR DB2                *
      *----------------------------------------------------------------*
       6360-CKRS-CHECK                 SECTION.                         
      *----------------------------------------------------------------*
      *                                                                 
           MOVE '6360-CKRS-CHECK'                                       
                                       TO FRWKGHEA-IDEN-PARAGRAFO       
           INITIALIZE PARM-CKRS0105                                     
      *                                                                 
           MOVE 'D'                    TO PARM-OP                       
      *                                                                 
           MOVE 'DB2 '                 TO PARM-SSID                     
           MOVE 'DBOT204C'             TO PARM-PLAN                     
      *                                                                 
           CALL WRK-CKRS0105           USING PARM-CKRS0105              
      *                                                                 
           IF (RETURN-CODE             NOT EQUAL ZEROS)                 
               PERFORM 5000-FINALIZAR                                   
           END-IF                                                       
      *                                                                 
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       6360-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
       9993-ERRO-DB2                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           SET ERRO-DB2                TO TRUE.                         
                                                                        
           MOVE 'DBTO204C'             TO FRWKGHEA-NOME-PROGRAMA.       
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.           
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.            
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.               
           MOVE WRK-FRWKGDB2           TO WRK-BLOCO-INFO-ERRO           
                                         (1:FRWKGHEA-TAM-DADOS).        
           MOVE FRWKGDB2-SQLCODE       TO WRK-MASCARA-SQLCODE.          
                                                                        
           DISPLAY ' '                                                  
           DISPLAY 'FRWKGDB2-NOME-TABELA   = ' FRWKGDB2-NOME-TABELA.    
           DISPLAY 'FRWKGDB2-STORED-PROC   = ' FRWKGDB2-STORED-PROC.    
           DISPLAY 'FRWKGDB2-LOCAL         = ' FRWKGDB2-LOCAL.          
           DISPLAY 'FRWKGDB2-SQLCODE       = ' WRK-MASCARA-SQLCODE.     
           DISPLAY 'FRWKGDB2-SQLSTATE      = ' FRWKGDB2-SQLSTATE.       
           DISPLAY 'FRWKGDB2-SQLCA         = ' FRWKGDB2-SQLCA.          
                                                                        
           PERFORM 9999-ROTINA-ERRO.                                    
                                                                        
      *----------------------------------------------------------------*
       9993-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      ******************************************************************
      * ROTINA DE TRATAMENTO DE ERROS.                                  
      ***************************************************************** 
      *----------------------------------------------------------------*
       9994-CHAMAR-FRWK2999            SECTION.                         
      *----------------------------------------------------------------*
           MOVE 'DBTO204C'             TO FRWKGHEA-NOME-PROGRAMA.       
           DISPLAY '************************* DBTO204C'                 
                   ' *************************'.                        
           DISPLAY '* FRWK2999 CHAMADO. SEGUEM SEUS PARAMETROS          
      -            '       *'.                                          
           DISPLAY '*                                                   
      -            '       *'.                                          
           DISPLAY '* FRWKGHEA-TIPO-ERRO      = ' FRWKGHEA-TIPO-ERRO    
                   '                             *'.                    
           DISPLAY '* FRWKGHEA-NOME-PROGRAMA  = ' FRWKGHEA-NOME-PROGRAMA
                   '                       *'.                          
           DISPLAY '* FRWKGHEA-IDEN-PARAGRAFO = '                       
                   FRWKGHEA-IDEN-PARAGRAFO                              
                   ' *'.                                                
           DISPLAY '* FRWKGHEA-TAM-DADOS      = ' FRWKGHEA-TAM-DADOS    
                   '                          *'.                       
           DISPLAY '************************* DBTO204C'                 
                   ' *************************'.                        
           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.             
           DISPLAY '************************* DBTO204C'                 
                   ' *************************'.                        
           DISPLAY '* PARAMETROS RETORNADOS PELO FRWK2999               
      -            '       *'.                                          
           DISPLAY '*                                                   
      -            '       *'.                                          
           DISPLAY '* FRWKGERR-COD-RETORNO    = ' FRWKGERR-COD-RETORNO  
                   '                             *'.                    
           DISPLAY '* FRWKGERR-COD-ERRO       = ' FRWKGERR-COD-ERRO     
                   '                           *'.                      
           DISPLAY '* FRWKGERR-COD-MENSAGEM   = ' FRWKGERR-COD-MENSAGEM 
                   '                       *'.                          
           DISPLAY '************************* DBTO204C'                 
                   ' *************************'.                        
                                                                        
      *----------------------------------------------------------------*
       9994-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA ABENDAR O SISTEMA.                                  
      *----------------------------------------------------------------*
       9995-ROTINA-ABEND               SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.  
           DISPLAY ' '.                                                 
                                                                        
           CALL 'BRAD0450'             USING WRK-ABEND-BAT              
                                             WRK-DUMP-BAT.              
                                                                        
      *----------------------------------------------------------------*
       9995-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA FINALIZACAO DO PROGRAMA                             
      *----------------------------------------------------------------*
       9998-STOP-RUN                   SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           STOP RUN.                                                    
                                                                        
      *----------------------------------------------------------------*
       9998-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO.                                 
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.                         
      *----------------------------------------------------------------*
                                                                        
           PERFORM 9994-CHAMAR-FRWK2999.                                
                                                                        
           IF WRK-ABENDAR                                               
              PERFORM 9995-ROTINA-ABEND                                 
           END-IF.                                                      
                                                                        
           PERFORM 9998-STOP-RUN.                                       
                                                                        
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*
                                                                        
