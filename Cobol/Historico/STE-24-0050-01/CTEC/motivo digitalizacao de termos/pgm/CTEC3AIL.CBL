      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3AIL.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3AIL                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.LISTAR.MOTIVO.DIGITALIZACAO          *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TMOTVO_PEND_DIGIT_DOCTO                         CTECB027    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDAI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKGE COMMAREA FRWK1999 (LOG DE ERRO)                    *
      *    I#FRWKHE COMMAREA FRWK1999 (AREA COMUM ERROS)               *
      *    I#FRWKD2 COMMAREA FRWK1999 (LOG DE ERROS DB2)               *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*
       77  IND-1                       PIC  9(003) VALUE ZEROS COMP-3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3AIL'.
           05 WRK-CSR-FIM              PIC  X(001) VALUE 'N'.

       01  WRK-AREA-NULOS.
           05 WRK-CSIT-REG-CTA-BCRIA-N PIC S9(004) VALUE ZEROS COMP.
           05 WRK-CUSUAR-MANUT-N       PIC S9(004) VALUE ZEROS COMP.
           05 WRK-HMANUT-REG-N         PIC S9(004) VALUE ZEROS COMP.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O CTEC3401   *'.
      *----------------------------------------------------------------*
       01  WRK-CTEC3401                PIC X(08)       VALUE 'CTEC3401'.
       01  WRK-AREA-CTEC3401.
           COPY CTECW00I.
           COPY CTECWDCI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA DE COMUNICACAO DO PROGRAMA FRWK1999 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC X(001) OCCURS 0
                                       TO 526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA PARA BOOK DE CONEXAO COM DB2 *'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.
      
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*
      
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      * >> TMOTVO_PEND_DIGIT_DOCTO
           EXEC SQL
               INCLUDE CTECB027
           END-EXEC.
      *
      *  PAGINACAO INICIAL 
           EXEC SQL
             DECLARE CSR01-CTECB027   CURSOR FOR
              SELECT CMOTVO_PEND_DIGIT
                    , RMOTVO_PEND_DIGIT
                    , CUSUAR_INCL
                    , HINCL_REG
                    , CUSUAR_MANUT
                    , HMANUT_REG
               FROM DB2PRD.TMOTVO_PEND_DIGIT_DOCTO
              WHERE CMOTVO_PEND_DIGIT  >= :CTECB027.CMOTVO-PEND-DIGIT
            ORDER BY CMOTVO_PEND_DIGIT ASC
            FETCH FIRST 51 ROWS ONLY
           END-EXEC.

      *  PAGINACAO ANTERIOR 
           EXEC SQL
             DECLARE CSR02-CTECB027   CURSOR FOR
              SELECT  CMOTVO_PEND_DIGIT
                    , RMOTVO_PEND_DIGIT
                    , CUSUAR_INCL
                    , HINCL_REG
                    , CUSUAR_MANUT
                    , HMANUT_REG
               FROM DB2PRD.TMOTVO_PEND_DIGIT_DOCTO
              WHERE CMOTVO_PEND_DIGIT  <= :CTECB027.CMOTVO-PEND-DIGIT
            ORDER BY CMOTVO_PEND_DIGIT DESC
            FETCH FIRST 51 ROWS ONLY
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDAI.
      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR.
      *
           PERFORM 2000-PROCESSAR.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    INICIALIZACAO DO PROGRAMA                                   *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  FRWKGERR-REGISTRO
                                       FRWKGHEA-REGISTRO
                                       FRWKGDB2-REGISTRO
                                       CTECW00I-BLOCO-RETORNO
                                       OF DFHCOMMAREA.
      
           PERFORM 1100-CONSISTIR-ENTRADA.
      
           MOVE 'N'                    TO WRK-CSR-FIM.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    CONSISTE DADOS DE ENTRADA                                   *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  CTECWDAI-E-QTD-REG     NOT NUMERIC
           OR  CTECWDAI-E-QTD-REG      EQUAL ZEROS
           OR  CTECWDAI-E-QTD-REG      GREATER 50
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           END-IF.
      *
           IF  CTECWDAI-E-CMOTVO-PEND-DIGIT 
                                       NOT NUMERIC
              MOVE ZEROS               TO CTECWDAI-E-CMOTVO-PEND-DIGIT 
           END-IF.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 2100-TRATAR-CHAVE-PAGINACAO.

           IF CTECWDAI-P-INICIAL 
           OR CTECWDAI-P-PRIMEIRA
           OR CTECWDAI-P-SEGUINTE
              PERFORM 2200-TRATAR-PAG-SEGUINTE
           ELSE
              PERFORM 2300-TRATAR-PAG-ANTERIOR
           END-IF.


      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-TRATAR-CHAVE-PAGINACAO     SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
             WHEN CTECWDAI-P-INICIAL
             WHEN CTECWDAI-P-PRIMEIRA
                  MOVE ZEROS           TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
             WHEN CTECWDAI-P-SEGUINTE
                  IF CTECWDAI-ULT-CHAVE
                                       GREATER CTECWDAI-PRI-CHAVE
                     MOVE CTECWDAI-ULT-CMOTVO-DIGIT 
                                       TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
                  ELSE
                     MOVE CTECWDAI-PRI-CMOTVO-DIGIT 
                                       TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
                  END-IF

             WHEN CTECWDAI-P-ANTERIOR
                  IF CTECWDAI-ULT-CHAVE
                                       GREATER CTECWDAI-PRI-CHAVE
                     MOVE CTECWDAI-PRI-CMOTVO-DIGIT 
                                       TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
                  ELSE
                     MOVE CTECWDAI-ULT-CMOTVO-DIGIT 
                                       TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
                  END-IF

             WHEN CTECWDAI-P-ULTIMA
                  MOVE 999             TO CMOTVO-PEND-DIGIT 
                                       OF CTECB027
             WHEN OTHER
                  MOVE 08              TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
                  MOVE '0030'          TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
                  MOVE 'CTEC0041'      TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
                  PERFORM 3000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-TRATAR-PAG-SEGUINTE        SECTION.
      *----------------------------------------------------------------*
           
           PERFORM 2210-OPEN-CSR01.
      
           PERFORM 2220-FETCH-CSR01.
           
           PERFORM                    UNTIL CTECWDAI-E-QTD-REG 
                                      GREATER CTECWDAI-S-NUM-REG 
                                      OR WRK-CSR-FIM EQUAL 'S'
                 
               PERFORM 2400-MOVER-SAIDA 

               PERFORM 2220-FETCH-CSR01
           END-PERFORM. 

           PERFORM 2230-CLOSE-CSR01.
           
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2210-OPEN-CSR01                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                OPEN CSR01-CTECB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-OPEN           TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2210-OPEN-CSR01'  TO FRWKGHEA-IDEN-PARAGRAFO               
               MOVE '0050'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.           

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2220-FETCH-CSR01                SECTION.
      *----------------------------------------------------------------*       
      *
           INITIALIZE WRK-AREA-NULOS.
      *
           EXEC SQL
              FETCH CSR01-CTECB027
               INTO  :CTECB027.CMOTVO-PEND-DIGIT
                   , :CTECB027.RMOTVO-PEND-DIGIT
                   , :CTECB027.CUSUAR-INCL
                   , :CTECB027.HINCL-REG
                   , :CTECB027.CUSUAR-MANUT
                         :WRK-CUSUAR-MANUT-N
                   , :CTECB027.HMANUT-REG
                         :WRK-HMANUT-REG-N
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2220-FETCH-CSR01' TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF

           IF (SQLCODE                 EQUAL +100)
               MOVE 'S'                TO WRK-CSR-FIM
           ELSE
              IF WRK-CUSUAR-MANUT-N    LESS ZEROS
                 MOVE SPACES           TO CUSUAR-MANUT
                                       OF CTECB027
              END-IF

              IF WRK-HMANUT-REG-N      LESS ZEROS
                 MOVE SPACES           TO HMANUT-REG
                                       OF CTECB027
              END-IF
           END-IF.

      *----------------------------------------------------------------*       
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2230-CLOSE-CSR01                SECTION.
      *----------------------------------------------------------------*       

           EXEC SQL
                CLOSE CSR01-CTECB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-CLOSE          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2230-CLOSE-CSR01'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0090'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*       
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*
       2300-TRATAR-PAG-ANTERIOR        SECTION.
      *----------------------------------------------------------------*
           
           PERFORM 2310-OPEN-CSR02.
      
           PERFORM 2320-FETCH-CSR02.
           
           PERFORM                    UNTIL CTECWDAI-E-QTD-REG 
                                      GREATER CTECWDAI-S-NUM-REG 
                                      OR WRK-CSR-FIM EQUAL 'S'
                 
               PERFORM 2400-MOVER-SAIDA 

               PERFORM 2320-FETCH-CSR02
           END-PERFORM. 

           PERFORM 2330-CLOSE-CSR02.    

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2310-OPEN-CSR02                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                OPEN CSR02-CTECB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-OPEN           TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2210-OPEN-CSR01'  TO FRWKGHEA-IDEN-PARAGRAFO               
               MOVE '0051'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.           

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2320-FETCH-CSR02                SECTION.
      *----------------------------------------------------------------*       
      *
           INITIALIZE WRK-AREA-NULOS.
      *
           EXEC SQL
              FETCH CSR02-CTECB027
               INTO :CTECB027.CMOTVO-PEND-DIGIT
                   ,:CTECB027.RMOTVO-PEND-DIGIT
                   ,:CTECB027.CUSUAR-INCL
                   ,:CTECB027.HINCL-REG
                   ,:CTECB027.CUSUAR-MANUT
                         :WRK-CUSUAR-MANUT-N
                   ,:CTECB027.HMANUT-REG
                         :WRK-HMANUT-REG-N
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2320-FETCH-CSR02' TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0061'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 

           END-IF

           IF (SQLCODE                 EQUAL +100)
               MOVE 'S'                TO WRK-CSR-FIM
           ELSE
              IF WRK-CUSUAR-MANUT-N    LESS ZEROS
                 MOVE SPACES           TO CUSUAR-MANUT
                                       OF CTECB027
              END-IF

              IF WRK-HMANUT-REG-N      LESS ZEROS
                 MOVE SPACES           TO HMANUT-REG
                                       OF CTECB027
              END-IF
           END-IF.

      *----------------------------------------------------------------*       
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2330-CLOSE-CSR02                SECTION.
      *----------------------------------------------------------------*       

           EXEC SQL
                CLOSE CSR02-CTECB027
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-CLOSE          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2330-CLOSE-CSR02'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0091'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*       
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*
       2400-MOVER-SAIDA                SECTION.
      *----------------------------------------------------------------*
           
           ADD 1                       TO IND-1.
           MOVE IND-1                  TO CTECWDAI-S-NUM-REG.

           MOVE CMOTVO-PEND-DIGIT      OF CTECB027
                                       TO CTECWDAI-S-CMOTVO-PEND-DIGIT 
                                          (IND-1).

           MOVE RMOTVO-PEND-DIGIT      OF CTECB027
                                       TO CTECWDAI-S-RMOTVO-PEND-DIGIT  
                                          (IND-1).
           
           IF CUSUAR-INCL              OF CTECB027 
                                       NOT EQUAL SPACES
              MOVE CUSUAR-INCL         OF CTECB027
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDAI-S-CUSUAR-INCL  
                                          (IND-1)
              PERFORM 2410-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDAI-S-IUSUAR-INCL
                                          (IND-1)
           ELSE
              MOVE SPACES              TO CTECWDAI-S-CUSUAR-INCL  
                                          (IND-1)
                                          CTECWDAI-S-IUSUAR-INCL
                                          (IND-1)
           END-IF.

           MOVE HINCL-REG              OF CTECB027
                                       TO CTECWDAI-S-HINCL-REG 
                                          (IND-1).

           IF CUSUAR-MANUT             OF CTECB027
                                       NOT EQUAL SPACES
              MOVE CUSUAR-MANUT        OF CTECB027
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDAI-S-CUSUAR-MANUT  
                                          (IND-1)
              PERFORM 2410-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDAI-S-IUSUAR-MANUT 
                                          (IND-1)
           ELSE
              MOVE SPACES              TO CTECWDAI-S-CUSUAR-MANUT  
                                          (IND-1)
                                          CTECWDAI-S-IUSUAR-MANUT
                                          (IND-1)
           END-IF.

           MOVE HMANUT-REG             OF CTECB027
                                       TO CTECWDAI-S-HMANUT-REG 
                                          (IND-1).

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2410-OBTER-NOME-USUARIO         SECTION.
      *----------------------------------------------------------------*
          
           EXEC CICS LINK
                PROGRAM (WRK-CTEC3401)
                COMMAREA (WRK-AREA-CTEC3401)
                LENGTH (LENGTH OF WRK-AREA-CTEC3401)
                NOHANDLE
           END-EXEC.
      
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA
               MOVE '2410-OBTER-NOME-USUARIO'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF.
      
           IF CTECW00I-COD-RETORNO     OF WRK-AREA-CTEC3401 
                                       NOT EQUAL ZEROS
              MOVE 'NAO LOCALIZADO'    TO CTECWDCI-S-IUSUAR 
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.    
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   ROTINA RESPONSAVEL PELA FINALIZACAO DO PROGRAMA              *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *   ROTINA PARA TRATAMENTO DE ERRO DB2                           *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-DB2               TO TRUE.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE.
           MOVE SQLCA                  TO FRWKGDB2-SQLCA.
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE.
           MOVE 16                     TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA. 
           MOVE WRK-AREA-ERRO-DB2      TO WRK-BLOCO-INFO-ERRO.
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-CICS              TO TRUE.
           MOVE 16                     TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA. 
           MOVE 'CTEC0030'             TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA. 
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE EIBFN                  TO FRWKGCIC-EIBFN.
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE.
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE.
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP.
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2.
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN.
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS).
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *   ROTINA PARA CHAMADA DA API DE ERROS                          *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM  (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH   (LENGTH        OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC.
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.
      *
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
