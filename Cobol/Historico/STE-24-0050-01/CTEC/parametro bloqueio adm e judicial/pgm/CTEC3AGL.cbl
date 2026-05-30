      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3AGL.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3AGL                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.LISTAR.PARÂMETRO.BLOQUEIO.JUDEADM    *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TPARM_ENCRR_CTA_BLOQ                            CTECB028    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDGI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
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
       77  IND-2                       PIC  9(003) VALUE ZEROS COMP-3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050) VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3AGL'.
           05 WRK-CSR-FIM              PIC  X(001) VALUE 'N'.
      
      *    VARIAVEIS PARA ROTINA QUE OBTEM DESCRICAO DO MOTVO
      *    NO ELEMENTO BLOQ      
           05 WRK-FIM-LISTA            PIC  X(001) VALUE 'N'.
           05 WRK-ACHOU                PIC  X(001) VALUE 'N'.

       01  WRK-AREA-NULOS.
           05 WRK-NULL-01              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-02              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-03              PIC S9(004) VALUE ZEROS COMP.
           05 WRK-NULL-04              PIC S9(004) VALUE ZEROS COMP.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O CTEC3401   *'.
      *----------------------------------------------------------------*
       01  WRK-CTEC3401                PIC X(08)       VALUE 'CTEC3401'.
       01  WRK-AREA-CTEC3401.
           COPY CTECW00I.
           COPY CTECWDCI.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(051)      VALUE
           '*   AREA PARA COMUNICACAO COM O BLOQ308I   *'.
      *----------------------------------------------------------------*
       01  WRK-BLOQ308I                PIC X(08)       VALUE 'BLOQ308I'.
       01  WRK-AREA-BLOQ308I.
           COPY BLOQW000.
           COPY BLOQWC8I.

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
      * >> TPARM_ENCRR_CTA_BLOQ
           EXEC SQL
               INCLUDE CTECB028
           END-EXEC.
      *
      *  PAGINACAO INICIAL 
           EXEC SQL
             DECLARE CSR01-CTECB028   CURSOR FOR
              SELECT CPARM_ENCRR_BLOQ
                   , CMOTVO_SIT_CTA
                   , CTPO_BLOQ_OPER
                   , CINDCD_TPO_JUDIC
                   , CSIT_PARM_ENCRR
                   , CUSUAR_INCL
                   , HINCL_REG
                   , CUSUAR_MANUT
                   , HMANUT_REG
               FROM DB2PRD.TPARM_ENCRR_CTA_BLOQ
              WHERE CPARM_ENCRR_BLOQ  >= :CTECB028.CPARM-ENCRR-BLOQ
            ORDER BY CPARM_ENCRR_BLOQ ASC
            FETCH FIRST 51 ROWS ONLY
           END-EXEC.

      *  PAGINACAO ANTERIOR 
           EXEC SQL
             DECLARE CSR02-CTECB028   CURSOR FOR
              SELECT CPARM_ENCRR_BLOQ
                   , CMOTVO_SIT_CTA
                   , CTPO_BLOQ_OPER
                   , CINDCD_TPO_JUDIC
                   , CSIT_PARM_ENCRR
                   , CUSUAR_INCL
                   , HINCL_REG
                   , CUSUAR_MANUT
                   , HMANUT_REG
               FROM DB2PRD.TMOTVO_PEND_DIGIT_DOCTO
              WHERE CPARM_ENCRR_BLOQ  <= :CTECB028.CPARM-ENCRR-BLOQ
            ORDER BY CPARM_ENCRR_BLOQ DESC
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
           COPY CTECWDGI.
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
           IF  CTECWDGI-E-QTD-REG     NOT NUMERIC
           OR  CTECWDGI-E-QTD-REG      EQUAL ZEROS
           OR  CTECWDGI-E-QTD-REG      GREATER 50
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
           END-IF.
      *
           IF  CTECWDGI-E-CPARM-ENCRR-BLOQ 
                                       NOT NUMERIC
              MOVE ZEROS               TO CTECWDGI-E-CPARM-ENCRR-BLOQ
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

           IF CTECWDGI-P-INICIAL 
           OR CTECWDGI-P-PRIMEIRA
           OR CTECWDGI-P-SEGUINTE
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
             WHEN CTECWDGI-P-INICIAL
             WHEN CTECWDGI-P-PRIMEIRA
                  MOVE ZEROS           TO CPARM-ENCRR-BLOQ  
                                       OF CTECB028
             WHEN CTECWDGI-P-SEGUINTE
                  IF CTECWDGI-ULT-CHAVE
                                       GREATER CTECWDGI-PRI-CHAVE
                     MOVE CTECWDGI-ULT-CPARM-BLOQ  
                                       TO CPARM-ENCRR-BLOQ  
                                       OF CTECB028
                  ELSE
                     MOVE CTECWDGI-PRI-CPARM-BLOQ  
                                       TO CPARM-ENCRR-BLOQ 
                                       OF CTECB028
                  END-IF

             WHEN CTECWDGI-P-ANTERIOR
                  IF CTECWDGI-ULT-CHAVE
                                       GREATER CTECWDGI-PRI-CHAVE
                     MOVE CTECWDGI-PRI-CPARM-BLOQ  
                                       TO CPARM-ENCRR-BLOQ  
                                       OF CTECB028
                  ELSE
                     MOVE CTECWDGI-ULT-CPARM-BLOQ 
                                       TO CPARM-ENCRR-BLOQ  
                                       OF CTECB028
                  END-IF

             WHEN CTECWDGI-P-ULTIMA
                  MOVE 999             TO CPARM-ENCRR-BLOQ  
                                       OF CTECB028
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
           
           PERFORM                    UNTIL CTECWDGI-E-QTD-REG 
                                      GREATER CTECWDGI-S-NUM-REG 
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
                OPEN CSR01-CTECB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-OPEN           TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
              FETCH CSR01-CTECB028
                INTO :CTECB028.CPARM-ENCRR-BLOQ
                   , :CTECB028.CMOTVO-SIT-CTA
                         :WRK-NULL-01
                   , :CTECB028.CTPO-BLOQ-OPER
                   , :CTECB028.CINDCD-TPO-JUDIC
                         :WRK-NULL-02
                   , :CTECB028.CSIT-PARM-ENCRR
                   , :CTECB028.CUSUAR-INCL
                   , :CTECB028.HINCL-REG
                   , :CTECB028.CUSUAR-MANUT
                         :WRK-NULL-03
                   , :CTECB028.HMANUT-REG
                         :WRK-NULL-04
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
              IF WRK-NULL-01    LESS ZEROS
                 MOVE ZEROS            TO CMOTVO-SIT-CTA 
                                       OF CTECB028
              END-IF
              
              IF WRK-NULL-02    LESS ZEROS
                 MOVE SPACES           TO CINDCD-TPO-JUDIC 
                                       OF CTECB028
              END-IF

              IF WRK-NULL-03    LESS ZEROS
                 MOVE SPACES           TO CUSUAR-MANUT
                                       OF CTECB028
              END-IF

              IF WRK-NULL-04      LESS ZEROS
                 MOVE SPACES           TO HMANUT-REG
                                       OF CTECB028
              END-IF
           END-IF.

      *----------------------------------------------------------------*       
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2230-CLOSE-CSR01                SECTION.
      *----------------------------------------------------------------*       

           EXEC SQL
                CLOSE CSR01-CTECB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-CLOSE          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
           
           PERFORM                    UNTIL CTECWDGI-E-QTD-REG 
                                      GREATER CTECWDGI-S-NUM-REG 
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
                OPEN CSR02-CTECB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-OPEN           TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
              FETCH CSR02-CTECB028
                INTO :CTECB028.CPARM-ENCRR-BLOQ
                   , :CTECB028.CMOTVO-SIT-CTA
                         :WRK-NULL-01
                   , :CTECB028.CTPO-BLOQ-OPER
                   , :CTECB028.CINDCD-TPO-JUDIC
                         :WRK-NULL-02
                   , :CTECB028.CSIT-PARM-ENCRR
                   , :CTECB028.CUSUAR-INCL
                   , :CTECB028.HINCL-REG
                   , :CTECB028.CUSUAR-MANUT
                         :WRK-NULL-03
                   , :CTECB028.HMANUT-REG
                         :WRK-NULL-04
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-FETCH          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
              IF WRK-NULL-01    LESS ZEROS
                 MOVE ZEROS            TO CMOTVO-SIT-CTA 
                                       OF CTECB028
              END-IF
              
              IF WRK-NULL-02    LESS ZEROS
                 MOVE SPACES           TO CINDCD-TPO-JUDIC 
                                       OF CTECB028
              END-IF

              IF WRK-NULL-03    LESS ZEROS
                 MOVE SPACES           TO CUSUAR-MANUT
                                       OF CTECB028
              END-IF

              IF WRK-NULL-04      LESS ZEROS
                 MOVE SPACES           TO HMANUT-REG
                                       OF CTECB028
              END-IF
           END-IF.

      *----------------------------------------------------------------*       
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *----------------------------------------------------------------*       
       2330-CLOSE-CSR02                SECTION.
      *----------------------------------------------------------------*       

           EXEC SQL
                CLOSE CSR02-CTECB028
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
               SET  DB2-CLOSE          TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TPARM_ENCRR_CTA_BLOQ'
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
           MOVE IND-1                  TO CTECWDGI-S-NUM-REG.

           MOVE CPARM-ENCRR-BLOQ       OF CTECB028
                                       TO CTECWDGI-ULT-CPARM-BLOQ 
                                          CTECWDGI-S-CPARM-ENCRR-BLOQ   
                                          (IND-1).

           IF IND-1                    EQUAL 1
              MOVE CPARM-ENCRR-BLOQ    OF CTECB028
                                       TO CTECWDGI-PRI-CPARM-BLOQ   
           END-IF.

           MOVE CMOTVO-SIT-CTA         OF CTECB028
                                       TO CTECWDGI-S-CMOTVO-SIT-CTA  
                                          (IND-1).

           MOVE CINDCD-TPO-JUDIC       OF CTECB028
                                       TO CTECWDGI-S-CINDCD-TPO-JUDIC  
                                          (IND-1).

           MOVE CTPO-BLOQ-OPER         OF CTECB028
                                       TO CTECWDGI-S-CTPO-BLOQ-OPER  
                                          (IND-1).
     *                      
           IF CTPO-BLOQ-OPER           OF CTECB028
                                       EQUAL 1 
              PERFORM 2420-OBTER-DESC-BLOQ-ADM
           ELSE
              PERFORM 2430-OBTER-DESC-BLOQ-JUDIC
           END-IF.

           MOVE CSIT-PARM-ENCRR        OF CTECB028
                                       TO CTECWDGI-S-CSIT-PARM-ENCRR  
                                          (IND-1).

           IF CUSUAR-INCL              OF CTECB028 
                                       NOT EQUAL SPACES
              MOVE CUSUAR-INCL         OF CTECB028
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDGI-S-CUSUAR-INCL  
                                          (IND-1)
              PERFORM 2410-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDGI-S-IUSUAR-INCL
                                          (IND-1)
           ELSE
              MOVE SPACES              TO CTECWDGI-S-CUSUAR-INCL  
                                          (IND-1)
                                          CTECWDGI-S-IUSUAR-INCL
                                          (IND-1)
           END-IF.

           MOVE HINCL-REG              OF CTECB028
                                       TO CTECWDGI-S-HINCL-REG 
                                          (IND-1).

           IF CUSUAR-MANUT             OF CTECB028
                                       NOT EQUAL SPACES
              MOVE CUSUAR-MANUT        OF CTECB028
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDGI-S-CUSUAR-MANUT  
                                          (IND-1)
              PERFORM 2410-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDGI-S-IUSUAR-MANUT 
                                          (IND-1)
           ELSE
              MOVE SPACES              TO CTECWDGI-S-CUSUAR-MANUT  
                                          (IND-1)
                                          CTECWDGI-S-IUSUAR-MANUT
                                          (IND-1)
           END-IF.

           MOVE HMANUT-REG             OF CTECB028
                                       TO CTECWDGI-S-HMANUT-REG 
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
       2420-OBTER-DESC-BLOQ-ADM        SECTION.
      *----------------------------------------------------------------*
           INITIALIZE BLOQWC8I-BLOCO-ENTRADA 

           MOVE 1                      TO BLOQWC8I-E-CTPO-OPERACAO.
           MOVE 'I'                    TO BLOQWC8I-INDICADOR-PAGINACAO. 
           MOVE 'N'                    TO WRK-ACHOU   
                                          WRK-FIM-LISTA   
           
           PERFORM                     UNTIL WRK-ACHOU 
                                       EQUAL 'S'
                                       OR WRK-FIM-LISTA
                                       EQUAL 'S'
               PERFORM 2421-ACESSAR-BLOQ308I

               PERFORM 2422-LOCALIZAR-DESCRICAO

           END-PERFORM.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.    
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2421-ACESSAR-BLOQ308I           SECTION.       
      *----------------------------------------------------------------*

           EXEC CICS LINK
                PROGRAM (WRK-BLOQ308I)
                COMMAREA (WRK-AREA-BLOQ308I)
                LENGTH (LENGTH OF WRK-AREA-BLOQ308I)
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
      
           IF BLOQW000-COD-RETORNO     OF WRK-AREA-BLOQ308I  
                                       NOT EQUAL ZEROS AND 01
               MOVE 16                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA
               MOVE '0280'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA
               MOVE 'CTEC0030'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA

               PERFORM 3000-FINALIZAR
           END-IF.

           IF BLOQW000-COD-RETORNO     OF WRK-AREA-BLOQ308I  
                                       EQUAL ZEROS
              MOVE 'S'                 TO WRK-FIM-LISTA
           ELSE
              MOVE 'S'                 TO BLOQWC8I-INDICADOR-PAGINACAO 
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2422-LOCALIZAR-DESCRICAO        SECTION.
      *----------------------------------------------------------------*
           
           PERFORM                     VARYING IND-2
                                       FROM 1
                                       BY 1
                                       UNTIL IND-2 
                                       GREATER BLOQWC8I-NUM-LINHAS 
                                       OR WRK-ACHOU 
                                       EQUAL 'S'
           
              IF BLOQWC8I-E-CMOTVO-SIT-CTA(IND-2)
                                       EQUAL CTECWDGI-S-CMOTVO-SIT-CTA 
                                             (IND-1)
                 MOVE BLOQWC8I-S-RJUSTF-BLOQ(IND-2)
                                       TO CTECWDGI-S-RMOTVO-SIT-CTA
                                          (IND-1)
                 MOVE 'S'              TO WRK-ACHOU
              END-IF
           END-PERFORM. 

      *----------------------------------------------------------------*
       2422-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2430-OBTER-DESC-BLOQ-JUDIC      SECTION. 
      *----------------------------------------------------------------*
           
           EVALUATE CINDCD-TPO-JUDIC  OF CTECB028 
              WHEN 'V'
                 MOVE 'BLOQUEIO VALOR' 
                                      TO CTECWDGI-S-RINDCD-TPO-JUDIC 
                                         (IND-1)
              WHEN 'T'
                 MOVE 'BLOQUEIO TOTAL' 
                                      TO CTECWDGI-S-RINDCD-TPO-JUDIC 
                                         (IND-1)
              WHEN 'A'
                 MOVE 'AMBOS (TOTAL + VALOR)' 
                                      TO CTECWDGI-S-RINDCD-TPO-JUDIC 
                                         (IND-1)

              WHEN OTHER 
                 MOVE 'CODIGO NAO IDENTIFICADO'
                                      TO CTECWDGI-S-RINDCD-TPO-JUDIC 
                                         (IND-1)
           END-EVALUATE.


      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
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
