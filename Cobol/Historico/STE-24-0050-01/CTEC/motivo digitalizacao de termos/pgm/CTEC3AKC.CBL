      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CTEC3AKC.
       AUTHOR.     HERIBERTO GIANNASI.
      *================================================================*
      *                      S T E F A N I N I                         *
      *================================================================*
      *    PROGRAMA....: CTEC3AKC                                      *
      *    ANALISTA....:  HERIBERTO GIANNASI               - STEFANINI *
      *    DATA........:  10/2024                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PROGRAMA FUNCIONAL QUE ATENDE A FUNCAO       *
      *                   CTEC.SI.DETALHAR.MOTIVO.DIGITALIZACAO        *  
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TMOTVO_PEND_DIGIT_DOCTO                         CTECB027    *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    CTECW00I - BOOK DE CONTROLE / RETORNO                       *
      *    CTECWDBI - BOOK DE COMUNICACAO ENTRE COORDENADOR E FUNCIONAL*
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
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05 WRK-FRWK1999             PIC  X(008) VALUE 'FRWK1999'.
           05 WRK-PROGRAMA             PIC  X(008) VALUE 'CTEC3AKC'.

       01  WRK-AREA-NULOS.
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

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           COPY CTECW00I.
           COPY CTECWDBI.

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
      
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    CONSISTE DADOS DE ENTRADA                                   *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  CTECWDBI-E-CMOTVO-PEND-DIGIT 
                                       NOT NUMERIC
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0010'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
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
           PERFORM 2100-CONSULTA-MOTVO-DIGIT.
           
           PERFORM 2200-MOVER-SAIDA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2100-CONSULTA-MOTVO-DIGIT        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT CMOTVO_PEND_DIGIT
                   , RMOTVO_PEND_DIGIT
                   , CUSUAR_INCL
                   , HINCL_REG
                   , CUSUAR_MANUT
                   , HMANUT_REG
                INTO :CTECB027.CMOTVO-PEND-DIGIT
                   , :CTECB027.RMOTVO-PEND-DIGIT
                   , :CTECB027.CUSUAR-INCL
                   , :CTECB027.HINCL-REG
                   , :CTECB027.CUSUAR-MANUT
                         :WRK-CUSUAR-MANUT-N
                   , :CTECB027.HMANUT-REG
                         :WRK-HMANUT-REG-N
                 FROM DB2PRD.TMOTVO_PEND_DIGIT_DOCTO
                WHERE CMOTVO_PEND_DIGIT = :CTECB027.CMOTVO-PEND-DIGIT
           END-EXEC.
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
               SET  DB2-SELECT         TO TRUE
               MOVE SQLCODE            TO FRWKGDB2-SQLCODE
               MOVE 'TMOTVO_PEND_DIGIT_DOCTO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2100-CONSULTA-MOTVO-DIGIT' 
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0060'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0031'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM 9000-ERRO-DB2
           END-IF

           IF (SQLCODE                 EQUAL +100)
               MOVE 08                 TO CTECW00I-COD-RETORNO
                                       OF DFHCOMMAREA 
               MOVE '0011'             TO CTECW00I-COD-ERRO
                                       OF DFHCOMMAREA 
               MOVE 'CTEC0046'         TO CTECW00I-COD-MENSAGEM
                                       OF DFHCOMMAREA 
               PERFORM  3000-FINALIZAR
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
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-MOVER-SAIDA                SECTION.
      *----------------------------------------------------------------*
           
           MOVE CMOTVO-PEND-DIGIT      OF CTECB027
                                       TO CTECWDBI-S-CMOTVO-PEND-DIGIT 

           MOVE RMOTVO-PEND-DIGIT      OF CTECB027
                                       TO CTECWDBI-S-RMOTVO-PEND-DIGIT  
           
           IF CUSUAR-INCL              OF CTECB027 
                                       NOT EQUAL SPACES
              MOVE CUSUAR-INCL         OF CTECB027
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDBI-S-CUSUAR-INCL  
              PERFORM 2210-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDBI-S-IUSUAR-INCL
           ELSE
              MOVE SPACES              TO CTECWDBI-S-CUSUAR-INCL  
                                          CTECWDBI-S-IUSUAR-INCL
           END-IF.

           MOVE HINCL-REG              OF CTECB027
                                       TO CTECWDBI-S-HINCL-REG. 

           IF CUSUAR-MANUT             OF CTECB027
                                       NOT EQUAL SPACES
              MOVE CUSUAR-MANUT        OF CTECB027
                                       TO CTECWDCI-E-CUSUAR 
                                          CTECWDBI-S-CUSUAR-MANUT  
              PERFORM 2210-OBTER-NOME-USUARIO 
              
              MOVE CTECWDCI-S-IUSUAR   TO CTECWDBI-S-IUSUAR-MANUT 
           ELSE
              MOVE SPACES              TO CTECWDBI-S-CUSUAR-MANUT  
                                          CTECWDBI-S-IUSUAR-MANUT
           END-IF.

           MOVE HMANUT-REG             OF CTECB027
                                       TO CTECWDBI-S-HMANUT-REG.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2210-OBTER-NOME-USUARIO         SECTION.
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
       2210-99-FIM.                    EXIT.    
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
