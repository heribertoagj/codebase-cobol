      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2Y2E.
       AUTHOR.     CAPGEMINI.

      *================================================================*
      *    PROGRAMA....:  GFCT2Y2E                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  A PARTIR DO ARQUIVO ENVIADO PELO GESTOR
      *                   CONVERTE TARIFA                              *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/O           INCLUDE/BOOK     *
      *                ARQENTTA          I              GFCTWK2G       *
      *                ARQDATAS          I              GOTFW99C       *
      *                CARGASOK          O              GOTRWU3I       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    GFCTWK2G - BOOK DE ENTRADA-TARIFAS PERMITIDAS PARA ESTORNO  *
      *    GOTRWU3I - BOOK DE SAIDA-TARIFAS PERMITIDAS ESTOR.FORMATADA*
      *    CALEWAAA - BOOK DO CALE2000 - FORNECE DATA E HORA ATUAL     *
      *    CALEWAAN - BOOK DE CALE2000 -FORNECE QTD DIAS CORRIDOS 1 MES*
      *    I#FRWKAR - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS        *
      *    I#FRWKGE - BOOK PARA TRATAMENTO DE ERROS DE ARQUIVOS        *
      *    I#FRWKLI - BOOK PARA TRATAMENTO DE ERROS LIVRE              *
      *    I#FRWKMD - BOOK PARA TRATAMENTO DE ERROS DE MODULO          *
      *    I#CKRS04 - BOOK DE CONEXAO DB2                              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    CKRS1000 - MODULO PARA INDICAR PROCESSAMWENTO               *
      *    CKRS0105 - MODULO PARA INICIAR E FINALIZAR CONEXAO DB2      *
      *    CALE2000 - MODULO DE CALENDARIO                             *
      *    FRWK2999 - GRAVAR LOG DE ERRO P/ BATCH                      *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS   COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQENTTA ASSIGN      TO   UT-S-ARQENTTA
                      FILE STATUS      IS   WRK-FS-ARQENTTA.

           SELECT ARQDATAS ASSIGN      TO   UT-S-ARQDATAS
                      FILE STATUS      IS   WRK-FS-ARQDATAS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA TARIFAS PERMITIDAS               *
      *            ORG. SEQUENCIAL     -   LRECL   =  136              *
      *----------------------------------------------------------------*
       FD  ARQENTTA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ARQENTTA                 PIC  X(136).

      *----------------------------------------------------------------*
      *   INPUT:   ARQUIVO DE ENTRADA DATAS DO PROCESSAMENTO DO SISTEMA*
      *            ORG. SEQUENCIAL     -   LRECL   =  047              *
      *----------------------------------------------------------------*
       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ARQDATAS                 PIC  X(047).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC X(050)          VALUE
           '*** INICIO DA WORKING-STORAGE SECTION         ****'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC X(050)          VALUE
           '*** AREA DE COMUNICACAO COM CALE2000           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       77  WRK-CALE2000                PIC X(08)           VALUE
           'CALE2000'.

       COPY 'CALEWAAA'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC X(050)          VALUE
           '*** AREA DE AUXILIARES                         ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *


       01 WRK-DATAS.
        05 WRK-DPROCM-ATUAL.
           10 WRK-DD-DPROCM-ATUAL      PIC 9(02)           VALUE ZEROS.
           10 FILLER                   PIC X(01)           VALUE SPACES.
           10 WRK-MM-DPROCM-ATUAL      PIC 9(02)           VALUE ZEROS.
           10 FILLER                   PIC X(01)           VALUE SPACES.
           10 WRK-AA-DPROCM-ATUAL      PIC 9(04)           VALUE ZEROS.

       01  WRK-RETURN-CODE-S9          PIC +9(004)         VALUE ZEROS.
       01  FILLER REDEFINES            WRK-RETURN-CODE-S9.
           05 FILLER                   PIC  X(001).
           05 WRK-RETURN-CODE-9        PIC  9(004).

       77  ACU-INSERT                  PIC  9(009) COMP-3  VALUE ZEROS.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-ACUMULADORES.
           05 WRK-LIDOS-ARQENTTA       PIC 9(09) COMP-3    VALUE ZEROS.
           05 WRK-LIDOS-ARQDATAS       PIC 9(09) COMP-3    VALUE ZEROS.

       01  WRK-MASK                    PIC ZZZ.ZZZ.ZZ9     VALUE ZEROS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER               PIC X(32) VALUE
           '*  AREA DE DISPLAY         *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-CTRL-DISPLAYS.
           05  WRK-DT-COMPILED         PIC  X(16)          VALUE SPACES.
           05  WRK-DS-COMPILED         PIC  X(08)BBBX(08)  VALUE SPACES.
           05  WRK-HR-EXECUTE          PIC  9(8)           VALUE ZEROS.
           05  WRK-HR-EXECUCAO         PIC  9(8)           VALUE ZEROS.
           05  WRK-HR-EXECUCAO-X       REDEFINES WRK-HR-EXECUCAO
                                       PIC  X(8).
           05  WRK-DT-EXECUTE          PIC  9(6)           VALUE ZEROS.
           05  WRK-DT-EXECUCAO         PIC  9(8)           VALUE ZEROS.
           05  WRK-DT-EXECUCAO-X       REDEFINES  WRK-DT-EXECUCAO
                                       PIC  X(8).
      *

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE TESTE DE FILE-STATUS               ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FS-ARQENTTA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA ENTRADA ARQENTTA PERSONALIZADAS         *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       COPY 'GFCTWK2G'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ENTRADA ARQDATAS                   ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01 WRK-ARQDATAS.
           COPY 'GOTFW99C'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO FRWK2999                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-FRWK2999                PIC  X(008)         VALUE
           'FRWK2999'.

       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
              10 WRK-CHAR-INFO-ERRO    PIC  X(001) OCCURS 0 TO 600
                                       TIMES DEPENDING ON
                                       FRWKGHEA-TAM-DADOS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
      *          AREA DE ACESSO AO MODULO DE CHECKPOINT/RESTART        *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       77  WRK-CKRS0105             PIC X(008)       VALUE
           'CKRS0105'.

           COPY 'I#CKRS04'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE ARQUIVO      ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-ARQUIVO.
           COPY 'I#FRWKAR'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ FORMATACAO DE ERRO DE MODULO       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-MODULO.
           COPY 'I#FRWKMD'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0450                           ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-BRAD0450.
           05 WRK-0450-ABEND-BAT       PIC S9(004) COMP    VALUE +1111.
           05 WRK-0450-DUMP-BAT        PIC  X(001)         VALUE 'S'.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ERROS DB2                        ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

       01  WRK-AREA-ERRO-DB2.
           COPY 'I#FRWKDB'.

       01  WRK-SQLCODE-AUX             PIC S9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SQLCODE-AUX.
           05 FILLER                   PIC  9(006).
           05 WRK-SQLCODE-9-3          PIC S9(003).

       01  WRK-MASCARA-RC              PIC ZZZ9            VALUE ZEROS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA P/ INCLUDES DB2                       ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

      *
           EXEC SQL
               INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I0
           END-EXEC.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE SECTION             ***'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE '0000-ROTINA-PRINCIPAL'
                                       TO  FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 9500-00-MONTA-COMPILED
      *
           DISPLAY '*---------------------------------------------*'
           DISPLAY '| GOTF2ABA - COMPILED : ' WRK-DS-COMPILED '   |'
           DISPLAY '|  INICIO DA EXECUCAO : ' WRK-DT-EXECUCAO
                                        '   ' WRK-HR-EXECUCAO '   |'
           DISPLAY '+---------------------------------------------+'.

           PERFORM 1000-INICIAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 2005-CAPTURA-DATAS.

           PERFORM 3000-PROCESSAR      UNTIL     WRK-FS-ARQENTTA
                                       EQUAL     '10'.


           PERFORM 9000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           MOVE '1000-INICIAR'         TO FRWKGHEA-IDEN-PARAGRAFO.

           CALL 'CKRS1000'.

           PERFORM 7000-INICIAR-CKRS0105.

           INITIALIZE FRWKGHEA-REGISTRO
                      FRWKGARQ-REGISTRO
                      FRWKGDB2-REGISTRO.

           OPEN INPUT  ARQENTTA
                       ARQDATAS.

           PERFORM 1100-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1100-TESTAR-FILE-STATUS'
                                       TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 1110-TESTAR-FS-ARQENTTA.

           PERFORM 1120-TESTAR-FS-ARQDATAS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQENTTA         SECTION.
      *----------------------------------------------------------------*

           MOVE '1110-TESTAR-FS-ARQENTTA'
                                       TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-ARQENTTA         NOT EQUAL  '00'
               MOVE 'ARQENTTA'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-ARQENTTA    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-ARQDATAS         SECTION.
      *----------------------------------------------------------------*

           MOVE '1120-TESTAR-FS-ARQDATAS'
                                       TO   FRWKGHEA-IDEN-PARAGRAFO.

           IF  WRK-FS-ARQDATAS         NOT EQUAL  '00'
               MOVE 'ARQDATAS'         TO FRWKGARQ-NOME-ARQUIVO
               MOVE WRK-FS-ARQDATAS    TO FRWKGARQ-FILE-STATUS
               PERFORM 9100-FORMATAR-ERRO-ARQUIVO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           MOVE '2000-VERIFICAR-VAZIO' TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2200-LER-ARQDATAS.

           IF (WRK-LIDOS-ARQDATAS      EQUAL ZEROS)
               DISPLAY '************ GOTF2ABA ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO ARQDATAS VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GOTF2ABA ************'
               PERFORM 9000-FINALIZAR
           END-IF.

           PERFORM 2100-LER-ARQENTTA.

           IF (WRK-LIDOS-ARQENTTA      EQUAL ZEROS)
               DISPLAY '************ GOTF2ABA ************'
               DISPLAY '*                                *'
               DISPLAY '*     ARQUIVO ARQENTTA VAZIO     *'
               DISPLAY '*       PROGRAMA ENCERRADO       *'
               DISPLAY '*                                *'
               DISPLAY '************ GOTF2ABA ************'
               PERFORM 9000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2005-CAPTURA-DATAS              SECTION.
      *----------------------------------------------------------------*

           MOVE '2005-CAPTURA-DATAS'   TO   FRWKGHEA-IDEN-PARAGRAFO.

           PERFORM 2400-OBTER-DATA-HORA.

      *----------------------------------------------------------------*
       2005-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ARQENTTA               SECTION.
      *----------------------------------------------------------------*

           MOVE '2100-LER-ARQENTTA'    TO   FRWKGHEA-IDEN-PARAGRAFO.

           READ ARQENTTA               INTO GFCTWK2G-REGISTRO.

           IF WRK-FS-ARQENTTA EQUAL '10'
               GO TO 2100-99-FIM
           ELSE
               PERFORM 1110-TESTAR-FS-ARQENTTA
               ADD  1                  TO   WRK-LIDOS-ARQENTTA
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2200-LER-ARQDATAS               SECTION.
      *----------------------------------------------------------------*

           MOVE '2200-LER-ARQDATAS'    TO   FRWKGHEA-IDEN-PARAGRAFO.

           READ ARQDATAS               INTO WRK-ARQDATAS.

           IF WRK-FS-ARQDATAS EQUAL '10'
               NEXT SENTENCE
           ELSE
               PERFORM 1120-TESTAR-FS-ARQDATAS
               ADD  1                  TO   WRK-LIDOS-ARQDATAS
               PERFORM 2210-MONTAR-DATAS-ARQDATAS
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2210-MONTAR-DATAS-ARQDATAS      SECTION.
      *----------------------------------------------------------------*

           MOVE '2210-MONTAR-DATAS-ARQDATAS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE GOTFW99C-DPROCM-ATUAL  TO WRK-DPROCM-ATUAL.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       2400-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           MOVE '2400-OBTER-DATA-HORA' TO FRWKGHEA-IDEN-PARAGRAFO.

           CALL WRK-CALE2000           USING CALEWAAA.

           IF (CALEWAAA-COD-RET NOT EQUAL ZEROS)
               DISPLAY '*********** GOTFV2VUF ***********'
               DISPLAY '*                              *'
               DISPLAY '* ERRO MODULO CALE2000         *'
               DISPLAY '* ---------------------------- *'
               DISPLAY '* COD-RETORNO.: ' CALEWAAA-COD-RET
                                         '             *'
               DISPLAY '* COD-ERRO....: ' CALEWAAA-COD-ERRO
                                           '           *'
               DISPLAY '* COD-MENSAGEM: ' CALEWAAA-COD-MSGEM
                                               '       *'
               DISPLAY '*                              *'
               DISPLAY '*********** GOTFV2VUF ***********'
               MOVE WRK-CALE2000       TO FRWKGMOD-NOME-MODULO
               MOVE CALEWAAA-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
               PERFORM 9300-TRATAR-ERRO-MODULO
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3310-UPDATE-LIN-GFCTB0C6.

           PERFORM 2100-LER-ARQENTTA.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3310-UPDATE-LIN-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*
      *
             MOVE '3310-UPDATE-LIN-GFCTB0C6'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
      *
              MOVE GFCTWK2G-CSERVC-TARIF
                                       TO  CSERVC-TARIF
                                       OF  GFCTB0C6

              MOVE GFCTWK2G-CAGPTO-CTA TO  CAGPTO-CTA
                                       OF  GFCTB0C6

              MOVE GFCTWK2G-CSEQ-AGPTO-CTA
                                       TO  CSEQ-AGPTO-CTA
                                       OF  GFCTB0C6

              MOVE GFCTWK2G-DINIC-PRMSS-PCOTE
                                       TO  DINIC-PRMSS-PCOTE
                                       OF  GFCTB0C6

              MOVE GFCTWK2G-COD-USUARIO
                                       TO  CFUNC-MANUT
                                       OF  GFCTB0C6

              EXEC SQL
                  DELETE FROM DB2PRD.TPRMSS_GRP_CLI
                  WHERE
                      CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                  AND CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
                  AND CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
                  AND DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
              END-EXEC

              IF (SQLCODE                 NOT EQUAL ZEROS) OR
                 (SQLWARN0                EQUAL 'W')
                  SET DB2-UPDATE          TO TRUE
                  MOVE 'TPRMSS_GRP_CLI'
                                          TO FRWKGDB2-NOME-TABELA
                  MOVE '3210'             TO FRWKGMOD-COD-ERRO
                  SET  DB2-SELECT         TO TRUE
                  PERFORM 9200-FORMATAR-ERRO-DB2
             END-IF


              EXEC SQL
                  DELETE FROM DB2PRD.PRMSS_ADSAO_PCOTE
                  WHERE
                      CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                  AND CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
                  AND CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
                  AND DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
              END-EXEC


              IF (SQLCODE                 NOT EQUAL ZEROS) OR
                 (SQLWARN0                EQUAL 'W')
                  SET DB2-UPDATE          TO TRUE
                  MOVE 'PRMSS_ADSAO_PCOTE'
                                          TO FRWKGDB2-NOME-TABELA
                  MOVE '3210'             TO FRWKGMOD-COD-ERRO
                  SET  DB2-SELECT         TO TRUE
                  PERFORM 9200-FORMATAR-ERRO-DB2
             END-IF



             IF (SQLCODE                 EQUAL ZEROS)
               PERFORM 5200-COMMIT-CONEXAO-DB2
               ADD 1                   TO ACU-INSERT
             END-IF.
      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR REALIZAR A DESCONEXAO COM O DB2      *
      *----------------------------------------------------------------*
       5200-COMMIT-CONEXAO-DB2         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'P'                    TO PARM-OP
           MOVE 'DB2'                  TO PARM-SSID
           MOVE SPACES                 TO PARM-PLAN
      *
           CALL WRK-CKRS0105           USING PARM-CKRS0105
      *
           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE RETURN-CODE        TO WRK-RETURN-CODE-S9
               MOVE WRK-RETURN-CODE-9  TO WRK-MASCARA-RC
               DISPLAY '********** PROGRAMA GOTF2ABB **********'
               DISPLAY '*                                     *'
               DISPLAY '*     ERRO NO COMMIT DA CKRS0105      *'
               DISPLAY '*          RETURN-CODE:' WRK-MASCARA-RC
               '           *'
               DISPLAY '*                                     *'
               DISPLAY '***************************************'
               PERFORM 9000-FINALIZAR
           END-IF.
      *
      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       7000-INICIAR-CKRS0105         SECTION.
      *----------------------------------------------------------------*

           MOVE '7000-INICIAR-CKRS0105'
                                     TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'C'                  TO PARM-OP.
           MOVE 'DB2'                TO PARM-SSID.
           MOVE SPACES               TO PARM-PLAN.

           CALL WRK-CKRS0105         USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-ENCERRAR-CKRS0105        SECTION.
      *----------------------------------------------------------------*

           MOVE '7100-ENCERRAR-CKRS0105'
                                     TO FRWKGHEA-IDEN-PARAGRAFO.

           MOVE 'D'                  TO PARM-OP.
           MOVE SPACES               TO PARM-PLAN.

           CALL WRK-CKRS0105         USING PARM-CKRS0105.

      *----------------------------------------------------------------*
       7100-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '9000-FINALIZAR'     TO FRWKGHEA-IDEN-PARAGRAFO.
      *
           DISPLAY '*********** GOTF2ABA ***********'
           DISPLAY '*                              *'
           DISPLAY '*     RESUMO PROCESSAMENTO     *'
           DISPLAY '* ---------------------------- *'
           MOVE    WRK-LIDOS-ARQENTTA
                                     TO WRK-MASK
           DISPLAY '* LIDOS  ARQENTTA : ' WRK-MASK
                                                 '*'
           MOVE    WRK-LIDOS-ARQDATAS
                                     TO WRK-MASK
           DISPLAY '* LIDOS  ARQDATAS : ' WRK-MASK
                                                 '*'
           MOVE     ACU-INSERT    TO WRK-MASK
           DISPLAY '* REG   ATUALIZADO: ' WRK-MASK
                                                 '*'
           DISPLAY '*********** GOTF2ABA ***********'

           PERFORM 9500-00-MONTA-COMPILED

           DISPLAY '+---------------------------------------------+'
           DISPLAY '| GOTF2ABA - COMPILED : ' WRK-DS-COMPILED '   |'
           DISPLAY '|   FINAL DA EXECUCAO : ' WRK-DT-EXECUCAO
                                        '   ' WRK-HR-EXECUCAO '   |'
           DISPLAY '*---------------------------------------------*'.

           PERFORM 7100-ENCERRAR-CKRS0105.

           CLOSE ARQENTTA
                 ARQDATAS.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-FORMATAR-ERRO-ARQUIVO      SECTION.
      *----------------------------------------------------------------*

           SET ERRO-ARQUIVO            TO TRUE.

           MOVE FRWKGARQ-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-ARQUIVO  TO WRK-BLOCO-INFO-ERRO.

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           SET  ERRO-DB2               TO  TRUE.

           MOVE FRWKGDB2-TAM-LAYOUT    TO  FRWKGHEA-TAM-DADOS.
           MOVE FRWKGHEA-IDEN-PARAGRAFO(1:16)
                                       TO  FRWKGDB2-LOCAL.
           MOVE SQLSTATE               TO  FRWKGDB2-SQLSTATE.
           MOVE SQLCA                  TO  FRWKGDB2-SQLCA.
           MOVE SQLCODE                TO  WRK-SQLCODE-AUX.
           MOVE WRK-SQLCODE-9-3        TO  FRWKGDB2-SQLCODE2.
           MOVE WRK-AREA-ERRO-DB2      TO  WRK-BLOCO-INFO-ERRO.

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9300-TRATAR-ERRO-MODULO         SECTION.
      *----------------------------------------------------------------*

           SET ERRO-MODULO             TO TRUE.

           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-AREA-ERRO-MODULO   TO WRK-BLOCO-INFO-ERRO.

           DISPLAY ' '.
           DISPLAY 'FRWKGMOD-NOME-MODULO   = ' FRWKGMOD-NOME-MODULO.
           DISPLAY 'FRWKGMOD-COD-RETORNO   = ' FRWKGMOD-COD-RETORNO.
           DISPLAY 'FRWKGMOD-COD-ERRO      = ' FRWKGMOD-COD-ERRO.
           DISPLAY 'FRWKGMOD-COD-MENSAGEM  = ' FRWKGMOD-COD-MENSAGEM.

           PERFORM 9900-TRATAR-ERRO.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9500-00-MONTA-COMPILED          SECTION.
      *----------------------------------------------------------------*
      *
           MOVE    WHEN-COMPILED       TO  WRK-DT-COMPILED.
           MOVE    WRK-DT-COMPILED     TO  WRK-DS-COMPILED.
           MOVE    WRK-DT-COMPILED (1:2)
                                       TO  WRK-DS-COMPILED   (4:2).
           MOVE    WRK-DT-COMPILED (4:2)
                                       TO  WRK-DS-COMPILED   (1:2).
           ACCEPT  WRK-HR-EXECUTE      FROM  TIME.
           MOVE    WRK-HR-EXECUTE (1:2)TO  WRK-HR-EXECUCAO   (1:2).
           MOVE    '.'                 TO  WRK-HR-EXECUCAO-X (3:1).
           MOVE    WRK-HR-EXECUTE (3:2)TO  WRK-HR-EXECUCAO   (4:2).
           MOVE    '.'                 TO  WRK-HR-EXECUCAO-X (6:1).
           MOVE    WRK-HR-EXECUTE (5:2)TO  WRK-HR-EXECUCAO   (7:2).
           ACCEPT  WRK-DT-EXECUTE      FROM  DATE.
           MOVE    WRK-DT-EXECUTE (1:2)TO  WRK-DT-EXECUCAO   (7:2).
           MOVE    '/'                 TO  WRK-DT-EXECUCAO-X (6:1).
           MOVE    WRK-DT-EXECUTE (3:2)TO  WRK-DT-EXECUCAO   (4:2).
           MOVE    '/'                 TO  WRK-DT-EXECUCAO-X (3:1).
           MOVE    WRK-DT-EXECUTE (5:2)TO  WRK-DT-EXECUCAO   (1:2).
      *
      *----------------------------------------------------------------*
       9500-99-MONTA-COMPILED.         EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9900-TRATAR-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GOTF2ABA'             TO FRWKGHEA-NOME-PROGRAMA.

           PERFORM 9990-GRAVAR-LOG-ERRO.

           PERFORM 9999-ABENDAR-PROGRAMA.

           GOBACK.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9990-GRAVAR-LOG-ERRO            SECTION.
      *----------------------------------------------------------------*

           CALL WRK-FRWK2999           USING WRK-AREA-ERRO.

      *----------------------------------------------------------------*
       9990-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ABENDAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*** BRAD0450 CHAMADO PARA ABENDAR O PROGRAMA ***'.
           DISPLAY ' '.

           CALL 'BRAD0450'             USING WRK-0450-ABEND-BAT
                                             WRK-0450-DUMP-BAT.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
