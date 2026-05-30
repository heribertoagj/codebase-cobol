      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID.  GFCT4470.
       AUTHOR.      CIBELE BARBOSA.
      *===============================================================*
      *                         S O N D A - I T                       *
      *---------------------------------------------------------------*
      *    PROGRAMA....:  GFCT4470                                    *
      *    PROGRAMADOR:   CIBELE BARBOSA            - SONDA IT        *
      *    ANALISTA CPM:  CIBELE BARBOSA            - SONDA IT        *
      *    DATA........:  13/05/2014                                  *
      *---------------------------------------------------------------*
      *    OBJETIVO....:  LIMPAR REGISTROS DO ARQUIVO SIGB            *
      *---------------------------------------------------------------*
      *    BCO DE DADOS:                                              *
      *                DB2                            INCLUDE/BOOK    *
      *                  TABLES                                       *
      *                  DB2PRD.PARM_DATA_PROCM       GFCTB0A1        *
      *    ARQUIVOS....:                                              *
      *               DDNAME                          INCLUDE/BOOK    *
      *                 ARQSIGBE                        GFCTWBBD      *
      *                 ARQSIGBS                        GFCTWBBD      *
      *---------------------------------------------------------------*
      *    BOOK'S......:                                              *
      *    POL7100C - AREA DE TRATAMENTO DE ERRO PELA BRAD7100.       *
      *---------------------------------------------------------------*
      *    MODULOS......:                                             *
      *    BRAD1050 - CONEXAO COM BANCO DE DADOS DB2                  *
      *    BRAD7100 - TRATAMENTO DE ERROS.                            *
      *===============================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQSIGBE  ASSIGN     TO ARQSIGBE
                      FILE  STATUS     IS WRK-FS-ARQSIGBE.

           SELECT ARQSIGBS  ASSIGN     TO ARQSIGBS
                      FILE  STATUS     IS WRK-FS-ARQSIGBS.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    INPUT :  ARQUIVO DE ENTRADA                                 *
      *             ORG. SEQUENCIAL    - LRECL = 0034                  *
      *----------------------------------------------------------------*

       FD  ARQSIGBE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQSIGBE                 PIC  X(034).

      *----------------------------------------------------------------*
      *    OUTPUT:  ARQUIVO DE SAIDA                                   *
      *             ORG. SEQUENCIAL    - LRECL = 0034                  *
      *----------------------------------------------------------------*

       FD  ARQSIGBS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ARQSIGBS                 PIC  X(034).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING GFCT4470 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                PIC  Z.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MSG-PARM-VZIO           PIC  X(075)         VALUE
           'PARAMETRO INCORRETO,    PROGRAMA CANCELADO'.
       01  IND-M                       PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-MESES                   PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-MES                     PIC  9(002) COMP-3  VALUE ZEROS.
       01  WRK-ANO                     PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-6                  PIC X(006)         VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-DATA-6.
           05 WRK-DATA-6-NUM           PIC 9(06).

       01  WRK-DATA-ANOMES.
           05  WRK-ANO-ANOMES          PIC  9(004)         VALUE ZEROS.
           05  WRK-MES-ANOMES          PIC  9(002)         VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05  ACU-LIDOS-ARQSIGBE      PIC  9(010) COMP-3  VALUE ZEROS.
           05  ACU-GRAVA-ARQSIGBS      PIC  9(010) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** ARQUIVOS ARQSIGBE/S ***'.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQUIVOS.
           05  WRK-FS-ARQSIGBE         PIC  X(002)         VALUE '00'.
           05  WRK-FS-ARQSIGBS         PIC  X(002)         VALUE '00'.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           03  FILLER                  PIC  X(007)         VALUE SPACES.
           03  FILLER                  PIC  X(007)         VALUE
               '* ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(020)         VALUE
               ' - FILE-INDICADOR = '.
           03  WRK-FS                  PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE ' *'.
           03  FILLER                  PIC  X(016)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING-STORAGE GFCT4470 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-AREA.
           05  LNK-TAMANHO             PIC S9(004) COMP.
           05  LNK-MESES               PIC  9(002).

      *================================================================*
       PROCEDURE DIVISION              USING LNK-AREA.

      *----------------------------------------------------------------*
      *    CONTROLE GERAL DE PROCESSOS                                 *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0800-VERIFICAR-VAZIO.

           PERFORM 1000-PROCESSAR      UNTIL WRK-FS-ARQSIGBE
                                       EQUAL '10'.

           PERFORM 1200-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           PERFORM 0200-CONSISTIR-CAMPOS.

           CALL 'BRAD1050'.

           OPEN INPUT  ARQSIGBE
                OUTPUT ARQSIGBS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQUIVOS.

           PERFORM 0400-SELECT-TB-GFCTB0A1.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *     CONSISTENCIA DOS CAMPOS                                    *
      *----------------------------------------------------------------*
       0200-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           IF  LNK-TAMANHO             NOT EQUAL 02 OR
               LNK-MESES               NOT NUMERIC

               DISPLAY ' '
               DISPLAY '*************** GFCT4470 ****************'
               DISPLAY '*                                       *'
               DISPLAY '*      ERRO NO PARAMETRO INFORMADO      *'
               DISPLAY '*                                       *'
               DISPLAY '*************** GFCT4470 ****************'
               DISPLAY ' '

               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE WRK-MSG-PARM-VZIO  TO ERR-TEXTO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TESTE DE FILE STATUS DOS ARQUIVOS USADOS                   *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQUIVOS         SECTION.
      *----------------------------------------------------------------*

           PERFORM  0500-TESTAR-FS-ARQSIGBE.

           PERFORM  0600-TESTAR-FS-ARQSIGBS.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       0400-SELECT-TB-GFCTB0A1         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT DPROCM_ATUAL
               INTO :GFCTB0A1.DPROCM-ATUAL
               FROM DB2PRD.PARM_DATA_PROCM
               WHERE CSIST_PRINC = 'GFCT'
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)    OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.
           PERFORM 0700-CALCULAR-DATA.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DO ARQUIVO ARQSIGBE
      *----------------------------------------------------------------*
       0500-TESTAR-FS-ARQSIGBE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQSIGBE         NOT EQUAL '00'
               MOVE WRK-FS-ARQSIGBE     TO WRK-FS
               MOVE WRK-MSG-FS          TO ERR-TEXTO
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'ARQSIGBE'          TO WRK-ARQUIVO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTAR FILE STATUS DO ARQUIVO ARQSIGBE
      *----------------------------------------------------------------*
       0600-TESTAR-FS-ARQSIGBS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQSIGBS         NOT EQUAL '00'
               MOVE WRK-FS-ARQSIGBS     TO WRK-FS
               MOVE WRK-MSG-FS          TO ERR-TEXTO
               MOVE 'APL'               TO ERR-TIPO-ACESSO
               MOVE 'ARQSIGBS'          TO WRK-ARQUIVO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       0700-CALCULAR-DATA              SECTION.
      *----------------------------------------------------------------*

           MOVE LNK-MESES          TO WRK-MESES.

           MOVE DPROCM-ATUAL       TO WRK-DATA-AUX.
           MOVE WRK-MES-AUX        TO WRK-MES.
           MOVE WRK-ANO-AUX        TO WRK-ANO.

           PERFORM VARYING IND-M       FROM 1 BY 1
             UNTIL IND-M           GREATER WRK-MESES
                OR IND-M           GREATER 12

                   SUBTRACT 1      FROM WRK-MES

                   IF  WRK-MES     EQUAL ZEROS
                       MOVE 12     TO WRK-MES

                       SUBTRACT 1  FROM WRK-ANO
                   END-IF

           MOVE WRK-MES            TO WRK-MES-ANOMES.
           MOVE WRK-ANO            TO WRK-ANO-ANOMES.
           MOVE WRK-DATA-ANOMES    TO WRK-DATA-6.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE O ARQUIVO DE ENTRADA ESTA VAZIO    *
      *----------------------------------------------------------------*
       0800-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0900-LER-ARQSIGBE.

           IF  WRK-FS-ARQSIGBE         EQUAL '10'
               DISPLAY '************** GFCT4470 **************'
               DISPLAY '*                                    *'
               DISPLAY '*       ARQUIVO ARQSIGBE VAZIO       *'
               DISPLAY '*     PROCESSAMENTO   FINALIZADO     *'
               DISPLAY '*                                    *'
               DISPLAY '************** GFCT4470 **************'
           END-IF.

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LER ARQUIVO DO SIGB                                         *
      *----------------------------------------------------------------*
       0900-LER-ARQSIGBE               SECTION.
      *----------------------------------------------------------------*

           READ ARQSIGBE               INTO GFCTWBBD-REG.

           IF  WRK-FS-ARQSIGBE         EQUAL '10'
               GO                      TO 0900-99-FIM
           END-IF.


           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-ARQSIGBE.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL                                     *
      *----------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF WBBD-AAAA-MM-APURACAO NOT GREATER WRK-DATA-6-NUM
              PERFORM 0900-LER-ARQSIGBE
           ELSE
              PERFORM 1100-GRAVAR-ARQSIGBS
              PERFORM 0900-LER-ARQSIGBE
           END-IF.
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO ARQSIGBS                                     *
      *----------------------------------------------------------------*
       1100-GRAVAR-ARQSIGBS            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-ARQSIGBS           FROM GFCTWBBD-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0600-TESTAR-FS-ARQSIGBS.

           ADD 1                       TO ACU-GRAVA-ARQSIGBS.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------
      *----------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                        *
      *----------------------------------------------------------------*
       1200-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQSIGBE
                 ARQSIGBS.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQUIVOS.
           PERFORM 1300-EMITIR-DISPLAY.

           STOP RUN.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1300-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '******************** GFCT4456 *******************'.
           DISPLAY '*                                               *'.

           MOVE ACU-LIDOS-ARQSIGBE      TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM ARQSIGBE .....:' WRK-MASCARA
                   ' *'.

           MOVE ACU-GRAVA-ARQSIGBS      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS NO ARQSIGBS...:' WRK-MASCARA
                   ' *'.
           DISPLAY '*                                               *'.
           DISPLAY '******************** GFCT4456 *******************'.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DE ERRO USANDO A BRAD7100                    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4470'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL  'DB2'

               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
