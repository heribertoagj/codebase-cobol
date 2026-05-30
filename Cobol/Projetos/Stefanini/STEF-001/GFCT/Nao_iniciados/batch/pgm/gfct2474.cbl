      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2474
       AUTHOR MARCUS VINICIUS.
      *================================================================*
      *                 W I P R O   I N F O S E R V E R                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2474                                     *
      *    ANALISTA     : MARCUS VINICIUS - INFOSERVER - GRUPO 38      *
      *    DATA         : FEV/2018                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      IDENTIFICA ADESOES DAS CESTAS JA VENCIDAS.                *
      *       ATUALMENTE SOMENTE CESTAS NEXT - 1184 E 1187             *
      *       ACRESCENTANDO INFORMACAO DO CPF/CNPJ DO CLIENTE          *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ARQDATAS                              I#GFCT06            *
      *      ADESNEXT                              GFCTWE04            *
      *      RTADESAO                              --------            *
      *      ADMESCOR                              I#GFCTSX            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - MODULO TRATAMENTO DE ERRO.                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT ADESNEXT ASSIGN      TO UT-S-ADESNEXT
                      FILE STATUS      IS WRK-FS-ADESNEXT.

           SELECT ARQDATAS ASSIGN      TO UT-S-ARQDATAS
                      FILE STATUS      IS WRK-FS-ARQDATAS.

           SELECT ADMESCOR ASSIGN      TO UT-S-ADMESCOR
                      FILE STATUS      IS WRK-FS-ADMESCOR.

           SELECT RTADESAO ASSIGN      TO UT-S-RTADESAO
                      FILE STATUS      IS WRK-FS-RTADESAO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  ADESOES DAS CESTAS NEXT  - 1184 E 1187               *
      *           ORG.SEQUENCIAL  -  LRECL = 094                       *
      *----------------------------------------------------------------*

       FD  ADESNEXT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'GFCTWE04'.

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO DE DATAS DA ROTINA DE TARIFAS             *
      *              ORG. SEQUENCIAL   -   LRECL = 100                 *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCT06'.

      *----------------------------------------------------------------*
      *   OUTPUT: ADESOES DAS CESTAS NEXT - FORMATO PARA A DIARIA      *
      *           ORG.SEQUENCIAL  -  LRECL = 030                       *
      *----------------------------------------------------------------*

       FD  ADMESCOR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTSX'.

      *----------------------------------------------------------------*
      *   OUTPUT: ADESOES DAS CESTAS NEXT - ENVIADO PARA CEPT          *
      *           ORG.SEQUENCIAL  -  LRECL = 060                       *
      *----------------------------------------------------------------*

       FD  RTADESAO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RTADESAO.
           05  FD-AGENCIA           PIC  9(005).
           05  FD-CONTA             PIC  9(007).
           05  FD-TARIFA-GFCT       PIC  9(005).
           05  FD-TARIFA-LEGADO     PIC  9(005).
           05  FD-CENTRO-CUSTO      PIC  X(004).
           05  FD-DATA-INIC-VIG     PIC  9(008).
           05  FD-DATA-FIM-VIG      PIC  9(008).
           05  FILLER               PIC  X(018).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT2474 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-REG-LIDOS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GFCTB0F0         PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-DATAS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GRAV-AD          PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GRAV-RT          PIC 9(012) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(008)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

           05  WRK-DATA-DB2.
               10  WRK-DIA-DB2         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-MES-DB2         PIC  9(002)         VALUE ZEROES.
               10  FILLER              PIC  X(001)         VALUE '.'.
               10  WRK-ANO-DB2         PIC  9(004)         VALUE ZEROES.

           05  WRK-TARIFA-ATU          PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-TARIFA-ANT          PIC S9(005) COMP-3  VALUE ZEROS.

           05 WRK-FLAG-811             PIC  X(001)         VALUE SPACES.

           05  WRK-CAMPO-5S            PIC S9(005)V        VALUE ZEROS.
           05  WRK-CAMPO-5-SEMS        REDEFINES WRK-CAMPO-5S
                                       PIC  9(005).

           05  WRK-CAMPO-7S            PIC S9(007)V        VALUE ZEROS.
           05  WRK-CAMPO-7-SEMS        REDEFINES WRK-CAMPO-7S
                                       PIC  9(007).

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS PARA PERIODO       *'.
      *---------------------------------------------------------------*

       01  WRK-DATA-INIC-AD            PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INIC-AD-R          REDEFINES WRK-DATA-INIC-AD.
           05  WRK-DIA-INI-AD          PIC  9(002).
           05  WRK-MES-INI-AD          PIC  9(002).
           05  WRK-ANO-INI-AD          PIC  9(004).

       01  WRK-DATA-FIM-AD             PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-FIM-AD-R          REDEFINES WRK-DATA-FIM-AD.
           05  WRK-DIA-FIM-AD          PIC  9(002).
           05  WRK-MES-FIM-AD          PIC  9(002).
           05  WRK-ANO-FIM-AD          PIC  9(004).

       01  WRK-DATA-INIC-AD-VIG        PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-FIM-ADESAO         PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-FIM-R              REDEFINES WRK-DATA-FIM-ADESAO.
           05  WRK-DIA-FIM             PIC  9(002).
           05  WRK-PTO5                PIC  X(001).
           05  WRK-MES-FIM             PIC  9(002).
           05  WRK-PTO6                PIC  X(001).
           05  WRK-ANO-FIM             PIC  9(004).



       01  WRK-DATA-AUX                PIC  9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-AUX.
           05  FILLER                  PIC  9(001).
           05  WRK-DIA-AUX             PIC  9(002).
           05  WRK-MES-AUX             PIC  9(002).
           05  WRK-ANO-AUX             PIC  9(004).

       01  WRK-GFCTSX-DINI             PIC  9(009) COMP-3 VALUE ZEROS.
       01  WRK-GFCTSX-DFIM             PIC  9(009) COMP-3 VALUE ZEROS.

       01  WRK-MES-AUX-COMP            PIC  9(002) COMP-3 VALUE ZEROS.
       01  WRK-ANO-AUX-COMP            PIC  9(004) COMP-3 VALUE ZEROS.

       01  WRK-DATA-INI-PERIODO-ATU.
           03  WRK-DIA-INI-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-INI-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-INI-PERIODO-ATU PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-FIM-PERIODO-ATU.
           03  WRK-DIA-FIM-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-FIM-PERIODO-ATU PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-FIM-PERIODO-ATU PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-AUX0.
           05 WRK-DIA-AUX0             PIC  9(002) VALUE ZEROS.
           05 WRK-P1                   PIC  X(001) VALUE SPACES.
           05 WRK-MES-AUX0             PIC  9(002) VALUE ZEROS.
           05 WRK-P2                   PIC  X(001) VALUE SPACES.
           05 WRK-ANO-AUX0             PIC  9(004) VALUE ZEROS.

       01  WRK-DATA-AUX1-R             PIC 9(009) VALUE ZEROS.
       01  WRK-DATA-AUX1 REDEFINES WRK-DATA-AUX1-R.
           03  FILLER                  PIC  9(001).
           03  WRK-ANO-AUX1            PIC  9(004).
           03  WRK-MES-AUX1            PIC  9(002).
           03  WRK-DIA-AUX1            PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        AREA POOL0025         *'.
      *----------------------------------------------------------------*

       01  WRK-0025-DATA-ENTRADA       PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-DATA-SAIDA         PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-0025-NUMERO-DIAS        PIC S9(005) COMP-3  VALUE ZEROS.

       01  WRK-ERRO-POOL0025.
           05  FILLER                  PIC  X(040)         VALUE
               'ERRO MODULO POOL0025 - RETURN CODE = '.
           05  WRK-ERRO-0025-RET-COD   PIC  X(004)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA FILE-STATUS            *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ADESNEXT             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQDATAS             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ADMESCOR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-RTADESAO             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(007)          VALUE SPACES.
           05  FILLER                  PIC X(007)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(013)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(017)          VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB0F0
           END-EXEC.

           EJECT
           EXEC SQL
             INCLUDE CLIEB007
           END-EXEC.

           EJECT
           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2474   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-VERIFICA-VAZIO.

           PERFORM 1000-PROCESSA-ADESOES-NEXT
                    UNTIL     WRK-FS-ADESNEXT   EQUAL '10'.

           PERFORM 4000-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ADESNEXT
                       ARQDATAS
                OUTPUT ADMESCOR
                       RTADESAO.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-ADESNEXT.

           PERFORM 0350-TESTAR-FS-ARQDATAS.

           PERFORM 0400-TESTAR-FS-ADMESCOR.

           PERFORM 0450-TESTAR-FS-RTADESAO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ADESNEXT                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ADESNEXT         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ADESNEXT          NOT EQUAL '00'
              MOVE 'ADESNEXT'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ADESNEXT     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQDATAS                    *
      *----------------------------------------------------------------*
       0350-TESTAR-FS-ARQDATAS         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQDATAS          NOT EQUAL '00'
              MOVE 'ARQDATAS'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQDATAS     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ADMESCOR                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ADMESCOR         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ADMESCOR          NOT EQUAL '00'
              MOVE 'ADMESCOR'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ADMESCOR     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO RTADESAO                    *
      *----------------------------------------------------------------*
       0450-TESTAR-FS-RTADESAO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-RTADESAO          NOT EQUAL '00'
              MOVE 'RTADESAO'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-RTADESAO     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS            *
      *----------------------------------------------------------------*
       0500-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-ARQUIVO-ADESNEXT.

           IF  WRK-FS-ADESNEXT         EQUAL '10'
               DISPLAY '***************** GFCT2474 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*      ARQUIVO COM CESTAS NEXT ESTA        *'
               DISPLAY '*          VAZIO !!!                       *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2474 *****************'
           END-IF.

           PERFORM 0650-LER-ARQUIVO-ARQDATAS

           IF WRK-FS-ARQDATAS          EQUAL '10'
               DISPLAY '**************** GFCT2474 *****************'
               DISPLAY '*                                         *'
               DISPLAY '*        ARQUIVO DIARIO DE DATAS DO       *'
               DISPLAY '*               TARIFAS VAZIO             *'
               DISPLAY '*                                         *'
               DISPLAY '**************** GFCT2474 *****************'
               MOVE 'ARQDATAS'         TO WRK-NOME-ARQ
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADESNEXT    TO WRK-FS
               MOVE WRK-ERRO-FS        TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO ADESNEXT                                *
      *----------------------------------------------------------------*
       0600-LER-ARQUIVO-ADESNEXT       SECTION.
      *----------------------------------------------------------------*

           READ ADESNEXT.

           IF  WRK-FS-ADESNEXT         EQUAL '10'
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ADESNEXT.

           ADD 1                       TO ACU-REG-LIDOS.

           MOVE  E04-CSERVC-TARIF      TO WRK-TARIFA-ATU.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO ARQDATAS                                *
      *----------------------------------------------------------------*
       0650-LER-ARQUIVO-ARQDATAS       SECTION.
      *----------------------------------------------------------------*

           READ ARQDATAS.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               GO                      TO 0650-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0350-TESTAR-FS-ARQDATAS.

           ADD 1                       TO ACU-REG-DATAS.

           PERFORM 0700-TRATAR-PERIODO.

      *----------------------------------------------------------------*
       0650-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAR  PERIODO  PARA  O  ARQUIVO  ADMESCOR                *
      *----------------------------------------------------------------*
       0700-TRATAR-PERIODO             SECTION.
      *----------------------------------------------------------------*

      *--- DATA INICIO DO PERIODO ATUAL (MES ATUAL)

           MOVE  GFCT06-DPROCM-ATUAL   TO WRK-DATA-DB2.

           MOVE 01                     TO WRK-DIA-DB2

           MOVE WRK-DATA-DB2           TO WRK-DATA-INI-PERIODO-ATU.

      *--- DATA FIM DO PERIODO ATUAL (MES ATUAL)

           MOVE WRK-DIA-DB2            TO WRK-DIA-AUX.
           MOVE WRK-MES-DB2            TO WRK-MES-AUX-COMP.
           MOVE WRK-ANO-DB2            TO WRK-ANO-AUX-COMP.

           IF  WRK-MES-AUX-COMP        EQUAL 12
               MOVE 01                 TO WRK-MES-AUX-COMP
               ADD  1                  TO WRK-ANO-AUX-COMP
           ELSE
               ADD  1                  TO WRK-MES-AUX-COMP
           END-IF.

           MOVE WRK-MES-AUX-COMP       TO WRK-MES-AUX.
           MOVE WRK-ANO-AUX-COMP       TO WRK-ANO-AUX.

           MOVE WRK-DATA-AUX           TO WRK-0025-DATA-ENTRADA.
           MOVE -1                     TO WRK-0025-NUMERO-DIAS.

           CALL 'POOL0025'             USING WRK-0025-DATA-ENTRADA
                                             WRK-0025-NUMERO-DIAS
                                             WRK-0025-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO

               MOVE RETURN-CODE        TO WRK-ERRO-0025-RET-COD
               MOVE WRK-ERRO-POOL0025  TO ERR-TEXTO

               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-0025-DATA-SAIDA    TO WRK-DATA-AUX.
           MOVE WRK-DIA-AUX            TO WRK-DIA-FIM-PERIODO-ATU.
           MOVE WRK-MES-AUX            TO WRK-MES-FIM-PERIODO-ATU.
           MOVE WRK-ANO-AUX            TO WRK-ANO-FIM-PERIODO-ATU.

           MOVE WRK-DATA-INI-PERIODO-ATU TO WRK-DATA-AUX0.
           MOVE WRK-DIA-AUX0             TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX0             TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX0             TO WRK-ANO-AUX1.
           MOVE WRK-DATA-AUX1-R          TO WRK-GFCTSX-DINI.

           MOVE WRK-DATA-FIM-PERIODO-ATU TO WRK-DATA-AUX0.
           MOVE WRK-DIA-AUX0             TO WRK-DIA-AUX1.
           MOVE WRK-MES-AUX0             TO WRK-MES-AUX1.
           MOVE WRK-ANO-AUX0             TO WRK-ANO-AUX1.
           MOVE WRK-DATA-AUX1-R          TO WRK-GFCTSX-DFIM.

           MOVE WRK-DATA-INI-PERIODO-ATU TO WRK-DATA-AUX0.
           MOVE WRK-DIA-AUX0             TO WRK-DIA-INI-AD.
           MOVE WRK-MES-AUX0             TO WRK-MES-INI-AD.
           MOVE WRK-ANO-AUX0             TO WRK-ANO-INI-AD.
           MOVE WRK-DATA-INIC-AD         TO WRK-DATA-INIC-AD-VIG.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       1000-PROCESSA-ADESOES-NEXT      SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TARIFA-ATU          NOT EQUAL   WRK-TARIFA-ANT
               PERFORM 2000-SELECT-GFCTB0F0
               MOVE  WRK-TARIFA-ATU    TO  WRK-TARIFA-ANT
           END-IF.

           PERFORM 3000-GRAVA-REGISTRO.

           PERFORM 0600-LER-ARQUIVO-ADESNEXT.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     OBTEM TARIFA CORRELATA                                     *
      *----------------------------------------------------------------*
       2000-SELECT-GFCTB0F0            SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-TARIFA-ATU           TO CSERVC-TARIF OF GFCTB0F0.
           MOVE 'SCMP'                   TO CSIST-CORRL  OF GFCTB0F0.

           EXEC SQL
             SELECT
                   CSIST_CORRL,
                   CSERVC_TARIF_CORRL,
                   CSERVC_TARIF
             INTO
                   :GFCTB0F0.CSIST-CORRL,
                   :GFCTB0F0.CSERVC-TARIF-CORRL,
                   :GFCTB0F0.CSERVC-TARIF
             FROM   DB2PRD.TARIF_CORRL_SIST
             WHERE
                   CSERVC_TARIF   = :GFCTB0F0.CSERVC-TARIF      AND
                   CSIST_CORRL    = :GFCTB0F0.CSIST-CORRL
           END-EXEC.

           IF (SQLCODE NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'TARIF_CORRL_SIST'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0010'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL -811
               IF WRK-FLAG-811 NOT EQUAL 'S'
                   DISPLAY '-811 - REGISTRO DUPLICADO'
                   DISPLAY 'CSERVC-TARIF = ' CSERVC-TARIF OF GFCTB0F0
                   MOVE 'S'                TO WRK-FLAG-811
               END-IF
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO CSERVC-TARIF-CORRL OF GFCTB0F0
               MOVE SPACES             TO CSIST-CORRL        OF GFCTB0F0
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO SAIDA                                       *
      *----------------------------------------------------------------*
       3000-GRAVA-REGISTRO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-GRAVA-RTADESAO.

           PERFORM 3200-GRAVA-ADMESCOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO RTADESAO                                    *
      *----------------------------------------------------------------*
       3100-GRAVA-RTADESAO             SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO WRK-CAMPO-5S.
           MOVE WRK-CAMPO-5-SEMS       TO FD-AGENCIA.
           MOVE E04-CCTA-CLI           TO WRK-CAMPO-7S.
           MOVE WRK-CAMPO-7-SEMS       TO FD-CONTA.
           MOVE E04-CSERVC-TARIF       TO WRK-CAMPO-5S.
           MOVE WRK-CAMPO-5-SEMS       TO FD-TARIFA-GFCT.

           MOVE CSERVC-TARIF-CORRL OF GFCTB0F0
                                       TO WRK-CAMPO-5S.
           MOVE WRK-CAMPO-5-SEMS       TO FD-TARIFA-LEGADO.
           MOVE CSIST-CORRL        OF GFCTB0F0
                                       TO FD-CENTRO-CUSTO.

           MOVE WRK-DATA-INIC-AD-VIG   TO FD-DATA-INIC-VIG.

           MOVE E04-DFIM-ADSAO-INDVD   TO WRK-DATA-FIM-ADESAO.
           MOVE WRK-DIA-FIM            TO WRK-DIA-FIM-AD.
           MOVE WRK-MES-FIM            TO WRK-MES-FIM-AD.
           MOVE WRK-ANO-FIM            TO WRK-ANO-FIM-AD.
           MOVE WRK-DATA-FIM-AD        TO FD-DATA-FIM-VIG.

           WRITE FD-RTADESAO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0450-TESTAR-FS-RTADESAO.

           ADD 1                       TO ACU-REG-GRAV-RT.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO ADMESCOR                                    *
      *----------------------------------------------------------------*
       3200-GRAVA-ADMESCOR             SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO WRK-CAMPO-5S.
           MOVE WRK-CAMPO-5-SEMS       TO GFCTSX-AGENCIA.
           MOVE E04-CCTA-CLI           TO WRK-CAMPO-7S.
           MOVE WRK-CAMPO-7-SEMS       TO GFCTSX-CONTA.

           PERFORM 3220-OBTER-POSTO.

           IF  E04-CCTA-CLI            GREATER 999999
               MOVE CPOSTO-SERVC OF CLIEB007
                                       TO WRK-CAMPO-5S
               MOVE WRK-CAMPO-5-SEMS   TO GFCTSX-PAB
           ELSE
               MOVE CPOSTO-SERVC OF CLIEV008
                                       TO WRK-CAMPO-5S
               MOVE WRK-CAMPO-5-SEMS   TO GFCTSX-PAB
           END-IF.

           MOVE E04-CSERVC-TARIF       TO  WRK-CAMPO-5S.
           MOVE WRK-CAMPO-5-SEMS       TO  GFCTSX-PACOTE

           MOVE WRK-GFCTSX-DINI        TO  GFCTSX-DINI.
           MOVE WRK-GFCTSX-DFIM        TO  GFCTSX-DFIM.

           MOVE ZEROS                  TO  GFCTSX-AGPTO.
           MOVE SPACES                 TO  GFCTSX-VISUALIZA.

           WRITE REG-ADESCOMP.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ADMESCOR.

           ADD 1                       TO ACU-REG-GRAV-AD.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O POSTO BANCARIO                               *
      ******************************************************************
      *----------------------------------------------------------------*
       3220-OBTER-POSTO                SECTION.
      *----------------------------------------------------------------*

           IF E04-CCTA-CLI             GREATER 999999
              PERFORM 3230-BUSCAR-CLIEB007
           ELSE
              PERFORM 3240-BUSCAR-CLIEV008
           END-IF.

      *----------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCA POSTO NA TABELA CLIEB007.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3230-BUSCAR-CLIEB007            SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO CJUNC-DEPDC       OF CLIEB007.
           MOVE E04-CCTA-CLI           TO CCTA-CLI          OF CLIEB007.

           EXEC SQL
             SELECT
                   CPOSTO_SERVC
             INTO
                   :CLIEB007.CPOSTO-SERVC
             FROM   DB2PRD.CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEB007.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEB007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )    OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0020'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100 )
              MOVE  ZEROS  TO  CPOSTO-SERVC OF CLIEB007
           END-IF.

      *----------------------------------------------------------------*
       3230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCA POSTO NA TABELA CLIEV008.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3240-BUSCAR-CLIEV008            SECTION.
      *----------------------------------------------------------------*

           MOVE E04-CJUNC-DEPDC        TO CJUNC-DEPDC       OF CLIEV008.
           MOVE E04-CCTA-CLI           TO CCTA-CLI          OF CLIEV008.

           EXEC SQL
             SELECT
                   CPOSTO_SERVC
             INTO
                   :CLIEV008.CPOSTO-SERVC
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC        AND
                   CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )    OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0030'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100 )
              MOVE  ZEROS  TO  CPOSTO-SERVC OF CLIEV008
           END-IF.

      *----------------------------------------------------------------*
       3240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       4000-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 4500-EMITIR-DISPLAY-TOTAIS.

           CLOSE ADESNEXT
                 ARQDATAS
                 ADMESCOR
                 RTADESAO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DE PROCESSAMENTO
      *----------------------------------------------------------------*
       4500-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '                                               '.
           DISPLAY '***************** GFCT2474 ********************'.
           DISPLAY '*                                             *'.
           DISPLAY '*            PROGRAMA GFCT2474                *'.
           DISPLAY '*                                             *'.
           DISPLAY '*---------------------------------------------*'.

           IF  ACU-REG-LIDOS           EQUAL ZEROS
               DISPLAY '*                                             *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO             *'
           ELSE
               DISPLAY '*                                             *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO COM SUCESSO       *'
               DISPLAY '*                                             *'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*                                             *'

               MOVE ACU-REG-DATAS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS ARQDATAS.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-LIDOS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS ADESNEXT.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GFCTB0F0       TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS GFCTB0F0.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GRAV-AD        TO WRK-MASCARA

               DISPLAY '*  REGISTROS GRAV. ADMESCOR.: ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GRAV-RT        TO WRK-MASCARA

               DISPLAY '*  REGISTROS GRAV. RTADESAO.: ' WRK-MASCARA
                                                               ' *'
           END-IF.

           DISPLAY '*                                             *'.
           DISPLAY '***************** GFCT2474 ********************'.

      *----------------------------------------------------------------*
       4500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2474'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL 'APL'
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
           ELSE
               CALL 'BRAD7100'         USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
