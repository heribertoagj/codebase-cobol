      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1645.
       AUTHOR. JEAN ELIAS.
      *================================================================*
      *                     C P M   S I S T E M A S                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1645                                     *
      *    PROGRAMADOR  : JEAN ELIAS                     - CPM/FPOLIS  *
      *    ANALISTA CPM : ALEXANDRE PEREIRA              - CPM/FPOLIS  *
      *    ANALISTA     : MARIA V. TORQUATO - PROCWORK   - GRUPO 50    *
      *    DATA         : 01/02/2007                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      LE EVENTOS SUSPENSOS (SUSPENSO) COM AGRUPAMENTOS, SE      *
      *      SUSPENSAO DA COBRANCA DA TARIFA POR AGRUPAMENTO, ESTA     *
      *      LIBERADA (ARQTB0E7), GRAVA REGISTRO AGENDADO (AGENDADO)   *
      *      SENAO DESPREZA O REGISTRO.                                *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ARQTB0E7                              I#GFCTYK            *
      *      SUSPENSO                              I#GFCTYL            *
      *      AGENDADO                              I#GFCTYJ            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.SUSP_TARIF_CTA                 GFCTB0E8            *
      *      DB2PRD.SUSP_TARIF_AGPTO               GFCTB0E7            *
      *      DB2PRD.SUSP_GRP_CLI                   GFCTB0E3            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - MODULO TRATAMENTO DE ERRO.                     *
      *      POOL1050 - CONEXAO COM DB2                                *
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

           SELECT ARQTB0E7 ASSIGN      TO UT-S-ARQTB0E7
                      FILE STATUS      IS WRK-FS-ARQTB0E7.

           SELECT SUSPENSO ASSIGN      TO UT-S-SUSPENSO
                      FILE STATUS      IS WRK-FS-SUSPENSO.

           SELECT AGENDADO ASSIGN      TO UT-S-AGENDADO
                      FILE STATUS      IS WRK-FS-AGENDADO.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  TARIFAS LIBERADAS EM D+1 RECEBIDO                    *
      *           ORG.SEQUENCIAL  -  LRECL = 067                       *
      *----------------------------------------------------------------*

       FD  ARQTB0E7
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQTB0E7             PIC X(067).

      *----------------------------------------------------------------*
      *   INPUT:  REGISTRO EVENTO SUSPENSOS GFCTB092                   *
      *           ORG.SEQUENCIAL  -  LRECL = 093                       *
      *----------------------------------------------------------------*

       FD  SUSPENSO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-SUSPENSO             PIC X(093).

      *----------------------------------------------------------------*
      *   OUTPUT: REGISTROS EVENTO LIBERADO E AGENDADOS                *
      *           ORG.SEQUENCIAL  -  LRECL = 073                       *
      *----------------------------------------------------------------*

       FD  AGENDADO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-AGENDADO             PIC X(073).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT1645 *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-BATCH                PIC  X(008)         VALUE
              'BATCH'.
           05 WRK-MASCARA              PIC ZZZZ.ZZZ.ZZ9    VALUE ZEROS.
           05 WRK-COUNT                PIC S9(009) COMP    VALUE ZEROS.
           05 WRK-FIM                  PIC  X(001)         VALUE SPACES.
           05 WRK-REGISTRO-DESPREZADO  PIC  X(001)         VALUE SPACES.
           05 WRK-REGISTRO-AGENDADO    PIC  X(001)         VALUE SPACES.
           05 WRK-ACHOU                PIC  X(001)         VALUE SPACES.
           05 WRK-IND                  PIC  9(009)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-LIDOS-SUSPENSO       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-LIDOS-ARQTB0E7       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-GRAVA-AGENDADO       PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-DESPREZADOS          PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-IND04                PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-IND14                PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-IND15                PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-IND18                PIC 9(009) COMP-3   VALUE ZEROS.
           05 ACU-IND19                PIC 9(009) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQTB0E7             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SUSPENSO             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-AGENDADO             PIC  X(002)         VALUE SPACES.
       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01 WRK-LEITURA                  PIC  X(013)         VALUE
          ' NA  LEITURA '.
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
       01  FILLER                      PIC X(032)          VALUE
           '*     VARIAVEIS AUX DATA       *'.
      *----------------------------------------------------------------*

       01  WRK-DATAS.
           05  WRK-DATA-DB2.
             10 WRK-DD-DB2             PIC 9(02)           VALUE ZEROS.
             10 FILLER                 PIC X(01)           VALUE SPACES.
             10 WRK-MM-DB2             PIC 9(02)           VALUE ZEROS.
             10 FILLER                 PIC X(01)           VALUE SPACES.
             10 WRK-AAAA-DB2           PIC 9(04)           VALUE ZEROS.
           05  WRK-DINIC-INV           PIC 9(08)           VALUE ZEROS.
           05  FILLER   REDEFINES      WRK-DINIC-INV.
             10 WRK-ANO-DINIC          PIC 9(04).
             10 WRK-MES-DINIC          PIC 9(02).
             10 WRK-DIA-DINIC          PIC 9(02).
           05  WRK-DFIM-INV            PIC 9(08)           VALUE ZEROS.
           05  FILLER   REDEFINES      WRK-DFIM-INV.
             10 WRK-ANO-DFIM           PIC 9(04).
             10 WRK-MES-DFIM           PIC 9(02).
             10 WRK-DIA-DFIM           PIC 9(02).
           05  WRK-DOCOR-INV           PIC 9(08)           VALUE ZEROS.
           05  FILLER   REDEFINES      WRK-DOCOR-INV.
             10 WRK-ANO-DOCOR          PIC 9(04).
             10 WRK-MES-DOCOR          PIC 9(02).
             10 WRK-DIA-DOCOR          PIC 9(02).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA TABELA INTERNA CLIENTE  *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA04.
           05 TAB-TABELA04-RED         OCCURS 200 TIMES.
               10 TAB-TB04-TARIFA      PIC S9(005) COMP-3  VALUE ZEROS.
ST2506*        10 TAB-TB04-CGC         PIC S9(009) COMP-3  VALUE ZEROS.
ST2506*        10 TAB-TB04-FILIAL      PIC S9(005) COMP-3  VALUE ZEROS.
ST2506         10 TAB-TB04-CGC         PIC X(009)          VALUE SPACES.
ST2506         10 TAB-TB04-FILIAL      PIC X(004)          VALUE SPACES.
               10 TAB-TB04-CONTROLE    PIC  X(002)         VALUE SPACES.
               10 TAB-TB04-DATA-INIC   PIC S9(008) COMP-3  VALUE ZEROS.
               10 TAB-TB04-DATA-FIM    PIC S9(008) COMP-3  VALUE ZEROS.
               10 TAB-TB04-DATA-COBR   PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA TABELA INTERNA AGENCIA  *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA14.
           05 TAB-TABELA14-RED OCCURS 200 TIMES.
               10 TAB-TB14-TARIFA      PIC S9(005) COMP-3  VALUE ZEROS.
               10 TAB-TB14-EMPRESA     PIC S9(005) COMP-3  VALUE ZEROS.
               10 TAB-TB14-AGENCIA     PIC S9(005) COMP-3  VALUE ZEROS.
               10 TAB-TB14-DATA-INIC   PIC S9(008) COMP-3  VALUE ZEROS.
               10 TAB-TB14-DATA-FIM    PIC S9(008) COMP-3  VALUE ZEROS.
               10 TAB-TB14-TODAS-AG    PIC  X(001)         VALUE SPACES.
               10 TAB-TB14-DATA-COBR   PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA TABELA INTERNA POSTO    *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA15.
           05 TAB-TABELA15-RED         OCCURS 200 TIMES.
              10 TAB-TB15-TARIFA       PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB15-EMPRESA      PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB15-AGENCIA      PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB15-POSTO        PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB15-DATA-INIC    PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB15-DATA-FIM     PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB15-DATA-COBR    PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA TABELA INTERNA MUNICIPIO*'.
      *----------------------------------------------------------------*

       01  TAB-TABELA18.
           05 TAB-TABELA18-RED         OCCURS 200 TIMES.
              10 TAB-TB18-TARIFA       PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB18-MUNICIPIO    PIC S9(007) COMP-3  VALUE ZEROS.
              10 TAB-TB18-DATA-INIC    PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB18-DATA-FIM     PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB18-DATA-COBR    PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* AREA TABELA INTERNA UF       *'.
      *----------------------------------------------------------------*

       01  TAB-TABELA19.
           05 WRK-TABELA19-RED         OCCURS 200 TIMES.
              10 TAB-TB19-TARIFA       PIC S9(005) COMP-3  VALUE ZEROS.
              10 TAB-TB19-UF           PIC  X(002)         VALUE SPACES.
              10 TAB-TB19-DATA-INIC    PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB19-DATA-FIM     PIC S9(008) COMP-3  VALUE ZEROS.
              10 TAB-TB19-DATA-COBR    PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*       AREA TO POOL7100*'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    REGISTRO DE ARQTB0E7      *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTYK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    REGISTRO DE SUSPENSO      *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTYL'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    REGISTRO DE AGENDADO      *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTYJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA DE TABELAS DB2         *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0E3
           END-EXEC.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0600-PROCESSAR-PRINCIPAL.

           PERFORM 2300-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQTB0E7
                       SUSPENSO
                OUTPUT AGENDADO.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           INITIALIZE  TAB-TABELA04.

           INITIALIZE  TAB-TABELA14.

           INITIALIZE  TAB-TABELA15.

           INITIALIZE  TAB-TABELA18.

           INITIALIZE  TAB-TABELA19.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-ARQTB0E7.

           PERFORM 0400-TESTAR-FS-SUSPENSO.

           PERFORM 0500-TESTAR-FS-AGENDADO.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQTB0E7                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQTB0E7         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQTB0E7          NOT EQUAL '00'
              MOVE 'ARQTB0E7'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQTB0E7     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO SUSPENSO                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-SUSPENSO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-SUSPENSO          NOT EQUAL '00'
              MOVE 'SUSPENSO'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-SUSPENSO     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO AGENDADO                    *
      *----------------------------------------------------------------*
       0500-TESTAR-FS-AGENDADO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-AGENDADO          NOT EQUAL '00'
              MOVE 'AGENDADO'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-AGENDADO     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO                                              *
      *----------------------------------------------------------------*
       0600-PROCESSAR-PRINCIPAL        SECTION.
      *----------------------------------------------------------------*

           PERFORM 0700-MONTA-TAB-TARIFA-LIBERADA
                                       UNTIL
                               WRK-FIM EQUAL 'F'.

           MOVE 'N'                    TO WRK-FIM.

           PERFORM 1300-LER-SUSPENSO.

           PERFORM 1400-PROCESSAMENTO  UNTIL
                               WRK-FIM EQUAL 'F'.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     MONTAGEM TABELA TARIFA LIBERADA ARQTB0E7                   *
      *----------------------------------------------------------------*
       0700-MONTA-TAB-TARIFA-LIBERADA  SECTION.
      *----------------------------------------------------------------*

           READ ARQTB0E7               INTO GFCTYK-GFCTB0E7.

           IF  WRK-FS-ARQTB0E7         EQUAL '10'
               MOVE 'F'                TO WRK-FIM
               GO                      TO 0700-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQTB0E7.

           ADD 1                       TO ACU-LIDOS-ARQTB0E7.

           MOVE GFCTYK-DINIC-SUSP-COBR TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DINIC.
           MOVE WRK-MM-DB2             TO WRK-MES-DINIC.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DINIC.
           MOVE GFCTYK-DFIM-SUSP-COBR  TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DFIM.
           MOVE WRK-MM-DB2             TO WRK-MES-DFIM.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DFIM.

           EVALUATE GFCTYK-CAGPTO-CTA
               WHEN 4
                   PERFORM 0800-CARREGA-GRUPO-CLIENTE

               WHEN 14
                   PERFORM 0900-CARREGA-GRUPO-AGENCIA

               WHEN 15
                   PERFORM 1000-CARREGA-GRUPO-PAB

               WHEN 18
                   PERFORM 1100-CARREGA-GRUPO-MUNICIPIO

               WHEN 19
                   PERFORM 1200-CARREGA-GRUPO-UF
           END-EVALUATE.

      *----------------------------------------------------------------*
       0700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CARREGA TABELA INTERNA AGRUPAMENTO CLIENTE                 *
      *----------------------------------------------------------------*
       0800-CARREGA-GRUPO-CLIENTE      SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO ACU-IND04.

            IF ACU-IND04               GREATER 200
               DISPLAY '************** GFCT1645 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*   ESTOUROU TABELA INTERNA CLIENTE   *'
               DISPLAY '*        PROCESSAMENTO CANCELADO      *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT1645 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               STRING 'ESTOUROU TABELA INTERNA CLIENTE'
                      ' - PROCESSAMENTO CANCELADO'
               DELIMITED BY SIZE       INTO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE GFCTYK-CSERVC-TARIF    TO TAB-TB04-TARIFA(ACU-IND04).
           MOVE GFCTYK-CCGC-CPF        TO TAB-TB04-CGC(ACU-IND04).
           MOVE GFCTYK-CFLIAL-CGC      TO TAB-TB04-FILIAL(ACU-IND04).
           MOVE GFCTYK-CCTRL-CPF-CGC   TO TAB-TB04-CONTROLE(ACU-IND04).
           MOVE WRK-DINIC-INV          TO TAB-TB04-DATA-INIC(ACU-IND04).
           MOVE WRK-DFIM-INV           TO TAB-TB04-DATA-FIM (ACU-IND04).
           MOVE GFCTYK-DCOBR-SUSP-LIBRD
                                       TO TAB-TB04-DATA-COBR(ACU-IND04).

      *----------------------------------------------------------------*
       0800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CARREGA TABELA INTERNA AGRUPAMENTO AGENCIA                 *
      *----------------------------------------------------------------*
       0900-CARREGA-GRUPO-AGENCIA      SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO ACU-IND14.

            IF ACU-IND14               GREATER 200
               DISPLAY '************** GFCT1645 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*   ESTOUROU TABELA INTERNA AGENCIA   *'
               DISPLAY '*        PROCESSAMENTO CANCELADO      *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT1645 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               STRING 'ESTOUROU TABELA INTERNA AGENCIA'
                      ' - PROCESSAMENTO CANCELADO'
               DELIMITED BY SIZE       INTO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE GFCTYK-CSERVC-TARIF    TO TAB-TB14-TARIFA(ACU-IND14).
           MOVE GFCTYK-CEMPR-INC       TO TAB-TB14-EMPRESA(ACU-IND14).
           MOVE GFCTYK-CDEPDC          TO TAB-TB14-AGENCIA(ACU-IND14).
           MOVE WRK-DINIC-INV          TO TAB-TB14-DATA-INIC(ACU-IND14).
           MOVE WRK-DFIM-INV           TO TAB-TB14-DATA-FIM(ACU-IND14).
           MOVE GFCTYK-CINDCD-AGPTO-TOT
                                       TO TAB-TB14-TODAS-AG(ACU-IND14).
           MOVE GFCTYK-DCOBR-SUSP-LIBRD
                                       TO TAB-TB14-DATA-COBR(ACU-IND14).

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CARREGA TABELA INTERNA AGRUPAMENTO PAB                     *
      *----------------------------------------------------------------*
       1000-CARREGA-GRUPO-PAB          SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO ACU-IND15.

            IF ACU-IND15               GREATER 200
               DISPLAY '************** GFCT1645 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*     ESTOUROU TABELA INTERNA PAB     *'
               DISPLAY '*       PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT1645 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               STRING 'ESTOUROU TABELA INTERNA PAB'
                      ' - PROCESSAMENTO CANCELADO'
               DELIMITED BY SIZE       INTO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE GFCTYK-CSERVC-TARIF    TO TAB-TB15-TARIFA(ACU-IND15).
           MOVE GFCTYK-CEMPR-INC       TO TAB-TB15-EMPRESA(ACU-IND15).
           MOVE GFCTYK-CDEPDC          TO TAB-TB15-AGENCIA(ACU-IND15).
           MOVE GFCTYK-CPOSTO-SERVC    TO TAB-TB15-POSTO(ACU-IND15).
           MOVE WRK-DINIC-INV          TO TAB-TB15-DATA-INIC(ACU-IND15).
           MOVE WRK-DFIM-INV           TO TAB-TB15-DATA-FIM(ACU-IND15).
           MOVE GFCTYK-DCOBR-SUSP-LIBRD
                                       TO TAB-TB15-DATA-COBR(ACU-IND15).

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CARREGA TABELA INTERNA AGRUPAMENTO MUNICIPIO               *
      *----------------------------------------------------------------*
       1100-CARREGA-GRUPO-MUNICIPIO    SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO ACU-IND18.

            IF ACU-IND18               GREATER 200
               DISPLAY '************** GFCT1645 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*  ESTOUROU TABELA INTERNA MUNICIPIO  *'
               DISPLAY '*       PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT1645 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               STRING 'ESTOUROU TABELA INTERNA MUNICIPIO'
                      ' - PROCESSAMENTO CANCELADO'
               DELIMITED BY SIZE       INTO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE GFCTYK-CSERVC-TARIF    TO TAB-TB18-TARIFA(ACU-IND18).
           MOVE GFCTYK-CMUN-IBGE       TO TAB-TB18-MUNICIPIO(ACU-IND18).
           MOVE WRK-DINIC-INV          TO TAB-TB18-DATA-INIC(ACU-IND18).
           MOVE WRK-DFIM-INV           TO TAB-TB18-DATA-FIM(ACU-IND18).
           MOVE GFCTYK-DCOBR-SUSP-LIBRD
                                       TO TAB-TB18-DATA-COBR(ACU-IND18).

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CARREGA TABELA INTERNA AGRUPAMENTO  UF                     *
      *----------------------------------------------------------------*
       1200-CARREGA-GRUPO-UF           SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO ACU-IND19.

            IF ACU-IND19               GREATER 200
               DISPLAY '************** GFCT1645 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*     ESTOUROU TABELA INTERNA  UF     *'
               DISPLAY '*       PROCESSAMENTO CANCELADO       *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT1645 ***************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               STRING 'ESTOUROU TABELA INTERNA UF'
                      ' - PROCESSAMENTO CANCELADO'
               DELIMITED BY SIZE       INTO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE GFCTYK-CSERVC-TARIF    TO TAB-TB19-TARIFA(ACU-IND19).
           MOVE GFCTYK-CSGL-UF         TO TAB-TB19-UF(ACU-IND19).
           MOVE WRK-DINIC-INV          TO TAB-TB19-DATA-INIC(ACU-IND19).
           MOVE WRK-DFIM-INV           TO TAB-TB19-DATA-FIM(ACU-IND19).
           MOVE GFCTYK-DCOBR-SUSP-LIBRD
                                       TO TAB-TB19-DATA-COBR(ACU-IND19).

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO SUSPENSO                                *
      *----------------------------------------------------------------*
       1300-LER-SUSPENSO               SECTION.
      *----------------------------------------------------------------*

           READ SUSPENSO               INTO GFCTYL-GFCTB092.

           IF  WRK-FS-SUSPENSO         EQUAL '10'
               MOVE 'F'                TO WRK-FIM
               GO                      TO 1300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-SUSPENSO.

           ADD 1                       TO ACU-LIDOS-SUSPENSO.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO                                              *
      *----------------------------------------------------------------*
       1400-PROCESSAMENTO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-REGISTRO-DESPREZADO.
           MOVE 'N'                    TO WRK-REGISTRO-AGENDADO.
           MOVE 'F'                    TO WRK-ACHOU.

           PERFORM 1500-PESQUISA-TAB-CLIENTE.

           IF  WRK-REGISTRO-DESPREZADO
                                       EQUAL 'N'
           AND WRK-REGISTRO-AGENDADO   EQUAL 'N'
               PERFORM 1800-PESQUISA-TAB-AGENCIA
           END-IF.

           IF  WRK-REGISTRO-DESPREZADO
                                       EQUAL 'N'
           AND WRK-REGISTRO-AGENDADO   EQUAL 'N'
               PERFORM 2000-PESQUISA-TAB-POSTO
           END-IF.

           IF  WRK-REGISTRO-DESPREZADO
                                       EQUAL 'N'
           AND WRK-REGISTRO-AGENDADO   EQUAL 'N'
               PERFORM 2100-PESQUISA-TAB-MUNI
           END-IF.

           IF  WRK-REGISTRO-DESPREZADO
                                       EQUAL 'N'
           AND WRK-REGISTRO-AGENDADO   EQUAL 'N'
               PERFORM 2200-PESQUISA-TAB-UF
           END-IF.

           IF  WRK-REGISTRO-DESPREZADO
                                       EQUAL 'S'
               ADD 1                   TO ACU-DESPREZADOS
           END-IF.

           PERFORM 1300-LER-SUSPENSO.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA TABELA DE CLIENTES                                *
      *----------------------------------------------------------------*
       1500-PESQUISA-TAB-CLIENTE       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-IND04
                                          WRK-IND.

           MOVE GFCTYL-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DOCOR.
           MOVE WRK-MM-DB2             TO WRK-MES-DOCOR.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DOCOR.

           PERFORM VARYING ACU-IND04   FROM 1 BY 1 UNTIL
               ACU-IND04               GREATER 200 OR
               TAB-TB04-TARIFA(ACU-IND04)
                                       EQUAL ZEROS
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB04-TARIFA(ACU-IND04)
               AND GFCTYL-CPF-CNPJ     EQUAL TAB-TB04-CGC(ACU-IND04)
               AND GFCTYL-FILIAL       EQUAL TAB-TB04-FILIAL(ACU-IND04)
               AND GFCTYL-CONTROLE     EQUAL
                                         TAB-TB04-CONTROLE(ACU-IND04)
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB04-DATA-INIC(ACU-IND04)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB04-DATA-FIM(ACU-IND04)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND04      TO WRK-IND
                   MOVE 999            TO ACU-IND04
             END-IF
           END-PERFORM.

           IF  WRK-ACHOU               EQUAL 'V'
               PERFORM 1600-PESQUISA-SUSP-AGENCIA
               IF  WRK-COUNT           EQUAL ZEROS
                   MOVE WRK-IND        TO ACU-IND04
                   MOVE 02             TO GFCTYJ-CSIT-EVNTO-RECBD
                   MOVE TAB-TB04-DATA-COBR(ACU-IND04)
                                       TO GFCTYJ-DAGNDA-DEB-MOVTO
                   PERFORM 1700-GRAVA-AGENDADO
                   MOVE 'S'            TO WRK-REGISTRO-AGENDADO
               ELSE
                   MOVE 'S'            TO WRK-REGISTRO-DESPREZADO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA SUSPENSAO POR AGENCIA                             *
      *----------------------------------------------------------------*
       1600-PESQUISA-SUSP-AGENCIA      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYL-CAG-DSTNO-MOVTO TO CJUNC-DEPDC  OF GFCTB0E8.

           MOVE GFCTYL-CCTA-DSTNO-MOVTO
                                       TO CCTA-CLI     OF GFCTB0E8.

           MOVE GFCTYL-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0E8.

           MOVE GFCTYL-DOCOR-EVNTO     TO DINIC-SUSP-COBR
                                                       OF GFCTB0E8.

           MOVE GFCTYL-DOCOR-EVNTO     TO DFIM-SUSP-COBR
                                                       OF GFCTB0E8.

           MOVE 0                      TO WRK-COUNT.

           EXEC SQL
               SELECT
                     COUNT(*)
               INTO
                     :WRK-COUNT
               FROM  DB2PRD.SUSP_TARIF_CTA
               WHERE CJUNC_DEPDC       = :GFCTB0E8.CJUNC-DEPDC
               AND   CCTA_CLI          = :GFCTB0E8.CCTA-CLI
               AND   CSERVC_TARIF      = :GFCTB0E8.CSERVC-TARIF
               AND   ((DINIC_SUSP_COBR = :GFCTB0E8.DINIC-SUSP-COBR)
               OR    (DINIC_SUSP_COBR  < :GFCTB0E8.DINIC-SUSP-COBR))
               AND   ((DFIM_SUSP_COBR  = :GFCTB0E8.DFIM-SUSP-COBR)
               OR    (DFIM_SUSP_COBR   > :GFCTB0E8.DFIM-SUSP-COBR))
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS)
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_CTA'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0001'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVA REGISTRO AGENDADOS                                    *
      *----------------------------------------------------------------*
       1700-GRAVA-AGENDADO             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTYL-DENVIO-MOVTO-TARIF
                                       TO GFCTYJ-DENVIO-MOVTO-TARIF.
           MOVE GFCTYL-CROTNA-ORIGE-MOVTO
                                       TO GFCTYJ-CROTNA-ORIGE-MOVTO.
           MOVE GFCTYL-CNRO-ARQ-MOVTO  TO GFCTYJ-CNRO-ARQ-MOVTO.
           MOVE GFCTYL-CSEQ-MOVTO      TO GFCTYJ-CSEQ-MOVTO.
           MOVE GFCTYL-CSERVC-TARIF    TO GFCTYJ-CSERVC-TARIF.
           MOVE GFCTYL-CTPO-CTA-MOVTO  TO GFCTYJ-CTPO-CTA-MOVTO.
           MOVE GFCTYL-CBCO-DSTNO-MOVTO
                                       TO GFCTYJ-CBCO-DSTNO-MOVTO.
           MOVE GFCTYL-CAG-DSTNO-MOVTO TO GFCTYJ-CAG-DSTNO-MOVTO.
           MOVE GFCTYL-CCTA-DSTNO-MOVTO
                                       TO GFCTYJ-CCTA-DSTNO-MOVTO.
           MOVE GFCTYL-CPAB-DSTNO-MOVTO
                                       TO GFCTYJ-CPAB-DSTNO-MOVTO.
           MOVE GFCTYL-DEFETV-DEB-MOVTO
                                       TO GFCTYJ-DEFETV-DEB-MOVTO.
           MOVE GFCTYL-DOCOR-EVNTO     TO GFCTYJ-DOCOR-EVNTO.

           WRITE FD-REG-AGENDADO       FROM GFCTYJ-GFCTB092.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 0500-TESTAR-FS-AGENDADO.

           ADD 1                       TO ACU-GRAVA-AGENDADO.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA TABELA DE AGENCIAS                                *
      *----------------------------------------------------------------*
       1800-PESQUISA-TAB-AGENCIA       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-IND14
                                          WRK-IND.

           MOVE GFCTYL-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DOCOR.
           MOVE WRK-MM-DB2             TO WRK-MES-DOCOR.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DOCOR.

           PERFORM VARYING ACU-IND14   FROM 1 BY 1 UNTIL
               ACU-IND14               GREATER 200 OR
               TAB-TB14-TARIFA(ACU-IND14)
                                       EQUAL ZEROS
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB14-TARIFA(ACU-IND14)
               AND TAB-TB14-TODAS-AG(ACU-IND14)
                                       EQUAL 'S'
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB14-DATA-INIC(ACU-IND14)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB14-DATA-FIM(ACU-IND14)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND14      TO WRK-IND
                   MOVE 999            TO ACU-IND14
               ELSE
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB14-TARIFA(ACU-IND14)
               AND GFCTYL-CBCO-DSTNO-MOVTO
                                       EQUAL TAB-TB14-EMPRESA(ACU-IND14)
               AND GFCTYL-CAG-DSTNO-MOVTO
                                       EQUAL TAB-TB14-AGENCIA(ACU-IND14)
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB14-DATA-INIC(ACU-IND14)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB14-DATA-FIM(ACU-IND14)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND14      TO WRK-IND
                   MOVE 999            TO ACU-IND14
               END-IF
               END-IF
           END-PERFORM.

           IF  WRK-ACHOU               EQUAL 'V'
               PERFORM 1600-PESQUISA-SUSP-AGENCIA
               IF  WRK-COUNT           EQUAL ZEROS
                   PERFORM 1900-PESQUISA-SUSP-CLIENTE
                   IF  WRK-COUNT       EQUAL ZEROS
                       MOVE WRK-IND    TO ACU-IND14
                       MOVE 02         TO GFCTYJ-CSIT-EVNTO-RECBD
                       MOVE TAB-TB14-DATA-COBR(ACU-IND14)
                                       TO GFCTYJ-DAGNDA-DEB-MOVTO
                       PERFORM 1700-GRAVA-AGENDADO
                       MOVE 'S'        TO WRK-REGISTRO-AGENDADO
                   ELSE
                       MOVE 'S'        TO WRK-REGISTRO-DESPREZADO
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-REGISTRO-DESPREZADO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA SUSPENSAO POR CLIENTE                             *
      *----------------------------------------------------------------*
       1900-PESQUISA-SUSP-CLIENTE      SECTION.
      *----------------------------------------------------------------*

ST25X6*    MOVE GFCTYL-CPF-CNPJ        TO CCGC-CPF         OF GFCTB0E3.
ST25X6     MOVE GFCTYL-CPF-CNPJ        TO CCGC-CPF-ST      OF GFCTB0E3.

ST25X6*    MOVE GFCTYL-FILIAL          TO CFLIAL-CGC       OF GFCTB0E3.
ST25X6     MOVE GFCTYL-FILIAL          TO CFLIAL-CGC-ST    OF GFCTB0E3.

ST25X6*    MOVE GFCTYL-CONTROLE        TO CCTRL-CPF-CGC    OF GFCTB0E3.
ST25X6     MOVE GFCTYL-CONTROLE        TO CCTRL-CPF-CGC-ST OF GFCTB0E3.

           MOVE GFCTYL-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB0E7.

           MOVE 04                     TO CAGPTO-CTA       OF GFCTB0E7.

           MOVE 'N'                    TO CINDCD-LIBRC-COBR
                                                           OF GFCTB0E7.

           MOVE GFCTYL-DOCOR-EVNTO     TO DINIC-SUSP-COBR  OF GFCTB0E7.

           MOVE GFCTYL-DOCOR-EVNTO     TO DFIM-SUSP-COBR   OF GFCTB0E7.

           MOVE 0                      TO WRK-COUNT.

           EXEC SQL
               SELECT
                     COUNT(*)
               INTO
                     :WRK-COUNT
               FROM  DB2PRD.SUSP_TARIF_AGPTO T1,
                     DB2PRD.SUSP_GRP_CLI T2
               WHERE
                     T1.CAGPTO_CTA      =  T2.CAGPTO_CTA
               AND   T1.CSERVC_TARIF    =  T2.CSERVC_TARIF
               AND   T1.CSEQ_AGPTO_CTA  =  T2.CSEQ_AGPTO_CTA
               AND   T1.DINIC_SUSP_COBR =  T2.DINIC_SUSP_COBR
               AND   T1.CAGPTO_CTA      =  :GFCTB0E7.CAGPTO-CTA
               AND   T1.CSERVC_TARIF    =  :GFCTB0E7.CSERVC-TARIF
               AND   T1.CINDCD_LIBRC_COBR
                                        =  :GFCTB0E7.CINDCD-LIBRC-COBR
ST25X6*        AND   T2.CCGC_CPF        =  :GFCTB0E3.CCGC-CPF
ST25X6*        AND   T2.CFLIAL_CGC      =  :GFCTB0E3.CFLIAL-CGC
ST25X6*        AND   T2.CCTRL_CPF_CGC   =  :GFCTB0E3.CCTRL-CPF-CGC
ST25X6         AND   T2.CCGC_CPF_ST     =  :GFCTB0E3.CCGC-CPF-ST
ST25X6         AND   T2.CFLIAL_CGC_ST   =  :GFCTB0E3.CFLIAL-CGC-ST
ST25X6         AND   T2.CCTRL_CPF_CGC_ST =  :GFCTB0E3.CCTRL-CPF-CGC-ST
               AND ((T1.DINIC_SUSP_COBR <  :GFCTB0E7.DINIC-SUSP-COBR)
               OR   (T1.DINIC_SUSP_COBR =  :GFCTB0E7.DINIC-SUSP-COBR))
               AND ((T1.DFIM_SUSP_COBR  >  :GFCTB0E7.DFIM-SUSP-COBR)
               OR   (T1.DFIM_SUSP_COBR  =  :GFCTB0E7.DFIM-SUSP-COBR))
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS)
                MOVE 'DB2'             TO ERR-TIPO-ACESSO
                MOVE 'SUSP_TARIF_AGPTO'
                                       TO ERR-DBD-TAB
                MOVE 'SELECT'          TO ERR-FUN-COMANDO
                MOVE SQLCODE           TO ERR-SQL-CODE
                MOVE '0002'            TO ERR-LOCAL
                MOVE SPACES            TO ERR-SEGM
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA TABELA DE POSTOS                                  *
      *----------------------------------------------------------------*
       2000-PESQUISA-TAB-POSTO         SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-IND15
                                          WRK-IND.

           MOVE GFCTYL-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DOCOR.
           MOVE WRK-MM-DB2             TO WRK-MES-DOCOR.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DOCOR.

           PERFORM VARYING ACU-IND15   FROM 1 BY 1 UNTIL
               ACU-IND15               GREATER 200 OR
               TAB-TB15-TARIFA(ACU-IND15)
                                       EQUAL ZEROS
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB15-TARIFA(ACU-IND15)
               AND GFCTYL-CBCO-DSTNO-MOVTO
                                       EQUAL TAB-TB15-EMPRESA(ACU-IND15)
               AND GFCTYL-CAG-DSTNO-MOVTO
                                       EQUAL TAB-TB15-AGENCIA(ACU-IND15)
               AND GFCTYL-CPOSTAL      EQUAL TAB-TB15-POSTO(ACU-IND15)
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB15-DATA-INIC(ACU-IND15)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB15-DATA-FIM(ACU-IND15)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND15      TO WRK-IND
                   MOVE 999            TO ACU-IND15
               END-IF
           END-PERFORM.

           IF  WRK-ACHOU               EQUAL 'V'
               PERFORM 1600-PESQUISA-SUSP-AGENCIA
               IF  WRK-COUNT           EQUAL ZEROS
                   PERFORM 1900-PESQUISA-SUSP-CLIENTE
                   IF  WRK-COUNT       EQUAL ZEROS
                       MOVE WRK-IND    TO ACU-IND15
                       MOVE 02         TO GFCTYJ-CSIT-EVNTO-RECBD
                       MOVE TAB-TB15-DATA-COBR(ACU-IND15)
                                       TO GFCTYJ-DAGNDA-DEB-MOVTO
                       PERFORM 1700-GRAVA-AGENDADO
                       MOVE 'S'        TO WRK-REGISTRO-AGENDADO
                   ELSE
                       MOVE 'S'        TO WRK-REGISTRO-DESPREZADO
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-REGISTRO-DESPREZADO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA TABELA DE MUNICIPIOS                              *
      *----------------------------------------------------------------*
       2100-PESQUISA-TAB-MUNI          SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-IND18
                                          WRK-IND.

           MOVE GFCTYL-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DOCOR.
           MOVE WRK-MM-DB2             TO WRK-MES-DOCOR.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DOCOR.

           PERFORM VARYING ACU-IND18   FROM 1 BY 1 UNTIL
               ACU-IND18               GREATER 200 OR
               TAB-TB18-TARIFA(ACU-IND18)
                                       EQUAL ZEROS
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB18-TARIFA(ACU-IND18)
               AND GFCTYL-CMUN-RURAL   EQUAL
                                         TAB-TB18-MUNICIPIO(ACU-IND18)
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB18-DATA-INIC(ACU-IND18)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB18-DATA-FIM(ACU-IND18)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND18      TO WRK-IND
                   MOVE 999            TO ACU-IND18
               END-IF
           END-PERFORM.

           IF  WRK-ACHOU                EQUAL 'V'
               PERFORM 1600-PESQUISA-SUSP-AGENCIA
               IF  WRK-COUNT           EQUAL ZEROS
                   PERFORM 1900-PESQUISA-SUSP-CLIENTE
                   IF  WRK-COUNT       EQUAL ZEROS
                       MOVE WRK-IND    TO ACU-IND18
                       MOVE 02         TO GFCTYJ-CSIT-EVNTO-RECBD
                       MOVE TAB-TB18-DATA-COBR(ACU-IND18)
                                       TO GFCTYJ-DAGNDA-DEB-MOVTO
                       PERFORM 1700-GRAVA-AGENDADO
                       MOVE 'S'        TO WRK-REGISTRO-AGENDADO
                   ELSE
                       MOVE 'S'        TO WRK-REGISTRO-DESPREZADO
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-REGISTRO-DESPREZADO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PESQUISA TABELA DE UF                                      *
      *----------------------------------------------------------------*
       2200-PESQUISA-TAB-UF            SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO ACU-IND19
                                          WRK-IND.

           MOVE GFCTYL-DOCOR-EVNTO     TO WRK-DATA-DB2.
           MOVE WRK-DD-DB2             TO WRK-DIA-DOCOR.
           MOVE WRK-MM-DB2             TO WRK-MES-DOCOR.
           MOVE WRK-AAAA-DB2           TO WRK-ANO-DOCOR.

           PERFORM VARYING ACU-IND19   FROM 1 BY 1 UNTIL
               ACU-IND19               GREATER 200 OR
               TAB-TB19-TARIFA(ACU-IND19)
                                       EQUAL ZEROS
               IF  GFCTYL-CSERVC-TARIF EQUAL TAB-TB19-TARIFA(ACU-IND19)
               AND GFCTYL-CSGL-UF      EQUAL TAB-TB19-UF(ACU-IND19)
               AND WRK-DOCOR-INV       NOT LESS
                                         TAB-TB19-DATA-INIC(ACU-IND19)
               AND WRK-DOCOR-INV       NOT GREATER
                                         TAB-TB19-DATA-FIM(ACU-IND19)
                   MOVE 'V'            TO WRK-ACHOU
                   MOVE ACU-IND19      TO WRK-IND
                   MOVE 999            TO ACU-IND19
               END-IF
           END-PERFORM.

           IF  WRK-ACHOU               EQUAL 'V'
               PERFORM 1600-PESQUISA-SUSP-AGENCIA
               IF  WRK-COUNT           EQUAL ZEROS
                   PERFORM 1900-PESQUISA-SUSP-CLIENTE
                   IF  WRK-COUNT       EQUAL ZEROS
                       MOVE WRK-IND    TO ACU-IND19
                       MOVE 02         TO GFCTYJ-CSIT-EVNTO-RECBD
                       MOVE TAB-TB19-DATA-COBR(ACU-IND19)
                                       TO GFCTYJ-DAGNDA-DEB-MOVTO
                       PERFORM 1700-GRAVA-AGENDADO
                       MOVE 'S'        TO WRK-REGISTRO-AGENDADO
                   ELSE
                       MOVE 'S'        TO WRK-REGISTRO-DESPREZADO
                   END-IF
               ELSE
                   MOVE 'S'            TO WRK-REGISTRO-DESPREZADO
               END-IF
PROC       ELSE
PROC           MOVE 'S'                TO WRK-REGISTRO-DESPREZADO
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       2300-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2400-EMITIR-DISPLAY.

           CLOSE ARQTB0E7
                 SUSPENSO
                 AGENDADO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR ESTATISCA DE PROCESSMANTO                           *
      *----------------------------------------------------------------*
       2400-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           IF  ACU-GRAVA-AGENDADO      NOT EQUAL ZEROS
               DISPLAY
               '*-----------------------------------------------*'
               DISPLAY
               '*           PROGRAMA  GFCT1645                  *'
               DISPLAY
               '*           ------------------                  *'
               DISPLAY
               '*                                               *'
               DISPLAY
               '*     PROCESSAMENTO EFETUADO COM SUCESSO        *'
               DISPLAY
               '*-----------------------------------------------*'
               DISPLAY
               '*-----------------------------------------------*'

               DISPLAY
               '*************************************************'
      -        '*************************'

               MOVE ACU-LIDOS-SUSPENSO TO WRK-MASCARA
               DISPLAY
               '*  TOTAL DE REGISTROS LIDOS     NO SUSPENSO      '
      -        '        = ' WRK-MASCARA '  *'

               MOVE ACU-LIDOS-ARQTB0E7 TO WRK-MASCARA
               DISPLAY
               '*  TOTAL DE REGISTROS LIDOS     NO ARQTB0E7      '
      -        '        = ' WRK-MASCARA '  *'

               MOVE ACU-GRAVA-AGENDADO TO WRK-MASCARA
               DISPLAY
               '*  TOTAL DE REGISTROS GRAVADOS  NO AGENDADO      '
      -        '        = ' WRK-MASCARA '  *'

               MOVE ACU-DESPREZADOS    TO WRK-MASCARA
               DISPLAY
               '*  TOTAL DE REGISTROS DESPREZADOS                '
      -        '        = ' WRK-MASCARA '  *'
               DISPLAY
               '*************************************************'
      -        '*************************'

           ELSE
               DISPLAY
               '*-----------------------------------------------*'
               DISPLAY
               '*           PROGRAMA GFCT1645                   *'
               DISPLAY
               '*           -----------------                   *'
               DISPLAY
               '*                                               *'
               DISPLAY
               '*        ARQUIVO DE SUSPENSO VAZIO              *'
               DISPLAY
               '*-----------------------------------------------*'
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1645'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
