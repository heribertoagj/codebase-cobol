      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1857.
       AUTHOR.     MARCUS VINICIUS.
      *================================================================*
      *                 W I P R O   I N F O S E R V E R                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1857                                     *
      *    ANALISTA     : MARCUS VINICIUS - INFOSERVER - GRUPO 38      *
      *    DATA         : JUL/2018                                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  CANCELAMENTO MASSIVO DE CONTAS DO AGRUP.     *
      *    PERSONALIZADO.                                              *
      *      CONSISTE E PREPARA ARQUIVO DO GESTOR PARA CANCELAMENTO.   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                                  INCLUDE/BOOK      *
      *        MVTOENTR                                 GFCTWATF       *
      *        MVTOIMAG                                 GFCTWATG       *
      *        MVTODESP                                 GFCTWATH       *
      *        RELATO                                       -          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                   DB2PRD.TPO_AGPTO_CTA          GFCTB0F3       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.TCTA_AGPTO_PERSO       GFCTB0J5       *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#BRAD7C - AREA DE TRATAMENTO DE ERRO PELA BRAD7100.        *
      *    I#GFCTTB - AREA DO MODULO GFCT5517.                         *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
      *    BRAD1570 - CONSISTIR DATA.                                  *
      *    BRAD7100 - TRATAMENTO DE ERROS.                             *
      *    BRAD7600 - OBTER DATA E HORA.                               *
      *    GFCT5517 - OBTER NOME DO CLIENTE ATRAVES DA AGENCIA E CONTA.*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT MVTOENTR ASSIGN      TO UT-S-MVTOENTR
                      FILE STATUS      IS WRK-FS-MVTOENTR.

           SELECT MVTOIMAG ASSIGN      TO UT-S-MVTOIMAG
                      FILE STATUS      IS WRK-FS-MVTOIMAG.

           SELECT MVTODESP ASSIGN      TO UT-S-MVTODESP
                      FILE STATUS      IS WRK-FS-MVTODESP.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
                      FILE STATUS      IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  : ARQUIVO DE ENTRARA                                 *
      *             ORG. SEQUENCIAL   -   LRECL = 070                  *
      *----------------------------------------------------------------*

       FD  MVTOENTR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-MVTOENTR                 PIC  X(070).

      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO DE SAIDA                                   *
      *             ORG. SEQUENCIAL   -   LRECL = 100                  *
      *----------------------------------------------------------------*

       FD  MVTOIMAG
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-MVTOIMAG                 PIC  X(100).

      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO DE SAIDA                                   *
      *             ORG. SEQUENCIAL   -   LRECL = 070                  *
      *----------------------------------------------------------------*

       FD  MVTODESP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-MVTODESP                 PIC  X(070).

      *----------------------------------------------------------------*
      *    OUTPUT : ARQUIVO DE RELATORIO                               *
      *             ORG. SEQUENCIAL   -   LRECL = 133                  *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-RELATO                   PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA WORKING GFCT1857 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-MVTOENTR          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MVTOIMAG          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-MVTODESP          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-CONTAS                  PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-TOTAL-CONTAS            PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(002) COMP-3  VALUE 99.
       77  ACU-PAGINA                  PIC  9(002) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-CAGPTO-CTA              PIC  9(003)         VALUE ZEROS.
       77  WRK-CAGPTO-CTA-ANT          PIC  9(003)         VALUE ZEROS.
       77  WRK-CBCO                    PIC  9(003)         VALUE ZEROS.
       77  WRK-CAG-BCRIA               PIC  9(005)         VALUE ZEROS.
       77  WRK-CCTA-BCRIA-CLI          PIC  9(013)         VALUE ZEROS.
       77  WRK-DT-FIM-VGCIA            PIC  X(010)         VALUE SPACES.
       77  WRK-STATUS                  PIC  X(001)         VALUE SPACES.
       77  WRK-MENSAGEM                PIC  X(020)         VALUE SPACES.
       77  WRK-DESPREZADO              PIC  X(001)         VALUE SPACES.
       77  WRK-MSG01                   PIC  X(020)         VALUE
           'AGRUP. NAO EXISTE   '.
       77  WRK-MSG02                   PIC  X(020)         VALUE
           'AGRUP. NAO E PERSNL.'.
       77  WRK-MSG03                   PIC  X(020)         VALUE
           'CONTA NAO VIGENTE   '.
       77  WRK-MSG04                   PIC  X(020)         VALUE
           'DATA INFORM. INVLDA.'.
       77  WRK-MSG05                   PIC  X(020)         VALUE
           'DATA FIM < QUE DPROX'.
       77  WRK-MSG06                   PIC  X(020)         VALUE
           'DATA FIM < QUE INIC.'.

       01  WRK-9DE13                   PIC  9(013)         VALUE ZEROS.
       01  WRK-9DE13-R                 REDEFINES WRK-9DE13.
           05  FILLER                  PIC  X(006).
           05  WRK-9DE7                PIC  9(007).

       01  WRK-DATA1                   PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R1                 REDEFINES WRK-DATA1.
           05  WRK-ANO-AUX1            PIC  9(004).
           05  WRK-MES-AUX1            PIC  9(002).
           05  WRK-DIA-AUX1            PIC  9(002).

       01  WRK-DATA2                   PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R2                 REDEFINES WRK-DATA2.
           05  WRK-ANO-AUX2            PIC  9(004).
           05  WRK-MES-AUX2            PIC  9(002).
           05  WRK-DIA-AUX2            PIC  9(002).

       01  WRK-DATA3                   PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R3                 REDEFINES WRK-DATA3.
           05  WRK-ANO-AUX3            PIC  9(004).
           05  WRK-MES-AUX3            PIC  9(002).
           05  WRK-DIA-AUX3            PIC  9(002).

       01  WRK-DATA-AUX.
           05  WRK-AUX-DIA             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-AUX-MES             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-AUX-ANO             PIC  9(004)         VALUE ZEROS.

       01  WRK-TIMESTAMP-AUX.
           05  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  WRK-HOR-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MIN-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-SEG-AUX             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MIL-AUX             PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TESTE DE FILE-STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-MVTOENTR             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-MVTOIMAG             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-MVTODESP             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-RELATO               PIC  X(002)         VALUE SPACES.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** MENSAGENS DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE '* '.
           05  FILLER                  PIC  X(005)         VALUE
               'ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE-STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE ' *'.
           05  FILLER                  PIC  X(007)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DOS ARQUIVOS ***'.
      *----------------------------------------------------------------*

       COPY GFCTWATF.

       COPY GFCTWATG.

       COPY GFCTWATH.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD1570 ***'.
      *----------------------------------------------------------------*

       01  WRK-1570-ROTINA-DATAS.
           05  WRK-1570-DATA.
               10 FILLER               PIC  X(002)         VALUE SPACES.
               10 WRK-1570-DATA-CONS   PIC  9(008)         VALUE ZEROS.
           05  WRK-1570-DATA-R         REDEFINES
               WRK-1570-DATA           PIC  X(010).
           05  WRK-1570-FORMATO-DATA   PIC  9(002)         VALUE ZEROS.
           05  WRK-1570-MENSAGEM       PIC  X(080)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DATA-JULIANA        PIC  9(005) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAMMDD         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-DATA-AAAAMMDD       PIC  9(009) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSS         PIC  9(007) COMP-3  VALUE ZEROS.
           05  WRK-HORA-HHMMSSMMMMMM   PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TIMESTAMP.
               10  WRK-ANO             PIC  9(004)         VALUE ZEROS.
               10  WRK-MES             PIC  9(002)         VALUE ZEROS.
               10  WRK-DIA             PIC  9(002)         VALUE ZEROS.
               10  WRK-HOR             PIC  9(002)         VALUE ZEROS.
               10  WRK-MIN             PIC  9(002)         VALUE ZEROS.
               10  WRK-SEG             PIC  9(002)         VALUE ZEROS.
               10  WRK-MIL             PIC  9(006)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO GFCT5517 ***'.
      *----------------------------------------------------------------*

       COPY I#GFCTTB.

       01  WRK-IO-PCB.
           05  WRK-IO-LTERM            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-IO-STATUS           PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-LTERM           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  WRK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA RELATORIO ***'.
      *----------------------------------------------------------------*

       01  CABEC1-REL1-1.
           05  FILLER                  PIC  X(001)         VALUE '1'.
           05  CB1-REL1-DIA            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-MES            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  CB1-REL1-ANO            PIC  9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(041)         VALUE SPACES.
           05  FILLER                  PIC  X(011)         VALUE
               'B A N C O'.
           05  FILLER                  PIC  X(017)         VALUE
               'B R A D E S C O'.
           05  FILLER                  PIC  X(045)         VALUE 'S/A'.
           05  CB1-REL1-HOR            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-MIN            PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE ':'.
           05  CB1-REL1-SEG            PIC  9(002)         VALUE ZEROS.

       01  CABEC2-REL1-2.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(041)         VALUE
               'GFCT1857'.
           05  FILLER                  PIC  X(083)         VALUE
               'GFCT - GESTAO, FLEXIBILIZACAO E COBRANCA DE TARIFAS'.
           05  FILLER                  PIC  X(006)         VALUE 'PAG.'.
           05  CB2-REL1-PAG            PIC  9(002)         VALUE ZEROS.

       01  CABEC3-REL1-3.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(039)         VALUE SPACES.
           05  FILLER                  PIC  X(028)         VALUE
               'AGRUPAMENTO PERSONALIZADO - '.
           05  FILLER                  PIC  X(030)         VALUE
               'CANCELAMENTO MASSIVO DE CONTAS'.

       01  CABEC4-REL1-TRACO.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(132)        VALUE ALL '-'.

       01  CABEC5-REL1-5.
           05  FILLER                  PIC  X(001)         VALUE '0'.
           05  FILLER                  PIC  X(041)         VALUE SPACES.
           05  FILLER                  PIC  X(014)         VALUE
               'AGRUPAMENTO - '.
           05  CB5-REL1-AGRUP-NRO      PIC  9(003)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE '   '.
           05  CB5-REL1-AGRUP-DESC     PIC  X(030)         VALUE SPACES.

       01  CABEC6-REL1-6.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(085)         VALUE SPACES.
           05  FILLER                  PIC  X(023)         VALUE
               '------ VIGENCIA -------'.

       01  CABEC7-REL1-7.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(027)         VALUE SPACES.
           05  FILLER                  PIC  X(018)         VALUE
               'AGENCIA'.
           05  FILLER                  PIC  X(007)         VALUE
               'CONTA'.
           05  FILLER                  PIC  X(031)         VALUE
               'NOME DO CLIENTE'.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               'INICIO      '.
           05  FILLER                  PIC  X(003)         VALUE 'FIM'.

       01  CABEC8-REL1-BRANCO.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(132)         VALUE SPACES.

       01  LINDET1-REL1-DET.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  FILLER                  PIC  X(029)         VALUE SPACES.
           05  LD1-REL1-DET-AGENCIA    PIC 9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  LD1-REL1-DET-CONTA      PIC  9(013)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  LD1-REL1-DET-CLIENTE    PIC X(030)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  LD1-REL1-DET-DIA-INI    PIC 9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-DET-MES-INI    PIC 9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-DET-ANO-INI    PIC 9(004)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE SPACES.
           05  LD1-REL1-DET-DIA-FIM    PIC 9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-DET-MES-FIM    PIC 9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  LD1-REL1-DET-ANO-FIM    PIC 9(004)         VALUE ZEROS.

       01  LINTOT1-REL1-TOTA.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(029)         VALUE SPACES.
           05  FILLER                  PIC  X(053)         VALUE
               'Quantidade de Contas canceladas para o agrupamento : '.
           05  LT1-REL1-CONTA          PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

       01  LINTOT2-REL1-TOTG.
           05  FILLER                  PIC  X(001)         VALUE '-'.
           05  FILLER                  PIC  X(029)         VALUE SPACES.
           05  FILLER                  PIC  X(052)         VALUE
               'Quantidade Total de Contas canceladas ............: '.
           05  LT2-REL1-TOTAL          PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0F3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0J5
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING GFCT1857 ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    ROTINA PRINCIPAL.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 1000-INICIALIZAR-PROGRAMA.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM UNTIL WRK-FS-MVTOENTR EQUAL '10'
               PERFORM 3000-PROCESSAR
               MOVE WRK-CAGPTO-CTA       TO WRK-CAGPTO-CTA-ANT
               PERFORM 2100-LER-MVTOENTR
           END-PERFORM.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE INICIALIZACAO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  MVTOENTR
                OUTPUT MVTOIMAG
                       MVTODESP
                       RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DATA-HORA.

           MOVE WRK-DIA                TO CB1-REL1-DIA.
           MOVE WRK-MES                TO CB1-REL1-MES.
           MOVE WRK-ANO                TO CB1-REL1-ANO.
           MOVE WRK-HOR                TO CB1-REL1-HOR.
           MOVE WRK-MIN                TO CB1-REL1-MIN.
           MOVE WRK-SEG                TO CB1-REL1-SEG.

           PERFORM 1300-OBTER-DATA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-MVTOENTR.

           PERFORM 1120-TESTAR-FS-MVTOIMAG.

           PERFORM 1130-TESTAR-FS-MVTODESP.

           PERFORM 1140-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO MVTOENTR.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-MVTOENTR         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVTOENTR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVTOENTR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-MVTOENTR    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO MVTOIMAG.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-MVTOIMAG         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVTOIMAG         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVTOIMAG'         TO WRK-NOME-ARQ
               MOVE WRK-FS-MVTOIMAG    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO MVTODESP.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-MVTODESP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-MVTODESP         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'MVTODESP'         TO WRK-NOME-ARQ
               MOVE WRK-FS-MVTODESP    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE-STATUS DO ARQUIVO RELATO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELATO  '         TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     ACESSAR BRAD7600.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER DATA.                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-OBTER-DATA                 SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT
                   DPROCM_ATUAL,
                   DPROCM_PROX
               INTO
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
               FROM  DB2PRD.PARM_DATA_PROCM
               WHERE CSIST_PRINC  = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-PROX            OF GFCTB0A1
                                       TO WRK-DATA-AUX.
           MOVE WRK-AUX-DIA            TO WRK-DIA-AUX3.
           MOVE WRK-AUX-MES            TO WRK-MES-AUX3.
           MOVE WRK-AUX-ANO            TO WRK-ANO-AUX3.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA VERIFICAR ARQUIVO VAZIO.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-MVTOENTR.

           IF  WRK-FS-MVTOENTR         EQUAL '10'
               DISPLAY '********* GFCT1857 ********'
               DISPLAY '*                         *'
               DISPLAY '* ARQUIVO MVTOENTR VAZIO  *'
               DISPLAY '* PROCESSAMENTO ENCERRADO *'
               DISPLAY '*                         *'
               DISPLAY '********* GFCT1857 ********'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO MVTOENTR.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-MVTOENTR               SECTION.
      *----------------------------------------------------------------*

           READ MVTOENTR               INTO WATF-GFCTB0J5.

           IF  WRK-FS-MVTOENTR         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-MVTOENTR.

           MOVE WATF-CAGPTO-CTA        TO WRK-CAGPTO-CTA.
           MOVE WATF-CBCO              TO WRK-CBCO.
           MOVE WATF-CAG-BCRIA         TO WRK-CAG-BCRIA.
           MOVE WATF-CCTA-BCRIA-CLI    TO WRK-CCTA-BCRIA-CLI.
           MOVE WATF-DT-FIM-VGCIA      TO WRK-DT-FIM-VGCIA.

           INSPECT WRK-DT-FIM-VGCIA    REPLACING ALL '/' BY '.'.

           ADD 1                       TO ACU-LIDOS-MVTOENTR.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-DESPREZADO.

           PERFORM 3100-OBTER-AGRUPAMENTO.

           IF  WRK-DESPREZADO          EQUAL 'S'
               PERFORM 3600-GRAVAR-MVTODESP
               GO TO 3000-99-FIM
           END-IF.

           PERFORM 3400-VERIFICAR-VIGENTE.

           IF  WRK-DESPREZADO          EQUAL 'S'
               PERFORM 3600-GRAVAR-MVTODESP
               GO TO 3000-99-FIM
           END-IF.

           IF   WRK-DT-FIM-VGCIA       EQUAL '00.00.0000'
                MOVE DPROCM-PROX       OF GFCTB0A1
                                       TO WRK-DATA-AUX
           ELSE
               PERFORM 3500-CONSISTIR-DATA

               IF  WRK-DESPREZADO          EQUAL 'S'
                   PERFORM 3600-GRAVAR-MVTODESP
                   GO TO 3000-99-FIM
               END-IF
           END-IF.

           PERFORM 3550-TRATAR-DATA-CANC.

           IF  WRK-DESPREZADO          EQUAL 'S'
               PERFORM 3600-GRAVAR-MVTODESP
               GO TO 3000-99-FIM
           END-IF.

           PERFORM 3700-GRAVAR-MVTOIMAG.

           PERFORM 3800-OBTEM-NOME-CLIENTE.

           PERFORM 3900-GRAVAR-RELATORIO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER AGRUPAMENTO PERSONALIZADO.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-OBTER-AGRUPAMENTO          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CAGPTO-CTA         TO CAGPTO-CTA        OF GFCTB0F3.
           MOVE ZEROS                  TO CINDCD-EXCL-LOGCA OF GFCTB0F3.

           EXEC SQL
               SELECT
                   RAGPTO_CTA,
                   CINDCD_AGPTO_PERSO
               INTO
                   :GFCTB0F3.RAGPTO-CTA,
                   :GFCTB0F3.CINDCD-AGPTO-PERSO
               FROM  DB2PRD.TPO_AGPTO_CTA
               WHERE CAGPTO_CTA        = :GFCTB0F3.CAGPTO-CTA
                 AND CINDCD_EXCL_LOGCA = :GFCTB0F3.CINDCD-EXCL-LOGCA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TPO_AGPTO_CTA'    TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE WRK-MSG01          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
               GO TO 3100-99-FIM
           END-IF.

           IF  CINDCD-AGPTO-PERSO      OF GFCTB0F3
                                       NOT EQUAL 'S'
               MOVE WRK-MSG02          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICAR-VIGENTE.                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-VERIFICAR-VIGENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CAGPTO-CTA         TO CAGPTO-CTA      OF GFCTB0J5.
           MOVE WRK-CAG-BCRIA          TO CAG-BCRIA       OF GFCTB0J5.
           MOVE WRK-CCTA-BCRIA-CLI     TO CCTA-BCRIA-CLI  OF GFCTB0J5.
           MOVE DPROCM-PROX OF GFCTB0A1
                                       TO DFIM-VGCIA-REG  OF GFCTB0J5.

           EXEC SQL
               SELECT
                   CAGPTO_CTA,
                   CAG_BCRIA,
                   CCTA_BCRIA_CLI,
                   DINIC_VGCIA_REG,
                   DFIM_VGCIA_REG
               INTO
                   :GFCTB0J5.CAGPTO-CTA,
                   :GFCTB0J5.CAG-BCRIA,
                   :GFCTB0J5.CCTA-BCRIA-CLI,
                   :GFCTB0J5.DINIC-VGCIA-REG,
                   :GFCTB0J5.DFIM-VGCIA-REG
               FROM  DB2PRD.TCTA_AGPTO_PERSO
               WHERE CAGPTO_CTA      = :GFCTB0J5.CAGPTO-CTA
                 AND CBCO            = 237
                 AND CAG_BCRIA       = :GFCTB0J5.CAG-BCRIA
                 AND CCTA_BCRIA_CLI  = :GFCTB0J5.CCTA-BCRIA-CLI
                 AND DFIM_VGCIA_REG >= :GFCTB0J5.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE       NOT EQUAL ZEROS AND +100 AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TCTA_AGPTO_PERSO' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE WRK-MSG03          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ACESSAR BRAD1570 PARA CONSISTIR DATA.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-1570-ROTINA-DATAS.

           INITIALIZE WRK-1570-ROTINA-DATAS.

           MOVE WRK-DT-FIM-VGCIA       TO WRK-1570-DATA.
           MOVE 9                      TO WRK-1570-FORMATO-DATA.

           CALL 'BRAD1570'             USING WRK-1570-DATA-R
                                             WRK-1570-FORMATO-DATA
                                             WRK-1570-MENSAGEM.

           IF  RETURN-CODE             EQUAL 4
               MOVE WRK-MSG04          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
               GO TO 3500-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAR DATAS PARA IDENTIFICAR SE STATUS EH DE DELECAO       *
      *      OU DE CANCELAMENTO.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3550-TRATAR-DATA-CANC           SECTION.
      *----------------------------------------------------------------*

           MOVE DINIC-VGCIA-REG        OF GFCTB0J5
                                       TO WRK-DATA-AUX.
           MOVE WRK-AUX-DIA            TO WRK-DIA-AUX1.
           MOVE WRK-AUX-MES            TO WRK-MES-AUX1.
           MOVE WRK-AUX-ANO            TO WRK-ANO-AUX1.

           MOVE WRK-DT-FIM-VGCIA       TO WRK-DATA-AUX.
           MOVE WRK-AUX-DIA            TO WRK-DIA-AUX2.
           MOVE WRK-AUX-MES            TO WRK-MES-AUX2.
           MOVE WRK-AUX-ANO            TO WRK-ANO-AUX2.

           IF  WRK-DATA2               LESS  WRK-DATA3
               MOVE WRK-MSG05          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
               GO TO 3550-99-FIM
           END-IF.

           IF  WRK-DATA2               LESS  WRK-DATA1
               MOVE WRK-MSG06          TO WRK-MENSAGEM
               MOVE 'S'                TO WRK-DESPREZADO
               GO TO 3550-99-FIM
           END-IF.

           IF  WRK-DATA2               EQUAL  WRK-DATA1
               MOVE WRK-DT-FIM-VGCIA   TO WRK-DATA-AUX
               MOVE 'D'                TO WRK-STATUS
           ELSE
               MOVE WRK-DT-FIM-VGCIA   TO WRK-DATA-AUX
               MOVE 'C'                TO WRK-STATUS
           END-IF.

      *----------------------------------------------------------------*
       3550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DE SAIDA MVTODESP.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-MVTODESP            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WATH-REG-DESPRE.

           INITIALIZE WATH-REG-DESPRE.

           MOVE WRK-CAGPTO-CTA         TO WATH-CAGPTO-CTA.
           MOVE WRK-CBCO               TO WATH-CBCO.
           MOVE WRK-CAG-BCRIA          TO WATH-CAG-BCRIA.
           MOVE WRK-CCTA-BCRIA-CLI     TO WATH-CCTA-BCRIA-CLI.
           MOVE WRK-DT-FIM-VGCIA       TO WATH-DT-FIM-VGCIA.
           MOVE WRK-MENSAGEM           TO WATH-MENSAGEM.

           WRITE FD-MVTODESP           FROM WATH-REG-DESPRE.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-MVTODESP.

           ADD 1                       TO ACU-GRAVA-MVTODESP.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR ARQUIVO DE SAIDA MVTOIMAG.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-GRAVAR-MVTOIMAG            SECTION.
      *----------------------------------------------------------------*

           PERFORM 1200-OBTER-DATA-HORA.

           MOVE SPACES                 TO WATG-REG.

           INITIALIZE WATG-REG.

           MOVE WRK-ANO                TO WRK-ANO-AUX.
           MOVE WRK-MES                TO WRK-MES-AUX.
           MOVE WRK-DIA                TO WRK-DIA-AUX.
           MOVE WRK-HOR                TO WRK-HOR-AUX.
           MOVE WRK-MIN                TO WRK-MIN-AUX.
           MOVE WRK-SEG                TO WRK-SEG-AUX.
           MOVE WRK-MIL                TO WRK-MIL-AUX.
           MOVE WRK-CAGPTO-CTA         TO WATG-CAGPTO-CTA.
           MOVE WRK-CBCO               TO WATG-CBCO.
           MOVE WRK-CAG-BCRIA          TO WATG-CAG-BCRIA.
           MOVE WRK-CCTA-BCRIA-CLI     TO WATG-CCTA-BCRIA-CLI.
           MOVE DINIC-VGCIA-REG OF GFCTB0J5
                                       TO WATG-DINIC-VGCIA-REG.
           MOVE WRK-DATA-AUX           TO WATG-DFIM-VGCIA-REG.
           MOVE 'B'                    TO WATG-CIDTFD-PROCM-ENCRR.
           MOVE 1857                   TO WATG-CFUNC-ENCRR-VGCIA.
           MOVE ' '                    TO WATG-CFUNC-ENCRR-VGCIA-N.
           MOVE WRK-TIMESTAMP-AUX
                                       TO WATG-HVGCIA-REGRA.
           MOVE WRK-STATUS             TO WATG-STATUS.

           WRITE FD-MVTOIMAG           FROM WATG-REG.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-MVTOIMAG.

           ADD 1                       TO ACU-GRAVA-MVTOIMAG.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *     ACESSAR MODULO GFCT5517.                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-OBTEM-NOME-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTTB-AREA-GFCT5517.

           MOVE 'B'                    TO GFCTTB-AMBIENTE.
           MOVE WRK-CAG-BCRIA          TO GFCTTB-CJUNC-DEPDC.
           MOVE WRK-CCTA-BCRIA-CLI     TO WRK-9DE13.
           MOVE WRK-9DE7               TO GFCTTB-CCTA-CLI.
           MOVE 'GFCT5517'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTTB-AREA-GFCT5517
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  GFCTTB-COD-RETORNO      EQUAL 2
               MOVE 'CLIENTE NAO ENCONTRADO NO CLIE'
                                       TO GFCTTB-IPSSOA-COPLT
           END-IF.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR-RELATORIO.                                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-GRAVAR-RELATORIO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CAGPTO-CTA          NOT EQUAL WRK-CAGPTO-CTA-ANT AND
               ACU-PAGINA              NOT EQUAL ZEROS
               PERFORM 3910-IMPRIMIR-TOTAL-PARC
           END-IF.

           IF  ACU-LINHAS              GREATER 57
               PERFORM 3920-IMPRIMIR-CABEC
           END-IF.

           PERFORM 3930-IMPRIMIR-DETALHE.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR TOTAL.                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3910-IMPRIMIR-TOTAL-PARC        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-CONTAS             TO LT1-REL1-CONTA.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-RELATO             FROM LINTOT1-REL1-TOTA.

           PERFORM 1140-TESTAR-FS-RELATO.

           ADD ACU-CONTAS              TO ACU-TOTAL-CONTAS.

           MOVE ZEROS                  TO ACU-CONTAS.
           MOVE 99                     TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR CABECALHO.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       3920-IMPRIMIR-CABEC             SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-RELATO             FROM CABEC1-REL1-1.
           PERFORM 1140-TESTAR-FS-RELATO.

           MOVE ACU-PAGINA             TO CB2-REL1-PAG.

           WRITE FD-RELATO             FROM CABEC2-REL1-2.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC3-REL1-3.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC4-REL1-TRACO.
           PERFORM 1140-TESTAR-FS-RELATO.

           MOVE WATG-CAGPTO-CTA        TO CB5-REL1-AGRUP-NRO.
           MOVE RAGPTO-CTA OF GFCTB0F3 TO CB5-REL1-AGRUP-DESC.

           WRITE FD-RELATO             FROM CABEC5-REL1-5.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC6-REL1-6.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC7-REL1-7.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM CABEC8-REL1-BRANCO.
           PERFORM 1140-TESTAR-FS-RELATO.

           MOVE 9                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR LINHA DE DETALHE.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3930-IMPRIMIR-DETALHE           SECTION.
      *----------------------------------------------------------------*

           MOVE WATG-CAG-BCRIA         TO LD1-REL1-DET-AGENCIA.
           MOVE WATG-CCTA-BCRIA-CLI    TO LD1-REL1-DET-CONTA.
           MOVE GFCTTB-IPSSOA-COPLT(1:30)
                                       TO LD1-REL1-DET-CLIENTE.
           MOVE WATG-DINIC-VGCIA-REG TO WRK-DATA-AUX.
           MOVE WRK-AUX-DIA            TO LD1-REL1-DET-DIA-INI.
           MOVE WRK-AUX-MES            TO LD1-REL1-DET-MES-INI.
           MOVE WRK-AUX-ANO            TO LD1-REL1-DET-ANO-INI.
           MOVE WATG-DFIM-VGCIA-REG    TO WRK-DATA-AUX.
           MOVE WRK-AUX-DIA            TO LD1-REL1-DET-DIA-FIM.
           MOVE WRK-AUX-MES            TO LD1-REL1-DET-MES-FIM.
           MOVE WRK-AUX-ANO            TO LD1-REL1-DET-ANO-FIM.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-RELATO             FROM LINDET1-REL1-DET.

           PERFORM 1140-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-LINHAS
                                          ACU-CONTAS.

      *----------------------------------------------------------------*
       3930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    IMPRIMIR LINHA DE TOTAL GERAL.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3940-IMPRIMIR-TOTAL-GERAL       SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-TOTAL-CONTAS       TO LT2-REL1-TOTAL.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-RELATO             FROM CABEC8-REL1-BRANCO.

           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-RELATO             FROM LINTOT2-REL1-TOTG.

           PERFORM 1140-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       3940-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FINALIZAR PROCESSAMENTO.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF  ACU-CONTAS              GREATER ZEROS
               PERFORM 3910-IMPRIMIR-TOTAL-PARC
               PERFORM 3940-IMPRIMIR-TOTAL-GERAL
           END-IF.

           CLOSE MVTOENTR
                 MVTOIMAG
                 MVTODESP
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 4100-EMITIR-ESTATISTICAS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR ESTATISTICAS DE PROCESSAMENTO.                       *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-ESTATISTICAS        SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-MVTOENTR     TO WRK-MASCARA.

           DISPLAY '*----------------------------------------*'.
           DISPLAY '*            PROGRAMA GFCT1857           *'.
           DISPLAY '*            -----------------           *'.
           DISPLAY '*                                        *'.
           DISPLAY '*   PROCESSAMENTO EFETUADO COM SUCESSO   *'.
           DISPLAY '*----------------------------------------*'.
           DISPLAY '*----------------------------------------*'.
           DISPLAY '*                                        *'.
           DISPLAY '* REGS LIDOS MVTOENTR      = ' WRK-MASCARA
                                                           ' *'.

           MOVE ACU-GRAVA-MVTOIMAG     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS MVTOIMAG   = ' WRK-MASCARA
                                                           ' *'.
           MOVE ACU-GRAVA-MVTODESP     TO WRK-MASCARA.

           DISPLAY '* REGS GRAVADOS MVTODESP   = ' WRK-MASCARA
                                                           ' *'.
           DISPLAY '*----------------------------------------*'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DE ERRO.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1857'             TO ERR-PGM.

           IF  ERR-TIPO-ACESSO         EQUAL  'DB2'
               CALL 'BRAD7100'         USING   WRK-BATCH
                                               ERRO-AREA
                                               SQLCA
           ELSE
               CALL 'BRAD7100'         USING   WRK-BATCH
                                               ERRO-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
