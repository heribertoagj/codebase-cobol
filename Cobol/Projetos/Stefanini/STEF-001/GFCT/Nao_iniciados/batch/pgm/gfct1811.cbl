      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1811.
       AUTHOR.     VINICIUS CRISTIAN MADUREIRA.
      *================================================================*
      *.     S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1811                                    *
      *    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    ANALISTA....:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
      *    DATA........:   19/05/2008                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   APLICAR DESCONTO POR CARENCIA, A PARTIR DA  *
      *                    DATA DE ADESAO AO PACOTE.                   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ADPACOTE                  I#GFCTVE          *
      *                    FRANQT92                  I#GFCTVA          *
      *                    FRANQT41                  I#GFCTV4          *
      *                    SEGCOBRA                  I#GFCTVA          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.V01CTA_POUPANCA        CLIEV007      *
      *                    DB2PRD.V01CTA_CORRENTE        CLIEV008      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
JC0413*                    DB2PRD.PARM_SERVC_TARIF       GFCTB0A2      *
      *                    DB2PRD.ADSAO_INDVD_PCOTE      GFCTB009      *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100.     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD1050 - CONEXAO COM DB2.                                 *
      *    BRAD1285 - SOMAR OU SUBTRAIR UM NUMERO DE DIAS EM UMA DATA. *
      *    BRAD7100 - TRATAMENTO DE ERROS PADRAO BRADESCO.             *
      *    BRAD7600 - OBTER DATA/HORA ATUAL DO SISTEMA.                *
      *    BRAD0025 - SOMA E SUBTRAI DIAS NA DATA.                     *
      *    BRAD1640 - SOMA DE DIAS, MESES E ANOS EM UMA DETERMINADA    *
      *               DATA COM SECULO.                                 *
      *================================================================*
060509*================================================================*
060509*      S O N D A  P R O C W O R K  -  C O N S U L T O R I A      *
060509*----------------------------------------------------------------*
060509*    PROGRAMADOR.:   VINICIUS CRISTIAN MADUREIRA - SONDPROC/GP.50*
060509*    ANALISTA....:   WAGNER SILVA                - SONDPROC/GP.50*
060509*    DATA........:   06/05/2009                                  *
060509*                                                                *
060509*    OBJETIVO    :   RETIRAR TRATAMENTOS E MUDAR BOOK DE SAIDA.  *
060509*================================================================*
      *===============================================================*
      *                   M A N U T E N C O E S                       *
      *===============================================================*
      *---------------------------------------------------------------*
      *    OBJETIVO....:   ALTERACAO DO TAMANHO DA COLUNA DA TABELA   *
      *     (JC0413)       GFCTBA2 - QDIA_CAREN_TARIF DE 9(02) PARA   *
      *                    9(03) - A INC.I#GFCTVE POSSUE O CAMPO      *
      *                    QDIA-CAREN-TARIF (LAYOUT DO ARQUIVO DE     *
      *                    ENTRADA).                                  *
      *    OBS.........:   APENAS RECOMPILADO.                        *
      *---------------------------------------------------------------*
      *    PROGRAMADOR.:   J.C.R.                    - SONDA IT       *
      *    ANALISTA DDS:   HELENA                    - GP.50          *
      *    ANALISTA....:   PAGNNOCA                  - SONDA IT       *
      *    DATA........:   ABRIL/2013                                 *
      *---------------------------------------------------------------*
      *===============================================================*
RU0125*================================================================*
RU0125*                        A L T E R A C A O                       *
RU0125*----------------------------------------------------------------*
RU0125* ANALISTA       :  MARCIO ALEXANDRO RUI                         *
RU0125* DATA           :  JAN/2025                                     *
RU0125* OBJETIVO       :  INCLUSAO DE CPTCAO-TABELA                    *
RU0125*================================================================*

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

           SELECT ADPACOTE ASSIGN      TO UT-S-ADPACOTE
           FILE STATUS                 IS WRK-FS-ADPACOTE.

           SELECT FRANQT92 ASSIGN      TO UT-S-FRANQT92
           FILE STATUS                 IS WRK-FS-FRANQT92.

           SELECT FRANQT41 ASSIGN      TO UT-S-FRANQT41
           FILE STATUS                 IS WRK-FS-FRANQT41.

           SELECT SEGCOBRA ASSIGN      TO UT-S-SEGCOBRA
           FILE STATUS                 IS WRK-FS-SEGCOBRA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : REGISTROS A COBRAR.                                 *
      *            ORG. SEQUENCIAL     -   LRECL = 300                 *
      *----------------------------------------------------------------*

       FD  ADPACOTE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB092.         *
      *            ORG. SEQUENCIAL     -   LRECL = 250                 *
      *----------------------------------------------------------------*

       FD  FRANQT92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-FRANQT92                PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT: FRANQUEADOS PARA GRAVAR NA TABELA GFCTB041.         *
      *            ORG. SEQUENCIAL     -   LRECL = 150                 *
      *----------------------------------------------------------------*

       FD  FRANQT41
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTV4'.

      *----------------------------------------------------------------*
      *    OUTPUT: SEGUEM PARA CONTINUAR PROCESSO DE COBRANCA.         *
      *            ORG. SEQUENCIAL     -   LRECL = 250                 *
      *----------------------------------------------------------------*

       FD  SEGCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

060509 COPY 'I#GFCTVE'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-ADPACOTE          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-FRANQT41          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-SEGCOBRA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-NAO-COBRADOS            PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-BRAD1205-OC             PIC  9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA DE TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

       77  WRK-FS-ADPACOTE             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT92             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FRANQT41             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SEGCOBRA             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA  LEITURA '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZZ.ZZZ.999   VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-ERR-MSG                 PIC  X(042)         VALUE SPACES.
       77  WRK-ERR-CHAVE               PIC  X(040)         VALUE SPACES.

       01  WRK-VALOR                   PIC  9(009)V9(04) COMP-3
                                                           VALUE ZEROS.
       01  WRK-PERCENTUAL              PIC  9(003)V9(04) COMP-3
                                                           VALUE ZEROS.
       01  WRK-VLR-T41                 PIC  9(009)V9(02) COMP-3
                                                           VALUE ZEROS.
       01  WRK-AG-CTA.
           05  WRK-AGENCIA             PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(003)         VALUE ' - '.
           05  WRK-CONTA               PIC  9(013)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           05 WRK-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE SPACES.
           05 WRK-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  WRK-DPROCM                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DPROCM-R                REDEFINES           WRK-DPROCM
                                       PIC  X(008).

       01  WRK-DFINAL                  PIC  9(008)         VALUE ZEROS.
       01  WRK-DFINAL-R                REDEFINES           WRK-DFINAL
                                       PIC  X(008).

       01  WRK-DATA-AGEND.
           05 WRK-ANO-AGEND            PIC  9(004)         VALUE ZEROS.
           05 WRK-MES-AGEND            PIC  9(002)         VALUE ZEROS.
           05 WRK-DIA-AGEND            PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-AUX                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX-X              REDEFINES           WRK-DATA-AUX
                                       PIC  X(008).

       01  WRK-DATA-AUX2               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-AUX2-X             REDEFINES          WRK-DATA-AUX2.
           05  WRK-ANO-AUX2            PIC  9(004).
           05  WRK-MES-AUX2            PIC  9(002).
           05  WRK-DIA-AUX2            PIC  9(002).

       01  WRK-PER-INI                 PIC  9(008)         VALUE ZEROS.
       01  WRK-PER-INI-X               REDEFINES WRK-PER-INI.
           05 WRK-ANO-PER-INI          PIC  9(004).
           05 WRK-MES-PER-INI          PIC  9(002).
           05 WRK-DIA-PER-INI          PIC  9(002).

       01  WRK-PER-FIM                 PIC  9(008)         VALUE ZEROS.
       01  WRK-PER-FIM-X               REDEFINES WRK-PER-FIM.
           05 WRK-ANO-PER-FIM          PIC  9(004).
           05 WRK-MES-PER-FIM          PIC  9(002).
           05 WRK-DIA-PER-FIM          PIC  9(002).

       01  WRK-EVENTO-INV              PIC  9(008) VALUE ZEROS.
       01  WRK-EVENTO-INV-R            REDEFINES WRK-EVENTO-INV
                                       PIC  X(008).

       01  WRK-TIMESTAMP-AUX.
           05 WRK-ANO-TMS              PIC  9(004)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-MES-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-DIA-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '-'.
           05 WRK-HOR-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIN-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-SEG-TMS              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MIL-TMS              PIC  9(006)         VALUE ZEROS.

       01  WRK-PER-CARENCIA            PIC  9(001)         VALUE ZEROS.
       01  WRK-QTD-MESES-CARE          PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-INICIO.
           05 WRK-DIA-INI              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-INI              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-INI              PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           05 WRK-DIA-FIM              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-MES-FIM              PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(001)         VALUE '.'.
           05 WRK-ANO-FIM              PIC  9(004)         VALUE ZEROS.

       01  WRK-DTINI-INV               PIC  9(008)         VALUE ZEROS.
       01  WRK-DTINI-INV-R             REDEFINES WRK-DTINI-INV.
           05 WRK-INI-ANO-INV          PIC  9(004).
           05 WRK-INI-MES-INV          PIC  9(002).
           05 WRK-INI-DIA-INV          PIC  9(002).

       01  WRK-DTFIM-INV               PIC  9(008)         VALUE ZEROS.
       01  WRK-DTFIM-INV-R             REDEFINES WRK-DTFIM-INV.
           05 WRK-FIM-ANO-INV          PIC  9(004).
           05 WRK-FIM-MES-INV          PIC  9(002).
           05 WRK-FIM-DIA-INV          PIC  9(002).

       01  WRK-DATA-0025               PIC  9(009)        VALUE ZEROS.
       01  WRK-DATA-0025-R             REDEFINES WRK-DATA-0025.
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-0025            PIC  9(004).
           05  WRK-MES-0025            PIC  9(002).
           05  WRK-DIA-0025            PIC  9(002).

       01  WRK-0025                    PIC  9(009) COMP-3 VALUE ZEROS.

060509 01  WRK-S9-5                    PIC S9(005)         VALUE ZEROS.
060509 01  FILLER                      REDEFINES WRK-S9-5.
060509     05  WRK-9-5                 PIC  9(005).
060509
060509 01  WRK-S9-3                    PIC S9(003)         VALUE ZEROS.
060509 01  FILLER                      REDEFINES WRK-S9-3.
060509     05  WRK-9-3                 PIC  9(003).
060509
060509 01  WRK-S9-13                   PIC S9(013)         VALUE ZEROS.
060509 01  FILLER                      REDEFINES WRK-S9-13.
060509     05  WRK-9-13                PIC  9(013).

RU0125 01 WRK-CPTCAO-GFCTB041          PIC  X(010) VALUE SPACES.
RU0125 01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB041.
           05 WRK-CPTCAO-B041          PIC 9(002).
RU0125     05 FILLER                   PIC X(008).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES *'.
      *----------------------------------------------------------------*

       01  WRK-CHV-ATU.
           05 WRK-CHV-ATU-CLI.
             10 WRK-CHV-ATU-BANCO      PIC  9(003)         VALUE ZEROS.
             10 WRK-CHV-ATU-AGENCIA    PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ATU-CONTA      PIC  9(013)         VALUE ZEROS.
           05 WRK-CHV-ATU-DADOS.
             10 WRK-CHV-ATU-TARIFA     PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ATU-DT-OCOR    PIC  9(008)         VALUE ZEROS.
             10 WRK-CHV-ATU-DT-OCOR-X  REDEFINES WRK-CHV-ATU-DT-OCOR
                                       PIC  X(008).

       01  WRK-CHV-ANT.
           05 WRK-CHV-ANT-CLI.
             10 WRK-CHV-ANT-BANCO      PIC  9(003)         VALUE ZEROS.
             10 WRK-CHV-ANT-AGENCIA    PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ANT-CONTA      PIC  9(013)         VALUE ZEROS.
           05 WRK-CHV-ANT-DADOS.
             10 WRK-CHV-ANT-TARIFA     PIC  9(005)         VALUE ZEROS.
             10 WRK-CHV-ANT-DT-OCOR    PIC  9(008)         VALUE ZEROS.
             10 WRK-CHV-ANT-DT-OCOR-X  REDEFINES WRK-CHV-ANT-DT-OCOR
                                       PIC  X(008).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA PARA BRAD1285'.
      *----------------------------------------------------------------*

       01  WRK-MSG001                  PIC  X(030)         VALUE
           '* ERRO NO ACESSO A BRAD1285 *'.

       01  WRK-AREA-BRAD1285.
           05  WRK-DT-INFERIOR-BRAD    PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-QTDE-DIAS-1285      PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-DT-SUPERIOR-BRAD    PIC  9(008) COMP-3  VALUE ZEROS.
           05  WRK-MENSAGEM            PIC  X(050)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(016)         VALUE
           'AREA DA BRAD0025'.
      *----------------------------------------------------------------*

       01  WRK-DATA-ENTRADA            PIC   9(009) COMP-3  VALUE ZEROS.
       01  WRK-NUMERO-DIAS             PIC  S9(005) COMP-3  VALUE ZEROS.
       01  WRK-DATA-SAIDA              PIC   9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA BRAD1640'.
      *----------------------------------------------------------------*

       01  1640-CAMPOS.
           02  1640-DATA-ENTRADA    PIC X(10)  VALUE SPACES.
           02  1640-FORMATO-ENTRADA PIC 9(2)   VALUE ZEROS.
           02  1640-TIPO-INCREMENTO PIC X(2)   VALUE SPACES.
           02  1640-INCREMENTO      PIC 9(3)   VALUE ZEROS.
           02  1640-FORMATO-SAIDA   PIC 9(02)  VALUE ZEROS.
           02  1640-DATA-SAIDA      PIC X(10)  VALUE SPACES.
           02  1640-MENSAGEM        PIC X(80)  VALUE SPACES.

       01  WRK-DATA-DB2-1640.
           05  WRK-DIA-DB2-1640        PIC  9(002).
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-DB2-1640        PIC  9(002).
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-DB2-1640        PIC  9(004).

       01  WRK-NUMERO-MESES            PIC  9(003) VALUE ZEROS.

       01  WRK-DATA-SAIDA1640.
           05  WRK-DIA-SAIDA1640       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-MES-SAIDA1640       PIC  9(002) VALUE ZEROS.
           05  FILLER                  PIC  X(001) VALUE '.'.
           05  WRK-ANO-SAIDA1640       PIC  9(004) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DA BRAD7600'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05 WRK-DATA-JULIANA         PIC  9(005) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAMMDD          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-DATA-AAAAMMDD        PIC  9(009) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSS          PIC  9(007) COMP-3  VALUE ZEROS.
           05 WRK-HORA-HHMMSSMMMMMM    PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TIMESTAMP.
             10 WRK-ANO                PIC  9(004)         VALUE ZEROS.
             10 WRK-MES                PIC  9(002)         VALUE ZEROS.
             10 WRK-DIA                PIC  9(002)         VALUE ZEROS.
             10 WRK-HOR                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIN                PIC  9(002)         VALUE ZEROS.
             10 WRK-SEG                PIC  9(002)         VALUE ZEROS.
             10 WRK-MIL                PIC  9(006)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* MENSAGEM DE ERRO DE FILE-STATUS *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-TEXTO.
           05 FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
           05 WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
           05 FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05 WRK-NOME-ARQ             PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
           05 WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
           05 FILLER                   PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ARQ. SAIDA *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTVA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
JC0413       INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      ******************************************************************
      *    CHAMA AS ROTINAS A SEREM PROCESSADAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-ADPACOTE     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCEDIMENTOS INICIAIS E BUSCAR DATA DE PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           OPEN INPUT  ADPACOTE
                OUTPUT FRANQT92
                       FRANQT41
                       SEGCOBRA.

           PERFORM 1010-DATA-PROCESSAMENTO

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'BRAD7600'             USING WRK-DATA-HORA.

           MOVE WRK-ANO                TO WRK-ANO-TMS.
           MOVE WRK-MES                TO WRK-MES-TMS.
           MOVE WRK-DIA                TO WRK-DIA-TMS.
           MOVE WRK-HOR                TO WRK-HOR-TMS.
           MOVE WRK-MIN                TO WRK-MIN-TMS.
           MOVE WRK-SEG                TO WRK-SEG-TMS.
           MOVE WRK-MIL                TO WRK-MIL-TMS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    BUSCAR DATA PROCESSAMENTO.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1010-DATA-PROCESSAMENTO         SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ANTER,
                   DPROCM_ATUAL,
                   DPROCM_PROX
             INTO
                   :GFCTB0A1.DPROCM-ANTER,
                   :GFCTB0A1.DPROCM-ATUAL,
                   :GFCTB0A1.DPROCM-PROX
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM ' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE  SQLCODE           TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE  SPACES            TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE           INTO WRK-DPROCM-R.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS.                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ADPACOTE.

           PERFORM 1120-TESTAR-FS-FRANQT92.

           PERFORM 1130-TESTAR-FS-FRANQT41.

           PERFORM 1140-TESTAR-FS-SEGCOBRA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO ADPACOTE.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ADPACOTE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ADPACOTE         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ADPACOTE    TO WRK-FILE-STATUS
               MOVE 'ADPACOTE'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT92.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-FRANQT92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT92    TO WRK-FILE-STATUS
               MOVE 'FRANQT92'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO FRANQT41.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1130-TESTAR-FS-FRANQT41         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FRANQT41         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-FRANQT41    TO WRK-FILE-STATUS
               MOVE 'FRANQT41'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO SEGCOBRA.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1140-TESTAR-FS-SEGCOBRA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGCOBRA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-SEGCOBRA    TO WRK-FILE-STATUS
               MOVE 'SEGCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE ARQUIVO ESTA VAZIO.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ADPACOTE.

           IF WRK-FS-ADPACOTE          EQUAL '10'
              DISPLAY '******************** GFCT1811 ******************'
              DISPLAY '*                                              *'
              DISPLAY '*          ARQUIVO DE ADPACOTE VAZIO           *'
              DISPLAY '*                                              *'
              DISPLAY '******************** GFCT1811 ******************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO ADPACOTE                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-ADPACOTE               SECTION.
      *----------------------------------------------------------------*

           READ ADPACOTE.

           IF  WRK-FS-ADPACOTE         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ADPACOTE.

060509     MOVE VE-CBCO-DSTNO-MOVTO OF ADPACOTE TO WRK-S9-3.
060509     MOVE WRK-9-3                         TO WRK-CHV-ATU-BANCO.
060509     MOVE VE-CAG-DSTNO-MOVTO  OF ADPACOTE TO WRK-S9-5.
060509     MOVE WRK-9-5                         TO WRK-CHV-ATU-AGENCIA.
060509     MOVE VE-CCTA-DSTNO-MOVTO OF ADPACOTE TO WRK-S9-13.
060509     MOVE WRK-9-13                        TO WRK-CHV-ATU-CONTA.
060509     MOVE VE-CSERVC-TARIF     OF ADPACOTE TO WRK-S9-5.
060509     MOVE WRK-9-5                         TO WRK-CHV-ATU-TARIFA.
           MOVE VE-DOCOR-EVNTO      OF ADPACOTE TO WRK-DATA-DB2.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE           INTO WRK-CHV-ATU-DT-OCOR-X.

           STRING WRK-ANO-DB2 WRK-MES-DB2 WRK-DIA-DB2
           DELIMITED BY SIZE           INTO WRK-EVENTO-INV.

           ADD 1                       TO ACU-LIDOS-ADPACOTE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY ' '.
VINI  *    XISPLAY ' '.
VINI  *    XISPLAY 'REINICIO...CHV.ADPACOTE:'.
VINI  *    XISPLAY 'AGENCIA          = 'WRK-CHV-ATU-AGENCIA.
VINI  *    XISPLAY 'CONTA            = 'WRK-CHV-ATU-CONTA.
VINI  *    XISPLAY 'TARIFA           = 'WRK-CHV-ATU-TARIFA.
VINI  *    XISPLAY 'DATA OCORRENCIA  = 'VE-DOCOR-EVNTO OF ADPACOTE.
VINI  *    XISPLAY 'DATA AGENDAMENTO = 'VE-DAGNDA-DEB-MOVTO
VINI  *                                                OF ADPACOTE.

           IF  WRK-CHV-ATU NOT EQUAL WRK-CHV-ANT
VINI  *        XISPLAY ' '
VINI  *        XISPLAY 'SELECT-GFTCB009 - DATA INICIO'
               PERFORM 3010-SELECT-GFTCB009
           END-IF.

           IF  WRK-DTINI-INV NOT EQUAL ZEROS
VINI  *        XISPLAY ' '
VINI  *        XISPLAY 'SELECT-GFTCB0A2 - DADOS CARENCIA'
               PERFORM 3015-SELECT-GFTCB0A2
           ELSE
               ADD 1 TO ACU-DESPREZADOS
060509         MOVE REG-IGFCTVE OF ADPACOTE TO REG-IGFCTVE OF SEGCOBRA
060509         MOVE VE-VTARIF-LIQ-MOVTO OF ADPACOTE
060509                                  TO GFCTVA-VTARIF-LIQ-MOVTO
               PERFORM 3900-GRAVAR-SEGCOBRA
VINI  *        XISPLAY ' '
VINI  *        XISPLAY 'GRAVOU SEGCOBRA, DATA INICIO 009 = ZEROS'
           END-IF.

           MOVE WRK-CHV-ATU TO WRK-CHV-ANT.

           PERFORM 2100-LER-ADPACOTE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3010-SELECT-GFTCB009       SECTION.
      *----------------------------------------------------------------*

060509     MOVE VE-CAG-DSTNO-MOVTO  OF ADPACOTE
                                    TO CJUNC-DEPDC       OF GFCTB009.
060509     MOVE VE-CCTA-DSTNO-MOVTO OF ADPACOTE
                                    TO CCTA-CLI          OF GFCTB009.
060509     MOVE VE-CSERVC-TARIF     OF ADPACOTE
                                    TO CSERVC-TARIF      OF GFCTB009.
           MOVE 0                   TO CINDCD-EXCL-REG   OF GFCTB009.
060509     MOVE VE-DOCOR-EVNTO      OF ADPACOTE
                                    TO DINIC-ADSAO-INDVD OF GFCTB009
                                       DFIM-ADSAO-INDVD  OF GFCTB009.

           EXEC SQL
             SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   CSERVC_TARIF,
                   HINCL_REG,
                   CFUNC_MANUT_INCL,
                   CFUNC_MANUT,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   HMANUT_REG,
                   CINDCD_EXCL_REG
             INTO
                   :GFCTB009.CJUNC-DEPDC,
                   :GFCTB009.CCTA-CLI,
                   :GFCTB009.CSERVC-TARIF,
                   :GFCTB009.HINCL-REG,
                   :GFCTB009.CFUNC-MANUT-INCL,
                   :GFCTB009.CFUNC-MANUT,
                   :GFCTB009.DINIC-ADSAO-INDVD,
                   :GFCTB009.DFIM-ADSAO-INDVD,
                   :GFCTB009.HMANUT-REG,
                   :GFCTB009.CINDCD-EXCL-REG
             FROM   DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                   CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC       AND
                   CCTA_CLI            = :GFCTB009.CCTA-CLI          AND
                   CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF      AND
                   CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG   AND
                   DINIC_ADSAO_INDVD  <= :GFCTB009.DINIC-ADSAO-INDVD AND
                   DFIM_ADSAO_INDVD   >= :GFCTB009.DFIM-ADSAO-INDVD  AND
                   HINCL_REG           =
                (SELECT MAX(HINCL_REG)
                 FROM DB2PRD.ADSAO_INDVD_PCOTE
                 WHERE
                   CJUNC_DEPDC         = :GFCTB009.CJUNC-DEPDC       AND
                   CCTA_CLI            = :GFCTB009.CCTA-CLI          AND
                   CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF      AND
                   CINDCD_EXCL_REG     = :GFCTB009.CINDCD-EXCL-REG   AND
                   DINIC_ADSAO_INDVD  <= :GFCTB009.DINIC-ADSAO-INDVD AND
                   DFIM_ADSAO_INDVD   >= :GFCTB009.DFIM-ADSAO-INDVD)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
                MOVE 'DB2'               TO ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE' TO ERR-DBD-TAB
                MOVE 'SELECT'            TO ERR-FUN-COMANDO
                MOVE  SQLCODE            TO ERR-SQL-CODE
                MOVE '0511'              TO ERR-LOCAL
                MOVE  SPACES             TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                   EQUAL ZEROS
               MOVE DINIC-ADSAO-INDVD TO WRK-DATA-INICIO
           ELSE
               MOVE ZEROS             TO WRK-DATA-INICIO
           END-IF.

VINI  *    XISPLAY 'DINIC-ADSAO-INDVD = 'WRK-DATA-INICIO.

           MOVE WRK-DIA-INI TO WRK-INI-DIA-INV.
           MOVE WRK-MES-INI TO WRK-INI-MES-INV.
           MOVE WRK-ANO-INI TO WRK-INI-ANO-INV.

      *----------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3015-SELECT-GFTCB0A2       SECTION.
      *----------------------------------------------------------------*

060509     MOVE VE-CSERVC-TARIF OF ADPACOTE
                                       TO CSERVC-TARIF      OF GFCTB0A2.
060509     MOVE VE-DOCOR-EVNTO  OF ADPACOTE
                                       TO DINIC-VGCIA-TARIF OF GFCTB0A2
                                          DFIM-VGCIA-TARIF  OF GFCTB0A2.

           EXEC SQL
             SELECT
                   CINDCD_PER_CAREN,
                   QMES_CAREN_TARIF,
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF
             INTO
                   :GFCTB0A2.CINDCD-PER-CAREN,
                   :GFCTB0A2.QMES-CAREN-TARIF,
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF      AND
                   DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF AND
                   DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF  (SQLCODE                    NOT EQUAL ZEROS) OR
               (SQLWARN0                   EQUAL 'W')
                MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF  '  TO ERR-DBD-TAB
                MOVE 'SELECT    '          TO ERR-FUN-COMANDO
                MOVE SQLCODE               TO ERR-SQL-CODE
                MOVE '0512'                TO ERR-LOCAL
                MOVE SPACES                TO ERR-SEGM
                PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                   EQUAL ZEROS
               MOVE CINDCD-PER-CAREN TO WRK-PER-CARENCIA
               MOVE QMES-CAREN-TARIF TO WRK-QTD-MESES-CARE
           END-IF.

           IF WRK-PER-CARENCIA EQUAL ZEROS
               PERFORM 3017-CALC-DTFIM
           ELSE
               IF WRK-PER-CARENCIA EQUAL 1
                   PERFORM 3020-CALC-DTFIM-ULTIMO-DIA
               ELSE
                   IF WRK-PER-CARENCIA EQUAL 2
                       PERFORM 3025-CALC-DTFIM-MES
                   END-IF
               END-IF
           END-IF.

           MOVE WRK-DIA-FIM TO WRK-FIM-DIA-INV.
           MOVE WRK-MES-FIM TO WRK-FIM-MES-INV.
           MOVE WRK-ANO-FIM TO WRK-FIM-ANO-INV.

VINI  *    XISPLAY ' '.
VINI  *    XISPLAY 'TRATAMENTO PARA INICIO APURACAO'.
VINI  *    XISPLAY 'DATA OCORRENCIA = 'WRK-EVENTO-INV.
VINI  *    XISPLAY 'DATA INICIO     = 'WRK-DTINI-INV.
VINI  *    XISPLAY 'DATA FIM        = 'WRK-DTFIM-INV.
VINI  *    XISPLAY 'VE-PDESC-CAREN  = '
VINI  *                                VE-PDESC-CAREN-TARIF OF ADPACOTE.

           IF WRK-EVENTO-INV NOT LESS WRK-DTINI-INV    AND
                             NOT GREATER WRK-DTFIM-INV AND
060509        VE-PDESC-CAREN-TARIF OF ADPACOTE NOT EQUAL ZEROS
               PERFORM 3050-APURAR
           ELSE
               ADD 1  TO ACU-DESPREZADOS
060509         MOVE REG-IGFCTVE OF ADPACOTE TO REG-IGFCTVE OF SEGCOBRA
060509         MOVE VE-VTARIF-LIQ-MOVTO OF ADPACOTE
060509                                  TO GFCTVA-VTARIF-LIQ-MOVTO
               PERFORM 3900-GRAVAR-SEGCOBRA

VINI  *        XISPLAY ' '
VINI  *        XISPLAY 'GRAVOU SEGCOBRA, DT OCORRENCIA NAO ESTA ENTRE '
VINI  *        XISPLAY 'INICIO E FIM OBTIDAS COM O PER-CARENCIA'

           END-IF.

      *----------------------------------------------------------------*
       3015-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3017-CALC-DTFIM       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DIA-INI TO WRK-DIA-0025.
           MOVE WRK-MES-INI TO WRK-MES-0025.
           MOVE WRK-ANO-INI TO WRK-ANO-0025.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE WRK-DATA-0025          TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-ENTRADA.
JC0413     MOVE VE-QDIA-CAREN-TARIF    OF ADPACOTE
060509                                 TO WRK-NUMERO-DIAS.

VINI  *    XISPLAY 'CALC-DTFIM'.
VINI  *    XISPLAY 'WRK-DATA-ENTRADA    = 'WRK-DATA-ENTRADA.
VINI  *    XISPLAY 'VE-QDIA-CAREN-TARIF = 'VE-QDIA-CAREN-TARIF
VINI  *                                                    OF ADPACOTE.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-0025.
           MOVE WRK-DIA-0025           TO WRK-DIA-FIM.
           MOVE WRK-MES-0025           TO WRK-MES-FIM.
           MOVE WRK-ANO-0025           TO WRK-ANO-FIM.

VINI  *    XISPLAY 'WRK-DATA-SAIDA = 'WRK-DATA-SAIDA.

      *----------------------------------------------------------------*
       3017-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3020-CALC-DTFIM-ULTIMO-DIA       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DIA-INI TO WRK-DIA-0025.
           MOVE WRK-MES-INI TO WRK-MES-0025.
           MOVE WRK-ANO-INI TO WRK-ANO-0025.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE 01 TO WRK-DIA-0025

           IF WRK-MES-0025 EQUAL 12
               ADD  1  TO WRK-ANO-0025
               MOVE 01 TO WRK-MES-0025
           ELSE
               ADD 1 TO WRK-MES-0025
           END-IF.

           MOVE WRK-DATA-0025          TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-ENTRADA.
           MOVE -01                    TO WRK-NUMERO-DIAS.

VINI  *    XISPLAY '3020-CALC-DTFIM-ULTIMO-DIA'.
VINI  *    XISPLAY 'WRK-DATA-ENTRADA = 'WRK-DATA-ENTRADA.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-0025.
           MOVE WRK-DIA-0025           TO WRK-DIA-FIM.
           MOVE WRK-MES-0025           TO WRK-MES-FIM.
           MOVE WRK-ANO-0025           TO WRK-ANO-FIM.

VINI  *    XISPLAY 'DATA-FIM = 'WRK-DATA-SAIDA.

      *----------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3025-CALC-DTFIM-MES       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DATA-INICIO       TO 1640-DATA-ENTRADA.
           MOVE 09                    TO 1640-FORMATO-ENTRADA
                                         1640-FORMATO-SAIDA.
           MOVE WRK-QTD-MESES-CARE    TO 1640-INCREMENTO.
           MOVE 'M'                   TO 1640-TIPO-INCREMENTO.

VINI  *    XISPLAY '3025-CALC-DTFIM-MES'.
VINI  *    XISPLAY '1640-DATA-ENTRADA  = '1640-DATA-ENTRADA.
VINI  *    XISPLAY 'WRK-QTD-MESES-CARE = 'WRK-QTD-MESES-CARE.

           CALL 'BRAD1640'             USING 1640-DATA-ENTRADA
                                             1640-FORMATO-ENTRADA
                                             1640-TIPO-INCREMENTO
                                             1640-INCREMENTO
                                             1640-FORMATO-SAIDA
                                             1640-DATA-SAIDA
                                             1640-MENSAGEM.

           IF RETURN-CODE              NOT EQUAL ZEROS
              DISPLAY '**** GFCT1811 - BRAD 1640 ****'
              DISPLAY '*                            *'
              DISPLAY '* ERRO NA BRAD               *'
              DISPLAY '* '1640-MENSAGEM
              DISPLAY '*                            *'
              DISPLAY '**** GFCT1811 - BRAD 1640 ****'
              PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE 1640-DATA-SAIDA        TO WRK-DATA-FIM.

VINI  *    XISPLAY '1640-DATA-SAIDA = '1640-DATA-SAIDA.

      ** TRATAMENTO PARA OBTER O ULTIMO DIA DO MES *********************

           MOVE WRK-DIA-FIM TO WRK-DIA-0025.
           MOVE WRK-MES-FIM TO WRK-MES-0025.
           MOVE WRK-ANO-FIM TO WRK-ANO-0025.

           INITIALIZE WRK-DATA-SAIDA
                      WRK-DATA-ENTRADA.

           MOVE 01 TO WRK-DIA-0025

           IF WRK-MES-0025 EQUAL 12
               ADD  1  TO WRK-ANO-0025
               MOVE 01 TO WRK-MES-0025
           ELSE
               ADD 1 TO WRK-MES-0025
           END-IF.

           MOVE WRK-DATA-0025          TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-ENTRADA.
           MOVE -01                    TO WRK-NUMERO-DIAS.

           CALL 'BRAD0025'             USING WRK-DATA-ENTRADA
                                             WRK-NUMERO-DIAS
                                             WRK-DATA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'ERRO NA BRAD0025 - DADOS INCONSISTENTES'
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DATA-SAIDA         TO WRK-0025.
           MOVE WRK-0025               TO WRK-DATA-0025.
           MOVE WRK-DIA-0025           TO WRK-DIA-FIM.
           MOVE WRK-MES-0025           TO WRK-MES-FIM.
           MOVE WRK-ANO-0025           TO WRK-ANO-FIM.

VINI  *    XISPLAY 'ULTIMO DIA DO MES BRAD0025'.
VINI  *    XISPLAY 'WRK-DATA-SAIDA = 'WRK-DATA-SAIDA.

      *---------------------------------------------------------------*
       3025-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3050-APURAR       SECTION.
      *----------------------------------------------------------------*

VINI  *    XISPLAY ' '.
VINI  *    XISPLAY 'INICIO DA APURACAO'.
VINI  *    XISPLAY 'TIPO DE CARENCIA = 'VE-CINDCD-INIC-CAREN OF ADPACOTE

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

060509     MOVE VE-GFCTB092     OF ADPACOTE TO GFCTVA-GFCTB092.
060509     MOVE REG-IGFCTVE     OF ADPACOTE TO REG-IGFCTVE OF SEGCOBRA.
060509     MOVE VE-CTRATO-MOVTO OF ADPACOTE TO GFCTVA-CTRATO-MOVTO.
           MOVE ZEROS                  TO GFCTVA-QTD-FQ-COMPON
                                          GFCTVA-QTD-EXC-DIA
                                          GFCTVA-QTD-EXC-ACU
                                          GFCTVA-AGENCIA-CREC
                                          GFCTVA-CONTA-CREC
                                          GFCTVA-CSGMTO-GSTAO-TARIF
                                          GFCTVA-PACOTE-EVENTO.
           MOVE SPACES                 TO GFCTVA-FLAG-DATA.

VINI  *    XISPLAY ' '.
VINI  *    XISPLAY 'CALCULAR-VLR-COBRAR'.

           PERFORM 3400-CALCULAR-VLR-COBRAR

           IF  GFCTVA-VTARIF-LIQ-MOVTO EQUAL ZEROS
VINI  *        XISPLAY ' '
VINI  *        XISPLAY 'GFCTVA-VTARIF-LIQ-MOVTO IGUAL A ZEROS'
               PERFORM 3500-GRAVAR-FRANQUEADOS
               PERFORM 3600-GRAVAR-FRANQUIA41
               ADD 1 TO ACU-NAO-COBRADOS
               GO TO 3050-99-FIM
           END-IF.

060509     IF  VE-VTARIF-LIQ-MOVTO OF ADPACOTE
                                   NOT EQUAL GFCTVA-VTARIF-LIQ-MOVTO
               PERFORM 3600-GRAVAR-FRANQUIA41

VINI  *        XISPLAY 'GRAVOU FRANQT41 - VLR LIQ VE DIFERENTE '
VINI  *                                             'VLR LIQ VA'
           END-IF.

           PERFORM 3900-GRAVAR-SEGCOBRA.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CALCULAR VALOR A COBRAR                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-CALCULAR-VLR-COBRAR        SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-PERCENTUAL = 100 - VE-PDESC-CAREN-TARIF
060509                                         OF ADPACOTE.

VINI  *    XISPLAY 'PDESC / PDESC CALC' VE-PDESC-CAREN-TARIF OF ADPACOTE
      *                                             ' / ' WRK-PERCENTUAL

           IF  WRK-PERCENTUAL           EQUAL ZEROS
               MOVE GFCTVA-VTARIF-LIQ-MOVTO TO WRK-VLR-T41
               MOVE ZEROS               TO GFCTVA-VTARIF-LIQ-MOVTO

VINI  *        XISPLAY 'PERC ZERADO    ' WRK-VLR-T41 '/'
VINI  *                                    GFCTVA-VTARIF-LIQ-MOVTO

           ELSE
               COMPUTE WRK-VALOR = (VE-VTARIF-LIQ-MOVTO
060509                                      OF ADPACOTE / 100)
               COMPUTE GFCTVA-VTARIF-LIQ-MOVTO =
                                            (WRK-VALOR * WRK-PERCENTUAL)
               COMPUTE WRK-VLR-T41 =
060509                   (VE-VTARIF-LIQ-MOVTO OF ADPACOTE
                                          - GFCTVA-VTARIF-LIQ-MOVTO)

VINI  *        XISPLAY 'VTARIF-LIQ / 100  ' WRK-VALOR
VINI  *        XISPLAY 'VLR P/ 92         ' WRK-VALOR ' * '
VINI  *                WRK-PERCENTUAL    ' = ' GFCTVA-VTARIF-LIQ-MOVTO
VINI  *        XISPLAY 'VLR P/ 41         ' VE-VTARIF-LIQ-MOVTO
VINI  *                                     OF ADPACOTE ' - '
VINI  *                GFCTVA-VTARIF-LIQ-MOVTO ' = ' WRK-VLR-T41

           END-IF.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR FRANQUEADOS                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-GRAVAR-FRANQUEADOS         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVA-VTARIF-LIQ-MOVTO   = ZEROS
               MOVE 16                  TO GFCTVA-CSIT-EVNTO-RECBD
               MOVE ZEROS               TO GFCTVA-VTARIF-LIQ-MOVTO
060509         MOVE VE-DAGNDA-DEB-MOVTO OF ADPACOTE
                                        TO GFCTVA-DEFETV-DEB-MOVTO
           END-IF.

VINI  *    XISPLAY 'VLR GRAVADO FRANQ92.' GFCTVA-VTARIF-LIQ-MOVTO.

           WRITE REG-FRANQT92         FROM GFCTVA-GFCTB092.

           PERFORM 1120-TESTAR-FS-FRANQT92.

           ADD 1                        TO ACU-GRAVA-FRANQT92.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR FRANQUIA 41                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-GRAVAR-FRANQUIA41          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTV4-GFCTB041.

           INITIALIZE GFCTV4-GFCTB041.

           MOVE 4                      TO GFCTV4-CIDTFD-TPO-DESC.
           MOVE 2                      TO GFCTV4-CIDTFD-MOTVO-DESC.
           MOVE ZEROS                  TO GFCTV4-CIDTFD-DESC.
           MOVE 1                      TO GFCTV4-CPARM-DESC.
060509     MOVE VE-QEVNTO-AGRUP        OF ADPACOTE
                                       TO GFCTV4-QEVNTO-DESC.
           MOVE WRK-VLR-T41            TO GFCTV4-VCONCS-DESC.
           MOVE SPACES                 TO GFCTV4-CCONTR-FLEXZ.
060509     MOVE VE-DENVIO-MOVTO-TARIF  OF ADPACOTE
                                       TO GFCTV4-DENVIO-MOVTO-TARIF.
RU0125     MOVE  GFCTV4-DENVIO-MOVTO-TARIF
RU0125                                 TO WRK-CPTCAO-GFCTB041.
RU0125     MOVE  WRK-CPTCAO-B041       TO GFCTV4-CPTCAO.
060509     MOVE VE-CROTNA-ORIGE-MOVTO  OF ADPACOTE
                                       TO GFCTV4-CROTNA-ORIGE-MOVTO.
060509     MOVE VE-CNRO-ARQ-MOVTO      OF ADPACOTE
                                       TO GFCTV4-CNRO-ARQ-MOVTO.
060509     MOVE VE-CSEQ-MOVTO          OF ADPACOTE
                                       TO GFCTV4-CSEQ-MOVTO.
060509     MOVE VE-CBCO-DSTNO-MOVTO    OF ADPACOTE
                                       TO GFCTV4-CBCO-DSTNO-MOVTO.
060509     MOVE VE-CAG-DSTNO-MOVTO     OF ADPACOTE
                                       TO GFCTV4-CAG-DSTNO-MOVTO.
060509     MOVE VE-CCTA-DSTNO-MOVTO    OF ADPACOTE
                                       TO GFCTV4-CCTA-DSTNO-MOVTO.
060509     MOVE VE-CPAB-DSTNO-MOVTO    OF ADPACOTE
                                       TO GFCTV4-CPAB-DSTNO-MOVTO.
060509     MOVE VE-CSERVC-TARIF        OF ADPACOTE
                                       TO GFCTV4-CSERVC-TARIF.
060509     MOVE VE-DOCOR-EVNTO         OF ADPACOTE
                                       TO GFCTV4-DOCOR-EVNTO.
060509     MOVE VE-HPREST-SERVC-MOVTO  OF ADPACOTE
                                       TO GFCTV4-HPREST-MOVTO.
           MOVE WRK-TIMESTAMP-AUX      TO GFCTV4-IDTFD-TIMESTAMP.

VINI  *    XISPLAY 'VLR GRAVADO FRANQ41.' GFCTV4-VCONCS-DESC.

           WRITE GFCTV4-GFCTB041.

           PERFORM 1130-TESTAR-FS-FRANQT41.

           ADD 1                       TO ACU-GRAVA-FRANQT41.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    GRAVAR SEGCOBRA                                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-GRAVAR-SEGCOBRA            SECTION.
      *----------------------------------------------------------------*

060509     MOVE GFCTVA-VTARIF-LIQ-MOVTO TO
060509                                  VE-VTARIF-LIQ-MOVTO OF SEGCOBRA.

060509     WRITE REG-IGFCTVE OF SEGCOBRA.

           PERFORM 1140-TESTAR-FS-SEGCOBRA.

           ADD 1                       TO ACU-GRAVA-SEGCOBRA.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCEDIMENTOS REALIZADOS NO FINAL DO PROCESSAMENTO.         *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-TOTAIS.

           CLOSE ADPACOTE
                 FRANQT92
                 FRANQT41
                 SEGCOBRA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITIR TOTAIS                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-TOTAIS              SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ADPACOTE     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1811 ******************'.
           DISPLAY '*                                              *'.
           DISPLAY '*              PROGRAMA GFCT1811               *'.
           DISPLAY '*              -----------------               *'.
           DISPLAY '*                                              *'.
           DISPLAY '*  LIDOS NO ARQUIVO ADPACOTE...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '*  REGISTROS NAO PROCESSADOS...: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT92     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT92: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-FRANQT41     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO FRANQT41: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-GRAVA-SEGCOBRA     TO WRK-MASCARA.

           DISPLAY '*  GRAVADOS NO ARQUIVO SEGCOBRA: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.

           MOVE ACU-NAO-COBRADOS       TO WRK-MASCARA.

           DISPLAY '*  REGISTROS NAO COBRADOS......: ' WRK-MASCARA '  *'
           DISPLAY '*                                              *'.
           DISPLAY '******************** GFCT1811 ******************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1811'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

