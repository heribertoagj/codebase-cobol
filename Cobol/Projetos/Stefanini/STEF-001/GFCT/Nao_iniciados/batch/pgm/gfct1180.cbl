      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1180.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT1180                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS AUGUSTO BOSCATO   - CPM PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   09/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR  REGISTROS COBRADOS - COBRANCA    *
      *                    UNIFICADA.                                  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ATLZUNIF                  I#GFCTIL          *
      *                    DETUNIFI                  I#GFCTIL          *
      *                    CTRLDATA                  I#GFCT06          *
      *                    SAIUNIFI                  I#GFCTIL          *
      *                    FITCONTB                  I#GFCTJH          *
      *                    OCOR1180                  I#GFCTIL          *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                       INCLUDE/BOOK        *
      *                    DB2PRD.MOVTO_EVNTO_CRRTT  GFCTB092          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA DE COMUNICACAO COM A POOL7100               *
      *    I#GFCT0M - AREA DE COMUNICACAO DE ERROS                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - MODULO PARA CONEXAO COM DB2                      *
      *    POOL7100 - MODULO PARA TRATAMENTO DE ERROS                  *
      *    GFCT5525 - MODULO PARA ATUALIZAR SITUACAO EVENTO            *
      *================================================================*
      *                      A L T E R A C A O                         *
      *================================================================*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   22/11/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   INIBIR ATUALIZACAO DO CPO DAGNDA-DEB-MOVTO  *
      *================================================================*
      *================================================================*
      *    PROGRAMA....:   ALTERACAO                                   *
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   16/01/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ATUALIZAR AG/CTA DEBITO OU CENTRALIZADORA,  *
      *                    EM CASO DE TRAG NO MORA/POUP                *
      *----------------------------------------------------------------*
      *================================================================*
      *                        A L T E R A C A O                       *
      *================================================================*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP. 50  *
      *    DATA........:   03/05/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ACERTOS DIVERSOS                            *
      *     04 - MOVER 14 P/ SITUACAO-EVENTO  EM CASO DE ERRO          *
      *     05 - SUBSTITUIR ACESSO DA GFCT5525 POR UPDATE E ARQUIVO    *
      *     06 - DISPLAY SE REG ATLZUNIF NAO ENCONTRADO NO DETAUNIF    *
      *          LIMITADO A 1000 OCORRENCIAS                           *
      *     07 - SE COD-RETORNO = 001 OU 012, GRAVAR REGISTRO NO ARQUI-*
      *          VO "OCOR1180 - ARQUIVO DE ERROS DE PROCESSAMENTO      *
      *----------------------------------------------------------------*
070625*================================================================*
070625*                        A L T E R A C A O                       *
070625*================================================================*
070625*    ANALISTA....:   VINICIUS C. MADUREIRA   - PROCKWORK GP. 50  *
070625*    DATA........:   25/06/2007                                  *
070625*----------------------------------------------------------------*
070625*    OBJETIVO....:   TRATAR  FERIADO.                            *
070625*                                                                *
070625*    SE COD-RETORNO IGUAL A 'F' MOVER '2 - AGENDADO' PARA        *
070625*    CSIT_EVNTO_RECBD                                            *
070625*                                                                *
070625*----------------------------------------------------------------*
070626*================================================================*
070626*                        A L T E R A C A O                       *
070626*================================================================*
070626*    ANALISTA....:   VINICIUS C. MADUREIRA   - PROCKWORK GP. 50  *
070626*    DATA........:   26/06/2007                                  *
070626*----------------------------------------------------------------*
070626*    OBJETIVO....:   CRIACAO DO ARQUIVO ARQDEBIT - REGISTROS     *
070626*                    EFETIVAMENTE DEBITADOS                      *
070626*----------------------------------------------------------------*
071126*================================================================*
071126*=                      A L T E R A C A O                       =*
071126*================================================================*
071126*    ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50     *
071126*    DATA........:   26/11/2007                                  *
071126*----------------------------------------------------------------*
071126*    OBJETIVO....:   INCLUIR ARQUIVO CONTENDO INFORMACOES SOBRE  *
071126*                    REGISTROS COBRADOS                          *
071126*================================================================*
071127*================================================================*
071127*=                      A L T E R A C A O                       =*
071127*================================================================*
071127*   ANALISTA....:   VINICIUS C. MADUREIRA - PROCWORK GP. 50      *
071127*   DATA........:   27/11/2007                                   *
071127*----------------------------------------------------------------*
071127*   OBJETIVOS...:                                                *
071127*    SUBSTITUIR UPDATE DA TABELA GFCTBO92 POR GERACAO DE ARQUIVO *
071127*================================================================*
030408*================================================================*
030408*=                      A L T E R A C A O                       =*
030408*================================================================*
030408*   ANALISTA....:   WAGNER SILVA - PROCWORK GP. 50               *
030408*   DATA........:   03/04/2008                                   *
030408*----------------------------------------------------------------*
030408*   OBJETIVOS...:                                                *
030408*    SUBSTITUIR REGISTRO DE ATLZUNIF PELO DE DETUNIFI NA GERACAO *
030408*    DO ARQCOBR2                                                 *
030408*================================================================*
BI1210*=                      A L T E R A C A O                       =*
BI1210*================================================================*
BI1210*   ANALISTA....:   UBIRAJARA (BIRA) - PROCWORK GP. 50           *
BI1210*   DATA........:   20/12/2010                                   *
BI1210*----------------------------------------------------------------*
BI1210*   OBJETIVOS...:                                                *
BI1210*    MOVIMENTAR O CAMPO (COBR-NUM-PARC) DE DETUNIF PARA          *
BI1210*    (GFCT67-NUM-PARC) NA GRAVACAO DE ARQATU92.                  *
BI1210*================================================================*

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

           SELECT ATLZUNIF  ASSIGN     TO UT-S-ATLZUNIF
           FILE STATUS                 IS WRK-FS-ATLZUNIF.

           SELECT DETUNIFI ASSIGN      TO UT-S-DETUNIFI
           FILE STATUS                 IS WRK-FS-DETUNIFI.

           SELECT CTRLDATA ASSIGN      TO UT-S-CTRLDATA
           FILE STATUS                 IS WRK-FS-CTRLDATA.

           SELECT SAIUNIFI ASSIGN      TO UT-S-SAIUNIFI
           FILE STATUS                 IS WRK-FS-SAIUNIFI.

           SELECT FITCONTB ASSIGN      TO UT-S-FITCONTB
           FILE STATUS                 IS WRK-FS-FITCONTB.

           SELECT OCOR1180 ASSIGN      TO UT-S-OCOR1180
           FILE STATUS                 IS WRK-FS-OCOR1180.

05         SELECT ATLZB0H4 ASSIGN      TO UT-S-ATLZB0H4
05         FILE STATUS                 IS WRK-FS-ATLZB0H4.

070626     SELECT ARQDEBIT  ASSIGN     TO UT-S-ARQDEBIT
070626     FILE STATUS                 IS WRK-FS-ARQDEBIT.

071126     SELECT ARQCOBR2  ASSIGN     TO UT-S-ARQCOBR2
071126     FILE STATUS                 IS WRK-FS-ARQCOBR2.

071127     SELECT ARQATU92  ASSIGN     TO UT-S-ARQATU92
071127     FILE STATUS                 IS WRK-FS-ARQATU92.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  REGISTROS UNIFICADOS COBRADOS PARA ATUALIZACAO NA   *
      *            TAB. GFCTB092.                                      *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ATLZUNIF
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT:  DETALHES DE REGISTROS ENVIADOS PARA COBRANCA        *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  DETUNIFI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    INPUT:  CONTROLE DE DATAS GFCT                              *
      *            ORG. SEQUENCIAL     -   LRECL   =   100             *
      *----------------------------------------------------------------*

       FD  CTRLDATA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCT06'.

      *----------------------------------------------------------------*
      *    OUTPUT: DETALHES NAO ENCONTRADOS EM ATLZUNIF                *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAIUNIFI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQ. P/ GERACAO DA FITA4                            *
      *            ORG. SEQUENCIAL     -   LRECL   =   130             *
      *----------------------------------------------------------------*

       FD  FITCONTB
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTJH'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE ERROS DE PROCESSAMENTO                   *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  OCOR1180
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS  0 RECORDS.

       COPY 'I#GFCTIL'.

05    *----------------------------------------------------------------*
05    *    INPUT:  REGISTROS PARA ATUALIZACAO DA TABELA GFCTB0H4       *
05    *            ORG. SEQUENCIAL     -   LRECL   =   050             *
05    *----------------------------------------------------------------*
05
05     FD  ATLZB0H4
05         RECORDING MODE IS F
05         LABEL RECORD IS STANDARD
05         BLOCK CONTAINS  0 RECORDS.
05
05     COPY 'I#GFCTZD'.

070626*----------------------------------------------------------------*
070626*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
070626*            ORG. SEQUENCIAL     -   LRECL   =   080             *
070626*----------------------------------------------------------------*
070626
070626 FD  ARQDEBIT
070626     RECORDING MODE IS F
070626     LABEL RECORD IS STANDARD
070626     BLOCK CONTAINS  0 RECORDS.
070626
070626 COPY 'I#GFCTZJ'.
070626

071126*----------------------------------------------------------------*
071126*   OUTPUT:  REGISTROS EFETIVAMENTE DEBITADOS                    *
071126*            ORG. SEQUENCIAL     -   LRECL   =   340             *
071126*----------------------------------------------------------------*
071126
071126 FD  ARQCOBR2
071126     RECORDING MODE IS F
071126     LABEL RECORD IS STANDARD
071126     BLOCK CONTAINS  0 RECORDS.
071126
071126 COPY 'I#GFCTMW'.

071127*----------------------------------------------------------------*
071127*   OUTPUT:  REGISTROS PARA ATUALIZACAO NA TABELA GFCTB092       *
071127*            ORG. SEQUENCIAL     -   LRECL   =   090             *
071127*----------------------------------------------------------------*
071127
071127 FD  ARQATU92
071127     RECORDING MODE IS F
071127     LABEL RECORD IS STANDARD
071127     BLOCK CONTAINS  0 RECORDS.
071127
071127 COPY 'I#GFCT67'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)          VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* ACUMULADORES *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ATLZUNIF          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-DETUNIFI          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-CTRLDATA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-SAIUNIFI           PIC  9(009) COMP-3  VALUE ZEROS.
071126 77  ACU-GRAV-ARQCOBR2           PIC  9(009) COMP-3  VALUE ZEROS.
070626 77  ACU-GRAV-ARQDEBIT           PIC  9(009) COMP-3  VALUE ZEROS.
071127 77  ACU-GRAV-ARQATU92           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-FITCONTB           PIC  9(009) COMP-3  VALUE ZEROS.
05     77  ACU-GRAV-ATLZB0H4           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAV-OCOR1180           PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-DESPREZADOS             PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-COMMIT                  PIC  9(005) COMP-3  VALUE ZEROS.
06     77  ACU-NF-DETALHES             PIC  9(009) COMP-3  VALUE ZEROS.
06     77  ACU-ACHOU-DETALHES          PIC  9(009) COMP-3  VALUE ZEROS.
070625 77  ACU-FERIADO                 PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-VALOR-DEBITAR           PIC  9(11)V9(2) COMP-3 VALUE 0.
       77  WRK-VALOR-DEBITO            PIC  9(11)V9(2) COMP-3 VALUE 0.
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* TESTES DE FILE STATUS *'.
      *----------------------------------------------------------------*

05     77  WRK-FS-ATLZB0H4             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ATLZUNIF             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-DETUNIFI             PIC  X(002)         VALUE SPACES.
070626 77  WRK-FS-ARQDEBIT             PIC  X(002)         VALUE SPACES.
071126 77  WRK-FS-ARQCOBR2             PIC  X(002)         VALUE SPACES.
071127 77  WRK-FS-ARQATU92             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-CTRLDATA             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-SAIUNIFI             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-FITCONTB             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-OCOR1180             PIC  X(002)         VALUE SPACES.

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
           '* VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-MASCARA                 PIC  ZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77  WRK-SQLCODE                 PIC  9(003)         VALUE ZEROS.
       77  WRK-RETURN-CODE             PIC  9(003)         VALUE ZEROS.
       77  WRK-VLR-COBRADO-EVENTO      PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO                PIC  9(011)V99      VALUE ZEROS.
       77  WRK-VR-SALDO-PK             PIC  9(011)V99 COMP-3 VALUE 0.
       77  WRK-COBR-VALOR-DEBITAR-PK   PIC  9(011)V99 COMP-3 VALUE 0.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE MENSAGEM DE ERRO *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           05  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           05  WRK-ARQUIVO             PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           05  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CHAVES DOS ARQUIVOS *'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-ATLZUNIF.
           05 WRK-DATA-ENVIO-ATL       PIC  9(008)         VALUE ZEROS.
           05 WRK-DIG-CONTRATO-ATL     PIC  X(001)         VALUE SPACES.
           05 WRK-NUM-CONTRATO-ATL     PIC  9(007)         VALUE ZEROS.

       01  WRK-CHAVE-DETUNIFI.
           05 WRK-DATA-ENVIO-DET       PIC  9(008)         VALUE ZEROS.
           05 WRK-DIG-CONTRATO-DET     PIC  X(001)         VALUE SPACES.
           05 WRK-NUM-CONTRATO-DET     PIC  9(007)         VALUE ZEROS.

05    *----------------------------------------------------------------*
05     77  FILLER                      PIC  X(050)         VALUE
05         '*** AREA PARA VARIAVEIS DE DATAS ***'.
05    *----------------------------------------------------------------*

070626 01  WRK-DEFETV-DEB-MOVTO        PIC  X(010)         VALUE SPACES.

       01  FILLER.
           05 WRK-VALOR-RED-11-2     PIC  9(11)V9(2)       VALUE 0.
           05 FILLER REDEFINES       WRK-VALOR-RED-11-2.
              10 FILLER              PIC  X(2).
              10 WRK-VALOR-RED-9-2   PIC  9(9)V9(2).

       01  FILLER.
           05  WRK-VALOR-S           PIC +9(9)V9(2)        VALUE ZEROS.
           05  FILLER REDEFINES      WRK-VALOR-S.
               10 WRK-VALOR-S-S      PIC  X(1).
               10 WRK-VALOR-S-R      PIC  9(9)V9(2).

070626 01  WRK-DATA-DEB.
070626     03  WRK-DIA-DEB             PIC  9(002)         VALUE ZEROS.
070626     03  FILLER                  PIC  X(001)         VALUE '.'.
070626     03  WRK-MES-DEB             PIC  9(002)         VALUE ZEROS.
070626     03  FILLER                  PIC  X(001)         VALUE '.'.
070626     03  WRK-ANO-DEB             PIC  9(004)         VALUE ZEROS.

070626 01  WRK-DATA-EVT.
070626     03  WRK-DIA-EVT             PIC  9(002)         VALUE ZEROS.
070626     03  FILLER                  PIC  X(001)         VALUE '.'.
070626     03  WRK-MES-EVT             PIC  9(002)         VALUE ZEROS.
070626     03  FILLER                  PIC  X(001)         VALUE '.'.
070626     03  WRK-ANO-EVT             PIC  9(004)         VALUE ZEROS.

05     01  WRK-DATA.
05         03  W-DD                    PIC 9(02) VALUE ZEROS.
05         03  FILLER                  PIC X(01) VALUE SPACES.
05         03  W-MM                    PIC 9(02) VALUE ZEROS.
05         03  FILLER                  PIC X(01) VALUE SPACES.
05         03  W-AAAA                  PIC 9(04) VALUE ZEROS.
05
05     01  WRK-TIMESTAMP-H4.
05         03  WRK-DTIME.
05             05  WRK-AAAA            PIC 9(04)    VALUE ZEROS.
05             05  WRK-TRACO1          PIC X(01)    VALUE SPACES.
05             05  WRK-MM              PIC 9(02)    VALUE ZEROS.
05             05  WRK-TRACO2          PIC X(01)    VALUE SPACES.
05             05  WRK-DD              PIC 9(02)    VALUE ZEROS.
05         03  WRK-RESTO.
05             05  WRK-TRACO3          PIC X(01)    VALUE SPACES.
05             05  WRK-HR              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO1          PIC X(01)    VALUE SPACES.
05             05  WRK-MI              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO2          PIC X(01)    VALUE SPACES.
05             05  WRK-SG              PIC 9(02)    VALUE ZEROS.
05             05  WRK-PONTO3          PIC X(01)    VALUE SPACES.
05             05  WRK-DS              PIC 9(06)    VALUE ZEROS.
05
05     01  WRK-TIMESTAMP-SIST.
05         05  WRK-AAAA-S              PIC 9(04)    VALUE ZEROS.
05         05  WRK-MM-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-DD-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-HR-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-MI-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-SG-S                PIC 9(02)    VALUE ZEROS.
05         05  WRK-DS-S                PIC 9(06)    VALUE ZEROS.
05
05     01  WRK-DT-R                    PIC 9(09)    VALUE ZEROS.
05     01  WRK-DATA-AAAAMMDD.
05         05  WRK-DATA-LIXO           PIC 9(01)    VALUE ZEROS.
05         05  WRK-DATA-AAAA           PIC 9(04)    VALUE ZEROS.
05         05  WRK-DATA-MM             PIC 9(02)    VALUE ZEROS.
05         05  WRK-DATA-DD             PIC 9(02)    VALUE ZEROS.

PCT    01 WRK-CPTCAO-GFCTB0H4          PIC  X(026) VALUE SPACES.
       01 FILLER                       REDEFINES WRK-CPTCAO-GFCTB0H4.
           05 FILLER                   PIC X(023).
PCT        05 WRK-CPTCAO-B0H4          PIC 9(003).

05
05    *---------------------------------------------------------------*
05     01  FILLER                      PIC  X(050)         VALUE
05         '*** AREA DA POOL7600 ***'.
05    *---------------------------------------------------------------*
05
05     01  WRK-DATA-HORA.
05         03  WRK-DT-JULIANA          PIC  9(005) COMP-3  VALUE ZEROS.
05         03  WRK-DT-AAMMDD           PIC  9(007) COMP-3  VALUE ZEROS.
05         03  WRK-DT-AAAAMMDD         PIC  9(009) COMP-3  VALUE ZEROS.
05         03  WRK-TI-HHMMSS           PIC  9(007) COMP-3  VALUE ZEROS.
05         03  WRK-TI-HHMMSSMMMMMM     PIC  9(013) COMP-3  VALUE ZEROS.
05         03  WRK-TIMESTAMP           PIC  X(020)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* COMUNICACAO COM MODULO GFCT5525 *'.
      *----------------------------------------------------------------*

       01  WRK-5525-AREA-ENTRADA.
           05  WRK-5525-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5525-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5525-DENVIO-MOVTO   PIC  X(010)         VALUE SPACES.
           05  WRK-5525-CROTNA-ORIGE   PIC  X(004)         VALUE SPACES.
           05  WRK-5525-CNRO-ARQ       PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-CSEQ-MOVTO     PIC  9(011)         VALUE ZEROS.
           05  WRK-5525-CSIT-EVNTO     PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-CORRENTISTA    PIC  X(001)         VALUE SPACES.

       01  WRK-5525-AREA-SAIDA.
           05  WRK-5525-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5525-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5525-DESC-MSG       PIC  X(070)         VALUE SPACES.

       COPY 'I#GFCT0M'.

       01  WRK-IO-PCB.
           05  WRK-IO-TERM                 PIC  X(008)     VALUE SPACES.
           05  FILLER                      PIC  X(002)     VALUE SPACES.
           05  WRK-IO-STA                  PIC  X(002)     VALUE SPACES.
           05  FILLER                      PIC  X(012)     VALUE SPACES.
           05  WRK-IO-MODNAME              PIC  X(008)     VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-PCBNAME             PIC  X(008)     VALUE SPACES.
           05  FILLER                      PIC  X(002)     VALUE SPACES.
           05  WRK-ALT-STATUS              PIC  X(002)     VALUE SPACES.
           05  FILLER                      PIC  X(012)     VALUE SPACES.
           05  WRK-ALT-MODNAME             PIC  X(008)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DA POOL7100 *'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB092
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-CHAVE-DETUNIFI  EQUAL HIGH-VALUES
06                                     AND
06                 WRK-CHAVE-ATLZUNIF  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  ATLZUNIF
                       DETUNIFI
                       CTRLDATA
                OUTPUT SAIUNIFI
                       FITCONTB
071126                 ARQCOBR2
                       OCOR1180
05                     ATLZB0H4
070626                 ARQDEBIT
071127                 ARQATU92.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.
05         PERFORM 1200-FORMATAR-TIMESTAMP-H4.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-ATLZUNIF.

           PERFORM 1120-TESTAR-FS-DETUNIFI.

           PERFORM 1130-TESTAR-FS-CTRLDATA.

           PERFORM 1140-TESTAR-FS-SAIUNIFI.

           PERFORM 1150-TESTAR-FS-FITCONTB.

           PERFORM 1160-TESTAR-FS-OCOR1180.

05         PERFORM 1170-TESTAR-FS-ATLZB0H4.

070626     PERFORM 1180-TESTAR-FS-ARQDEBIT.

071126     PERFORM 1190-TESTAR-FS-ARQCOBR2.

071127     PERFORM 1191-TESTAR-FS-ARQATU92.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-ATLZUNIF         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATLZUNIF         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ATLZUNIF    TO WRK-FILE-STATUS
               MOVE 'ATLZUNIF '        TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-DETUNIFI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-DETUNIFI         NOT EQUAL '00'
               MOVE WRK-FS-DETUNIFI    TO WRK-FILE-STATUS
               MOVE 'DETUNIFI'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-CTRLDATA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLDATA         NOT EQUAL '00'
               MOVE WRK-FS-CTRLDATA    TO WRK-FILE-STATUS
               MOVE 'CTRLDATA'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-SAIUNIFI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAIUNIFI         NOT EQUAL '00'
               MOVE WRK-FS-SAIUNIFI    TO WRK-FILE-STATUS
               MOVE 'SAIUNIFI'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-FITCONTB         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-FITCONTB         NOT EQUAL '00'
               MOVE WRK-FS-FITCONTB    TO WRK-FILE-STATUS
               MOVE 'FITCONTB'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1160-TESTAR-FS-OCOR1180         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-OCOR1180         NOT EQUAL '00'
               MOVE WRK-FS-OCOR1180    TO WRK-FILE-STATUS
               MOVE 'OCOR1180'         TO WRK-ARQUIVO
               MOVE WRK-MSG-FS         TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

05    *----------------------------------------------------------------*
05     1170-TESTAR-FS-ATLZB0H4         SECTION.
05    *----------------------------------------------------------------*
05
05         IF  WRK-FS-ATLZB0H4         NOT EQUAL '00'
05             MOVE WRK-FS-ATLZB0H4    TO WRK-FILE-STATUS
05             MOVE 'OCOR1180'         TO WRK-ARQUIVO
05             MOVE WRK-MSG-FS         TO ERR-TEXTO
05             MOVE 'APL'              TO ERR-TIPO-ACESSO
05             PERFORM 9999-PROCESSAR-ROTINA-ERRO
05         END-IF.
05
05    *----------------------------------------------------------------*
05     1170-99-FIM.                    EXIT.
05    *----------------------------------------------------------------*

070626******************************************************************
070626* TESTAR FILE STATUS DO ARQUIVO ARQDEBIT                         *
070626******************************************************************
070626*----------------------------------------------------------------*
070626 1180-TESTAR-FS-ARQDEBIT         SECTION.
070626*----------------------------------------------------------------*
070626
070626     IF  WRK-FS-ARQDEBIT         NOT EQUAL '00'
070626         MOVE WRK-FS-ARQDEBIT    TO WRK-FILE-STATUS
070626         MOVE 'ARQDEBIT'         TO WRK-ARQUIVO
070626         MOVE WRK-MSG-FS         TO ERR-TEXTO
070626         MOVE 'APL'              TO ERR-TIPO-ACESSO
070626         PERFORM 9999-PROCESSAR-ROTINA-ERRO
070626     END-IF.
070626
070626*----------------------------------------------------------------*
070626 1180-99-FIM.                    EXIT.
070626*----------------------------------------------------------------*

071126******************************************************************
071126* TESTAR FILE STATUS DO ARQUIVO ARQCOBR2                         *
071126******************************************************************
071126*----------------------------------------------------------------*
071126 1190-TESTAR-FS-ARQCOBR2         SECTION.
071126*----------------------------------------------------------------*
071126
071126     IF  WRK-FS-ARQCOBR2         NOT EQUAL '00'
071126         MOVE WRK-FS-ARQCOBR2    TO WRK-FILE-STATUS
071126         MOVE 'ARQCOBR2'         TO WRK-ARQUIVO
071126         MOVE WRK-MSG-FS         TO ERR-TEXTO
071126         MOVE 'APL'              TO ERR-TIPO-ACESSO
071126         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071126     END-IF.
071126
071126*----------------------------------------------------------------*
071126 1190-99-FIM.                    EXIT.
071126*----------------------------------------------------------------*

071127******************************************************************
071127* TESTAR FILE STATUS DO ARQUIVO ARQATU92                         *
071127******************************************************************
071127*----------------------------------------------------------------*
071127 1191-TESTAR-FS-ARQATU92         SECTION.
071127*----------------------------------------------------------------*
071127
071127     IF  WRK-FS-ARQATU92         NOT EQUAL '00'
071127         MOVE WRK-FS-ARQATU92    TO WRK-FILE-STATUS
071127         MOVE 'ARQATU92'         TO WRK-ARQUIVO
071127         MOVE WRK-MSG-FS         TO ERR-TEXTO
071127         MOVE 'APL'              TO ERR-TIPO-ACESSO
071127         PERFORM 9999-PROCESSAR-ROTINA-ERRO
071127     END-IF.
071127
071127*----------------------------------------------------------------*
071127 1191-99-FIM.                    EXIT.
071127*----------------------------------------------------------------*

05    *----------------------------------------------------------------*
05     1200-FORMATAR-TIMESTAMP-H4     SECTION.
05    *----------------------------------------------------------------*
05
05         CALL 'POOL7600'             USING WRK-DATA-HORA.
05
05         MOVE WRK-DT-AAAAMMDD        TO WRK-DT-R.
05         MOVE WRK-DT-R               TO WRK-DATA-AAAAMMDD.
05         MOVE WRK-DATA-DD            TO WRK-DD.
05         MOVE WRK-DATA-MM            TO WRK-MM.
05         MOVE WRK-DATA-AAAA          TO WRK-AAAA.
05         MOVE '-'                    TO WRK-TRACO1 WRK-TRACO2.
05
05         MOVE  WRK-TIMESTAMP         TO WRK-TIMESTAMP-SIST.
05         MOVE  WRK-HR-S              TO WRK-HR.
05         MOVE  WRK-MI-S              TO WRK-MI.
05         MOVE  WRK-SG-S              TO WRK-SG.
05         MOVE  WRK-DS-S              TO WRK-DS.
05         MOVE  '-'                   TO WRK-TRACO3.
05         MOVE  '.'                   TO WRK-PONTO1
05                                        WRK-PONTO2
05                                        WRK-PONTO3.
05
05    *----------------------------------------------------------------*
05     1200-99-FIM.                    EXIT.
05    *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ATLZUNIF.

           IF  WRK-CHAVE-ATLZUNIF      EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1180 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO ATLZUNIF VAZIO          *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1180 *****************'
           END-IF.

           PERFORM 2200-LER-DETUNIFI.

           IF  WRK-CHAVE-DETUNIFI       EQUAL HIGH-VALUES
               DISPLAY '***************** GFCT1180 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO DETUNIFI VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1180 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

           PERFORM 2300-LER-CTRLDATA.

           IF  WRK-FS-CTRLDATA          EQUAL '10'
               DISPLAY '***************** GFCT1180 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*          ARQUIVO CTRLDATA VAZIO          *'
               DISPLAY '*         PROCESSAMENTO  ENCERRADO         *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT1180 *****************'
               PERFORM 4000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ATLZUNIF               SECTION.
      *----------------------------------------------------------------*

           READ ATLZUNIF.

           IF  WRK-FS-ATLZUNIF         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-ATLZUNIF
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-ATLZUNIF.

           MOVE COBR-DATA-ENVIO        OF ATLZUNIF
                                       TO WRK-DATA-ENVIO-ATL.
           MOVE COBR-DIG-CONTRATO      OF ATLZUNIF
                                       TO WRK-DIG-CONTRATO-ATL.
           MOVE COBR-NUM-CONTRATO      OF ATLZUNIF
                                       TO WRK-NUM-CONTRATO-ATL.

06         MOVE 0                      TO ACU-ACHOU-DETALHES
           ADD  1                      TO ACU-LIDOS-ATLZUNIF.

           MOVE COBR-VALOR-DEBITADO  OF ATLZUNIF
                TO WRK-VR-SALDO.


      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-DETUNIFI               SECTION.
      *----------------------------------------------------------------*

           READ DETUNIFI.

           IF  WRK-FS-DETUNIFI         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-DETUNIFI
               GO TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-DETUNIFI.

           MOVE COBR-DATA-ENVIO        OF DETUNIFI
                                       TO WRK-DATA-ENVIO-DET.
           MOVE COBR-DIG-CONTRATO      OF DETUNIFI
                                       TO WRK-DIG-CONTRATO-DET.
           MOVE COBR-NUM-CONTRATO      OF DETUNIFI
                                       TO WRK-NUM-CONTRATO-DET.

           ADD 1                       TO ACU-LIDOS-DETUNIFI.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-LER-CTRLDATA               SECTION.
      *----------------------------------------------------------------*

           READ CTRLDATA.

           IF  WRK-FS-CTRLDATA         EQUAL '10'
               GO TO 2300-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-CTRLDATA.

           ADD 1                       TO ACU-LIDOS-CTRLDATA.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO                 TO WRK-OPERACAO.

           IF  WRK-CHAVE-DETUNIFI            LESS WRK-CHAVE-ATLZUNIF
               MOVE REG-COBRANCA             OF DETUNIFI
                                             TO REG-COBRANCA OF SAIUNIFI
               WRITE REG-COBRANCA            OF SAIUNIFI
               PERFORM 1140-TESTAR-FS-SAIUNIFI
               ADD 1                         TO ACU-GRAV-SAIUNIFI
               PERFORM 2200-LER-DETUNIFI
           ELSE
               IF  WRK-CHAVE-DETUNIFI        GREATER WRK-CHAVE-ATLZUNIF
06                 IF  ACU-ACHOU-DETALHES    =       ZEROS
06                     ADD 1                TO       ACU-NF-DETALHES
06                     IF  ACU-NF-DETALHES   <       1001
06                         DISPLAY 'REGISTRO UNIFICADO SEM DETALHES ='
06                         COBR-CHAVE-COBRANCA   OF ATLZUNIF
06                     END-IF
06                 END-IF
                   PERFORM 2100-LER-ATLZUNIF
               ELSE
06                 ADD 1                     TO ACU-ACHOU-DETALHES
                   PERFORM 3100-ACESSAR-GFCTB092
                   IF  SQLCODE               NOT EQUAL ZEROS OR
                       CSIT-EVNTO-RECBD      OF GFCTB092
                                             NOT EQUAL 13
                       ADD 1                 TO ACU-DESPREZADOS
                       PERFORM 2200-LER-DETUNIFI
                       GO TO 3000-99-FIM
                   END-IF
                   EVALUATE TRUE
                       WHEN COBR-COD-RETORNO OF ATLZUNIF
WS                                           EQUAL '000' OR '010'
WS                                              OR 'I'   OR 'P'
______*____________________MOVE_10_TO_COBR-VALOR-DEBITADO_OF_ATLZUNIF
______*____________________MOVE_5_TO_COBR-VALOR-DEBITAR_OF_DETUNIFI
                           IF COBR-VALOR-DEBITADO OF ATLZUNIF
                              GREATER ZEROS
                              IF COBR-VALOR-DEBITADO
                                 OF ATLZUNIF
                                 NOT LESS COBR-VALOR-DEBITAR
                                          OF DETUNIFI
                                 MOVE COBR-VALOR-DEBITADO OF ATLZUNIF
                                      TO WRK-VALOR-DEBITO
                                 MOVE COBR-VALOR-DEBITAR OF DETUNIFI
                                      TO WRK-VALOR-DEBITAR
                                 SUBTRACT WRK-VALOR-DEBITAR FROM
                                          WRK-VALOR-DEBITO
                                          GIVING WRK-VALOR-DEBITO
                                 MOVE WRK-VALOR-DEBITO TO
                                      COBR-VALOR-DEBITADO OF ATLZUNIF
                                 MOVE WRK-VALOR-DEBITAR   TO
                                      WRK-VALOR-RED-11-2
                                 MOVE WRK-VALOR-RED-9-2   TO
                                      WRK-VALOR-S-R
                                 MOVE '+' TO WRK-VALOR-S-S
                                 MOVE WRK-VALOR-S TO
                                      VTARIF-DEB-MOVTO OF GFCTB092
                              ELSE
                                 MOVE COBR-VALOR-DEBITADO OF ATLZUNIF
                                      TO WRK-VALOR-DEBITO
                                 MOVE WRK-VALOR-DEBITO    TO
                                      WRK-VALOR-RED-11-2
                                 MOVE WRK-VALOR-RED-9-2   TO
                                      WRK-VALOR-S-R
                                 MOVE '+' TO WRK-VALOR-S-S
                                 MOVE WRK-VALOR-S TO
                                      VTARIF-DEB-MOVTO OF GFCTB092
                                 MOVE ZEROS TO COBR-VALOR-DEBITADO
                                               OF ATLZUNIF
                              END-IF
                           ELSE
                              MOVE ZEROS
                                   TO VTARIF-DEB-MOVTO OF GFCTB092
                           END-IF
                           MOVE GFCT06-DPROCM-ANTER
WS                                           TO DEFETV-DEB-MOVTO
                                                           OF GFCTB092
                           PERFORM 3200-ATUALIZAR-GFCTB092
______*____________________IF__SQLCODE_______EQUAL_ZEROS
                           PERFORM 3300-ATUALIZAR-SIT-EVENTO
                           PERFORM 3400-GRAVAR-FITA4
______*____________________END-IF

______*________________WHEN_COBR-COD-RETORNO_OF_ATLZUNIF
______*______________________________________EQUAL_'009'
______*____________________ADD_1_____________TO_ACU-DESPREZADOS

11/06_*________________WHEN_COBR-COD-RETORNO_OF_ATLZUNIF
______*______________________________________EQUAL_'F'
______*____________________MOVE_GFCT06-DPROCM-PROX
WS____*______________________________________TO_DAGNDA-DEB-MOVTO
______*____________________________________________________OF_GFCTB092
______*____________________PERFORM_3200-ATUALIZAR-GFCTB092

070625                 WHEN COBR-COD-RETORNO OF ATLZUNIF
070625                                       EQUAL 'F' OR '009'
070625                     PERFORM 3200-ATUALIZAR-GFCTB092

07                     WHEN OTHER
WS                         MOVE COBR-RETORNO OF ATLZUNIF
WS                           TO COBR-RETORNO OF DETUNIFI
                           MOVE REG-COBRANCA OF DETUNIFI
                                             TO REG-COBRANCA OF OCOR1180
                           MOVE 'GFCT0518'   TO COBR-JOB     OF OCOR1180
                           MOVE 'GFCT1180'   TO COBR-PROGRAMA
                                                             OF OCOR1180
04                         MOVE 14           TO COBR-SITUACAO-EVENTO
04                                                           OF OCOR1180
                           WRITE REG-COBRANCA OF OCOR1180
                           PERFORM 1160-TESTAR-FS-OCOR1180
                           ADD 1             TO ACU-GRAV-OCOR1180
                   END-EVALUATE
                   PERFORM 2200-LER-DETUNIFI
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-ACESSAR-GFCTB092           SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF DETUNIFI
                                       TO DENVIO-MOVTO-TARIF
                                                           OF GFCTB092.
           MOVE COBR-ROTINA-ORIG       OF DETUNIFI
                                       TO CROTNA-ORIGE-MOVTO
                                                           OF GFCTB092.
           MOVE COBR-NUM-ARQ-ORIG      OF DETUNIFI
                                       TO CNRO-ARQ-MOVTO   OF GFCTB092.
           MOVE COBR-SEQ-ARQ-ORIG      OF DETUNIFI
                                       TO CSEQ-MOVTO       OF GFCTB092.

           EXEC SQL
             SELECT
                   DENVIO_MOVTO_TARIF,
                   CROTNA_ORIGE_MOVTO,
                   CNRO_ARQ_MOVTO,
                   CSEQ_MOVTO,
                   CSERVC_TARIF,
                   CMIDIA_SERVC_MOVTO,
                   CPERIF_SERVC_MOVTO,
                   QEVNTO_AGRUP,
                   HPREST_SERVC_MOVTO,
                   CTPO_CTA_MOVTO,
                   CBCO_DSTNO_MOVTO,
                   CAG_DSTNO_MOVTO,
                   CCTA_DSTNO_MOVTO,
                   CPAB_DSTNO_MOVTO,
                   CAG_CTLZA_DEB,
                   CCTA_CTLZA_DEB,
                   VTARIF_BRUTO_MOVTO,
                   VTARIF_LIQ_MOVTO,
                   VTARIF_DEB_MOVTO,
                   DAGNDA_DEB_MOVTO,
                   DEFETV_DEB_MOVTO,
                   QMAX_TENTV_DEB,
                   QTENTV_DEB_MOVTO,
                   CSIT_EVNTO_RECBD,
                   CIDTFD_PGMC_COBR,
                   DOCOR_EVNTO,
                   VTARIF_CADTR_SIST
             INTO
                   :GFCTB092.DENVIO-MOVTO-TARIF,
                   :GFCTB092.CROTNA-ORIGE-MOVTO,
                   :GFCTB092.CNRO-ARQ-MOVTO,
                   :GFCTB092.CSEQ-MOVTO,
                   :GFCTB092.CSERVC-TARIF,
                   :GFCTB092.CMIDIA-SERVC-MOVTO,
                   :GFCTB092.CPERIF-SERVC-MOVTO,
                   :GFCTB092.QEVNTO-AGRUP,
                   :GFCTB092.HPREST-SERVC-MOVTO,
                   :GFCTB092.CTPO-CTA-MOVTO,
                   :GFCTB092.CBCO-DSTNO-MOVTO,
                   :GFCTB092.CAG-DSTNO-MOVTO,
                   :GFCTB092.CCTA-DSTNO-MOVTO,
                   :GFCTB092.CPAB-DSTNO-MOVTO,
                   :GFCTB092.CAG-CTLZA-DEB,
                   :GFCTB092.CCTA-CTLZA-DEB,
                   :GFCTB092.VTARIF-BRUTO-MOVTO,
                   :GFCTB092.VTARIF-LIQ-MOVTO,
                   :GFCTB092.VTARIF-DEB-MOVTO,
                   :GFCTB092.DAGNDA-DEB-MOVTO,
                   :GFCTB092.DEFETV-DEB-MOVTO,
                   :GFCTB092.QMAX-TENTV-DEB,
                   :GFCTB092.QTENTV-DEB-MOVTO,
                   :GFCTB092.CSIT-EVNTO-RECBD,
                   :GFCTB092.CIDTFD-PGMC-COBR,
                   :GFCTB092.DOCOR-EVNTO,
                   :GFCTB092.VTARIF-CADTR-SIST
             FROM   DB2PRD.MOVTO_EVNTO_CRRTT
             WHERE
                   DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
                   CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
                   CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
                   CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
WS             MOVE COBR-RETORNO OF ATLZUNIF
WS                                     TO COBR-RETORNO OF DETUNIFI
WS             MOVE REG-COBRANCA       OF DETUNIFI
                                       TO REG-COBRANCA     OF OCOR1180
               MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1180
               MOVE 'GFCT1180'         TO COBR-PROGRAMA    OF OCOR1180
04             MOVE 14                 TO COBR-SITUACAO-EVENTO
04                                                         OF OCOR1180
               MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1180
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1180
               MOVE 'ERRO NO SELECT DA TABELA'
                                       TO COBR-DESCRICAO   OF OCOR1180
               WRITE REG-COBRANCA      OF OCOR1180
               PERFORM 1160-TESTAR-FS-OCOR1180
               ADD 1                   TO ACU-GRAV-OCOR1180
           END-IF.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-ATUALIZAR-GFCTB092         SECTION.
      *----------------------------------------------------------------*
05    *---> ATUALIZA CSIT-EVENTO
070625      IF COBR-COD-RETORNO OF ATLZUNIF EQUAL 'F'
070625         MOVE 02 TO CSIT-EVNTO-RECBD   OF GFCTB092
070625         ADD     1         TO  ACU-FERIADO
070625      ELSE
               IF COBR-COD-RETORNO OF ATLZUNIF EQUAL '009'
                  MOVE 03  TO CSIT-EVNTO-RECBD   OF GFCTB092
                  ADD     1         TO  ACU-FERIADO
               ELSE
                  MOVE 05     TO CSIT-EVNTO-RECBD OF GFCTB092
               END-IF
070625      END-IF.

WS    *---> ATUALIZA AGENCIA/CONTA NO ARQUIVO ATLZUNIF
           MOVE COBR-TRAG-EXT       OF ATLZUNIF
                                    TO COBR-TRAG-EXT    OF DETUNIFI.
           MOVE COBR-AGENCIA-RET    OF DETUNIFI
                                    TO COBR-AGENCIA-DEB OF DETUNIFI.
           MOVE COBR-CONTA-RET      OF DETUNIFI
                                    TO COBR-CONTA-DEB   OF DETUNIFI.

WS    *---> ATUALIZA AGENCIA/CONTA NA TABELA GFCTB092
           IF COBR-CENTRALIZ        OF DETUNIFI   = 'S'
              MOVE COBR-AGENCIA-RET OF DETUNIFI  TO  CAG-CTLZA-DEB
              MOVE COBR-CONTA-RET   OF DETUNIFI  TO  CCTA-CTLZA-DEB
           ELSE
              MOVE COBR-AGENCIA-RET OF DETUNIFI  TO  CAG-DSTNO-MOVTO
              MOVE COBR-CONTA-RET   OF DETUNIFI  TO  CCTA-DSTNO-MOVTO
           END-IF.

071127***** INICIO ****** UPDATE SUBSTITUIDO POR ARQUIVO *************
      *    EXEC SQL
      *      UPDATE   DB2PRD.MOVTO_EVNTO_CRRTT
      *        SET
WS    *            CAG_DSTNO_MOVTO    = :GFCTB092.CAG-DSTNO-MOVTO ,
WS    *            CCTA_DSTNO_MOVTO   = :GFCTB092.CCTA-DSTNO-MOVTO,
WS    *            CAG_CTLZA_DEB      = :GFCTB092.CAG-CTLZA-DEB   ,
WS    *            CCTA_CTLZA_DEB     = :GFCTB092.CCTA-CTLZA-DEB  ,
      *            VTARIF_DEB_MOVTO   = :GFCTB092.VTARIF-DEB-MOVTO,
      *            DAGNDA_DEB_MOVTO   = :GFCTB092.DAGNDA-DEB-MOVTO,
      *            DEFETV_DEB_MOVTO   = :GFCTB092.DEFETV-DEB-MOVTO,
05    *            CSIT_EVNTO_RECBD   = :GFCTB092.CSIT-EVNTO-RECBD
      *      WHERE
      *            DENVIO_MOVTO_TARIF = :GFCTB092.DENVIO-MOVTO-TARIF AND
      *            CROTNA_ORIGE_MOVTO = :GFCTB092.CROTNA-ORIGE-MOVTO AND
      *            CNRO_ARQ_MOVTO     = :GFCTB092.CNRO-ARQ-MOVTO     AND
      *            CSEQ_MOVTO         = :GFCTB092.CSEQ-MOVTO
      *    END-EXEC.
      *
      *    IF (SQLCODE                 NOT EQUAL ZEROS) OR
      *       (SQLWARN0                EQUAL 'W'      )
WS    *        MOVE COBR-RETORNO OF ATLZUNIF
WS    *                                TO COBR-RETORNO OF DETUNIFI
WS    *        MOVE REG-COBRANCA       OF DETUNIFI
      *                                TO REG-COBRANCA     OF OCOR1180
      *        MOVE 'GFCT0518'         TO COBR-JOB         OF OCOR1180
      *        MOVE 'GFCT1180'         TO COBR-PROGRAMA    OF OCOR1180
04    *        MOVE 14                 TO COBR-SITUACAO-EVENTO
04    *                                                    OF OCOR1180
      *        MOVE 'GFCTB092'         TO COBR-RECURSO     OF OCOR1180
      *        MOVE SQLCODE            TO WRK-SQLCODE
      *        MOVE WRK-SQLCODE        TO COBR-COD-RETORNO OF OCOR1180
      *        MOVE 'ERRO NO UPDATE DA TABELA'
      *                                TO COBR-DESCRICAO   OF OCOR1180
      *        WRITE REG-COBRANCA      OF OCOR1180
      *        PERFORM 1160-TESTAR-FS-OCOR1180
      *        ADD 1                   TO ACU-GRAV-OCOR1180
      *    END-IF.

071127     MOVE DENVIO-MOVTO-TARIF TO GFCT67-DENVIO-MOVTO-TARIF
071127     MOVE CROTNA-ORIGE-MOVTO TO GFCT67-CROTNA-ORIGE-MOVTO
071127     MOVE CNRO-ARQ-MOVTO     TO GFCT67-CNRO-ARQ-MOVTO
071127     MOVE CSEQ-MOVTO         TO GFCT67-CSEQ-MOVTO
071127     MOVE CAG-DSTNO-MOVTO    TO GFCT67-CAG-DSTNO-MOVTO
071127     MOVE CCTA-DSTNO-MOVTO   TO GFCT67-CCTA-DSTNO-MOVTO
071127     MOVE CAG-CTLZA-DEB      TO GFCT67-CAG-CTLZA-DEB
071127     MOVE CCTA-CTLZA-DEB     TO GFCT67-CCTA-CTLZA-DEB
071127     MOVE QTENTV-DEB-MOVTO   TO GFCT67-QTENTV-DEB-MOVTO
071127     MOVE DAGNDA-DEB-MOVTO   TO GFCT67-DAGNDA-DEB-MOVTO
071127     MOVE VTARIF-DEB-MOVTO   TO GFCT67-VTARIF-DEB-MOVTO
071127     MOVE DEFETV-DEB-MOVTO   TO GFCT67-DEFETV-DEB-MOVTO
071127     MOVE CSIT-EVNTO-RECBD   TO GFCT67-CSIT-EVNTO-RECBD.

BI1210     MOVE COBR-NUM-PARC      OF DETUNIFI
BI1210                             TO GFCT67-NUM-PARC.

071127     WRITE GFCT67-GFCTB092.
071127
071127     PERFORM 1191-TESTAR-FS-ARQATU92.
071127
071127     ADD 1                       TO ACU-GRAV-ARQATU92.

071127**** FIM ********** UPDATE SUBSTITUIDO POR ARQUIVO *************

071126     PERFORM 3212-GRAVAR-ARQCOBR2.

071127*    ADD 1                       TO ACU-COMMIT.
071127*
071127*    IF  ACU-COMMIT              EQUAL 500
071127*        PERFORM 3210-EFETUAR-COMMIT
071127*        MOVE ZEROS              TO ACU-COMMIT
071127*    END-IF.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

071127*----------------------------------------------------------------*
071127*3210-EFETUAR-COMMIT             SECTION.
071127*----------------------------------------------------------------*
071127*
071127*    EXEC SQL
071127*        COMMIT
071127*    END-EXEC.
071127*
071127*    IF  SQLCODE                 NOT EQUAL ZEROS
071127*        MOVE 'DB2'              TO ERR-TIPO-ACESSO
071127*        MOVE 'COMMIT'           TO ERR-DBD-TAB
071127*                                   ERR-FUN-COMANDO
071127*        MOVE  SQLCODE           TO ERR-SQL-CODE
071127*        MOVE '0010'             TO ERR-LOCAL
071127*        MOVE  SPACES            TO ERR-SEGM
071127*        PERFORM 9999-PROCESSAR-ROTINA-ERRO
071127*    END-IF.
071127*
071127*----------------------------------------------------------------*
071127*3210-99-FIM.                    EXIT.
071127*----------------------------------------------------------------*

071126******************************************************************
071126* ROTINA PARA GRAVAR ARQCOBR2                                    *
071126******************************************************************
071126*----------------------------------------------------------------*
071126 3212-GRAVAR-ARQCOBR2            SECTION.
071126*----------------------------------------------------------------*
071126
030408*    MOVE REG-COBRANCA     OF ATLZUNIF TO REG-GFCTMW.
030408     MOVE REG-COBRANCA     OF DETUNIFI TO REG-GFCTMW.
071126     MOVE QMAX-TENTV-DEB   OF GFCTB092 TO GFCTMW-TENTV-DEB-MAX.
071126     MOVE VTARIF-LIQ-MOVTO OF GFCTB092 TO GFCTMW-VALOR-LIQ92.
071126     MOVE QTENTV-DEB-MOVTO OF GFCTB092 TO GFCTMW-TENTV-DEB-ATU.
071126     MOVE VTARIF-DEB-MOVTO OF GFCTB092 TO GFCTMW-VALOR-COBR92.
071126     MOVE CSIT-EVNTO-RECBD OF GFCTB092 TO GFCTMW-SITUACAO-EVENTO.


           IF WRK-VR-SALDO                   NOT LESS
              COBR-VALOR-DEBITAR OF DETUNIFI
              MOVE COBR-VALOR-DEBITAR OF DETUNIFI TO
                   GFCTMW-VALOR-DEBITADO
              MOVE WRK-VR-SALDO TO WRK-VR-SALDO-PK
              MOVE COBR-VALOR-DEBITAR OF DETUNIFI TO
              WRK-COBR-VALOR-DEBITAR-PK
              SUBTRACT WRK-COBR-VALOR-DEBITAR-PK FROM
              WRK-VR-SALDO-PK
              MOVE WRK-VR-SALDO-PK TO WRK-VR-SALDO
           ELSE
              MOVE WRK-VR-SALDO TO
                   GFCTMW-VALOR-DEBITADO
              MOVE ZEROS TO WRK-VR-SALDO
           END-IF.



071126
071126     WRITE REG-GFCTMW.
071126
071126     PERFORM 1190-TESTAR-FS-ARQCOBR2.
071126
071126     ADD 1                       TO ACU-GRAV-ARQCOBR2.
071126
071126*----------------------------------------------------------------*
071126 3212-99-FIM.                    EXIT.
071126*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-ATUALIZAR-SIT-EVENTO       SECTION.
      *----------------------------------------------------------------*
05         MOVE DENVIO-MOVTO-TARIF     OF GFCTB092
05                                     TO ZD-DENVIO-MOVTO-TARIF.
05         MOVE CROTNA-ORIGE-MOVTO     OF GFCTB092
05                                     TO ZD-CROTNA-ORIGE-MOVTO.
05         MOVE CNRO-ARQ-MOVTO         OF GFCTB092
05                                     TO ZD-CNRO-ARQ-MOVTO.
05         MOVE CSEQ-MOVTO             OF GFCTB092
05                                     TO ZD-CSEQ-MOVTO.
05         MOVE WRK-TIMESTAMP-H4       TO ZD-HTROCA-SIT-EVNTO.
05
           MOVE WRK-TIMESTAMP-H4       TO WRK-CPTCAO-GFCTB0H4
           MOVE WRK-CPTCAO-B0H4        TO ZD-CPTCAO-TBELA

           MOVE 05                     TO ZD-CSIT-EVNTO-RECBD.
05
05         WRITE ZD-REGISTRO.
05
05         PERFORM 1170-TESTAR-FS-ATLZB0H4.
05
05         ADD 1                       TO ACU-GRAV-ATLZB0H4.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3400-GRAVAR-FITA4               SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-DATA-ORIG         OF DETUNIFI
                                       TO GFCTJH-DT-ENVIO-MOVTO.
           MOVE COBR-ROTINA-ORIG       OF DETUNIFI
                                       TO GFCTJH-CD-ROTINA-ORIGEM
           MOVE COBR-NUM-ARQ-ORIG      OF DETUNIFI
                                       TO GFCTJH-CD-NUMERO-ARQUIVO.
           MOVE COBR-SEQ-ARQ-ORIG      OF DETUNIFI
                                       TO GFCTJH-CD-SEQ-MOVTO.
           MOVE 4                      TO GFCTJH-CD-ORIGEM
                                          GFCTJH-CD-TIPO-SOLTC.
           MOVE COBR-TARIFA-ORIG       OF DETUNIFI
                                       TO GFCTJH-TARIFA.
           MOVE COBR-VALOR-DEBITAR     OF DETUNIFI
                                       TO GFCTJH-VALOR.
           MOVE COBR-BANCO-DEB         OF DETUNIFI
                                       TO GFCTJH-BANCO.
           MOVE COBR-AGENCIA-DEB       OF DETUNIFI
                                       TO GFCTJH-AGENCIA.
           MOVE COBR-CONTA-DEB         OF DETUNIFI
                                       TO GFCTJH-CONTA.
           MOVE COBR-BANCO-ORIG        OF DETUNIFI
                                       TO GFCTJH-BANCO-ORIGEM.
           MOVE COBR-AGENCIA-ORIG      OF DETUNIFI
                                       TO GFCTJH-AGENCIA-ORIGEM.
           MOVE COBR-DATA-EVNTO-ORIG   OF DETUNIFI
                                       TO GFCTJH-DATA-EVENTO.

           WRITE GFCTJH-ENTRADA.

           PERFORM 1150-TESTAR-FS-FITCONTB.

           ADD 1                       TO ACU-GRAV-FITCONTB.

070626     PERFORM 3410-GRAVAR-ARQDEBIT.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

070626******************************************************************
070626* ROTINA PARA GRAVAR ARQUIVO DE EVENTOS EFETIVAMENTE COBRADOS    *
070626******************************************************************
070626*----------------------------------------------------------------*
070626 3410-GRAVAR-ARQDEBIT            SECTION.
070626*----------------------------------------------------------------*
070626
070626     MOVE COBR-AGENCIA-DEB       OF DETUNIFI
070626                                 TO GFCTZJ-COD-AGENCIA.
070626     MOVE COBR-CONTA-DEB         OF DETUNIFI
070626                                 TO GFCTZJ-NUM-CONTA.
070626     MOVE COBR-GRUPO-DEB         OF DETUNIFI
070626                                 TO GFCTZJ-COD-GRUPO.
070626     MOVE COBR-SUBGRUPO-DEB      OF DETUNIFI
070626                                 TO GFCTZJ-COD-SUB-GRUPO.
070626     MOVE COBR-CGC-CPF-MUNERO    OF DETUNIFI
070626                                 TO GFCTZJ-CNPJ-CPF-PRINCIPAL.
070626     MOVE COBR-CGC-CPF-FILIAL    OF DETUNIFI
070626                                 TO GFCTZJ-CNPJ-CPF-FILIAL.
070626     MOVE COBR-CGC-CPF-CONTROLE  OF DETUNIFI
070626                                 TO GFCTZJ-CNPJ-CPF-CONTROLE.
070626     MOVE COBR-TARIFA-ORIG       OF DETUNIFI
070626                                 TO GFCTZJ-COD-EVENTO-TARIFA.
070626     MOVE DEFETV-DEB-MOVTO       OF GFCTB092
070626                                 TO WRK-DATA-DEB.
070626     MOVE WRK-DIA-DEB            TO GFCTZJ-DIA-DEBITO.
070626     MOVE WRK-MES-DEB            TO GFCTZJ-MES-DEBITO.
070626     MOVE WRK-ANO-DEB            TO GFCTZJ-ANO-DEBITO.
070626     MOVE ZEROS                  TO GFCTZJ-COD-IDENT-COBRANCA.
070626     MOVE COBR-VALOR-DEBITAR     OF DETUNIFI
070626                                 TO GFCTZJ-VLR-COBRADO.
070626
070626     MOVE DOCOR-EVNTO            OF GFCTB092
070626                                 TO WRK-DATA-EVT.
070626     MOVE WRK-DIA-EVT            TO GFCTZJ-DIA-EVENTO.
070626     MOVE WRK-MES-EVT            TO GFCTZJ-MES-EVENTO.
070626     MOVE WRK-ANO-EVT            TO GFCTZJ-ANO-EVENTO.
070626
070626     IF   COBR-COD-RETORNO       OF ATLZUNIF
070626                                 IS NUMERIC
070626          MOVE 1                 TO GFCTZJ-COD-IDENT-ORIG-DEB
070626     ELSE
070626          MOVE 2                 TO GFCTZJ-COD-IDENT-ORIG-DEB.
070626
070626     WRITE GFCTZJ-REGISTRO.
070626
070626     PERFORM 1180-TESTAR-FS-ARQDEBIT.
070626
070626     ADD 1                       TO ACU-GRAV-ARQDEBIT.
070626
070626*----------------------------------------------------------------*
070626 3410-99-FIM.                    EXIT.
070626*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

071127*    IF  ACU-COMMIT              GREATER ZEROS
071127*        PERFORM 3210-EFETUAR-COMMIT
071127*    END-IF.

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ATLZUNIF
                 DETUNIFI
                 CTRLDATA
                 SAIUNIFI
                 FITCONTB
                 OCOR1180
                 ATLZB0H4
070626           ARQDEBIT
071127           ARQATU92.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ATLZUNIF     TO WRK-MASCARA.

           DISPLAY '******************** GFCT1180 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGISTROS LIDOS EM ATLZUNIF...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-LIDOS-DETUNIFI     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM DETUNIFI...: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-LIDOS-CTRLDATA     TO WRK-MASCARA.

           DISPLAY '* REGISTROS LIDOS EM CTRLDATA...: ' WRK-MASCARA
                                                                   ' *'.
           MOVE ACU-GRAV-SAIUNIFI      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM SAIUNIFI: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAV-FITCONTB      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM FITCONTB: ' WRK-MASCARA
                                                                   ' *'.

           MOVE ACU-GRAV-OCOR1180      TO WRK-MASCARA.

           DISPLAY '* REGISTROS GRAVADOS EM OCOR1180: ' WRK-MASCARA
                                                                   ' *'.

071101*    MOVE ACU-ATLZ-GFCTB092      TO WRK-MASCARA.
071101     MOVE ACU-GRAV-ARQATU92      TO WRK-MASCARA.

           DISPLAY '* REGISTROS A ATUALIZAR GFCTB092: ' WRK-MASCARA
                                                                  ' *'.
070625     MOVE ACU-FERIADO            TO WRK-MASCARA.
070625     DISPLAY '* FERIADO NA AGENCIA POUP.......: ' WRK-MASCARA
070625                                                             ' *'.

070626     MOVE ACU-GRAV-ARQDEBIT      TO WRK-MASCARA.
070626
070626     DISPLAY '* REGISTROS GRAVADOS EM ARQDEBIT: ' WRK-MASCARA
070626                                                             ' *'.

071126     MOVE ACU-GRAV-ARQCOBR2      TO WRK-MASCARA.
071126
071126     DISPLAY '* REGISTROS GRAVADOS EM ARQCOBR2: ' WRK-MASCARA
                                                                   ' *'.

05         MOVE ACU-GRAV-ATLZB0H4      TO WRK-MASCARA.
05         DISPLAY '* REGISTROS GRAVADOS EM GFCTB0H4: ' WRK-MASCARA
05                                                                 ' *'.
           MOVE ACU-DESPREZADOS        TO WRK-MASCARA.

           DISPLAY '* REGISTROS DESPREZADOS.........: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT1180 ********************'.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 4                      TO RETURN-CODE.
           MOVE 'GFCT1180'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
