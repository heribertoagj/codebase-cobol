      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT4625.
       AUTHOR.     DIEGO DE SOUZA.
      *================================================================*
      *                    C P M  -  S I S T E M A S.                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT4625                                    *
      *    PROGRAMADOR.:   DIEGO DE SOUZA          - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ADRIANO LOPES SANT'ANNA - CPM PATO BRANCO   *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK / GP.50  *
      *    DATA........:   06/09/2006                                  *
      *----------------------------------------------------------------*
080929*    OBJETIVO....:   CALCULO DA DATA DE PROCESSAMENTO.           *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                          INCLUDE/BOOK    *
      *                    TRABALHO                      I#GFCTV0      *
      *                    ATUTAB92                      I#GFCTA3      *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.PARM_SERVC_TARIF      GFCTB0A2       *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERROS PELA POOL7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL1050 - CONEXAO COM DB2                                  *
      *    POOL7100 - TRATAMENTO DE ERROS PADRAO BRADESCO              *
      *    POOL1205 - IDENTIFICAR DIAS UTEIS E INFORMACOES ADICIONAIS  *
      *    POOL1640 - SOMAR DIAS, MESES E ANOS EM UMA DATA             *
      *================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK / GP.50  *
      *    DATA........:   13/03/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   MUDAR DATA DE PROCESSAMENTO DE PROX PARA    *
      *                    ATUAL                                       *
021007*================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK / GP.50  *
      *    DATA........:   02/10/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   DEIXAR DE ISENTAR OU PRE-AGENDAR EVENTOS    *
      *                    NESSE MOMENTO                               *
021007*----------------------------------------------------------------*
301007*================================================================*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK / GP.50  *
      *    DATA........:   30/10/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   SE PERIODO DE CALCULO DIFERENTE DE 1,2,3,4  *
      *                    MOVER DATA DO SISTEMA PARA DATA DE DEBITO.  *
301007*----------------------------------------------------------------*
      *
0208  *================================================================*
0208  *                     A L T E R A C A O                          *
0208  *----------------------------------------------------------------*
0208  *    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
0208  *    DATA........:   12/02/2008                                  *
0208  *    OBJETIVO....:   INCLUIR TRATAMENTO PARA V0-DAGNDA-DEB-MOVTO *
0208  *                    PREENCHIDA                                  *
0208  *================================================================*
260208*================================================================*
260208*                     A L T E R A C A O                          *
260208*----------------------------------------------------------------*
260208*    ANALISTA....:   WAGNER                - PROCWORK - GRUPO 50 *
260208*    DATA........:   26/02/2008                                  *
260208*    OBJETIVO....:   INCLUIR DIA DE COBRANCA PARA CTRATO-MOVTO = *
260208*                    4(MENSAL)                                   *
260208*================================================================*
080928*================================================================*
080928*       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
080928*----------------------------------------------------------------*
080928*    ANALISTA....:  WAGNER SILVA          - SOND PROCWORK - GP.50*
080928*    PROGRAMADOR.:  VINICIUS C. MADUREIRA - SOND PROCWORK - GP.50*
080928*    DATA........:  29/09/2008                                   *
080928*    OBJETIVO....:  DIVERSAS ALTERACOES:                         *
080928*                                                                *
080928*    INCLUSAO DE NOVOS CONCEITOS:                                *
080928*    - DIA DA COBRANCA EH O DIA UTIL DO MES EM QUE A COBRANCA    *
080928*      SERA EFETUADA.                                            *
080928*    - DIAS DE RETENCAO SAO DIAS UTEIS A ACRESCENTAR NA DATA     *
080928*      APURADA.                                                  *
080928*    - SE A DATA DA COBRANCA (COM DIA DA COBRANCA E RETENCAO)    *
080928*      TARIFA FOR INFERIOR A DATA DO PROCESSAMENTO ELA SERA      *
080928*      SUBSTITUIDA PELA DO PROCESSAMENTO.                        *
080928*    - SE A DATA DA COBRANCA (COM DIA DA COBRANCA E RETENCAO)    *
080928*      TARIFA FOR SUPERIOR AO DIA DA COBRANCA A DATA SERA        *
080928*      SUBSTITUIDA PELO PROXIMO DIA DE COBRANCA.                 *
080928*                                                                *
080928*================================================================*
1208  *================================================================*
1208  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
1208  *----------------------------------------------------------------*
1208  *    ANALISTA....:  WAGNER SILVA        - SONDA PROCWORK - GP.50 *
1208  *    DATA........:  22/12/2008                                   *
1208  *    OBJETIVO....:  INCLUIR NOVO CONCEITO:                       *
1208  *                                                                *
1208  *    - DIA DO MES - QDIA-COBR-TARIF - TAB A2 - DIA DA COBRANCA   *
1208  *    - ANTECOPA OU POSTERGA - BCIDTFD-COBR-FERI - TAB. A2        *
1208  *      - SE 'S' ANTECIPA O PAGTO, EM CASO DE DIA NAO UTIL        *
1208  *================================================================*
0109  *================================================================*
0109  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0109  *----------------------------------------------------------------*
0109  *    ANALISTA....:  WAGNER SILVA        - SONDA PROCWORK - GP.50 *
0109  *    DATA........:  06/01/2009                                   *
0109  *    OBJETIVO....:                                               *
0109  *             PASSAR A APLICAR A RETENCAO APOS O DIA DA COBRANCA *
0109  *================================================================*
0109  *================================================================*
0109  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0109  *----------------------------------------------------------------*
0109  *    ANALISTA....:  WAGNER SILVA        - SONDA PROCWORK - GP.50 *
0109  *    DATA........:  07/01/2009                                   *
0109  *    OBJETIVO....:  RETIRAR DO PROGRAMA A ROTINA DE RETENCAO     *
0109  *================================================================*
0309  *================================================================*
0309  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0309  *----------------------------------------------------------------*
0309  *    ANALISTA....:  WAGNER SILVA        - SONDA PROCWORK - GP.50 *
0309  *    DATA........:  05/03/2009                                   *
0309  *    OBJETIVO....:  ACERTO CAMPO MES PARA HEXAVISION             *
0309  *================================================================*

0709  *================================================================*
0709  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0709  *----------------------------------------------------------------*
0709  *    ANALISTA....:  WAGNER SILVA - PROCWORK - ALOCACAO GP.50     *
0709  *    DATA........:  08/07/2009                                   *
0709  *    OBJETIVO....:  SE CTPO_SERVC_TARIF = 2 ( TARIFA PACOTE)     *
0709  *                   PASSAR A AGENDAR COBRANCA P/ PROXIMO DIA UTIL*
0709  *================================================================*

0709  *================================================================*
0709  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0709  *----------------------------------------------------------------*
0709  *    ANALISTA....:  WAGNER SILVA - PROCWORK - ALOCACAO GP.50     *
0709  *    DATA........:  14/07/2009                                   *
0709  *    OBJETIVOS...:                                               *
0709  *                1- A TARIFA PACOTE PASSA A SER AGENDADA A       *
0709  *                   PARTIR DO PRIMEIRO DIA DO MES  DA DATA       *
0709  *                   DO PROCESSAMENTO.                            *
0709  *                                                                *
0709  *                2- AS TARIFAS TIPO XX XX XX PASSAM A ACEITAR    *
0709  *                   AGENDAMENTO COM DATA RETROATIVA.             *
0709  *================================================================*

0410  *================================================================*
0410  *       S O N D A   P R O C W O R K  -  A L T E R A C A O        *
0410  *----------------------------------------------------------------*
0410  *    ANALISTA....:  FABIO VISCONDE - PROCWORK                    *
0410  *    DATA........:  26/04/2010                                   *
0410  *    OBJETIVO....:  SE V0-CTRATO-MOVTO = 2  ( TARIFA PACOTE)     *
0410  *                      MOVER V0-VTARIF-BRUTO-MOVTO PARA          *
0410  *                      A3-VTARIF-LIQ-MOV                         *
      *================================================================*

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

           SELECT TRABALHO ASSIGN      TO UT-S-TRABALHO
           FILE STATUS                 IS WRK-FS-TRABALHO.

           SELECT ATUTAB92 ASSIGN      TO UT-S-ATUTAB92
           FILE STATUS                 IS WRK-FS-ATUTAB92.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT : DADOS DAS TABELAS GFCTB033 E 92.                    *
      *            ORG. SEQUENCIAL     -    LRECL = 160                *
      *----------------------------------------------------------------*

       FD  TRABALHO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTV0'.

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO PARA ATUALIZACAO DA TABELA GFCTB092         *
      *            ORG. SEQUENCIAL     -    LRECL = 132                *
      *----------------------------------------------------------------*

       FD  ATUTAB92
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY 'I#GFCTA3'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).

       01  WRK-TAB.
           02  WRK-TB                  OCCURS 100.
               05 WRK-DATA-1               PIC X(10) VALUE SPACE.
               05 FILLER                   PIC X(02) VALUE ' -'.
               05 WRK-PER                  PIC 9(01) VALUE ZEROS.
               05 FILLER                   PIC X(02) VALUE '- '.
               05 WRK-DATA-2               PIC X(10) VALUE SPACE.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(033)         VALUE
           'INICIO DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

           COPY 'GFCTWC01'.

______*01__WRK-CINDCD-PER-COBR-IN.
______*____05_WRK-CINDCD-PER-COBR-IN-R____PIC_9(01)_VALUE_0.


        01  WRK-DATA-DMAB.
           05  WRK-DATA-DMAB-DIA       PIC  9(02)          VALUE ZERO.
           05  WRK-DATA-DMAB-PT1       PIC  X(01)          VALUE '.'.
           05  WRK-DATA-DMAB-MES       PIC  9(02)          VALUE ZERO.
           05  WRK-DATA-DMAB-PT2       PIC  X(01)          VALUE '.'.
           05  WRK-DATA-DMAB-ANO       PIC  9(04)          VALUE ZERO.

       01  WRK-DATA-DMA.
           05  WRK-DATA-DMA-DIA        PIC  9(02)          VALUE ZERO.
           05  WRK-DATA-DMA-MES        PIC  9(02)          VALUE ZERO.
           05  WRK-DATA-DMA-ANO        PIC  9(04)          VALUE ZERO.
       01  WRK-DATA-DMA-R   REDEFINES
           WRK-DATA-DMA                PIC  9(08).

       01  WRK-DATA-AMD.
           05  WRK-DATA-AMD-ANO        PIC  9(04)          VALUE ZERO.
           05  WRK-DATA-AMD-MES        PIC  9(02)          VALUE ZERO.
           05  WRK-DATA-AMD-DIA        PIC  9(02)          VALUE ZERO.
       01  WRK-DATA-AMD-R   REDEFINES
           WRK-DATA-AMD                PIC  9(08).


       77  WRK-MODULO                  PIC  X(08)          VALUE SPACE.
       77  WRK-COUNT-ERROS             PIC S9(005)V COMP-3 VALUE ZEROS.
       77  WRK-CINDCD-PER-COBR         PIC  9(001)         VALUE ZEROS.
       77  WRK-COUNT-PER-COBR          PIC S9(009)V COMP-3 VALUE ZEROS.
       77  WRK-CSERVC-TARIF-DISP       PIC  9(004)         VALUE ZEROS.
       77  WRK-CSIT-EVNTO-RECBD        PIC  9(002)         VALUE ZEROS.
       77  WRK-CTM                     PIC S9(009)V  COMP-3  VALUE ZERO.
       77  WRK-ACHOU                   PIC  X(001)           VALUE 'N'.
       77  WRK-CTX                     PIC S9(009)V  COMP-3  VALUE ZERO.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(019)         VALUE
           'AREA DE INDEXADORES'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(020)         VALUE
           'AREA DE ACUMULADORES'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-TRABALHO          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVA-ATUTAB92          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-TARIF-NAO-ENCONT        PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-DT-1                    PIC  9(008)   COMP-3  VALUE ZERO.
       77  WRK-DT-2                    PIC  9(008)   COMP-3  VALUE ZERO.
______*77__WRK-PERIODO_________________PIC__9(001)___COMP-3__VALUE_ZERO.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(029)         VALUE
           'AREA DE TESTES DE FILE STATUS'.
      *----------------------------------------------------------------*

       77  WRK-FS-TRABALHO             PIC  X(002)         VALUE SPACES.
       77  WRK-FS-ATUTAB92             PIC  X(002)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(030)         VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-TARIFA-ENCONTRADA       PIC  X(001)         VALUE SPACES.
       77  WRK-INIC-VIG                PIC  9(008)         VALUE ZEROS.
       77  WRK-FIM-VIG                 PIC  9(008)         VALUE ZEROS.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9    VALUE ZEROS.
       77  WRK-MASC2                   PIC ----.---.--9    VALUE ZEROS.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(029)         VALUE
           'AREA PARA CHAVES DO PROGRAMA'.
      *---------------------------------------------------------------*

       01  WRK-CHAVE-ATUAL.
           05  WRK-CHV-ATU-OCOR        PIC  9(008)         VALUE ZEROS.
           05  WRK-CHV-ATU-TARIFA      PIC  9(005)         VALUE ZEROS.

       01  WRK-CHAVE-ANTERIOR.
           05  WRK-CHV-ANT-OCOR        PIC  9(008)         VALUE ZEROS.
           05  WRK-CHV-ANT-TARIFA      PIC  9(005)         VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(028)         VALUE
           'AREA PARA AUXILIARES DE DATA'.
      *---------------------------------------------------------------*

       01  WRK-DOCOR-EVNTO             PIC  X(010)         VALUE SPACES.

       01  WRK-DT-SISTEMA              PIC  X(010)         VALUE SPACES.
       01  WRK-DT-SISTEMA-R            REDEFINES WRK-DT-SISTEMA.
           05  WRK-DIA-DT-SISTEMA      PIC  9(002).
           05  WRK-PONTO-1             PIC  X(001).
           05  WRK-MES-DT-SISTEMA      PIC  9(002).
           05  WRK-PONTO-2             PIC  X(001).
           05  WRK-ANO-DT-SISTEMA      PIC  9(004).

       01  WRK-DT-SISTEMA-INV          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-SISTEMA-INV-R        REDEFINES WRK-DT-SISTEMA-INV.
           05  WRK-ANO-SISTEMA         PIC  9(004).
           05  WRK-MES-SISTEMA         PIC  9(002).
           05  WRK-DIA-SISTEMA         PIC  9(002).

0208   01  WRK-DT-AGENDTO-INV          PIC  9(008)         VALUE ZEROS.
0208   01  WRK-DT-AGENDTO-INV-R        REDEFINES WRK-DT-AGENDTO-INV.
0208       05  WRK-ANO-AGENDTO         PIC  9(004).
0208       05  WRK-MES-AGENDTO         PIC  9(002).
0208       05  WRK-DIA-AGENDTO         PIC  9(002).

       01  WRK-DT-APURADA              PIC  X(010)         VALUE SPACES.
       01  WRK-DT-APURADA-R            REDEFINES WRK-DT-APURADA.
           05  WRK-DIA-DT-APURADA      PIC  9(002).
           05  WRK-PONTO-3             PIC  X(001).
           05  WRK-MES-DT-APURADA      PIC  9(002).
           05  WRK-PONTO-4             PIC  X(001).
           05  WRK-ANO-DT-APURADA      PIC  9(004).

       01  WRK-AUX-ANO                 PIC  9(004) COMP-3 VALUE ZERO.
       01  WRK-AUX-MES                 PIC  9(002) COMP-3 VALUE ZERO.
       01  WRK-MES-DT-APURADA-C        PIC  9(002) COMP-3 VALUE ZERO.

       01  WRK-DT-DB2                  PIC  X(010)         VALUE SPACES.
       01  WRK-DT-DB2-R                REDEFINES WRK-DT-DB2.
           05  WRK-DIA-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-MES-DB2             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-DB2             PIC  9(004).

       01  WRK-DATA-APU                PIC  X(010)         VALUE SPACES.
       01  WRK-DATA-APU-R            REDEFINES WRK-DATA-APU.
           05  WRK-APU-DIA             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-APU-MES             PIC  9(002).
           05  FILLER                  PIC  X(001).
           05  WRK-APU-ANO             PIC  9(004).

       01  WRK-DATA-1205               PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-1205-R             REDEFINES WRK-DATA-1205.
           05  WRK-ANO-1205            PIC  9(004).
           05  WRK-MES-1205            PIC  9(002).
           05  WRK-DIA-1205            PIC  9(002).

       01  WRK-ANO-1205-COMP           PIC  9(004) COMP-3  VALUE ZEROS.
       01  WRK-MES-1205-COMP           PIC  9(002) COMP-3  VALUE ZEROS.

       01  WRK-DT-APURADA-I            PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-APURADA-I-R          REDEFINES WRK-DT-APURADA-I.
           05  WRK-ANO-APU-I           PIC  9(004).
           05  WRK-MES-APU-I           PIC  9(002).
           05  WRK-DIA-APU-I           PIC  9(002).

       01  WRK-DATA-EDI.
           05  WRK-DIA-EDI             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-EDI             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-EDI             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA                    PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-R                  REDEFINES WRK-DATA.
           05  WRK-DIA                 PIC  9(002).
           05  WRK-MES                 PIC  9(002).
           05  WRK-ANO                 PIC  9(004).

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-INV-R              REDEFINES WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(004).
           05  WRK-MES-INV             PIC  9(002).
           05  WRK-DIA-INV             PIC  9(002).

       01  WRK-DATA-S-INV              PIC  9(008)         VALUE ZEROS.
       01  WRK-DATA-S-INV-R            REDEFINES WRK-DATA-S-INV.
           05  WRK-ANO-S-INV           PIC  9(004).
           05  WRK-MES-S-INV           PIC  9(002).
           05  WRK-DIA-S-INV           PIC  9(002).

       01  WRK-DATA-EVT-AMD            PIC  9(010)         VALUE ZEROS.
       01  WRK-DATA-EVT-AMD-R          REDEFINES WRK-DATA-EVT-AMD.
           05  FILLER                  PIC  9(002).
           05  WRK-ANO-EVT-AMD         PIC  9(004).
           05  WRK-MES-EVT-AMD         PIC  9(002).
           05  WRK-DIA-EVT-AMD         PIC  9(002).

       01  WRK-AUX-1-S                 PIC +9(001)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-1-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-1               PIC  9(001).

       01  WRK-AUX-2-S                 PIC +9(002)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-2-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-2               PIC  9(002).

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(017)         VALUE
           'AREAS DA POOL1205'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-P1205              PIC  X(031)         VALUE
           '** ERRO NO ACESSO A POOL1205 **'.

       01  WRK-MENSAGEM-1205           PIC  X(050)         VALUE SPACES.

       01  WRK-AREA-POOL1205.
           05  WRK-DT-ENVIADA          PIC  9(008)  COMP-3 VALUE ZEROS.
           05  WRK-OPCAO               PIC  X(001)         VALUE SPACES.
           05  WRK-DT-JULIANA          PIC  9(007)  COMP-3 VALUE ZEROS.
           05  WRK-DT-EDITADA          PIC  X(010)         VALUE SPACES.
           05  WRK-DT-GREGORI          PIC  9(008)         VALUE ZEROS.
           05  WRK-DIA-SEMANA          PIC  X(013)         VALUE SPACES.
           05  WRK-MES-EXTENSO         PIC  X(009)         VALUE SPACES.
           05  WRK-DIA-UT-ANT          PIC  9(008)         VALUE ZEROS.
           05  WRK-DIA-UT-POS          PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(018)         VALUE
           'AREA PARA POOL1640'.
      *----------------------------------------------------------------*

       01  WRK-AREA-POOL1640.
           05  WRK-DT-ENTRADA-1640     PIC  X(010)         VALUE SPACES.
           05  WRK-FORMATO-ENT-1640    PIC  9(002)         VALUE ZEROS.
           05  WRK-TP-INCREM-1640      PIC  X(002)         VALUE SPACES.
           05  WRK-INCREMENTO-1640     PIC  9(003)         VALUE ZEROS.
           05  WRK-FORMATO-SAI-1640    PIC  9(002)         VALUE ZEROS.
           05  WRK-DT-SAIDA-1640       PIC  X(010)         VALUE SPACES.
           05  WRK-MENSAGEM-1640       PIC  X(080)         VALUE SPACES.

       01  WRK-INCREMENTO-1640-COMP    PIC  9(003) COMP-3  VALUE ZEROS.

       01  WRK-MENSAGEM-1640-E.
           05  WRK-MENSAGEM-1640-R     PIC  X(040)         VALUE SPACES.
           05  FILLER                  PIC  X(040)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(031)         VALUE
           'MENSAGEM DE ERRO DE FILE-STATUS'.
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
       01  FILLER                      PIC  X(024)         VALUE
           'AREA PARA TABELA INTERNA'.
      *----------------------------------------------------------------*

       01  TAB-TABELA-DIAS.
           05 FILLER                 PIC  X(014) VALUE '1DOMINGO'.
           05 FILLER                 PIC  X(014) VALUE '2SEGUNDA-FEIRA'.
           05 FILLER                 PIC  X(014) VALUE '3TERCA-FEIRA'.
           05 FILLER                 PIC  X(014) VALUE '4QUARTA-FEIRA'.
           05 FILLER                 PIC  X(014) VALUE '5QUINTA-FEIRA'.
           05 FILLER                 PIC  X(014) VALUE '6SEXTA-FEIRA'.
           05 FILLER                 PIC  X(014) VALUE '7SABADO'.
       01  FILLER      REDEFINES     TAB-TABELA-DIAS.
           05 FILLER                 OCCURS 7 TIMES.
              10 TAB-DIA-CINIC       PIC  9(001).
              10 TAB-DIA-SEMANA      PIC  X(013).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(028)         VALUE
           'AREA PARA TRATAMENTO DE ERRO'.
      *----------------------------------------------------------------*

       COPY 'POL7100C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(013)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A2
           END-EXEC.


           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.
            MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C
               WRK-DATA-DMA-R.

      ******************************************************************
      *    FLUXO DE PROCESSAMENTO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FS-TRABALHO     EQUAL '10'.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONEXAO COM DB2, ABERTURA DE ARQUIVOS E OBTENCAO DE DATA    *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           OPEN INPUT  TRABALHO
                OUTPUT ATUTAB92.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           PERFORM 1200-OBTER-DT-SIST.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DOS ARQUIVOS                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-TRABALHO.

           PERFORM 1120-TESTAR-FS-ATUTAB92.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO TRABALHO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-TESTAR-FS-TRABALHO         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-TRABALHO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-TRABALHO    TO WRK-FILE-STATUS
               MOVE 'TRABALHO'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TESTE DE FILE STATUS DO ARQUIVO ATUTAB92                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1120-TESTAR-FS-ATUTAB92         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ATUTAB92         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-FS-ATUTAB92    TO WRK-FILE-STATUS
               MOVE 'ATUTAB92'         TO WRK-NOME-ARQ
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DATA DO SISTEMA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DT-SIST              SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
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
                                       TO WRK-DT-SISTEMA.

           MOVE '.'                    TO WRK-PONTO-1
                                          WRK-PONTO-2.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA SE O ARQUIVO DE ENTRADA ESTA VAZIO.                *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-TRABALHO.

           IF  WRK-FS-TRABALHO         EQUAL '10'
               DISPLAY '***************** GFCT4625 ****************'
               DISPLAY '*                                         *'
               DISPLAY '*        ARQUIVO DE TRABALHO VAZIO        *'
               DISPLAY '*                                         *'
               DISPLAY '***************** GFCT4625 ****************'
               MOVE 4                  TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    LEITURA DO ARQUIVO TRABALHO E FORMATACAO DE CHAVE.          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-LER-TRABALHO               SECTION.
      *----------------------------------------------------------------*

           READ TRABALHO.

           IF  WRK-FS-TRABALHO         EQUAL '10'
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-TRABALHO.

           MOVE V0-CSERVC-TARIF        TO WRK-AUX-5-S
           MOVE WRK-AUX-5              TO WRK-CHV-ATU-TARIFA.
           MOVE V0-DOCOR-EVNTO-AMD     TO WRK-DATA-EVT-AMD.
           MOVE WRK-DIA-EVT-AMD        TO WRK-DIA-INV.
           MOVE WRK-MES-EVT-AMD        TO WRK-MES-INV.
           MOVE WRK-ANO-EVT-AMD        TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-CHV-ATU-OCOR.

           ADD 1                       TO ACU-LIDOS-TRABALHO.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF (WRK-CHAVE-ATUAL         NOT EQUAL WRK-CHAVE-ANTERIOR)
               PERFORM 3100-BUSCAR-DADOS-TARIFA
               MOVE WRK-CHAVE-ATUAL    TO WRK-CHAVE-ANTERIOR
           END-IF.

********   XISPLAY '                                                  '.
********   XISPLAY '******************* NOVO REGISTRO ****************'.
********   XISPLAY 'TARIFA             = ' V0-CSERVC-TARIF.
********   XISPLAY 'DATA OCOR.         = ' V0-DOCOR-EVNTO.
********   XISPLAY 'WRK-DT-SISTEMA     = ' WRK-DT-SISTEMA.
********   XISPLAY 'CPER_CALC_TARIF    = ' CPER-CALC-TARIF.
********   XISPLAY '   1=DIA / 2=SEM /3=QUINZ 4=MES'.
********   XISPLAY 'CINIC_PER_CALC     = ' CINIC-PER-CALC.
********   XISPLAY '   1DOM /2SEG /3TER /4QUA /5QUI/ 6SEX /7SAB'.
********   XISPLAY 'DDIA_COBR_TARIF    = ' DDIA-COBR-TARIF.
********   XISPLAY 'QDIA_RTCAO_COBR    = ' QDIA-RTCAO-COBR.
********   XISPLAY 'QDIA_COBR_TARIF    = ' QDIA-COBR-TARIF.
********   XISPLAY 'CIDTFD_COBR_FERI   = ' CIDTFD-COBR-FERI.
********   XISPLAY 'CTPO-SERVC-TARIF   = 'CTPO-SERVC-TARIF OF GFCTB0D8.

           IF (WRK-TARIFA-ENCONTRADA   EQUAL 'S')
0208             IF V0-DAGNDA-DEB-MOVTO   EQUAL '01.01.0001'
0208                PERFORM 3200-APURAR-DT-AGENDAMENTO
1208                PERFORM 3260-TRATAR-QDIA-COBR
080928              PERFORM 3250-TRATAR-DIA-COBRANCA
0109*************___PERFORM_3300-TRATAR-DIAS-RETENCAO
080928              PERFORM 3330-TRATAR-OUTROS-ACERTOS
0208             ELSE
0208                PERFORM 3350-TRATAR-DATA-PREENCHIDA
0208             END-IF
                 IF CINDCD-PER-COBR OF GFCTB0A2 GREATER ZEROS
                    MOVE CINDCD-PER-COBR OF GFCTB0A2 TO
                         WRK-CAMPO-TIRA-SINAL
                    MOVE WRK-CAMPO-TIRA-SINAL-01         TO
                         WRK-CINDCD-PER-COBR
                 ELSE
                    MOVE ZEROS TO WRK-CINDCD-PER-COBR
                 END-IF
                 IF WRK-CINDCD-PER-COBR NOT EQUAL ZEROS AND
                   (V0-CTRATO-MOVTO     EQUAL 3 OR 4)
                    PERFORM 3340-TRATAR-PER-COBR
                 END-IF
                 PERFORM 3400-GRAVAR-ATUTAB092
           ELSE
      ****    XISPLAY '*** REGISTRO NAO ENCONTRADO NA TABELA GFCTB0A2 '
      ****    XISPLAY '*** TARIFA:' V0-CSERVC-TARIF
      ****             ' DATA OCORRENCIA: ' V0-DOCOR-EVNTO
              ADD 1                   TO ACU-TARIF-NAO-ENCONT
           END-IF.

           PERFORM 2100-LER-TRABALHO.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTEM DADOS DA TARIFA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-BUSCAR-DADOS-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE V0-CSERVC-TARIF        TO CSERVC-TARIF      OF GFCTB0A2.
           MOVE V0-DOCOR-EVNTO         TO WRK-DOCOR-EVNTO.

           EXEC SQL
             SELECT
                   CPER_CALC_TARIF,
                   CINIC_PER_CALC,
260208             DDIA_COBR_TARIF,
                   QDIA_RTCAO_COBR,
                   DINIC_VGCIA_TARIF,
                   DFIM_VGCIA_TARIF,
1208               QDIA_COBR_TARIF,
1208               CIDTFD_COBR_FERI,
                   CINDCD_PER_COBR
             INTO
                   :GFCTB0A2.CPER-CALC-TARIF,
                   :GFCTB0A2.CINIC-PER-CALC,
260208             :GFCTB0A2.DDIA-COBR-TARIF,
                   :GFCTB0A2.QDIA-RTCAO-COBR,
                   :GFCTB0A2.DINIC-VGCIA-TARIF,
                   :GFCTB0A2.DFIM-VGCIA-TARIF,
1208               :GFCTB0A2.QDIA-COBR-TARIF,
1208               :GFCTB0A2.CIDTFD-COBR-FERI,
                   :GFCTB0A2.CINDCD-PER-COBR
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE
                   CSERVC_TARIF        = :GFCTB0A2.CSERVC-TARIF
               AND :WRK-DOCOR-EVNTO    BETWEEN DINIC_VGCIA_TARIF
                                           AND DFIM_VGCIA_TARIF
              ORDER  BY HIDTFD_MANUT_SERVC DESC
030921       FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
               MOVE 'SELECT    '       TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF (SQLCODE                 EQUAL +100)
               MOVE 'N'                TO WRK-TARIFA-ENCONTRADA
           ELSE
               MOVE DINIC-VGCIA-TARIF  OF GFCTB0A2
                                       TO WRK-DATA-EDI
               MOVE WRK-DIA-EDI        TO WRK-DIA-INV
               MOVE WRK-MES-EDI        TO WRK-MES-INV
               MOVE WRK-ANO-EDI        TO WRK-ANO-INV
               MOVE WRK-DATA-INV       TO WRK-INIC-VIG
               MOVE DFIM-VGCIA-TARIF   OF GFCTB0A2
                                       TO WRK-DATA-EDI
               MOVE WRK-DIA-EDI        TO WRK-DIA-INV
               MOVE WRK-MES-EDI        TO WRK-MES-INV
               MOVE WRK-ANO-EDI        TO WRK-ANO-INV
               MOVE WRK-DATA-INV       TO WRK-FIM-VIG
               MOVE 'S'                TO WRK-TARIFA-ENCONTRADA
           END-IF.

0709  *-------------------- BUSCA CTPO_SERVC_TARIF --------------------*
0709       MOVE V0-CSERVC-TARIF        TO CSERVC-TARIF      OF GFCTB0D8.
0709
0709       EXEC SQL
0709         SELECT
0709               CTPO_SERVC_TARIF
0709         INTO
0709               :GFCTB0D8.CTPO-SERVC-TARIF
0709         FROM   DB2PRD.SERVC_TARIF_PRINC
0709         WHERE
0709               CSERVC_TARIF        = :GFCTB0D8.CSERVC-TARIF
0709       END-EXEC.
0709
0709       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0709          (SQLWARN0                EQUAL 'W'               )
0709           MOVE 'DB2'              TO ERR-TIPO-ACESSO
0709           MOVE 'SERVC_TARIF_PRIN' TO ERR-DBD-TAB
0709           MOVE 'SELECT    '       TO ERR-FUN-COMANDO
0709           MOVE SQLCODE            TO ERR-SQL-CODE
0709           MOVE '0020'             TO ERR-LOCAL
0709           MOVE SPACES             TO ERR-SEGM
0709           PERFORM 9999-PROCESSAR-ROTINA-ERRO
0709       END-IF.
0709
0709       IF (SQLCODE                 EQUAL +100)
0709           MOVE 'X'                TO WRK-TARIFA-ENCONTRADA
0709       ELSE
0709           MOVE 'S'                TO WRK-TARIFA-ENCONTRADA
0709       END-IF.

080928***  XISPLAY ' '.
080928***  XISPLAY '*********************************************'.
080928***  XISPLAY ' '.
080928***  XISPLAY '** REGISTRO **'.
080928***  XISPLAY 'TARIFA             = 'CSERVC-TARIF     OF GFCTB0A2.
080928***  XISPLAY 'DATA OCOR.         = 'V0-DOCOR-EVNTO.
080928***  XISPLAY 'WRK-DT-SISTEMA     = 'WRK-DT-SISTEMA.
080928***  XISPLAY 'CTPO-SERVC-TARIF   = 'CTPO-SERVC-TARIF OF GFCTB0D8.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    APURA DATA DE AGENDAMENTO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-APURAR-DT-AGENDAMENTO      SECTION.
      *----------------------------------------------------------------*

080928***  XISPLAY ' '.
080928**   XISPLAY '** APURA DATA DE AGENDAMENTO **'.
080928**   XISPLAY 'PERIODO DE CALCULO = 'CPER-CALC-TARIF.

           MOVE V0-GFCTB092                TO REG-GFCTB092.
           MOVE V0-DOCOR-EVNTO             TO WRK-DT-APURADA.
0503       MOVE WRK-MES-DT-APURADA         TO WRK-MES-DT-APURADA-C.

0709       IF   CTPO-SERVC-TARIF           OF GFCTB0D8
0709                                    EQUAL 2
0709            MOVE WRK-DT-SISTEMA        TO WRK-DT-APURADA
0709            MOVE 01                    TO WRK-DIA-DT-APURADA
0709            MOVE WRK-DT-APURADA        TO A3-DAGNDA-DEB-MOVTO
0709            GO TO 3200-DATA-APURADA
0709       END-IF.

           EVALUATE CPER-CALC-TARIF        OF GFCTB0A2

               WHEN  1
                   MOVE WRK-DT-SISTEMA     TO WRK-DT-APURADA
                                              A3-DAGNDA-DEB-MOVTO
080928***  XISPLAY 'DATA SISTEMA EH MOVIDA PRA APURADA E AGENDAMENTO'
               WHEN  2

                   MOVE WRK-DIA-DT-APURADA TO WRK-DIA-INV
                   MOVE WRK-MES-DT-APURADA TO WRK-MES-INV
                   MOVE WRK-ANO-DT-APURADA TO WRK-ANO-INV
                   MOVE CINIC-PER-CALC     OF GFCTB0A2
                                           TO WRK-AUX-1-S
                   MOVE WRK-AUX-1          TO IND-1
                   MOVE 001                TO WRK-INCREMENTO-1640
                   MOVE 'DC'               TO WRK-TP-INCREM-1640
                   MOVE SPACES             TO WRK-DIA-SEMANA

                   PERFORM 3220-BUSCAR-DIA-SEMANA  UNTIL
                       WRK-DIA-SEMANA      EQUAL TAB-DIA-SEMANA(IND-1)

080928***  XISPLAY 'EH SOMADO 1 DIA CORRIDO NA DATA ATE CHEGAR NO DIA'
      ***  XISPLAY 'DA SEMANA DO CINIC-PER-CALC = 'CINIC-PER-CALC

                   IF (WRK-OPCAO           NOT EQUAL 'U')
                       MOVE 001            TO WRK-INCREMENTO-1640
                       MOVE 'DU'           TO WRK-TP-INCREM-1640
                       PERFORM 3210-ACESSAR-POOL1640

080928***  XISPLAY 'OBTEM PROXIMO DIA UTIL DA DATA = 'WRK-DT-APURADA

                   END-IF
                   PERFORM 3222-VERIFICAR-DATA-SEMANAL
               WHEN  3
                   IF  WRK-DIA-DT-APURADA  LESS 16
                       MOVE 16             TO WRK-DIA-DT-APURADA
080928***  XISPLAY 'SE DIA EH MENOR QUE DIA 16, EH MOVIDO 16 PRO DIA'
                   ELSE
                       MOVE 01             TO WRK-DIA-DT-APURADA
                       MOVE WRK-MES-DT-APURADA   TO WRK-AUX-MES
                       ADD  01             TO WRK-AUX-MES
                                              WRK-MES-DT-APURADA-C
                       MOVE WRK-AUX-MES    TO  WRK-MES-DT-APURADA
                       IF   WRK-MES-DT-APURADA-C  GREATER 12
                            MOVE 1             TO WRK-MES-DT-APURADA
                            MOVE WRK-ANO-DT-APURADA  TO WRK-AUX-ANO
                            ADD  1             TO WRK-AUX-ANO
                            MOVE WRK-AUX-ANO   TO WRK-ANO-DT-APURADA
                       END-IF
080928***  XISPLAY 'MOVE 01 PARA DIA E XDD 1 NO MES'
                   END-IF

               WHEN  4
                   MOVE 01                 TO WRK-DIA-DT-APURADA
                   MOVE WRK-MES-DT-APURADA TO WRK-AUX-MES
                   ADD  01                 TO WRK-AUX-MES
                                              WRK-MES-DT-APURADA-C
                   MOVE WRK-AUX-MES        TO WRK-MES-DT-APURADA
                   IF   WRK-MES-DT-APURADA-C  GREATER 12
                        MOVE 1             TO WRK-MES-DT-APURADA
                        MOVE WRK-ANO-DT-APURADA TO WRK-AUX-ANO
                        ADD  1             TO WRK-AUX-ANO
                        MOVE WRK-AUX-ANO    TO WRK-ANO-DT-APURADA
                   END-IF

080928***  XISPLAY 'MOVE 01 PARA DIA E XDD 1 NO MES'
301007         WHEN  OTHER

080928***  XISPLAY 'CINIC-PER-CALC = OUTRO!'


301007             MOVE WRK-DT-SISTEMA     TO WRK-DT-APURADA
301007                                        A3-DAGNDA-DEB-MOVTO
080928***  XISPLAY 'DATA SISTEMA EH MOVIDA PRA APURADA E AGENDAMENTO'
           END-EVALUATE.

      ******************************************************************
      *    FIM DAS REGRAS DE APURACAO                                  *
      ******************************************************************
0709   3200-DATA-APURADA.

080928***  XISPLAY ' '.
080928***  XISPLAY 'VERIFICAR SE DATA APURADA EH DIA UTIL OU NAO'.
080928***  XISPLAY 'DATA APURADA ATUAL = 'WRK-DT-APURADA.

080928     MOVE WRK-DT-APURADA  TO WRK-DATA-APU.
080928     MOVE WRK-APU-DIA     TO WRK-DIA-1205.
080928     MOVE WRK-APU-MES     TO WRK-MES-1205.
080928     MOVE WRK-APU-ANO     TO WRK-ANO-1205.

080928     PERFORM 3251-ACESSAR-POOL1205.

080928     MOVE WRK-ANO-1205    TO WRK-ANO-DT-APURADA.
080928     MOVE WRK-MES-1205    TO WRK-MES-DT-APURADA.
080928     MOVE WRK-DIA-1205    TO WRK-DIA-DT-APURADA.

080928***  XISPLAY 'DATA APURADA DIA UTIL = 'WRK-DT-APURADA.
080928***  XISPLAY 'DATA AGENDAMENTO      = 'A3-DAGNDA-DEB-MOVTO.
      ***  XISPLAY '**** DT.APURADA CONFORME PERIODO = ' WRK-DT-APURADA.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    SOMA QUANTIDADE DE DIAS CORRIDOS NA DATA APURADA.           *
      ******************************************************************
      *----------------------------------------------------------------*
       3210-ACESSAR-POOL1640           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DT-APURADA         TO WRK-DT-ENTRADA-1640.
           MOVE 09                     TO WRK-FORMATO-ENT-1640
                                          WRK-FORMATO-SAI-1640.

           CALL 'POOL1640'             USING WRK-DT-ENTRADA-1640
                                             WRK-FORMATO-ENT-1640
                                             WRK-TP-INCREM-1640
                                             WRK-INCREMENTO-1640
                                             WRK-FORMATO-SAI-1640
                                             WRK-DT-SAIDA-1640
                                             WRK-MENSAGEM-1640.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MENSAGEM-1640  TO WRK-MENSAGEM-1640-E
               MOVE WRK-MENSAGEM-1640-R   TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-DT-SAIDA-1640      TO WRK-DT-APURADA.

      *----------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ENCONTRA PROXIMO DIA DA SEMANA DEFINIDO EM CINIC-PER-CALC   *
      ******************************************************************
      *---------------------------------------------------------------*
       3220-BUSCAR-DIA-SEMANA          SECTION.
      *---------------------------------------------------------------*

           PERFORM 3210-ACESSAR-POOL1640.

           MOVE WRK-DT-SAIDA-1640      TO WRK-DATA-EDI.
           MOVE WRK-DIA-EDI            TO WRK-DIA-INV.
           MOVE WRK-MES-EDI            TO WRK-MES-INV.
           MOVE WRK-ANO-EDI            TO WRK-ANO-INV.
           MOVE WRK-DATA-INV           TO WRK-DT-ENVIADA.

           PERFORM 3221-ACESSAR-POOL1205.

           MOVE WRK-DT-EDITADA         TO WRK-DT-APURADA.
           MOVE '.'                    TO WRK-PONTO-3
                                          WRK-PONTO-4.

      *---------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      ******************************************************************
      *    IDENTIFICA DIA DA SEMANA E INFORMACOES ADICIONAIS           *
      ******************************************************************
      *---------------------------------------------------------------*
       3221-ACESSAR-POOL1205           SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO WRK-OPCAO.

           CALL  'POOL1205'            USING WRK-AREA-POOL1205
                                             WRK-MENSAGEM-1205.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-ERRO-P1205     TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3221-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3222-VERIFICAR-DATA-SEMANAL    SECTION.
      *---------------------------------------------------------------*

080928***  XISPLAY ' '.
080928***  XISPLAY 'VERIFICAR SE DIA OBTIDO NO SEMANAL EH MENOR QUE '
      ***          'DATA DO SISTEMA'.

080928     MOVE WRK-DT-APURADA     TO WRK-DT-DB2.
080928     MOVE WRK-DIA-DB2        TO WRK-DIA-S-INV.
080928     MOVE WRK-MES-DB2        TO WRK-MES-S-INV.
080928     MOVE WRK-ANO-DB2        TO WRK-ANO-S-INV.
080928
      ***  XISPLAY 'DATA AGEND. OBTIDA = 'WRK-DATA-S-INV

080928     MOVE WRK-DIA-DT-SISTEMA TO WRK-DIA-SISTEMA.
080928     MOVE WRK-MES-DT-SISTEMA TO WRK-MES-SISTEMA.
080928     MOVE WRK-ANO-DT-SISTEMA TO WRK-ANO-SISTEMA.

      ***  XISPLAY 'DATA SISTEMA = 'WRK-DT-SISTEMA-INV

           IF WRK-DATA-S-INV LESS WRK-DT-SISTEMA-INV
               MOVE WRK-DIA-DT-APURADA TO WRK-DIA-INV
               MOVE WRK-MES-DT-APURADA TO WRK-MES-INV
               MOVE WRK-ANO-DT-APURADA TO WRK-ANO-INV
               MOVE CINIC-PER-CALC     OF GFCTB0A2
                                       TO WRK-AUX-1-S
               MOVE WRK-AUX-1          TO IND-1
               MOVE 001                TO WRK-INCREMENTO-1640
               MOVE 'DC'               TO WRK-TP-INCREM-1640
               MOVE SPACES             TO WRK-DIA-SEMANA

               PERFORM 3220-BUSCAR-DIA-SEMANA  UNTIL
                   WRK-DIA-SEMANA      EQUAL TAB-DIA-SEMANA(IND-1)

080928***  XISPLAY 'EH SOMADO 1 DIA CORRIDO NA DATA ATE CHEGAR NO DIA'
      ***  XISPLAY 'DA SEMANA DO CINIC-PER-CALC = 'CINIC-PER-CALC

               IF (WRK-OPCAO           NOT EQUAL 'U')
080928***  XISPLAY 'OBTEM PROXIMO DIA UTIL DA DATA = 'WRK-DT-APURADA
                   MOVE 001            TO WRK-INCREMENTO-1640
                   MOVE 'DU'           TO WRK-TP-INCREM-1640
                   PERFORM 3210-ACESSAR-POOL1640

               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3222-99-FIM.                   EXIT.
      *---------------------------------------------------------------*

260208******************************************************************
080928*    TRATA DIA DE COBRANCA - INCLUIDA EM 26/02/2008              *
080928******************************************************************
080928*----------------------------------------------------------------*
080928 3250-TRATAR-DIA-COBRANCA        SECTION.
080928*----------------------------------------------------------------*
080928
080928*    DIA DA COBRANCA EH O DIA UTIL DO MES EM QUE A COBRANCA SERA
080928*    EFETUADA. SE MENOR QUE A DATA DE AGENDAMENTO APURADA, COBRAR
080928*    NO PROXIMO MES.
080928
080928     MOVE WRK-DIA-DT-APURADA TO WRK-DIA-APU-I.
080928     MOVE WRK-MES-DT-APURADA TO WRK-MES-APU-I.
080928     MOVE WRK-ANO-DT-APURADA TO WRK-ANO-APU-I.
080928
080928     IF (V0-CTRATO-MOVTO     EQUAL 3 OR 4)
1208           IF ((DDIA-COBR-TARIF OF GFCTB0A2 GREATER ZEROS) AND
1208              (QDIA-COBR-TARIF  OF GFCTB0A2 EQUAL   ZEROS))
080928             MOVE WRK-DT-APURADA  TO WRK-DATA-APU
080928             MOVE 01              TO WRK-DIA-1205
080928             MOVE WRK-APU-MES     TO WRK-MES-1205
080928             MOVE WRK-APU-ANO     TO WRK-ANO-1205
080928             PERFORM 3251-ACESSAR-POOL1205
080928             MOVE ZEROS              TO WRK-INCREMENTO-1640-COMP
080928             ADD  DDIA-COBR-TARIF    TO WRK-INCREMENTO-1640-COMP
080928             SUBTRACT 1            FROM WRK-INCREMENTO-1640-COMP
080928             MOVE WRK-INCREMENTO-1640-COMP
080928                                     TO WRK-INCREMENTO-1640
080928             MOVE 'DU'               TO WRK-TP-INCREM-1640
080928             MOVE WRK-ANO-1205       TO WRK-ANO-DT-APURADA
080928             MOVE WRK-MES-1205       TO WRK-MES-DT-APURADA
080928             MOVE WRK-DIA-1205       TO WRK-DIA-DT-APURADA
080928             MOVE '.'                TO WRK-PONTO-3
080928                                        WRK-PONTO-4
080928             PERFORM 3210-ACESSAR-POOL1640
080928             MOVE WRK-ANO-DT-APURADA TO WRK-ANO-1205
080928             MOVE WRK-MES-DT-APURADA TO WRK-MES-1205
080928             MOVE WRK-DIA-DT-APURADA TO WRK-DIA-1205
080928
080928             IF  WRK-DT-APURADA-I    NOT GREATER WRK-DATA-1205
080928                 MOVE WRK-DIA-1205   TO WRK-DIA-DT-APURADA
080928                 MOVE WRK-MES-1205   TO WRK-MES-DT-APURADA
080928                 MOVE WRK-ANO-1205   TO WRK-ANO-DT-APURADA
080928                 MOVE '.'            TO WRK-PONTO-3
080928                                        WRK-PONTO-4
080928             ELSE
                       MOVE WRK-MES-1205   TO WRK-MES-1205-COMP
                       MOVE WRK-ANO-1205   TO WRK-ANO-1205-COMP
080928                 IF WRK-MES-1205 EQUAL 12
080928                     MOVE 01         TO WRK-MES-1205
080928                     ADD   1         TO WRK-ANO-1205-COMP
080928                     MOVE WRK-ANO-1205-COMP
080928                                     TO WRK-ANO-1205
080928                 ELSE
080928                     ADD 1           TO WRK-MES-1205-COMP
080928                     MOVE WRK-MES-1205-COMP
080928                                     TO WRK-MES-1205
080928                 END-IF
080928                 MOVE 01             TO WRK-DIA-1205
080928                 PERFORM 3251-ACESSAR-POOL1205
080928                 MOVE ZEROS              TO
080928                                         WRK-INCREMENTO-1640-COMP
080928                 ADD  DDIA-COBR-TARIF    TO
080928                                         WRK-INCREMENTO-1640-COMP
080928                 SUBTRACT 1            FROM
080928                                         WRK-INCREMENTO-1640-COMP
080928                 MOVE WRK-INCREMENTO-1640-COMP
080928                                         TO WRK-INCREMENTO-1640
080928                 MOVE 'DU'               TO WRK-TP-INCREM-1640
080928                 MOVE WRK-ANO-1205       TO WRK-ANO-DT-APURADA
080928                 MOVE WRK-MES-1205       TO WRK-MES-DT-APURADA
080928                 MOVE WRK-DIA-1205       TO WRK-DIA-DT-APURADA
080928                 MOVE '.'                TO WRK-PONTO-3
080928                                            WRK-PONTO-4
080928                 PERFORM 3210-ACESSAR-POOL1640
080928                 MOVE WRK-ANO-DT-APURADA TO WRK-ANO-1205
080928                 MOVE WRK-MES-DT-APURADA TO WRK-MES-1205
080928                 MOVE WRK-DIA-DT-APURADA TO WRK-DIA-1205
080928                 MOVE WRK-DIA-1205   TO WRK-DIA-DT-APURADA
080928                 MOVE WRK-MES-1205   TO WRK-MES-DT-APURADA
080928                 MOVE WRK-ANO-1205   TO WRK-ANO-DT-APURADA
080928                 MOVE '.'            TO WRK-PONTO-3
080928                                        WRK-PONTO-4
080928             END-IF
               END-IF
           END-IF.
      ***  XISPLAY '**** DDIA-COBR - SOMENTE PARA TIPO 3 E 4'
      ***  XISPLAY '**** DT.APURADA C/ DIA COBR DDIA = ' WRK-DT-APURADA.
080928
080928*---------------------------------------------------------------*
080928 3250-99-FIM.                    EXIT.
080928*---------------------------------------------------------------*

080928*----------------------------------------------------------------*
080928 3251-ACESSAR-POOL1205       SECTION.
080928*----------------------------------------------------------------*
080928
080928     MOVE WRK-DATA-1205 TO WRK-DT-ENVIADA.
080928     MOVE SPACES        TO WRK-OPCAO.
080928
080928     CALL  'POOL1205'   USING WRK-AREA-POOL1205
080928                              WRK-MENSAGEM-1205.
080928
080928
080928     IF  RETURN-CODE    NOT EQUAL ZEROS OR
080928         WRK-OPCAO      EQUAL 'I'
080928         MOVE 'APL'          TO ERR-TIPO-ACESSO
080928         MOVE WRK-ERRO-P1205 TO ERR-TEXTO
080928         PERFORM 9999-PROCESSAR-ROTINA-ERRO
080928     END-IF.
080928
080928     IF WRK-OPCAO NOT EQUAL 'U'
080928         MOVE WRK-DIA-UT-POS TO WRK-DATA
080928         MOVE WRK-DIA        TO WRK-DIA-1205
080928         MOVE WRK-MES        TO WRK-MES-1205
080928         MOVE WRK-ANO        TO WRK-ANO-1205
080928     END-IF.
080928
080928*---------------------------------------------------------------*
080928 3251-99-FIM.                    EXIT.
080928*---------------------------------------------------------------*

1208  ******************************************************************
1208  *    TRATA DIA DE COBRANCA - INCLUIDA EM 22/12/2008              *
1208  ******************************************************************
1208  *----------------------------------------------------------------*
1208   3260-TRATAR-QDIA-COBR           SECTION.
1208  *----------------------------------------------------------------*
1208
1208  *    QDIA-COBR-TARIF E O DIA EFETIVO EM QUE A TARIFA SERA COBRADA
1208  *    SE O DIA NAO FOR UTIL VERIFICA CIDTFD-COBR-FERI.
1208  *       SE = S -> COBRA NO DIA UTIL ANTERIOR
1208  *       SENAO  -> COBRA NO DIA UTIL POSTERIOR
1208
1208***    XISPLAY '**************************************************'
1208***    XISPLAY '**************************************************'
1208***    XISPLAY '** TRATA QDIA-COBR ANTES  **'
1208***    XISPLAY 'TRATO-MOVTO        = ' V0-CTRATO-MOVTO
1208***    XISPLAY 'DATA APURADA ANTES = ' WRK-DT-APURADA
1208***    XISPLAY 'QDIA-COBR-TARIF    = ' QDIA-COBR-TARIF
1208***    XISPLAY 'DDIA-COBR-TARIF    = ' DDIA-COBR-TARIF
1208***    XISPLAY 'CIDTFD-COBR-FERI   = ' CIDTFD-COBR-FERI
1208***    XISPLAY ' '
1208
1208       IF (V0-CTRATO-MOVTO     EQUAL 3 OR 4)
1208           IF ((QDIA-COBR-TARIF OF GFCTB0A2 GREATER ZEROS) AND
1208              ( DDIA-COBR-TARIF OF GFCTB0A2 EQUAL   ZEROS))
1208               MOVE WRK-DT-APURADA  TO WRK-DATA-APU
                   MOVE QDIA-COBR-TARIF TO WRK-AUX-2-S
                   MOVE WRK-AUX-2       TO WRK-DIA-1205
1208               MOVE WRK-APU-MES     TO WRK-MES-1205
1208               MOVE WRK-APU-ANO     TO WRK-ANO-1205
1208               PERFORM 3261-ACESSAR-POOL1205
1208           END-IF
1208       END-IF.
    *****  XISPLAY '**** QDIA-COBR - SOMENTE PARA TIPO 3 E 4'
    *****  XISPLAY '**** DT.APURADA C/ DIA FIXO QDIA = ' WRK-DT-APURADA.
1208
1208  *---------------------------------------------------------------*
1208   3260-99-FIM.                    EXIT.
1208  *---------------------------------------------------------------*

1208  *----------------------------------------------------------------*
1208   3261-ACESSAR-POOL1205       SECTION.
1208  *----------------------------------------------------------------*
1208
1208       MOVE WRK-DATA-1205 TO WRK-DT-ENVIADA.
1208       MOVE SPACES        TO WRK-OPCAO.
1208
1208       CALL  'POOL1205'   USING WRK-AREA-POOL1205
1208                                WRK-MENSAGEM-1205.
1208
1208       IF  RETURN-CODE    NOT EQUAL ZEROS OR
1208           WRK-OPCAO      EQUAL 'I'
1208           MOVE 'APL'          TO ERR-TIPO-ACESSO
1208           MOVE WRK-ERRO-P1205 TO ERR-TEXTO
1208           PERFORM 9999-PROCESSAR-ROTINA-ERRO
1208       END-IF.
1208
1208       IF  WRK-OPCAO              EQUAL 'U'
1208           MOVE WRK-DIA-1205         TO WRK-DIA-DT-APURADA
1208           MOVE WRK-MES-1205         TO WRK-MES-DT-APURADA
1208           MOVE WRK-ANO-1205         TO WRK-ANO-DT-APURADA
1208           MOVE '.'                  TO WRK-PONTO-3
1208                                        WRK-PONTO-4
1208       ELSE
1208           IF   CIDTFD-COBR-FERI  EQUAL 'S'
1208                MOVE WRK-DIA-UT-ANT  TO WRK-DATA
1208                MOVE WRK-DIA         TO WRK-DIA-DT-APURADA
1208                MOVE WRK-MES         TO WRK-MES-DT-APURADA
1208                MOVE WRK-ANO         TO WRK-ANO-DT-APURADA
1208                MOVE '.'             TO WRK-PONTO-3
1208                                        WRK-PONTO-4
1208           ELSE
1208                MOVE WRK-DIA-UT-POS  TO WRK-DATA
1208                MOVE WRK-DIA         TO WRK-DIA-DT-APURADA
1208                MOVE WRK-MES         TO WRK-MES-DT-APURADA
1208                MOVE WRK-ANO         TO WRK-ANO-DT-APURADA
1208                MOVE '.'             TO WRK-PONTO-3
1208                                        WRK-PONTO-4
1208           END-IF
1208       END-IF.
1208
1208****   XISPLAY '** TRATA QDIA-COBR APOS   **'
1208****   XISPLAY 'WRK-OPCAO          = ' WRK-OPCAO
1208****   XISPLAY 'CIDTFD-COBR-FERI   = ' CIDTFD-COBR-FERI
1208****   XISPLAY 'WRK-DIA-UT-ANT     = ' WRK-DIA-UT-ANT
1208****   XISPLAY 'WRK-DIA-UT-POS     = ' WRK-DIA-UT-POS
1208****   XISPLAY 'WRK-DT-APURADA     = ' WRK-DT-APURADA
1208****   XISPLAY ' '.
1208
1208  *---------------------------------------------------------------*
1208   3261-99-FIM.                    EXIT.
1208  *---------------------------------------------------------------*

      ******************************************************************
      *    TRATA DIAS DE RETENCAO                                      *
      ******************************************************************
______*----------------------------------------------------------------*
______*3300-TRATAR-DIAS-RETENCAO_______SECTION.
______*----------------------------------------------------------------*
______*
080928*____DIAS_DE_RETENCAO_SAO_DIAS_UTEIS_A_ACRESCENTAR_NA_DATA_APURADA
______*
______*****IF_(V0-CTRATO-MOVTO_____________EQUAL_3*OR*4)
______*********IF_(QDIA-RTCAO-COBR_________OF_GFCTB0A2
______*____________________________________GREATER_ZEROS)
______*************MOVE_QDIA-RTCAO-COBR____OF_GFCTB0A2
______*____________________________________TO_WRK-INCREMENTO-1640
______*************MOVE_'DU'_______________TO_WRK-TP-INCREM-1640
______*
______*____XISPLAY_'DATA_APURADA_ANTES_DE_CALCULAR_A_RETENCAO'
______*____XISPLAY_'DATA_APURADA_______=_'WRK-DT-APURADA
______*
______*************PERFORM_3210-ACESSAR-POOL1640
______*
080928*____XISPLAY_'_'
080928*____XISPLAY_'**_TRATA_DIAS_DE_RETENCAO_**'
080928*____XISPLAY_'V0-CTRATO-MOVTO____=_'V0-CTRATO-MOVTO
080928*____XISPLAY_'DIAS_RETENCAO______=_'QDIA-RTCAO-COBR
080928*____XISPLAY_'DATA_APURADA_______=_'WRK-DT-APURADA
______*
080928*____XISPLAY_'_'
080928*____XISPLAY_'QDIA-RTCAO-COBR_IGUAL_A_ZEROS,_NAO_EH_TRATADA_'
______*____________'RETENCAO'
080928*____XISPLAY_'_'
______*********END-IF
080928*____XISPLAY_'_'
080928*____XISPLAY_'V0-CTRATO-MOVTO_DIFERENTE_DE_3_OU_4,_NAO_EH_TRATADA'
______*____________'_RETENCAO'
080928*____XISPLAY_'_'
______*****END-IF.
______*
______*____XISPLAY_'****_DT.APURADA_RETENCAO_________=_'_WRK-DT-APURADA.
______*
______*----------------------------------------------------------------*
______*3300-99-FIM.____________________EXIT.
______*----------------------------------------------------------------*

080928*----------------------------------------------------------------*
080928 3330-TRATAR-OUTROS-ACERTOS     SECTION.
080928*----------------------------------------------------------------*
080928
080928*    SE DATA APURADA INFERIOR A DATA DE PROCESSAMENTO A DATA
080928*    DE AGENDAMENTO SERA A DATA DO PROCESSAMENTO.
0709  *    REGRA RETIRADA EM 14/07/09
080928
080928     MOVE 01                         TO V0-CIDTFD-PGMC-COBR.
080928
080928     IF (CPER-CALC-TARIF             OF GFCTB0A2
080928                                     EQUAL 2 OR 3 OR 4)
080928         MOVE WRK-DIA-DT-APURADA     TO WRK-DIA-INV
080928         MOVE WRK-MES-DT-APURADA     TO WRK-MES-INV
080928         MOVE WRK-ANO-DT-APURADA     TO WRK-ANO-INV
080928         MOVE WRK-DIA-DT-SISTEMA     TO WRK-DIA-SISTEMA
080928         MOVE WRK-MES-DT-SISTEMA     TO WRK-MES-SISTEMA
080928         MOVE WRK-ANO-DT-SISTEMA     TO WRK-ANO-SISTEMA
080928         IF (WRK-DATA-INV            NOT GREATER
080928                                     WRK-DT-SISTEMA-INV)
0709***************XOVE WRK-DT-SISTEMA     TO A3-DAGNDA-DEB-MOVTO
0709               MOVE WRK-DT-APURADA     TO A3-DAGNDA-DEB-MOVTO
080928         ELSE
080928             MOVE WRK-DT-APURADA     TO A3-DAGNDA-DEB-MOVTO
080928             MOVE 02                 TO A3-CIDTFD-PGMC-COBR
080928             IF (V0-CTRATO-MOVTO     EQUAL 3 OR 4)
080928                 IF (QDIA-RTCAO-COBR GREATER ZEROS)
080928                     MOVE 03         TO A3-CIDTFD-PGMC-COBR
080928                     MOVE 17         TO A3-CSIT-EVNTO-RECBD
080928                 END-IF
080928             END-IF
080928         END-IF
080928     END-IF.
080928
080928*    INCLUSAO DE VALOR BRUTO E LIQUIDO PARA MOVTO 1 E 51
080928
080928     IF (V0-CTRATO-MOVTO             EQUAL 1 OR 51)
080928         MOVE V0-VTARIF-CADTR-SIST   TO A3-VTARIF-BRUTO-MOVTO
080928                                        A3-VTARIF-LIQ-MOVTO
080928     END-IF.
080928
080928*    INCLUSAO DE DATA DE AGENDAMENTO PARA MOVTO 3 E 4
080928
080928     IF (A3-VTARIF-LIQ-MOVTO         EQUAL ZEROS) AND
080928        (V0-VTARIF-CADTR-SIST    NOT EQUAL ZEROS) AND
080928        (V0-CTRATO-MOVTO             EQUAL 3 OR 4)
080928         MOVE 17                     TO A3-CSIT-EVNTO-RECBD
080928         MOVE A3-DAGNDA-DEB-MOVTO    TO A3-DEFETV-DEB-MOVTO
080928     ELSE
080928         MOVE 17                     TO A3-CSIT-EVNTO-RECBD
080928     END-IF.
*********  XISPLAY '**** DT.APURADA FINAL            = '
*********                                           A3-DAGNDA-DEB-MOVTO.

260410*    TRATAMENTO PARA MOVTO = 2
260410
260410     IF V0-CTRATO-MOVTO = 2
260410        MOVE  V0-VTARIF-BRUTO-MOVTO  TO A3-VTARIF-LIQ-MOVTO
260410     END-IF.
260410
080928*----------------------------------------------------------------*
080928 3330-99-FIM.                    EXIT.
080928*----------------------------------------------------------------*

      ******************************************************************
      *    TRATA PERIODO DE COBRANCA                                   *
      ******************************************************************
       3340-TRATAR-PER-COBR SECTION.

______*____MOVE_V0-GFCTB092________________TO_REG-GFCTB092.

           IF WRK-TARIFA-ENCONTRADA EQUAL 'S'
              MOVE A3-DENVIO-MOVTO-TARIF  TO
                   C01-DENVIO-MOVTO-TARIF
              MOVE A3-CSERVC-TARIF        TO
                   C01-CSERVC-TARIF
              MOVE A3-CTPO-CTA-MOVTO      TO
                   C01-CTPO-CTA-MOVTO
              MOVE A3-DAGNDA-DEB-MOVTO    TO
                   C01-DAGNDA-DEB-MOVTO
              MOVE A3-CSIT-EVNTO-RECBD    TO
                   C01-CSIT-EVNTO-RECBD
              MOVE A3-DOCOR-EVNTO         TO
                   C01-DOCOR-EVNTO
              MOVE WRK-CINDCD-PER-COBR    TO
                   C01-PERIODICI
              MOVE SPACES                 TO
                   C01-DAGNDA-DEB-CALC
                   C01-MSG-ERRO
              MOVE ZEROS                  TO
                   C01-COD-RETORNO
                   C01-LOCAL-ERRO
              MOVE 'GFCT5410' TO WRK-MODULO
              CALL WRK-MODULO USING REG-GFCTWC01
              IF C01-COD-RETORNO EQUAL ZEROS
                 IF C01-DAGNDA-DEB-CALC NOT EQUAL A3-DAGNDA-DEB-MOVTO
                    PERFORM 3341-VERIFICA-MUDANCA-DATA
                 END-IF
              ELSE
                 IF WRK-COUNT-ERROS LESS 100
                    ADD 1 TO WRK-COUNT-ERROS
                    PERFORM 3410-TRATA-ERRO-GFCT5410
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       3340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    VERIFICA MUDANCA DE DATA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       3341-VERIFICA-MUDANCA-DATA      SECTION.
      *----------------------------------------------------------------*

           MOVE C01-DAGNDA-DEB-CALC TO WRK-DATA-DMAB.
           MOVE WRK-DATA-DMAB-DIA   TO WRK-DATA-AMD-DIA.
           MOVE WRK-DATA-DMAB-MES   TO WRK-DATA-AMD-MES.
           MOVE WRK-DATA-DMAB-ANO   TO WRK-DATA-AMD-ANO.
           MOVE WRK-DATA-AMD-R      TO WRK-DT-1.

           MOVE A3-DAGNDA-DEB-MOVTO TO WRK-DATA-DMAB.
           MOVE WRK-DATA-DMAB-DIA   TO WRK-DATA-AMD-DIA.
           MOVE WRK-DATA-DMAB-MES   TO WRK-DATA-AMD-MES.
           MOVE WRK-DATA-DMAB-ANO   TO WRK-DATA-AMD-ANO.
           MOVE WRK-DATA-AMD-R      TO WRK-DT-2.

           IF WRK-DT-2 LESS WRK-DT-1
              IF WRK-CTM NOT GREATER 100
                 MOVE 'N' TO WRK-ACHOU
                 PERFORM VARYING WRK-CTX FROM 1 BY 1 UNTIL
                         WRK-CTX GREATER WRK-CTM
                    MOVE C01-PERIODICI TO WRK-CAMPO-TIRA-SINAL-17
                    IF WRK-DATA-1(WRK-CTX) EQUAL C01-DOCOR-EVNTO AND
                       WRK-PER(WRK-CTX) EQUAL WRK-CAMPO-TIRA-SINAL-01
                       MOVE 'S' TO WRK-ACHOU
                       MOVE WRK-CTM TO WRK-CTX
                    END-IF
                 END-PERFORM
                 IF WRK-ACHOU EQUAL 'N'
                    ADD 1 TO WRK-CTM
                    IF WRK-CTM NOT GREATER 100
                       MOVE C01-DOCOR-EVNTO     TO WRK-DATA-1(WRK-CTM)
                       MOVE C01-PERIODICI       TO
                            WRK-CAMPO-TIRA-SINAL-17
                       MOVE WRK-CAMPO-TIRA-SINAL-01 TO
                            WRK-PER(WRK-CTM)
                       MOVE C01-DAGNDA-DEB-CALC TO WRK-DATA-2(WRK-CTM)
                    END-IF
                 END-IF
              END-IF
              MOVE C01-DAGNDA-DEB-CALC TO A3-DAGNDA-DEB-MOVTO
              ADD 1 TO WRK-COUNT-PER-COBR
           END-IF.

      *----------------------------------------------------------------*
       3341-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      *    TRATA DATA DE AGENDAMENTO PREENCHIDA                        *
      ******************************************************************
      *----------------------------------------------------------------*
0208   3350-TRATAR-DATA-PREENCHIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE V0-GFCTB092                TO REG-GFCTB092.

           MOVE A3-DAGNDA-DEB-MOVTO        TO WRK-DATA-EDI
           MOVE WRK-DIA-EDI                TO WRK-DIA-AGENDTO
           MOVE WRK-MES-EDI                TO WRK-MES-AGENDTO
           MOVE WRK-ANO-EDI                TO WRK-ANO-AGENDTO

           MOVE WRK-DIA-DT-SISTEMA         TO WRK-DIA-SISTEMA
           MOVE WRK-MES-DT-SISTEMA         TO WRK-MES-SISTEMA
           MOVE WRK-ANO-DT-SISTEMA         TO WRK-ANO-SISTEMA

           IF   WRK-DT-AGENDTO-INV          > WRK-DT-SISTEMA-INV
                MOVE 02                    TO A3-CIDTFD-PGMC-COBR
           ELSE
                MOVE 01                    TO A3-CIDTFD-PGMC-COBR
           END-IF.

           MOVE 17                         TO A3-CSIT-EVNTO-RECBD.

080928***  XISPLAY ' '.
080928***  XISPLAY '** 3350-TRATAR-DATA-PREENCHIDA **'.
080928***  XISPLAY ' '.

      *----------------------------------------------------------------*
0208   3350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA QUE EFETUA GRAVACAO DO ARQUIVO ATUTAB092.            *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-GRAVAR-ATUTAB092           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-GFCTB092.

           PERFORM 1120-TESTAR-FS-ATUTAB92.

           ADD 01                      TO ACU-GRAVA-ATUTAB92.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    TRATAMENTO DE ERRO DO MODULO GFCT5410
      ******************************************************************
       3410-TRATA-ERRO-GFCT5410 SECTION.

           MOVE C01-CSERVC-TARIF TO WRK-CAMPO-TIRA-SINAL.
           MOVE WRK-CAMPO-TIRA-SINAL-04 TO WRK-CSERVC-TARIF-DISP.
           MOVE C01-CSIT-EVNTO-RECBD TO WRK-CAMPO-TIRA-SINAL.
           MOVE WRK-CAMPO-TIRA-SINAL-02 TO WRK-CSIT-EVNTO-RECBD.

           DISPLAY ' '.
           DISPLAY '=============   GFCT4620 ===================='.
           DISPLAY '======= ERRO NO MODULO GFCT5410 ============='.
           DISPLAY 'DENVIO = ' C01-DENVIO-MOVTO-TARIF.
           DISPLAY 'TARIF. = ' WRK-CSERVC-TARIF-DISP.
           DISPLAY 'DT.AG. = ' C01-DAGNDA-DEB-MOVTO.
           DISPLAY 'CSIT   = ' WRK-CSIT-EVNTO-RECBD.
           DISPLAY 'DT.EVEN= ' C01-DOCOR-EVNTO.
           DISPLAY 'PERIOD.= ' C01-PERIODICI.


      *----------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAMENTO DE ARQUIVOS, TESTE DE FILE STATUS E ENCERRAMENTO *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE TRABALHO
                 ATUTAB92.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EMITE DISPLEI COM TOTAIS DE PROCESSAMENTO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-TRABALHO     TO WRK-MASCARA.

           DISPLAY '******************** GFCT4625 ********************'.
           DISPLAY '*                                                *'.
           DISPLAY '* REGS. LIDOS - TRABALHO ..........: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.

           MOVE ACU-GRAVA-ATUTAB92     TO WRK-MASCARA.

           DISPLAY '* REGS. GRAVADOS - ATUTAB92 .......: ' WRK-MASCARA
                                                                   ' *'.
           DISPLAY '*                                                *'.

           MOVE ACU-TARIF-NAO-ENCONT   TO WRK-MASCARA.

           DISPLAY '* REGS. NAO ENCONTRADOS - GFCTB0A2 : ' WRK-MASCARA
                                                                   ' *'.
           MOVE WRK-COUNT-PER-COBR   TO WRK-MASC2.

           DISPLAY '* DATA ATUALIZADA POR PER. COBR   : ' WRK-MASC2
                                                                   ' *'.

           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT4625 ********************'.

           PERFORM 4105-DISPLAY-PERIODO.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       4105-DISPLAY-PERIODO            SECTION.
      *----------------------------------------------------------------*
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT4625 ********************'.
           DISPLAY '*      APURACAO CALCULO DA PERIODICIDADE         *'.
           DISPLAY '*                                                *'.
           PERFORM VARYING WRK-CTX FROM 1 BY 1 UNTIL
                           WRK-CTX GREATER WRK-CTM
                   DISPLAY '*      ' WRK-TB(WRK-CTX)
           END-PERFORM.
           DISPLAY '*                                                *'.
           DISPLAY '******************** GFCT4625 ********************'.
           DISPLAY '*                                                *'.


      *----------------------------------------------------------------*
       4105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    EXECUTA PROCEDIMENTO DE CANCELAMENTO DO PGM EM CASO DE ERRO *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT4625'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
