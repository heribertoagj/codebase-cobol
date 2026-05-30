      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CRUR1O11.
       AUTHOR.     RONALDO CORREA.
      *================================================================*
      *             C A P G E M I N I  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: CRUR1O11                                      *
      *    PROGRAMADOR.: RONALDO CORREA          - CAPGEMINI - ALPHA   *
      *    ANALISTA CPM: ANISIO DOS SANTOS       - CAPGEMINI - ALPHA   *
      *    DATA........: 01/08/2014                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: RETORNO DADOS DO SICOR                        *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *    TCTRL_ENVIO_INFO                              CRURB0U2      *
      *    TLIN_MSGEM_REMSS                              CRURB0UA      *
      *    TLIN_MSGEM_RETOR                              CRURB0UB      *
RC    *    TCONTR_CREDT_RURAL                            CRURB008      *
VANS  *    THIST_CREDT_RURAL                             CRURB0LJ      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO)                  *
      *    I#FRWKCI - COMMAREA FRWK1999 (LOG DE ERROS CICS)            *
      *    I#FRWKMD - COMMAREA FRWK1999 (LOG DE ERROS MODULO)          *
      *    I#FRWKDB - COMMAREA FRWK1999 (LOG DE ERROS DB2)             *
      *    CRURWE11 - AREA DE COMUNICACAO COM CHAMADOR                 *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO.     *
      *================================================================*
      * ALTERACOES :                                                   *
      *                                                                *
      * DATA       AUTOR    ALTERACAO
      * ---------- -------- ------------------------------------------ *
      * 03/12/2014 RONALDO  QUANDO RETORNA INCLUSAO OK COR0001R1       *
      *                     UPDATE NA TABELA TCONTR_CREDT_RURAL(6).    *
      *----------------------------------------------------------------*
      * DATA       AUTOR    ALTERACAO
      * ---------- -------- ------------------------------------------ *
      * 23/12/2014 EDUARDO  UPDATE DA DATA DE GERACAO 'IMPRESSAO' NA   *
      *                     TABELA TCONTR_CREDT_RURAL QUANDO FORMALIZA *
AS2108*----------------------------------------------------------------*
.     * DATA       AUTOR    ALTERACAO
.     * ---------- -------- ------------------------------------------ *
AS2108* 21/08/2015 ALEX     INCLUSAO DO HISTORICO DO CONTRATO          *
EL2508* ---------- -------- ------------------------------------------ *
.     * DATA       AUTOR    ALTERACAO
.     * ---------- -------- ------------------------------------------ *
.     * 24/08/2015 EVERTON  PARA CODIGO DE ERRO 'ECOR0021' "DATA DE    *
.     *                     EMISSAO INCOMPATIVEL COM A DATA DE ENVIO"  *
.     *                     NAO GRAVA NA TABELA DE RETORNO PARA QUE O  *
.     *                     MESMO FIQUE DISPONIVEL PARA REENVIO AO SICOR
EL2508*================================================================*
EL1115* ---------- -------- ------------------------------------------ *
.     * DATA       AUTOR    ALTERACAO
.     * ---------- -------- ------------------------------------------ *
.     * 05/11/2015 EVERTON  PARA CODIGO DE ERRO 'EGEN0050' "ERRO DE    *
.     *                     PROCESSAMENTO" NAO GRAVA NA TABELA DE      *
.     *                     PARA QUE O MESMO FIQUE DISPONIVEL PARA     *
.     *                     REENVIO AO SICOR                           *
EL1115*================================================================*
COR6  * ---------- -------- ------------------------------------------ *
COR6  * DATA       AUTOR    ALTERACAO
COR6  * ---------- -------- ------------------------------------------ *
COR6  * 13/12/2015 EVANDRO  TRATAR NOVAS MENSAGENS DE RETORNO COR0006  *
COR6  *                     E COR0006R1                                *
COR6  *================================================================*
BR0618* ---------- -------- ------------------------------------------ *
BR0618* DATA       AUTOR    ALTERACAO
BR0618* ---------- -------- ------------------------------------------ *
BR0618* 22/06/2018 PATRICK  TRATAR COR0001R1 CONFORME COR0001E         *
BR0618*                     QUANDO CRURWW11-COD-ERRO NOT EQUAL '****'  *
BR0618*================================================================*
BR0818* ---------- -------- ------------------------------------------ *
BR0818* DATA       AUTOR    ALTERACAO
BR0818* ---------- -------- ------------------------------------------ *
BR0818* 20/08/2018 HAILTON  MOVE '***********' PARA CPROT-MSGEM-RETOR  *
BR0818*                     QUANDO CRURWW11-COD-ERRO NOT EQUAL '****'  *
BR0818*================================================================*
7C0120*================================================================*
.     *    DATA........: 01/2020                                       *
.     *    ANALISTA....: EDSON MATSUMOTO - 7COMM                       *
.     *    OBJETIVO....: ALTERAR A FORMA DE ENVIO DA CONTINUACAO, SO   *
.     *                  SERA ENVIADO DEPOIS DE RECEBER O RETORNO,     *
.     *                  POIS AS MSG ESTAO SENDO ENVIADOS FORA DE ORDEM*
7C0120*================================================================*
VANS  *    ALTERACAO : ATUALIZACAO DA SITUACAO DO CONTRATO COM 5 E DO  *
VANS  *                HISTORICO DA SITUACAO DO CONTRATO COM 5 OU 50,  *
VANS  *                ALEM DA RESERVA DE VALORES NA DOTACAO E DOS     *
VANS  *                CENTROS DE CUSTO FNEB E CFUR (LEGADO), QUE SERAO*
VANS  *                ATUALIZADOS SOMENTE QUANDO RETORNO OK DO BACEN  *
VANS  *                PARA A COR0002R1 - PROCESSO BATCH AUTOMATICO    *
VANS  *                NOVO E PROCESSO ONLINE JA EXISTENTE; TRATAMENTO *
VANS  *                DA COR0002E QUANDO MENSAGEM DE RETORNO DO BACEN *
VANS  *                FOR ECOR0001 OU ECOR0002 (CANCELAR CONTRATO NO  *
VANS  *                CRUR NESTES DOIS CASOS)                         *
VANS  *    ANALISTA  : VANESSA CHRISTINA SONETI                        *
VANS  *    DATA      : 08/09/2022                                      *
VANS  *    EMPRESA   : 7COMM                                           *
VANS  *================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
STE001*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  VICTORINO                                    *
.     *    DATA........:  MAIO / 2024                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.08                                   *
.     *    REMARKS.....:  STEFANINI                                    *
.     *    OBJETIVO....:  RECOMPILADO POR CAUSA DA ALTERACAO NO BOOK   *
.     *                   CRURWI21.                                    *
STE001*================================================================*
030724*----------------------------------------------------------------*
.     *                  A L T E R A C A O                             *
.     *----------------------------------------------------------------*
.     *    ANALISTA    : FABRICA       - WIPRO                         *
.     *    DATA........: 03/07/2024                                    *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....: ADEQUACAO AMBIENTE NVTI                       *
030724*================================================================*
STE002*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  VICTORINO                                    *
.     *    DATA........:  SETEMBRO / 2024                              *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.08                                   *
.     *    REMARKS.....:  STEFANINI                                    *
.     *    OBJETIVO....:  RECOMPILADO POR CAUSA DA ALTERACAO NO BOOK   *
.     *                   CRURWI21.                                    *
STE002*================================================================*

STE003*----------------------------------------------------------------*
  ||  *                  A L T E R A C A O                             *
  ||  *----------------------------------------------------------------*
  ||  *    ANALISTA    : VICTORINO     - STEFANINI                     *
  ||  *    DATA........: NOVEMBRO / 2024                               *
  ||  *----------------------------------------------------------------*
  ||  *    OBJETIVO....: PROMOTE PARA ATUALIZAR VERSAO DO BOOK CRURW08C*
STE003*================================================================*
RUSSO1*----------------------------------------------------------------*
.     *                  A L T E R A C A O                             *
.     *----------------------------------------------------------------*
.     *    ANALISTA    : RODRIGO KONSTANTINOVAS - BRADESCO             *
.     *    DATA........: 09/2025                                       *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....: TRATAR NOVAS MENSAGENS DE RETORNO DO ENVIO    *
.     *                  MASSIVO DA COR0003 E COR0003R1.               *
.     *                  RETIRADA DO SELECT MAX DA TABELA CRURB0WD PARA*
.     *                  CONSIDERAR O OBJETO SEQUENCE IMPLEMENTADO POR *
.     *                  MOTIVO DO ENVIO MASSIVO DA COR0003            *
RUSSO1*================================================================*

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
       77  FILLER                      PIC X(32)           VALUE
           '** INICIO DA WORKING CRUR1O11 **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)          VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05 WRK-PROGRAMA             PIC  X(008)         VALUE
              'CRUR1O11'.
           05 WRK-FRWK1999             PIC  X(008)         VALUE
              'FRWK1999'.
           05 WRK-CRUR408S             PIC X(008)  VALUE 'CRUR408S'.
           05 WRK-CRUR4S1I             PIC X(008)  VALUE 'CRUR4S1I'.
VANS       05 WRK-FNEB3BZL             PIC X(008)  VALUE 'FNEB3BZL'.
VANS       05 WRK-CRUR3XGO             PIC X(008)  VALUE 'CRUR3XGO'.
VANS       05 WRK-FNEB4O2O             PIC X(008)  VALUE 'FNEB4O2O'.
VANS       05 WRK-FNEB3YBA             PIC X(008)  VALUE 'FNEB3YBA'.
VANS       05 WRK-CFUR3P6M             PIC X(008)  VALUE 'CFUR3P6M'.
7C0120     05 WRK-RCOR7000             PIC X(008)  VALUE 'RCOR7000'.
COR6       05 WRK-CRUR3C6I             PIC X(008)  VALUE 'CRUR3C6I'.
COR6       05 WRK-CALE1000             PIC X(008)  VALUE 'CALE1000'.

COR6       05 WRK-NMNTRC-MSGEM         PIC 9(015)  VALUE ZEROS.
COR6       05 WRK-NLIN                 PIC 9(003)  VALUE ZEROS.
           05 WRK-TIMESTAMP            PIC X(026)  VALUE SPACES.
TESTE      05 WRK-SQLCODE              PIC +999999999 VALUE ZEROS.
EL2508*
.          05  WRK-DATE-ATUAL          PIC  X(010)     VALUE SPACES.
.          05  WRK-DATE-ATUAL-2        REDEFINES       WRK-DATE-ATUAL.
.              10  WRK-DIA-2           PIC  9(002).
.              10  FILLER              PIC  X(001).
.              10  WRK-MES-2           PIC  9(002).
.              10  FILLER              PIC  X(001).
.              10  WRK-ANO-2           PIC  9(004).
.     *
EL2508     05  WRK-DATA-ATUAL          PIC  9(008)     VALUE ZEROS.

7C0120     05 WRK-SEQ-MSG              PIC 9(003) COMP-3 VALUE ZEROS.
7C0120     05 WRK-COUNT                PIC S9(010) COMP-3 VALUE ZEROS.
EL1115     05 WRK-REENVIA              PIC X(001)      VALUE SPACES.
.            88 WRK-REENVIA-SIM                        VALUE 'S'.
EL1115       88 WRK-REENVIA-NAO                        VALUE 'N'.

           05 WRK-CODE100-B0UA         PIC  X(01)  VALUE 'N'.
      *
AS2108 01  WRK-S9-10                   PIC +9(010)     VALUE ZEROS.
.      01  FILLER                     REDEFINES WRK-S9-10.
.          05  FILLER                  PIC  X(001).
.          05  WRK-9-10                PIC  9(010).
.     *
.      01  WRK-S9-03                   PIC +9(003)     VALUE ZEROS.
.      01  FILLER                     REDEFINES WRK-S9-03.
.          05  FILLER                  PIC  X(001).
AS2108     05  WRK-9-03                PIC  9(003).

       01  WRK-B0UB-NULL.
           05 WRK-B0UB-CUS-MAN-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B0UB-HMA-REG-NULL    PIC S9(04) COMP VALUE +0.
      *
       01  WRK-B0UA-NULL.
           05 WRK-B0UA-CUS-MAN-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B0UA-HMA-REG-NULL    PIC S9(04) COMP VALUE +0.
      *
       01  WRK-B0U2-NULL.
           05 WRK-B0U2-CUS-MAN-NULL    PIC S9(04) COMP VALUE +0.
      *
VANS   01  WRK-B008-NULL.
VANS       05 WRK-B8-CMOTVO-SIT-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-NCONTR-FUMAG-NULL PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CREFT-BACEN-NULL  PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-PSPREAD-NULL      PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-DGERAC-DOCTO-NULL PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-PTX-EFETV-NULL    PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-DENVIO-REG-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-NLOTE-NULL        PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-QCOOP-LOTE-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-NCDULA-CREDT-NULL PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CCDULA-RURAL-NULL PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CEXCL-SIST-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CUSUAR-MANUT-NULL PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-HMANUT-REG-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-RMOTVO-SIT-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-DEXCL-CONTR-NULL  PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CPSSOA-NULL       PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CFICHA-CAD-NULL   PIC S9(04) COMP VALUE ZEROS.
VANS       05 WRK-B8-CINDCD-RENDA-PREVT-NULL
VANS                                   PIC S9(04) COMP VALUE ZEROS.
VANS  *
COR6  *----------------------------------------------------------------*
COR6   01  FILLER                      PIC X(032)          VALUE
COR6       '*** VARIAVEIS DE NULIDADE    ***'.
COR6  *----------------------------------------------------------------*
COR6
COR6   01  FILLER.
COR6       05 WRK-CPSSOA-CRUR-NULL     PIC S9(04) COMP VALUE +0.
COR6       05 WRK-CTPO-CRUR-NULL       PIC S9(04) COMP VALUE +0.
COR6       05 WRK-NSEQ-CRUR-NULL       PIC S9(04) COMP VALUE +0.
COR6       05 WRK-CPSSOA-FNEB-NULL     PIC S9(04) COMP VALUE +0.
COR6       05 WRK-CTPO-FNEB-NULL       PIC S9(04) COMP VALUE +0.
COR6       05 WRK-NSEQ-FNEB-NULL       PIC S9(04) COMP VALUE +0.
COR6  *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           'AREA DE COMUNICACAO DO PROGRAMA FRWK1999'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 0 TO 00526 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.

      *----------------------------------------------------------------*
       77  FILLER PIC X(34) VALUE  '** AREA DA API - CALE1000 **      '.
      *----------------------------------------------------------------*
           COPY 'I#CALE01'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY 'I#FRWKMD'.
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO CICS(API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-CICS.
           COPY 'I#FRWKCI'.
      *
TESTE *----------------------------------------------------------------*
TESTE  01  FILLER                      PIC X(32)           VALUE
TESTE      '* AREA PARA ERRO CICS / DB2    *'.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      COPY 'I#POOLB6'.
TESTE  01  WRK-TEXTO-ERRO              PIC X(79) VALUE SPACES.
TESTE  01  FILLER.
TESTE      05 WRK-CPSSOA-JURID-CONTR-L PIC +9(10) VALUE ZEROS.
TESTE      05 WRK-CTPO-CONTR-NEGOC-L   PIC +9(03) VALUE ZEROS.
TESTE      05 WRK-NSEQ-CONTR-NEGOC-L   PIC +9(10) VALUE ZEROS.
TESTE      05 WRK-NENVIO-INFO-RECOR-L  PIC +9(09) VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE INTERFACE COM SERVICO CRUR408S'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRUR408S.
           COPY CRURW00W.
           COPY CRURW08C.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE INTERFACE COM SERVICO CRUR4S1I'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CRUR4S1I.
           COPY CRURW00W.
           COPY CRURWS1C.
VANS  *----------------------------------------------------------------*
VANS   01  FILLER                      PIC X(050)      VALUE
VANS       'AREA DE INTERFACE COM SERVICO FNEB3BZL'.
VANS  *----------------------------------------------------------------*
VANS   01  WRK-AREA-FNEB3BZL.
VANS       COPY FNEBW000.
VANS       COPY FNEBWBZI.
VANS  *----------------------------------------------------------------*
VANS   01  FILLER                      PIC X(050)      VALUE
VANS       'AREA DE INTERFACE COM SERVICO CRUR3XGO'.
VANS  *----------------------------------------------------------------*
VANS   01  WRK-AREA-CRUR3XGO.
VANS       COPY CRURW00W.
VANS       COPY CRURWXGI.
VANS  *----------------------------------------------------------------*
VANS   01  FILLER                      PIC X(050)      VALUE
VANS       'AREA DE INTERFACE COM SERVICO FNEB4O2O'.
VANS  *----------------------------------------------------------------*
VANS   01  WRK-AREA-FNEB4O2O.
VANS       COPY FNEBW000.
VANS       COPY FNEBWI9C.
VANS  *----------------------------------------------------------------*
VANS   01  FILLER                      PIC X(050)      VALUE
VANS       'AREA DE INTERFACE COM SERVICO FNEB3YBA'.
VANS  *----------------------------------------------------------------*
VANS   01  WRK-AREA-FNEB3YBA.
VANS       COPY FNEBW000.
VANS       COPY FNEBWYBI.
VANS  *----------------------------------------------------------------*
VANS   01  FILLER                      PIC X(050)      VALUE
VANS       'AREA DE INTERFACE COM SERVICO CFUR3P6M'.
VANS  *----------------------------------------------------------------*
VANS   01  WRK-AREA-CFUR3P6M.
VANS       COPY CFURW00W.
VANS       COPY CFURWP6I.
COR6  *----------------------------------------------------------------*
COR6   01  FILLER                      PIC X(050)      VALUE
COR6       'AREA DE INTERFACE COM SERVICO CRUR3C6I'.
COR6  *----------------------------------------------------------------*
COR6   01  WRK-AREA-CRUR3C6I.
COR6       COPY CRURW00W.
COR6       COPY CRURWE12.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
 .     01  FILLER                      PIC X(050)      VALUE
 .         'AREA DE INTERFACE COM SERVICO FUNCIONAL ESVB1061'.
 .    *----------------------------------------------------------------*
       01  WRK-AREA-ESVB1061.
 .         COPY 'I#ESVB61'.
      *
LL1705*--> AREA DE ACESSO A FUNCAO ESVB
 .    *
 .     01  WRK-ESVB1061                PIC  X(08)      VALUE 'ESVB1061'.
 .     01  WRK-ESVB1061-PARMS.
 .         05 WRK-AMBIENTE             PIC X(008)      VALUE SPACES.
 .         05 WRK-AREA-PARAMETRO.
 .            10 WRK-CPSSOA            PIC  9(10)      VALUE ZEROS.
 .            10 WRK-CPSSOA-JURID-CONTR
 .                                     PIC  9(10)      VALUE ZEROS.
 .            10 WRK-CTPO-CONTR-NEGOC  PIC  9(03)      VALUE ZEROS.
              10 WRK-NSEQ-CONTR-NEGOC  PIC  9(10)      VALUE ZEROS.
 .            10 FILLER                PIC  X(267)     VALUE SPACES.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0001E          ***'.
      *---------------------------------------------------------------*

       COPY CRURWI21.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0001R1         ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-CRURWW11.
           COPY CRURWW11.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0002R1         ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-CRURWW21.
           COPY CRURWW21.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM COR0002E          ***'.
      *---------------------------------------------------------------*

       01  WRK-AREA-CRURWI41.
           COPY CRURWI41.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM GEN0004           ***'.
      *---------------------------------------------------------------*
       COPY 'I#RCOR05'.

       01  FILLER                      PIC X(079)          VALUE
           '*** LAYOUT DA MENSAGEM TMNF              ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR03'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO DB2  (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-DB2.
           COPY 'I#FRWKDB'.
      *
7C0120*---------------------------------------------------------------*
.     *    '*** AREA PARA CHAMADA MODULO RCOR7000 ***'.
.     *---------------------------------------------------------------*
.     *
.      COPY 'I#RCOR10'.
7C0120*
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE TRATAMENTO DE ERRO LIVRE'.
      *----------------------------------------------------------------*
       01 WRK-AREA-ERRO-LIVRE.
          COPY 'I#FRWKLI'.

      *                                                                *
      *----------------------------------------------------------------*
      *    '*** AREA PARA DECLARADAO DE DCLGENS ***'.
      *----------------------------------------------------------------*
      *                                                                *
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *                                                                *
      *    TABELA TCTRL_ENVIO_INFO                                     *
      *                                                                *
           EXEC SQL
               INCLUDE CRURB0U2
           END-EXEC.
      *                                                                *
      *    TABELA TLIN_MSGEM_REMSS                                     *
      *                                                                *
           EXEC SQL
               INCLUDE CRURB0UA
           END-EXEC.
      *                                                                *
      *                                                                *
      *    TABELA TLIN_MSGEM_RETOR                                     *
      *                                                                *
           EXEC SQL
               INCLUDE CRURB0UB
           END-EXEC.
      *                                                                *
RC    *    TABELA TCONTR_CREDT_RURAL                                   *
      *                                                                *
RC         EXEC SQL
RC             INCLUDE CRURB008
RC         END-EXEC.
      *
      *
VANS       EXEC SQL
VANS           INCLUDE CRURB0LJ
VANS       END-EXEC.
VANS  *
VANS  *
           EXEC SQL
               INCLUDE CRURB0MS
           END-EXEC.
      *                                                                *
COR6  *----------------------------------------------------------------*
COR6   01  FILLER                      PIC X(032)      VALUE
COR6       '*** INCLUDE CRURB0WD         ***'.
COR6  *----------------------------------------------------------------*
COR6  *                                                                *
COR6       EXEC SQL
COR6           INCLUDE CRURB0WD
COR6       END-EXEC.
COR6  *                                                                *
      *                                                                *
           EXEC SQL
               DECLARE CSR01-CRURB0UA CURSOR FOR
               SELECT  CPSSOA_JURID_CONTR,
                       CTPO_CONTR_NEGOC,
                       NSEQ_CONTR_NEGOC,
                       NENVIO_INFO_RECOR,
                       NLIN_MSGEM_REMSS
                  FROM DB2PRD.TLIN_MSGEM_REMSS
              WHERE CPSSOA_JURID_CONTR  =  :CRURB0UA.CPSSOA-JURID-CONTR
                AND CTPO_CONTR_NEGOC    =  :CRURB0UA.CTPO-CONTR-NEGOC
                AND NSEQ_CONTR_NEGOC    =  :CRURB0UA.NSEQ-CONTR-NEGOC
                AND NENVIO_INFO_RECOR   =  :CRURB0UA.NENVIO-INFO-RECOR
                AND CTRNSM_INFO_BACEN   =  :CRURB0UA.CTRNSM-INFO-BACEN
                AND NLIN_MSGEM_REMSS    >  :CRURB0UA.NLIN-MSGEM-REMSS
              ORDER BY NLIN_MSGEM_REMSS ASC
           END-EXEC.
      *
           EXEC SQL
             DECLARE CSR01-CRURB0MS CURSOR WITH HOLD FOR
              SELECT CPSSOA
                FROM DB2PRD.TPRTCP_PSSOA_RURAL
               WHERE CPSSOA_JURID_CONTR = :CRURB0MS.CPSSOA-JURID-CONTR
                 AND CTPO_CONTR_NEGOC   = :CRURB0MS.CTPO-CONTR-NEGOC
                 AND NSEQ_CONTR_NEGOC   = :CRURB0MS.NSEQ-CONTR-NEGOC
                 AND CTPO_PRTCP_PSSOA   = 20
            FETCH FIRST 1 ROWS ONLY
           END-EXEC.
      *
ID6854
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*** FIM DA WORKING CRUR1O11 ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01 DFHCOMMAREA.
          COPY CRURWE11.

      *================================================================*
       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR CONTER AS PRINCIPAIS SECTIONS.          *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
      ***  LOG APENAS PARA CONTROLE
           STRING 'PROGRAMA CRUR1O11 ACIONADO'
                             DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
           SET ERRO-LIVRE              TO TRUE
           MOVE '0000-INICIAR'         TO FRWKGHEA-IDEN-PARAGRAFO
           MOVE 'CRUR1421'             TO FRWKGLIV-COD-MENSAGEM

      ***  PROGRAMA SEGUIRA APOS ESTE ACIONAMENTO DE 9500-ERRO-LIVRE
           PERFORM 9500-ERRO-LIVRE
           MOVE SPACES                 TO FRWKGLIV-COD-MENSAGEM
      ***
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA INICIALIZACAO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FRWKGERR-REGISTRO
                      FRWKGHEA-REGISTRO
                      FRWKGDB2-REGISTRO
                      FRWKGLIV-REGISTRO
                      CRURWE11-SAIDA

COR6       PERFORM 1050-OBTER-DATA-SISTEMA

           PERFORM 1100-CONSISTIR-ENTRADA
EL2508*
.     *    RECEBIMENTO DA DATA CORRENTE
.     *
.          EXEC SQL
.               SET :WRK-DATE-ATUAL = CURRENT DATE
.          END-EXEC
.     *
.          MOVE WRK-ANO-2              TO WRK-DATA-ATUAL(1:4)
.          MOVE WRK-MES-2              TO WRK-DATA-ATUAL(5:2)
EL2508     MOVE WRK-DIA-2              TO WRK-DATA-ATUAL(7:2)
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1050-OBTER-DATA-SISTEMA         SECTION.
      *----------------------------------------------------------------*
      *
           MOVE '1050-OBTER-DATA-SISTEMA'
                                          TO FRWKGHEA-IDEN-PARAGRAFO.
      *
           INITIALIZE                     CALE01-ENTRADA.
      *
           MOVE 'F3'                      TO  CALE01-FUNCAO
           MOVE 'SF3002'                  TO  CALE01-SUB-FUNCAO
      *
           PERFORM 4500-ACESSAR-CALE1000.

           MOVE CALE01-HH-TIMESTAMP-DB2   TO WRK-TIMESTAMP.

      *
      *----------------------------------------------------------------*
       1050-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA CONSISTENCIA DOS DADOS DE ENTRADA.     *
      *----------------------------------------------------------------*
       1100-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*
      *
           IF  CRURWE11-COD-MSG      EQUAL SPACES
               STRING  'CODIGO DA MENSAGEM NAO INFORMADO'
                       DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               SET     ERRO-LIVRE  TO    TRUE
                MOVE '1100-CONSISTIR-ENTRADA'
                                   TO    FRWKGHEA-IDEN-PARAGRAFO
               MOVE    'CRUR1108'  TO    FRWKGLIV-COD-MENSAGEM
               MOVE 02             TO    CRURWE11-COD-RETORNO
               PERFORM 9500-ERRO-LIVRE
           END-IF.

           IF  CRURWE11-SISMSG     EQUAL SPACES
               STRING  'MENSAGEM NAO INFORMADO'
                       DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               SET     ERRO-LIVRE  TO    TRUE
                MOVE '1100-CONSISTIR-ENTRADA'
                                   TO    FRWKGHEA-IDEN-PARAGRAFO
               MOVE    'CRUR1108'  TO    FRWKGLIV-COD-MENSAGEM
               MOVE 02             TO    CRURWE11-COD-RETORNO
               PERFORM 9500-ERRO-LIVRE
           END-IF.

      ***  LOG APENAS PARA CONTROLE
           STRING 'CRURWE11-COD-MSG: ' CRURWE11-ENTRADA
                             DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
           SET ERRO-LIVRE              TO TRUE
           MOVE '1100-CONSISTIR-ENTRADA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
           MOVE 'CRUR1421'             TO FRWKGLIV-COD-MENSAGEM

      ***  PROGRAMA SEGUIRA APOS ESTE ACIONAMENTO DE 9500-ERRO-LIVRE
           PERFORM 9500-ERRO-LIVRE
           MOVE SPACES                 TO FRWKGLIV-COD-MENSAGEM
      ***
           EVALUATE  CRURWE11-COD-MSG
               WHEN 'COR0001R1'
               WHEN 'COR0001E '
               WHEN 'COR0002R1'
               WHEN 'COR0002E '
COR6           WHEN 'COR0006R1'
COR6           WHEN 'COR0006E '
RUSSO1         WHEN 'COR0003R1'
RUSSO1         WHEN 'COR0003E '
               WHEN 'GEN0004  '
               WHEN 'TMNF     '
                    MOVE ZEROS          TO CRURWE11-COD-RETORNO
               WHEN OTHER
                    STRING  'CODIGO DA MENSAGEM INVALIDO'
                            DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
                    SET     ERRO-LIVRE  TO    TRUE
                    MOVE '1100-CONSISTIR-ENTRADA'
                                        TO    FRWKGHEA-IDEN-PARAGRAFO
                    MOVE    'CRUR1108'  TO    FRWKGLIV-COD-MENSAGEM
                    MOVE 02             TO    CRURWE11-COD-RETORNO
                    PERFORM 9500-ERRO-LIVRE
           END-EVALUATE
           .
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO PROCESSAMENTO DO PROGRAMA.             *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2000.WE11-COD-MSG: ' CRURWE11-COD-MSG
TESTE             ' NRO-IF: ' CRURWE11-NRO-CTRL-IF
TESTE             ' SEQ: ' CRURWE11-SEQ-MSG
TESTE             ' TS: ' WRK-TIMESTAMP
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
           IF  CRURWE11-COD-MSG  EQUAL  'GEN0004  '
               MOVE CRURWE11-SISMSG(1: LENGTH OF RCOR05-REGISTRO)
                                       TO RCOR05-REGISTRO
               MOVE  RCOR05-NRO-OPE-ORIG
                                       TO CTRNSM-INFO-BACEN OF CRURB0UA
               PERFORM 2200-SELECT-CRURB0UA
           ELSE
               MOVE CRURWE11-NRO-CTRL-IF TO
                                        CRQUIS-FINCR-BACEN  OF CRURB0U2
               PERFORM 2100-SELECT-CRURB0U2
           END-IF.

           PERFORM 2300-VERIFICA-RETORNOS
      *
           .
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR IF TABELA TCTRL_ENVIO_INFO            *
      *----------------------------------------------------------------*
       2100-SELECT-CRURB0U2            SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             SELECT CPSSOA_JURID_CONTR,
                    CTPO_CONTR_NEGOC,
                    NSEQ_CONTR_NEGOC,
                    NENVIO_INFO_RECOR,
VANS                CSIT_MSGEM_BACEN,
VANS                CUSUAR_INCL
               INTO :CRURB0U2.CPSSOA-JURID-CONTR,
                    :CRURB0U2.CTPO-CONTR-NEGOC,
                    :CRURB0U2.NSEQ-CONTR-NEGOC,
                    :CRURB0U2.NENVIO-INFO-RECOR,
VANS                :CRURB0U2.CSIT-MSGEM-BACEN,
VANS                :CRURB0U2.CUSUAR-INCL
               FROM DB2PRD.TCTRL_ENVIO_INFO
             WHERE CRQUIS_FINCR_BACEN =  :CRURB0U2.CRQUIS-FINCR-BACEN
ID6260       ORDER BY NENVIO_INFO_RECOR DESC
ID6260       FETCH FIRST 1 ROWS ONLY
           END-EXEC
      *
           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TCTRL_ENVIO_INFO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2100-SELECT-CRURB0U2'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0010'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF

TESTE      MOVE SQLCODE TO WRK-SQLCODE
TESTE      STRING '2100.CRQUIS: ' CRQUIS-FINCR-BACEN OF CRURB0U2
TESTE             ' SQLCODE: ' WRK-SQLCODE
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
           IF  CRURWE11-COD-MSG         EQUAL 'COR0006R1'
RUSSO1                                     OR 'COR0003R1'
               GO TO 2100-99-FIM
           END-IF.

           IF  SQLCODE                  EQUAL +100
                MOVE ZEROS          TO    CRURWE11-COD-RETORNO
                PERFORM 3000-FINALIZAR
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
ID6854*----------------------------------------------------------------*
ID6854*    ROTINA PARA VERIFICAR SE EXISTE MAIS REGISTROS PARA ENVIO   *
ID6854*----------------------------------------------------------------*
ID6854 2130-VERIFICA-CONT              SECTION.
ID6854*----------------------------------------------------------------*
ID6854*
ID6854     PERFORM 2140-COUNT-CRURB0U2
ID6854*
ID6854     PERFORM 2150-SELECT-CRURB0U2-CONT
ID6854     .
ID6854*
ID6854*----------------------------------------------------------------*
ID6854 2130-99-FIM.                    EXIT.
ID6854*----------------------------------------------------------------*
7C0120*----------------------------------------------------------------*
.     *    ROTINA PARA VERIFICAR QUANTOS REGISTROS TEM PARA ENVIAR     *
.     *----------------------------------------------------------------*
.      2140-COUNT-CRURB0U2            SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE ZEROS      TO WRK-COUNT
.          MOVE 5          TO CSIT-MSGEM-BACEN  OF CRURB0U2
.          MOVE 'COR0001'  TO CMSGEM-SIST-BACEN OF CRURB0U2
ID6854     MOVE SPACES     TO CTRNSM-INFO-BACEN OF CRURB0UA
.     *
.          EXEC SQL
.            SELECT COUNT(*)
.              INTO :WRK-COUNT
ID6854         FROM DB2PRD.TCTRL_ENVIO_INFO A
ID6854         JOIN DB2PRD.TLIN_MSGEM_REMSS B
ID6854           ON A.CPSSOA_JURID_CONTR = B.CPSSOA_JURID_CONTR
ID6854          AND A.CTPO_CONTR_NEGOC   = B.CTPO_CONTR_NEGOC
ID6854          AND A.NSEQ_CONTR_NEGOC   = B.NSEQ_CONTR_NEGOC
ID6854          AND A.NENVIO_INFO_RECOR  = B.NENVIO_INFO_RECOR
ID6854        WHERE A.CRQUIS_FINCR_BACEN = :CRURB0U2.CRQUIS-FINCR-BACEN
ID6854          AND A.CSIT_MSGEM_BACEN   = :CRURB0U2.CSIT-MSGEM-BACEN
ID6854          AND A.CMSGEM_SIST_BACEN  = :CRURB0U2.CMSGEM-SIST-BACEN
ID6854          AND B.CTRNSM_INFO_BACEN  = :CRURB0UA.CTRNSM-INFO-BACEN
.          END-EXEC
.     *
.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
.             (SQLWARN0                 EQUAL 'W')
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'TCTRL_ENVIO_INFO'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '2140-COUNT-CRURB0U2'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '0005'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF
TESTE      MOVE WRK-COUNT TO WRK-SQLCODE
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2140.CRQUIS-FINCR-BACEN: ' CRQUIS-FINCR-BACEN
TESTE                                         OF CRURB0U2
TESTE            ' COUNT: ' WRK-SQLCODE
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
.          .
.     *
.     *----------------------------------------------------------------*
.      2140-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*

7C0120*----------------------------------------------------------------*
.     *    ROTINA PARA VERIFICAR SE TEM REGISTROS DE CONTINUACAO       *
.     *    A SER ENVIADO                                               *
ID6854*----------------------------------------------------------------*
ID6854 2150-SELECT-CRURB0U2-CONT       SECTION.
ID6854*----------------------------------------------------------------*
ID6854*
ID6854     MOVE SPACES                 TO CTRNSM-INFO-BACEN OF CRURB0UA
ID6854*
ID6854     EXEC SQL
ID6854       SELECT A.CPSSOA_JURID_CONTR,
ID6854              A.CTPO_CONTR_NEGOC,
ID6854              A.NSEQ_CONTR_NEGOC,
ID6854              A.NENVIO_INFO_RECOR,
ID6854              A.CSIT_MSGEM_BACEN
ID6854        INTO :CRURB0U2.CPSSOA-JURID-CONTR,
ID6854             :CRURB0U2.CTPO-CONTR-NEGOC,
ID6854             :CRURB0U2.NSEQ-CONTR-NEGOC,
ID6854             :CRURB0U2.NENVIO-INFO-RECOR,
ID6854             :CRURB0U2.CSIT-MSGEM-BACEN
ID6854         FROM DB2PRD.TCTRL_ENVIO_INFO A
ID6854         JOIN DB2PRD.TLIN_MSGEM_REMSS B
ID6854           ON A.CPSSOA_JURID_CONTR = B.CPSSOA_JURID_CONTR
ID6854          AND A.CTPO_CONTR_NEGOC   = B.CTPO_CONTR_NEGOC
ID6854          AND A.NSEQ_CONTR_NEGOC   = B.NSEQ_CONTR_NEGOC
ID6854          AND A.NENVIO_INFO_RECOR  = B.NENVIO_INFO_RECOR
ID6854        WHERE A.CRQUIS_FINCR_BACEN = :CRURB0U2.CRQUIS-FINCR-BACEN
ID6854          AND A.CSIT_MSGEM_BACEN   = :CRURB0U2.CSIT-MSGEM-BACEN
ID6854          AND A.CMSGEM_SIST_BACEN  = :CRURB0U2.CMSGEM-SIST-BACEN
ID6854          AND B.CTRNSM_INFO_BACEN  = :CRURB0UA.CTRNSM-INFO-BACEN
ID6854       ORDER BY NENVIO_INFO_RECOR ASC
ID6854       FETCH FIRST 1 ROWS ONLY
ID6854     END-EXEC.

.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                 EQUAL 'W')
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'TCTRL_ENVIO_INFO'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '2150-SELECT-CRURB0U2-CONT'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '0005'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF
.     *
.          IF  SQLCODE                  EQUAL +100
.               GO TO 2150-99-FIM
.          END-IF
.          .
.     *
.     *----------------------------------------------------------------*
.      2150-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*
.     *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR OBTER DADOS NA TABELA CRURBOU2          *
      *----------------------------------------------------------------*
      *2120-OBTER-DADOS-CRURB0U2       SECTION.
      *----------------------------------------------------------------*
      *
      *    E*XEC SQL
      *      S*ELECT CSIT_MSGEM_BACEN,
      *             HMANUT_REG
      *        I*NTO :CRURB0U2.CSIT-MSGEM-BACEN,
      *             :CRURB0U2.HMANUT-REG
      *        F*ROM DB2PRD.TCTRL_ENVIO_INFO
      *      W*HERE CPSSOA_JURID_CONTR  =  :CRURB0U2.CPSSOA-JURID-CONTR
      *        A*ND CTPO_CONTR_NEGOC    =  :CRURB0U2.CTPO-CONTR-NEGOC
      *        A*ND NSEQ_CONTR_NEGOC    =  :CRURB0U2.NSEQ-CONTR-NEGOC
      *        A*ND NENVIO_INFO_RECOR   =  :CRURB0U2.NENVIO-INFO-RECOR
      *    E*ND-EXEC
      *
      *    I*F (SQLCODE                 NOT EQUAL ZEROS) OR
      *       (SQLWARN0                EQUAL 'W')
      *        S*ET  DB2-SELECT         TO TRUE
      *        M*OVE 'TCTRL_ENVIO_INFO'
      *                                TO FRWKGDB2-NOME-TABELA
      *        M*OVE '2120-OBTER-DADOS-CRURB0U2'
      *                                TO FRWKGHEA-IDEN-PARAGRAFO
      *        M*OVE '0040'             TO FRWKGDB2-LOCAL
      *        P*ERFORM 9000-ERRO-DB2
      *    E*ND-IF
      *
      *     .
      *
      *----------------------------------------------------------------*
      *2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
7C0120*----------------------------------------------------------------*
.     *    ROTINA PARA VERIFICAR TRANSMISSAO NA TABELA TLIN_MSGEM_REMSS*
.     *----------------------------------------------------------------*
.      2160-SELECT-CRURB0UA-CONT       SECTION.
.     *----------------------------------------------------------------*
.     *
MILTON     MOVE NENVIO-INFO-RECOR      OF CRURB0U2
MILTON                                 TO NENVIO-INFO-RECOR OF CRURB0UA
TESTE      MOVE NENVIO-INFO-RECOR OF CRURB0UA TO WRK-SQLCODE
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2160.NENVIO-INFO: ' WRK-SQLCODE
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
.          EXEC SQL
.            SELECT NLIN_MSGEM_REMSS
.                  ,RLIN_MSGEM_ENVID
.                  ,CTRNSM_INFO_BACEN
.              INTO :CRURB0UA.NLIN-MSGEM-REMSS,
.                   :CRURB0UA.RLIN-MSGEM-ENVID,
.                   :CRURB0UA.CTRNSM-INFO-BACEN
.              FROM DB2PRD.TLIN_MSGEM_REMSS
.              WHERE CPSSOA_JURID_CONTR = :CRURB0UA.CPSSOA-JURID-CONTR
.                AND CTPO_CONTR_NEGOC   = :CRURB0UA.CTPO-CONTR-NEGOC
.                AND NSEQ_CONTR_NEGOC   = :CRURB0UA.NSEQ-CONTR-NEGOC
.                AND NENVIO_INFO_RECOR  > :CRURB0UA.NENVIO-INFO-RECOR
              ORDER BY NENVIO_INFO_RECOR ASC
.             FETCH FIRST 1 ROW ONLY
.          END-EXEC
.     *
.          IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                 EQUAL 'W')
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'TLIN_MSGEM_REMSS'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '2160-SELECT-CRURB0UA-CONT'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '0021'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF
.
.          IF  SQLCODE                  EQUAL +100
.               GO TO 2160-99-FIM
.          ELSE
                PERFORM 2170-ENVIO-RCOR7000
.          END-IF
.          .
.     *
.     *----------------------------------------------------------------*
.      2160-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*
      *
7C0120*---------------------------------------------------------------*
.      2170-ENVIO-RCOR7000         SECTION.
.     *---------------------------------------------------------------*
.     *
.          INITIALIZE  RCOR10-REGISTRO.
.     *
.          MOVE 'CRUR'                 TO RCOR10-CCUSTO.
.
           MOVE CRURWE11-NRO-CTRL-IF   TO RCOR10-NRO-CTRLIF

           MOVE CRURWE11-SEQ-MSG       TO WRK-SEQ-MSG
           ADD  1                      TO WRK-SEQ-MSG
           MOVE WRK-SEQ-MSG            TO RCOR10-SEQ-MSG

           IF WRK-COUNT GREATER 2
.              MOVE 'S'                TO RCOR10-IND-CONTINUA
.          ELSE
.              MOVE 'N'                TO RCOR10-IND-CONTINUA
.          END-IF.
.     *
.     *
.          MOVE RLIN-MSGEM-ENVID-LEN   TO RCOR10-TAM-SISMSG
TESTE                                     WRK-SQLCODE
.     *
.          MOVE RLIN-MSGEM-ENVID-TEXT
.                                      TO RCOR10-SISMSG.
.     *
.          EXEC CICS LINK
.               PROGRAM  (WRK-RCOR7000)
.               COMMAREA (RCOR10-REGISTRO)
.               LENGTH   (LENGTH OF RCOR10-REGISTRO)
.               NOHANDLE
.          END-EXEC.
.     *
.          IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
.              MOVE '2170-ENVIO-RCOR7000'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
.     *
.          IF (RCOR10-COD-RETORNO NOT EQUAL ZEROS)
.              MOVE '2360'             TO CRURW00W-COD-ERRO
.                                      OF WRK-AREA-CRUR408S
.              MOVE '2170-ENVIO-RCOR7000'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9400-ERRO-CICS
.          END-IF
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2170.SEQ-MSG: ' RCOR10-SEQ-MSG
TESTE             'ID-CNT: 'RCOR10-IND-CONTINUA
TESTE             ' CD-RET: ' RCOR10-COD-RETORNO
TESTE             ' TAM: ' WRK-SQLCODE
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
COR6*****      REGISTRA GLOG PARA ANALISE, MAS O PROCESSAMENTO NAO
COR6*****      EH INTERROMPIDO (ERRO NAO EH GRAVE)
COR6           STRING 'RCOR10-REGISTRO:'
COR6                  RCOR10-REGISTRO
COR6                  DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
COR6           MOVE '2170-ENVIO-RCOR7000'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6           PERFORM 9200-ERRO-LIVRE
COR6*****      CONTINUA
.     *
.          .
.     *
.     *----------------------------------------------------------------*
.      2170-99-FIM.                    EXIT.
7C0120*----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR TRANSMISSAO NA TABELA TLIN_MSGEM_REMSS*
      *----------------------------------------------------------------*
       2200-SELECT-CRURB0UA            SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
             SELECT CPSSOA_JURID_CONTR,
                    CTPO_CONTR_NEGOC,
                    NSEQ_CONTR_NEGOC,
                    NENVIO_INFO_RECOR
               INTO :CRURB0UA.CPSSOA-JURID-CONTR,
                    :CRURB0UA.CTPO-CONTR-NEGOC,
                    :CRURB0UA.NSEQ-CONTR-NEGOC,
                    :CRURB0UA.NENVIO-INFO-RECOR
               FROM DB2PRD.TLIN_MSGEM_REMSS
             WHERE CTRNSM_INFO_BACEN  =  :CRURB0UA.CTRNSM-INFO-BACEN
              FETCH FIRST 1 ROW ONLY
           END-EXEC
      *
           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TCTRL_ENVIO_INFO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2200-SELECT-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0020'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF

           IF  SQLCODE                  EQUAL +100
                MOVE ZEROS          TO    CRURWE11-COD-RETORNO
                PERFORM 3000-FINALIZAR
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR IDENTIFICAR RETORNOS                    *
      *---------------------------------------------------------------*
       2300-VERIFICA-RETORNOS       SECTION.
      *---------------------------------------------------------------*
      *
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2300.CRURWE11-COD-MSG: ' CRURWE11-COD-MSG
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
            EVALUATE  CRURWE11-COD-MSG
                WHEN 'COR0001R1'
BR0618             MOVE CRURWE11-SISMSG   TO CRURWW11-REGISTRO
BR0618
BR0618             IF  CRURWW11-COD-ERRO NOT EQUAL '********'
BR0618                 PERFORM 2320-TRATA-COR0001E
BR0618             ELSE
                       PERFORM 2310-TRATA-COR0001R1
                       PERFORM 2995-ACESSAR-ESVB1061
BR0618             END-IF
                WHEN 'COR0001E '
                   PERFORM 2320-TRATA-COR0001E
                WHEN 'COR0002R1'
                   PERFORM 2330-TRATA-COR0002R1
                WHEN 'COR0002E '
                   PERFORM 2340-TRATA-COR0002E
COR6            WHEN 'COR0006R1'
COR6               PERFORM 2370-TRATA-COR0006R1
COR6            WHEN 'COR0006E '
COR6               PERFORM 2380-TRATA-COR0006E
RUSSO1          WHEN 'COR0003R1'
RUSSO1             PERFORM 2390-TRATA-COR0003R1-E
RUSSO1          WHEN 'COR0003E '
RUSSO1             PERFORM 2390-TRATA-COR0003R1-E
                WHEN 'GEN0004  '
                   PERFORM 2350-TRATA-GEN0004
                WHEN 'TMNF     '
                   PERFORM 2360-TRATA-TMNF
           END-EVALUATE.

      *---------------------------------------------------------------*
       2300-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO TRATAMENTO COR0001R1                   *
      *----------------------------------------------------------------*
       2310-TRATA-COR0001R1            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 4000-SELECIONAR-MAX-RETOR

           MOVE 'RCOR8005' TO CUSUAR-INCL        OF CRURB0UB
           MOVE CRURWW11-NRO-REF-BCCOR
                           TO CPROT-MSGEM-RETOR  OF CRURB0UB
           PERFORM 4100-INCLUIR-CRURB0UB
      *    **** RETORNO OK INCLUSAO *
           MOVE 9                 TO CSIT-MSGEM-BACEN  OF CRURB0U2
           MOVE 'RCOR8005'        TO CUSUAR-MANUT      OF CRURB0U2
           MOVE CRURWW11-COD-MSG  TO CMSGEM-SIST-BACEN OF CRURB0U2
           PERFORM 4200-UPDATE-CRURB0U2
      *


RC           PERFORM 4300-UPDATE-CRURB008
      *
AS2108       PERFORM 2311-ACESSAR-CRUR408S
.     *
AS2108       PERFORM 2313-ACESSAR-CRUR4S1I
           .
      *
      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ACESSO AO MODULO CRUR408S                                      *
      *----------------------------------------------------------------*
       2311-ACESSAR-CRUR408S       SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2312-MOVER-DADOS-CRUR408S
      *
           EXEC CICS LINK
                PROGRAM  (WRK-CRUR408S)
                COMMAREA (WRK-AREA-CRUR408S)
                LENGTH         (LENGTH OF WRK-AREA-CRUR408S)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0080'             TO CRURW00W-COD-ERRO
                                       OF WRK-AREA-CRUR408S
               MOVE '2300-ACESSAR-CRUR408S'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR408S
             WHEN 00
                  CONTINUE
             WHEN OTHER
      *
                  MOVE WRK-CRUR408S    TO FRWKGMOD-NOME-MODULO
      *
                  MOVE CRURW00W-BLOCO-RETORNO
                                       OF WRK-AREA-CRUR408S
                                       TO FRWKGMOD-BLOCO-RETORNO
      *
                  MOVE '2300-ACESSAR-CRUR408S'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                  PERFORM 9300-ERRO-MODULO
           END-EVALUATE
      *
           .
      *
      *----------------------------------------------------------------*
       2311-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * MOVIMENTO DOS DADOS PARA O BOOK DE ENTRADA DO BASICO           *
      *----------------------------------------------------------------*
       2312-MOVER-DADOS-CRUR408S       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURW00W-REGISTRO
                                       OF WRK-AREA-CRUR408S
                                       CRURW08C-REGISTRO
                                       OF WRK-AREA-CRUR408S
      *
           MOVE CPSSOA-JURID-CONTR     OF CRURB0U2
                                       TO WRK-S9-10
           MOVE WRK-9-10               TO CRURW08C-E-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR408S
      *
           MOVE CTPO-CONTR-NEGOC       OF CRURB0U2
                                       TO WRK-S9-03
           MOVE WRK-9-03               TO CRURW08C-E-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
      *
           MOVE NSEQ-CONTR-NEGOC       OF CRURB0U2
                                       TO WRK-S9-10
           MOVE WRK-9-10               TO CRURW08C-E-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
           .
      *----------------------------------------------------------------*
       2312-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ACESSO AO MODULO CRUR4S1I                                      *
      *----------------------------------------------------------------*
       2313-ACESSAR-CRUR4S1I       SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2314-MOVER-DADOS-CRUR4S1I
      *
           EXEC CICS LINK
                PROGRAM  (WRK-CRUR4S1I)
                COMMAREA (WRK-AREA-CRUR4S1I)
                LENGTH         (LENGTH OF WRK-AREA-CRUR4S1I)
                NOHANDLE
           END-EXEC
      *
           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE '0110'             TO CRURW00W-COD-ERRO
                                       OF WRK-AREA-CRUR408S
               MOVE '2500-ACESSAR-CRUR4S1I'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9400-ERRO-CICS
           END-IF
      *
           EVALUATE CRURW00W-COD-RETORNO
                                       OF WRK-AREA-CRUR4S1I
             WHEN 00
                 CONTINUE
             WHEN OTHER
      *
                 MOVE WRK-CRUR4S1I     TO FRWKGMOD-NOME-MODULO
      *
                 MOVE CRURW00W-BLOCO-RETORNO
                                       OF WRK-AREA-CRUR4S1I
                                       TO FRWKGMOD-BLOCO-RETORNO
      *
                 MOVE '2500-ACESSAR-CRUR4S1I'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
                 PERFORM 9300-ERRO-MODULO
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2313-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * MOVIMENTO DOS DADOS PARA O BOOK DE ENTRADA DO BASICO           *
      *----------------------------------------------------------------*
       2314-MOVER-DADOS-CRUR4S1I       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURW00W-REGISTRO
                                       OF WRK-AREA-CRUR4S1I
                                       CRURWS1C-REGISTRO
                                       OF WRK-AREA-CRUR4S1I
      *
           MOVE CRURW08C-S-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-NSEQ-CONTR-NEGOC
                                       OF  WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR4S1I
      *
           EXEC SQL
                 SET :WRK-TIMESTAMP = CURRENT TIMESTAMP
           END-EXEC
      *
           MOVE WRK-TIMESTAMP          TO CRURWS1C-E-HINCL-REG-HIST
      *
           MOVE CRURW08C-S-CPERDC      OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CPERDC
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CORIGE-REC-CREDT
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CORIGE-REC-CREDT
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CMOTVO-SIT-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CMOTVO-SIT-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CSIT-OPER-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CSIT-OPER-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CIDIOM      OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CIDIOM
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-NCONTR-FUMAG-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-NCONTR-FUMAG-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-VCONTR-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-VCONTR-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CREFT-BACEN-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CREFT-BACEN-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CSCRTZ-CONTR-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CSCRTZ-CONTR-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CCONVE-CONTR-CREDT
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CCONVE-CONTR-CREDT
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CUSO-LIM-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CUSO-LIM-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-PSPREAD-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-PSPREAD-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DEFTVC-CONTR-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DEFTVC-CONTR-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DGERAC-DOCTO-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DGERAC-DOCTO-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DVLR-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DVLR-CONTR-NEGOC
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-PTX-EFETV-JURO
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-PTX-EFETV-JURO
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-VCTBIL-CONTR-CREDT
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-VCTBIL-CONTR-CREDT
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DENVIO-REG-BACEN
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DENVIO-REG-BACEN
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CSIT-REG-BACEN
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CSIT-REG-BACEN
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-NLOTE-CONTR-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-NLOTE-CONTR-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-QCOOP-LOTE-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-QCOOP-LOTE-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DLIM-APROV-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DLIM-APROV-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CGRAU-SVRDE-INCON
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CGRAU-SVRDE-INCON
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-NCDULA-CREDT-RURAL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-NCDULA-CREDT-RURAL
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CCDULA-RURAL-BACEN
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CCDULA-RURAL-BACEN
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CMOTVO-EXCL-BACEN
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CEXCL-SIST-BACEN
                                       OF WRK-AREA-CRUR4S1I
      *
           MOVE CRURW08C-S-CUSUAR-INCL OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CUSUAR-INCL
                                       OF WRK-AREA-CRUR4S1I
      *
           MOVE CRURW08C-S-HINCL-REG   OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-HINCL-REG
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-RMOTVO-SIT-OPER
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-RMOTVO-SIT-OPER
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-CFORMA-LIQDC-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CFORMA-LIQDC-CONTR
                                       OF WRK-AREA-CRUR4S1I
           MOVE CRURW08C-S-DEXCL-CONTR-BACEN
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-DEXCL-CONTR-BACEN
                                       OF WRK-AREA-CRUR4S1I
      *
           MOVE 'A'                    TO CRURWS1C-E-CINDCD-TPO-MANUT
                                       OF WRK-AREA-CRUR4S1I
      *
           IF (CRURW08C-S-CUSUAR-MANUT OF WRK-AREA-CRUR408S
                                       EQUAL SPACES)
               MOVE CRURW08C-S-CUSUAR-INCL
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CUSUAR-MANUT
                                       OF WRK-AREA-CRUR4S1I
               MOVE CRURW08C-S-HINCL-REG
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-HMANUT-REG
                                       OF WRK-AREA-CRUR4S1I
           ELSE
               MOVE CRURW08C-S-CUSUAR-MANUT
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-CUSUAR-MANUT
                                       OF WRK-AREA-CRUR4S1I
               MOVE CRURW08C-S-HMANUT-REG
                                       OF WRK-AREA-CRUR408S
                                       TO CRURWS1C-E-HMANUT-REG
                                       OF WRK-AREA-CRUR4S1I
           END-IF
           .
      *----------------------------------------------------------------*
       2314-99-FIM.                     EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO TRATAMENTO COR0001E                    *
      *----------------------------------------------------------------*
       2320-TRATA-COR0001E             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWE11-SISMSG(1: LENGTH OF CRUR21-COR0001-1)
                            TO CRUR21-COR0001-1
EL1115     SET WRK-REENVIA-NAO         TO TRUE
.     *
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2320.CRUR21-CODMSG-E: ' CRUR21-CODMSG-E
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
.          IF (CRUR21-CODMSG-E         EQUAL 'EGEN0050'   )
.             SET WRK-REENVIA-SIM      TO TRUE
.          END-IF
.     *
.          IF (CRUR21-DTEMS-E          EQUAL 'ECOR0021'   ) AND
.             (CRUR21-DTEMS            LESS WRK-DATA-ATUAL)
.             SET WRK-REENVIA-SIM      TO TRUE
EL1115     END-IF

EL2508     IF  WRK-REENVIA-SIM
.     *    **** PENDENTE INCLUSÃ-O BACEN *
.             MOVE 3                   TO CSIT-MSGEM-BACEN  OF CRURB0U2
.             MOVE 'RCOR8015'          TO CUSUAR-MANUT      OF CRURB0U2
.             MOVE CRUR21-CODMSG       TO CMSGEM-SIST-BACEN OF CRURB0U2
.             PERFORM 4200-UPDATE-CRURB0U2
EL2508     ELSE
              PERFORM 4000-SELECIONAR-MAX-RETOR
              MOVE 'RCOR8015'          TO CUSUAR-INCL       OF CRURB0UB
BR0818        MOVE '***********'
                                       TO CPROT-MSGEM-RETOR OF CRURB0UB
              PERFORM 4100-INCLUIR-CRURB0UB
      *    **** ERRO DE INCLUSAO *
              MOVE 1                   TO CSIT-MSGEM-BACEN  OF CRURB0U2
              MOVE 'RCOR8015'          TO CUSUAR-MANUT      OF CRURB0U2
              MOVE CRUR21-CODMSG       TO CMSGEM-SIST-BACEN OF CRURB0U2
              PERFORM 4200-UPDATE-CRURB0U2
           END-IF
EL2508     .
      *
      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO TRATAMENTO COR0002R1                  *
      *----------------------------------------------------------------*
       2330-TRATA-COR0002R1            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWE11-SISMSG(1: LENGTH OF CRURWW21-REGISTRO)
                            TO CRURWW21-REGISTRO

           PERFORM 4000-SELECIONAR-MAX-RETOR

           MOVE 'RCOR8025' TO CUSUAR-INCL        OF CRURB0UB
           MOVE SPACES     TO CPROT-MSGEM-RETOR  OF CRURB0UB
           PERFORM 4100-INCLUIR-CRURB0UB
      *    **** RETORNO OK EXCLUSAO *
           MOVE 10                TO CSIT-MSGEM-BACEN  OF CRURB0U2
           MOVE 'RCOR8025'        TO CUSUAR-MANUT      OF CRURB0U2
           MOVE CRURWW21-COD-MSG  TO CMSGEM-SIST-BACEN OF CRURB0U2
           PERFORM 4200-UPDATE-CRURB0U2
           PERFORM 4400-UPDATE-DTEXCL
VANS
VANS       PERFORM 2331-SELECT-CONTR
VANS       PERFORM 2332-INCLUIR-HIST
VANS
VANS       PERFORM 4410-OBTER-INFO-PROP
VANS
VANS       PERFORM 4420-REVERT-RESERV-DOTAC
VANS
VANS       PERFORM 4430-OBTER-SITUACAO-PROPOSTA
VANS
VANS       IF FNEBWI9C-S-CSIT-PPSTA-NEGOC
VANS                                   EQUAL 16
VANS          PERFORM 4440-ACESSAR-FNEB3YBA
VANS       END-IF
VANS
VANS       IF FNEBWI9C-S-CSIT-PPSTA-NEGOC
VANS                                   NOT EQUAL 10 AND 11
VANS          PERFORM 4450-CANC-RESEV-PROP-CFUR
VANS       END-IF
           .
      *
      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

VANS  *----------------------------------------------------------------*
VANS  * ROTINA RESPONSAVEL POR SELECIONAR O CONTRATO
VANS  *----------------------------------------------------------------*
VANS   2331-SELECT-CONTR               SECTION.
VANS  *----------------------------------------------------------------*
VANS  *
VANS       INITIALIZE                  CRURB0LJ
VANS
VANS       MOVE  CPSSOA-JURID-CONTR OF CRURB0U2
VANS                                TO CPSSOA-JURID-CONTR OF CRURB008
VANS       MOVE  CTPO-CONTR-NEGOC   OF CRURB0U2
VANS                                TO CTPO-CONTR-NEGOC   OF CRURB008
VANS       MOVE  NSEQ-CONTR-NEGOC   OF CRURB0U2
VANS                                TO NSEQ-CONTR-NEGOC   OF CRURB008
VANS
VANS       EXEC SQL
VANS         SELECT  CPSSOA_JURID_CONTR
VANS                ,CTPO_CONTR_NEGOC
VANS                ,NSEQ_CONTR_NEGOC
VANS                ,CPERDC
VANS                ,CORIGE_REC_CREDT
VANS                ,CMOTVO_SIT_RURAL
VANS                ,CSIT_OPER_RURAL
VANS                ,CIDIOM
VANS                ,NCONTR_FUMAG_RURAL
VANS                ,VCONTR_RURAL
VANS                ,CREFT_BACEN_CONTR
VANS                ,CSCRTZ_CONTR_RURAL
VANS                ,CCONVE_CONTR_CREDT
VANS                ,CUSO_LIM_RURAL
VANS                ,PSPREAD_CONTR
VANS                ,DEFTVC_CONTR_RURAL
VANS                ,DGERAC_DOCTO_CONTR
VANS                ,DVLR_CONTR_NEGOC
VANS                ,PTX_EFETV_JURO
VANS                ,VCTBIL_CONTR_CREDT
VANS                ,DENVIO_REG_BACEN
VANS                ,CSIT_REG_BACEN
VANS                ,NLOTE_CONTR_RURAL
VANS                ,QCOOP_LOTE_RURAL
VANS                ,DLIM_APROV_CONTR
VANS                ,CGRAU_SVRDE_INCON
VANS                ,NCDULA_CREDT_RURAL
VANS                ,CCDULA_RURAL_BACEN
VANS                ,CMOTVO_EXCL_BACEN
VANS                ,CUSUAR_INCL
VANS                ,HINCL_REG
VANS                ,CUSUAR_MANUT
VANS                ,HMANUT_REG
VANS                ,RMOTVO_SIT_OPER
VANS                ,CFORMA_LIQDC_CONTR
VANS                ,DEXCL_CONTR_BACEN
VANS                ,CPSSOA
VANS                ,CFICHA_CAD_RURAL
VANS                ,CINDCD_RENDA_PREVT
VANS          INTO  :CRURB0LJ.CPSSOA-JURID-CONTR
VANS               ,:CRURB0LJ.CTPO-CONTR-NEGOC
VANS               ,:CRURB0LJ.NSEQ-CONTR-NEGOC
VANS               ,:CRURB0LJ.CPERDC
VANS               ,:CRURB0LJ.CORIGE-REC-CREDT
VANS               ,:CRURB0LJ.CMOTVO-SIT-RURAL
VANS                           :WRK-B8-CMOTVO-SIT-NULL
VANS               ,:CRURB0LJ.CSIT-OPER-RURAL
VANS               ,:CRURB0LJ.CIDIOM
VANS               ,:CRURB0LJ.NCONTR-FUMAG-RURAL
VANS                           :WRK-B8-NCONTR-FUMAG-NULL
VANS               ,:CRURB0LJ.VCONTR-RURAL
VANS               ,:CRURB0LJ.CREFT-BACEN-CONTR
VANS                           :WRK-B8-CREFT-BACEN-NULL
VANS               ,:CRURB0LJ.CSCRTZ-CONTR-RURAL
VANS               ,:CRURB0LJ.CCONVE-CONTR-CREDT
VANS               ,:CRURB0LJ.CUSO-LIM-RURAL
VANS               ,:CRURB0LJ.PSPREAD-CONTR
VANS                           :WRK-B8-PSPREAD-NULL
VANS               ,:CRURB0LJ.DEFTVC-CONTR-RURAL
VANS               ,:CRURB0LJ.DGERAC-DOCTO-CONTR
VANS                           :WRK-B8-DGERAC-DOCTO-NULL
VANS               ,:CRURB0LJ.DVLR-CONTR-NEGOC
VANS               ,:CRURB0LJ.PTX-EFETV-JURO
VANS                           :WRK-B8-PTX-EFETV-NULL
VANS               ,:CRURB0LJ.VCTBIL-CONTR-CREDT
VANS               ,:CRURB0LJ.DENVIO-REG-BACEN
VANS                           :WRK-B8-DENVIO-REG-NULL
VANS               ,:CRURB0LJ.CSIT-REG-BACEN
VANS               ,:CRURB0LJ.NLOTE-CONTR-RURAL
VANS                           :WRK-B8-NLOTE-NULL
VANS               ,:CRURB0LJ.QCOOP-LOTE-RURAL
VANS                           :WRK-B8-QCOOP-LOTE-NULL
VANS               ,:CRURB0LJ.DLIM-APROV-CONTR
VANS               ,:CRURB0LJ.CGRAU-SVRDE-INCON
VANS               ,:CRURB0LJ.NCDULA-CREDT-RURAL
VANS                           :WRK-B8-NCDULA-CREDT-NULL
VANS               ,:CRURB0LJ.CCDULA-RURAL-BACEN
VANS                           :WRK-B8-CCDULA-RURAL-NULL
VANS               ,:CRURB0LJ.CEXCL-SIST-BACEN
VANS                           :WRK-B8-CEXCL-SIST-NULL
VANS               ,:CRURB0LJ.CUSUAR-INCL
VANS               ,:CRURB0LJ.HINCL-REG
VANS               ,:CRURB0LJ.CUSUAR-MANUT
VANS                           :WRK-B8-CUSUAR-MANUT-NULL
VANS               ,:CRURB0LJ.HMANUT-REG
VANS                           :WRK-B8-HMANUT-REG-NULL
VANS               ,:CRURB0LJ.RMOTVO-SIT-OPER
VANS                           :WRK-B8-RMOTVO-SIT-NULL
VANS               ,:CRURB0LJ.CFORMA-LIQDC-CONTR
VANS               ,:CRURB0LJ.DEXCL-CONTR-BACEN
VANS                           :WRK-B8-DEXCL-CONTR-NULL
VANS               ,:CRURB0LJ.CPSSOA
VANS                           :WRK-B8-CPSSOA-NULL
VANS               ,:CRURB0LJ.CFICHA-CAD-RURAL
VANS                           :WRK-B8-CFICHA-CAD-NULL
VANS               ,:CRURB0LJ.CINDCD-RENDA-PREVT
VANS                           :WRK-B8-CINDCD-RENDA-PREVT-NULL
VANS           FROM DB2PRD.TCONTR_CREDT_RURAL
VANS          WHERE CPSSOA_JURID_CONTR = :CRURB008.CPSSOA-JURID-CONTR
VANS            AND CTPO_CONTR_NEGOC   = :CRURB008.CTPO-CONTR-NEGOC
VANS            AND NSEQ_CONTR_NEGOC   = :CRURB008.NSEQ-CONTR-NEGOC
VANS       END-EXEC
VANS  *
VANS       IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
VANS       OR (SQLWARN0                EQUAL 'W')
VANS           SET DB2-SELECT          TO TRUE
VANS           MOVE 'TCONTR_CREDT_RURAL'
VANS                                   TO FRWKGDB2-NOME-TABELA
VANS           MOVE '2331-SELECT-CONTR'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           MOVE '2331'             TO FRWKGDB2-LOCAL
VANS           PERFORM 9000-ERRO-DB2
VANS       END-IF.
VANS
VANS       IF (SQLCODE                 EQUAL +100)
VANS           STRING  'REGISTRO NAO ENCONTRADO TCONTR_CREDT_RURAL'
VANS                   DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
VANS           SET     ERRO-LIVRE  TO    TRUE
VANS           MOVE '2331-SELECT-CONTR'
VANS                               TO    FRWKGHEA-IDEN-PARAGRAFO
VANS           MOVE    'CRUR0507'  TO    FRWKGLIV-COD-MENSAGEM
VANS           MOVE 99             TO    CRURWE11-COD-RETORNO
VANS           PERFORM 9500-ERRO-LIVRE
VANS       ELSE
VANS           IF  CUSUAR-INCL         OF CRURB0U2
VANS                                   EQUAL 'AUTOMATIC'
VANS               MOVE 50             TO CSIT-OPER-RURAL
VANS                                   OF CRURB0LJ
VANS           ELSE
VANS               MOVE 5              TO CSIT-OPER-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CMOTVO-SIT-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CMOTVO-SIT-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-NCONTR-FUMAG-NULL
VANS                                   NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO NCONTR-FUMAG-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CREFT-BACEN-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CREFT-BACEN-CONTR
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-PSPREAD-NULL  NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO PSPREAD-CONTR
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-DGERAC-DOCTO-NULL NOT EQUAL ZEROS
VANS              MOVE SPACES          TO DGERAC-DOCTO-CONTR
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-PTX-EFETV-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO PTX-EFETV-JURO
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-DENVIO-REG-NULL  NOT EQUAL ZEROS
VANS              MOVE SPACES          TO DENVIO-REG-BACEN
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-NLOTE-NULL    NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO NLOTE-CONTR-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-QCOOP-LOTE-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO QCOOP-LOTE-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-NCDULA-CREDT-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO NCDULA-CREDT-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CCDULA-RURAL-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CCDULA-RURAL-BACEN
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CEXCL-SIST-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CEXCL-SIST-BACEN
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CUSUAR-MANUT-NULL NOT EQUAL ZEROS
VANS              MOVE SPACES          TO CUSUAR-MANUT
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-HMANUT-REG-NULL NOT EQUAL ZEROS
VANS              MOVE SPACES          TO HMANUT-REG
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-RMOTVO-SIT-NULL NOT EQUAL ZEROS
VANS              MOVE SPACES          TO RMOTVO-SIT-OPER
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-DEXCL-CONTR-NULL NOT EQUAL ZEROS
VANS              MOVE SPACES          TO DEXCL-CONTR-BACEN
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CPSSOA-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CPSSOA
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CFICHA-CAD-NULL  NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CFICHA-CAD-RURAL
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS           IF WRK-B8-CINDCD-RENDA-PREVT-NULL NOT EQUAL ZEROS
VANS              MOVE ZEROS           TO CINDCD-RENDA-PREVT
VANS                                   OF CRURB0LJ
VANS           END-IF
VANS
VANS        END-IF
VANS       .
VANS  *
VANS  *----------------------------------------------------------------
VANS   2331-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *
VANS  *----------------------------------------------------------------*
VANS  * ROTINA RESPONSAVEL POR INSERIR HISTORICO DO CONTRATO
VANS  *----------------------------------------------------------------*
VANS   2332-INCLUIR-HIST               SECTION.
VANS  *----------------------------------------------------------------*
VANS  *
VANS       MOVE '2'                    TO CINDCD-TPO-MANUT
VANS                                   OF CRURB0LJ
VANS  *
VANS       IF  CUSUAR-MANUT            OF CRURB0LJ
VANS                                   EQUAL SPACES
VANS           MOVE CUSUAR-INCL        OF CRURB0LJ
VANS                                   TO CUSUAR-MANUT
VANS                                   OF CRURB0LJ
VANS           MOVE HINCL-REG          OF CRURB0LJ
VANS                                   TO HMANUT-REG
VANS                                   OF CRURB0LJ
VANS       END-IF
VANS  *
VANS       EXEC SQL
VANS         INSERT INTO DB2PRD.THIST_CREDT_RURAL
VANS                     (CPSSOA_JURID_CONTR
VANS                     ,CTPO_CONTR_NEGOC
VANS                     ,NSEQ_CONTR_NEGOC
VANS                     ,HINCL_REG_HIST
VANS                     ,CPERDC
VANS                     ,CORIGE_REC_CREDT
VANS                     ,CMOTVO_SIT_RURAL
VANS                     ,CSIT_OPER_RURAL
VANS                     ,CIDIOM
VANS                     ,NCONTR_FUMAG_RURAL
VANS                     ,VCONTR_RURAL
VANS                     ,CREFT_BACEN_CONTR
VANS                     ,CSCRTZ_CONTR_RURAL
VANS                     ,CCONVE_CONTR_CREDT
VANS                     ,CUSO_LIM_RURAL
VANS                     ,PSPREAD_CONTR
VANS                     ,DEFTVC_CONTR_RURAL
VANS                     ,DGERAC_DOCTO_CONTR
VANS                     ,DVLR_CONTR_NEGOC
VANS                     ,PTX_EFETV_JURO
VANS                     ,VCTBIL_CONTR_CREDT
VANS                     ,DENVIO_REG_BACEN
VANS                     ,CSIT_REG_BACEN
VANS                     ,NLOTE_CONTR_RURAL
VANS                     ,QCOOP_LOTE_RURAL
VANS                     ,DLIM_APROV_CONTR
VANS                     ,CGRAU_SVRDE_INCON
VANS                     ,NCDULA_CREDT_RURAL
VANS                     ,CCDULA_RURAL_BACEN
VANS                     ,CEXCL_SIST_BACEN
VANS                     ,CINDCD_TPO_MANUT
VANS                     ,CUSUAR_INCL
VANS                     ,HINCL_REG
VANS                     ,CUSUAR_MANUT
VANS                     ,HMANUT_REG
VANS                     ,RMOTVO_SIT_OPER
VANS                     ,CFORMA_LIQDC_CONTR
VANS                     ,DEXCL_CONTR_BACEN
VANS                     ,CPSSOA
VANS                     ,CFICHA_CAD_RURAL
VANS                     ,CINDCD_RENDA_PREVT)
VANS               VALUES(:CRURB0LJ.CPSSOA-JURID-CONTR
VANS                     ,:CRURB0LJ.CTPO-CONTR-NEGOC
VANS                     ,:CRURB0LJ.NSEQ-CONTR-NEGOC
VANS                     ,CURRENT TIMESTAMP
VANS                     ,:CRURB0LJ.CPERDC
VANS                     ,:CRURB0LJ.CORIGE-REC-CREDT
VANS                     ,:CRURB0LJ.CMOTVO-SIT-RURAL
VANS                            :WRK-B8-CMOTVO-SIT-NULL
VANS                     ,:CRURB0LJ.CSIT-OPER-RURAL
VANS                     ,:CRURB0LJ.CIDIOM
VANS                     ,:CRURB0LJ.NCONTR-FUMAG-RURAL
VANS                            :WRK-B8-NCONTR-FUMAG-NULL
VANS                     ,:CRURB0LJ.VCONTR-RURAL
VANS                     ,:CRURB0LJ.CREFT-BACEN-CONTR
VANS                            :WRK-B8-CREFT-BACEN-NULL
VANS                     ,:CRURB0LJ.CSCRTZ-CONTR-RURAL
VANS                     ,:CRURB0LJ.CCONVE-CONTR-CREDT
VANS                     ,:CRURB0LJ.CUSO-LIM-RURAL
VANS                     ,:CRURB0LJ.PSPREAD-CONTR
VANS                            :WRK-B8-PSPREAD-NULL
VANS                     ,:CRURB0LJ.DEFTVC-CONTR-RURAL
VANS                     ,:CRURB0LJ.DGERAC-DOCTO-CONTR
VANS                            :WRK-B8-DGERAC-DOCTO-NULL
VANS                     ,:CRURB0LJ.DVLR-CONTR-NEGOC
VANS                     ,:CRURB0LJ.PTX-EFETV-JURO
VANS                            :WRK-B8-PTX-EFETV-NULL
VANS                     ,:CRURB0LJ.VCTBIL-CONTR-CREDT
VANS                     ,:CRURB0LJ.DENVIO-REG-BACEN
VANS                            :WRK-B8-DENVIO-REG-NULL
VANS                     ,:CRURB0LJ.CSIT-REG-BACEN
VANS                     ,:CRURB0LJ.NLOTE-CONTR-RURAL
VANS                            :WRK-B8-NLOTE-NULL
VANS                     ,:CRURB0LJ.QCOOP-LOTE-RURAL
VANS                            :WRK-B8-QCOOP-LOTE-NULL
VANS                     ,:CRURB0LJ.DLIM-APROV-CONTR
VANS                     ,:CRURB0LJ.CGRAU-SVRDE-INCON
VANS                     ,:CRURB0LJ.NCDULA-CREDT-RURAL
VANS                            :WRK-B8-NCDULA-CREDT-NULL
VANS                     ,:CRURB0LJ.CCDULA-RURAL-BACEN
VANS                            :WRK-B8-CCDULA-RURAL-NULL
VANS                     ,:CRURB0LJ.CEXCL-SIST-BACEN
VANS                            :WRK-B8-CEXCL-SIST-NULL
VANS                     ,:CRURB0LJ.CINDCD-TPO-MANUT
VANS                     ,:CRURB0LJ.CUSUAR-INCL
VANS                     ,:CRURB0LJ.HINCL-REG
VANS                     ,:CRURB0LJ.CUSUAR-MANUT
VANS                            :WRK-B8-CUSUAR-MANUT-NULL
VANS                     ,:CRURB0LJ.HMANUT-REG
VANS                            :WRK-B8-HMANUT-REG-NULL
VANS                     ,:CRURB0LJ.RMOTVO-SIT-OPER
VANS                            :WRK-B8-RMOTVO-SIT-NULL
VANS                     ,:CRURB0LJ.CFORMA-LIQDC-CONTR
VANS                     ,:CRURB0LJ.DEXCL-CONTR-BACEN
VANS                            :WRK-B8-DEXCL-CONTR-NULL
VANS                     ,:CRURB0LJ.CPSSOA
VANS                            :WRK-B8-CPSSOA-NULL
VANS                     ,:CRURB0LJ.CFICHA-CAD-RURAL
VANS                            :WRK-B8-CFICHA-CAD-NULL
VANS                     ,:CRURB0LJ.CINDCD-RENDA-PREVT
VANS                            :WRK-B8-CINDCD-RENDA-PREVT-NULL)
VANS
VANS       END-EXEC
VANS  *
VANS       IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
VANS       OR (SQLWARN0                EQUAL 'W')
VANS           SET DB2-INSERT          TO TRUE
VANS           MOVE 'THIST_CREDT_RURAL'
VANS                                   TO FRWKGDB2-NOME-TABELA
VANS           MOVE '2332-INCLUIR-HIST'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           MOVE '2332'             TO FRWKGDB2-LOCAL
VANS           PERFORM 9000-ERRO-DB2
VANS       END-IF.
VANS
VANS       IF (SQLCODE                 EQUAL -803)
VANS           STRING  'REGISTRO DUPLICADO THIST_CREDT_RURAL'
VANS                   DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
VANS           SET     ERRO-LIVRE  TO    TRUE
VANS           MOVE '2332-INCLUIR-HIST'
VANS                               TO    FRWKGHEA-IDEN-PARAGRAFO
VANS           MOVE    'CRUR2195'  TO    FRWKGLIV-COD-MENSAGEM
VANS           MOVE 99             TO    CRURWE11-COD-RETORNO
VANS           PERFORM 9500-ERRO-LIVRE
VANS       END-IF
VANS       .
VANS  *
VANS  *----------------------------------------------------------------*
VANS   2332-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *
      *----------------------------------------------------------------*
VANS  * ROTINA RESPONSAVEL PELO TRATAMENTO COR0002E                   *
      *----------------------------------------------------------------*
       2340-TRATA-COR0002E             SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWE11-SISMSG(1: LENGTH OF CRURWI41-REGISTRO)
                            TO CRURWI41-REGISTRO

           PERFORM 4000-SELECIONAR-MAX-RETOR

           MOVE 'RCOR8035' TO CUSUAR-INCL        OF CRURB0UB
           MOVE CRURWI41-NUM-REF-BACEN
                           TO CPROT-MSGEM-RETOR  OF CRURB0UB
           PERFORM 4100-INCLUIR-CRURB0UB
      *    **** ERRO DE EXCLUSAO *
           MOVE 2                 TO CSIT-MSGEM-BACEN  OF CRURB0U2
           MOVE 'RCOR8035'        TO CUSUAR-MANUT      OF CRURB0U2
           MOVE CRURWI41-CODMSG   TO CMSGEM-SIST-BACEN OF CRURB0U2
           PERFORM 4200-UPDATE-CRURB0U2
VANS
VANS       IF  CRURWI41-CODMSG-E       EQUAL 'ECOR0001' OR 'ECOR0002'
VANS           PERFORM 4400-UPDATE-DTEXCL
VANS           PERFORM 2331-SELECT-CONTR
VANS           PERFORM 2332-INCLUIR-HIST
VANS
VANS           PERFORM 4410-OBTER-INFO-PROP
VANS
VANS           PERFORM 4420-REVERT-RESERV-DOTAC
VANS
VANS           PERFORM 4430-OBTER-SITUACAO-PROPOSTA
VANS
VANS           IF FNEBWI9C-S-CSIT-PPSTA-NEGOC
VANS                                   EQUAL 16
VANS              PERFORM 4440-ACESSAR-FNEB3YBA
VANS           END-IF
VANS
VANS           IF FNEBWI9C-S-CSIT-PPSTA-NEGOC
VANS                                   NOT EQUAL 10 AND 11
VANS              PERFORM 4450-CANC-RESEV-PROP-CFUR
VANS           END-IF
VANS       END-IF
           .
      *
      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO TRATAMENTO GEN0004                   *
      *----------------------------------------------------------------*
       2350-TRATA-GEN0004              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWE11-SISMSG(1: LENGTH OF RCOR05-REGISTRO)
                            TO RCOR05-REGISTRO

           MOVE  CPSSOA-JURID-CONTR OF CRURB0UA
                                    TO CPSSOA-JURID-CONTR OF CRURB0U2
           MOVE  CTPO-CONTR-NEGOC   OF CRURB0UA
                                    TO CTPO-CONTR-NEGOC   OF CRURB0U2
           MOVE  NSEQ-CONTR-NEGOC   OF CRURB0UA
                                    TO NSEQ-CONTR-NEGOC   OF CRURB0U2
           MOVE  NENVIO-INFO-RECOR  OF CRURB0UA
                                    TO NENVIO-INFO-RECOR  OF CRURB0U2

           PERFORM 4000-SELECIONAR-MAX-RETOR

           MOVE 'RCOR8045' TO CUSUAR-INCL        OF CRURB0UB
           MOVE SPACES     TO CPROT-MSGEM-RETOR  OF CRURB0UB
           PERFORM 4100-INCLUIR-CRURB0UB
      *    **** ERRO GEN0004 *
           MOVE 12                TO CSIT-MSGEM-BACEN  OF CRURB0U2
           MOVE 'RCOR8045'        TO CUSUAR-MANUT      OF CRURB0U2
           MOVE RCOR05-COD-MSG    TO CMSGEM-SIST-BACEN OF CRURB0U2
           PERFORM 4200-UPDATE-CRURB0U2
           .
      *
      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELO TRATAMENTO GEN0004                   *
      *----------------------------------------------------------------*
       2360-TRATA-TMNF                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE CRURWE11-SISMSG(1: LENGTH OF RCOR03-REGISTRO)
                            TO RCOR03-REGISTRO

TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2360.RCOR03-CRETOR-PRINC: ' RCOR03-CRETOR-PRINC
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO

           IF  RCOR03-CRETOR-PRINC NUMERIC
           AND RCOR03-CRETOR-PRINC EQUAL ZEROS
               PERFORM 2130-VERIFICA-CONT
               PERFORM 2400-UPDATE-MSG-REMSS
               PERFORM 2160-SELECT-CRURB0UA-CONT
           ELSE
               PERFORM 4000-SELECIONAR-MAX-RETOR
               MOVE 'RCOR8055' TO CUSUAR-INCL        OF CRURB0UB
               MOVE SPACES     TO CPROT-MSGEM-RETOR  OF CRURB0UB
               PERFORM 4100-INCLUIR-CRURB0UB
      *        **** ERRO TMNF *
               MOVE 11                TO CSIT-MSGEM-BACEN  OF CRURB0U2
               MOVE 'RCOR8055'        TO CUSUAR-MANUT      OF CRURB0U2
               MOVE 'TMNF     '       TO CMSGEM-SIST-BACEN OF CRURB0U2
               PERFORM 4200-UPDATE-CRURB0U2
           END-IF

           .
      *
      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

COR6  *----------------------------------------------------------------*
COR6  * ROTINA RESPONSAVEL PELO TRATAMENTO COR0006R1                  *
COR6  *----------------------------------------------------------------*
COR6   2370-TRATA-COR0006R1            SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6       MOVE CRURWE11-SISMSG(1:     LENGTH OF CRURWW21-REGISTRO)
COR6                                   TO CRURWW21-REGISTRO
COR6
COR6       PERFORM 2375-GRAVAR-STRING-COR0006
COR6
COR6       PERFORM 2379-INTERPRETAR-COR0006R1
COR6
COR6       .
COR6  *----------------------------------------------------------------*
COR6   2370-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
COR6  *----------------------------------------------------------------*
COR6   2375-GRAVAR-STRING-COR0006      SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6       MOVE CRURWE11-SISMSG(1:     LENGTH OF CRURWW21-REGISTRO)
COR6                                   TO CRURWW21-REGISTRO
COR6
RUSSO1**  ROTINA INIBIDA PARA NAO HAVER 2 COMANDOS 'NEXT VALUE' ANTES
RUSSO1**  DO -INSERT
RUSSO1**  -PERFORM 2376-SELECT-MAX-CRURB0WD.
COR6
COR6       PERFORM 2377-SELECT-CONTRATO-ORIGINAL.
COR6
COR6       PERFORM 2378-INSERT-CRURB0WD.
COR6       .
COR6  *----------------------------------------------------------------*
COR6   2375-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
COR6  *----------------------------------------------------------------*
RUSSO1*2376-SELECT-MAX-CRURB0WD       SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6  **  -MOVE ZEROS                 TO NMNTRC-MSGEM-RURAL OF CRURB0WD
COR6  **
COR6  **  -EXEC SQL
COR6  **    -SELECT
COR6  **      -VALUE ( MAX ( NMNTRC_MSGEM_RURAL ) , 0 )
COR6  **    -INTO
COR6  **      -:CRURB0WD.NMNTRC-MSGEM-RURAL
COR6  **    -FROM  DB2PRD.TMNTRC_CONTR_RURAL
COR6  **    -WHERE NMNTRC_MSGEM_RURAL  >= :CRURB0WD.NMNTRC-MSGEM-RURAL
COR6  **  -END-EXEC.
COR6  **
COR6  **  -IF (SQLCODE                 NOT EQUAL ZEROS )
COR6  **  -OR (SQLWARN0                EQUAL 'W')
COR6  **      -SET  DB2-SELECT         TO TRUE
COR6  **      -MOVE 'TMNTRC_CONTR_RURAL'
COR6  **                              -TO FRWKGDB2-NOME-TABELA
COR6  **      -MOVE '2376-SELECT-MAX-CRURB0WD'
COR6  **                              -TO FRWKGHEA-IDEN-PARAGRAFO
COR6  **      -MOVE '2376'             TO FRWKGDB2-LOCAL
COR6  **      -PERFORM 9000-ERRO-DB2
COR6  **  -END-IF.
COR6  **
COR6  *----------------------------------------------------------------*
RUSSO1*2376-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
COR6  *----------------------------------------------------------------*
COR6   2377-SELECT-CONTRATO-ORIGINAL   SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6       MOVE CRURWE11-NRO-CTRL-IF   TO CRQUIS-FINCR-BACEN OF CRURB0WD
COR6  *
COR6       EXEC SQL
COR6         SELECT
URB6           CPSSOA_JURID_CONTR
COR6         , CTPO_CONTR_NEGOC
COR6         , NSEQ_CONTR_NEGOC
COR6         , CJURID_NEGOC_LIBRC
COR6         , CTPO_CONTR_LIBRC
COR6         , NCONTR_LIBRC_ORIGN
COR6         INTO
COR6           :CRURB0WD.CPSSOA-JURID-CONTR :WRK-CPSSOA-CRUR-NULL
COR6         , :CRURB0WD.CTPO-CONTR-NEGOC   :WRK-CTPO-CRUR-NULL
COR6         , :CRURB0WD.NSEQ-CONTR-NEGOC   :WRK-NSEQ-CRUR-NULL
COR6         , :CRURB0WD.CJURID-NEGOC-LIBRC :WRK-CPSSOA-FNEB-NULL
COR6         , :CRURB0WD.CTPO-CONTR-LIBRC   :WRK-CTPO-FNEB-NULL
COR6         , :CRURB0WD.NCONTR-LIBRC-ORIGN :WRK-NSEQ-FNEB-NULL
COR6         FROM   DB2PRD.TMNTRC_CONTR_RURAL
COR6         WHERE  CRQUIS_FINCR_BACEN  = :CRURB0WD.CRQUIS-FINCR-BACEN
COR6         AND    CMSGEM_SIST_BACEN   = 'COR0006'
COR6       END-EXEC
COR6
COR6       IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
COR6       OR (SQLWARN0                EQUAL 'W')
COR6           SET  DB2-SELECT         TO TRUE
COR6           MOVE 'TMNTRC_CONTR_RURAL'
COR6                                   TO FRWKGDB2-NOME-TABELA
COR6           MOVE '2377-SELECT-CONTRATO-ORIGINAL'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6           MOVE '2377'             TO FRWKGDB2-LOCAL
COR6           PERFORM 9000-ERRO-DB2
COR6       END-IF.
COR6
COR6       IF  SQLCODE                 EQUAL +100
COR6           MOVE -1                 TO WRK-CPSSOA-FNEB-NULL
COR6                                      WRK-CTPO-FNEB-NULL
COR6                                      WRK-NSEQ-FNEB-NULL
COR6                                      WRK-CPSSOA-CRUR-NULL
COR6                                      WRK-CTPO-CRUR-NULL
COR6                                      WRK-NSEQ-CRUR-NULL
COR6*****      REGISTRA GLOG PARA ANALISE, MAS O PROCESSAMENTO NAO
COR6*****      EH INTERROMPIDO (ERRO NAO EH GRAVE)
COR6           STRING 'COR0006(ENVIO) NAO ENCONTRADA:'
COR6                  CRQUIS-FINCR-BACEN  OF CRURB0WD
COR6                  DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
COR6           MOVE '2377-SELECT-CONTRATO-ORIG'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6           PERFORM 9200-ERRO-LIVRE
COR6*****      CONTINUA
COR6       END-IF.
COR6
COR6       IF  WRK-CPSSOA-CRUR-NULL    NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  CPSSOA-JURID-CONTR
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6       IF  WRK-CTPO-CRUR-NULL      NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  CTPO-CONTR-NEGOC
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6       IF  WRK-NSEQ-CRUR-NULL      NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  NSEQ-CONTR-NEGOC
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6       IF  WRK-CPSSOA-FNEB-NULL    NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  CJURID-NEGOC-LIBRC
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6       IF  WRK-CTPO-FNEB-NULL      NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  CTPO-CONTR-LIBRC
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6       IF  WRK-NSEQ-FNEB-NULL      NOT EQUAL ZEROS
COR6           MOVE ZEROS              TO  NCONTR-LIBRC-ORIGN
COR6                                   OF  CRURB0WD
COR6       END-IF.
COR6
COR6  *----------------------------------------------------------------*
COR6   2377-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
COR6  *----------------------------------------------------------------*
COR6  * ROTINA RESPONSAVEL PELO TRATAMENTO COR0006E                  *
RUSSO1* ROTINA RESPONSAVEL PELO TRATAMENTO COR0003E                  *
COR6  *----------------------------------------------------------------*
COR6   2378-INSERT-CRURB0WD            SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
RUSSO1**  -EXEC SQL
.OR6  **    -SELECT MAX(NMNTRC_MSGEM_RURAL)+1
.OR6  **    -INTO :CRURB0WD.NMNTRC-MSGEM-RURAL
.OR6  **    -FROM DB2PRD.TMNTRC_CONTR_RURAL
.OR6  **  -END-EXEC
.OR6  **
.OR6  **  -IF (SQLCODE          NOT EQUAL ZEROS AND +100 AND -305 ) OR
.
.          EXEC SQL
.              SET :CRURB0WD.NMNTRC-MSGEM-RURAL = NEXT VALUE
.              FOR  DB2PRD.SEQ_CRURN003_NMNTRC_MSGEM_RURAL
.          END-EXEC.
.
RUSSO1     IF (SQLCODE          NOT EQUAL ZEROS) OR
COR6          (SQLWARN0         EQUAL 'W')
COR6           SET  DB2-SELECT         TO TRUE
COR6           MOVE 'TMNTRC_CONTR_RURAL'
COR6                                   TO FRWKGDB2-NOME-TABELA
COR6           PERFORM 9000-ERRO-DB2
COR6       END-IF
COR6
RUSSO1**  -IF  SQLCODE                 EQUAL -305
.OR6  **      -MOVE 1                  TO WRK-NMNTRC-MSGEM
.OR6  **  -ELSE
.OR6  **      -MOVE NMNTRC-MSGEM-RURAL OF CRURB0WD
.OR6  **                              -TO WRK-NMNTRC-MSGEM
.OR6  **  -END-IF
.
.          MOVE NMNTRC-MSGEM-RURAL OF CRURB0WD
RUSSO1                                 TO WRK-NMNTRC-MSGEM
COR6
COR6       MOVE WRK-NMNTRC-MSGEM       TO NMNTRC-MSGEM-RURAL OF CRURB0WD
COR6       MOVE CRURWE11-COD-MSG       TO CMSGEM-SIST-BACEN  OF CRURB0WD
COR6       MOVE WRK-TIMESTAMP          TO HMANUT-REG         OF CRURB0WD
COR6       MOVE CRURWE11-TAM-SISMSG    TO WCONTD-MSGEM-RURAL-LEN
COR6                                                         OF CRURB0WD
COR6       MOVE CRURWE11-SISMSG        TO WCONTD-MSGEM-RURAL-TEXT
COR6                                                         OF CRURB0WD
COR6
COR6       EXEC SQL
COR6         INSERT INTO DB2PRD.TMNTRC_CONTR_RURAL
COR6         ( NMNTRC_MSGEM_RURAL
COR6         , CMSGEM_SIST_BACEN
COR6         , CRQUIS_FINCR_BACEN
COR6         , CPSSOA_JURID_CONTR
COR6         , CTPO_CONTR_NEGOC
COR6         , NSEQ_CONTR_NEGOC
COR6         , CJURID_NEGOC_LIBRC
COR6         , CTPO_CONTR_LIBRC
COR6         , NCONTR_LIBRC_ORIGN
COR6         , HMANUT_REG
COR6         , WCONTD_MSGEM_RURAL    )
COR6         VALUES
COR6         ( :CRURB0WD.NMNTRC-MSGEM-RURAL
COR6         , :CRURB0WD.CMSGEM-SIST-BACEN
COR6         , :CRURB0WD.CRQUIS-FINCR-BACEN
COR6         , :CRURB0WD.CPSSOA-JURID-CONTR :WRK-CPSSOA-CRUR-NULL
COR6         , :CRURB0WD.CTPO-CONTR-NEGOC   :WRK-CTPO-CRUR-NULL
COR6         , :CRURB0WD.NSEQ-CONTR-NEGOC   :WRK-NSEQ-CRUR-NULL
COR6         , :CRURB0WD.CJURID-NEGOC-LIBRC :WRK-CPSSOA-FNEB-NULL
COR6         , :CRURB0WD.CTPO-CONTR-LIBRC   :WRK-CTPO-FNEB-NULL
COR6         , :CRURB0WD.NCONTR-LIBRC-ORIGN :WRK-NSEQ-FNEB-NULL
COR6         , :CRURB0WD.HMANUT-REG
COR6         , :CRURB0WD.WCONTD-MSGEM-RURAL )
COR6       END-EXEC.
COR6
COR6       IF (SQLCODE                 NOT EQUAL ZEROS )
COR6       OR (SQLWARN0                EQUAL 'W')
COR6           SET  DB2-INSERT         TO TRUE
COR6           MOVE 'TMNTRC_CONTR_RURAL'
COR6                                   TO FRWKGDB2-NOME-TABELA
COR6           MOVE '2378-INSERT-CRURB0WD'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6           MOVE '2378'             TO FRWKGDB2-LOCAL
COR6           PERFORM 9000-ERRO-DB2
COR6       END-IF.
COR6
COR6  *----------------------------------------------------------------*
COR6   2378-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
COR6  *----------------------------------------------------------------*
COR6   2379-INTERPRETAR-COR0006R1      SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6       INITIALIZE CRURW00W-BLOCO-RETORNO OF WRK-AREA-CRUR3C6I.
COR6  *
COR6       MOVE CRURWE11-COD-MSG       TO CRURWE12-COD-MSG
COR6       MOVE CRURWE11-NRO-CTRL-IF   TO CRURWE12-NRO-CTRL-IF
COR6       MOVE CRURWE11-TAM-SISMSG    TO CRURWE12-TAM-SISMSG
COR6       MOVE CRURWE11-SEQ-MSG       TO CRURWE12-SEQ-MSG
COR6       MOVE NMNTRC-MSGEM-RURAL     OF CRURB0WD
COR6                                   TO CRURWE12-NMNTRC-MSGEM
COR6       MOVE CRURWE11-SISMSG        TO CRURWE12-SISMSG
COR6  *
COR6       EXEC CICS LINK
COR6            PROGRAM  (WRK-CRUR3C6I)
COR6            COMMAREA (WRK-AREA-CRUR3C6I)
COR6            LENGTH   (LENGTH OF WRK-AREA-CRUR3C6I)
COR6            NOHANDLE
COR6       END-EXEC
COR6  *
COR6       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
COR6           MOVE '2379'             TO CRURW00W-COD-ERRO
COR6                                   OF WRK-AREA-CRUR408S
COR6           MOVE '2379-INTERPRETAR-COR0006R1'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6           PERFORM 9400-ERRO-CICS
COR6       END-IF
COR6  *
COR6       EVALUATE CRURW00W-COD-RETORNO
COR6                                   OF WRK-AREA-CRUR3C6I
COR6         WHEN 00
COR6              CONTINUE
COR6         WHEN OTHER
COR6  *
COR6              MOVE WRK-CRUR3C6I    TO FRWKGMOD-NOME-MODULO
COR6  *
COR6              MOVE CRURW00W-BLOCO-RETORNO
COR6                                   OF WRK-AREA-CRUR3C6I
COR6                                   TO FRWKGMOD-BLOCO-RETORNO
COR6  *
COR6              MOVE '2379-INTERPRETAR-COR0006R1'
COR6                                   TO FRWKGHEA-IDEN-PARAGRAFO
COR6              PERFORM 9300-ERRO-MODULO
COR6       END-EVALUATE
COR6  *
COR6       .
COR6  *
COR6  *----------------------------------------------------------------*
COR6   2379-99-FIM.                     EXIT.
COR6  *----------------------------------------------------------------*
COR6  *
COR6  *----------------------------------------------------------------*
COR6  * ROTINA RESPONSAVEL PELO TRATAMENTO COR0006E                  *
COR6  *----------------------------------------------------------------*
COR6   2380-TRATA-COR0006E             SECTION.
COR6  *----------------------------------------------------------------*
COR6  *
COR6       MOVE CRURWE11-SISMSG(1:     LENGTH OF CRURWW21-REGISTRO)
COR6                                   TO CRURWW21-REGISTRO
COR6
COR6       PERFORM 2375-GRAVAR-STRING-COR0006.
COR6
COR6  *----------------------------------------------------------------*
COR6   2380-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6
RUSSO1*----------------------------------------------------------------*
RUSSO1* ROTINA RESPONSAVEL PELO TRATAMENTO COR0003R1 E COR0003E       *
RUSSO1*----------------------------------------------------------------*
RUSSO1 2390-TRATA-COR0003R1-E          SECTION.
RUSSO1*----------------------------------------------------------------*
.     *
.          MOVE CRURWE11-SISMSG(1:     LENGTH OF CRURWW21-REGISTRO)
.                                      TO CRURWW21-REGISTRO
.
.          PERFORM 2391-SELECT-CONTRATO-ORIGINAL.
.
.          PERFORM 2378-INSERT-CRURB0WD.
.
RUSSO1*----------------------------------------------------------------*
RUSSO1 2390-99-FIM.                    EXIT.
RUSSO1*----------------------------------------------------------------*
RUSSO1
RUSSO1*----------------------------------------------------------------*
RUSSO1 2391-SELECT-CONTRATO-ORIGINAL   SECTION.
RUSSO1*----------------------------------------------------------------*
.     *
.          MOVE CRURWE11-NRO-CTRL-IF   TO CRQUIS-FINCR-BACEN OF CRURB0WD
.     *
.          EXEC SQL
.            SELECT
.              CPSSOA_JURID_CONTR
.            , CTPO_CONTR_NEGOC
.            , NSEQ_CONTR_NEGOC
.            , CJURID_NEGOC_LIBRC
.            , CTPO_CONTR_LIBRC
.            , NCONTR_LIBRC_ORIGN
.            INTO
.              :CRURB0WD.CPSSOA-JURID-CONTR :WRK-CPSSOA-CRUR-NULL
.            , :CRURB0WD.CTPO-CONTR-NEGOC   :WRK-CTPO-CRUR-NULL
.            , :CRURB0WD.NSEQ-CONTR-NEGOC   :WRK-NSEQ-CRUR-NULL
.            , :CRURB0WD.CJURID-NEGOC-LIBRC :WRK-CPSSOA-FNEB-NULL
.            , :CRURB0WD.CTPO-CONTR-LIBRC   :WRK-CTPO-FNEB-NULL
.            , :CRURB0WD.NCONTR-LIBRC-ORIGN :WRK-NSEQ-FNEB-NULL
.            FROM   DB2PRD.TMNTRC_CONTR_RURAL
.            WHERE  CRQUIS_FINCR_BACEN  = :CRURB0WD.CRQUIS-FINCR-BACEN
.            AND    CMSGEM_SIST_BACEN   = 'COR0003'
.          END-EXEC
.
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
.          OR (SQLWARN0                EQUAL 'W')
.              SET  DB2-SELECT         TO TRUE
.              MOVE 'TMNTRC_CONTR_RURAL'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '2391-SELECT-CONTRATO-ORIGINAL'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE '2391'             TO FRWKGDB2-LOCAL
.              PERFORM 9000-ERRO-DB2
.          END-IF.
.
.          IF  SQLCODE                 EQUAL +100
.              MOVE -1                 TO WRK-CPSSOA-FNEB-NULL
.                                         WRK-CTPO-FNEB-NULL
.                                         WRK-NSEQ-FNEB-NULL
.                                         WRK-CPSSOA-CRUR-NULL
.                                         WRK-CTPO-CRUR-NULL
.                                         WRK-NSEQ-CRUR-NULL
.   *****      REGISTRA GLOG PARA ANALISE, MAS O PROCESSAMENTO NAO
.   *****      EH INTERROMPIDO (ERRO NAO EH GRAVE)
.              STRING 'COR0003(ENVIO) NAO ENCONTRADA:'
.                     CRQUIS-FINCR-BACEN  OF CRURB0WD
.                     DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
.              MOVE '2391-SELECT-CONTRATO-ORIG'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9200-ERRO-LIVRE
.   *****      CONTINUA
.          END-IF.
.
.          IF  WRK-CPSSOA-CRUR-NULL    NOT EQUAL ZEROS
.              MOVE ZEROS              TO  CPSSOA-JURID-CONTR
.                                      OF  CRURB0WD
.          END-IF.
.
.          IF  WRK-CTPO-CRUR-NULL      NOT EQUAL ZEROS
.              MOVE ZEROS              TO  CTPO-CONTR-NEGOC
.                                      OF  CRURB0WD
.          END-IF.
.
.          IF  WRK-NSEQ-CRUR-NULL      NOT EQUAL ZEROS
.              MOVE ZEROS              TO  NSEQ-CONTR-NEGOC
.                                      OF  CRURB0WD
.          END-IF.
.
.          IF  WRK-CPSSOA-FNEB-NULL    NOT EQUAL ZEROS
.              MOVE ZEROS              TO  CJURID-NEGOC-LIBRC
.                                      OF  CRURB0WD
.          END-IF.
.
.          IF  WRK-CTPO-FNEB-NULL      NOT EQUAL ZEROS
.              MOVE ZEROS              TO  CTPO-CONTR-LIBRC
.                                      OF  CRURB0WD
.          END-IF.
.
.          IF  WRK-NSEQ-FNEB-NULL      NOT EQUAL ZEROS
.              MOVE ZEROS              TO  NCONTR-LIBRC-ORIGN
.                                      OF  CRURB0WD
.          END-IF.
.
RUSSO1*----------------------------------------------------------------*
RUSSO1 2391-99-FIM.                    EXIT.
RUSSO1*----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA ATUALIZAR MENSAGEM ENVIADAS                             *
      *----------------------------------------------------------------*
       2400-UPDATE-MSG-REMSS           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 'N'                TO   WRK-CODE100-B0UA

           PERFORM 2500-OPEN-CSR01-CRURB0UA

           PERFORM 2600-FETCH-CSR01-CRURB0UA

TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2400.WRK-CODE100-B0UA: ' WRK-CODE100-B0UA
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO
           IF  WRK-CODE100-B0UA  EQUAL 'S'
               STRING  'CONTRATO NAO ENCONTRADO TLIN_MSGEM_REMSS'
                       DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               SET     ERRO-LIVRE  TO    TRUE
               MOVE '2400-UPDATE-MSG-REMSS'
                                   TO    FRWKGHEA-IDEN-PARAGRAFO
               MOVE    'CRUR1108'  TO    FRWKGLIV-COD-MENSAGEM
               MOVE 03             TO    CRURWE11-COD-RETORNO
               PERFORM 9500-ERRO-LIVRE
           END-IF

           PERFORM UNTIL (WRK-CODE100-B0UA  EQUAL 'S')
                   PERFORM 2800-UPDATE-CRURB0UA
                   PERFORM 2600-FETCH-CSR01-CRURB0UA
           END-PERFORM

           PERFORM 2700-CLOSE-CSR01-CRURB0UA
           .
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO CURSOR.                                  *
      *----------------------------------------------------------------*
       2500-OPEN-CSR01-CRURB0UA        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  CPSSOA-JURID-CONTR OF CRURB0U2
                                    TO CPSSOA-JURID-CONTR OF CRURB0UA
TESTE                                  WRK-CPSSOA-JURID-CONTR-L
           MOVE  CTPO-CONTR-NEGOC   OF CRURB0U2
                                    TO CTPO-CONTR-NEGOC   OF CRURB0UA
TESTE                                  WRK-CTPO-CONTR-NEGOC-L
           MOVE  NSEQ-CONTR-NEGOC   OF CRURB0U2
                                    TO NSEQ-CONTR-NEGOC   OF CRURB0UA
TESTE                                  WRK-NSEQ-CONTR-NEGOC-L
           MOVE  NENVIO-INFO-RECOR  OF CRURB0U2
                                    TO NENVIO-INFO-RECOR  OF CRURB0UA
TESTE                                  WRK-NENVIO-INFO-RECOR-L

           MOVE ZEROS               TO NLIN-MSGEM-REMSS   OF CRURB0UA
           MOVE SPACES              TO CTRNSM-INFO-BACEN  OF CRURB0UA
TESTE      MOVE SPACES               TO WRK-TEXTO-ERRO
TESTE      STRING '2500.' WRK-CPSSOA-JURID-CONTR-L '/'
TESTE                     WRK-CTPO-CONTR-NEGOC-L '/'
TESTE                     WRK-NSEQ-CONTR-NEGOC-L '/'
TESTE                     WRK-NENVIO-INFO-RECOR-L
TESTE          DELIMITED BY SIZE   INTO WRK-TEXTO-ERRO
TESTE      PERFORM 9600-FORMATAR-ERRO-MODULO

           EXEC SQL
               OPEN CSR01-CRURB0UA
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0050'             TO FRWKGDB2-LOCAL
               MOVE 'TLIN_MSGEM_REMSS'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2500-OPEN-CSR01-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL FETCH CSR01-CRURB0UA
      *----------------------------------------------------------------*
       2600-FETCH-CSR01-CRURB0UA       SECTION.
      *----------------------------------------------------------------*
      *
           EXEC SQL
               FETCH CSR01-CRURB0UA INTO
                  :CRURB0UA.CPSSOA-JURID-CONTR
                 ,:CRURB0UA.CTPO-CONTR-NEGOC
                 ,:CRURB0UA.NSEQ-CONTR-NEGOC
                 ,:CRURB0UA.NENVIO-INFO-RECOR
                 ,:CRURB0UA.NLIN-MSGEM-REMSS
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0060'             TO FRWKGDB2-LOCAL
               MOVE 'TLIN_MSGEM_REMSS'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2600-FETCH-CSR01-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF

           IF (SQLCODE                 EQUAL +100)
              MOVE 'S'   TO  WRK-CODE100-B0UA
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO CURSOR.                                *
      *----------------------------------------------------------------*
       2700-CLOSE-CSR01-CRURB0UA       SECTION.
      *----------------------------------------------------------------*
      *

           EXEC SQL
               CLOSE CSR01-CRURB0UA
           END-EXEC

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0070'             TO FRWKGDB2-LOCAL
               MOVE 'TLIN_MSGEM_REMSS'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2700-CLOSE-CSR01-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL ATUALIZAR CRURB0UA
      *----------------------------------------------------------------*
       2800-UPDATE-CRURB0UA            SECTION.
      *----------------------------------------------------------------*
      *
           MOVE RCOR03-CNRO-UNIC-OPER TO
                                      CTRNSM-INFO-BACEN OF CRURB0UA
           MOVE 'RCOR8055'            TO CUSUAR-MANUT OF CRURB0UA
           MOVE WRK-TIMESTAMP         TO HMANUT-REG   OF CRURB0UA
           MOVE ZEROS                 TO WRK-B0UA-CUS-MAN-NULL
           MOVE ZEROS                 TO WRK-B0UA-HMA-REG-NULL

           EXEC SQL
             UPDATE   DB2PRD.TLIN_MSGEM_REMSS
                SET   CTRNSM_INFO_BACEN =  :CRURB0UA.CTRNSM-INFO-BACEN
                     ,CUSUAR_MANUT      =  :CRURB0UA.CUSUAR-MANUT
                                              :WRK-B0UA-CUS-MAN-NULL
                     ,HMANUT_REG        =  CURRENT TIMESTAMP
              WHERE CPSSOA_JURID_CONTR  =  :CRURB0UA.CPSSOA-JURID-CONTR
                AND CTPO_CONTR_NEGOC    =  :CRURB0UA.CTPO-CONTR-NEGOC
                AND NSEQ_CONTR_NEGOC    =  :CRURB0UA.NSEQ-CONTR-NEGOC
                AND NENVIO_INFO_RECOR   =  :CRURB0UA.NENVIO-INFO-RECOR
                AND NLIN_MSGEM_REMSS    =  :CRURB0UA.NLIN-MSGEM-REMSS
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-UPDATE          TO TRUE
               MOVE '0080'             TO FRWKGDB2-LOCAL
               MOVE 'TLIN_MSGEM_REMSS'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2800-UPDATE-CRURB0UA'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR ABRIR O CURSOR CRURB0MS                 *
      *----------------------------------------------------------------*
       2900-OPEN-CRURB0MS              SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB0MS
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS
      *
           MOVE CRURW08C-S-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO CPSSOA-JURID-CONTR
                                       OF CRURB0MS
           MOVE CRURW08C-S-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO CTPO-CONTR-NEGOC
                                       OF CRURB0MS
           MOVE CRURW08C-S-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO NSEQ-CONTR-NEGOC
                                       OF CRURB0MS
      *
           EXEC SQL
                OPEN CSR01-CRURB0MS
           END-EXEC
      *                                                                *
           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
               SET DB2-OPEN            TO TRUE
               MOVE '0520'             TO FRWKGDB2-LOCAL
               MOVE 'TPRTCP_PSSOA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2900-OPEN-CRURB0MS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.
      *                                                                *
      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR LER O CURSOR CRURB0MS                   *
      *----------------------------------------------------------------*
       2950-FETCH-CRURB0MS             SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           EXEC SQL
               FETCH CSR01-CRURB0MS
                INTO :CRURB0MS.CPSSOA
           END-EXEC
      *                                                                *
           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
               SET DB2-FETCH           TO TRUE
               MOVE '0530'             TO FRWKGDB2-LOCAL
               MOVE 'TPRTCP_PSSOA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2950-FETCH-CRURB0MS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
      *                                                                *
           .
      *
      *----------------------------------------------------------------*
       2950-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL TRATAR EMITENTE                             *
      *----------------------------------------------------------------*
       2955-TRATAR-EMITENTE            SECTION.
      *----------------------------------------------------------------*
      *                                                                *
            PERFORM 2900-OPEN-CRURB0MS
            PERFORM 2950-FETCH-CRURB0MS
            PERFORM 2960-CLOSE-CRURB0MS
      *
           .
      *
      *----------------------------------------------------------------*
       2955-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL POR FECHAR O CURSOR CRURB0MS                *
      *----------------------------------------------------------------*
       2960-CLOSE-CRURB0MS             SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           EXEC SQL
               CLOSE CSR01-CRURB0MS
           END-EXEC
      *                                                                *
           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-CLOSE           TO TRUE
               MOVE '0550'             TO FRWKGDB2-LOCAL
               MOVE 'TPRTCP_PSSOA_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '2960-CLOSE-CRURB0MS'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF.
      *                                                                *
      *----------------------------------------------------------------*
       2960-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
 .    *    ROTINA PARA ACIONAR PROCESSO ESVB                           *
 .    *----------------------------------------------------------------*
 .     2995-ACESSAR-ESVB1061           SECTION.
 .    *----------------------------------------------------------------*
 .    *
 .         INITIALIZE                     ESVBEL61-REGISTRO
 .                                        WRK-ESVB1061-PARMS
 .    *
 .         MOVE 2                      TO ESVBEL61-E-CODVERSAO
 .    *
 .         MOVE 'I922440  '            TO ESVBEL61-E-CUSUAR-SOLCT
 .    *
 .         MOVE 'CRUR1O11'             TO ESVBEL61-E-PGM-CHAMADOR
 .    *
 .         MOVE 'CRUR'                 TO ESVBEL61-E-CSIST
 .    *
 .         EXEC CICS
 .             ASSIGN APPLID(WRK-AMBIENTE)
 .             NOHANDLE
 .         END-EXEC
 .    *
 .         IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
 .             MOVE '0220'             TO FRWKGMOD-COD-ERRO
 .             MOVE '2995-ACESSAR-ESVB1061'
 .                                     TO FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9400-ERRO-CICS
 .         END-IF
 .    *
 .    *    OS PROCESSOS AINDA INDEFINIDOS ESTA MOVENDO 999 P T0DOS
 .    *
 .    *--> DESENVOLVIMENTO - M04 / M20
 .    *
 .         IF (WRK-AMBIENTE(6:3)       EQUAL 'M04' OR 'M20')
 .             MOVE 999                TO ESVBEL61-E-CFUNCL-PROCS
 .         END-IF
 .    *
 .    *--> TESTE INTEGRADO - M50
 .    *
 .         IF (WRK-AMBIENTE(6:3)       EQUAL 'M50')
 .             MOVE 388                TO ESVBEL61-E-CFUNCL-PROCS
 .         END-IF
 .    *
 .    *--> HOMOLOGACAO - M11
 .    *
 .         IF (WRK-AMBIENTE(6:3)       EQUAL 'M11')
 .             MOVE 394                TO ESVBEL61-E-CFUNCL-PROCS
 .         END-IF
030724*
"     *--> NOVO TI -  M66 / M67 / M68
"     *
"          IF (WRK-AMBIENTE(6:3)       EQUAL 'M66' OR 'M67' OR 'M68')
"              MOVE 2047               TO ESVBEL61-E-CFUNCL-PROCS
030724     END-IF
 .    *
 .    *--> PRODUCAO - M12
 .    *
 .         IF (WRK-AMBIENTE(6:3)       EQUAL 'M12')
 .             MOVE 2047               TO ESVBEL61-E-CFUNCL-PROCS
 .         END-IF
 .    *
 .         MOVE 'S'                    TO ESVBEL61-E-AGENDA-IMEDIATA
 .    *
 .         MOVE SPACES                 TO ESVBEL61-E-DATA-AGENDA
 .    *
 .         MOVE SPACES                 TO ESVBEL61-E-HORA-AGENDA
 .    *
           PERFORM 2955-TRATAR-EMITENTE
      *
           MOVE CPSSOA                 OF CRURB0MS
                                       TO WRK-S9-10
           MOVE WRK-9-10               TO WRK-CPSSOA
      *
           MOVE CRURW08C-S-CPSSOA-JURID-CONTR
                                       OF WRK-AREA-CRUR408S
                                       TO WRK-CPSSOA-JURID-CONTR
           MOVE CRURW08C-S-CTPO-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO WRK-CTPO-CONTR-NEGOC
           MOVE CRURW08C-S-NSEQ-CONTR-NEGOC
                                       OF WRK-AREA-CRUR408S
                                       TO WRK-NSEQ-CONTR-NEGOC
 .    *
 .         MOVE WRK-AREA-PARAMETRO     TO ESVBEL61-E-PARAMETRO
 .    *
 .         EXEC CICS LINK
 .               PROGRAM  (WRK-ESVB1061)
 .               COMMAREA (WRK-AREA-ESVB1061)
 .               LENGTH   (LENGTH      OF WRK-AREA-ESVB1061)
 .               NOHANDLE
 .         END-EXEC
 .    *
 .         IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
 .             MOVE 16                 TO  FRWKGMOD-COD-RETORNO
 .             MOVE '0230'             TO  FRWKGMOD-COD-ERRO
 .             MOVE '2995-ACESSAR-ESVB1061'
 .                                     TO  FRWKGHEA-IDEN-PARAGRAFO
 .             PERFORM 9400-ERRO-CICS
 .         END-IF
 .    *
 .         EVALUATE ESVBEL61-COD-RETORNO
 .             WHEN 00
 .                  CONTINUE
 .             WHEN OTHER
 .                  MOVE 16            TO  FRWKGMOD-COD-RETORNO
 .                  MOVE '0240'        TO  FRWKGMOD-COD-ERRO
 .                  MOVE ESVBEL61-COD-MENSAGEM
 .                                     TO  FRWKGMOD-COD-MENSAGEM
 .                  MOVE WRK-ESVB1061  TO  FRWKGMOD-NOME-MODULO
 .                  MOVE ESVBEL61-BLOCO-RETORNO(1:14)
 .                                     TO  FRWKGMOD-BLOCO-RETORNO
 .                  MOVE '2995-ACESSAR-ESVB1061'
 .                                     TO  FRWKGHEA-IDEN-PARAGRAFO
 .                  PERFORM 9300-ERRO-MODULO
 .         END-EVALUATE
 .         .
 .    *----------------------------------------------------------------*
 .     2995-99-FIM.                    EXIT.
LL1705*----------------------------------------------------------------*
 .    *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL PELA FINALIZACAO DO PROGRAMA.               *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           IF (CRURWE11-COD-RETORNO    NOT EQUAL ZEROS)
               EXEC CICS SYNCPOINT
                    ROLLBACK
                    NOHANDLE
               END-EXEC

               IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
                   CONTINUE
               END-IF
           END-IF

           EXEC CICS
               RETURN
           END-EXEC
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR OBTER O MAIOR NUMERO DE RETORNO      *
      *----------------------------------------------------------------*
       4000-SELECIONAR-MAX-RETOR       SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  CRURB0UB
                                       REPLACING ALPHANUMERIC BY SPACES
                                                      NUMERIC BY ZEROS

           MOVE  CPSSOA-JURID-CONTR OF CRURB0U2
                                    TO CPSSOA-JURID-CONTR OF CRURB0UB
           MOVE  CTPO-CONTR-NEGOC   OF CRURB0U2
                                    TO CTPO-CONTR-NEGOC   OF CRURB0UB
           MOVE  NSEQ-CONTR-NEGOC   OF CRURB0U2
                                    TO NSEQ-CONTR-NEGOC   OF CRURB0UB
           MOVE  NENVIO-INFO-RECOR  OF CRURB0U2
                                    TO NENVIO-INFO-RECOR  OF CRURB0UB
      *
           EXEC SQL
             SELECT MAX(NLIN_MSGEM_RETOR)+1
               INTO :CRURB0UB.NLIN-MSGEM-RETOR
               FROM DB2PRD.TLIN_MSGEM_RETOR
             WHERE CPSSOA_JURID_CONTR  =  :CRURB0UB.CPSSOA-JURID-CONTR
               AND CTPO_CONTR_NEGOC    =  :CRURB0UB.CTPO-CONTR-NEGOC
               AND NSEQ_CONTR_NEGOC    =  :CRURB0UB.NSEQ-CONTR-NEGOC
               AND NENVIO_INFO_RECOR   =  :CRURB0UB.NENVIO-INFO-RECOR
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               SET  DB2-SELECT         TO TRUE
               MOVE 'TLIN_MSGEM_RETOR'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '4000-SELECIONAR-MAX-RETOR'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE '0090'             TO FRWKGDB2-LOCAL
               PERFORM 9000-ERRO-DB2
           END-IF

           IF SQLCODE                 EQUAL -305
              MOVE 1                  TO  WRK-NLIN
           ELSE
              MOVE NLIN-MSGEM-RETOR   OF CRURB0UB
                                      TO WRK-NLIN
           END-IF
           .
      *
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      * INCLIR REGISTRO TABELA TLIN_MSGEM_RETOR                        *
      *----------------------------------------------------------------*
       4100-INCLUIR-CRURB0UB           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE  CPSSOA-JURID-CONTR OF CRURB0U2
                                    TO CPSSOA-JURID-CONTR OF CRURB0UB
           MOVE  CTPO-CONTR-NEGOC   OF CRURB0U2
                                    TO CTPO-CONTR-NEGOC   OF CRURB0UB
           MOVE  NSEQ-CONTR-NEGOC   OF CRURB0U2
                                    TO NSEQ-CONTR-NEGOC   OF CRURB0UB
           MOVE  NENVIO-INFO-RECOR  OF CRURB0U2
                                    TO NENVIO-INFO-RECOR  OF CRURB0UB

           MOVE WRK-NLIN            TO NLIN-MSGEM-RETOR   OF CRURB0UB

           MOVE 1                   TO CPTCAO-TBELA       OF CRURB0UB

           MOVE 'A'                 TO CSIT-REG-RURAL     OF CRURB0UB

           MOVE WRK-TIMESTAMP       TO HINCL-REG          OF CRURB0UB

           MOVE -1                  TO WRK-B0UB-CUS-MAN-NULL
           MOVE -1                  TO WRK-B0UB-HMA-REG-NULL

           MOVE CRURWE11-TAM-SISMSG TO RLIN-MSGEM-RETOR-LEN
                                                          OF CRURB0UB.

           MOVE CRURWE11-SISMSG     TO RLIN-MSGEM-RETOR-TEXT
                                                          OF CRURB0UB.

      *
           EXEC SQL
                INSERT INTO DB2PRD.TLIN_MSGEM_RETOR
                      (CPSSOA_JURID_CONTR
                      ,CTPO_CONTR_NEGOC
                      ,NSEQ_CONTR_NEGOC
                      ,NENVIO_INFO_RECOR
                      ,NLIN_MSGEM_RETOR
                      ,CPTCAO_TBELA
                      ,CPROT_MSGEM_RETOR
                      ,CSIT_REG_RURAL
                      ,CUSUAR_INCL
                      ,HINCL_REG
                      ,CUSUAR_MANUT
                      ,HMANUT_REG
                      ,RLIN_MSGEM_RETOR)
                VALUES
                      (:CRURB0UB.CPSSOA-JURID-CONTR
                      ,:CRURB0UB.CTPO-CONTR-NEGOC
                      ,:CRURB0UB.NSEQ-CONTR-NEGOC
                      ,:CRURB0UB.NENVIO-INFO-RECOR
                      ,:CRURB0UB.NLIN-MSGEM-RETOR
                      ,:CRURB0UB.CPTCAO-TBELA
                      ,:CRURB0UB.CPROT-MSGEM-RETOR
                      ,:CRURB0UB.CSIT-REG-RURAL
                      ,:CRURB0UB.CUSUAR-INCL
                      , CURRENT TIMESTAMP
                      ,:CRURB0UB.CUSUAR-MANUT
                         :WRK-B0UB-CUS-MAN-NULL
                      ,:CRURB0UB.HMANUT-REG
                         :WRK-B0UB-HMA-REG-NULL
                      ,:CRURB0UB.RLIN-MSGEM-RETOR)
           END-EXEC
      *
           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W')
               SET  DB2-INSERT         TO TRUE
               MOVE '4100-INCLUIR-CRURB0UB'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               MOVE 'TLIN_MSGEM_RETOR'
                                       TO FRWKGDB2-NOME-TABELA
               PERFORM 9000-ERRO-DB2
           END-IF
      *
           IF (SQLCODE                 EQUAL -803)
               STRING  'REGISTRO DUPLICADO TLIN_MSGEM_RETOR'
                       DELIMITED BY SIZE INTO FRWKGLIV-PARAMETROS
               SET     ERRO-LIVRE  TO    TRUE
               MOVE '2400-UPDATE-MSG-REMSS'
                                   TO    FRWKGHEA-IDEN-PARAGRAFO
               MOVE    'CRUR1108'  TO    FRWKGLIV-COD-MENSAGEM
               MOVE 99             TO    CRURWE11-COD-RETORNO
               PERFORM 9500-ERRO-LIVRE
           END-IF
           .
      *
      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL ATUALIZAR CRURB0U2
      *----------------------------------------------------------------*
       4200-UPDATE-CRURB0U2            SECTION.
      *----------------------------------------------------------------*
      *
      *
           MOVE ZEROS                 TO WRK-B0U2-CUS-MAN-NULL

           EXEC SQL
             UPDATE   DB2PRD.TCTRL_ENVIO_INFO
                SET   CMSGEM_SIST_BACEN =  :CRURB0U2.CMSGEM-SIST-BACEN
                     ,CSIT_MSGEM_BACEN  =  :CRURB0U2.CSIT-MSGEM-BACEN
                     ,CUSUAR_MANUT      =  :CRURB0U2.CUSUAR-MANUT
                                              :WRK-B0U2-CUS-MAN-NULL
                     ,HMANUT_REG        =   CURRENT TIMESTAMP
              WHERE CPSSOA_JURID_CONTR  =  :CRURB0U2.CPSSOA-JURID-CONTR
                AND CTPO_CONTR_NEGOC    =  :CRURB0U2.CTPO-CONTR-NEGOC
                AND NSEQ_CONTR_NEGOC    =  :CRURB0U2.NSEQ-CONTR-NEGOC
                AND NENVIO_INFO_RECOR   =  :CRURB0U2.NENVIO-INFO-RECOR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-UPDATE          TO TRUE
               MOVE '0100'             TO FRWKGDB2-LOCAL
               MOVE 'TCTRL_ENVIO_INFO'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '4200-UPDATE-CRURB0U2'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *
      *----------------------------------------------------------------*
RC    * ROTINA RESPONSAVEL ATUALIZAR CRURB008
      *----------------------------------------------------------------*
RC     4300-UPDATE-CRURB008            SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE ZEROS                 TO WRK-B0U2-CUS-MAN-NULL
.          MOVE 6                     TO CSIT-OPER-RURAL   OF CRURB008
.          MOVE 'RCOR8005'            TO CUSUAR-MANUT      OF CRURB008
.          MOVE WRK-TIMESTAMP         TO HMANUT-REG        OF CRURB008
.
.          EXEC SQL
.            UPDATE   DB2PRD.TCONTR_CREDT_RURAL
.               SET   CSIT_OPER_RURAL    = :CRURB008.CSIT-OPER-RURAL
.                    ,CUSUAR_MANUT       = :CRURB008.CUSUAR-MANUT
.                                              :WRK-B0U2-CUS-MAN-NULL
.                    ,DENVIO_REG_BACEN   =  CURRENT DATE
ED2312*******RETIRAR GRAV DA DATA DGERAC_DOCTO_CONTR (DT FORMALIZACAO)
ED2312*******QUANDO PGM DE IMPRESSAO FOR CONSTRUIDO
ED2312               ,DGERAC_DOCTO_CONTR =  CURRENT DATE
ED2312*******RETIRAR GRAV DA DATA DGERAC_DOCTO_CONTR (DT FORMALIZACAO)
ED2312*******QUANDO PGM DE IMPRESSAO FOR CONSTRUIDO
.                    ,HMANUT_REG         =  CURRENT TIMESTAMP
.             WHERE CPSSOA_JURID_CONTR   = :CRURB0U2.CPSSOA-JURID-CONTR
.               AND CTPO_CONTR_NEGOC     = :CRURB0U2.CTPO-CONTR-NEGOC
.               AND NSEQ_CONTR_NEGOC     = :CRURB0U2.NSEQ-CONTR-NEGOC
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS) OR
.             (SQLWARN0                EQUAL 'W')
.              SET DB2-UPDATE          TO TRUE
.              MOVE '0200'             TO FRWKGDB2-LOCAL
.              MOVE 'TCONTR_CREDT_RURAL'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '4300-UPDATE-CRURB008'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              PERFORM 9000-ERRO-DB2
.          END-IF
.          .
.     *
.     *----------------------------------------------------------------*
RC     4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA RESPONSAVEL ATUALIZAR CRURB008 - DT EXCLUSAO - COR0002R1
      *----------------------------------------------------------------*
       4400-UPDATE-DTEXCL              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ZEROS                 TO WRK-B0U2-CUS-MAN-NULL
VANS       MOVE 5                     TO CSIT-OPER-RURAL   OF CRURB008
           MOVE 'RCOR8025'            TO CUSUAR-MANUT      OF CRURB008
           MOVE WRK-TIMESTAMP         TO HMANUT-REG        OF CRURB008

           EXEC SQL
             UPDATE   DB2PRD.TCONTR_CREDT_RURAL
VANS            SET   CSIT_OPER_RURAL    = :CRURB008.CSIT-OPER-RURAL
VANS                 ,DEXCL_CONTR_BACEN =  CURRENT DATE
                     ,CUSUAR_MANUT      =  :CRURB008.CUSUAR-MANUT
                                              :WRK-B0U2-CUS-MAN-NULL
                     ,HMANUT_REG        =   CURRENT TIMESTAMP
              WHERE CPSSOA_JURID_CONTR  =  :CRURB0U2.CPSSOA-JURID-CONTR
                AND CTPO_CONTR_NEGOC    =  :CRURB0U2.CTPO-CONTR-NEGOC
                AND NSEQ_CONTR_NEGOC    =  :CRURB0U2.NSEQ-CONTR-NEGOC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               SET DB2-UPDATE          TO TRUE
               MOVE '0220'             TO FRWKGDB2-LOCAL
               MOVE 'TCONTR_CREDT_RURAL'
                                       TO FRWKGDB2-NOME-TABELA
               MOVE '4400-UPDATE-DTEXCL'
                                       TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9000-ERRO-DB2
           END-IF
           .
      *
      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
VANS  *----------------------------------------------------------------*
VANS  *     ACESSAR FNEB3BZL LISTAR PROPOSTA DO CONTRATO (REINCIDENCIA)*
VANS  *----------------------------------------------------------------*
VANS   4410-OBTER-INFO-PROP            SECTION.
VANS  *----------------------------------------------------------------*
VANS  *
VANS       INITIALIZE                  FNEBWBZI-BLOCO-ENTRADA
VANS                                   FNEBWBZI-BLOCO-PAGINACAO
VANS                                   FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB3BZL
VANS  *
VANS       MOVE 1                      TO FNEBWBZI-E-MAX-OCCOR
VANS  *
VANS       MOVE 'I'                    TO FNEBWBZI-INDICADOR-PAGINACAO
VANS  *
VANS       MOVE CPSSOA-JURID-CONTR     OF CRURB0U2
VANS                                   TO WRK-S9-10
VANS       MOVE WRK-9-10               TO FNEBWBZI-E-CPSSOA-JURID-VINC
VANS                                   OF WRK-AREA-FNEB3BZL
VANS  *
VANS       MOVE CTPO-CONTR-NEGOC       OF CRURB0U2
VANS                                   TO WRK-S9-03
VANS       MOVE WRK-9-03               TO FNEBWBZI-E-CTPO-CONTR-VINC
VANS                                   OF WRK-AREA-FNEB3BZL
VANS  *
VANS       MOVE NSEQ-CONTR-NEGOC       OF CRURB0U2
VANS                                   TO WRK-S9-10
VANS       MOVE WRK-9-10               TO FNEBWBZI-E-NSEQ-CONTR-VINC
VANS                                   OF WRK-AREA-FNEB3BZL
VANS  *
VANS       EXEC CICS LINK
VANS            PROGRAM  (WRK-FNEB3BZL)
VANS            COMMAREA (WRK-AREA-FNEB3BZL)
VANS            LENGTH   (LENGTH       OF WRK-AREA-FNEB3BZL)
VANS            NOHANDLE
VANS       END-EXEC
VANS  *
VANS       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
VANS           MOVE '4410-OBTER-INFO-PROP'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           PERFORM 9400-ERRO-CICS
VANS       END-IF
VANS  *
VANS       EVALUATE FNEBW000-COD-RETORNO
VANS                                   OF WRK-AREA-FNEB3BZL
VANS           WHEN 00
VANS           WHEN 01
VANS                CONTINUE
VANS           WHEN OTHER
VANS                MOVE WRK-FNEB3BZL  TO FRWKGMOD-NOME-MODULO
VANS                MOVE FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB3BZL
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                MOVE '4410-OBTER-INFO-PROP'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                PERFORM 9300-ERRO-MODULO
VANS       END-EVALUATE
VANS       .
VANS  *
VANS  *----------------------------------------------------------------*
VANS   4410-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *
VANS  *----------------------------------------------------------------*
VANS  *    ROTINA RESPONSAVEL POR RERVERTER RESERVA NAS BASES DE DOTACA*
VANS  *----------------------------------------------------------------*
VANS   4420-REVERT-RESERV-DOTAC        SECTION.
VANS  *----------------------------------------------------------------*
VANS  *                                                                *
VANS       INITIALIZE                  CRURWXGI-REGISTRO
VANS                                   CRURW00W-BLOCO-RETORNO
VANS                                   OF WRK-AREA-CRUR3XGO
VANS  *                                                                *
VANS       MOVE CPSSOA-JURID-CONTR     OF CRURB0U2
VANS                                   TO WRK-S9-10
VANS       MOVE WRK-9-10               TO CRURWXGI-E-CPSSOA-JURID-CONTR
VANS                                   OF WRK-AREA-CRUR3XGO
VANS  *
VANS       MOVE CTPO-CONTR-NEGOC       OF CRURB0U2
VANS                                   TO WRK-S9-03
VANS       MOVE WRK-9-03               TO CRURWXGI-E-CTPO-CONTR-NEGOC
VANS                                   OF WRK-AREA-CRUR3XGO
VANS  *
VANS       MOVE NSEQ-CONTR-NEGOC       OF CRURB0U2
VANS                                   TO WRK-S9-10
VANS       MOVE WRK-9-10               TO CRURWXGI-E-NSEQ-CONTR-NEGOC
VANS                                   OF WRK-AREA-CRUR3XGO
VANS  *
VANS       EXEC CICS LINK
VANS            PROGRAM  (WRK-CRUR3XGO)
VANS            COMMAREA (WRK-AREA-CRUR3XGO)
VANS            LENGTH   (LENGTH       OF WRK-AREA-CRUR3XGO)
VANS            NOHANDLE
VANS       END-EXEC
VANS  *                                                                *
VANS       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
VANS           MOVE '4420-REVERT-RESERV-DOTAC '
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           PERFORM 9400-ERRO-CICS
VANS       END-IF
VANS  *                                                                *
VANS       EVALUATE CRURW00W-COD-RETORNO
VANS                                   OF WRK-AREA-CRUR3XGO
VANS           WHEN 00
VANS                CONTINUE
VANS  *                                                                *
VANS           WHEN OTHER
VANS                MOVE WRK-CRUR3XGO  TO FRWKGMOD-NOME-MODULO
VANS                MOVE CRURW00W-BLOCO-RETORNO
VANS                                   OF WRK-AREA-CRUR3XGO
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                MOVE '4420-REVERT-RESERV-DOTAC'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                PERFORM 9300-ERRO-MODULO
VANS       END-EVALUATE
VANS       .
VANS  *                                                                *
VANS  *----------------------------------------------------------------*
VANS   4420-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *                                                                *
VANS  *----------------------------------------------------------------*
VANS  *     ACESSAR FNEB4O2O OBTER SITUACAO DA PROPOSTA                *
VANS  *----------------------------------------------------------------*
VANS   4430-OBTER-SITUACAO-PROPOSTA    SECTION.
VANS  *----------------------------------------------------------------*
VANS  *
VANS       INITIALIZE                  FNEBWI9C-BLOCO-ENTRADA
VANS                                   FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB4O2O
VANS  *
VANS       MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
VANS                                   TO FNEBWI9C-E-CPSSOA-JURID-CONTR
VANS  *
VANS       MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
VANS                                   TO FNEBWI9C-E-CTPO-CONTR-NEGOC
VANS  *
VANS       MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
VANS                                   TO FNEBWI9C-E-NSEQ-CONTR-NEGOC
VANS  *
VANS       EXEC CICS LINK
VANS            PROGRAM  (WRK-FNEB4O2O)
VANS            COMMAREA (WRK-AREA-FNEB4O2O)
VANS            LENGTH   (LENGTH       OF WRK-AREA-FNEB4O2O)
VANS            NOHANDLE
VANS       END-EXEC
VANS  *
VANS       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
VANS           MOVE '4430-OBTER-SITUACAO-PROPOSTA'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           PERFORM 9400-ERRO-CICS
VANS       END-IF
VANS  *
VANS       EVALUATE FNEBW000-COD-RETORNO
VANS                                   OF WRK-AREA-FNEB4O2O
VANS           WHEN 00
VANS           WHEN 01
VANS                CONTINUE
VANS           WHEN OTHER
VANS                MOVE WRK-FNEB4O2O  TO FRWKGMOD-NOME-MODULO
VANS                MOVE FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB4O2O
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                MOVE '4430-OBTER-SITUACAO-PROPOSTA'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                PERFORM 9300-ERRO-MODULO
VANS       END-EVALUATE
VANS       .
VANS  *
VANS  *----------------------------------------------------------------*
VANS   4430-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *
VANS  *----------------------------------------------------------------*
VANS  *    ROTINA RESPONSAVEL POR ATUALIZAR STATUS PRPSTA ELEMENTO FNEB*
VANS  *----------------------------------------------------------------*
VANS   4440-ACESSAR-FNEB3YBA           SECTION.
VANS  *----------------------------------------------------------------*
VANS  *                                                                *
VANS       INITIALIZE                  FNEBWYBI-REGISTRO
VANS                                   FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB3YBA
VANS                                   REPLACING ALPHANUMERIC BY SPACES
VANS                                                  NUMERIC BY ZEROS
VANS  *                                                                *
VANS       MOVE CPSSOA-JURID-CONTR     OF  CRURB008
VANS                                   TO  FNEBWYBI-E-CPSSOA-CONTR-VINC
VANS  *                                                                *
VANS       MOVE CTPO-CONTR-NEGOC       OF  CRURB008
VANS                                   TO  FNEBWYBI-E-CTPO-CONTR-VINC
VANS  *                                                                *
VANS       MOVE NSEQ-CONTR-NEGOC       OF  CRURB008
VANS                                   TO  FNEBWYBI-E-NSEQ-CONTR-VINC
VANS  *                                                                *
VANS       MOVE 2
VANS                                   TO  FNEBWYBI-E-IDEN-TIPO-COMN
VANS  *                                                                *
VANS       MOVE WRK-TIMESTAMP          TO  FNEBWYBI-E-DINIC-PPSTA-NEGOC
VANS  *                                                                *
VANS       MOVE VCTBIL-CONTR-CREDT     OF  CRURB0LJ
VANS                                   TO  FNEBWYBI-E-VALO-PPSTA-NEGOC
VANS  *                                                                *
VANS       EXEC CICS LINK
VANS            PROGRAM  (WRK-FNEB3YBA)
VANS            COMMAREA (WRK-AREA-FNEB3YBA)
VANS            LENGTH   (LENGTH       OF WRK-AREA-FNEB3YBA)
VANS            NOHANDLE
VANS       END-EXEC
VANS  *                                                                *
VANS       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
VANS           MOVE '4440-ACESSAR-FNEB3YBA'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           PERFORM 9400-ERRO-CICS
VANS       END-IF
VANS  *                                                                *
VANS       EVALUATE FNEBW000-COD-RETORNO
VANS                                   OF WRK-AREA-FNEB3YBA
VANS           WHEN 00
VANS                CONTINUE
VANS  *                                                                *
VANS           WHEN OTHER
VANS
VANS                MOVE WRK-FNEB3YBA
VANS                                   TO FRWKGMOD-NOME-MODULO
VANS                MOVE FNEBW000-BLOCO-RETORNO
VANS                                   OF WRK-AREA-FNEB3YBA
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                MOVE '4440-ACESSAR-FNEB3YBA'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                PERFORM 9300-ERRO-MODULO
VANS
VANS       END-EVALUATE
VANS       .
VANS  *                                                                *
VANS  *----------------------------------------------------------------*
VANS   4440-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *                                                                *
VANS  *----------------------------------------------------------------*
VANS  *    ROTINA RESPONSAVEL POR ATUALIZAR A BASE CFUR REGST REJEITADO*
VANS  *----------------------------------------------------------------*
VANS   4450-CANC-RESEV-PROP-CFUR       SECTION.
VANS  *----------------------------------------------------------------*
VANS  *                                                                *
VANS       INITIALIZE                  CFURWP6I-REGISTRO
VANS                                   CFURW00W-BLOCO-RETORNO
VANS                                   OF WRK-AREA-CFUR3P6M
VANS  *                                                                *
VANS       MOVE 'CRUR'                 TO CFURWP6I-E-CSIST
VANS  *                                                                *
VANS       MOVE CPSSOA-JURID-CONTR     OF CRURB008
VANS                                   TO CFURWP6I-E-CPSSOA-DETNT-REC
VANS  *                                                                *
VANS       MOVE CRURWXGI-S-CREC-FINCR-FINAN
VANS                                   OF WRK-AREA-CRUR3XGO
VANS                                   TO CFURWP6I-E-CREC-FINCR-FINAN
VANS  *                                                                *
VANS       MOVE FNEBWBZI-S-CPSSOA-PPSTA-NEGOC(1)
VANS                                   TO CFURWP6I-E-CPSSOA-JURID-PPSTA
VANS  *                                                                *
VANS       MOVE FNEBWBZI-S-CCLASF-PPSTA-NEGOC(1)
VANS                                   TO CFURWP6I-E-CTPO-PPSTA-NEGOC
VANS  *                                                                *
VANS       MOVE FNEBWBZI-S-NSEQ-PPSTA-NEGOC(1)
VANS                                   TO CFURWP6I-E-NSEQ-PPSTA-NEGOC
VANS  *                                                                *
VANS       MOVE VCTBIL-CONTR-CREDT     OF CRURB0LJ
VANS                                   TO CFURWP6I-E-VRESER-LIM-REC
VANS  *                                                                *
VANS       MOVE WRK-DATE-ATUAL         TO CFURWP6I-E-DRESER-LIM-REC
VANS  *                                                                *
VANS       MOVE 'PROPOSTA REJEITADA PELO USUARIO'
VANS                                   TO CFURWP6I-E-RMOTVO-ATULZ-REG
VANS  *                                                                *
VANS       MOVE 'RCOR8025'             TO CFURWP6I-E-CUSUAR
VANS  *                                                                *
VANS       EXEC CICS LINK
VANS            PROGRAM  (WRK-CFUR3P6M)
VANS            COMMAREA (WRK-AREA-CFUR3P6M)
VANS            LENGTH   (LENGTH       OF WRK-AREA-CFUR3P6M)
VANS            NOHANDLE
VANS       END-EXEC
VANS  *                                                                *
VANS       IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
VANS           MOVE '4450-CANC-RESEV-PROP-CFUR'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS           PERFORM 9400-ERRO-CICS
VANS       END-IF
VANS  *                                                                *
VANS       EVALUATE CFURW00W-COD-RETORNO
VANS                                   OF WRK-AREA-CFUR3P6M
VANS           WHEN 00
VANS                CONTINUE
VANS  *                                                                *
VANS           WHEN 08
VANS                IF CFURW00W-COD-MENSAGEM
VANS                                   OF WRK-AREA-CFUR3P6M
VANS                                   EQUAL 'CFUR0017'
VANS                    MOVE 00        TO CFURW00W-COD-RETORNO
VANS                                   OF WRK-AREA-CFUR3P6M
VANS                ELSE
VANS                    MOVE WRK-CFUR3P6M
VANS                                   TO FRWKGMOD-NOME-MODULO
VANS                    MOVE CFURW00W-BLOCO-RETORNO
VANS                                   OF WRK-AREA-CFUR3P6M
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                    MOVE '4450-CANC-RESEV-PROP-CFUR'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                    PERFORM 9300-ERRO-MODULO
VANS                END-IF
VANS  *
VANS           WHEN OTHER
VANS                MOVE WRK-CFUR3P6M  TO FRWKGMOD-NOME-MODULO
VANS                MOVE CFURW00W-BLOCO-RETORNO
VANS                                   OF WRK-AREA-CFUR3P6M
VANS                                   TO FRWKGMOD-BLOCO-RETORNO
VANS                MOVE '4450-CANC-RESEV-PROP-CFUR'
VANS                                   TO FRWKGHEA-IDEN-PARAGRAFO
VANS                PERFORM 9300-ERRO-MODULO
VANS       END-EVALUATE
VANS       .
VANS  *                                                                *
VANS  *----------------------------------------------------------------*
VANS   4450-99-FIM.                    EXIT.
VANS  *----------------------------------------------------------------*
VANS  *
COR6  *----------------------------------------------------------------*
COR6   4500-ACESSAR-CALE1000                SECTION.
COR6  *----------------------------------------------------------------*
COR6
COR6       MOVE  '4500-ACESSAR-CALE1000'    TO  FRWKGHEA-IDEN-PARAGRAFO
COR6
COR6       INITIALIZE  CALE01-SAIDA
COR6                   CALE01-RETORNO
COR6
COR6       MOVE  'CALE0001'                 TO  CALE01-ID-BLOCO
COR6       MOVE  1182                       TO  CALE01-TAM-BLOCO
COR6
COR6       EXEC CICS LINK
COR6            PROGRAM  (WRK-CALE1000)
COR6            COMMAREA (CALE01-REGISTRO)
COR6            LENGTH   (LENGTH OF CALE01-REGISTRO)
COR6       END-EXEC
COR6
COR6       IF EIBRESP                       NOT EQUAL DFHRESP(NORMAL)
COR6          MOVE   '0009'                 TO  FRWKGMOD-COD-ERRO
COR6          PERFORM 9400-ERRO-CICS
COR6       END-IF
COR6
COR6       IF CALE01-COD-RETORNO            NOT EQUAL ZEROS
COR6          MOVE CALE01-COD-RETORNO       TO CRURWE11-COD-RETORNO
COR6                                           FRWKGMOD-COD-RETORNO
COR6          MOVE CALE01-COD-ERRO          TO FRWKGMOD-COD-ERRO
COR6          MOVE CALE01-COD-MENSAGEM-GMSG
COR6                                        TO FRWKGMOD-COD-MENSAGEM
COR6          MOVE WRK-CALE1000             TO FRWKGMOD-NOME-MODULO
COR6          MOVE '4500-ACESSAR-CALE1000'
COR6                                        TO FRWKGHEA-IDEN-PARAGRAFO
COR6          PERFORM 9300-ERRO-MODULO
COR6       END-IF.
COR6
COR6  *----------------------------------------------------------------*
COR6   4500-99-FIM.                         EXIT.
COR6  *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO DB2.                         *
      *----------------------------------------------------------------*
       9000-ERRO-DB2                   SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-DB2               TO TRUE
           MOVE 99                     TO CRURWE11-COD-RETORNO
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGDB2-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE SQLSTATE               TO FRWKGDB2-SQLSTATE
           MOVE SQLCA                  TO FRWKGDB2-SQLCA
           MOVE SQLCODE                TO FRWKGDB2-SQLCODE
           MOVE WRK-COPY-DB2           TO WRK-BLOCO-INFO-ERRO

      *
           PERFORM 9999-API-ERROS
           .
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
COR6  *----------------------------------------------------------------*
COR6   9200-ERRO-LIVRE                 SECTION.
COR6  *----------------------------------------------------------------*
COR6  *                                                                *
COR6       SET  ERRO-LIVRE             TO TRUE
COR6  *
COR6       MOVE 'CRUR1O11'             TO FRWKGHEA-NOME-PROGRAMA
COR6       MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
COR6       MOVE 'CRUR1421'             TO FRWKGLIV-COD-MENSAGEM
COR6       MOVE 1                      TO FRWKGLIV-IDIOMA
COR6       MOVE WRK-AREA-ERRO-LIVRE
COR6                         (1:LENGTH OF WRK-AREA-ERRO-LIVRE)
COR6                                   TO WRK-BLOCO-INFO-ERRO
COR6  *                                                                *
COR6       EXEC CICS LINK
COR6           PROGRAM  (WRK-FRWK1999)
COR6           COMMAREA (WRK-AREA-ERRO)
COR6           LENGTH   (LENGTH OF WRK-AREA-ERRO)
COR6           NOHANDLE
COR6       END-EXEC
COR6  *
COR6       IF (EIBRESP NOT EQUAL DFHRESP(NORMAL))
COR6           CONTINUE
COR6       END-IF
COR6       .
COR6  *----------------------------------------------------------------*
COR6   9200-99-FIM.                    EXIT.
COR6  *----------------------------------------------------------------*
COR6  *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO MODULO                       *
      *----------------------------------------------------------------*
       9300-ERRO-MODULO                SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-MODULO            TO TRUE
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TRATAMENTO DE ERRO CICS.                        *
      *----------------------------------------------------------------*
       9400-ERRO-CICS                  SECTION.
      *----------------------------------------------------------------*
      *
           SET  ERRO-CICS              TO TRUE
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGCIC-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE EIBFN                  TO FRWKGCIC-EIBFN
           MOVE EIBRCODE               TO FRWKGCIC-EIBRCODE
           MOVE EIBRSRCE               TO FRWKGCIC-EIBRSRCE
           MOVE EIBRESP                TO FRWKGCIC-EIBRESP
           MOVE EIBRESP2               TO FRWKGCIC-EIBRESP2
           MOVE EIBTASKN               TO FRWKGCIC-EIBTASKN
           MOVE WRK-COPY-CICS          TO WRK-BLOCO-INFO-ERRO
      *
           PERFORM 9999-API-ERROS.
      *
      *----------------------------------------------------------------*
       9400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      * ROTINA PARA TRATAMENTO DE ERRO LIVRE.                          *
      *----------------------------------------------------------------*
       9500-ERRO-LIVRE                 SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA
           MOVE FRWKGLIV-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS
           MOVE 1                      TO FRWKGLIV-IDIOMA
           MOVE WRK-AREA-ERRO-LIVRE    TO WRK-BLOCO-INFO-ERRO
                                          (1:FRWKGHEA-TAM-DADOS)

           PERFORM 9999-API-ERROS
           .
      *
      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
TESTE *----------------------------------------------------------------*
TESTE  9600-FORMATAR-ERRO-MODULO       SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      MOVE 'VRS001'               TO ERR-VERSAO.
TESTE      MOVE ZEROS                  TO ERR-COD-MSG.
TESTE
TESTE      MOVE WRK-TEXTO-ERRO         TO ERR-TEXTO-MSG
TESTE      MOVE ZEROS                  TO ERR-RETURN-CODE.
TESTE      MOVE 'APL'                  TO ERR-TIPO-ACESSO.
TESTE      MOVE 'CRUR1O11'             TO ERR-PGM.
TESTE      MOVE SPACES                 TO ERR-TEXTO.
TESTE      MOVE WRK-PROGRAMA           TO ERR-MODULO.
TESTE      MOVE DFHEIBLK               TO ERR-DFHEIBLK.
TESTE
TESTE      EXEC CICS ASSIGN
TESTE                APPLID(ERR-CICS)
TESTE      END-EXEC.
TESTE
TESTE      PERFORM 9610-GRAVAR-LOG-CDES.
TESTE
TESTE *----------------------------------------------------------------*
TESTE  9600-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------*
TESTE *----------------------------------------------------------------*
TESTE  9610-GRAVAR-LOG-CDES            SECTION.
TESTE *----------------------------------------------------------------*
TESTE
TESTE      EXEC CICS START
TESTE          TRANSID('CD11')
TESTE          FROM   (ERRO-AREA)
TESTE          LENGTH (+520)
TESTE          NOHANDLE
TESTE      END-EXEC.
TESTE
TESTE      IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
TESTE          CONTINUE
TESTE      END-IF.
TESTE
TESTE *----------------------------------------------------------------*
TESTE  9610-99-FIM.                    EXIT.
TESTE *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * ROTINA PARA CHAMADA DA API DE ERROS                            *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*
      *
           EXEC CICS LINK
               PROGRAM (WRK-FRWK1999)
               COMMAREA (WRK-AREA-ERRO)
               LENGTH (LENGTH OF WRK-AREA-ERRO)
               NOHANDLE
           END-EXEC

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF

      ***  DESVIA DO FINALIZAR EM LOGS DE CONTROLE
           IF  FRWKGLIV-COD-MENSAGEM   NOT EQUAL 'CRUR1421'
               PERFORM 3000-FINALIZAR
           END-IF
           .
      *
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
