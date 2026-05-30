      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1172.
       AUTHOR.     ANDRE LUIZ LONGO.
      *================================================================*
      *                     C P M - S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  GFCT1172                                     *
      *    PROGRAMADOR.:  ANDRE LUIZ LONGO         -  CPM PATO BRANCO  *
      *    ANALISTA CPM:  ELIAS AUGUSTO BOSCATO    -  CPM PATO BRANCO  *
      *    ANALISTA....:  WAGNER SILVA             -  PROCWORK / GP 50 *
      *    DATA........:  01/02/2006                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  OBTER DADOS DA TARIFA.                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME                      INCLUDE/BOOK        *
      *                    ENTCOBRA                 I#GFCTIL           *
      *                    SAICOBRA                 I#GFCTIL           *
      *                    ARCOCORR                 I#GFCTIL           *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                     INCLUDE/BOOK        *
      *                    DB2PRD.HIST_PARM_TARIF    GFCTB086          *
      *                    DB2PRD.TASSOC_AGPTO_CTBIL GFCTB0I8          *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    POL7100C - AREA PARA TRATAMENTO DE ERRO PELA POOL7100       *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL7100 - TRATAMENTO DE ERROS                              *
      *================================================================*
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *    ANALISTA....:   WAGNER SILVA            - PROCKWORK GP.50   *
      *    DATA........:   10/02/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ACERTO LOGICO                               *
      *================================================================*
      *                        A L T E R A C A O                       *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR.:  FERNANDO NAIM SCHMITZ    - CPM/FPOLIS        *
      *    ANALISTA CPM:  MARCIO RODRIGO DA CUNHA  - CPM/FPOLIS        *
      *    ANALISTA....:  VAGNER SILVA - PROCWORK  - GRUPO 50          *
      *    DATA........:  14/12/2006                                   *
      *                                                                *
      *    OBJETIVO.....: INCLUIR PESQUISA DE AGRUPAMENTO CONTABIL.    *
      *                                                                *
      *================================================================*
122010*                        A L T E R A C A O                       *
122010*================================================================*
122010*                                                                *
122010*    PROGRAMADOR.:   FABRICA          -  SONDA PROCWORK          *
122010*    ANALISTA....:   ANTONIO PAGNOCCA - SONDA PROCWORK           *
122010*    DATA........:   DEZ/2010                                    *
122010*    OBJETIVO....:   DISTRIBUICAO DAS PARCELAS NO ENVIO DA CO-   *
122010*                    BRANCA E ATUALIZACAO DA PARCELA PARA 13 -   *
122010*                    ENVIADA.                                    *
122010*                                                                *
BI0411*----------------------------------------------------------------*
BI0411*                 U L T I M A   A L T E R A C A O                *
BI0411*----------------------------------------------------------------*
BI0411*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0411       *
BI0411*----------------------------------------------------------------*
BI0411*                                                                *
BI0411*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0411*    DATA........:  04 / 2011                                    *
BI0411*    OBJETIVO....:  TRATAR OS CAMPOS NOVOS CINDCD_REPAS_CLULR ,  *
BI0411*                                          VREPAS_CREDT_CLULR E  *
BI0411*                                          QDIA_RTCAO_CREDT      *
BI0411*                   DAS TABELAS GFCTB086 E GFCTB0A2.             *
BI0411*                                                                *
ESC   *----------------------------------------------------------------*
ESC   *                 U L T I M A   A L T E R A C A O                *
ESC   *----------------------------------------------------------------*
ESC   *        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0411       *
ESC   *----------------------------------------------------------------*
ESC   *                                                                *
ESC   *    ANALISTA....:  ESCOBAR         - WIPRO                      *
ESC   *    DATA........:  08 / 2018                                    *
ESC   *    OBJETIVO....:  ALTERA APURACAO DO CAMPO                     *
ESC   *                   WRK-COBR-VALOR-DEBITAR                       *
ESC   *                                                                *
ESC   *================================================================*
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

           SELECT ENTCOBRA ASSIGN      TO UT-S-ENTCOBRA
           FILE STATUS                 IS WRK-FS-ENTCOBRA.

           SELECT SAICOBRA  ASSIGN     TO UT-S-SAICOBRA
           FILE STATUS                 IS WRK-FS-SAICOBRA.

           SELECT ARCOCORR  ASSIGN     TO UT-S-ARCOCORR
           FILE STATUS                 IS WRK-FS-ARCOCORR.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE EVENTOS A COBRAR GFCTB092                *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ENTCOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-ENTCOBRA                PIC  X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE EVENTOS A COBRAR MORA                    *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  SAICOBRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-SAICOBRA                PIC  X(300).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE OCORRENCIAS NO PROCESSAMENTO             *
      *            ORG. SEQUENCIAL     -   LRECL   =   300             *
      *----------------------------------------------------------------*

       FD  ARCOCORR
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-ARCOCORR                PIC  X(300).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ACUMULADORES ***'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ENTCOBRA          PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-SAICOBRA       PIC  9(009) COMP-3  VALUE ZEROS.
ESC    77  ACU-DESPREZA-SAICOBRA       PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS-ARCOCORR       PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.
       77  WRK-SQLCODE                 PIC  X(009)         VALUE SPACES.
       77  WRK-ERRO                    PIC  X(001)         VALUE SPACES.
       77  WRK-AGRUPAMENTO             PIC S9(5)           VALUE ZEROS.
122010 77  WRK-FLAG                    PIC  X(001)         VALUE SPACES.
122010 77  WRK-PARC-I                  PIC S9(003) COMP-3  VALUE ZEROS.
122010 77  WRK-PARC-F                  PIC S9(003) COMP-3  VALUE ZEROS.
ESC    77  WRK-COBR-VALOR-DEB-CS       PIC S9(11)V9(2)
ESC                                        COMP-3          VALUE ZEROS.

       01  WRK-DATA-AUX.
           03  WRK-DIA-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-AUX             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-AUX             PIC  9(004)         VALUE ZEROS.

       01  WRK-RETORNO.
           03 WRK-JOB                  PIC X(08)      VALUE 'GFCT0614'.
           03 WRK-PROGRAMA             PIC X(08)      VALUE 'GFCT1172'.
           03 WRK-RECURSO              PIC X(08)      VALUE SPACES.
           03 WRK-COD-RETORNO          PIC X(03)      VALUE SPACES.
           03 WRK-VALOR-DEBITADO       PIC 9(11)V9(2) VALUE ZEROS.
           03 WRK-DESCRICAO            PIC X(50)      VALUE SPACES.

       01  WRK-COD-RET-3.
           03 WRK-FILLER             PIC X(01)         VALUE '0'.
           03 WRK-COD-RET-2          PIC X(02)         VALUE SPACES.

122010 01  WRK-DATA-DB2.
122010     05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
122010     05  WRK-PT1-DB2             PIC  X(001)         VALUE '.'.
122010     05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
122010     05  WRK-PT2-DB2             PIC  X(001)         VALUE '.'.
122010     05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.
122010
122010 01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
122010 01  FILLER   REDEFINES    WRK-DATA-AMD.
122010     05  WRK-ANO-AMD             PIC  9(004).
122010     05  WRK-MES-AMD             PIC  9(002).
122010     05  WRK-DIA-AMD             PIC  9(002).
122010
122010 01  WRK-TAM-03-COM-S-E          PIC -9(003)         VALUE ZEROS.
122010 01  FILLER REDEFINES WRK-TAM-03-COM-S-E.
122010     05  FILLER                  PIC  X(001).
122010     05  WRK-TAM-03-SEM-S        PIC  9(003).
122010
122010 01  WRK-TAM-05-COM-S            PIC S9(005)         VALUE ZEROS.
122010
122010 01  WRK-TAM-09-COM-S            PIC S9(009)         VALUE ZEROS.
122010 01  FILLER REDEFINES WRK-TAM-09-COM-S.
122010     05  FILLER                  PIC  X(006).
122010     05  WRK-TAM-03-COM-S        PIC S9(003).
122010
122010 01  WRK-TAM-09-V99-COM-S        PIC -9(009)V99      VALUE ZEROS.
122010 01  FILLER REDEFINES WRK-TAM-09-V99-COM-S.
122010     05  FILLER                  PIC  X(001).
122010     05  WRK-TAM-09-V99-SEM-S    PIC  9(009)V99.
122010
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTE DE FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FS-ENTCOBRA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-SAICOBRA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARCOCORR             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA '.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA  '.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGEM DE ERRO ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-POOL7100.
           03  FILLER                  PIC  X(008)         VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(013)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(017)         VALUE
               ' - FILE STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(003)         VALUE ' **'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CHAVES ***'.
      *----------------------------------------------------------------*

       01  WRK-CHAVE-ATU.
           05  WRK-CHV-TARIFA          PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-DATA            PIC  9(008)         VALUE ZEROS.

       01  WRK-CHAVE-ANT.
           05  WRK-CHV-TARIFA-ANT      PIC  9(005)         VALUE ZEROS.
           05  WRK-CHV-DATA-ANT        PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM MODULO GFCT5529 ***'.
      *----------------------------------------------------------------*

       01  WRK-5529-AREA-ENTRADA.
           03  WRK-5529-AMBIENTE                   PIC  X(001).
           03  WRK-5529-CHAMADOR                   PIC  X(008).
           03  WRK-5529-TARIFA                     PIC  9(005).
           03  WRK-5529-DATA                       PIC  X(010).

       01  WRK-5529-AREA-SAIDA.
           03  WRK-5529-COD-RETORNO                PIC 9(002).
      *         00 - PROCESSAMENTO OK
      *         01 - AMBIENTE DIFERENTE DE 'B' E 'O'
      *         02 - TARIFA NAO CADASTRADA
      *         03 - PARAMETROS DE PESQUISA INVALIDOS
      *         04 - PROGRAMA CHAMADOR NAO INFORMADO
      *         05 - TARIFA NAO VIGENTE NA DATA INFORMADA
      *         99 - ERRO DB2
           03  WRK-5529-COD-SQL-ERRO               PIC S9(003).
           03  WRK-5529-COD-MSG-GFCT               PIC  9(004).
           03  WRK-5529-DESC-MSG                   PIC  X(070).
           03  WRK-5529-QTD-OCORR                  PIC  9(005).
           03  WRK-5529-DADOS-RETORNO.
               05  WRK-5529-CSERVC-TARIF           PIC  9(005).
               05  WRK-5529-DATA-VGCIA-TARIF       PIC  X(010).
               05  WRK-5529-CIDTFD-LCTO            PIC  9(005).
               05  WRK-5529-CINDCD-DEB-PCIAL       PIC  X(001).
               05  WRK-5529-CINDCD-PCIAL-ULT       PIC  X(001).
               05  WRK-5529-CTPO-CTA-DEB-TARIF     PIC  9(001).
               05  WRK-5529-CCART-DEB-TARIF        PIC  9(005).
               05  WRK-5529-RSEGDA-LIN-EXTRT       PIC  X(032).
               05  WRK-5529-VMIN-ENVIO-COBR        PIC  9(011)V99.
122010         05  WRK-5529-CINDCD-PCMTO-COBR      PIC  X(001)
122010                                                     VALUE SPACES.
122010         05  WRK-5529-QPCELA-COBR-TARIF      PIC  9(002)
122010                                                     VALUE ZEROS.
122010         05  WRK-5529-CIDTFD-COBR-PCELA      PIC  X(001)
122010                                                     VALUE SPACES.
122010         05  WRK-5529-QDIA-COBR-PCELA        PIC  9(003)
122010                                                     VALUE ZEROS.
122010         05  WRK-5529-CINDCD-CREDT-CLULR     PIC  X(001)
122010                                                     VALUE SPACES.
BI0411         05  WRK-5529-CINDCD-REPAS-CLULR     PIC  9(001)
BI0411                                                     VALUE ZEROS.
BI0411         05  WRK-5529-VREPAS-CREDT-CLULR     PIC S9(009)V9(002)
BI0411                                                     VALUE ZEROS.
BI0411         05  WRK-5529-QDIA-RTCAO-CREDT       PIC  9(003)
BI0411                                                     VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-LTERM                        PIC  X(008).
           03  FILLER                              PIC  X(002).
           03  WRK-IO-STATUS                       PIC  X(002).
           03  FILLER                              PIC  X(012).
           03  WRK-IO-MODNAME                      PIC  X(008).

       01  WRK-ALT-PCB.
           03  WRK-ALT-LTERM                       PIC  X(008).
           03  FILLER                              PIC  X(002).
           03  WRK-ALT-STATUS                      PIC  X(002).
           03  FILLER                              PIC  X(012).
           03  WRK-ALT-MODNAME                     PIC  X(008).

       COPY I#GFCT0M.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INCLUDE I#GFCTIL ***'.
      *----------------------------------------------------------------*

       COPY I#GFCTIL.


122010*----------------------------------------------------------------*
122010 01  FILLER                      PIC  X(050)         VALUE
122010     '*** AREA COPIA DA INCLUDE I#GFCTIL   ***'.
122010*----------------------------------------------------------------*
122010 01 WRK-REG-SAICOBRA.
122010    05 WRK-COBR-CHAVE-COBRANCA.
122010       10 WRK-COBR-DATA-ENVIO         PIC 9(08)      VALUE ZEROS.
122010       10 WRK-COBR-DIG-CONTRATO       PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-NUM-CONTRATO       PIC 9(07)      VALUE ZEROS.
122010    05 WRK-COBR-CHAVE-ORIGEM.
122010       10 WRK-COBR-DATA-ORIG          PIC X(10)      VALUE SPACES.
122010       10 WRK-COBR-ROTINA-ORIG        PIC X(04)      VALUE SPACES.
122010       10 WRK-COBR-NUM-ARQ-ORIG       PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-SEQ-ARQ-ORIG       PIC 9(11)      VALUE ZEROS.
122010       10 WRK-COBR-TARIFA-ORIG        PIC 9(05)      VALUE ZEROS.
122010    05 WRK-COBR-CGC-CPF.
ST25X6*      10 WRK-COBR-CGC-CPF-MUNERO     PIC 9(09)      VALUE ZEROS.
ST25X6       10 WRK-COBR-CGC-CPF-MUNERO     PIC X(009)     VALUE SPACES.
ST25X6*      10 WRK-COBR-CGC-CPF-FILIAL     PIC 9(05)      VALUE ZEROS.
ST25X6       10 WRK-COBR-CGC-CPF-FILIAL     PIC X(004)     VALUE SPACES.
ST25X6*      10 WRK-COBR-CGC-CPF-CONTROLE   PIC 9(02)      VALUE ZEROS.
ST25X6       10 WRK-COBR-CGC-CPF-CONTROLE   PIC X(002)     VALUE SPACES.
122010    05 WRK-COBR-DADOS-DEBITO.
122010       10 WRK-COBR-BANCO-DEB          PIC 9(03)      VALUE ZEROS.
122010       10 WRK-COBR-AGENCIA-DEB        PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-CONTA-DEB          PIC 9(07)      VALUE ZEROS.
122010       10 WRK-COBR-DIGITO-DEB         PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-CARTEIRA-DEB       PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-COD-LCTO-DEB       PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-COD-MOEDA-DEB      PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-RAZAO.
122010          15 WRK-COBR-GRUPO-DEB       PIC 9(03)      VALUE ZEROS.
122010          15 WRK-COBR-SUBGRUPO-DEB    PIC 9(03)      VALUE ZEROS.
122010       10 WRK-COBR-VALOR-DEBITAR      PIC 9(11)V9(2) VALUE ZEROS.
122010    05 WRK-COBR-DETALHES-COBRANCA.
122010       10 WRK-COBR-SITUACAO-EVENTO    PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-TIPO-COBRANCA      PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-TIPO-ARQUIVO       PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-DEB-PARCIAL        PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-TIPO-SALDO         PIC 9(02)      VALUE ZEROS.
122010       10 WRK-COBR-TIPO-CONTA         PIC 9(01)      VALUE ZEROS.
122010       10 WRK-COBR-INCONDICIONAL      PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-ACEITA-PARCELADO   PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-PARCIAL-ULT-REPIQ  PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-VALOR-MINIMO       PIC 9(11)V9(2) VALUE ZEROS.
122010       10 WRK-COBR-BANCO-ORIG         PIC 9(03)      VALUE ZEROS.
122010       10 WRK-COBR-AGENCIA-ORIG       PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-DATA-EVNTO-ORIG    PIC 9(08)      VALUE ZEROS.
122010       10 WRK-COBR-SEG-LINHA-EXTRATO  PIC X(32)      VALUE SPACES.
122010    05 WRK-COBR-RETORNO.
122010       10 WRK-COBR-JOB                PIC X(08)      VALUE SPACES.
122010       10 WRK-COBR-PROGRAMA           PIC X(08)      VALUE SPACES.
122010       10 WRK-COBR-RECURSO            PIC X(08)      VALUE SPACES.
122010       10 WRK-COBR-COD-RETORNO        PIC X(03)      VALUE SPACES.
122010       10 WRK-COBR-VALOR-DEBITADO     PIC 9(11)V9(2) VALUE ZEROS.
122010       10 WRK-COBR-DESCRICAO          PIC X(50)      VALUE SPACES.
122010    05 WRK-COBR-TRAG-EXT.
122010       10 WRK-COBR-CENTRALIZ          PIC X(01)      VALUE SPACES.
122010       10 WRK-COBR-AGENCIA-RET        PIC 9(05)      VALUE ZEROS.
122010       10 WRK-COBR-CONTA-RET          PIC 9(07)      VALUE ZEROS.
122010    05 WRK-COBR-DOCTO-EXTRT           PIC 9(09) COMP-3
122010                                                     VALUE ZEROS.
122010    05 WRK-COBR-NUM-PARC              PIC 9(03) COMP-3
122010                                                     VALUE ZEROS.
122010    05 FILLER                         PIC X(01)      VALUE SPACES.
122010
122010*----------------------------------------------------------------*
122010 01  FILLER  PIC X(32)  VALUE  '*   AREAS CHECKPOINT/RESTART   *'.
122010*----------------------------------------------------------------*
122010
122010 01  WRK-CKRS0100                PIC  X(08)          VALUE
122010     'CKRS0100'.
122010
122010 COPY 'I#CKRS01'.
122010
122010 01  WRK-AREA-RESTART.
122010     05  WRK-CKP-LIDOS-ENTCOBRA  PIC  9(09)          VALUE ZEROS.
122010     05  WRK-CKP-GRAVA-SAICOBRA  PIC  9(09)          VALUE ZEROS.
122010     05  WRK-CKP-GRAVA-ARCOCORR  PIC  9(09)          VALUE ZEROS.
122010
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL7100 ***'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB086
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0I8
           END-EXEC.

122010     EXEC SQL
122010         INCLUDE GFCTB0I3
122010     END-EXEC.
122010
ESC        EXEC SQL
ESC            INCLUDE GFCTB092
ESC        END-EXEC.
ESC
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
122010 0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-VERIFICAR-VAZIO.

           PERFORM 300000-PROCESSAR    UNTIL
                   WRK-FS-ENTCOBRA     EQUAL '10'.

           PERFORM 400000-FINALIZAR.

      *----------------------------------------------------------------*
122010 0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

122010*----------------------------------------------------------------*
122010*    ROTINA PESQUISA-PARCELA
122010*----------------------------------------------------------------*
122010 1000-PESQUISA-PARCELA           SECTION.
122010*----------------------------------------------------------------*
122010
122010     MOVE WRK-COBR-NUM-PARC      TO  WRK-PARC-I
122010     COMPUTE WRK-COBR-NUM-PARC = WRK-COBR-NUM-PARC + 1
122010     MOVE WRK-COBR-NUM-PARC      TO  WRK-PARC-F
122010
122010     PERFORM 1100-ACESSAR-GFCTB0I3
122010
122010     IF  SQLCODE EQUAL ZEROS
122010         IF  CSIT-EVNTO-RECBD  OF GFCTB0I3 EQUAL 2 OR 3
122010             MOVE CPCELA-TARIF       TO  WRK-TAM-03-COM-S-E
122010             MOVE WRK-TAM-03-SEM-S   TO  WRK-COBR-NUM-PARC
122010
ESC                COMPUTE WRK-COBR-VALOR-DEB-CS  =
ESC                                   VTARIF-LIQ-MOVTO OF GFCTB0I3 -
ESC                                   VTARIF-DEB-MOVTO OF GFCTB0I3
ESC                MOVE WRK-COBR-VALOR-DEB-CS TO  WRK-TAM-09-V99-COM-S
122010             MOVE WRK-TAM-09-V99-SEM-S
122010                                     TO  WRK-COBR-VALOR-DEBITAR
122010
122010*************MOVE 13                 TO  WRK-COBR-SITUACAO-EVENTO
122010
122010             MOVE DAGNDA-DEB-MOVTO OF GFCTB0I3   TO  WRK-DATA-DB2
122010             MOVE WRK-ANO-DB2        TO  WRK-ANO-AMD
122010             MOVE WRK-MES-DB2        TO  WRK-MES-AMD
122010             MOVE WRK-DIA-DB2        TO  WRK-DIA-AMD
122010             MOVE WRK-DATA-AMD       TO  WRK-COBR-DATA-ENVIO
122010
ESC                IF WRK-COBR-VALOR-DEB-CS GREATER ZEROS
ESC                  WRITE REG-SAICOBRA  FROM WRK-REG-SAICOBRA
ESC                  PERFORM 112000-TESTAR-FS-SAICOBRA
ESC                  ADD 1                   TO ACU-GRAVADOS-SAICOBRA
ESC                ELSE
ESC                  ADD 1                   TO ACU-DESPREZA-SAICOBRA
ESC                END-IF
122010         END-IF
ESC            PERFORM 1200-UPDATE-GFCTB0I3
ESC            PERFORM 1300-UPDATE-GFCTB092
122010
122010         MOVE CPCELA-TARIF           TO  WRK-TAM-05-COM-S
122010         IF  WRK-TAM-05-COM-S        EQUAL CTOT-PCELA-TARIF
122010             MOVE 'S'                TO  WRK-FLAG
122010         END-IF
122010     END-IF.
122010
122010*----------------------------------------------------------------*
122010 1000-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA ACESSAR-GFCTB0I3
122010*----------------------------------------------------------------*
122010 1100-ACESSAR-GFCTB0I3           SECTION.
122010*----------------------------------------------------------------*
122010
122010     MOVE WRK-COBR-ROTINA-ORIG       TO  CROTNA-ORIGE-MOVTO
122010                                                     OF GFCTB0I3
122010
122010     MOVE WRK-COBR-DATA-ORIG         TO  DENVIO-MOVTO-TARIF
122010                                                     OF GFCTB0I3
122010     MOVE '.'                        TO  DENVIO-MOVTO-TARIF
122010                                             OF GFCTB0I3(3:1)
122010     MOVE '.'                        TO  DENVIO-MOVTO-TARIF
122010                                             OF GFCTB0I3(6:1)
122010
122010     MOVE WRK-COBR-NUM-ARQ-ORIG      TO  CNRO-ARQ-MOVTO
122010                                                     OF GFCTB0I3
122010
122010     MOVE WRK-COBR-SEQ-ARQ-ORIG      TO  CSEQ-MOVTO  OF GFCTB0I3
122010
122010     MOVE WRK-COBR-DATA-ENVIO        TO  WRK-DATA-AMD
122010     MOVE WRK-DIA-AMD                TO  WRK-DIA-DB2
122010     MOVE WRK-MES-AMD                TO  WRK-MES-DB2
122010     MOVE WRK-ANO-AMD                TO  WRK-ANO-DB2
122010     MOVE '.'                        TO  WRK-PT1-DB2 WRK-PT2-DB2
122010     MOVE WRK-DATA-DB2               TO  DAGNDA-DEB-MOVTO
122010                                                     OF GFCTB0I3
122010
122010     EXEC SQL
122010         SELECT  CPCELA_TARIF     ,
122010                 CTOT_PCELA_TARIF ,
122010                 VTARIF_BRUTO_TOT ,
122010                 VTARIF_CADTR_SIST,
122010                 VTARIF_LIQ_MOVTO ,
122010                 VTARIF_DEB_MOVTO ,
122010                 DEFETV_DEB_MOVTO ,
122010                 CSIT_EVNTO_RECBD ,
122010                 DAGNDA_DEB_MOVTO
122010
122010         INTO   :GFCTB0I3.CPCELA-TARIF     ,
122010                :GFCTB0I3.CTOT-PCELA-TARIF ,
122010                :GFCTB0I3.VTARIF-BRUTO-TOT ,
122010                :GFCTB0I3.VTARIF-CADTR-SIST,
122010                :GFCTB0I3.VTARIF-LIQ-MOVTO ,
122010                :GFCTB0I3.VTARIF-DEB-MOVTO ,
122010                :GFCTB0I3.DEFETV-DEB-MOVTO ,
122010                :GFCTB0I3.CSIT-EVNTO-RECBD ,
122010                :GFCTB0I3.DAGNDA-DEB-MOVTO
122010
122010         FROM   DB2PRD.TMOVTO_PCELD_CRRTT
122010
122010         WHERE  CROTNA_ORIGE_MOVTO  = :GFCTB0I3.CROTNA-ORIGE-MOVTO
122010           AND  DENVIO_MOVTO_TARIF  = :GFCTB0I3.DENVIO-MOVTO-TARIF
122010           AND  CNRO_ARQ_MOVTO      = :GFCTB0I3.CNRO-ARQ-MOVTO
122010           AND  CSEQ_MOVTO          = :GFCTB0I3.CSEQ-MOVTO
122010           AND  DAGNDA_DEB_MOVTO   <= :GFCTB0I3.DAGNDA-DEB-MOVTO
122010           AND  CPCELA_TARIF       >  :WRK-PARC-I
122010           AND  CPCELA_TARIF       <= :WRK-PARC-F
122010     END-EXEC.
122010
122010     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
122010        (SQLWARN0                EQUAL 'W'      )
122010         PERFORM 1110-DISPLAY-ERRO-SQLCODE
122010         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
122010         MOVE 'TMOVTO_PCELD_CRRTT'
122010                                 TO  ERR-DBD-TAB
122010         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
122010         MOVE  SQLCODE           TO  ERR-SQL-CODE
122010         MOVE '0001'             TO  ERR-LOCAL
122010         MOVE  SPACES            TO  ERR-SEGM
122010         PERFORM 999999-PROCESSAR-ROTINA-ERRO
122010     END-IF.
122010
122010     IF  SQLCODE                 EQUAL +100
122010         IF  WRK-FLAG            EQUAL 'N'
122010             MOVE 'S'            TO WRK-FLAG
122010         ELSE
122010             PERFORM 1120-DISPLAY-NAO-ENCONT
122010             MOVE 'S'            TO WRK-FLAG
122010         END-IF
122010     ELSE
122010         MOVE 'N'                TO WRK-FLAG
122010     END-IF.
122010
122010*----------------------------------------------------------------*
122010 1100-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA DISPLAY-ERRO-SQLCODE
122010*----------------------------------------------------------------*
122010 1110-DISPLAY-ERRO-SQLCODE       SECTION.
122010*----------------------------------------------------------------*
122010
122010     DISPLAY '************* GFCT1172 *************'
122010     DISPLAY '*   OCORREU UM ERRO NO ACESSO A    *'
122010     DISPLAY '*            BASE GFCTB0I3         *'
122010
122010     MOVE SQLCODE                TO  WRK-TAM-09-COM-S
122010     MOVE WRK-TAM-03-COM-S       TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   SQLCODE -> ' WRK-TAM-03-COM-S-E
122010                                '                *'
122010
122010     DISPLAY '*   CHAVE DE ACESSO:               *'
122010
122010     DISPLAY '*   ROTINA-ORIG: ' WRK-COBR-ROTINA-ORIG
122010                                  '              *'
122010     DISPLAY '*   DATA-ORIG: ' WRK-COBR-DATA-ORIG
122010                                      '          *'
122010     DISPLAY '*   NUM-ARQ-ORIG: ' WRK-COBR-NUM-ARQ-ORIG
122010                                 '               *'
122010     DISPLAY '*   SEQ-ARQ-ORIG: ' WRK-COBR-SEQ-ARQ-ORIG
122010                                          '      *'
122010     DISPLAY '*   DATA-ENVIO: ' WRK-COBR-DATA-ENVIO
122010                                     '           *'
122010     MOVE WRK-PARC-I             TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   PARC-I: ' WRK-TAM-03-SEM-S
122010                            '                    *'
122010     MOVE WRK-PARC-F             TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   PARC-F: ' WRK-TAM-03-SEM-S
122010                            '                    *'
122010     DISPLAY '************* GFCT1172 *************'.
122010
122010*----------------------------------------------------------------*
122010 1110-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA DISPLAY-NAO-ENCONT
122010*----------------------------------------------------------------*
122010 1120-DISPLAY-NAO-ENCONT         SECTION.
122010*----------------------------------------------------------------*
122010
122010     DISPLAY '************* GFCT1172 *************'
122010     DISPLAY '*   PARCELA NAO LOCALIZADA EM      *'
122010     DISPLAY '*             GFCTB0I3             *'
122010     DISPLAY '*   CHAVE DE ACESSO:               *'
122010
122010     DISPLAY '*   ROTINA-ORIG: ' WRK-COBR-ROTINA-ORIG
122010                                  '              *'
122010     DISPLAY '*   DATA-ORIG: ' WRK-COBR-DATA-ORIG
122010                                      '          *'
122010     DISPLAY '*   NUM-ARQ-ORIG: ' WRK-COBR-NUM-ARQ-ORIG
122010                                 '               *'
122010     DISPLAY '*   SEQ-ARQ-ORIG: ' WRK-COBR-SEQ-ARQ-ORIG
122010                                          '      *'
122010     DISPLAY '*   DATA-ENVIO: ' WRK-COBR-DATA-ENVIO
122010                                     '           *'
122010     MOVE WRK-PARC-I             TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   PARC-I: ' WRK-TAM-03-SEM-S
122010                            '                    *'
122010     MOVE WRK-PARC-F             TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   PARC-F: ' WRK-TAM-03-SEM-S
122010                            '                    *'
122010     DISPLAY '************* GFCT1172 *************'.
122010
122010*----------------------------------------------------------------*
122010 1120-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA UPDATE-GFCTB0I3
122010*----------------------------------------------------------------*
122010 1200-UPDATE-GFCTB0I3            SECTION.
122010*----------------------------------------------------------------*
122010
ESC
ESC        IF         WRK-COBR-VALOR-DEB-CS  GREATER ZEROS
ESC                   MOVE 13                TO CSIT-EVNTO-RECBD
ESC                                                        OF GFCTB0I3
ESC        ELSE
ESC                   MOVE 05                TO CSIT-EVNTO-RECBD
ESC                                                        OF GFCTB0I3
ESC        END-IF.

122010     EXEC SQL
122010         UPDATE DB2PRD.TMOVTO_PCELD_CRRTT
122010            SET CSIT_EVNTO_RECBD    = :GFCTB0I3.CSIT-EVNTO-RECBD
122010
122010         WHERE  CROTNA_ORIGE_MOVTO  = :GFCTB0I3.CROTNA-ORIGE-MOVTO
122010           AND  DENVIO_MOVTO_TARIF  = :GFCTB0I3.DENVIO-MOVTO-TARIF
122010           AND  CNRO_ARQ_MOVTO      = :GFCTB0I3.CNRO-ARQ-MOVTO
122010           AND  CSEQ_MOVTO          = :GFCTB0I3.CSEQ-MOVTO
122010           AND  CPCELA_TARIF        = :GFCTB0I3.CPCELA-TARIF
122010     END-EXEC.
122010
122010     IF (SQLCODE                 NOT EQUAL ZEROS) OR
122010        (SQLWARN0                EQUAL 'W'      )
122010         PERFORM 1210-DISPLAY-ERRO-SQLCODE
122010         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
122010         MOVE 'TMOVTO_PCELD_CRRTT'
122010                                 TO  ERR-DBD-TAB
122010         MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
122010         MOVE  SQLCODE           TO  ERR-SQL-CODE
122010         MOVE '0002'             TO  ERR-LOCAL
122010         MOVE  SPACES            TO  ERR-SEGM
122010         PERFORM 999999-PROCESSAR-ROTINA-ERRO
122010     END-IF.
122010
122010*----------------------------------------------------------------*
122010 1200-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA DISPLAY-ERRO-SQLCODE
122010*----------------------------------------------------------------*
122010 1210-DISPLAY-ERRO-SQLCODE       SECTION.
122010*----------------------------------------------------------------*
122010
122010     DISPLAY '************* GFCT1172 *************'
122010     DISPLAY '*   OCORREU UM ERRO NO UPDATE NA   *'
122010     DISPLAY '*            BASE GFCTB0I3         *'
122010
122010     MOVE SQLCODE                TO  WRK-TAM-09-COM-S
122010     MOVE WRK-TAM-03-COM-S       TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   SQLCODE -> ' WRK-TAM-03-COM-S-E
122010                                '                *'
122010
122010     DISPLAY '*   CHAVE DE ACESSO:               *'
122010
122010     DISPLAY '*   ROTINA-ORIG: ' WRK-COBR-ROTINA-ORIG
122010                                  '              *'
122010     DISPLAY '*   DATA-ORIG: ' WRK-COBR-DATA-ORIG
122010                                      '          *'
122010     DISPLAY '*   NUM-ARQ-ORIG: ' WRK-COBR-NUM-ARQ-ORIG
122010                                 '               *'
122010     DISPLAY '*   SEQ-ARQ-ORIG: ' WRK-COBR-SEQ-ARQ-ORIG
122010                                          '      *'
122010     MOVE CPCELA-TARIF           TO  WRK-TAM-03-COM-S-E
122010     DISPLAY '*   PARCELA-TARIFA: ' WRK-TAM-03-SEM-S
122010                                    '            *'
122010     DISPLAY '************* GFCT1172 *************'.
122010
122010*----------------------------------------------------------------*
122010 1210-99-FIM.                    EXIT.
122010*----------------------------------------------------------------*
ESC
ESC   *----------------------------------------------------------------*
ESC   *    ROTINA UPDATE-GFCTB092
ESC   *----------------------------------------------------------------*
ESC    1300-UPDATE-GFCTB092            SECTION.
ESC   *----------------------------------------------------------------*
ESC
ESC        IF         WRK-COBR-VALOR-DEB-CS  GREATER ZEROS
ESC                   MOVE 13                TO CSIT-EVNTO-RECBD
ESC                                                        OF GFCTB092
ESC        ELSE
ESC                   MOVE 05                TO CSIT-EVNTO-RECBD
ESC                                                        OF GFCTB092
ESC        END-IF.
ESC
ESC        MOVE       CROTNA-ORIGE-MOVTO     OF  GFCTB0I3
ESC                                          TO  CROTNA-ORIGE-MOVTO
ESC                                                        OF  GFCTB092
ESC        MOVE       DENVIO-MOVTO-TARIF     OF  GFCTB0I3
ESC                                          TO  DENVIO-MOVTO-TARIF
ESC                                                        OF  GFCTB092
ESC        MOVE       CNRO-ARQ-MOVTO         OF  GFCTB0I3
ESC                                          TO  CNRO-ARQ-MOVTO
ESC                                                        OF  GFCTB092
ESC        MOVE       CSEQ-MOVTO             OF  GFCTB0I3
ESC                                          TO  CSEQ-MOVTO
ESC                                                        OF  GFCTB092
ESC        EXEC SQL
ESC            UPDATE DB2PRD.MOVTO_EVNTO_CRRTT
ESC               SET CSIT_EVNTO_RECBD    = :GFCTB092.CSIT-EVNTO-RECBD
ESC            WHERE  CROTNA_ORIGE_MOVTO  = :GFCTB092.CROTNA-ORIGE-MOVTO
ESC              AND  DENVIO_MOVTO_TARIF  = :GFCTB092.DENVIO-MOVTO-TARIF
ESC              AND  CNRO_ARQ_MOVTO      = :GFCTB092.CNRO-ARQ-MOVTO
ESC              AND  CSEQ_MOVTO          = :GFCTB092.CSEQ-MOVTO
ESC        END-EXEC.
ESC
ESC        IF (SQLCODE                 NOT EQUAL ZEROS) OR
ESC           (SQLWARN0                EQUAL 'W'      )
ESC            PERFORM 1310-DISPLAY-ERRO-SQLCODE
ESC            MOVE 'DB2'              TO  ERR-TIPO-ACESSO
ESC            MOVE 'MOVTO_EVNTO_CRRTT'
ESC                                    TO  ERR-DBD-TAB
ESC            MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
ESC            MOVE  SQLCODE           TO  ERR-SQL-CODE
ESC            MOVE '0003'             TO  ERR-LOCAL
ESC            MOVE  SPACES            TO  ERR-SEGM
ESC            PERFORM 999999-PROCESSAR-ROTINA-ERRO
ESC        END-IF.
ESC
ESC   *----------------------------------------------------------------*
ESC    1300-99-FIM.                    EXIT.
ESC   *----------------------------------------------------------------*
ESC
ESC   *----------------------------------------------------------------*
ESC   *    ROTINA DISPLAY-ERRO-SQLCODE
ESC   *----------------------------------------------------------------*
ESC    1310-DISPLAY-ERRO-SQLCODE       SECTION.
ESC   *----------------------------------------------------------------*
ESC
ESC        DISPLAY '************* GFCT1172 *************'
ESC        DISPLAY '*   OCORREU UM ERRO NO UPDATE NA   *'
ESC        DISPLAY '*            BASE GFCTB092         *'
ESC
ESC        MOVE SQLCODE                TO  WRK-TAM-09-COM-S
ESC        MOVE WRK-TAM-03-COM-S       TO  WRK-TAM-03-COM-S-E
ESC        DISPLAY '*   SQLCODE -> ' WRK-TAM-03-COM-S-E
ESC                                   '                *'
ESC        DISPLAY '*   CHAVE DE ACESSO:               *'
ESC
ESC        DISPLAY '*   ROTINA-ORIG: ' WRK-COBR-ROTINA-ORIG
ESC                                     '              *'
ESC        DISPLAY '*   DATA-ORIG: ' WRK-COBR-DATA-ORIG
ESC                                         '          *'
ESC        DISPLAY '*   NUM-ARQ-ORIG: ' WRK-COBR-NUM-ARQ-ORIG
ESC                                    '               *'
ESC        DISPLAY '*   SEQ-ARQ-ORIG: ' WRK-COBR-SEQ-ARQ-ORIG
ESC                                             '      *'
ESC        DISPLAY '************* GFCT1172 *************'.
ESC
ESC   *----------------------------------------------------------------*
ESC    1310-99-FIM.                    EXIT.
ESC   *----------------------------------------------------------------*
ESC
122010*----------------------------------------------------------------*
122010*    ROTINA INICIALIZAR-CHECKPOINT
122010*----------------------------------------------------------------*
122010 9100-INICIALIZAR-CHECKPOINT  SECTION.
122010*---------------------------------------------------------------*
122010
122010*--> INICIALIZA CHECKPOINT RESTART DB2
122010
122010     MOVE SPACES                 TO  CK01-PLAN.
122010     MOVE 'I'                    TO  CK01-FUNCAO.
122010     MOVE 'DB2 '                 TO  CK01-ID-DB2.
122010     MOVE LENGTH OF WRK-AREA-RESTART
122010                                 TO  CK01-TAM-AREA-RESTART.
122010     MOVE WRK-AREA-RESTART       TO  CK01-AREA-RESTART.
122010
122010     PERFORM 9110-ROTINA-CHECKPONT-RESTART.
122010
122010     IF  CK01-STATUS             EQUAL 'REST'
122010         MOVE CK01-AREA-RESTART(1:27)
122010                                 TO  WRK-AREA-RESTART
122010         MOVE WRK-CKP-LIDOS-ENTCOBRA
122010                                 TO  ACU-LIDOS-ENTCOBRA
122010         MOVE WRK-CKP-GRAVA-SAICOBRA
122010                                 TO  ACU-GRAVADOS-SAICOBRA
122010         MOVE WRK-CKP-GRAVA-ARCOCORR
122010                                 TO  ACU-GRAVADOS-ARCOCORR
122010     END-IF.
122010
122010*---------------------------------------------------------------*
122010 9100-99-FIM. EXIT.
122010*---------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA ROTINA-CHECKPONT-RESTART
122010*----------------------------------------------------------------*
122010 9110-ROTINA-CHECKPONT-RESTART   SECTION.
122010*---------------------------------------------------------------*
122010
122010     CALL WRK-CKRS0100           USING CKRS01-INTERFACE.
122010
122010     IF  CK01-CODIGO-RETORNO     NOT EQUAL ZEROS
122010         MOVE 'APL'              TO ERR-TIPO-ACESSO
122010         MOVE CK01-TEXTO-MENSAGEM(1:75)
122010                                 TO ERR-TEXTO
122010         PERFORM 999999-PROCESSAR-ROTINA-ERRO
122010     END-IF.
122010
122010*---------------------------------------------------------------*
122010 9110-99-FIM. EXIT.
122010*---------------------------------------------------------------*
122010
122010*----------------------------------------------------------------*
122010*    ROTINA ATUALIZAR-CHECKPOINT
122010*----------------------------------------------------------------*
122010 9200-ATUALIZAR-CHECKPOINT  SECTION.
122010*---------------------------------------------------------------*
122010
122010*--> PROCESSA CHECKPOINT RESTART DB2
122010
122010     MOVE 'P'                    TO  CK01-FUNCAO.
122010     MOVE 'DB2'                  TO  CK01-ID-DB2.
122010
122010     MOVE ACU-LIDOS-ENTCOBRA     TO  WRK-CKP-LIDOS-ENTCOBRA
122010     MOVE ACU-GRAVADOS-SAICOBRA  TO  WRK-CKP-GRAVA-SAICOBRA
122010     MOVE ACU-GRAVADOS-ARCOCORR  TO  WRK-CKP-GRAVA-ARCOCORR
122010
122010     MOVE LENGTH OF WRK-AREA-RESTART
122010                                 TO CK01-TAM-AREA-RESTART.
122010     MOVE WRK-AREA-RESTART       TO CK01-AREA-RESTART.
122010
122010     PERFORM 9110-ROTINA-CHECKPONT-RESTART.
122010
122010*---------------------------------------------------------------*
122010 9200-99-FIM. EXIT.
122010*---------------------------------------------------------------*
122010
122010*===============================================================*
122010*    ROTINA FINALIZAR-CHECKPOINT
122010*===============================================================*
122010 9300-FINALIZAR-CHECKPOINT  SECTION.
122010*---------------------------------------------------------------*
122010
122010*--> FINALIZA CHECKPOINT RESTART DB2
122010
122010     MOVE 'F'                    TO  CK01-FUNCAO.
122010     MOVE 'DB2'                  TO  CK01-ID-DB2.
122010
122010     PERFORM 9110-ROTINA-CHECKPONT-RESTART.
122010
122010     MOVE ZEROS                  TO  RETURN-CODE.
122010
122010*---------------------------------------------------------------*
122010 9300-99-FIM. EXIT.
122010*---------------------------------------------------------------*
122010
      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

122010*****CALL 'POOL1050'.
122010     CALL 'CKRS1000'.
122010
122010     PERFORM 9100-INICIALIZAR-CHECKPOINT.

           OPEN INPUT  ENTCOBRA
                OUTPUT SAICOBRA
                       ARCOCORR.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-TESTAR-FILE-STATUS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 111000-TESTAR-FS-ENTCOBRA.

           PERFORM 112000-TESTAR-FS-SAICOBRA.

           PERFORM 113000-TESTAR-FS-ARCOCORR.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       111000-TESTAR-FS-ENTCOBRA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENTCOBRA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ENTCOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ENTCOBRA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-TESTAR-FS-SAICOBRA       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SAICOBRA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SAICOBRA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SAICOBRA    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       113000-TESTAR-FS-ARCOCORR       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARCOCORR         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARCOCORR'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARCOCORR    TO WRK-FILE-STATUS
               MOVE WRK-ERRO-POOL7100  TO ERR-TEXTO
               PERFORM 999999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       113000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-VERIFICAR-VAZIO          SECTION.
      *----------------------------------------------------------------*

           PERFORM 210000-LER-ENTCOBRA.

           IF  WRK-FS-ENTCOBRA         EQUAL '10'
               DISPLAY '************* GFCT1172 *************'
               DISPLAY '*                                  *'
               DISPLAY '*      ARQUIVO ENTCOBRA VAZIO      *'
               DISPLAY '*                                  *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO     *'
               DISPLAY '*                                  *'
               DISPLAY '************* GFCT1172 *************'
           END-IF.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-LER-ENTCOBRA             SECTION.
      *----------------------------------------------------------------*

           READ ENTCOBRA               INTO REG-COBRANCA.

           IF  WRK-FS-ENTCOBRA         EQUAL '10'
               GO TO 210000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 111000-TESTAR-FS-ENTCOBRA.

           MOVE COBR-TARIFA-ORIG       TO WRK-CHV-TARIFA.
           MOVE COBR-DATA-EVNTO-ORIG   TO WRK-CHV-DATA.
           MOVE 'GFCT0614'             TO COBR-JOB.
           MOVE 'GFCT1172'             TO COBR-PROGRAMA.

           ADD 1                       TO ACU-LIDOS-ENTCOBRA.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           IF  WRK-CHAVE-ATU           NOT EQUAL WRK-CHAVE-ANT
RET   *        DISPLAY '*********************** QUEBROU:' WRK-CHAVE-ATU
               PERFORM 310000-OBTER-DADOS-TARIFA
               MOVE WRK-CHAVE-ATU      TO WRK-CHAVE-ANT
           ELSE
               IF  WRK-ERRO         EQUAL 'N'
                   PERFORM 320000-GRAVAR-SAICOBRA
               ELSE
                   MOVE WRK-RETORNO     TO COBR-RETORNO
                   MOVE 14              TO COBR-SITUACAO-EVENTO
                   WRITE REG-ARCOCORR FROM REG-COBRANCA
                   PERFORM 113000-TESTAR-FS-ARCOCORR
                   ADD 1                TO ACU-GRAVADOS-ARCOCORR
               END-IF
           END-IF.

122010     PERFORM 9200-ATUALIZAR-CHECKPOINT.
122010
           PERFORM 210000-LER-ENTCOBRA.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       310000-OBTER-DADOS-TARIFA       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                        TO WRK-ERRO.
           MOVE 'B'                        TO WRK-5529-AMBIENTE.
           MOVE 'GFCT1172'                 TO WRK-5529-CHAMADOR.
           MOVE COBR-TARIFA-ORIG           TO WRK-5529-TARIFA.
           MOVE COBR-DATA-EVNTO-ORIG(1:4)  TO WRK-ANO-AUX.
           MOVE COBR-DATA-EVNTO-ORIG(5:2)  TO WRK-MES-AUX.
           MOVE COBR-DATA-EVNTO-ORIG(7:2)  TO WRK-DIA-AUX.
           MOVE WRK-DATA-AUX               TO WRK-5529-DATA.
           MOVE 'GFCT5529'                 TO WRK-MODULO.

           CALL WRK-MODULO                 USING WRK-5529-AREA-ENTRADA
                                                 WRK-5529-AREA-SAIDA
                                                 WRK-IO-PCB
                                                 WRK-ALT-PCB
                                                 GFCT0M-AREA-ERROS.

           IF  WRK-5529-COD-RETORNO        NOT EQUAL ZEROS
               MOVE  'S'                   TO WRK-ERRO
               MOVE  'GFCT5529'            TO WRK-RECURSO
               MOVE  WRK-5529-COD-RETORNO  TO WRK-COD-RET-2
               MOVE  WRK-COD-RET-3         TO WRK-COD-RETORNO
               MOVE  WRK-5529-DESC-MSG     TO WRK-DESCRICAO
               MOVE  WRK-RETORNO           TO COBR-RETORNO
               MOVE  14                    TO COBR-SITUACAO-EVENTO
               WRITE REG-ARCOCORR          FROM REG-COBRANCA
               PERFORM 113000-TESTAR-FS-ARCOCORR
               ADD 1                       TO ACU-GRAVADOS-ARCOCORR
               GO TO 310000-99-FIM
           END-IF.

           PERFORM 311000-BUSCAR-DEBITO-CONS.

      *----------------------------------------------------------------*
       310000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       311000-BUSCAR-DEBITO-CONS       SECTION.
      *----------------------------------------------------------------*

           MOVE COBR-TARIFA-ORIG       TO CSERVC-TARIF      OF GFCTB086.
      *    MOVE COBR-DATA-ORIG         TO DINIC-VGCIA-TARIF OF GFCTB086.
      *    MOVE COBR-DATA-ORIG         TO DFIM-VGCIA-TARIF  OF GFCTB086.
           MOVE WRK-DATA-AUX           TO DINIC-VGCIA-TARIF OF GFCTB086.
           MOVE WRK-DATA-AUX           TO DFIM-VGCIA-TARIF  OF GFCTB086.

           EXEC SQL
             SELECT
                   CINDCD_DEB_CONSL
             INTO
                   :GFCTB086.CINDCD-DEB-CONSL
             FROM   DB2PRD.HIST_PARM_TARIF
             WHERE
                   CSERVC_TARIF       = :GFCTB086.CSERVC-TARIF      AND
                  (DINIC_VGCIA_TARIF <= :GFCTB086.DINIC-VGCIA-TARIF AND
                   DFIM_VGCIA_TARIF  >= :GFCTB086.DFIM-VGCIA-TARIF) AND
                   CSEQ_MANUT_SERVC   IN
              (SELECT MAX (CSEQ_MANUT_SERVC)
               FROM DB2PRD.HIST_PARM_TARIF
                 WHERE
                   CSERVC_TARIF       = :GFCTB086.CSERVC-TARIF      AND
                  (DINIC_VGCIA_TARIF <= :GFCTB086.DINIC-VGCIA-TARIF AND
                   DFIM_VGCIA_TARIF  >= :GFCTB086.DFIM-VGCIA-TARIF))
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'S'                TO WRK-ERRO
               MOVE 'GFCT5529'         TO WRK-RECURSO
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE(7:3)   TO WRK-COD-RETORNO
               MOVE 'ERRO NO ACESSO A BASE'
                                       TO WRK-DESCRICAO
               MOVE  WRK-RETORNO       TO COBR-RETORNO
               MOVE  14                TO COBR-SITUACAO-EVENTO
               WRITE REG-ARCOCORR      FROM REG-COBRANCA
               PERFORM 113000-TESTAR-FS-ARCOCORR
               ADD 1                   TO ACU-GRAVADOS-ARCOCORR
               GO TO 311000-99-FIM
           END-IF.

           PERFORM 312000-VERIFICAR-AGRUPAMENTO.

           PERFORM 320000-GRAVAR-SAICOBRA.

      *----------------------------------------------------------------*
       311000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       312000-VERIFICAR-AGRUPAMENTO    SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-AGRUPAMENTO.
           MOVE COBR-TARIFA-ORIG       TO CSERVC-TARIF      OF GFCTB0I8.
      *    MOVE COBR-DATA-ORIG         TO DINIC-VGCIA-ASSOC OF GFCTB0I8.
      *    MOVE COBR-DATA-ORIG         TO DFIM-VGCIA-ASSOC  OF GFCTB0I8.
           MOVE WRK-DATA-AUX           TO DINIC-VGCIA-ASSOC OF GFCTB0I8.
           MOVE WRK-DATA-AUX           TO DFIM-VGCIA-ASSOC  OF GFCTB0I8.

           EXEC SQL
               SELECT  CAGPTO_CTBIL
               INTO   :GFCTB0I8.CAGPTO-CTBIL
               FROM    DB2PRD.TASSOC_AGPTO_CTBIL
               WHERE   CSERVC_TARIF       = :GFCTB0I8.CSERVC-TARIF
                 AND  (DINIC_VGCIA_ASSOC <= :GFCTB0I8.DINIC-VGCIA-ASSOC
                 AND   DFIM_VGCIA_ASSOC  >= :GFCTB0I8.DFIM-VGCIA-ASSOC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 'S'                TO WRK-ERRO
               MOVE 'GFCT5529'         TO WRK-RECURSO
               MOVE SQLCODE            TO WRK-SQLCODE
               MOVE WRK-SQLCODE(7:3)   TO WRK-COD-RETORNO
               MOVE 'ERRO NO ACESSO GFCTB0I8'
                                       TO WRK-DESCRICAO
               MOVE  WRK-RETORNO       TO COBR-RETORNO
               MOVE  14                TO COBR-SITUACAO-EVENTO
               WRITE REG-ARCOCORR      FROM REG-COBRANCA
               PERFORM 113000-TESTAR-FS-ARCOCORR
               ADD 1                   TO ACU-GRAVADOS-ARCOCORR
               GO TO 312000-99-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO WRK-AGRUPAMENTO
           ELSE
               MOVE CAGPTO-CTBIL OF GFCTB0I8
                                       TO WRK-AGRUPAMENTO
           END-IF.


      *----------------------------------------------------------------*
       312000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       320000-GRAVAR-SAICOBRA           SECTION.
      *----------------------------------------------------------------*

           IF  CINDCD-DEB-CONSL            OF GFCTB086
                                           EQUAL 'S'
               MOVE 5                      TO COBR-TIPO-COBRANCA
           ELSE
               MOVE ZEROS                  TO COBR-TIPO-COBRANCA
           END-IF.

           IF  WRK-AGRUPAMENTO             GREATER ZEROS
               MOVE 8                      TO COBR-TIPO-COBRANCA
           END-IF.

RET   *    DISPLAY 'SEQ/TARIFA/DT.EVNTO/AGRUP: '
      *             COBR-SEQ-ARQ-ORIG '/'
      *             COBR-TARIFA-ORIG  '/'
      *             WRK-DATA-AUX      '/'
      *             WRK-AGRUPAMENTO.

           MOVE WRK-5529-CCART-DEB-TARIF   TO COBR-CARTEIRA-DEB.
           MOVE WRK-5529-CIDTFD-LCTO       TO COBR-COD-LCTO-DEB.
           MOVE WRK-5529-CINDCD-PCIAL-ULT  TO COBR-PARCIAL-ULT-REPIQ.
           MOVE WRK-5529-VMIN-ENVIO-COBR   TO COBR-VALOR-MINIMO.
           MOVE WRK-5529-RSEGDA-LIN-EXTRT  TO COBR-SEG-LINHA-EXTRATO.
           MOVE WRK-5529-CTPO-CTA-DEB-TARIF TO COBR-TIPO-CONTA.
           MOVE 2                          TO COBR-TIPO-SALDO.
           MOVE 'N'                        TO COBR-INCONDICIONAL.
           MOVE ZEROS                      TO COBR-COD-MOEDA-DEB.

RET   *    DISPLAY 'TARIFA/CART-MOD/CART-GFCT='
RET   *         WRK-5529-TARIFA '/'
RET   *         WRK-5529-CCART-DEB-TARIF '/' COBR-CARTEIRA-DEB.

           IF  WRK-5529-CINDCD-DEB-PCIAL       EQUAL 'S'
               MOVE 1                          TO COBR-DEB-PARCIAL
           ELSE
               MOVE 2                          TO COBR-DEB-PARCIAL
           END-IF.

122010     IF  COBR-ACEITA-PARCELADO       NOT EQUAL 'S'
122010         MOVE ZEROS                  TO  COBR-NUM-PARC
122010     ELSE
122010         MOVE REG-COBRANCA           TO  WRK-REG-SAICOBRA
122010         MOVE 'I'                    TO  WRK-FLAG
122010
122010         IF  WRK-COBR-NUM-PARC       NOT NUMERIC
122010             MOVE ZEROS              TO  WRK-COBR-NUM-PARC
122010         END-IF
122010
122010         PERFORM 1000-PESQUISA-PARCELA
122010           UNTIL WRK-FLAG NOT EQUAL 'N' AND 'I'
122010             GO TO 320000-99-FIM
122010     END-IF.
122010
           MOVE 'N'                        TO COBR-ACEITA-PARCELADO.

           WRITE REG-SAICOBRA              FROM REG-COBRANCA.

           PERFORM 112000-TESTAR-FS-SAICOBRA.

           ADD 1                           TO ACU-GRAVADOS-SAICOBRA.

      *----------------------------------------------------------------*
       320000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       400000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 410000-EMITIR-TOTAIS.

           CLOSE ENTCOBRA
                 SAICOBRA
                 ARCOCORR.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 110000-TESTAR-FILE-STATUS.

122010     PERFORM 9300-FINALIZAR-CHECKPOINT.
122010
           GOBACK.

      *----------------------------------------------------------------*
       400000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       410000-EMITIR-TOTAIS            SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-ENTCOBRA     TO WRK-MASCARA.

           DISPLAY '****************************** GFCT1172 ***********'
                                                        '*************'.
           DISPLAY '*                                                '
                                                      '              *'.
           DISPLAY '*  TOTAL REGS. LIDOS ENTCOBRA................:   '
                                                  ' ' WRK-MASCARA '  *'.

           MOVE ACU-GRAVADOS-SAICOBRA   TO WRK-MASCARA.

           DISPLAY '*  TOTAL REGS. GRAVADOS SAICOBRA.............:   '
                                                  ' ' WRK-MASCARA '  *'.

           MOVE ACU-GRAVADOS-ARCOCORR   TO WRK-MASCARA.

           DISPLAY '*  TOTAL REGS. GRAVADOS ARCOCORR.............:   '
                                                  ' ' WRK-MASCARA '  *'.

           DISPLAY '*                                                '
                                                      '              *'.
ESC        MOVE ACU-DESPREZA-SAICOBRA   TO WRK-MASCARA.
ESC
ESC        DISPLAY '*  TOTAL REGS. DESPREZADOS ARCOCORR..........:   '
ESC                                               ' ' WRK-MASCARA '  *'.
ESC
ESC        DISPLAY '*                                                '
ESC                                                   '              *'.
           DISPLAY '****************************** GFCT1172 ***********'
                                                        '*************'.

      *----------------------------------------------------------------*
       410000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       999999-PROCESSAR-ROTINA-ERRO    SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1172'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       999999-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
