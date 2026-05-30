      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT1360.
       AUTHOR. ALINE THIESEN WOLFF.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT1360                                     *
      *    PROGRAMADORA : ALINE THIESEN WOLFF           - CPM/FPOLIS   *
      *    ANALISTA CPM : SILVANA FERREIRA              - CPM/FPOLIS   *
      *    ANALISTA     : VINICIUS - PROCWORK           - GRUPO 50     *
      *    DATA         : 14/11/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERACAO DO ARQUIVO TRIC DE ESTORNO A PARTIR DO ARQUIVO    *
      *      PREPARATORIO DA FITA4, COMPLEMENTANDO COM OS DADOS DAS    *
      *      TARIFAS.                                                  *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      PREFITA4                              I#GFCTHF            *
      *      ESTRTRIC                              I#GFCTG4            *
      *      RELATRIC                              --------            *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                               INCLUDE/BOOK          *
      *      DB2PRD.PARM_DATA_PROCM                GFCTB0A1            *
      *      DB2PRD.SERVC_TARIF_PRINC              GFCTB0D8            *
      *      DB2PRD.PARM_SERVC_TARIF               GFCTB0A2            *
      *      DB2PRD.TARIF_EVNTO_CTBIL              GFCTB047            *
SP1604*      DB2PRD.V01CTA_CORRENTE                CLIEV008            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL0431 - CALCULO DE DIGITO DA CONTA.                    *
      *      POOL1050 - MODULO PARA CONEXAO COM DB2.                   *
      *      POOL1260 - CALCULO DE DIGITO DA AGENCIA.                  *
      *      POOL7100 - TRATAMENTO DE ERROS DA APLICACAO.              *
      *      POOL7600 - OBTER DATA E HORA.                             *
      *                                                                *
      *================================================================*
      *               - U L T I M A   A L T E R A C A O -              *
      *================================================================*
      *                                                                *
      *      ANALISTA     : MARCUS VINICIUS    - PROCWORK TECNOLOGIA   *
      *      DATA         : 23/06/2006                                 *
      *                                                                *
      *      OBJETIVO     : CASO OCORRA DIFERENCA NA SOMATORIA DE VA-  *
      *                     LORES CALCULADOS A PARTIR DOS PERCENTUAIS  *
      *                     CONTABEIS, ESTA DIFERENCA DEVERA SER ADI-  *
      *                     CIONADA NO ULTIMO VALOR CORRESPONDENTE AO  *
      *                     ULTIMO PERCENTUAL CONTABIL PREENCHIDO.     *
      *                                                                *
      *================================================================*
      *                                                                *
      *    ALTERACAO :  PROCWORK - PROPOSTA 07-2287                    *
      *                                                                *
      *    ANALISTA     : MARCUS VINCIUS - PROCWORK  - GRUPO 50        *
      *    DATA         : 09/2007                                      *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      PASSA A MOVER RAZAO 10-51 PARA AS TARIFAS DE POUPANCA     *
      *    IDENTIFICADAS PELO CAMPO CTPO_CTA_DEB_TARIF - GFCTB0D8      *
      *                                                                *
SP1604*----------------------------------------------------------------*
SP1604*                 U L T I M A   A L T E R A C A O                *
SP1604*----------------------------------------------------------------*
SP1604*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP1604       *
SP1604*----------------------------------------------------------------*
SP1604*                                                                *
SP1604*    ANALISTA....:  MARCUS VINICIUS - SONDA/PROCWORK             *
SP1604*    DATA........:  16/04/2008                                   *
SP1604*    OBJETIVO....:  TRATAR TIPO DE LANCAMENTO DE ESTORNO PARA    *
SP1604*                   TIPO DE PESSOA JURIDICA                      *
SP1604*                                                                *
SP7008*----------------------------------------------------------------*
SP7008*                 U L T I M A   A L T E R A C A O                *
SP7008*----------------------------------------------------------------*
SP7008*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP1604       *
SP7008*----------------------------------------------------------------*
SP7008*                                                                *
SP7008*    ANALISTA....:  MARCUS VINICIUS - SONDA/PROCWORK             *
SP7008*    DATA........:  12/01/2009                                   *
SP7008*    OBJETIVO....:  AO IDENTIFICAR DADOS CONSOLIDADOS, MOVER     *
SP7008*                   DATA DE DEBITO PARA O NUMERO DO DOCUMENTO.   *
SP7008*                                                                *
020909*----------------------------------------------------------------*
020909*                 U L T I M A   A L T E R A C A O                *
020909*----------------------------------------------------------------*
020909*        STEFANINI      - CONSULTORIA - ALTERACAO - RO 2625755   *
020909*----------------------------------------------------------------*
020909*                                                                *
020909*    ANALISTA....:  ARNALDO - STEFANINI                          *
020909*    DATA........:  02/09/2009                                   *
020909*    OBJETIVO....:  TRATAR REGISTRO NAO ENCONTRADO NA TABELA     *
020909*                   TARIF_EVNTO_CTBIL. PF E PJ.                  *
020909*                                                                *
BI0411*                    A L T E R A C A O                           *
BI0411*----------------------------------------------------------------*
BI0411*                                                                *
BI0411*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0411*    DATA         : 04 / 2011                                    *
BI0411*                                                                *
BI0411*    OBJETIVO....:  TRATAR O CAMPO NOVO CLCTO-ESTRN-ANTER DE     *
BI0411*                   ESTORNO DE TARIFAS SEMESTRE ANTERIOR E       *
BI0411*                   TRATAR OS CAMPOS NOVOS CINDCD_REPAS_CLULR ,  *
BI0411*                                          VREPAS_CREDT_CLULR E  *
BI0411*                                          QDIA_RTCAO_CREDT      *
BI0411*                   DAS TABELAS GFCTB086 E GFCTB0A2.             *
BI0411*                                                                *
BI0611*----------------------------------------------------------------*
BI0611*                    A L T E R A C A O                           *
BI0611*----------------------------------------------------------------*
BI0611*                                                                *
BI0611*    ANALISTA     : UBIRAJARA MIRANDA(BIRA)  - PROCWORK          *
BI0611*    DATA         : 06 / 2011                                    *
BI0611*                                                                *
BI0611*    OBJETIVO....:  CORRECAO NA PESQUISA DA TRILHA CONTABIL      *
BI0611*                   QUANDO HOUVER MAIS DE UMA CONTA COM DATAS    *
BI0611*                   DE DEBITO DIFERENTES.                        *
BI0611*                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
      *----------------------------------------------------------------*

        SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT PREFITA4 ASSIGN      TO UT-S-PREFITA4
                      FILE STATUS      IS WRK-FS-PREFITA4.

           SELECT ESTRTRIC ASSIGN      TO UT-S-ESTRTRIC
                      FILE STATUS      IS WRK-FS-ESTRTRIC.

           SELECT RELATRIC ASSIGN      TO UT-S-RELATRIC
                      FILE STATUS      IS WRK-FS-RELATRIC.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQ GERADO - ESTORNO P/ CONTA. VIA TRIC           *
      *              ORG. SEQUENCIAL   -   LRECL = 0130                *
      *----------------------------------------------------------------*

       FD  PREFITA4
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTHF.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQ TRIC DE ESTORNO                               *
      *              ORG. SEQUENCIAL   -   LRECL = 0300                *
      *----------------------------------------------------------------*

       FD  ESTRTRIC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY I#GFCTG4.

      *----------------------------------------------------------------*
      *    OUTPUT :  RELATORIO DE OCORRENCIAS TRIC - ESTORNO           *
      *              ORG. SEQUENCIAL   -   LRECL = 0133                *
      *----------------------------------------------------------------*

       FD  RELATRIC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-RELATRIC                PIC X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT1360  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        INDEXADORES           *'.
      *----------------------------------------------------------------*

       01  IND-TARIF                   PIC 9(01)   COMP-3  VALUE ZEROS.
       01  IND-TRIC                    PIC 9(01)   COMP-3  VALUE ZEROS.

SP1604 01  IND-PF                      PIC  9(001)  COMP-3 VALUE ZEROS.
SP1604 01  IND-PJ                      PIC  9(001)  COMP-3 VALUE ZEROS.
BI0411 01  IND-ANTER                   PIC  9(001)  COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        ACUMULADORES          *'.
      *----------------------------------------------------------------*

       01  ACU-PAGINA                  PIC 9(05)           VALUE ZEROS.
       01  ACU-LINHAS                  PIC 9(02)           VALUE 99.
       01  ACU-LIDOS-PREFITA4          PIC 9(11)           VALUE ZEROS.
       01  ACU-QTD-TARIFAS-PESQ        PIC 9(11)           VALUE ZEROS.
       01  ACU-QTD-OCORS-PREFITA4      PIC 9(11)           VALUE ZEROS.
BI0411 01  ACU-SEMESTRE-ANT            PIC 9(11) COMP-3    VALUE ZEROS.
       01  ACU-GRAVA-ESTRTRIC          PIC 9(11)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-BATCH                   PIC X(08)           VALUE
           'BATCH'.
       01  WRK-FIM-CURSOR              PIC X(01)           VALUE SPACES.
       01  WRK-TARIFA-ANT              PIC 9(05)           VALUE ZEROS.
SP1604 01  WRK-AGENCIA-ANT             PIC  9(005) COMP-3  VALUE ZEROS.
SP1604 01  WRK-CONTA-ANT               PIC  9(013) COMP-3  VALUE ZEROS.
       01  WRK-MASCARA                 PIC ZZZ.ZZZ.ZZZ.ZZ9.
       01  WRK-MENSAGEM                PIC X(01)           VALUE SPACES.
       01  WRK-VALOR                   PIC 9(13)V99        VALUE ZEROS.
       01  WRK-VALOR-DIF               PIC 9(13)V99        VALUE ZEROS.
       01  WRK-GRP-RZ                  PIC 9(03)           VALUE ZEROS.
       01  WRK-SBG-RZ                  PIC 9(03)           VALUE ZEROS.
       01  WRK-NRO-DOC                 PIC 9(07)           VALUE ZEROS.

SP1604 01  WRK-ACHOU-CONTA-POUP        PIC  X(001)         VALUE SPACES.
SP1604 01  WRK-ACHOU-CONTA-CORR        PIC  X(001)         VALUE SPACES.
SP1604 01  WRK-ACHOU-CNPJ-CPF          PIC  X(001)         VALUE SPACES.

SP1604 01  WRK-TIPO-CONTA              PIC  9(001)         VALUE ZEROS.
SP1604 01  WRK-TIPO-PESSOA             PIC  X(001)         VALUE SPACES.

SP1604 01  WRK-CID-CLI                 PIC  X(026)         VALUE SPACES.

SP1604 01  WRK-CTPO-LCTO-ESTRN         PIC S9(002) COMP-3  VALUE ZEROS.
SP1604 01  WRK-CLCTO-ESTRN-PJ          PIC S9(002) COMP-3  VALUE ZEROS.

BI0411 01  WRK-SEMESTRE-MOV            PIC  X(002)         VALUE SPACES.
BI0411 01  WRK-SEMESTRE-ATU            PIC  X(002)         VALUE SPACES.

BI0411 01  WRK-DATA-AUX-ATU.
BI0411     03  WRK-MMDD-ATU.
BI0411         05  WRK-MM-ATU          PIC X(02)           VALUE SPACES.
BI0411         05  WRK-DD-ATU          PIC X(02)           VALUE SPACES.
BI0411     03  WRK-MMDD-ATU-R          REDEFINES WRK-MMDD-ATU PIC 9(04).
BI0411     03  WRK-AAAA-ATU            PIC X(04)           VALUE SPACES.

BI0411 01  WRK-DATA-AUX-MOV.
BI0411     03  WRK-MMDD-MOV.
BI0411         05  WRK-MM-MOV          PIC X(02)           VALUE SPACES.
BI0411         05  WRK-DD-MOV          PIC X(02)           VALUE SPACES.
BI0411     03  WRK-MMDD-MOV-R          REDEFINES WRK-MMDD-MOV PIC 9(04).
BI0411     03  WRK-AAAA-MOV            PIC X(04)           VALUE SPACES.

BI0411 01  WRK-AUX-3-V-S               PIC +9(003)V9(4)    VALUE ZEROS.
BI0411 01  FILLER                      REDEFINES WRK-AUX-3-V-S.
BI0411     05  FILLER                  PIC  X(001).
BI0411     05  WRK-AUX-3-V             PIC  9(003)V9(4).

       01  WRK-DATA-AUX.
           03  WRK-DD                  PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '/'.
           03  WRK-MM                  PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '/'.
           03  WRK-AAAA                PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-AAAAMMDD.
           03  WRK-DTA-AAAA            PIC 9(04)           VALUE ZEROS.
           03  WRK-DTA-MM              PIC 9(02)           VALUE ZEROS.
           03  WRK-DTA-DD              PIC 9(02)           VALUE ZEROS.
       01  WRK-DATA-AAAAMMDD-R         REDEFINES WRK-DATA-AAAAMMDD
                                       PIC 9(08).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA FILE-STATUS    *'.
      *----------------------------------------------------------------*

       01  WRK-FS-PREFITA4             PIC X(02)           VALUE SPACES.
       01  WRK-FS-ESTRTRIC             PIC X(02)           VALUE SPACES.
       01  WRK-FS-RELATRIC             PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)           VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC X(13)           VALUE
           'NA  LEITURA'.
       01  WRK-GRAVACAO                PIC X(13)           VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC X(13)           VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREAS DE MENSAGENS DE ERRO  *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS.
           03  FILLER                  PIC X(07)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE '* '.
           03  FILLER                  PIC X(05)           VALUE
               'ERRO '.
           03  WRK-OPERACAO            PIC X(13)           VALUE SPACES.
           03  FILLER                  PIC X(12)           VALUE
               ' DO ARQUIVO '.
           03  WRK-ARQUIVO             PIC X(08)           VALUE SPACES.
           03  FILLER                  PIC X(17)           VALUE
               ' - FILE-STATUS = '.
           03  WRK-FS                  PIC X(02)           VALUE SPACES.
           03  FILLER                  PIC X(02)           VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DE TABELA INTERNA    *'.
      *----------------------------------------------------------------*

SP1604 01  TAB-PF-TARIFA.
SP1604     03  TAB-PF-TARIF-CTBIL      OCCURS 4 TIMES.
SP1604         05  TAB-PF-CSERVC-TARIF PIC 9(05)           VALUE ZEROS.
SP1604         05  TAB-PF-CTPO-LCTO-CTBIL
SP1604                                 PIC 9(02)           VALUE ZEROS.
SP1604         05  TAB-PF-CSEQ-EVNTO-CTBIL
SP1604                                 PIC 9(02)           VALUE ZEROS.
SP1604         05  TAB-PF-CINDCD-DEPDC-CTBIL
SP1604                                 PIC 9(01)           VALUE ZEROS.
SP1604         05  TAB-PF-PATRIC-CTBIL PIC 9(03)V9999      VALUE ZEROS.
SP1604         05  TAB-PF-CDEPDC-CTLZO PIC 9(05)           VALUE ZEROS.
SP1604         05  TAB-PF-COPER-CTBIL  PIC X(07)           VALUE SPACES.
SP1604         05  TAB-PF-CGRP-RZ-CTBIL
SP1604                                 PIC 9(03)           VALUE ZEROS.
SP1604         05  TAB-PF-CSGRP-RZ-CTBIL
SP1604                                 PIC 9(03)           VALUE ZEROS.
SP1604         05  TAB-PF-CCTA-EVNTO-CTBIL
SP1604                                 PIC 9(07)           VALUE ZEROS.
SP1604         05  TAB-PF-CDOCTO-CTBIL PIC 9(07)           VALUE ZEROS.

SP1604 01  TAB-PJ-TARIFA.
SP1604     03  TAB-PJ-TARIF-CTBIL      OCCURS 4 TIMES.
SP1604         05  TAB-PJ-CSERVC-TARIF PIC 9(05)           VALUE ZEROS.
SP1604         05  TAB-PJ-CTPO-LCTO-CTBIL
SP1604                                 PIC 9(02)           VALUE ZEROS.
SP1604         05  TAB-PJ-CSEQ-EVNTO-CTBIL
SP1604                                 PIC 9(02)           VALUE ZEROS.
SP1604         05  TAB-PJ-CINDCD-DEPDC-CTBIL
SP1604                                 PIC 9(01)           VALUE ZEROS.
SP1604         05  TAB-PJ-PATRIC-CTBIL PIC 9(03)V9999      VALUE ZEROS.
SP1604         05  TAB-PJ-CDEPDC-CTLZO PIC 9(05)           VALUE ZEROS.
SP1604         05  TAB-PJ-COPER-CTBIL  PIC X(07)           VALUE SPACES.
SP1604         05  TAB-PJ-CGRP-RZ-CTBIL
SP1604                                 PIC 9(03)           VALUE ZEROS.
SP1604         05  TAB-PJ-CSGRP-RZ-CTBIL
SP1604                                 PIC 9(03)           VALUE ZEROS.
SP1604         05  TAB-PJ-CCTA-EVNTO-CTBIL
SP1604                                 PIC 9(07)           VALUE ZEROS.
SP1604         05  TAB-PJ-CDOCTO-CTBIL PIC 9(07)           VALUE ZEROS.

BI0411 01  TAB-SA-TARIFA.
BI0411     03  TAB-SA-TARIF-CTBIL      OCCURS 4 TIMES.
BI0411         05  TAB-SA-CSERVC-TARIF PIC 9(05)           VALUE ZEROS.
BI0411         05  TAB-SA-CTPO-LCTO-CTBIL
BI0411                                 PIC 9(02)           VALUE ZEROS.
BI0411         05  TAB-SA-CSEQ-EVNTO-CTBIL
BI0411                                 PIC 9(02)           VALUE ZEROS.
BI0411         05  TAB-SA-CINDCD-DEPDC-CTBIL
BI0411                                 PIC 9(01)           VALUE ZEROS.
BI0411         05  TAB-SA-PATRIC-CTBIL PIC 9(03)V9999      VALUE ZEROS.
BI0411         05  TAB-SA-CDEPDC-CTLZO PIC 9(05)           VALUE ZEROS.
BI0411         05  TAB-SA-COPER-CTBIL  PIC X(07)           VALUE SPACES.
BI0411         05  TAB-SA-CGRP-RZ-CTBIL
BI0411                                 PIC 9(03)           VALUE ZEROS.
BI0411         05  TAB-SA-CSGRP-RZ-CTBIL
BI0411                                 PIC 9(03)           VALUE ZEROS.
BI0411         05  TAB-SA-CCTA-EVNTO-CTBIL
BI0411                                 PIC 9(07)           VALUE ZEROS.
BI0411         05  TAB-SA-CDOCTO-CTBIL PIC 9(07)           VALUE ZEROS.

       01  TAB1-TARIFA.
           03  TAB1-TARIF-CTBIL        OCCURS 4 TIMES.
               05  TAB1-CSERVC-TARIF   PIC 9(05)           VALUE ZEROS.
               05  TAB1-CTPO-LCTO-CTBIL
                                       PIC 9(02)           VALUE ZEROS.
               05  TAB1-CSEQ-EVNTO-CTBIL
                                       PIC 9(02)           VALUE ZEROS.
               05  TAB1-CINDCD-DEPDC-CTBIL
                                       PIC 9(01)           VALUE ZEROS.
               05  TAB1-PATRIC-CTBIL   PIC 9(03)V9999      VALUE ZEROS.
               05  TAB1-CDEPDC-CTLZO   PIC 9(05)           VALUE ZEROS.
               05  TAB1-COPER-CTBIL    PIC X(07)           VALUE SPACES.
               05  TAB1-CGRP-RZ-CTBIL  PIC 9(03)           VALUE ZEROS.
               05  TAB1-CSGRP-RZ-CTBIL PIC 9(03)           VALUE ZEROS.
               05  TAB1-CCTA-EVNTO-CTBIL
                                       PIC 9(07)           VALUE ZEROS.
               05  TAB1-CDOCTO-CTBIL   PIC 9(07)           VALUE ZEROS.

       01  TAB2-TRIC.
           03  TAB2-EVNTO-TRIC         OCCURS 4 TIMES.
               05  TAB2-AG-DESTINO     PIC 9(05) COMP-3    VALUE ZEROS.
               05  TAB2-DAC-AG         PIC X(01)           VALUE SPACES.
               05  TAB2-GRP-RZ         PIC 9(03) COMP-3    VALUE ZEROS.
               05  TAB2-SBG-RZ         PIC 9(03) COMP-3    VALUE ZEROS.
               05  TAB2-CONTA          PIC 9(07) COMP-3    VALUE ZEROS.
               05  TAB2-DAC-CONTA      PIC X(01)           VALUE SPACES.
               05  TAB2-HIST-COMPL     PIC X(32)           VALUE SPACES.
               05  TAB2-NRO-DOC        PIC 9(07) COMP-3    VALUE ZEROS.
               05  TAB2-FILLER         PIC X(06)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA POOL0431       *'.
      *----------------------------------------------------------------*

       01  WRK-DIGITO-CTA              PIC X(01)           VALUE SPACES.
       01  WRK-TAMANHO                 PIC 9(02)           VALUE ZEROS.
       01  WRK-CONTA                   PIC 9(07) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA POOL1260       *'.
      *----------------------------------------------------------------*

       01  WRK-DIGITO                  PIC X(01)           VALUE SPACES.
       01  WRK-AGENCIA                 PIC 9(07) COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA POOL7600       *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3    VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3    VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3    VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3    VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3    VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      LINHAS DE CABECALHO     *'.
      *----------------------------------------------------------------*

       01  CABEC1.
           03  CB1-CARRO               PIC X(01)           VALUE '1'.
           03  CB1-DATA                PIC X(10)           VALUE SPACES.
           03  FILLER                  PIC X(50)           VALUE
               '                                        B A N C O '.
           03  FILLER                  PIC X(50)           VALUE
               ' B R A D E S C O  S/A                             '.
           03  FILLER                  PIC X(14)           VALUE
               '              '.
           03  CB1-HORA                PIC X(08)           VALUE SPACES.

       01  CABEC2.
           03  CB2-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               'GFCT1360                               GFCT - GEST'.
           03  FILLER                  PIC X(50)           VALUE
               'AO, FLEXIBILIZACAO E COBRANCA DE  TARIFAS         '.
           03  FILLER                  PIC X(28)           VALUE
               '                        PAG.'.
           03  CB2-PAGINA              PIC ZZ99.

       01  CABEC3.
           03  CB3-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                               RELATORIO DE OCORRE'.
           03  FILLER                  PIC X(50)           VALUE
               'NCIAS PARA GERACAO DO ARQUIVO TRIC - FITA CONTABIL'.

       01  CABEC4.
           03  CB4-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '--------------------------------------------------'.
           03  FILLER                  PIC X(50)           VALUE
               '--------------------------------------------------'.
           03  FILLER                  PIC X(32)           VALUE
               '--------------------------------'.

       01  CABEC5.
           03  CB5-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                                  '.
           03  FILLER                  PIC X(17)           VALUE
               '    MOVIMENTO DE '.
           03  CB5-DATA-MOVTO          PIC X(10)           VALUE SPACES.

       01  CABEC6.
           03  CB6-CARRO               PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(50)           VALUE
               'AGENCIA       CONTA     TARIFA      DT-EVENTO     '.
           03  FILLER                  PIC X(29)           VALUE
               'VALOR SOLICITADO     MENSAGEM'.

       01  CABEC7.
           03  CB7-CARRO               PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(50)           VALUE
               '                                    --------------'.
           03  FILLER                  PIC X(44)           VALUE
               '----- TOTALIZADOR FINAL --------------------'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       LINHAS DE DETALHE      *'.
      *----------------------------------------------------------------*

       01  LINDET1.
           03  LD1-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(01)           VALUE SPACES.
           03  LD1-AGENCIA             PIC ZZZZ9.
           03  FILLER                  PIC X(06)           VALUE SPACES.
           03  LD1-CONTA               PIC ZZZZZZ9.
           03  FILLER                  PIC X(06)           VALUE SPACES.
           03  LD1-TARIFA              PIC ZZZZ9.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  LD1-DTA-EVENTO          PIC X(10)           VALUE SPACES.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  LD1-VALOR               PIC Z.ZZZ.ZZZ.ZZ9,99.
           03  FILLER                  PIC X(05)           VALUE SPACES.
           03  LD1-MENSAGEM            PIC X(58)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        LINHAS DE TOTAL       *'.
      *----------------------------------------------------------------*

       01  LINTOT1.
           03  LT1-CARRO               PIC X(01)           VALUE '0'.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE L'.
           03  FILLER                  PIC X(32)           VALUE
               'IDOS DO PREFITA4..............: '.
           03  LT1-QTD-LIDOS           PIC ZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT2.
           03  LT2-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE T'.
           03  FILLER                  PIC X(32)           VALUE
               'ARIFAS PESQ NA GFCTB0D8.......: '.
           03  LT2-QTD-GFCTB0D8        PIC ZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT3.
           03  LT3-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE O'.
           03  FILLER                  PIC X(32)           VALUE
               'CORRENCIAS NO PREFITA4........: '.
           03  LT3-QTD-OCORR           PIC ZZ.ZZZ.ZZZ.ZZ9.

       01  LINTOT4.
           03  LT4-CARRO               PIC X(01)           VALUE ' '.
           03  FILLER                  PIC X(50)           VALUE
               '                                   QUANTIDADE DE G'.
           03  FILLER                  PIC X(32)           VALUE
               'RAVADOS NO ESTRTRIC...........: '.
           03  LT4-QTD-GRAVADOS        PIC ZZ.ZZZ.ZZZ.ZZ9.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA PARA POOL7100      *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
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
                INCLUDE GFCTB047
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS PARA CURSORES      *'.
      *----------------------------------------------------------------*

           EXEC SQL
                DECLARE CSR-GFCTB047 CURSOR FOR
                SELECT  CSERVC_TARIF,
                        CTPO_LCTO_CTBIL,
                        CSEQ_EVNTO_CTBIL,
                        CINDCD_DEPDC_CTBIL,
                        PATRIC_CTBIL,
                        CDEPDC_CTLZO,
                        COPER_CTBIL,
                        CGRP_RZ_CTBIL,
                        CSGRP_RZ_CTBIL,
                        CCTA_EVNTO_CTBIL,
                        CDOCTO_CTBIL
                FROM    DB2PRD.TARIF_EVNTO_CTBIL
                WHERE   CSERVC_TARIF       = :GFCTB047.CSERVC-TARIF
                  AND   CTPO_LCTO_CTBIL    = :GFCTB047.CTPO-LCTO-CTBIL
SP1604       ORDER BY   CTPO_LCTO_CTBIL,
SP1604                  CSEQ_EVNTO_CTBIL
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT1360   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL1050'.

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-VERIFICAR-VAZIO

           PERFORM 30000-PROCESSAR     UNTIL
                   WRK-FS-PREFITA4     EQUAL '10'.

           PERFORM 40000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  PREFITA4
                OUTPUT ESTRTRIC
                       RELATRIC.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 11000-TESTAR-FILE-STATUS.

           PERFORM 12000-OBTER-DATA-HORA.

           PERFORM 13000-OBTER-DATA-ATUAL.

BI0411     MOVE DPROCM-ATUAL OF GFCTB0A1(1:2)  TO WRK-DD-ATU.
BI0411     MOVE DPROCM-ATUAL OF GFCTB0A1(4:2)  TO WRK-MM-ATU.
BI0411     MOVE DPROCM-ATUAL OF GFCTB0A1(7:4)  TO WRK-AAAA-ATU.
BI0411
BI0411     IF   WRK-MMDD-ATU-R   GREATER   0630
BI0411          MOVE   'S2'      TO   WRK-SEMESTRE-ATU
BI0411     ELSE
BI0411          MOVE   'S1'      TO   WRK-SEMESTRE-ATU
BI0411     END-IF.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-TESTAR-FILE-STATUS        SECTION.
      *----------------------------------------------------------------*

           PERFORM 11100-TESTAR-FS-PREFITA4.

           PERFORM 11200-TESTAR-FS-ESTRTRIC.

           PERFORM 11300-TESTAR-FS-RELATRIC.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-TESTAR-FS-PREFITA4        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-PREFITA4         NOT EQUAL ZEROS
               MOVE 'PREFITA4'         TO WRK-ARQUIVO
               MOVE WRK-FS-PREFITA4    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11200-TESTAR-FS-ESTRTRIC        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ESTRTRIC         NOT EQUAL ZEROS
               MOVE 'ESTRTRIC'         TO WRK-ARQUIVO
               MOVE WRK-FS-ESTRTRIC    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       11200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11300-TESTAR-FS-RELATRIC        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATRIC         NOT EQUAL ZEROS
               MOVE 'RELATRIC'         TO WRK-ARQUIVO
               MOVE WRK-FS-RELATRIC    TO WRK-FS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-MSG-FS         TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       11300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-OBTER-DATA-HORA           SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           STRING  WRK-TIMESTAMP(07:02) '/'
                   WRK-TIMESTAMP(05:02) '/'
                   WRK-TIMESTAMP(01:04)
           DELIMITED BY SIZE           INTO CB1-DATA.

           STRING  WRK-TIMESTAMP(09:02) ':'
                   WRK-TIMESTAMP(11:02) ':'
                   WRK-TIMESTAMP(13:02)
           DELIMITED BY SIZE           INTO CB1-HORA.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-OBTER-DATA-ATUAL          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
                SELECT DPROCM_ATUAL
                INTO   :GFCTB0A1.DPROCM-ATUAL
                FROM   DB2PRD.PARM_DATA_PROCM
                WHERE  CSIST_PRINC      = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-VERIFICAR-VAZIO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-LER-PREFITA4.

           IF  WRK-FS-PREFITA4         EQUAL '10'
               DISPLAY '***********************************************'
               DISPLAY '*                                             *'
               DISPLAY '*          PROGRAMA GFCT1360                  *'
               DISPLAY '*          -----------------                  *'
               DISPLAY '*                                             *'
               DISPLAY '*    ARQUIVO PRE-FITA4 ESTA SEM REGISTROS     *'
               DISPLAY '*   --------------------------------------    *'
               DISPLAY '*                                             *'
               DISPLAY '***********************************************'
               PERFORM 40000-FINALIZAR
           END-IF.

           PERFORM 22000-OBTER-CONTABILIDADE.

           MOVE GFCTHF-TARIFA          TO WRK-TARIFA-ANT.

BI0611     PERFORM 2300-0-SELECIONA-TAB-CONTABIL

SP1604     IF  GFCTHF-AGENCIA          NOT EQUAL WRK-AGENCIA-ANT OR
SP1604         GFCTHF-CONTA            NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604         MOVE GFCTHF-AGENCIA     TO WRK-AGENCIA-ANT
SP1604         MOVE GFCTHF-CONTA       TO WRK-CONTA-ANT
SP1604     END-IF.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-LER-PREFITA4              SECTION.
      *----------------------------------------------------------------*

           READ PREFITA4.

           IF  WRK-FS-PREFITA4         EQUAL '10'
               GO                      TO 21000-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 11100-TESTAR-FS-PREFITA4.

           ADD 1                       TO ACU-LIDOS-PREFITA4.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-OBTER-CONTABILIDADE       SECTION.
      *----------------------------------------------------------------*

           PERFORM 22100-OBTER-TARIFA.

           PERFORM 22200-OBTER-HIST-COMPL.

           PERFORM 22300-OBTER-EVENTOS-CTBIL.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22100-OBTER-TARIFA              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTHF-TARIFA          TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
                SELECT CSERVC_TARIF,
P 2287                 CTPO_CTA_DEB_TARIF,
                       CTPO_LCTO_ESTRN,
SP1604                 CLCTO_ESTRN_PJ,
BI0411                 CLCTO_ESTRN_ANTER
                INTO   :GFCTB0D8.CSERVC-TARIF,
P 2287                 :GFCTB0D8.CTPO-CTA-DEB-TARIF,
                       :GFCTB0D8.CTPO-LCTO-ESTRN,
SP1604                 :GFCTB0D8.CLCTO-ESTRN-PJ,
BI0411                 :GFCTB0D8.CLCTO-ESTRN-ANTER
                FROM   DB2PRD.SERVC_TARIF_PRINC
                WHERE  CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0020'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

           ADD 1                       TO ACU-QTD-TARIFAS-PESQ.

      *----------------------------------------------------------------*
       22100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22200-OBTER-HIST-COMPL          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTHF-TARIFA          TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
                SELECT RSEGDA_LIN_EXTRT
                INTO   :GFCTB0A2.RSEGDA-LIN-EXTRT
                FROM   DB2PRD.PARM_SERVC_TARIF
                WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
P 2287            AND  DINIC_VGCIA_TARIF =
P 2287                 (SELECT MAX(DINIC_VGCIA_TARIF)
P 2287                    FROM  DB2PRD.PARM_SERVC_TARIF
P 2287                    WHERE  CSERVC_TARIF  = :GFCTB0A2.CSERVC-TARIF)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'PARM_SERVC_TARIF' TO ERR-DBD-TAB
               MOVE 'SELECT  '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       22200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22300-OBTER-EVENTOS-CTBIL       SECTION.
      *----------------------------------------------------------------*

SP1604*--- INITIALIZE                  TAB1-TARIFA.

SP1604     INITIALIZE                  TAB-PF-TARIFA
SP1604                                 TAB-PJ-TARIFA
BI0411                                 TAB-SA-TARIFA.

SP1604*--- MOVE ZEROS                  TO IND-TARIF.

SP1604     MOVE ZEROS                  TO IND-PF
SP1604                                    IND-PJ
BI0411                                    IND-ANTER.

SP2804     IF  CTPO-LCTO-ESTRN OF GFCTB0D8
SP2804                                 NOT EQUAL ZEROS
               MOVE SPACES             TO WRK-FIM-CURSOR

               PERFORM 22310-OPEN-CURSOR-PF

               PERFORM 22320-FETCH-CURSOR-PF

               IF  WRK-FIM-CURSOR      EQUAL 'S'
020909             MOVE ZEROS          TO CTPO-LCTO-ESTRN OF GFCTB0D8

020909***          MOVE 'DB2'          TO ERR-TIPO-ACESSO
020909***          MOVE 'TARIF_EVNTO_CTBIL'
020909***                              TO ERR-DBD-TAB
020909***          MOVE 'FETCH   '     TO ERR-FUN-COMANDO
020909***          MOVE SQLCODE        TO ERR-SQL-CODE
020909***          MOVE '0040'         TO ERR-LOCAL
020909***          MOVE SPACES         TO ERR-SEGM
020909***
020909***          PERFORM 99999-ROTINA-ERRO
               END-IF

               PERFORM 22330-PROCESSAR-CURSOR-PF UNTIL
                       WRK-FIM-CURSOR  EQUAL 'S'

               PERFORM 22340-CLOSE-CURSOR-PF
           END-IF.

SP1604     IF  CLCTO-ESTRN-PJ OF GFCTB0D8
SP1604                                 NOT EQUAL ZEROS
SP2804         MOVE SPACES             TO WRK-FIM-CURSOR
SP2804
SP2804         PERFORM 22350-OPEN-CURSOR-PJ
SP2804
SP2804         PERFORM 22360-FETCH-CURSOR-PJ
SP2804
SP2804         IF  WRK-FIM-CURSOR      EQUAL 'S'
020909             MOVE  ZEROS         TO CLCTO-ESTRN-PJ OF GFCTB0D8

020909***          MOVE 'DB2'          TO ERR-TIPO-ACESSO
020909***          MOVE 'TARIF_EVNTO_CTBIL'
020909***                              TO ERR-DBD-TAB
020909***          MOVE 'FETCH   '     TO ERR-FUN-COMANDO
020909***          MOVE SQLCODE        TO ERR-SQL-CODE
020909***          MOVE '0050'         TO ERR-LOCAL
020909***          MOVE SPACES         TO ERR-SEGM
020909***
020909***          PERFORM 99999-ROTINA-ERRO
SP2804         END-IF
SP2804
SP2804         PERFORM 22370-PROCESSAR-CURSOR-PJ
SP2804           UNTIL WRK-FIM-CURSOR  EQUAL 'S'
SP2804
SP2804         PERFORM 22380-CLOSE-CURSOR-PJ
SP2804     END-IF.

BI0411     IF  CLCTO-ESTRN-ANTER OF GFCTB0D8
BI0411                                 NOT EQUAL ZEROS
BI0411         MOVE SPACES             TO WRK-FIM-CURSOR
BI0411
BI0411         PERFORM 2241-0-OPEN-CURSOR-ANTER
BI0411
BI0411         PERFORM 2242-0-FETCH-CURSOR-ANTER
BI0411
BI0411         IF  WRK-FIM-CURSOR      EQUAL 'S'
BI0411             MOVE  ZEROS         TO CLCTO-ESTRN-ANTER OF GFCTB0D8
BI0411         END-IF
BI0411
BI0411         PERFORM 2243-0-PROCESSAR-CURSOR-ANTER
BI0411           UNTIL WRK-FIM-CURSOR  EQUAL 'S'
BI0411
BI0411         PERFORM 2244-0-CLOSE-CURSOR-ANTER
BI0411     END-IF.

      *----------------------------------------------------------------*
       22300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22310-OPEN-CURSOR-PF            SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF    OF GFCTB0D8
                                       TO CSERVC-TARIF    OF GFCTB047.
SP1604     MOVE CTPO-LCTO-ESTRN OF GFCTB0D8
SP1604                                 TO CTPO-LCTO-CTBIL OF GFCTB047.

           EXEC SQL
                OPEN CSR-GFCTB047
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'OPEN    '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0060'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       22310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22320-FETCH-CURSOR-PF           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH CSR-GFCTB047
                INTO  :GFCTB047.CSERVC-TARIF,
                      :GFCTB047.CTPO-LCTO-CTBIL,
                      :GFCTB047.CSEQ-EVNTO-CTBIL,
                      :GFCTB047.CINDCD-DEPDC-CTBIL,
                      :GFCTB047.PATRIC-CTBIL,
                      :GFCTB047.CDEPDC-CTLZO,
                      :GFCTB047.COPER-CTBIL,
                      :GFCTB047.CGRP-RZ-CTBIL,
                      :GFCTB047.CSGRP-RZ-CTBIL,
                      :GFCTB047.CCTA-EVNTO-CTBIL,
                      :GFCTB047.CDOCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'FETCH   '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0070'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       22320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22330-PROCESSAR-CURSOR-PF       SECTION.
      *----------------------------------------------------------------*

SP1604     ADD  1                      TO IND-PF.

SP1604     IF  IND-PF                  GREATER 4
               MOVE 'APL'              TO ERR-TIPO-ACESSO
SP1604         MOVE 'MAIS DE 4 OCORRENCIAS NA TABELA GFCTB047 PARA PF'
                                       TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

           PERFORM 22331-CARREGAR-TABELA-PF.

           PERFORM 22320-FETCH-CURSOR-PF.

      *----------------------------------------------------------------*
       22330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22331-CARREGAR-TABELA-PF        SECTION.
      *----------------------------------------------------------------*

SP1604     MOVE CSERVC-TARIF OF GFCTB047
SP1604                                 TO TAB-PF-CSERVC-TARIF(IND-PF).
SP1604     MOVE CTPO-LCTO-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CTPO-LCTO-CTBIL
SP1604                                                         (IND-PF).
SP1604     MOVE CSEQ-EVNTO-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CSEQ-EVNTO-CTBIL
SP1604                                                         (IND-PF).
SP1604     MOVE CINDCD-DEPDC-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CINDCD-DEPDC-CTBIL
SP1604                                                         (IND-PF).
SP1604     MOVE PATRIC-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-PATRIC-CTBIL(IND-PF).
SP1604     MOVE CDEPDC-CTLZO  OF GFCTB047
SP1604                                 TO TAB-PF-CDEPDC-CTLZO(IND-PF).
SP1604     MOVE COPER-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-COPER-CTBIL(IND-PF).
SP1604     MOVE CGRP-RZ-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CGRP-RZ-CTBIL(IND-PF).
SP1604     MOVE CSGRP-RZ-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CSGRP-RZ-CTBIL(IND-PF).
SP1604     MOVE CCTA-EVNTO-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CCTA-EVNTO-CTBIL
SP1604                                                         (IND-PF).
SP1604     MOVE CDOCTO-CTBIL OF GFCTB047
SP1604                                 TO TAB-PF-CDOCTO-CTBIL(IND-PF).

      *----------------------------------------------------------------*
       22331-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22340-CLOSE-CURSOR-PF           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-GFCTB047
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TARIF_EVNTO_CTBIL'
                                       TO ERR-DBD-TAB
               MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '0080'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       22340-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

SP2804*----------------------------------------------------------------*
SP2804 22350-OPEN-CURSOR-PJ            SECTION.
SP2804*----------------------------------------------------------------*
SP2804
SP2804     MOVE CSERVC-TARIF OF GFCTB0D8
SP2804                                 TO CSERVC-TARIF OF GFCTB047.
SP2804     MOVE CLCTO-ESTRN-PJ OF GFCTB0D8
SP2804                                 TO CTPO-LCTO-CTBIL OF GFCTB047.
SP2804
SP2804     EXEC SQL
SP2804          OPEN CSR-GFCTB047
SP2804     END-EXEC.
SP2804
SP2804     IF (SQLCODE                 NOT EQUAL ZEROS) OR
SP2804        (SQLWARN0                EQUAL 'W')
SP2804         MOVE 'DB2'              TO ERR-TIPO-ACESSO
SP2804         MOVE 'TARIF_EVNTO_CTBIL'
SP2804                                 TO ERR-DBD-TAB
SP2804         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
SP2804         MOVE SQLCODE            TO ERR-SQL-CODE
SP2804         MOVE '0090'             TO ERR-LOCAL
SP2804         MOVE SPACES             TO ERR-SEGM
SP2804
SP2804         PERFORM 99999-ROTINA-ERRO
SP2804     END-IF.
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22350-99-FIM.                   EXIT.
SP2804*----------------------------------------------------------------*
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22360-FETCH-CURSOR-PJ           SECTION.
SP2804*----------------------------------------------------------------*
SP2804
SP2804     EXEC SQL
SP2804          FETCH CSR-GFCTB047
SP2804          INTO  :GFCTB047.CSERVC-TARIF,
SP2804                :GFCTB047.CTPO-LCTO-CTBIL,
SP2804                :GFCTB047.CSEQ-EVNTO-CTBIL,
SP2804                :GFCTB047.CINDCD-DEPDC-CTBIL,
SP2804                :GFCTB047.PATRIC-CTBIL,
SP2804                :GFCTB047.CDEPDC-CTLZO,
SP2804                :GFCTB047.COPER-CTBIL,
SP2804                :GFCTB047.CGRP-RZ-CTBIL,
SP2804                :GFCTB047.CSGRP-RZ-CTBIL,
SP2804                :GFCTB047.CCTA-EVNTO-CTBIL,
SP2804                :GFCTB047.CDOCTO-CTBIL
SP2804     END-EXEC.
SP2804
SP2804     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
SP2804        (SQLWARN0                EQUAL 'W')
SP2804         MOVE 'DB2'              TO ERR-TIPO-ACESSO
SP2804         MOVE 'TARIF_EVNTO_CTBIL'
SP2804                                 TO ERR-DBD-TAB
SP2804         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
SP2804         MOVE SQLCODE            TO ERR-SQL-CODE
SP2804         MOVE '0100'             TO ERR-LOCAL
SP2804         MOVE SPACES             TO ERR-SEGM
SP2804
SP2804         PERFORM 99999-ROTINA-ERRO
SP2804     END-IF.
SP2804
SP2804     IF  SQLCODE                 EQUAL +100
SP2804         MOVE 'S'                TO WRK-FIM-CURSOR
SP2804     END-IF.
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22360-99-FIM.                   EXIT.
SP2804*----------------------------------------------------------------*
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22370-PROCESSAR-CURSOR-PJ       SECTION.
SP2804*----------------------------------------------------------------*
SP2804
SP2804     ADD  1                      TO IND-PJ.
SP2804
SP2804     IF  IND-PJ                  GREATER 4
SP2804         MOVE 'APL'              TO ERR-TIPO-ACESSO
SP2804         MOVE 'MAIS DE 4 OCORRENCIAS NA TABELA GFCTB047 PARA PJ'
SP2804                                 TO ERR-TEXTO
SP2804
SP2804         PERFORM 99999-ROTINA-ERRO
SP2804     END-IF.
SP2804
SP2804     PERFORM 22371-CARREGAR-TABELA-PJ.
SP2804
SP2804     PERFORM 22360-FETCH-CURSOR-PJ.
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22370-99-FIM.                   EXIT.
SP2804*----------------------------------------------------------------*
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22371-CARREGAR-TABELA-PJ        SECTION.
SP2804*----------------------------------------------------------------*
SP2804
SP2804     MOVE CSERVC-TARIF OF GFCTB047
SP2804                                 TO TAB-PJ-CSERVC-TARIF(IND-PJ).
SP2804     MOVE CTPO-LCTO-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CTPO-LCTO-CTBIL
SP2804                                                         (IND-PJ).
SP2804     MOVE CSEQ-EVNTO-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CSEQ-EVNTO-CTBIL
SP2804                                                         (IND-PJ).
SP2804     MOVE CINDCD-DEPDC-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CINDCD-DEPDC-CTBIL
SP2804                                                         (IND-PJ).
SP2804     MOVE PATRIC-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-PATRIC-CTBIL(IND-PJ).
SP2804     MOVE CDEPDC-CTLZO  OF GFCTB047
SP2804                                 TO TAB-PJ-CDEPDC-CTLZO(IND-PJ).
SP2804     MOVE COPER-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-COPER-CTBIL(IND-PJ).
SP2804     MOVE CGRP-RZ-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CGRP-RZ-CTBIL(IND-PJ).
SP2804     MOVE CSGRP-RZ-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CSGRP-RZ-CTBIL(IND-PJ).
SP2804     MOVE CCTA-EVNTO-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CCTA-EVNTO-CTBIL
SP2804                                                         (IND-PJ).
SP2804     MOVE CDOCTO-CTBIL OF GFCTB047
SP2804                                 TO TAB-PJ-CDOCTO-CTBIL(IND-PJ).
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22371-99-FIM.                   EXIT.
SP2804*----------------------------------------------------------------*
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22380-CLOSE-CURSOR-PJ           SECTION.
SP2804*----------------------------------------------------------------*
SP2804
SP2804     EXEC SQL
SP2804          CLOSE CSR-GFCTB047
SP2804     END-EXEC.
SP2804
SP2804     IF (SQLCODE                 NOT EQUAL ZEROS) OR
SP2804        (SQLWARN0                EQUAL 'W')
SP2804         MOVE 'DB2'              TO ERR-TIPO-ACESSO
SP2804         MOVE 'TARIF_EVNTO_CTBIL'
SP2804                                 TO ERR-DBD-TAB
SP2804         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
SP2804         MOVE SQLCODE            TO ERR-SQL-CODE
SP2804         MOVE '0110'             TO ERR-LOCAL
SP2804         MOVE SPACES             TO ERR-SEGM
SP2804
SP2804         PERFORM 99999-ROTINA-ERRO
SP2804     END-IF.
SP2804
SP2804*----------------------------------------------------------------*
SP2804 22380-99-FIM.                   EXIT.
SP2804*----------------------------------------------------------------*

BI0411*----------------------------------------------------------------*
BI0411 2241-0-OPEN-CURSOR-ANTER         SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     MOVE CSERVC-TARIF OF GFCTB0D8
BI0411                                 TO CSERVC-TARIF OF GFCTB047.
BI0411     MOVE CLCTO-ESTRN-ANTER OF GFCTB0D8
BI0411                                 TO CTPO-LCTO-CTBIL OF GFCTB047.
BI0411
BI0411     EXEC SQL
BI0411          OPEN CSR-GFCTB047
BI0411     END-EXEC.
BI0411
BI0411     IF (SQLCODE                 NOT EQUAL ZEROS) OR
BI0411        (SQLWARN0                EQUAL 'W')
BI0411         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BI0411         MOVE 'TARIF_EVNTO_CTBIL'
BI0411                                 TO ERR-DBD-TAB
BI0411         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BI0411         MOVE SQLCODE            TO ERR-SQL-CODE
BI0411         MOVE '0120'             TO ERR-LOCAL
BI0411         MOVE SPACES             TO ERR-SEGM
BI0411
BI0411         PERFORM 99999-ROTINA-ERRO
BI0411     END-IF.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2241-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2242-0-FETCH-CURSOR-ANTER        SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     EXEC SQL
BI0411          FETCH CSR-GFCTB047
BI0411          INTO  :GFCTB047.CSERVC-TARIF,
BI0411                :GFCTB047.CTPO-LCTO-CTBIL,
BI0411                :GFCTB047.CSEQ-EVNTO-CTBIL,
BI0411                :GFCTB047.CINDCD-DEPDC-CTBIL,
BI0411                :GFCTB047.PATRIC-CTBIL,
BI0411                :GFCTB047.CDEPDC-CTLZO,
BI0411                :GFCTB047.COPER-CTBIL,
BI0411                :GFCTB047.CGRP-RZ-CTBIL,
BI0411                :GFCTB047.CSGRP-RZ-CTBIL,
BI0411                :GFCTB047.CCTA-EVNTO-CTBIL,
BI0411                :GFCTB047.CDOCTO-CTBIL
BI0411     END-EXEC.
BI0411
BI0411     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BI0411        (SQLWARN0                EQUAL 'W')
BI0411         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BI0411         MOVE 'TARIF_EVNTO_CTBIL'
BI0411                                 TO ERR-DBD-TAB
BI0411         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BI0411         MOVE SQLCODE            TO ERR-SQL-CODE
BI0411         MOVE '0130'             TO ERR-LOCAL
BI0411         MOVE SPACES             TO ERR-SEGM
BI0411
BI0411         PERFORM 99999-ROTINA-ERRO
BI0411     END-IF.
BI0411
BI0411     IF  SQLCODE                 EQUAL +100
BI0411         MOVE 'S'                TO WRK-FIM-CURSOR
BI0411     END-IF.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2242-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2243-0-PROCESSAR-CURSOR-ANTER    SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     ADD  1                      TO IND-ANTER.
BI0411
BI0411     IF  IND-ANTER               GREATER 4
BI0411         MOVE 'APL'              TO ERR-TIPO-ACESSO
BI0411         MOVE 'MAIS DE 4 OCORRENCIAS NA GFCTB047 P/ SEMESTRE ANT.'
BI0411                                 TO ERR-TEXTO
BI0411
BI0411         PERFORM 99999-ROTINA-ERRO
BI0411     END-IF.
BI0411
BI0411     PERFORM 2243-1-CARREGAR-TABELA-ANTER.
BI0411
BI0411     PERFORM 2242-0-FETCH-CURSOR-ANTER.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2243-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2243-1-CARREGAR-TABELA-ANTER     SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     MOVE CSERVC-TARIF OF GFCTB047
BI0411                              TO TAB-SA-CSERVC-TARIF(IND-ANTER).
BI0411     MOVE CTPO-LCTO-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CTPO-LCTO-CTBIL
BI0411                                                    (IND-ANTER).
BI0411     MOVE CSEQ-EVNTO-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CSEQ-EVNTO-CTBIL
BI0411                                                    (IND-ANTER).
BI0411     MOVE CINDCD-DEPDC-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CINDCD-DEPDC-CTBIL
BI0411                                                    (IND-ANTER).
BI0411
BI0411     MOVE PATRIC-CTBIL OF GFCTB047
BI0411                              TO WRK-AUX-3-V-S.
BI0411     MOVE WRK-AUX-3-V         TO TAB-SA-PATRIC-CTBIL(IND-ANTER).
BI0411
BI0411     MOVE CDEPDC-CTLZO  OF GFCTB047
BI0411                              TO TAB-SA-CDEPDC-CTLZO(IND-ANTER).
BI0411     MOVE COPER-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-COPER-CTBIL(IND-ANTER).
BI0411     MOVE CGRP-RZ-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CGRP-RZ-CTBIL(IND-ANTER).
BI0411     MOVE CSGRP-RZ-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CSGRP-RZ-CTBIL(IND-ANTER).
BI0411     MOVE CCTA-EVNTO-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CCTA-EVNTO-CTBIL
BI0411                                                      (IND-ANTER).
BI0411     MOVE CDOCTO-CTBIL OF GFCTB047
BI0411                              TO TAB-SA-CDOCTO-CTBIL(IND-ANTER).
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2243-1-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2244-0-CLOSE-CURSOR-ANTER        SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     EXEC SQL
BI0411          CLOSE CSR-GFCTB047
BI0411     END-EXEC.
BI0411
BI0411     IF (SQLCODE                 NOT EQUAL ZEROS) OR
BI0411        (SQLWARN0                EQUAL 'W')
BI0411         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BI0411         MOVE 'TARIF_EVNTO_CTBIL'
BI0411                                 TO ERR-DBD-TAB
BI0411         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BI0411         MOVE SQLCODE            TO ERR-SQL-CODE
BI0411         MOVE '0140'             TO ERR-LOCAL
BI0411         MOVE SPACES             TO ERR-SEGM
BI0411
BI0411         PERFORM 99999-ROTINA-ERRO
BI0411     END-IF.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2244-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*

SP1604*----------------------------------------------------------------*
SP1604 2300-0-SELECIONA-TAB-CONTABIL    SECTION.
SP1604*----------------------------------------------------------------*

BI0411     IF  (GFCTHF-DATA-DEBITO  EQUAL  SPACES)
BI0411     OR  (GFCTHF-DATA-DEBITO  EQUAL  '01.01.0001')
BI0411         GO   TO   2300-1-MESMO-SEMESTRE
BI0411     END-IF.
BI0411
BI0411     PERFORM 2340-0-VERIFICA-SEMESTRE.
BI0411
BI0411     IF  WRK-AAAA-MOV    EQUAL    WRK-AAAA-ATU
BI0411         IF  WRK-SEMESTRE-MOV   EQUAL   WRK-SEMESTRE-ATU
BI0411             GO   TO   2300-1-MESMO-SEMESTRE
BI0411         END-IF
BI0411     END-IF.
BI0411
BI0411     ADD  1  TO   ACU-SEMESTRE-ANT.
BI0411     PERFORM 2330-0-TAB-CONTABIL-ANTER
BI0411     VARYING IND-ANTER       FROM 1 BY 1
BI0411       UNTIL IND-ANTER       GREATER 4
BI0411     GO   TO   2300-0-99-FIM.
BI0411
BI0411*MESMO SEMESTRE
BI0411 2300-1-MESMO-SEMESTRE.

SP1604     IF  GFCTHF-TIPO-PESSOA      EQUAL 1
SP1604         PERFORM 23100-MONTA-TAB-CONTABIL-PF
SP1604         VARYING IND-PF          FROM 1 BY 1
SP1604           UNTIL IND-PF          GREATER 4
SP1604     ELSE
SP1604         IF  GFCTHF-TIPO-PESSOA  EQUAL 2
SP1604             PERFORM 23200-MONTA-TAB-CONTABIL-PJ
SP1604             VARYING IND-PJ      FROM 1 BY 1
SP1604               UNTIL IND-PJ      GREATER 4
SP1604         END-IF
SP1604     END-IF.
SP1604
SP1604*----------------------------------------------------------------*
SP1604 2300-0-99-FIM.                   EXIT.
SP1604*----------------------------------------------------------------*
SP1604
SP1604*----------------------------------------------------------------*
SP1604 23100-MONTA-TAB-CONTABIL-PF     SECTION.
SP1604*----------------------------------------------------------------*
SP1604
SP1604     MOVE TAB-PF-CSERVC-TARIF(IND-PF)
SP1604                                 TO TAB1-CSERVC-TARIF(IND-PF).
SP1604     MOVE TAB-PF-CTPO-LCTO-CTBIL(IND-PF)
SP1604                                 TO TAB1-CTPO-LCTO-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CSEQ-EVNTO-CTBIL(IND-PF)
SP1604                                 TO TAB1-CSEQ-EVNTO-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CINDCD-DEPDC-CTBIL(IND-PF)
SP1604                                 TO TAB1-CINDCD-DEPDC-CTBIL
SP1604                                                         (IND-PF).
SP1604     MOVE TAB-PF-PATRIC-CTBIL(IND-PF)
SP1604                                 TO TAB1-PATRIC-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CDEPDC-CTLZO(IND-PF)
SP1604                                 TO TAB1-CDEPDC-CTLZO(IND-PF).
SP1604     MOVE TAB-PF-COPER-CTBIL(IND-PF)
SP1604                                 TO TAB1-COPER-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CGRP-RZ-CTBIL(IND-PF)
SP1604                                 TO TAB1-CGRP-RZ-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CSGRP-RZ-CTBIL(IND-PF)
SP1604                                 TO TAB1-CSGRP-RZ-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CCTA-EVNTO-CTBIL(IND-PF)
SP1604                                 TO TAB1-CCTA-EVNTO-CTBIL(IND-PF).
SP1604     MOVE TAB-PF-CDOCTO-CTBIL(IND-PF)
SP1604                                 TO TAB1-CDOCTO-CTBIL(IND-PF).
SP1604
SP1604*----------------------------------------------------------------*
SP1604 23100-99-FIM.                   EXIT.
SP1604*----------------------------------------------------------------*
SP1604
SP1604*----------------------------------------------------------------*
SP1604 23200-MONTA-TAB-CONTABIL-PJ     SECTION.
SP1604*----------------------------------------------------------------*
SP1604
SP1604     MOVE TAB-PJ-CSERVC-TARIF(IND-PJ)
SP1604                                 TO TAB1-CSERVC-TARIF(IND-PJ).
SP1604     MOVE TAB-PJ-CTPO-LCTO-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CTPO-LCTO-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CSEQ-EVNTO-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CSEQ-EVNTO-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CINDCD-DEPDC-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CINDCD-DEPDC-CTBIL
SP1604                                                         (IND-PJ).
SP1604     MOVE TAB-PJ-PATRIC-CTBIL(IND-PJ)
SP1604                                 TO TAB1-PATRIC-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CDEPDC-CTLZO(IND-PJ)
SP1604                                 TO TAB1-CDEPDC-CTLZO(IND-PJ).
SP1604     MOVE TAB-PJ-COPER-CTBIL(IND-PJ)
SP1604                                 TO TAB1-COPER-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CGRP-RZ-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CGRP-RZ-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CSGRP-RZ-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CSGRP-RZ-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CCTA-EVNTO-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CCTA-EVNTO-CTBIL(IND-PJ).
SP1604     MOVE TAB-PJ-CDOCTO-CTBIL(IND-PJ)
SP1604                                 TO TAB1-CDOCTO-CTBIL(IND-PJ).

BI0411*----------------------------------------------------------------*
BI0411 2330-0-TAB-CONTABIL-ANTER  SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     MOVE TAB-SA-CSERVC-TARIF(IND-ANTER)
BI0411                              TO TAB1-CSERVC-TARIF(IND-ANTER).
BI0411     MOVE TAB-SA-CTPO-LCTO-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CTPO-LCTO-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CSEQ-EVNTO-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CSEQ-EVNTO-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CINDCD-DEPDC-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CINDCD-DEPDC-CTBIL
BI0411                                                      (IND-ANTER).
BI0411     MOVE TAB-SA-PATRIC-CTBIL(IND-ANTER)
BI0411                              TO TAB1-PATRIC-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CDEPDC-CTLZO(IND-ANTER)
BI0411                              TO TAB1-CDEPDC-CTLZO(IND-ANTER).
BI0411     MOVE TAB-SA-COPER-CTBIL(IND-ANTER)
BI0411                              TO TAB1-COPER-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CGRP-RZ-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CGRP-RZ-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CSGRP-RZ-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CSGRP-RZ-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CCTA-EVNTO-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CCTA-EVNTO-CTBIL(IND-ANTER).
BI0411     MOVE TAB-SA-CDOCTO-CTBIL(IND-ANTER)
BI0411                              TO TAB1-CDOCTO-CTBIL(IND-ANTER).
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2330-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*

BI0411*----------------------------------------------------------------*
BI0411 2340-0-VERIFICA-SEMESTRE   SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     MOVE GFCTHF-DATA-DEBITO(1:2)        TO WRK-DD-MOV.
BI0411     MOVE GFCTHF-DATA-DEBITO(4:2)        TO WRK-MM-MOV.
BI0411     MOVE GFCTHF-DATA-DEBITO(7:4)        TO WRK-AAAA-MOV.
BI0411
BI0411     IF   WRK-MMDD-MOV-R   GREATER   0630
BI0411          MOVE   'S2'      TO   WRK-SEMESTRE-MOV
BI0411     ELSE
BI0411          MOVE   'S1'      TO   WRK-SEMESTRE-MOV
BI0411     END-IF.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2340-0-99-FIM.                   EXIT.
BI0411*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF  GFCTHF-TARIFA           NOT EQUAL WRK-TARIFA-ANT
               PERFORM 22000-OBTER-CONTABILIDADE

               MOVE GFCTHF-TARIFA      TO WRK-TARIFA-ANT
           END-IF.

BI0611         PERFORM 2300-0-SELECIONA-TAB-CONTABIL

SP1604     IF  GFCTHF-AGENCIA          NOT EQUAL WRK-AGENCIA-ANT OR
SP1604         GFCTHF-CONTA            NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604         MOVE GFCTHF-AGENCIA     TO WRK-AGENCIA-ANT
SP1604         MOVE GFCTHF-CONTA       TO WRK-CONTA-ANT
SP1604     END-IF.

           MOVE SPACES                 TO WRK-MENSAGEM.

           EVALUATE TRUE
SP1604       WHEN  GFCTHF-TIPO-PESSOA  EQUAL 3
SP1604             MOVE '4'            TO WRK-MENSAGEM
SP1604
SP1604             PERFORM 31000-MONTAR-RELATORIO
SP1604               UNTIL WRK-FS-PREFITA4
SP1604                                 EQUAL '10'
SP1604                  OR GFCTHF-AGENCIA
SP1604                                 NOT EQUAL WRK-AGENCIA-ANT
SP1604                  OR GFCTHF-CONTA
SP1604                                 NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604             GO                  TO 30000-99-FIM

SP1604       WHEN  GFCTHF-TIPO-PESSOA  EQUAL 4
SP1604             MOVE '5'            TO WRK-MENSAGEM
SP1604
SP1604             PERFORM 31000-MONTAR-RELATORIO
SP1604               UNTIL WRK-FS-PREFITA4
SP1604                                 EQUAL '10'
SP1604                  OR GFCTHF-AGENCIA
SP1604                                 NOT EQUAL WRK-AGENCIA-ANT
SP1604                  OR GFCTHF-CONTA
SP1604                                 NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604             GO                  TO 30000-99-FIM

SP1604       WHEN (GFCTHF-TIPO-PESSOA  EQUAL 1 AND
SP1604             CTPO-LCTO-ESTRN     EQUAL ZEROS) OR
SP1604            (GFCTHF-TIPO-PESSOA  EQUAL 2 AND
SP1604             CLCTO-ESTRN-PJ      EQUAL ZEROS)
SP1604             MOVE '6'            TO WRK-MENSAGEM
SP1604
SP1604             PERFORM 31000-MONTAR-RELATORIO
SP1604               UNTIL WRK-FS-PREFITA4
SP1604                                 EQUAL '10'
SP1604                  OR GFCTHF-AGENCIA
SP1604                                 NOT EQUAL WRK-AGENCIA-ANT
SP1604                  OR GFCTHF-CONTA
SP1604                                 NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604             GO                  TO 30000-99-FIM

SP1604       WHEN (TAB1-CINDCD-DEPDC-CTBIL(1)
SP1604                                 EQUAL 1 OR
SP1604             TAB1-CINDCD-DEPDC-CTBIL(2)
SP1604                                 EQUAL 1 OR
SP1604             TAB1-CINDCD-DEPDC-CTBIL(3)
SP1604                                 EQUAL 1 OR
SP1604             TAB1-CINDCD-DEPDC-CTBIL(4)
SP1604                                 EQUAL 1) AND
SP1604            (CTPO-CTA-DEB-TARIF OF GFCTB0D8
SP1604                                 EQUAL 1 AND
SP1604             GFCTHF-TIPO-CONTA   NOT EQUAL 1)
SP1604             MOVE '7'            TO WRK-MENSAGEM
SP1604
SP1604             PERFORM 31000-MONTAR-RELATORIO
SP1604               UNTIL WRK-FS-PREFITA4
SP1604                                 EQUAL '10'
SP1604                  OR GFCTHF-AGENCIA
SP1604                                 NOT EQUAL WRK-AGENCIA-ANT
SP1604                  OR GFCTHF-CONTA
SP1604                                 NOT EQUAL WRK-CONTA-ANT
SP1604
SP1604             GO                  TO 30000-99-FIM

             WHEN (TAB1-CINDCD-DEPDC-CTBIL(1)
                                       EQUAL ZEROS) AND
                  (TAB1-CINDCD-DEPDC-CTBIL(2)
                                       EQUAL ZEROS) AND
                  (TAB1-CINDCD-DEPDC-CTBIL(3)
                                       EQUAL ZEROS) AND
                  (TAB1-CINDCD-DEPDC-CTBIL(4)
                                       EQUAL ZEROS)
                   MOVE '1'            TO WRK-MENSAGEM

                   PERFORM 31000-MONTAR-RELATORIO UNTIL
                     (WRK-FS-PREFITA4  EQUAL '10') OR
                     (GFCTHF-TARIFA    NOT EQUAL WRK-TARIFA-ANT)

                   GO                  TO 30000-99-FIM

             WHEN ((TAB1-CINDCD-DEPDC-CTBIL(1)
                                       EQUAL 3)      AND
                   (TAB1-CDEPDC-CTLZO(1)
                                       EQUAL ZEROS)) OR
                  ((TAB1-CINDCD-DEPDC-CTBIL(2)
                                       EQUAL 3)      AND
                   (TAB1-CDEPDC-CTLZO(2)
                                       EQUAL ZEROS)) OR
                  ((TAB1-CINDCD-DEPDC-CTBIL(3)
                                       EQUAL 3)      AND
                   (TAB1-CDEPDC-CTLZO(3)
                                       EQUAL ZEROS)) OR
                  ((TAB1-CINDCD-DEPDC-CTBIL(4)
                                       EQUAL 3)      AND
                   (TAB1-CDEPDC-CTLZO(4)
                                       EQUAL ZEROS))
                   MOVE '2'            TO WRK-MENSAGEM

                   PERFORM 31000-MONTAR-RELATORIO UNTIL
                     (WRK-FS-PREFITA4  EQUAL '10') OR
                     (GFCTHF-TARIFA    NOT EQUAL WRK-TARIFA-ANT)

                   GO                  TO 30000-99-FIM

      *----------------------------------------------------------------*
      *--- CONDICAO INIBIDA APOS PEDIDO DO GESTOR DE TARIFAS, PARA QUE *
      *--- O ESTORNO SEM  REGISTROS  ASSUMA  A  AGENCIA  DESTINO  COMO *
      *--- AGENCIA ORIGEM. EM 02/02/2006.                              *
      *----------------------------------------------------------------*

             WHEN OTHER

                   PERFORM 32000-MONTAR-SAIDA

                   PERFORM 33000-GRAVAR-ESTRTRIC

                   PERFORM 21000-LER-PREFITA4
           END-EVALUATE.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31000-MONTAR-RELATORIO          SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 31100-IMPRIMIR-CABEC

               PERFORM 31200-IMPRIMIR-CABEC-DET
           END-IF.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE GFCTHF-AGENCIA         TO LD1-AGENCIA.
           MOVE GFCTHF-CONTA           TO LD1-CONTA.
           MOVE GFCTHF-TARIFA          TO LD1-TARIFA.
           MOVE GFCTHF-DATA-EVENTO(1:2)
                                       TO WRK-DD.
           MOVE GFCTHF-DATA-EVENTO(4:2)
                                       TO WRK-MM.
           MOVE GFCTHF-DATA-EVENTO(7:4)
                                       TO WRK-AAAA.
           MOVE WRK-DATA-AUX           TO LD1-DTA-EVENTO.
           MOVE GFCTHF-VALOR           TO LD1-VALOR.

           EVALUATE WRK-MENSAGEM
               WHEN '1'
                   MOVE 'NAO FOI DEFINIDO AGENCIA DESTINO PARA ESTA TARI
      -                 'FA'           TO LD1-MENSAGEM

               WHEN '2'
                   MOVE 'NAO FOI DEFINIDO AGENCIA CENTRALIZADORA PARA ES
      -                 'TA TARIFA'    TO LD1-MENSAGEM

      *----------------------------------------------------------------*
      *--- CONDICAO INIBIDA APOS PEDIDO DO GESTOR DE TARIFAS, PARA QUE *
      *--- O ESTORNO SEM  REGISTROS  ASSUMA  A  AGENCIA  DESTINO  COMO *
      *--- AGENCIA ORIGEM. EM 02/02/2006.                              *
      *----------------------------------------------------------------*

SP1604         WHEN '4'
SP1604             MOVE 'ERRO NA ROTINA CLIE - CLIENTE NAO ENCONTRADO'
SP1604                                 TO LD1-MENSAGEM
SP1604
SP1604         WHEN '5'
SP1604             MOVE 'ERRO NA ROTINA CLIE - CLIENTE SEM CNPJ/CPF'
SP1604                                 TO LD1-MENSAGEM
SP1604
SP1604         WHEN '6'
SP1604             MOVE 'TIPO DE LANCAMENTO CONTABIL IGUAL A ZEROS'
SP1604                                 TO LD1-MENSAGEM
SP1604
SP1604         WHEN '7'
SP1604             MOVE 'TIPO CTA DEB TARIFA DIVERGE DO TIPO PESSOA DA R
SP1604-                 'OTINA CLIE'   TO LD1-MENSAGEM

           END-EVALUATE.

           WRITE REG-RELATRIC          FROM LINDET1.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           ADD 1                       TO ACU-LINHAS
                                          ACU-QTD-OCORS-PREFITA4.

           PERFORM 21000-LER-PREFITA4.

      *----------------------------------------------------------------*
       31000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31100-IMPRIMIR-CABEC            SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINA.

           MOVE ACU-PAGINA             TO CB2-PAGINA.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
           MOVE DPROCM-ATUAL   OF GFCTB0A1(1:2)
                                       TO WRK-DD.
           MOVE DPROCM-ATUAL   OF GFCTB0A1(4:2)
                                       TO WRK-MM.
           MOVE DPROCM-ATUAL   OF GFCTB0A1(7:4)
                                       TO WRK-AAAA.
           MOVE WRK-DATA-AUX           TO CB5-DATA-MOVTO.

           WRITE REG-RELATRIC          FROM CABEC1.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM CABEC2.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM CABEC3.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM CABEC4.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM CABEC5.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM CABEC4.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           MOVE 7                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       31100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       31200-IMPRIMIR-CABEC-DET        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATRIC          FROM CABEC6.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATRIC          FROM CABEC4.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           ADD 3                       TO ACU-LINHAS.

      *----------------------------------------------------------------*
       31200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32000-MONTAR-SAIDA              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  GFCT-REG.

           MOVE 'GFCT'                 TO GFCT-PRODUTO.
           MOVE TAB1-COPER-CTBIL(1)    TO GFCT-OPERACAO.
           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2)
                                       TO WRK-DTA-DD.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2)
                                       TO WRK-DTA-MM.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4)
                                       TO WRK-DTA-AAAA.
           MOVE WRK-DATA-AAAAMMDD-R    TO GFCT-DATA-MOV.
           MOVE GFCTHF-VALOR           TO GFCT-VALOR-1.

           IF  TAB1-PATRIC-CTBIL(1)    NOT EQUAL ZEROS
               COMPUTE WRK-VALOR       = GFCTHF-VALOR
                                       * TAB1-PATRIC-CTBIL(1)
                                       / 100
               MOVE WRK-VALOR          TO GFCT-VALOR-2
           END-IF.

           IF  TAB1-PATRIC-CTBIL(2)    NOT EQUAL ZEROS
               COMPUTE WRK-VALOR       = GFCTHF-VALOR
                                       * TAB1-PATRIC-CTBIL(2)
                                       / 100
               MOVE WRK-VALOR          TO GFCT-VALOR-3
           END-IF.

           IF  TAB1-PATRIC-CTBIL(3)    NOT EQUAL ZEROS
               COMPUTE WRK-VALOR       = GFCTHF-VALOR
                                       * TAB1-PATRIC-CTBIL(3)
                                       / 100
               MOVE WRK-VALOR          TO GFCT-VALOR-4
           END-IF.

           IF  TAB1-PATRIC-CTBIL(4)    NOT EQUAL ZEROS
               COMPUTE WRK-VALOR       = GFCTHF-VALOR
                                       * TAB1-PATRIC-CTBIL(4)
                                       / 100
               MOVE WRK-VALOR          TO GFCT-VALOR-5
           END-IF.

           COMPUTE WRK-VALOR-DIF       = GFCT-VALOR-1 -
                                         (GFCT-VALOR-2 + GFCT-VALOR-3 +
                                          GFCT-VALOR-4 + GFCT-VALOR-5).

           IF  WRK-VALOR-DIF           NOT EQUAL ZEROS
               IF  TAB1-PATRIC-CTBIL(4)
                                       NOT EQUAL ZEROS
                   ADD   WRK-VALOR-DIF TO GFCT-VALOR-5
               ELSE
                   IF  TAB1-PATRIC-CTBIL(3)
                                       NOT EQUAL ZEROS
                       ADD   WRK-VALOR-DIF
                                       TO GFCT-VALOR-4
                   ELSE
                       IF  TAB1-PATRIC-CTBIL(2)
                                       NOT EQUAL ZEROS
                           ADD   WRK-VALOR-DIF
                                       TO GFCT-VALOR-3
                       ELSE
                           IF  TAB1-PATRIC-CTBIL(1)
                                       NOT EQUAL ZEROS
                               ADD   WRK-VALOR-DIF
                                       TO GFCT-VALOR-2
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

           MOVE GFCTHF-AGENCIA         TO GFCT-AG-ORIG
                                          WRK-AGENCIA.

           PERFORM 32100-OBTER-DIGITO.

           MOVE WRK-DIGITO             TO GFCT-DAC-ORIG.
           MOVE 'N'                    TO GFCT-ESTORNO.

           INITIALIZE                  TAB2-TRIC.
           MOVE ZEROS                  TO IND-TRIC.

           PERFORM 32200-TRATAR-EVENTOS VARYING
                   IND-TARIF           FROM 1 BY 1 UNTIL
                   IND-TARIF           GREATER 4.

           MOVE TAB2-TRIC              TO GFCT-EVENTOS.

      *----------------------------------------------------------------*
       32000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32100-OBTER-DIGITO              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-DIGITO.
           MOVE +4                     TO WRK-TAMANHO.

           CALL  'POOL1260'            USING WRK-AGENCIA
                                             WRK-DIGITO
                                             WRK-TAMANHO.

           IF  WRK-DIGITO              EQUAL  '.'
               DISPLAY '************ GFCT1360 ************'
               DISPLAY '*                                *'
               DISPLAY '*   ERRO NO ACESSO A POOL1260    *'
               DISPLAY '*    PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT1360 ************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO ACESSO A POOL1260'
                                       TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       32100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32200-TRATAR-EVENTOS            SECTION.
      *----------------------------------------------------------------*

           IF  TAB1-CSERVC-TARIF(IND-TARIF)
                                       EQUAL ZEROS
               MOVE 5                  TO IND-TARIF

               GO                      TO 32200-99-FIM
           END-IF.

           EVALUATE TAB1-CINDCD-DEPDC-CTBIL(IND-TARIF)
             WHEN 1

               ADD 1                   TO IND-TRIC
               MOVE GFCTHF-AGENCIA     TO TAB2-AG-DESTINO(IND-TRIC)
                                          WRK-AGENCIA

               PERFORM 32100-OBTER-DIGITO

               MOVE WRK-DIGITO         TO TAB2-DAC-AG(IND-TRIC)

P 2287         IF  CTPO-CTA-DEB-TARIF OF GFCTB0D8  EQUAL  2
P 2287             MOVE 010            TO TAB2-GRP-RZ(IND-TRIC)
P 2287             MOVE 051            TO TAB2-SBG-RZ(IND-TRIC)
               ELSE
SP1604             MOVE GFCTHF-GRUPO-RAZAO
SP1604                                 TO TAB2-GRP-RZ(IND-TRIC)
SP1604             MOVE GFCTHF-SUB-GRUPO-RAZAO
SP1604                                 TO TAB2-SBG-RZ(IND-TRIC)
               END-IF

               MOVE GFCTHF-CONTA       TO TAB2-CONTA(IND-TRIC)
                                          WRK-CONTA

               PERFORM 32220-OBTER-DIGITO-CONTA

               MOVE WRK-DIGITO-CTA     TO TAB2-DAC-CONTA(IND-TRIC)
               MOVE RSEGDA-LIN-EXTRT OF GFCTB0A2
                                       TO TAB2-HIST-COMPL(IND-TRIC)

               IF  TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       EQUAL ZEROS
SP7008             IF GFCTHF-CONSOLIDADO  EQUAL  2
SP7008                 STRING '0'
SP7008                        GFCTHF-DATA-DEBITO(1:2)
SP7008                        GFCTHF-DATA-DEBITO(4:2)
SP7008                        GFCTHF-DATA-DEBITO(9:2)
SP7008                 DELIMITED BY SIZE INTO WRK-NRO-DOC
SP7008
SP7008                 MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
SP7008             ELSE
                       STRING '0'
                              GFCTHF-DATA-EVENTO(1:2)
                              GFCTHF-DATA-EVENTO(4:2)
                              GFCTHF-DATA-EVENTO(9:2)
                       DELIMITED BY SIZE INTO WRK-NRO-DOC

                       MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
                   END-IF
               ELSE
                   MOVE TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       TO TAB2-NRO-DOC(IND-TRIC)
               END-IF

             WHEN 2

               ADD 1                   TO IND-TRIC

      *----------------------------------------------------------------*
      *--- CONDICAO ALTERADA APOS PEDIDO DO GESTOR DE TARIFAS,  PARA   *
      *--- QUE O ESTORNO SEM REGISTROS ASSUMA A AGENCIA DESTINO COMO   *
      *--- AGENCIA ORIGEM. EM 02/02/2006.                              *
      *----------------------------------------------------------------*
               IF  GFCTHF-CD-TIPO-SOLTC-ESTRN
                                       EQUAL 2
                   MOVE GFCTHF-AGENCIA TO TAB2-AG-DESTINO(IND-TRIC)
                                          WRK-AGENCIA
               ELSE
                   MOVE GFCTHF-AGENCIA-ORIGEM
                                       TO TAB2-AG-DESTINO(IND-TRIC)
                                          WRK-AGENCIA
               END-IF

               PERFORM 32100-OBTER-DIGITO

               MOVE WRK-DIGITO         TO TAB2-DAC-AG(IND-TRIC)
               MOVE TAB1-CGRP-RZ-CTBIL(IND-TARIF)
                                       TO TAB2-GRP-RZ(IND-TRIC)
               MOVE TAB1-CSGRP-RZ-CTBIL(IND-TARIF)
                                       TO TAB2-SBG-RZ(IND-TRIC)
               MOVE TAB1-CCTA-EVNTO-CTBIL(IND-TARIF)
                                       TO TAB2-CONTA(IND-TRIC)
                                          WRK-CONTA

               PERFORM 32220-OBTER-DIGITO-CONTA

               MOVE WRK-DIGITO-CTA     TO TAB2-DAC-CONTA(IND-TRIC)
               MOVE RSEGDA-LIN-EXTRT OF GFCTB0A2
                                       TO TAB2-HIST-COMPL(IND-TRIC)
               IF  TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       EQUAL ZEROS
SP7008             IF GFCTHF-CONSOLIDADO  EQUAL  2
SP7008                 STRING '0'
SP7008                        GFCTHF-DATA-DEBITO(1:2)
SP7008                        GFCTHF-DATA-DEBITO(4:2)
SP7008                        GFCTHF-DATA-DEBITO(9:2)
SP7008                 DELIMITED BY SIZE INTO WRK-NRO-DOC
SP7008
SP7008                 MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
SP7008             ELSE
                       STRING '0'
                              GFCTHF-DATA-EVENTO(1:2)
                              GFCTHF-DATA-EVENTO(4:2)
                              GFCTHF-DATA-EVENTO(9:2)
                       DELIMITED BY SIZE INTO WRK-NRO-DOC

                       MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
                   END-IF
               ELSE
                   MOVE TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       TO TAB2-NRO-DOC(IND-TRIC)
               END-IF

             WHEN 3
               ADD 1                   TO IND-TRIC

               MOVE TAB1-CDEPDC-CTLZO(IND-TARIF)
                                       TO TAB2-AG-DESTINO(IND-TRIC)
                                          WRK-AGENCIA

               PERFORM 32100-OBTER-DIGITO

               MOVE WRK-DIGITO         TO TAB2-DAC-AG(IND-TRIC)
               MOVE TAB1-CGRP-RZ-CTBIL(IND-TARIF)
                                       TO TAB2-GRP-RZ(IND-TRIC)
               MOVE TAB1-CSGRP-RZ-CTBIL(IND-TARIF)
                                       TO TAB2-SBG-RZ(IND-TRIC)
               MOVE TAB1-CCTA-EVNTO-CTBIL(IND-TARIF)
                                       TO TAB2-CONTA(IND-TRIC)
                                          WRK-CONTA

               PERFORM 32220-OBTER-DIGITO-CONTA

               MOVE WRK-DIGITO-CTA     TO TAB2-DAC-CONTA(IND-TRIC)
               MOVE RSEGDA-LIN-EXTRT OF GFCTB0A2
                                       TO TAB2-HIST-COMPL(IND-TRIC)

               IF  TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       EQUAL ZEROS
SP7008             IF GFCTHF-CONSOLIDADO  EQUAL  2
SP7008                 STRING '0'
SP7008                        GFCTHF-DATA-DEBITO(1:2)
SP7008                        GFCTHF-DATA-DEBITO(4:2)
SP7008                        GFCTHF-DATA-DEBITO(9:2)
SP7008                 DELIMITED BY SIZE INTO WRK-NRO-DOC
SP7008
SP7008                 MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
SP7008             ELSE
                       STRING '0'
                              GFCTHF-DATA-EVENTO(1:2)
                              GFCTHF-DATA-EVENTO(4:2)
                              GFCTHF-DATA-EVENTO(9:2)
                       DELIMITED BY SIZE INTO WRK-NRO-DOC

                       MOVE WRK-NRO-DOC TO TAB2-NRO-DOC(IND-TRIC)
                   END-IF
               ELSE
                   MOVE TAB1-CDOCTO-CTBIL(IND-TARIF)
                                       TO TAB2-NRO-DOC(IND-TRIC)
               END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       32200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       32220-OBTER-DIGITO-CONTA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-DIGITO-CTA.
           MOVE +4                     TO WRK-TAMANHO.

           CALL  'POOL0431'            USING WRK-CONTA
                                             WRK-DIGITO-CTA
                                             WRK-TAMANHO.

           IF  WRK-DIGITO-CTA          EQUAL  '.'
               DISPLAY '************ GFCT1360 ************'
               DISPLAY '*                                *'
               DISPLAY '*   ERRO NO ACESSO A POOL0431    *'
               DISPLAY '*    PROCESSAMENTO CANCELADO     *'
               DISPLAY '*                                *'
               DISPLAY '************ GFCT1360 ************'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO ACESSO A POOL0431'
                                       TO ERR-TEXTO

               PERFORM 99999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       32220-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       33000-GRAVAR-ESTRTRIC           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE GFCT-REG.

           PERFORM 11200-TESTAR-FS-ESTRTRIC.

           ADD 1                       TO ACU-GRAVA-ESTRTRIC.

      *----------------------------------------------------------------*
       33000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       40000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 41000-EMITIR-DISPLAY.

           PERFORM 42000-IMPRIMIR-LINTOT.

           CLOSE PREFITA4
                 ESTRTRIC
                 RELATRIC.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 11000-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       40000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       41000-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************************************************
      -            '***'.
           DISPLAY '*
      -            '  *'.
           DISPLAY '*                 PROGRAMA GFCT1360
      -            '  *'.
           DISPLAY '*                 -----------------
      -            '  *'.
           DISPLAY '*
      -            '  *'.
           DISPLAY '*             PROCESSAMENTO EFETUADO COM SUCESSO
      -            '  *'.
           DISPLAY '*             ----------------------------------
      -            '  *'.
           DISPLAY '*
      -            '  *'.

           MOVE ACU-LIDOS-PREFITA4     TO WRK-MASCARA.

           DISPLAY '* QTDE DE LIDOS DO PREFITA4        = ' WRK-MASCARA
                   '  *'.

           MOVE ACU-QTD-TARIFAS-PESQ   TO WRK-MASCARA.

           DISPLAY '* QTDE DE TARIFAS PESQ NA GFCTB0D8 = ' WRK-MASCARA
                   '  *'.

           MOVE ACU-QTD-OCORS-PREFITA4 TO WRK-MASCARA.

           DISPLAY '* QTDE DE OCORRENCIAS NO PREFITA4  = ' WRK-MASCARA
                   '  *'.

           MOVE ACU-GRAVA-ESTRTRIC     TO WRK-MASCARA.

           DISPLAY '* QTDE DE GRAVADOS NO ESTRTRIC     = ' WRK-MASCARA
                   '  *'.

BI0411     MOVE ACU-SEMESTRE-ANT       TO WRK-MASCARA.
BI0411
BI0411     DISPLAY '* QTDE DE TARIFAS SEMESTRE ANTERIOR= ' WRK-MASCARA
BI0411             '  *'.
           DISPLAY '****************************************************
      -            '***'.

      *----------------------------------------------------------------*
       41000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       42000-IMPRIMIR-LINTOT           SECTION.
      *----------------------------------------------------------------*

           PERFORM 31100-IMPRIMIR-CABEC.

           PERFORM 42100-IMPRIMIR-CABEC-TOT.

           MOVE ACU-LIDOS-PREFITA4     TO LT1-QTD-LIDOS.
           MOVE ACU-QTD-TARIFAS-PESQ   TO LT2-QTD-GFCTB0D8.
           MOVE ACU-QTD-OCORS-PREFITA4 TO LT3-QTD-OCORR.
           MOVE ACU-GRAVA-ESTRTRIC     TO LT4-QTD-GRAVADOS.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATRIC          FROM LINTOT1.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM LINTOT2.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM LINTOT3.

           PERFORM 11300-TESTAR-FS-RELATRIC.

           WRITE REG-RELATRIC          FROM LINTOT4.

           PERFORM 11300-TESTAR-FS-RELATRIC.

      *----------------------------------------------------------------*
       42000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       42100-IMPRIMIR-CABEC-TOT        SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE REG-RELATRIC          FROM CABEC7.

           PERFORM 11300-TESTAR-FS-RELATRIC.

      *----------------------------------------------------------------*
       42100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       99999-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1360'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       99999-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
