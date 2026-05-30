      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0883.
       AUTHOR.     MARCELO FARIA.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0883                                    *
      *    PROGRAMADOR.:   MARCELO FARIA           - CPM PATO BRANCO   *
      *    ANALISTA CPM:   ELIAS BOSCATO           - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK GP/75   *
      *    DATA........:   09/05/2006                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - LISTA DE CONSULTA DE PEDI- *
      *        DOS DE FLEXIBILIZACAO PARA AGRUPAMENTO.                 *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
      *                    DB2PRD.PDIDO_FLEXZ_AGPTO      GFCTB0B2      *
      *                    DB2PRD.PDIDO_FLEXZ_CLI        GFCTB0B3      *
      *                    DB2PRD.PDIDO_FLEXZ_AG         GFCTB0B1      *
      *                    DB2PRD.PDIDO_FLEXZ_SGMTO      GFCTB0C0      *
      *                    DB2PRD.PDIDO_FLEXZ_PAB        GFCTB0B8      *
      *                    DB2PRD.PDIDO_FLEXZ_PSTAL      GFCTB0B9      *
      *                    DB2PRD.PDIDO_FLEXZ_MUN        GFCTB0B7      *
      *                    DB2PRD.PDIDO_FLEXZ_UF         GFCTB0C1      *
0410  *                    DB2PRD.TPDIDO_FLEXZ_RZ        GFCTB0M7      *
110510*                    DB2PRD.RAZAO_CONTABIL         PLCCB004      *
      *                    DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5      *
      *                    DB2PRD.V01UF                  DCITV002      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
0212  *                    DB2PRD.PARM_SERVC_TARIF       GFCTB0A2      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTNJ - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNK - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTG2 - AREA DE COMUNICACAO - ENTRADA (GFCT5523)         *
      *    I#GFCTG3 - AREA DE COMUNICACAO - SAIDA   (GFCT5523)         *
      *    I#GFCTFZ - AREA DE COMUNICACAO - ENTRADA (GFCT5522)         *
      *    I#GFCTG1 - AREA DE COMUNICACAO - SAIDA   (GFCT5522)         *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA (GFCT5006)         *
      *    I#GFCT1X - AREA DE COMUNICACAO - ENTRADA (GFCT5006)         *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA (GFCT5109-GFCT5090)*
      *    I#GFCTGC - AREA DE COMUNICACAO - ENTRADA (GFCT5109-GFCT5090)*
      *    I#RURC88 - AREA DE COMUNICACAO COM MODULO RURC9020          *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCA DINAMICAMENTE UM MODULO                    *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    GFCT5090 - DESCRICAO POSTO SERVICO                          *
      *    GFCT5109 - OBTER DESCRICAO DE PAB                           *
      *    GFCT5505 - OBTER CODIGO DE JUNCAO                           *
      *    GFCT5522 - VERIFICA ON-LINE                                 *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS                     *
      *    DCIT8000 - VERIFICAR UF                                     *
      *    RURC9020 - VERIFICAR MUNICIPIO                              *
      *                                                                *
      *================================================================*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *                                                                *
      *----------------------------------------------------------------*
0212  *    ULTIMA ALTERACAO EM 02/12/2009 - HELIO SANTONI              *
0212  *    ALTERACOES NA LISTA DE FLEXIBILIZACOES DA CONSULTA HISTORICO*
0212  *    - TRATAMENTO DA COMBO CONJUNTO DE SERVICOS                  *
0212  *    - INCLUIR TRATAMENTO DA TABELA GFCTB0A2                     *
      *                                                                *
      *----------------------------------------------------------------*
0410  *    ULTIMA ALTERACAO EM 16/04/2010 - HELIO SANTONI NETO         *
0410  *    ALTERACOES REFERENTES AOS AGRUPAMENTOS 20 E 26.             *
      *                                                                *
BI0810*----------------------------------------------------------------*
BI0810*    ULTIMA ALTERACAO EM 12/08/2010 - BIRA                       *
BI0810*    FLEXIBILIZACAO PARA AGRUPAMENTO - CONSULTA HISTORICO DE PED.*
BI0810*    - ACERTO NO RESTART PARA NAO PERDER O ULT. REG. NA TELA.    *
BI0810*                                                                *
BI1211*----------------------------------------------------------------*
BI1211*    ULTIMA ALTERACAO EM 26/12/2011 - BIRA                       *
BI1211*    - AUMENTAR O TAMANHO DOS INDEXADORES PARA SUPORTAR VOLUME   *
BI1211*      MAIOR QUE 999 OCORRENCIAS (PGM EM LOOP).                  *
BI1211*                                                                *
BI1211*    - CORRECAO NO RESTART PARA NAO DUPLICAR REGISTROS           *
BI1211*      CONSULTADOS.                                              *
BI1211*                                                                *
BI1211*----------------------------------------------------------------*
060916*    ULTIMA ALTERACAO EM 06/09/2016 - VALERIA                    *
060916*    - PROCESSAR CURSORES 07 / 08 / 09 PARA OS AGRUPAMENTOS      *
060916*      SEM O COMBO DE CONJUNTO DE SERVICOS.                      *
060916*                                                                *
060916*    - CORRECAO NO RESTART PARA NAO PERDER REGISTROS QUANDO      *
060916*      NAO TEM O COMBO CONJUNTO DE SERVICOS.                     *
060916*                                                                *
VI0618*----------------------------------------------------------------*
VI0618*    ULTIMA ALTERACAO EM 04/06/2018 - VINICIUS CURTO             *
VI0618*    - CORRECAO DO PROCESSO DE RESTART/DESPREZA PARA OS          *
VI0618*      ARGUMENTOS DOS AGRUPAMENTOS!!!                            *
VI0618*                                                                *
VI0618*    OBS. SOMENTE DOS AGRUPAMERNTOS QUE POSSUAM ARQGUMENTOS!     *
VI0618*                                                                *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
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

060916 01  WRK-TARIFA-INI              PIC S9(5)V COMP-3   VALUE ZEROS.
060916 01  WRK-TARIFA-FIM              PIC S9(5)V COMP-3   VALUE 99999.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(009) COMP-3  VALUE ZEROS.
VI0618 77  IND-LIDOS                   PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA ACUMULADORES *'.
      *----------------------------------------------------------------*

VI0618 77  ACU-DESPREZA                PIC  9(007) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREAS AUXILIARES *'.
      *----------------------------------------------------------------*

0212   01  WRK-DINIC-FLEXZ             PIC  X(010)         VALUE SPACES.
0212   01  WRK-DFIM-FLEXZ              PIC  X(010)         VALUE SPACES.

       01  WRK-DPROCM-ATUAL-INV        PIC  9(008)         VALUE ZEROS.

       01  WRK-DINI-PRMSS-INV          PIC  9(008)         VALUE ZEROS.
       01  WRK-DFIM-PRMSS-INV          PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INV                PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES          WRK-DATA-INV.
           03  WRK-ANO-I               PIC  9(004).
           03  WRK-MES-I               PIC  9(002).
           03  WRK-DIA-I               PIC  9(002).

BI1211 01  WRK-QTDE-OCOR               PIC  9(007) COMP-3  VALUE ZEROS.
BI1211 01  WRK-QTDE-OCOR-PK            PIC  9(007) COMP-3  VALUE ZEROS.
0212   01  WRK-QTDE-TOT-REG            PIC  9(009) COMP-3  VALUE ZEROS.
0212   01  WRK-CCJTO-SERVC-INI         PIC S9(003) COMP-3  VALUE ZEROS.
0212   01  WRK-CCJTO-SERVC-FIM         PIC S9(003) COMP-3  VALUE ZEROS.

0212   01  WRK-AUX-5-S                 PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES          WRK-AUX-5-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-AUX-5               PIC  9(005).

ST2506*01  WRK-CCGC-CPF-S              PIC +9(009)        VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES       WRK-CCGC-CPF-S.
ST2506*    05  FILLER                  PIC  X(001).
ST2506*    05  WRK-CCGC-CPF            PIC  9(009).
ST2506*
ST2506*01  WRK-CFLIAL-CGC-S            PIC +9(005)        VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES       WRK-CFLIAL-CGC-S.
ST2506*    05  FILLER                  PIC  X(001).
ST2506*    05  WRK-CFLIAL-CGC          PIC  9(005).

0212   01  WRK-CCTRL-CPF-CGC-S         PIC +9(002)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CCTRL-CPF-CGC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CCTRL-CPF-CGC       PIC  9(002).

0212   01  WRK-CDEPDC-S                PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CDEPDC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CDEPDC              PIC  9(005).

0212   01  WRK-CPOSTO-SERVC-S          PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CPOSTO-SERVC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CPOSTO-SERVC        PIC  9(005).

0212   01  WRK-CSGMTO-TARIF-S          PIC +9(003)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CSGMTO-TARIF-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CSGMTO-TARIF        PIC  9(003).

0212   01  WRK-CMUN-IBGE-S             PIC +9(007)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CMUN-IBGE-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CMUN-IBGE           PIC  9(007).

0212   01  WRK-AUX-9-S                 PIC +9(009)         VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-AUX-9-S.
           05 FILLER                   PIC  X(001).
0212       05 WRK-AUX-9                PIC  9(009).
0212       05 FILLER                   REDEFINES WRK-AUX-9.
0212          10 FILLER                PIC  X(006).
0212          10 WRK-AUX-9-X.
0212             15 WRK-AUX-9-3        PIC  9(003).

0610XS 01  WRK-CGRP-RZ-CTBIL-S         PIC +9(003)        VALUE ZEROS.
HEXA   01  FILLER                      REDEFINES WRK-CGRP-RZ-CTBIL-S.
           05  FILLER                  PIC  X(001).
HEXA       05  WRK-CGRP-RZ-CTBIL       PIC  9(003).

0610XS 01  WRK-CSGRP-RZ-CTBIL-S        PIC +9(003)        VALUE ZEROS.
HEXA   01  FILLER                      REDEFINES WRK-CSGRP-RZ-CTBIL-S.
           05  FILLER                  PIC  X(001).
HEXA       05  WRK-CSGRP-RZ-CTBIL      PIC  9(003).

HEXA   01  WRK-COD-SQL-ERRO            PIC +9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(001).
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-SUB                 PIC  X(001)         VALUE 'N'.
       77  WRK-SITUACAO                PIC  9(002)         VALUE ZEROS.
       77  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.
       77  WRK-DPROCM-ATUAL            PIC  X(010)         VALUE SPACES.

       01  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MODULO DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

       01  WRK-POOL7100                 PIC X(107)         VALUE SPACES.
       01  WRK-SQLCA                    PIC X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA GFCT5505 *'.
      *----------------------------------------------------------------*

       01  WRK-5505-AREA-ENTRADA.
           05  WRK-5505-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5505-FUNC-BDSCO     PIC  X(007)         VALUE SPACES.
       01  WRK-5505-AREA-SAIDA.
           05  WRK-5505-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5505-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           05  WRK-5505-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5505-DADOS-RETORNO.
             10  WRK-5505-COD-JUNCAO   PIC  9(005)         VALUE ZEROS.

       01  WRK-IO-PCB.
           05  WRK-IO-LTERM            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  WRK-IO-STATUS           PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  WRK-IO-MODNAME          PIC X(008)          VALUE SPACES.

       01  WRK-ALT-PCB.
           05  WRK-ALT-LTERM           PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE SPACES.
           05  WRK-ALT-STATUS          PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE SPACES.
           05  WRK-ALT-MODNAME         PIC X(008)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5522 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5523 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5006 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM GFCT5109 - GFCT5090 *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREAS DE COMUNICACAO COM RURC9020 *'.
      *----------------------------------------------------------------*

       COPY 'I#RURC88'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B2
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B3
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C0
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B9
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0B7
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0C1
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
               INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

0212       EXEC SQL
0212           INCLUDE GFCTB0A2
0212       END-EXEC.

0410       EXEC SQL
0410           INCLUDE GFCTB0M7
0410       END-EXEC.

0510       EXEC SQL
0510           INCLUDE PLCCB004
0510       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
0212           DECLARE CSR01-GFCTB0B2-JN CURSOR FOR
0212           SELECT DISTINCT
0212               T1.CSERVC_TARIF
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212               DB2PRD.PARM_SERVC_TARIF  T2
0212         WHERE
0212               T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
0212         ORDER BY
0212               T1.CSERVC_TARIF
           END-EXEC.

           EXEC SQL
0212           DECLARE CSR02-GFCTB0B2-JN CURSOR FOR
0212           SELECT DISTINCT
0212               T1.CSERVC_TARIF
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212               DB2PRD.PARM_SERVC_TARIF  T2
0212         WHERE
0212               T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212           AND T1.CSIT_SOLTC_FLEXZ = 01
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
0212         ORDER BY
0212               T1.CSERVC_TARIF
           END-EXEC.

           EXEC SQL
0212           DECLARE CSR03-GFCTB0B2-JN CURSOR FOR
0212           SELECT DISTINCT
0212               T1.CSERVC_TARIF
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212               DB2PRD.PARM_SERVC_TARIF  T2
0212         WHERE
0212               T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212           AND T1.CSIT_SOLTC_FLEXZ = 02
TESTE          AND T1.DINIC_FLEXZ_AGPTO
TESTE                                  > :WRK-DINIC-FLEXZ
0212  *---     AND (T1.DINIC_FLEXZ_AGPTO
0212  *---                            <= :WRK-DINIC-FLEXZ
0212  *---     AND  T1.DFIM_FLEXZ_AGPTO
0212  *---                            >= :WRK-DFIM-FLEXZ )
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
           END-EXEC.

           EXEC SQL
0212           DECLARE CSR04-GFCTB0B2 CURSOR FOR
0212           SELECT
0212               HIDTFD_FLEXZ_AGPTO,
0212               DINIC_FLEXZ_AGPTO,
0212               CSIT_SOLTC_FLEXZ,
0212               CINDCD_AGPTO_TOT
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
0212         WHERE
0212               CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
0212           AND CSERVC_TARIF        = :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           EXEC SQL
0212           DECLARE CSR05-GFCTB0B2 CURSOR FOR
0212           SELECT
0212               CSERVC_TARIF,
0212               HIDTFD_FLEXZ_AGPTO,
0212               DINIC_FLEXZ_AGPTO,
0212               CSIT_SOLTC_FLEXZ,
0212               CINDCD_AGPTO_TOT
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
0212         WHERE
0212               CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
0212           AND CSIT_SOLTC_FLEXZ    = 01
0212           AND CSERVC_TARIF        = :GFCTB0B2.CSERVC-TARIF
0212         ORDER BY
0212               CSERVC_TARIF,
0212               DINIC_FLEXZ_AGPTO DESC
           END-EXEC.

           EXEC SQL
0212           DECLARE CSR06-GFCTB0B2 CURSOR FOR
0212           SELECT
0212               CSERVC_TARIF,
0212               HIDTFD_FLEXZ_AGPTO,
0212               DINIC_FLEXZ_AGPTO,
0212               CSIT_SOLTC_FLEXZ,
0212               CINDCD_AGPTO_TOT
0212         FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
0212         WHERE
0212               CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
0212           AND CSIT_SOLTC_FLEXZ    = 02
0212           AND CSERVC_TARIF        = :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

060916     EXEC SQL
060916         DECLARE CSR07-GFCTB0B2 CURSOR FOR
060916         SELECT
060916             CSERVC_TARIF,
060916             HIDTFD_FLEXZ_AGPTO,
060916             DINIC_FLEXZ_AGPTO,
060916             CSIT_SOLTC_FLEXZ,
060916             CINDCD_AGPTO_TOT
060916       FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
060916       WHERE
060916             CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
060916       AND   CSERVC_TARIF        BETWEEN :WRK-TARIFA-INI
060916                                     AND :WRK-TARIFA-FIM
060916     END-EXEC.
060916
060916     EXEC SQL
060916         DECLARE CSR08-GFCTB0B2 CURSOR FOR
060916         SELECT
060916             CSERVC_TARIF,
060916             HIDTFD_FLEXZ_AGPTO,
060916             DINIC_FLEXZ_AGPTO,
060916             CSIT_SOLTC_FLEXZ,
060916             CINDCD_AGPTO_TOT
060916       FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
060916       WHERE
060916             CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
060916       AND   CSERVC_TARIF        BETWEEN :WRK-TARIFA-INI
060916                                     AND :WRK-TARIFA-FIM
060916       AND   CSIT_SOLTC_FLEXZ    = 01
060916       ORDER BY
060916             CSERVC_TARIF,
060916             DINIC_FLEXZ_AGPTO DESC
060916     END-EXEC.
060916
060916     EXEC SQL
060916         DECLARE CSR09-GFCTB0B2 CURSOR FOR
060916         SELECT
060916             CSERVC_TARIF,
060916             HIDTFD_FLEXZ_AGPTO,
060916             DINIC_FLEXZ_AGPTO,
060916             CSIT_SOLTC_FLEXZ,
060916             CINDCD_AGPTO_TOT
060916       FROM  DB2PRD.PDIDO_FLEXZ_AGPTO
060916       WHERE
060916             CAGPTO_CTA          = :GFCTB0B2.CAGPTO-CTA
060916       AND   CSERVC_TARIF        BETWEEN :WRK-TARIFA-INI
060916                                     AND :WRK-TARIFA-FIM
060916       AND   CSIT_SOLTC_FLEXZ    = 02
060916     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNJ'.

       COPY 'I#GFCTNK'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTNJ-ENTRADA
                                             GFCTNK-SAIDA
                                             GFCT0M-AREA-ERROS.
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
               WRK-CAMPO-POE-SINAL-17-C.

      ******************************************************************
      * ROTINA INICIAL DO PROGRAMA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA QUE SERA PROCESSADA APENA UMA VEZ NO INICIO DO PROGRAMA *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTNK-SAIDA
                                          GFCTG2-ENTRADA
                                          GFCTG3-SAIDA
                                          GFCTFZ-ENTRADA
                                          GFCTG1-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTNK-SAIDA
                      GFCTG2-ENTRADA
                      GFCTG3-SAIDA
                      GFCTFZ-ENTRADA
                      GFCTG1-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1.

           MOVE 'N'                    TO WRK-FINALIZAR
                                          WRK-FIM-SUB.

           PERFORM 1100-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1200-INICIALIZAR-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DATA-ATUAL.

           PERFORM 1500-ACESSAR-GFCT5505.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR DISPONIBILIDADE DO SISTEMA               *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-SIST-DISPONIVEL  SECTION.
      *----------------------------------------------------------------*

           MOVE +100                           TO GFCTFZ-LL.
           MOVE  ZEROS                         TO GFCTFZ-ZZ.
           MOVE 'N'                            TO GFCTFZ-FIM.
           MOVE  ZEROS                         TO GFCTFZ-FUNC-BDSCO.
           MOVE  GFCTNJ-TRANSACAO              TO GFCTFZ-TRANSACAO.
           MOVE  GFCTNJ-FUNCAO                 TO GFCTFZ-FUNCAO.
           MOVE 'GFCT'                         TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'                     TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                     GFCTFZ-ENTRADA
                                                     GFCTG1-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNK-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                     EQUAL 9
               IF  GFCT0M-TIPO-ACESSO          NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO   TO GFCT0M-TEXTO
                   MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO    TO GFCTNK-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'GFCT0883'                 TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO                TO GFCTNK-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE              EQUAL 1
               MOVE 9                          TO GFCTNK-ERRO
               MOVE 'SISTEMA INDISPONIVEL'     TO GFCT0M-TEXTO
               MOVE WRK-MODULO                 TO GFCT0M-PROGRAMA
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZAR AREA DE SAIDA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-INICIALIZAR-SAIDA          SECTION.
      *----------------------------------------------------------------*

120510     MOVE +290                   TO GFCTNK-LL.
           MOVE  ZEROS                 TO GFCTNK-ZZ.
           MOVE  GFCTNJ-TRANSACAO      TO GFCTNK-TRANSACAO.
           MOVE  GFCTNJ-FUNCAO         TO GFCTNK-FUNCAO.

           IF  GFCTNJ-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTNK-QTDE-OCOR
           ELSE
               MOVE GFCTNJ-QTDE-OCOR   TO GFCTNK-QTDE-OCOR
           END-IF.

BI0810     IF  GFCTNJ-QTDE-TOT-REG     NOT NUMERIC
BI0810         MOVE ZEROS               TO GFCTNK-QTDE-TOT-REG
BI0810     ELSE
BI0810         MOVE GFCTNJ-QTDE-TOT-REG TO GFCTNK-QTDE-TOT-REG
BI0810     END-IF.

           MOVE GFCTNJ-FUNC-BDSCO      TO GFCTNK-FUNC-BDSCO.
           MOVE GFCTNJ-FILTRO          TO GFCTNK-FILTRO.

           MOVE 'N'                    TO GFCTNK-FIM.
           MOVE GFCTNJ-FLT-TIPO-HIST   TO GFCTNK-FLT-TIPO-HIST.

           MOVE  ZEROS                 TO GFCTNK-COD-SQL-ERRO
                                          GFCTNK-COD-MSG-ERRO
                                          GFCTNK-ERRO.
           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1310-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG        TO GFCTNK-DESC-MSG-ERRO.

0212       PERFORM
0212       VARYING IND-1               FROM 1 BY 1
0212         UNTIL IND-1               GREATER 4
ST2506*            MOVE ZEROS          TO GFCTNK-CGC-CPF     (IND-1)
ST2506*                                   GFCTNK-FILIAL      (IND-1)
ST2506             MOVE ZEROS          TO GFCTNK-AGENCIA     (IND-1)
0212                                      GFCTNK-POSTO       (IND-1)
0212                                      GFCTNK-SEGMENTO    (IND-1)
0212                                      GFCTNK-MUNICIPIO   (IND-1)
0212                                      GFCTNK-COD-TARIFA  (IND-1)
0212                                      GFCTNK-SEQUENCIA   (IND-1)
0212                                      GFCTNK-CONTROLE    (IND-1)
0410                                      GFCTNK-GRUPO-CTBIL (IND-1)
0410                                      GFCTNK-SUB-GRUPO-CTBIL
0410                                                         (IND-1)
ST2506             MOVE SPACES         TO GFCTNK-CGC-CPF     (IND-1)
ST2506                                    GFCTNK-FILIAL      (IND-1)
0212                                      GFCTNK-UF          (IND-1)
0212                                      GFCTNK-DESCR-AGPTO (IND-1)
0212                                      GFCTNK-DESCR-TARIFA(IND-1)
0212                                      GFCTNK-TIMESTAMP   (IND-1)
0212                                      GFCTNK-SITUACAO    (IND-1)
110510                                    GFCTNK-DESC-CTBIL (IND-1)
0212       END-PERFORM.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTIR DADOS DE ENTRADA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNJ-TRANSACAO        EQUAL SPACES             OR
               GFCTNJ-TRANSACAO        EQUAL LOW-VALUES         OR
               GFCTNJ-FUNCAO           EQUAL SPACES             OR
               GFCTNJ-FUNCAO           EQUAL LOW-VALUES         OR
               GFCTNJ-FUNC-BDSCO       EQUAL SPACES             OR
               GFCTNJ-FUNC-BDSCO       EQUAL LOW-VALUES         OR
0212           GFCTNJ-FLT-CAGPTO-CTA   NOT NUMERIC              OR
ST2506*        GFCTNJ-FLT-CCGC-CPF     NOT NUMERIC              OR
ST2506*        GFCTNJ-FLT-CFLIAL-CGC   NOT NUMERIC              OR
ST2506         GFCTNJ-FLT-CCGC-CPF     NOT EQUAL LOW-VALUES     OR
ST2506         GFCTNJ-FLT-CFLIAL-CGC   NOT EQUAL LOW-VALUES     OR
0212           GFCTNJ-FLT-CCTRL-CPF-CGC NOT NUMERIC             OR
0212           GFCTNJ-FLT-CDEPDC       NOT NUMERIC              OR
0212           GFCTNJ-FLT-CPOSTO-SERVC NOT NUMERIC              OR
0212           GFCTNJ-FLT-CSGMTO-TARIF NOT NUMERIC              OR
0212           GFCTNJ-FLT-CMUN-IBGE    NOT NUMERIC              OR
0212           GFCTNJ-FLT-CCJTO-SERVC  NOT NUMERIC              OR
0410           GFCTNK-FLT-GRUPO-CTBIL  NOT NUMERIC              OR
0410           GFCTNK-FLT-SUB-GRUPO-CTBIL
0410                                   NOT NUMERIC              OR
0212          (GFCTNJ-FLT-TIPO-HIST    NOT EQUAL 'T' AND 'P' AND 'D')
               MOVE 'N'                TO GFCTNK-FIM
               MOVE 1                  TO GFCTNK-ERRO
                                          GFCTNK-COD-MSG-ERRO
               MOVE ZEROS              TO GFCTNK-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE ZEROS TO GFCTNJ-FLT-CSGMTO-TARIF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DESCRICAO DA MENSAGEM                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1310-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE 100                      TO GFCTG2-LL.
           MOVE ZEROS                    TO GFCTG2-ZZ.
           MOVE GFCTNJ-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE GFCTNJ-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE GFCTNJ-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                      TO GFCTG2-TIPO-PROC.
           MOVE 'N'                      TO GFCTG2-FIM.

           MOVE 'GFCT5523'               TO WRK-MODULO.

           CALL WRK-MODULO USING
                                               GFCTG2-ENTRADA
                                               GFCTG3-SAIDA
                                               GFCT0M-AREA-ERROS.

           IF  RETURN-CODE               NOT EQUAL ZEROS
               MOVE 9                    TO GFCTNK-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 9
               MOVE 9                    TO GFCTNK-ERRO
               MOVE WRK-MODULO           TO GFCT0M-PROGRAMA
                                            WRK-NOME-MOD
               MOVE WRK-MSG01            TO GFCT0M-TEXTO
               MOVE 'APL'                TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'           TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO               EQUAL 1
               MOVE 'S'                  TO GFCTNK-FIM
               MOVE 1                    TO GFCTNK-ERRO
               MOVE GFCTG3-COD-SQL-ERRO  TO GFCTNK-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO  TO GFCTNK-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER DATA ATUAL.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DATA-ATUAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
               SELECT DPROCM_ATUAL
               INTO  :GFCTB0A1.DPROCM-ATUAL
               FROM   DB2PRD.PARM_DATA_PROCM
               WHERE  CSIST_PRINC      = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0A1'      TO WRK-NOME-TAB
               MOVE 'GFCT0883'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONCATENAR MENSAGEM                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1410-CONCATENAR-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB

           DELIMITED BY '  '           INTO GFCTNK-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ACESSAR MODULO GFCT5505.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-ACESSAR-GFCT5505           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5505-AREA-SAIDA.

           MOVE 'O'                    TO WRK-5505-AMBIENTE.
           MOVE GFCTNJ-FUNC-BDSCO      TO WRK-5505-FUNC-BDSCO.

           MOVE 'GFCT5505'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-5505-AREA-ENTRADA
                                             WRK-5505-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB.

           IF  WRK-5505-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  1                 TO GFCTNK-ERRO
               MOVE '0112'             TO GFCTNK-COD-MSG-ERRO
               MOVE  WRK-5505-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAMENTO DO PROGRAMA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
060916
060916     IF  GFCTNJ-FLT-CAGPTO-CTA  EQUAL
060916                                    04 OR 09 OR 14 OR 15 OR
060916                                    17 OR 18 OR 19 OR 20 OR 26
060916         PERFORM 2050-PROC-COM-CJTO-SERVICO
060916     ELSE
060916         PERFORM 2600-PROC-SEM-CJTO-SERVICO
060916     END-IF.
060916
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

060916*----------------------------------------------------------------*
060916 2050-PROC-COM-CJTO-SERVICO      SECTION.
060916*----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR.

           PERFORM 2100-ABRIR-CURSOR1.

BI0810     MOVE  GFCTNJ-QTDE-OCOR  TO  ACU-DESPREZA.

VI0618*    UTILIZA O CAMPO DE ACU-DESPREZA COMO FLAG PARA
VI0618*      TRATAR QTDE A SER DESPREZADA
VI0618     IF  ACU-DESPREZA            NOT EQUAL  ZEROS
VI0618         PERFORM 2200-LER-CURSOR1
VI0618         PERFORM 2300-PROCESSAR-CURSOR
VI0618               UNTIL IND-LIDOS   NOT LESS  ACU-DESPREZA  OR
VI0618                 WRK-FIM-CURSOR  EQUAL  'S'
VI0618     END-IF.

           IF  WRK-FIM-CURSOR          EQUAL 'S'
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  ZEROS             TO GFCTNK-ERRO
                                          GFCTNK-COD-SQL-ERRO
               EVALUATE WRK-SITUACAO
                   WHEN 01
                       MOVE 0827       TO GFCTNK-COD-MSG-ERRO
                                          GFCTG2-COD-MSG

                   WHEN 02
                       MOVE 0828       TO GFCTNK-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   WHEN 03
                       MOVE 0829       TO GFCTNK-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
               END-EVALUATE
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  ZEROS                 TO IND-1.

BI0810     IF    ACU-DESPREZA  EQUAL  ZEROS
BI0810           PERFORM 2200-LER-CURSOR1

VI0618     ELSE
VI0618*    DESLIGA O FLAG DE DESPREZA - PARA TRATAR PROCESSO NORMAL
VI0618           MOVE  ACU-DESPREZA    TO  WRK-QTDE-OCOR
VI0618           MOVE  ZEROS           TO  ACU-DESPREZA
VI0618     END-IF.

           PERFORM 2300-PROCESSAR-CURSOR
                       UNTIL IND-1     GREATER 4 OR
                    WRK-FIM-CURSOR     EQUAL 'S' OR
                    WRK-FINALIZAR      EQUAL 'S'.
           PERFORM 2400-FECHAR-CURSOR1.

060916*----------------------------------------------------------------*
060916 2050-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR DA TABELA GFCTB0B2.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ABRIR-CURSOR1              SECTION.
      *----------------------------------------------------------------*

           MOVE  GFCTNK-FLT-CAGPTO-CTA     TO CAGPTO-CTA   OF GFCTB0B2

0212       IF  GFCTNJ-FLT-CCJTO-SERVC   EQUAL ZEROS
0212           MOVE  ZEROS                 TO WRK-CCJTO-SERVC-INI
0212           MOVE  99999                 TO WRK-CCJTO-SERVC-FIM
0212       ELSE
0212           MOVE  GFCTNJ-FLT-CCJTO-SERVC
0212                                       TO WRK-CCJTO-SERVC-INI
0212                                          WRK-CCJTO-SERVC-FIM
0212       END-IF.

           MOVE  DPROCM-ATUAL OF GFCTB0A1  TO
                                           WRK-DINIC-FLEXZ
                                           WRK-DFIM-FLEXZ

           EVALUATE GFCTNJ-FLT-TIPO-HIST
               WHEN 'T'
                   MOVE  01                TO WRK-SITUACAO
ST2506*            IF  GFCTNJ-FLT-CCGC-CPF      EQUAL  ZEROS      AND
ST2506*                GFCTNJ-FLT-CCTRL-CPF-CGC EQUAL  ZEROS      AND
ST2506             IF  GFCTNJ-FLT-CCGC-CPF      EQUAL  SPACES     AND
ST2506                 GFCTNJ-FLT-CCTRL-CPF-CGC EQUAL  SPACES     AND
0212                   GFCTNJ-FLT-CDEPDC        EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CPOSTO-SERVC  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGMTO-TARIF  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CMUN-IBGE     EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CCJTO-SERVC   EQUAL  ZEROS      AND
0410                   GFCTNJ-FLT-GRUPO-CTBIL   EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGL-UF       EQUAL  SPACES
                       PERFORM 2110-COUNT-CRS-C01
0212               END-IF
                   PERFORM 2120-ABRIR-CRS-C01

               WHEN 'P'
                   MOVE  02                TO WRK-SITUACAO
0212               IF  GFCTNJ-FLT-CCGC-CPF      EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CCTRL-CPF-CGC EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CDEPDC        EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CPOSTO-SERVC  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGMTO-TARIF  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CMUN-IBGE     EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CCJTO-SERVC   EQUAL  ZEROS      AND
0410                   GFCTNJ-FLT-GRUPO-CTBIL   EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGL-UF       EQUAL  SPACES
                       PERFORM 2130-COUNT-CRS-C02
0212               END-IF
                   PERFORM 2140-ABRIR-CRS-C02

               WHEN OTHER
                   MOVE  03                TO WRK-SITUACAO
0212               IF  GFCTNJ-FLT-CCGC-CPF      EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CCTRL-CPF-CGC EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CDEPDC        EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CPOSTO-SERVC  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGMTO-TARIF  EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CMUN-IBGE     EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CCJTO-SERVC   EQUAL  ZEROS      AND
0410                   GFCTNJ-FLT-GRUPO-CTBIL   EQUAL  ZEROS      AND
0212                   GFCTNJ-FLT-CSGL-UF       EQUAL  SPACES
                       PERFORM 2150-COUNT-CRS-C03
0212               END-IF
                   PERFORM 2160-ABRIR-CRS-C03

           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR QUANTIDADE DE OCORRENCIAS DO CURSOR C01  *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-COUNT-CRS-C01              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
               INTO  :WRK-COUNT
0212           FROM   DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212                  DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
0212                  T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212             AND  T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212             AND  T2.CCJTO_SERVC
0212                                BETWEEN :WRK-CCJTO-SERVC-INI
0212                                    AND :WRK-CCJTO-SERVC-FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0020'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  0633              TO GFCTNK-COD-MSG-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR C01                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-ABRIR-CRS-C01              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR01-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0030'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR QUANTIDADE DE OCORRENCIAS DO CURSOR C02  *
      ******************************************************************
      *----------------------------------------------------------------*
       2130-COUNT-CRS-C02              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
               INTO  :WRK-COUNT
0212           FROM   DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212                  DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
0212               T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212           AND T1.CSIT_SOLTC_FLEXZ = 01
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0040'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  0633              TO GFCTNK-COD-MSG-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR C02.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2140-ABRIR-CRS-C02              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR02-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0050'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR QUANTIDADE DE OCORRENCIAS DO CURSOR C03  *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-COUNT-CRS-C03              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT COUNT(*)
               INTO  :WRK-COUNT
0212           FROM   DB2PRD.PDIDO_FLEXZ_AGPTO T1,
0212                  DB2PRD.PARM_SERVC_TARIF  T2
               WHERE
0212               T1.CAGPTO_CTA       = :GFCTB0B2.CAGPTO-CTA
0212           AND T1.CSIT_SOLTC_FLEXZ = 02
0212           AND (T1.DINIC_FLEXZ_AGPTO
0212                                  <= :WRK-DINIC-FLEXZ
0212           AND  T1.DFIM_FLEXZ_AGPTO
0212                                  >= :WRK-DFIM-FLEXZ )
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0060'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  0633              TO GFCTNK-COD-MSG-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR C03.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-ABRIR-CRS-C03              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR03-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0070'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VERIFICAR QUAL CURSOR SERA LIDO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-LER-CURSOR1                SECTION.
      *----------------------------------------------------------------*
           EVALUATE WRK-SITUACAO
               WHEN  01
                   PERFORM 2210-LER-CRS-C01

               WHEN  02
                   PERFORM 2220-LER-CRS-C02

               WHEN  03
                   PERFORM 2230-LER-CRS-C03

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LER CRS C01.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-LER-CRS-C01                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB0B2-JN INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0006'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0080'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0006               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTNK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCTNK-ERRO
                                          GFCTNK-COD-SQL-ERRO
                   MOVE 0007           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0007'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 2410-FECHAR-CRS-C01
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LER CRS C02.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CRS-C02                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR02-GFCTB0B2-JN INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0006'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0090'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0006               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTNK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCTNK-ERRO
                                          GFCTNK-COD-SQL-ERRO
                   MOVE 0007           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0007'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 2420-FECHAR-CRS-C02
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA LER CRS C03.                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-LER-CRS-C03                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR03-GFCTB0B2-JN INTO
                   :GFCTB0B2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0006'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0100'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0006               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCTNK-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCTNK-ERRO
                                          GFCTNK-COD-SQL-ERRO
                   MOVE 0007           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0007'         TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 2430-FECHAR-CRS-C03
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PROCESSAR-CURSOR                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

0212       EVALUATE WRK-SITUACAO
0212           WHEN  01
0212               PERFORM 2310-ABRIR-CRS04
0212               PERFORM 2510-FETCH-CRS04
0212
0212           WHEN  02
0212               PERFORM 2320-ABRIR-CRS05
0212               PERFORM 2520-FETCH-CRS05
0212
0212           WHEN  03
0212               PERFORM 2330-ABRIR-CRS06
0212               PERFORM 2530-FETCH-CRS06
0212
0212       END-EVALUATE.

VI0618     IF  ACU-DESPREZA            NOT EQUAL  ZEROS
VI0618         PERFORM 2340-MOVER-CAMPOS UNTIL
VI0618                      IND-LIDOS  NOT LESS ACU-DESPREZA  OR
VI0618                    WRK-FIM-SUB  EQUAL 'S'
VI0618         MOVE 'N'                TO WRK-FIM-SUB

VI0618     ELSE
               PERFORM 2340-MOVER-CAMPOS UNTIL
                                IND-1  GREATER   4  OR
                                WRK-FIM-SUB EQUAL 'S'
               MOVE 'N'                TO WRK-FIM-SUB

VI0618     END-IF.

0212       IF WRK-SITUACAO             EQUAL  01
0212           PERFORM 2350-FECHA-CRS04
0212
0212       ELSE
0212           IF WRK-SITUACAO         EQUAL  02
0212               PERFORM 2360-FECHA-CRS05
0212
0212           ELSE
0212               IF WRK-SITUACAO     EQUAL 03
0212                   PERFORM 2370-FECHA-CRS06
0212
0212               END-IF
0212           END-IF
0212       END-IF.

           PERFORM 2200-LER-CURSOR1.

           IF  IND-1                EQUAL 4
               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR C04                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2310-ABRIR-CRS04                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR04-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0110'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR C05                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2320-ABRIR-CRS05                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR05-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0120'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA ABRIR CURSOR C06                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2330-ABRIR-CRS06                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR06-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0130'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA MOVIMENTACAO DE CAMPOS.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2340-MOVER-CAMPOS               SECTION.
      *----------------------------------------------------------------*

0610XS     IF  WRK-SITUACAO            EQUAL 01 OR
0610XS        (WRK-SITUACAO            EQUAL 02 AND
0710XS         CSIT-SOLTC-FLEXZ OF GFCTB0B2
0710XS                                 EQUAL 01) OR
0610XS        (WRK-SITUACAO            EQUAL 03 AND
0710XS         CSIT-SOLTC-FLEXZ OF GFCTB0B2
0710XS                                 EQUAL 02 AND
0710XS         WRK-DINI-PRMSS-INV      GREATER WRK-DPROCM-ATUAL-INV)

               EVALUATE CAGPTO-CTA OF GFCTB0B2

                   WHEN 15
                        PERFORM 2341-SELECIONAR-PAB

                   WHEN 14
                        PERFORM 2342-SELECIONAR-AGENCIA

                   WHEN 17
                        PERFORM 2343-SELECIONAR-POSTAL

                   WHEN 4
                        PERFORM 2344-SELECIONAR-CLIENTE

                   WHEN 9
                        PERFORM 2345-SELECIONAR-SEGMENTO

                   WHEN 18
                        PERFORM 2346-SELECIONAR-MUNICIPIO

                   WHEN 19
                        PERFORM 2347-SELECIONAR-UF

0410               WHEN 20
0410                    PERFORM 2343-SELECIONAR-POSTAL

0410               WHEN 26
0410                    PERFORM 2351-SELECIONAR-RAZAO

                   WHEN OTHER
0212                    PERFORM 2348-MONTAR-OCOR-SAIDA

               END-EVALUATE
0610XS     END-IF.

0212       EVALUATE WRK-SITUACAO
0212           WHEN  01
0212               PERFORM 2510-FETCH-CRS04
0212
0212           WHEN  02
0212               PERFORM 2520-FETCH-CRS05
0212
0212           WHEN  03
0212               PERFORM 2530-FETCH-CRS06
0212
0212       END-EVALUATE.

           IF  IND-1                EQUAL 4
               MOVE 'S'                TO WRK-FIM-SUB
           END-IF.

      *----------------------------------------------------------------*
       2340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR PAB.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2341-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B8.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B8.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B8.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B8.CEMPR-INC,
                   :GFCTB0B8.CDEPDC,
                   :GFCTB0B8.CPOSTO-SERVC
               FROM DB2PRD.PDIDO_FLEXZ_PAB
               WHERE
                   CAGPTO_CTA           = :GFCTB0B8.CAGPTO-CTA   AND
                   CSERVC_TARIF         = :GFCTB0B8.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO   = :GFCTB0B8.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B8'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0140'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNJ-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNJ-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNJ-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB0B8      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB0B8      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.
           MOVE 'GFCT5109'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNK-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               IF  GFCTGC-ERRO                 EQUAL 1
300808             MOVE  'PAB NAO ENCONTRADO'  TO GFCTGC-DESC-POSTO(1)
               ELSE
300808             MOVE 'S'                    TO GFCTNK-FIM
300808             MOVE  GFCTGC-ERRO           TO GFCTNK-ERRO
300808             MOVE  GFCTGC-COD-SQL-ERRO   TO GFCTNK-COD-SQL-ERRO
300808             MOVE  GFCTGC-COD-MSG-ERRO   TO GFCTNK-COD-MSG-ERRO
                   STRING 'GFCT5109- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNK-DESC-MSG-ERRO
300808             PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

0212       MOVE  CDEPDC       OF GFCTB0B8  TO WRK-CDEPDC-S
0212       MOVE  CPOSTO-SERVC OF GFCTB0B8  TO WRK-CPOSTO-SERVC-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2341-99-FIM
VI0618     END-IF.

0212       IF (GFCTNJ-FLT-CDEPDC           EQUAL ZEROS            AND
0212           GFCTNJ-FLT-CPOSTO-SERVC     EQUAL ZEROS           ) OR
0212          ((GFCTNJ-FLT-CDEPDC          NOT EQUAL ZEROS        AND
0212            GFCTNJ-FLT-CDEPDC          EQUAL WRK-CDEPDC)      AND
0212           (GFCTNJ-FLT-CPOSTO-SERVC    NOT EQUAL ZEROS        AND
0212            GFCTNJ-FLT-CPOSTO-SERVC    EQUAL WRK-CPOSTO-SERVC))
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CDEPDC            TO GFCTNK-AGENCIA(IND-1)
0212           MOVE  WRK-CPOSTO-SERVC      TO GFCTNK-POSTO(IND-1)
0212           MOVE  GFCTGC-DESC-POSTO(1)  TO GFCTNK-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2341-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR AGENCIA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2342-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B1.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B1.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B1.

           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC
               INTO
                   :GFCTB0B1.CEMPR-INC,
                   :GFCTB0B1.CDEPDC
               FROM DB2PRD.PDIDO_FLEXZ_AG
               WHERE
                   CAGPTO_CTA          = :GFCTB0B1.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B1'      TO WRK-NOME-TAB
               MOVE 'GFCT0883'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES            TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010              TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                      TO GFCT1V-ENTRADA.
           MOVE  660                         TO GFCT1V-LL.
           MOVE  ZEROS                       TO GFCT1V-ZZ.
           MOVE  GFCTNJ-TRANSACAO            TO GFCT1V-TRANSACAO.
           MOVE  GFCTNJ-FUNCAO               TO GFCT1V-FUNCAO.
           MOVE  001                         TO GFCT1V-QTDE-OCOR
           MOVE  ZEROS                       TO GFCT1V-QTDE-TOT-REG.
           MOVE  GFCTNJ-FUNC-BDSCO           TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                          TO GFCT1V-FIM.
           MOVE  CDEPDC OF GFCTB0B1          TO GFCT1V-COD-DEPDC(1).
           MOVE  SPACES                      TO GFCT1X-SAIDA
                                                GFCT0M-AREA-ERROS.

0212       MOVE  CDEPDC  OF GFCTB0B1   TO WRK-CDEPDC-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2342-99-FIM
VI0618     END-IF.

0212       IF  GFCTNJ-FLT-CDEPDC          EQUAL ZEROS      OR
0212          (GFCTNJ-FLT-CDEPDC          NOT EQUAL ZEROS AND
0212           GFCTNJ-FLT-CDEPDC          EQUAL WRK-CDEPDC)
0212
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212
0212           MOVE  WRK-CDEPDC        TO GFCTNK-AGENCIA(IND-1)
0212
0212           IF  CINDCD-AGPTO-TOT    OF GFCTB0B2  EQUAL 'S'
0212               MOVE 'TODAS'        TO GFCTNK-DESCR-AGPTO(IND-1)
0212           ELSE

                   MOVE 'GFCT5006'               TO WRK-MODULO

                   INITIALIZE GFCT1X-SAIDA
                              GFCT0M-AREA-ERROS

                   CALL  WRK-MODULO          USING   GFCT1V-ENTRADA
                                                     GFCT1X-SAIDA
                                                     GFCT0M-AREA-ERROS

                  IF  RETURN-CODE            NOT EQUAL ZEROS
                      MOVE 9                  TO GFCTNK-ERRO
                      MOVE WRK-MODULO         TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
                      MOVE WRK-MSG01          TO GFCT0M-TEXTO
                      MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
                      MOVE 'GFCT0883'         TO GFCT0M-TRANSACAO
                      PERFORM 3000-FINALIZAR
                  END-IF

                  IF  GFCT1X-ERRO             NOT EQUAL ZEROS
                      MOVE 'S'                  TO GFCTNK-FIM
                      MOVE  GFCT1X-ERRO         TO GFCTNK-ERRO
                      MOVE  GFCT1X-COD-SQL-ERRO TO GFCTNK-COD-SQL-ERRO
                      MOVE  GFCT1X-COD-MSG-ERRO TO GFCTNK-COD-MSG-ERRO
                      IF  GFCTGC-ERRO           EQUAL 1
                          MOVE  GFCT1X-DESC-MSG-ERRO
                                               TO GFCTNK-DESC-MSG-ERRO
                      ELSE
                          STRING 'GFCT5006- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNK-DESC-MSG-ERRO
                      END-IF
                      PERFORM 3000-FINALIZAR
                   END-IF
0212               MOVE GFCT1X-DESC-DEPDC(1) (1:30)
0212                                   TO GFCTNK-DESCR-AGPTO(IND-1)
0212           END-IF
0212
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2342-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR POSTAL.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2343-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B9.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B9.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B9.
           EXEC SQL
               SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
               INTO
                   :GFCTB0B9.CEMPR-INC,
                   :GFCTB0B9.CDEPDC,
                   :GFCTB0B9.CPOSTO-SERVC
               FROM DB2PRD.PDIDO_FLEXZ_PSTAL
               WHERE
                   CAGPTO_CTA          = :GFCTB0B9.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B9.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B9.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B9'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_PSTAL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0160'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  SPACES                        TO GFCTGB-ENTRADA.
           MOVE  100                           TO GFCTGB-LL.
           MOVE  ZEROS                         TO GFCTGB-ZZ.
           MOVE  GFCTNJ-TRANSACAO              TO GFCTGB-TRANSACAO.
           MOVE  GFCTNJ-FUNCAO                 TO GFCTGB-FUNCAO.
           MOVE  ZEROS                         TO GFCTGB-QTDE-OCOR
                                                  GFCTGB-QTDE-TOT-REG.
           MOVE  GFCTNJ-FUNC-BDSCO             TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                            TO GFCTGB-FIM.
           MOVE  CDEPDC       OF GFCTB0B9      TO GFCTGB-COD-DEPDC.
           MOVE  CPOSTO-SERVC OF GFCTB0B9      TO GFCTGB-COD-POSTO.
           MOVE  SPACES                        TO GFCTGC-SAIDA
                                                  GFCT0M-AREA-ERROS.

           MOVE 'GFCT5090'                     TO WRK-MODULO.

           INITIALIZE GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           CALL WRK-MODULO USING
                                                     GFCTGB-ENTRADA
                                                     GFCTGC-SAIDA
                                                     GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                     NOT EQUAL ZEROS
               MOVE 9                          TO GFCTNK-ERRO
               MOVE WRK-MODULO                 TO WRK-NOME-MOD
                                                  GFCT0M-PROGRAMA
               MOVE WRK-MSG01                  TO GFCT0M-TEXTO
               MOVE 'APL'                      TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0883'                 TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO                     NOT EQUAL ZEROS
               IF  GFCTGC-ERRO                 EQUAL 1
020908             MOVE  GFCTGC-DESC-MSG-ERRO  TO GFCTGC-DESC-POSTO(1)
               ELSE
020908             MOVE 'S'                    TO GFCTNK-FIM
020908             MOVE  GFCTGC-ERRO           TO GFCTNK-ERRO
020908             MOVE  GFCTGC-COD-SQL-ERRO   TO GFCTNK-COD-SQL-ERRO
020908             MOVE  GFCTGC-COD-MSG-ERRO   TO GFCTNK-COD-MSG-ERRO
                   STRING 'GFCT5090- ' GFCTGC-DESC-MSG-ERRO
                           DELIMITED BY SIZE   INTO GFCTNK-DESC-MSG-ERRO
020908             PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

0212       MOVE  CDEPDC       OF GFCTB0B9  TO WRK-CDEPDC-S
0212       MOVE  CPOSTO-SERVC OF GFCTB0B9  TO WRK-CPOSTO-SERVC-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2343-99-FIM
VI0618     END-IF.

0212       IF (GFCTNJ-FLT-CDEPDC           EQUAL ZEROS            AND
0212           GFCTNJ-FLT-CPOSTO-SERVC     EQUAL ZEROS           ) OR
0212          (GFCTNJ-FLT-CDEPDC           EQUAL WRK-CDEPDC       AND
0212           GFCTNJ-FLT-CPOSTO-SERVC     EQUAL WRK-CPOSTO-SERVC)
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CDEPDC            TO GFCTNK-AGENCIA(IND-1)
0212           MOVE  WRK-CPOSTO-SERVC      TO GFCTNK-POSTO(IND-1)
0212           MOVE  GFCTGC-DESC-POSTO(1)  TO GFCTNK-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2343-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR CLIENTE.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2344-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B3.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B3.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B3.

           EXEC SQL
ST2506*        SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506*        INTO
ST2506*            :GFCTB0B3.CCGC-CPF,
ST2506*            :GFCTB0B3.CFLIAL-CGC,
ST2506*            :GFCTB0B3.CCTRL-CPF-CGC
ST2506         SELECT
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
ST2506         INTO
ST2506             :GFCTB0B3.CCGC-CPF-ST,
ST2506             :GFCTB0B3.CFLIAL-CGC-ST,
ST2506             :GFCTB0B3.CCTRL-CPF-CGC-ST
               FROM DB2PRD.PDIDO_FLEXZ_CLI
               WHERE
                   CAGPTO_CTA          = :GFCTB0B3.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0B3.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0B3.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B3'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_CLI'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0170'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    M0VE  CCGC-CPF      OF GFCTB0B3 TO WRK-CCGC-CPF-S
ST2506*    M0VE  CFLIAL-CGC    OF GFCTB0B3 TO WRK-CFLIAL-CGC-S
ST2506*    M0VE  CCTRL-CPF-CGC OF GFCTB0B3 TO WRK-CCTRL-CPF-CGC-S
ST2506     MOVE  CCTRL-CPF-CGC-ST OF GFCTB0B3 TO WRK-CCTRL-CPF-CGC-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2344-99-FIM
VI0618     END-IF.

ST2506*    IF (GFCTNJ-FLT-CCGC-CPF         EQUAL ZEROS             AND
ST2506*        GFCTNJ-FLT-CFLIAL-CGC       EQUAL ZEROS             AND
ST2506*        GFCTNJ-FLT-CCTRL-CPF-CGC    EQUAL ZEROS           )
ST2506     IF (GFCTNJ-FLT-CCGC-CPF         EQUAL SPACES            AND
ST2506         GFCTNJ-FLT-CFLIAL-CGC       EQUAL SPACES            AND
ST2506         GFCTNJ-FLT-CCTRL-CPF-CGC    EQUAL SPACES          )
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
ST2506*        M0VE  WRK-CCGC-CPF          TO GFCTNK-CGC-CPF(IND-1)
ST2506*        M0VE  WRK-CFLIAL-CGC        TO GFCTNK-FILIAL(IND-1)
ST2506         MOVE  CCGC-CPF-ST           OF GFCTB0B3 
ST2506                                     TO GFCTNK-CGC-CPF(IND-1)
ST2506         MOVE  CFLIAL-CGC-ST         OF GFCTB0B3
ST2506                                     TO GFCTNK-FILIAL(IND-1)
0212           MOVE  WRK-CCTRL-CPF-CGC     TO GFCTNK-CONTROLE(IND-1)
0212       ELSE
ST2506*       IF GFCTNJ-FLT-CCGC-CPF       EQUAL WRK-CCGC-CPF      AND
ST2506*          GFCTNJ-FLT-CFLIAL-CGC     EQUAL WRK-CFLIAL-CGC    AND
ST2506        IF GFCTNJ-FLT-CCGC-CPF       EQUAL CCGC-CPF-ST      
                                              OF GFCTB0B3 AND
ST2506           GFCTNJ-FLT-CFLIAL-CGC     EQUAL CFLIAL-CGC-ST    
                                              OF GFCTB0B3 AND
ST2506           GFCTNJ-FLT-CCTRL-CPF-CGC  EQUAL WRK-CCTRL-CPF-CGC
0212             PERFORM 2348-MONTAR-OCOR-SAIDA
ST2506*          MOVE  WRK-CCGC-CPF          TO GFCTNK-CGC-CPF(IND-1)
ST2506*          MOVE  WRK-CFLIAL-CGC        TO GFCTNK-FILIAL(IND-1)
ST2506*          MOVE  WRK-CCTRL-CPF-CGC     TO GFCTNK-CONTROLE(IND-1)
ST2506           MOVE  CCGC-CPF-ST           OF GFCTB0B3 
ST2506                                       TO GFCTNK-CGC-CPF(IND-1)
ST2506           MOVE  CFLIAL-CGC-ST         OF GFCTB0B3
ST2506                                       TO GFCTNK-FILIAL(IND-1)
0212             MOVE  WRK-CCTRL-CPF-CGC     TO GFCTNK-CONTROLE(IND-1)
BI1211        ELSE
                 MOVE GFCTNK-QTDE-OCOR TO WRK-QTDE-OCOR-PK
                 IF WRK-QTDE-OCOR LESS WRK-QTDE-OCOR-PK
                    MOVE  GFCTNK-QTDE-OCOR   TO WRK-QTDE-OCOR
                 END-IF
BI1211           ADD 1                       TO WRK-QTDE-OCOR
BI1211           MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212          END-IF
0212       END-IF.

      *----------------------------------------------------------------*
       2344-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR SEGMENTO.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2345-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C0.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C0.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C0.

           EXEC SQL
               SELECT CSGMTO_GSTAO_TARIF
               INTO  :GFCTB0C0.CSGMTO-GSTAO-TARIF
               FROM   DB2PRD.PDIDO_FLEXZ_SGMTO
               WHERE
                   CAGPTO_CTA          = :GFCTB0C0.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0C0.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C0.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0C0'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_SGMTO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0180'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGMTO-GSTAO-TARIF   OF GFCTB0C0
                                      TO CSGMTO-GSTAO-TARIF OF GFCTB0H5.

           EXEC SQL
               SELECT ISGMTO_GSTAO_TARIF
               INTO  :GFCTB0H5.ISGMTO-GSTAO-TARIF
               FROM  DB2PRD.TTPO_SGMTO_GSTAO
               WHERE CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0H5'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'TTPO_SGMTO_GSTAO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0190'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
030908         MOVE  'SEGMENTO NAO ENCONTRADO '
030908                                 TO ISGMTO-GSTAO-TARIF OF GFCTB0H5
           END-IF.

0212       MOVE  CSGMTO-GSTAO-TARIF    OF GFCTB0C0
0212                                   TO WRK-CSGMTO-TARIF-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2345-99-FIM
VI0618     END-IF.

0212       IF  GFCTNJ-FLT-CSGMTO-TARIF     EQUAL ZEROS      OR
0212          (GFCTNJ-FLT-CSGMTO-TARIF NOT EQUAL ZEROS      AND
0212           GFCTNJ-FLT-CSGMTO-TARIF     EQUAL WRK-CSGMTO-TARIF)
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CSGMTO-TARIF      TO GFCTNK-SEGMENTO(IND-1)
0212           MOVE  ISGMTO-GSTAO-TARIF    OF GFCTB0H5
0212                                       TO GFCTNK-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2345-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR MUNICIPIO.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2346-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0B7.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0B7.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0B7.

           EXEC SQL
               SELECT CMUN_IBGE
               INTO  :GFCTB0B7.CMUN-IBGE
               FROM   DB2PRD.PDIDO_FLEXZ_MUN
               WHERE
                   CAGPTO_CTA         = :GFCTB0B7.CAGPTO-CTA   AND
                   CSERVC_TARIF       = :GFCTB0B7.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB0B7.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B7'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_MUN'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0200'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           INITIALIZE AREA-RURC88.

           MOVE  CMUN-IBGE OF GFCTB0B7 TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  0732              TO GFCTNK-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE COD-RETORNO-RURC88
               WHEN 10
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0675           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0675'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 20
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0733           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0733'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 99
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0734           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0734'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.

0212       MOVE  CMUN-IBGE OF GFCTB0B7 TO WRK-CMUN-IBGE-S

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2346-99-FIM
VI0618     END-IF.

0212       IF  GFCTNJ-FLT-CMUN-IBGE       EQUAL ZEROS      OR
0212          (GFCTNJ-FLT-CMUN-IBGE  NOT  EQUAL ZEROS          AND
0212           GFCTNJ-FLT-CMUN-IBGE       EQUAL WRK-CMUN-IBGE )
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CMUN-IBGE     TO GFCTNK-MUNICIPIO(IND-1)
0212           MOVE  NOME-MUNIC-RURC88 TO GFCTNK-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2346-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA SELECIONAR UF.                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2347-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0C1.
           MOVE  CAGPTO-CTA            OF GFCTB0B2
                                       TO CAGPTO-CTA       OF GFCTB0C1.
           MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
                                       TO HIDTFD-FLEXZ-AGPTO
                                                           OF GFCTB0C1.

           EXEC SQL
               SELECT CSGL_UF
               INTO  :GFCTB0C1.CSGL-UF
               FROM   DB2PRD.PDIDO_FLEXZ_UF
               WHERE
                   CAGPTO_CTA          = :GFCTB0C1.CAGPTO-CTA   AND
                   CSERVC_TARIF        = :GFCTB0C1.CSERVC-TARIF AND
                   HIDTFD_FLEXZ_AGPTO  = :GFCTB0C1.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0C1'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_UF'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0210'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0926              TO GFCTNK-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE  CSGL-UF   OF GFCTB0C1 TO CSGL-UF          OF DCITV002.
           MOVE  DCITV002              TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNK-FIM
               MOVE  1                 TO GFCTNK-ERRO
               MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
               MOVE  0735              TO GFCTNK-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           EVALUATE WRK-DCIT-CODIGO-RETORNO
               WHEN 1
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0736           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0736'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 2
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0737           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0737'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 3
                   MOVE 'S'             TO GFCTNK-FIM
                   MOVE  1              TO GFCTNK-ERRO
                   MOVE  ZEROS          TO GFCTNK-COD-SQL-ERRO
                   MOVE  0738           TO GFCTNK-COD-MSG-ERRO
                   MOVE '0738'          TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG TO GFCTNK-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR

               WHEN 4
                   INITIALIZE               GFCT0M-ERRO-SQL
                   MOVE '0005'              TO GFCTG2-COD-MSG
                   PERFORM 1310-OBTER-DESC-MENSAGEM
                   MOVE ' - DCITV002'       TO WRK-NOME-TAB
                   MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
                   MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
                   MOVE 'V01_UF'            TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
                   MOVE  SQLCODE            TO GFCT0M-SQLCODE
                   MOVE '0220'              TO GFCT0M-LOCAL
                   MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
                   MOVE  9                  TO GFCTNK-ERRO
HEXA               MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.                  MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
                   MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
                   MOVE  0005               TO GFCTNK-COD-MSG-ERRO
                   PERFORM 1410-CONCATENAR-MENSAGEM
                   PERFORM 3000-FINALIZAR

           END-EVALUATE.


VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2347-99-FIM
VI0618     END-IF.

0212       IF (GFCTNJ-FLT-CSGL-UF         EQUAL SPACES   ) OR
0212          (GFCTNJ-FLT-CSGL-UF     NOT EQUAL SPACES     AND
0212           GFCTNJ-FLT-CSGL-UF         EQUAL CSGL-UF    OF GFCTB0C1)
0212           PERFORM 2348-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-AREA-DCLGEN-DCITV002
0212                                       TO DCITV002
0212           MOVE  IUF     OF DCITV002   TO GFCTNK-DESCR-AGPTO(IND-1)
0212           MOVE  CSGL-UF OF GFCTB0C1   TO GFCTNK-UF(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2347-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0212  *----------------------------------------------------------------*
0212   2348-MONTAR-OCOR-SAIDA          SECTION.
0212  *----------------------------------------------------------------*
0212
0212       ADD 1                       TO IND-1
0212
120510     COMPUTE GFCTNK-LL           = GFCTNK-LL + 180
0212
0212       MOVE  GFCTNK-QTDE-TOT-REG   TO WRK-QTDE-TOT-REG
0212       MOVE  GFCTNK-QTDE-OCOR      TO WRK-QTDE-OCOR
0212
0212       ADD   1                     TO WRK-QTDE-TOT-REG
0212                                      WRK-QTDE-OCOR.
0212
0212       MOVE  WRK-QTDE-TOT-REG      TO GFCTNK-QTDE-TOT-REG
0212       MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0212
0212       MOVE  CSERVC-TARIF          OF GFCTB0B2
0212                                   TO WRK-AUX-5-S
0212       MOVE  WRK-AUX-5             TO GFCTNK-COD-TARIFA(IND-1).
0212       MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
0212                                   TO GFCTNK-TIMESTAMP(IND-1).
0212       MOVE  1                     TO GFCTNK-SEQUENCIA(IND-1).
0212
0212       EVALUATE CSIT-SOLTC-FLEXZ OF GFCTB0B2
0212           WHEN 01
0212               MOVE 'PEDIDO         ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 02
0212               MOVE 'DEFERIDA       ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 03
0212               MOVE 'INDEFERIDA     ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 04
0212               MOVE 'INDEF POR PRAZO' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 05
0212               MOVE 'DEFERIDA CANC  ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 06
0212               MOVE 'PEDIDO CANCEL  ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 08
0212               MOVE 'ENCERRADA      ' TO GFCTNK-SITUACAO(IND-1)
0212
0212           WHEN 09
0212               MOVE 'CANCELADA TRAG ' TO GFCTNK-SITUACAO(IND-1)
0212
0212       END-EVALUATE.
0212
0212       PERFORM 2349-PESQUISAR-NOME-TARIFA.
0212
0212       MOVE  RSERVC-TARIF-REDZD    OF GFCTB0D8 (1:15)
0212                                   TO GFCTNK-DESCR-TARIFA(IND-1).
0212
0212       IF  GFCTNJ-FLT-CAGPTO-CTA  NOT EQUAL
0410                                      04 AND 09 AND 14 AND 15 AND
0410                                      17 AND 18 AND 19 AND 20 AND 26
0212
ST2506*        MOVE ZEROS              TO GFCTNK-CGC-CPF(IND-1)
ST2506*                                   GFCTNK-FILIAL(IND-1)
0212           MOVE ZEROS              TO GFCTNK-SEGMENTO(IND-1)
0212                                      GFCTNK-AGENCIA(IND-1)
0212                                      GFCTNK-POSTO(IND-1)
0212                                      GFCTNK-MUNICIPIO(IND-1)
0212                                      GFCTNK-CONTROLE(IND-1)
0410                                      GFCTNK-GRUPO-CTBIL (IND-1)
0410                                      GFCTNK-SUB-GRUPO-CTBIL
0410                                                         (IND-1)
ST2506         MOVE SPACES             TO GFCTNK-CGC-CPF(IND-1)
ST2506                                    GFCTNK-FILIAL(IND-1)
                                          GFCTNK-UF(IND-1)
0212                                      GFCTNK-DESCR-AGPTO(IND-1)
120510                                    GFCTNK-DESC-CTBIL (IND-1)
0212       END-IF.
0212
0212  *----------------------------------------------------------------*
0212   2348-99-FIM.                    EXIT.
0212  *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA PESQUISAR NOME TARIFA.                             *
      ******************************************************************
      *----------------------------------------------------------------*
       2349-PESQUISAR-NOME-TARIFA      SECTION.
      *----------------------------------------------------------------*

           MOVE  CSERVC-TARIF          OF GFCTB0B2
                                       TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
               SELECT RSERVC_TARIF_REDZD
               INTO  :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM   DB2PRD.SERVC_TARIF_PRINC
               WHERE  CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0010'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0D8'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0230'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0010               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2349-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C04.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2350-FECHA-CRS04                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR04-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0240'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0410  ******************************************************************
0410  * ROTINA PARA SELECIONAR RAZAO                                   *
0410  ******************************************************************
0410  *----------------------------------------------------------------*
0410   2351-SELECIONAR-RAZAO           SECTION.
0410  *----------------------------------------------------------------*
0410
0410       MOVE  CSERVC-TARIF          OF GFCTB0B2
0410                                   TO CSERVC-TARIF     OF GFCTB0M7.
0410       MOVE  CAGPTO-CTA            OF GFCTB0B2
0410                                   TO CAGPTO-CTA       OF GFCTB0M7.
0410       MOVE  HIDTFD-FLEXZ-AGPTO    OF GFCTB0B2
0410                                   TO HIDTFD-FLEXZ-AGPTO
0410                                                       OF GFCTB0M7
0410
0410       EXEC SQL
0410           SELECT
0410               CGRP_CTBIL_ATUAL,
0410               CSGRP_CTBIL
0410           INTO
0410               :GFCTB0M7.CGRP-CTBIL-ATUAL,
0410               :GFCTB0M7.CSGRP-CTBIL
0410           FROM DB2PRD.TPDIDO_FLEXZ_RZ
0410           WHERE
0410               CAGPTO_CTA          = :GFCTB0M7.CAGPTO-CTA   AND
0410               CSERVC_TARIF        = :GFCTB0M7.CSERVC-TARIF AND
0410               HIDTFD_FLEXZ_AGPTO  = :GFCTB0M7.HIDTFD-FLEXZ-AGPTO
0410       END-EXEC.
0410
0410       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0410          (SQLWARN0                 EQUAL 'W')
0410           INITIALIZE               GFCT0M-ERRO-SQL
0410           MOVE '0010'              TO GFCTG2-COD-MSG
0410           PERFORM 1310-OBTER-DESC-MENSAGEM
0410           MOVE ' - GFCTB0B9'       TO WRK-NOME-TAB
0410           MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
0410           MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
0410           MOVE 'TPDIDO_FLEXZ_RZ'   TO GFCT0M-NOME-TAB
0410           MOVE 'SELECT'            TO GFCT0M-COMANDO-SQL
0410           MOVE  SQLCODE            TO GFCT0M-SQLCODE
0410           MOVE '0160'              TO GFCT0M-LOCAL
0410           MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
0410           MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
0410           MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
0410           MOVE  0010               TO GFCTNK-COD-MSG-ERRO
0410           PERFORM 1410-CONCATENAR-MENSAGEM
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410       IF  SQLCODE                 EQUAL +100
0410           MOVE 'S'                TO GFCTNK-FIM
0410           MOVE  1                 TO GFCTNK-ERRO
0410           MOVE  ZEROS             TO GFCTNK-COD-SQL-ERRO
0410           MOVE  0926              TO GFCTNK-COD-MSG-ERRO
0410           MOVE '0926'             TO GFCTG2-COD-MSG
0410           PERFORM 1310-OBTER-DESC-MENSAGEM
0410           MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
0410           PERFORM 3000-FINALIZAR
0410       END-IF.
0410
0410
0410       MOVE  CGRP-CTBIL-ATUAL      OF GFCTB0M7
0410                                   TO WRK-CGRP-RZ-CTBIL-S
0410       MOVE  CSGRP-CTBIL           OF GFCTB0M7
0410                                   TO WRK-CSGRP-RZ-CTBIL-S
0410
110510     PERFORM 2540-BUSCAR-RAZAO-CONTABIL

VI0618     IF  ACU-DESPREZA            NOT EQUAL ZEROS
VI0618         ADD  1                  TO  IND-LIDOS
VI0618         GO  TO   2351-99-FIM
VI0618     END-IF.

0410       IF  GFCTNJ-FLT-GRUPO-CTBIL      EQUAL ZEROS            OR
0410          (GFCTNJ-FLT-GRUPO-CTBIL      EQUAL WRK-CGRP-RZ-CTBIL AND
0410           GFCTNJ-FLT-SUB-GRUPO-CTBIL  EQUAL WRK-CSGRP-RZ-CTBIL)
0410           PERFORM 2348-MONTAR-OCOR-SAIDA
0410           MOVE  WRK-CGRP-RZ-CTBIL  TO GFCTNK-GRUPO-CTBIL    (IND-1)
0410           MOVE  WRK-CSGRP-RZ-CTBIL TO GFCTNK-SUB-GRUPO-CTBIL(IND-1)
0410           MOVE  IRZ-CTBIL OF PLCCB004 TO
                                           GFCTNK-DESC-CTBIL (IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNK-QTDE-OCOR
0410       END-IF.
0410
0410  *----------------------------------------------------------------*
0410   2351-99-FIM.                    EXIT.
0410  *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C05.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2360-FECHA-CRS05                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0250'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C06.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2370-FECHA-CRS06                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR06-GFCTB0B2
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
                                           WRK-AUX-9-S
               MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
               MOVE '0260'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CURSOR1.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-FECHAR-CURSOR1             SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-SITUACAO
               WHEN  01
                   PERFORM 2410-FECHAR-CRS-C01

               WHEN  02
                   PERFORM 2420-FECHAR-CRS-C02

               WHEN  03
                   PERFORM 2430-FECHAR-CRS-C03

           END-EVALUATE.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C01.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-FECHAR-CRS-C01             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0270'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C02.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2420-FECHAR-CRS-C02             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0280'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FECHAR CRS C03.                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2430-FECHAR-CRS-C03             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR03-GFCTB0B2-JN
           END-EXEC.

           IF (SQLCODE                  NOT EQUAL ZEROS) OR
              (SQLWARN0                 EQUAL 'W')
               INITIALIZE               GFCT0M-ERRO-SQL
               MOVE '0011'              TO GFCTG2-COD-MSG
               PERFORM 1310-OBTER-DESC-MENSAGEM
               MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
               MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
               MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
               MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0290'              TO GFCT0M-LOCAL
               MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE  9                  TO GFCTNK-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
               MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
               MOVE  0011               TO GFCTNK-COD-MSG-ERRO
               PERFORM 1410-CONCATENAR-MENSAGEM
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2430-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0212  ******************************************************************
0212  * ROTINA PARA SELECIONAR CAMPOS                                  *
0212  ******************************************************************
0212  *----------------------------------------------------------------*
0212   2510-FETCH-CRS04              SECTION.
0212  *----------------------------------------------------------------*
0212
0212       EXEC SQL
0212           FETCH CSR04-GFCTB0B2 INTO
0212               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
0212               :GFCTB0B2.DINIC-FLEXZ-AGPTO,
0212               :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0212               :GFCTB0B2.CINDCD-AGPTO-TOT
0212       END-EXEC.
0212
0212       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0212          (SQLWARN0                 EQUAL 'W'       )
0212           INITIALIZE               GFCT0M-ERRO-SQL
0212           MOVE '0010'              TO GFCTG2-COD-MSG
0212           PERFORM 1310-OBTER-DESC-MENSAGEM
0212           MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
0212           MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
0212           MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
0212           MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0212           MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
0212           MOVE  SQLCODE            TO GFCT0M-SQLCODE
0212                                       WRK-AUX-9-S
0212           MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
0212           MOVE '0300'              TO GFCT0M-LOCAL
0212           MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
0212           MOVE  9                  TO GFCTNK-ERRO
0212           MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
0212           MOVE  0010               TO GFCTNK-COD-MSG-ERRO
0212           PERFORM 1410-CONCATENAR-MENSAGEM
0212           PERFORM 3000-FINALIZAR
0212       END-IF.
0212
0212       IF  SQLCODE                 EQUAL +100
0212           MOVE 'S'                TO WRK-FIM-SUB
0212       END-IF.
0212
0212  *----------------------------------------------------------------*
0212   2510-99-FIM.                  EXIT.
0212  *----------------------------------------------------------------*

0212  ******************************************************************
0212  * ROTINA PARA SELECIONAR CAMPOS                                  *
0212  ******************************************************************
0212  *----------------------------------------------------------------*
0212   2520-FETCH-CRS05              SECTION.
0212  *----------------------------------------------------------------*
0212
0212       EXEC SQL
0212           FETCH CSR05-GFCTB0B2 INTO
0212               :GFCTB0B2.CSERVC-TARIF,
0212               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
0212               :GFCTB0B2.DINIC-FLEXZ-AGPTO,
0212               :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0212               :GFCTB0B2.CINDCD-AGPTO-TOT
0212       END-EXEC.
0212
0212       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0212          (SQLWARN0                 EQUAL 'W'       )
0212           INITIALIZE               GFCT0M-ERRO-SQL
0212           MOVE '0010'              TO GFCTG2-COD-MSG
0212           PERFORM 1310-OBTER-DESC-MENSAGEM
0212           MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
0212           MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
0212           MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
0212           MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0212           MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
0212           MOVE  SQLCODE            TO GFCT0M-SQLCODE
0212                                       WRK-AUX-9-S
0212           MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
0212           MOVE '0310'              TO GFCT0M-LOCAL
0212           MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
0212           MOVE  9                  TO GFCTNK-ERRO
0212           MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
0212           MOVE  0010               TO GFCTNK-COD-MSG-ERRO
0212           PERFORM 1410-CONCATENAR-MENSAGEM
0212           PERFORM 3000-FINALIZAR
0212       END-IF.
0212
0212       IF  SQLCODE                 EQUAL +100
0212           MOVE 'S'                TO WRK-FIM-SUB
0212       END-IF.
0212
0212  *----------------------------------------------------------------*
0212   2520-99-FIM.                  EXIT.
0212  *----------------------------------------------------------------*

0212  ******************************************************************
0212  * ROTINA PARA SELECIONAR CAMPOS                                  *
0212  ******************************************************************
0212  *----------------------------------------------------------------*
0212   2530-FETCH-CRS06              SECTION.
0212  *----------------------------------------------------------------*
0212
0212       EXEC SQL
0212           FETCH CSR06-GFCTB0B2 INTO
0610               :GFCTB0B2.CSERVC-TARIF,
0212               :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
0212               :GFCTB0B2.DINIC-FLEXZ-AGPTO,
0212               :GFCTB0B2.CSIT-SOLTC-FLEXZ,
0212               :GFCTB0B2.CINDCD-AGPTO-TOT
0212       END-EXEC.
0212
0212       IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
0212          (SQLWARN0                 EQUAL 'W'       )
0212           INITIALIZE               GFCT0M-ERRO-SQL
0212           MOVE '0010'              TO GFCTG2-COD-MSG
0212           PERFORM 1310-OBTER-DESC-MENSAGEM
0212           MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
0212           MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
0212           MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
0212           MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
0212           MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
0212           MOVE  SQLCODE            TO GFCT0M-SQLCODE
0212                                       WRK-AUX-9-S
0212           MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
0212           MOVE '0320'              TO GFCT0M-LOCAL
0212           MOVE  SQLCA              TO GFCT0M-SQLCA-AREA
0212           MOVE  9                  TO GFCTNK-ERRO
0212           MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
0212           MOVE  0010               TO GFCTNK-COD-MSG-ERRO
0212           PERFORM 1410-CONCATENAR-MENSAGEM
0212           PERFORM 3000-FINALIZAR
0212       END-IF.
0212
0212       IF  SQLCODE                 EQUAL +100
0212           MOVE 'S'                TO WRK-FIM-SUB
0212       END-IF.

2106       IF  SQLCODE                 EQUAL ZEROS
2106           MOVE DPROCM-ATUAL OF GFCTB0A1
2106                                   TO WRK-DATA-DB2
2106           MOVE WRK-DIA-DB2        TO WRK-DIA-I
2106           MOVE WRK-MES-DB2        TO WRK-MES-I
2106           MOVE WRK-ANO-DB2        TO WRK-ANO-I
2106           MOVE WRK-DATA-INV       TO WRK-DPROCM-ATUAL-INV
2106
2106           MOVE DINIC-FLEXZ-AGPTO OF GFCTB0B2
2106                                   TO WRK-DATA-DB2
2106           MOVE WRK-DIA-DB2        TO WRK-DIA-I
2106           MOVE WRK-MES-DB2        TO WRK-MES-I
2106           MOVE WRK-ANO-DB2        TO WRK-ANO-I
2106           MOVE WRK-DATA-INV       TO WRK-DINI-PRMSS-INV
2106
2106           MOVE DFIM-FLEXZ-AGPTO OF GFCTB0B2
2106                                   TO WRK-DATA-DB2
2106           MOVE WRK-DIA-DB2        TO WRK-DIA-I
2106           MOVE WRK-MES-DB2        TO WRK-MES-I
2106           MOVE WRK-ANO-DB2        TO WRK-ANO-I
2106           MOVE WRK-DATA-INV       TO WRK-DFIM-PRMSS-INV
2106       END-IF.

0212  *----------------------------------------------------------------*
0212   2530-99-FIM.                  EXIT.
0212  *----------------------------------------------------------------*
      ******************************************************************
      *ACESSAR PLCC PARA BUSCAR DESCRICAO RAZAO CONTABIL               *
      ******************************************************************
0510  *----------------------------------------------------------------*
0510   2540-BUSCAR-RAZAO-CONTABIL SECTION.
0510  *----------------------------------------------------------------*
0510
0510       EXEC SQL
0510           SELECT  IRZ_CTBIL
0510           INTO   :IRZ-CTBIL
0510           FROM    DB2PRD.RAZAO_CONTABIL
0510           WHERE   CEMPR = 237
0510           AND     CGRP_CTBIL_ATUAL =:GFCTB0M7.CGRP-CTBIL-ATUAL
0510           AND     CSGRP_CTBIL      =:GFCTB0M7.CSGRP-CTBIL
0510       END-EXEC.
0510
0510       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0510          (SQLWARN0                EQUAL 'W'      )
0510           INITIALIZE              GFCT0M-ERRO-SQL
0510           MOVE '0010'             TO GFCTG2-COD-MSG
0212           PERFORM 1310-OBTER-DESC-MENSAGEM
0510           MOVE 'RAZAO_CONTABIL   '
0510                                   TO GFCT0M-NOME-TAB
0510           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0510           MOVE '0075'             TO GFCT0M-LOCAL
0510           MOVE  0010              TO GFCTNK-COD-MSG-ERRO
HEXA           MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
.              MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
0510           MOVE  9                 TO GFCTNK-ERRO
0510           MOVE 'S'                TO GFCTNK-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
0212           PERFORM 1410-CONCATENAR-MENSAGEM
0212           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
0510__*____IF__SQLCODE_________________EQUAL_+100
0510__*________MOVE__1_________________TO_GFCTNK-ERRO
0510__*________MOVE_ZEROS______________TO_GFCTNK-COD-SQL-ERRO
0510__*________MOVE__1690______________TO_GFCTNK-COD-MSG-ERRO
0510__*________MOVE_'1690'_____________TO_GFCTG2-COD-MSG
0510__*________MOVE_'S'________________TO_GFCTNK-FIM
0510__*________MOVE_GFCTG3-DESC-MSG____TO_GFCTNK-DESC-MSG-ERRO
0212__*________PERFORM_1410-CONCATENAR-MENSAGEM
0510__*________PERFORM_3000-FINALIZAR
0510__*____END-IF.
0510
           IF SQLCODE EQUAL +100
              MOVE 'NAO IDENTIFICADO' TO IRZ-CTBIL
           END-IF.

0510  *----------------------------------------------------------------*
0510   2540-99-FIM.                    EXIT.
0510  *----------------------------------------------------------------*

060916******************************************************************
060916* ROTINA PARA PROCESSAR-CURSOR-07-08-09                          *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2600-PROC-SEM-CJTO-SERVICO      SECTION.
060916*----------------------------------------------------------------*
060916
060916     MOVE  GFCTNK-FLT-CAGPTO-CTA     TO CAGPTO-CTA   OF GFCTB0B2
060916
060916     MOVE  'N'                       TO WRK-FIM-SUB
060916
060916     MOVE  DPROCM-ATUAL OF GFCTB0A1  TO
060916                                     WRK-DINIC-FLEXZ
060916                                     WRK-DFIM-FLEXZ
060916
060916     MOVE  GFCTNJ-QTDE-OCOR          TO  ACU-DESPREZA
060916
060916     EVALUATE GFCTNJ-FLT-TIPO-HIST
060916         WHEN 'T'
060916             MOVE  01                TO WRK-SITUACAO
060916             PERFORM 2620-ABRIR-CRS-C07
060916             IF    ACU-DESPREZA GREATER ZEROS
060916                   PERFORM 2670-FETCH-CRS07 ACU-DESPREZA TIMES
060916             END-IF
060916             PERFORM 2670-FETCH-CRS07
060916
060916         WHEN 'P'
060916             MOVE  02                TO WRK-SITUACAO
060916             PERFORM 2640-ABRIR-CRS-C08
060916             IF    ACU-DESPREZA GREATER ZEROS
060916                   PERFORM 2680-FETCH-CRS08 ACU-DESPREZA TIMES
060916             END-IF
060916             PERFORM 2680-FETCH-CRS08
060916
060916         WHEN OTHER
060916             MOVE  03                TO WRK-SITUACAO
060916             PERFORM 2660-ABRIR-CRS-C09
060916             IF    ACU-DESPREZA GREATER ZEROS
060916                   PERFORM 2690-FETCH-CRS09 ACU-DESPREZA TIMES
060916             END-IF
060916             PERFORM 2690-FETCH-CRS09
060916     END-EVALUATE.
060916
060916     IF  WRK-FIM-SUB             EQUAL 'S'
060916         MOVE 'S'                TO GFCTNK-FIM
060916         MOVE  ZEROS             TO GFCTNK-ERRO
060916                                    GFCTNK-COD-SQL-ERRO
060916         EVALUATE WRK-SITUACAO
060916             WHEN 01
060916                 MOVE 0827          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0827'        TO GFCTG2-COD-MSG
060916             WHEN 02
060916                 MOVE 0828          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0828'        TO GFCTG2-COD-MSG
060916             WHEN 03
060916                 MOVE 0829          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0829'        TO GFCTG2-COD-MSG
060916         END-EVALUATE
060916
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916     MOVE  ZEROS                 TO IND-1.
060916
060916     MOVE 'N'                    TO WRK-FIM-SUB
060916
060916     PERFORM 2700-MOVER-CAMPOS    UNTIL
060916                      IND-1     GREATER 4  OR
060916                      WRK-FIM-SUB   EQUAL 'S'.
060916
060916     IF  WRK-SITUACAO         EQUAL  01
060916         PERFORM 2710-FECHA-CRS07
060916
060916     ELSE
060916         IF  WRK-SITUACAO     EQUAL  02
060916             PERFORM 2720-FECHA-CRS08
060916
060916         ELSE
060916             IF  WRK-SITUACAO EQUAL  03
060916                 PERFORM 2730-FECHA-CRS09
060916
060916             END-IF
060916         END-IF
060916     END-IF.
060916
060916     IF  IND-1                EQUAL 4
060916         MOVE 'S'                TO WRK-FINALIZAR
060916     ELSE
060916       IF  WRK-FIM-SUB             EQUAL 'S'
060916         MOVE 'S'                TO GFCTNK-FIM
060916         MOVE  ZEROS             TO GFCTNK-ERRO
060916                                    GFCTNK-COD-SQL-ERRO
060916         EVALUATE WRK-SITUACAO
060916             WHEN 01
060916                 MOVE 0827          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0827'        TO GFCTG2-COD-MSG
060916
060916             WHEN 02
060916                 MOVE 0828          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0828'        TO GFCTG2-COD-MSG
060916             WHEN 03
060916                 MOVE 0829          TO GFCTNK-COD-MSG-ERRO
060916                 MOVE '0829'        TO GFCTG2-COD-MSG
060916         END-EVALUATE
060916
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE GFCTG3-DESC-MSG    TO GFCTNK-DESC-MSG-ERRO
060916         PERFORM 3000-FINALIZAR
060916       END-IF
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2600-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA ABRIR CURSOR C07                                   *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2620-ABRIR-CRS-C07            SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         OPEN CSR07-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916         MOVE '0340'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SQLCODE            TO WRK-COD-SQL-ERRO
060916         MOVE  WRK-COD-SQL-ERRO-2 TO GFCTNK-COD-SQL-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2620-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA ABRIR CURSOR C08                                   *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2640-ABRIR-CRS-C08              SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         OPEN CSR08-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0350'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2640-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA ABRIR CURSOR C09                                   *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2660-ABRIR-CRS-C09         SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         OPEN CSR09-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'OPEN'              TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0360'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2660-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA SELECIONAR CAMPOS CURSOR 07                        *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2670-FETCH-CRS07              SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         FETCH CSR07-GFCTB0B2 INTO
060916             :GFCTB0B2.CSERVC-TARIF,
060916             :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
060916             :GFCTB0B2.DINIC-FLEXZ-AGPTO,
060916             :GFCTB0B2.CSIT-SOLTC-FLEXZ,
060916             :GFCTB0B2.CINDCD-AGPTO-TOT
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
060916        (SQLWARN0                 EQUAL 'W'       )
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0370'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916     IF  SQLCODE                 EQUAL +100
060916         MOVE 'S'                TO WRK-FIM-SUB
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2670-99-FIM.                  EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA SELECIONAR CAMPOS CURSOR 08                        *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2680-FETCH-CRS08              SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         FETCH CSR08-GFCTB0B2 INTO
060916             :GFCTB0B2.CSERVC-TARIF,
060916             :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
060916             :GFCTB0B2.DINIC-FLEXZ-AGPTO,
060916             :GFCTB0B2.CSIT-SOLTC-FLEXZ,
060916             :GFCTB0B2.CINDCD-AGPTO-TOT
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
060916        (SQLWARN0                 EQUAL 'W'       )
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0380'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916     IF  SQLCODE                 EQUAL +100
060916         MOVE 'S'                TO WRK-FIM-SUB
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2680-99-FIM.                  EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA SELECIONAR CAMPOS CURSOR 09                        *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2690-FETCH-CRS09              SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         FETCH CSR09-GFCTB0B2 INTO
060916             :GFCTB0B2.CSERVC-TARIF,
060916             :GFCTB0B2.HIDTFD-FLEXZ-AGPTO,
060916             :GFCTB0B2.DINIC-FLEXZ-AGPTO,
060916             :GFCTB0B2.CSIT-SOLTC-FLEXZ,
060916             :GFCTB0B2.CINDCD-AGPTO-TOT
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
060916        (SQLWARN0                 EQUAL 'W'       )
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0010'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'FETCH '            TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0390'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000010            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916     IF  SQLCODE                 EQUAL +100
060916         MOVE 'S'                TO WRK-FIM-SUB
060916     END-IF.
060916
060916     IF  SQLCODE                 EQUAL ZEROS
060916         MOVE DPROCM-ATUAL OF GFCTB0A1
060916                                 TO WRK-DATA-DB2
060916         MOVE WRK-DIA-DB2        TO WRK-DIA-I
060916         MOVE WRK-MES-DB2        TO WRK-MES-I
060916         MOVE WRK-ANO-DB2        TO WRK-ANO-I
060916         MOVE WRK-DATA-INV       TO WRK-DPROCM-ATUAL-INV
060916
060916         MOVE DINIC-FLEXZ-AGPTO OF GFCTB0B2
060916                                 TO WRK-DATA-DB2
060916         MOVE WRK-DIA-DB2        TO WRK-DIA-I
060916         MOVE WRK-MES-DB2        TO WRK-MES-I
060916         MOVE WRK-ANO-DB2        TO WRK-ANO-I
060916         MOVE WRK-DATA-INV       TO WRK-DINI-PRMSS-INV
060916
060916         MOVE DFIM-FLEXZ-AGPTO OF GFCTB0B2
060916                                 TO WRK-DATA-DB2
060916         MOVE WRK-DIA-DB2        TO WRK-DIA-I
060916         MOVE WRK-MES-DB2        TO WRK-MES-I
060916         MOVE WRK-ANO-DB2        TO WRK-ANO-I
060916         MOVE WRK-DATA-INV       TO WRK-DFIM-PRMSS-INV
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2690-99-FIM.                  EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA MOVIMENTACAO DE CAMPOS.                            *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2700-MOVER-CAMPOS               SECTION.
060916*----------------------------------------------------------------*
060916
060916     IF  WRK-SITUACAO            EQUAL 01 OR
060916        (WRK-SITUACAO            EQUAL 02 AND
060916         CSIT-SOLTC-FLEXZ OF GFCTB0B2
060916                                 EQUAL 01) OR
060916        (WRK-SITUACAO            EQUAL 03 AND
060916         CSIT-SOLTC-FLEXZ OF GFCTB0B2
060916                                 EQUAL 02 AND
060916         WRK-DINI-PRMSS-INV      GREATER WRK-DPROCM-ATUAL-INV)
060916
060916         PERFORM 2348-MONTAR-OCOR-SAIDA
060916
060916     END-IF.
060916
060916     EVALUATE WRK-SITUACAO
060916         WHEN  01
060916             PERFORM 2670-FETCH-CRS07
060916
060916         WHEN  02
060916             PERFORM 2680-FETCH-CRS08
060916
060916         WHEN  03
060916             PERFORM 2690-FETCH-CRS09
060916     END-EVALUATE.
060916
060916     IF  IND-1                EQUAL 4
060916         MOVE 'S'                TO WRK-FIM-SUB
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2700-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA FECHAR CRS C07.                                    *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2710-FECHA-CRS07                SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         CLOSE CSR07-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0011'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0400'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000011            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2710-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA FECHAR CRS C08.                                    *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2720-FECHA-CRS08                SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         CLOSE CSR08-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0011'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0410'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000011            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2720-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*
060916
060916******************************************************************
060916* ROTINA PARA FECHAR CRS C09.                                    *
060916******************************************************************
060916*----------------------------------------------------------------*
060916 2730-FECHA-CRS09                SECTION.
060916*----------------------------------------------------------------*
060916
060916     EXEC SQL
060916         CLOSE CSR09-GFCTB0B2
060916     END-EXEC.
060916
060916     IF (SQLCODE                  NOT EQUAL ZEROS) OR
060916        (SQLWARN0                 EQUAL 'W')
060916         INITIALIZE               GFCT0M-ERRO-SQL
060916         MOVE '0011'              TO GFCTG2-COD-MSG
060916         PERFORM 1310-OBTER-DESC-MENSAGEM
060916         MOVE ' - GFCTB0B2'       TO WRK-NOME-TAB
060916         MOVE 'GFCT0883'          TO GFCT0M-PROGRAMA
060916         MOVE 'DB2'               TO GFCT0M-TIPO-ACESSO
060916         MOVE 'PDIDO_FLEXZ_AGPTO' TO GFCT0M-NOME-TAB
060916         MOVE 'CLOSE'             TO GFCT0M-COMANDO-SQL
060916         MOVE  SQLCODE            TO GFCT0M-SQLCODE
060916                                     WRK-AUX-9-S
060916         MOVE  WRK-AUX-9-3        TO GFCTNK-COD-SQL-ERRO
060916         MOVE '0420'              TO GFCT0M-LOCAL
060916         MOVE  SQLCA (1:136)      TO GFCT0M-SQLCA-AREA
060916         MOVE  9                  TO GFCTNK-ERRO
060916         MOVE  SPACES             TO GFCTNK-DESC-MSG-ERRO
060916         MOVE  0000011            TO GFCTNK-COD-MSG-ERRO
060916         PERFORM 1410-CONCATENAR-MENSAGEM
060916         PERFORM 3000-FINALIZAR
060916     END-IF.
060916
060916*----------------------------------------------------------------*
060916 2730-99-FIM.                    EXIT.
060916*----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZAR PROCESSAMENTO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
