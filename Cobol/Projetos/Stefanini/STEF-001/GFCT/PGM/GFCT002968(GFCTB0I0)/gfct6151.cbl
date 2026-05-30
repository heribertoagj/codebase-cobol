*================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT6151.
       AUTHOR.     PAGNOCCA - SONDA IT.

      ******************************************************************
      *   A L E R T A     A L E R T A     A L E R T A     A L E R T A  *
      *                                                                *
      * ESTE PROGRAMA UTILIZA LETRAS EM MINUSCULO P/ CAMPO  AMBIENTE   *
      *                                                                *
      *  AO ALTERAR O PROGRAMA NO TSO UTILIZAR CAPS OFF E CAPS ON      *
      *  AO ALTERAR O PROGRAMA NO ROSCOE UTILIZAR SET MODE X           *
      *                                                                *
      *  MAS PRINCIPALMENTE VALIDAR ANTES DE ENVIAR PARA A PRODUCAO    *
      *  SE AS COMPARACOES DO CAMPO AMBIENTE ESTAO CORRETAS COM b E o  *
      *                                                                *
      *   A L E R T A     A L E R T A     A L E R T A     A L E R T A  *
      ******************************************************************

      *================================================================*
      *      S O N D A  I T                                            *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA....:   GFCT6151                                    *
      *    DATA........:   DEZ/2014                                    *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    OBJETIVO....:   MODULO PARA INCLUSAO DE ADESAO INDIVIDUAL   *
      *                    A PACOTES CESTA CELULAR (IB)                *
      *                                                                *
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                       INCLUDE/BOOK      *
      *                    DB2PRD.PARM_DATA_PROCM      GFCTB0A1        *
      *                    DB2PRD.PARM_SERVC_TARIF     GFCTB0A2        *
      *                    DB2PRD.PRMSS_ADSAO_PCOTE    GFCTB0C6        *
      *                    DB2PRD.PRMSS_GRP_DEPDC      GFCTB0C7        *
      *                    DB2PRD.PRMSS_GRP_MUN        GFCTB0C9        *
      *                    DB2PRD.PRMSS_GRP_PAB        GFCTB0D0        *
      *                    DB2PRD.PRMSS_GRP_PSTAL      GFCTB0D1        *
      *                    DB2PRD.PRMSS_GRP_SGMTO      GFCTB0D2        *
      *                    DB2PRD.PRMSS_GRP_UF         GFCTB0D3        *
      *                    DB2PRD.SERVC_TARIF_PRINC    GFCTB0D8        *
      *                    DB2PRD.TPRMSS_GRP_CLI       GFCTB0I0        *
      *                    DB2PRD.V01CLIENTE_AGENCIA   CLIEV004        *
      *                    DB2PRD.V01CTA_POUPANCA      CLIEV007        *
      *                    DB2PRD.V01CTA_CORRENTE      CLIEV008        *
      *                    DB2PRD.ADSAO_INDVD_PCOTE    GFCTB009        *
      *                    DB2PRD.TADSAO_INDVD_CLULR   GFCTB0M2        *
      *                    DB2PRD.TOPER_FONE_CREDT     GFCTB0M4        *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    BOOK'S......:                                               *
      *       GFCTWANB - ENTRADA E SAIDA - INC MODULO CORPORATIVO      *
      *                  GFCT6151                                      *
      *       I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.        *
      *       I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.        *
      *       I#GFCTJM - ENTRADA E SAIDA - OBTER COD SEG GESTAO TARIFA *
      *       I#GFCTRK - ENTRADA E SAIDA - OBTER COD UF E MUNICIPIO DA *
      *                  AGENCIA.                                      *
BI0710*       GFCTWAN7 - AREA DE COMUNICACAO COM GFCT5307 - SAIDA      *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.                 *
      *    GFCT5534 - OBTER CODIGO SEGMENTO GESTAO TARIFA.             *
      *    GFCT5564 - OBTER COD DA UF E MUNICIPIO DA AGENCIA.          *
BI0710*    GFCT5507 - VALIDAR NUMERO DO CELULAR NA ADESAO.             *
      *                                                                *
      *                                                                *
      * FASE = '1' - OPERADORA = ZEROS -> SOH CONSISTE                 *
      * FASE = '2' - FAZ CONSISTENCIA COMPLETA                         *
      * FASE = '3' - FAZ ADESAO                                        *
      *                                                                *

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
       01  FILLER                      PIC  X(050)         VALUE
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

       01  WRK-DATA-DMAB               PIC  X(10)   VALUE '  .  .    '.
       01  FILLER REDEFINES WRK-DATA-DMAB.
           10 WRK-DATA-DMAB-D          PIC  9(002).
           10 FILLER                   PIC  X(001).
           10 WRK-DATA-DMAB-M          PIC  9(002).
           10 FILLER                   PIC  X(001).
           10 WRK-DATA-DMAB-A          PIC  9(004).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-NUM-CEL                 PIC +9(11)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S            PIC  X(01).
           05 WRK-NUM-CEL-Z            PIC  9(02).
           05 WRK-NUM-CEL-9            PIC  9(01).
           05 WRK-NUM-CEL-8            PIC  9(08).
       01  FILLER            REDEFINES WRK-NUM-CEL.
           05 WRK-NUM-CEL-S2           PIC  X(01).
           05 WRK-NUM-CEL-11           PIC  9(11).

       01  WRK-TAR-CEL                 PIC +9(05)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-TAR-CEL.
           05 WRK-TAR-CEL-S            PIC  X(01).
           05 WRK-TAR-CEL-5            PIC  9(05).

       01  WRK-OPE-CEL                 PIC +9(09)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-OPE-CEL.
           05 WRK-OPE-CEL-S            PIC  X(01).
           05 WRK-OPE-CEL-9            PIC  9(09).

       01  WRK-DDD-CEL                 PIC +9(03)          VALUE ZEROS.
       01  FILLER            REDEFINES WRK-DDD-CEL.
           05 WRK-DDD-CEL-S            PIC  X(01).
           05 WRK-DDD-CEL-3            PIC  9(03).


       01  WRK-DT-PROC-A1              PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROC-A1-R            REDEFINES WRK-DT-PROC-A1.
           03  WRK-ANO-PROC-A1         PIC  X(004).
           03  WRK-MES-PROC-A1         PIC  X(002).
           03  WRK-DIA-PROC-A1         PIC  X(002).


       01  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

       01  WRK-TIPO-PESSOA-CLIENTE     PIC  X(001)         VALUE SPACES.

       01  WRK-TIPO-CONTA-CLIENTE      PIC S9(001) COMP-3  VALUE ZEROS.

       01  WRK-RESTO                   PIC  9(003) COMP-3  VALUE ZEROS.
JUN10  01  WRK-INTEIRO                 PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-FIM-CURSOR              PIC  X(001)         VALUE 'N'.
       01  WRK-FIM-AGRUPAMENTO         PIC  X(001)         VALUE SPACES.

       01  WRK-TARIFA-VALIDA           PIC  X(001)         VALUE SPACES.

       01  WRK-1-VEZ-SEGMENTO          PIC  X(001)         VALUE 'N'.
       01  WRK-1-VEZ-CNPJ-CPF          PIC  X(001)         VALUE 'N'.
       01  WRK-1-VEZ-UF-MUNIC          PIC  X(001)         VALUE 'N'.

       01  WRK-EXISTE-COMPONENTE       PIC  X(001)         VALUE 'N'.
       01  WRK-EXISTE-AGRUPAMENTO      PIC  X(001)         VALUE SPACES.
       01  WRK-AGRUPAMENTO-VALIDO      PIC  X(001)         VALUE SPACES.
       01  WRK-PROX-AGRUPAMENTO        PIC  9(003)         VALUE ZEROS.

BI0412 01  WRK-CINDCD-ADSAO-INDVD      PIC  X(001)         VALUE 'I'.
BI0412 01  WRK-CINDCD-ADSAO-AMBAS      PIC  X(001)         VALUE 'A'.

       01  WRK-SEQ-AGPTO               PIC S9(009) COMP-3  VALUE ZEROS.

       01  WRK-DINIC-PRMSS-PCOTE       PIC  X(010)         VALUE SPACES.

       01  WRK-DT-PROCM-ATUAL          PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-PROCM-ATUAL-R        REDEFINES WRK-DT-PROCM-ATUAL.
           03  WRK-ANO-PROCM-ATUAL     PIC  9(004).
           03  WRK-MES-PROCM-ATUAL     PIC  9(002).
           03  WRK-DIA-PROCM-ATUAL     PIC  9(002).

       01  WRK-DT-INI-PRMSS-PCOTE      PIC  9(008)         VALUE ZEROS.
       01  WRK-DT-INI-PRMSS-PCOTE-R    REDEFINES WRK-DT-INI-PRMSS-PCOTE.
           03  WRK-ANO-INI-PRMSS-PCOTE PIC  9(004).
           03  WRK-MES-INI-PRMSS-PCOTE PIC  9(002).
           03  WRK-DIA-INI-PRMSS-PCOTE PIC  9(002).

       01  WRK-DT-INI-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DT-INI-ADSAO-INDVD.
           03  WRK-AAAAMM-INI-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-INI-ADSAO-INDVD.
               05  WRK-ANO-INI-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-INI-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-INI-ADSAO-INDVD PIC  9(002).

       01  WRK-DT-FIM-ADSAO-INDVD      PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DT-FIM-ADSAO-INDVD.
           03  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       PIC  9(006).
           03  FILLER                  REDEFINES
                                             WRK-AAAAMM-FIM-ADSAO-INDVD.
               05  WRK-ANO-FIM-ADSAO-INDVD
                                       PIC  9(004).
               05  WRK-MES-FIM-ADSAO-INDVD
                                       PIC  9(002).
           03  WRK-DIA-FIM-ADSAO-INDVD PIC  9(002).

       01  WRK-HINCL-REG               PIC  X(026)         VALUE SPACES.

       01  WRK-CESTA-CELULAR           PIC  X(001)         VALUE SPACES.

______*01__WRK-COUNT______*______*_____PIC_S9(009)_COMP____VALUE_ZEROS.

       01  WRK-MES                     PIC  9(002) COMP-3  VALUE ZEROS.
       01  WRK-ANO                     PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*  AREA PARA MENSAGEM INTERNA  *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-SCCE9100.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA MENSAGENS MODULO GFCT5523 - MENSAGENS *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-1                    PIC  X(070)        VALUE
           'NAO FOI POSSIVEL OBTER A MSG NO MODULO GFCT5523'.

       01  WRK-MSG-2                    PIC  X(070)        VALUE
           'PROBLEMA NO ACESSO A BASE DE MENSAGENS'.

       01  WRK-MSG-3                    PIC  X(070)        VALUE
           'MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE       PIC  X(001)         VALUE SPACES.
           05  WRK-5527-CHAMADOR       PIC  X(008)         VALUE SPACES.
           05  WRK-5527-AGENCIA-CLIENTE
                                       PIC  9(005)         VALUE ZEROS.
           05  WRK-5527-CONTA-CLIENTE  PIC  9(013)         VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
           05  WRK-5527-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           05  WRK-5527-COD-SQL-ERRO   PIC S9(003)         VALUE ZEROS.
           05  WRK-5527-COD-MSG-GFCT   PIC  9(004)         VALUE ZEROS.
           05  WRK-5527-DESC-MSG       PIC  X(070)         VALUE SPACES.
           05  WRK-5527-QTD-OCORR      PIC  9(005)         VALUE ZEROS.
           05  WRK-5527-DADOS-RETORNO.
ST25X6*        10  WRK-5527-CCGC-CPF   PIC  9(009)         VALUE ZEROS.
ST25X6         10  WRK-5527-CCGC-CPF   PIC  X(009)         VALUE SPACES.
ST25X6*        10  WRK-5527-CFLIAL-CGC PIC  9(004)         VALUE ZEROS.
ST25X6         10  WRK-5527-CFLIAL-CGC PIC  X(004)         VALUE SPACES.
               10  WRK-5527-CCTRL-CPF-CGC
                                       PIC  9(002)         VALUE ZEROS.
               10  WRK-5527-CPOSTO-SERVC
                                       PIC  9(003)         VALUE ZEROS.
               10  WRK-5527-CSGMTO-CLI PIC  9(003)         VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM             PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-IO-STA              PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  WRK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME         PIC  X(008)         VALUE SPACES.
           03  FILLER                  PIC  X(002)         VALUE SPACES.
           03  WRK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(012)         VALUE SPACES.
           03  WRK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *----------------------------------------------------------------*

______*COPY_'POL7100C'.

BI0710*----------------------------------------------------------------*
BI0710 01  FILLER                      PIC  X(050)         VALUE
BI0710     '* AREA DE COMUNICACAO COM GFCT5307 - SAIDA *'.
BI0710*----------------------------------------------------------------*
BI0710
BI0710 COPY 'GFCTWAN7'.
BI0710
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#GFCTJM'.

       COPY 'I#GFCTRK'.

       COPY 'I#GFCT0M'.

       COPY 'I#SCCE91'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
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
            INCLUDE GFCTB0C6
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0C7
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0C9
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0D0
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0D1
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0D2
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0D3
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0I0
           END-EXEC.

           EXEC SQL
            INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
            INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
            INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB029
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0M4
           END-EXEC.

           EXEC SQL
            INCLUDE GFCTB0N7
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSOR *'.
      *----------------------------------------------------------------*

           EXEC SQL
            DECLARE
                     CSR01-GFCTB029 CURSOR FOR
             SELECT
                     DISTINCT(CTARIF_COMPO_PCOTE)
               FROM
                     DB2PRD.COMPS_TARIF_PCOTE
              WHERE
                     CTARIF_PCOTE       = :GFCTB029.CTARIF-PCOTE
           ORDER BY
                     CTARIF_COMPO_PCOTE
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWANB'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTWANB-REG.
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
      * ROTINA PRINCIPAL.                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           IF  WANB-AMBIENTE           EQUAL 'B' OR 'b'
               CALL 'POOL1050'
           END-IF.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR-REGISTROS.

           PERFORM 9999-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA INICIALIZACAO DO PROGRAMA.                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WANB-SAIDA.

           INITIALIZE                  WANB-SAIDA.

           PERFORM 1100-CONSISTIR-DADOS.

           PERFORM 1200-OBTER-DATA-PROCESSAMENTO.

           PERFORM 1300-CONSISTE-PACOTE.

           PERFORM 1400-VALIDAR-CONTA.

           PERFORM 1500-CONSISTE-AGENCIA-CONTA.

           IF WANB-FASE EQUAL '2' OR '3'
              PERFORM 1600-VALIDAR-OPERADORA
           END-IF

           MOVE WANB-NUMERO TO WRK-NUM-CEL-11.
           MOVE '+'         TO WRK-NUM-CEL-S
                               WRK-TAR-CEL-S
                               WRK-OPE-CEL-S
                               WRK-DDD-CEL-S.

           IF WRK-DT-PROC-A1 NOT LESS 20120729
              IF WANB-FASE EQUAL '2' OR '3'
                 PERFORM 3105-VERIFICA-PERMISSAO-ADESAO
              END-IF
              PERFORM 1700-VALIDAR-CELULAR
           ELSE
              IF WRK-NUM-CEL-9     EQUAL ZEROS
                 PERFORM 1700-VALIDAR-CELULAR
              ELSE
                 MOVE 82                 TO WANB-COD-RETORNO
                 MOVE '1858'             TO GFCTG2-COD-MSG
                 MOVE 1858                TO WANB-COD-MENSAGEM
JUN10            PERFORM 9800-OBTER-DESC-MSG-S-FIN
                 MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012           MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
                 MOVE 4                  TO RETURN-CODE
                 PERFORM 9999-FINALIZAR
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DE ENTRADA.                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF  WANB-AMBIENTE           EQUAL 'o'
               MOVE 'O'                TO WANB-AMBIENTE
           END-IF.

           IF  WANB-AMBIENTE           EQUAL 'b'
               MOVE 'B'                TO WANB-AMBIENTE
           END-IF.

           IF WANB-FASE EQUAL '1' OR '2' OR '3'
              NEXT SENTENCE
           ELSE
              MOVE '1' TO WANB-FASE
           END-IF.

           IF (WANB-AGENCIA            NOT NUMERIC)                OR
              (WANB-AGENCIA            EQUAL ZEROS)                OR
              (WANB-CONTA              NOT NUMERIC)                OR
              (WANB-CONTA              EQUAL ZEROS)                OR
              (WANB-PACOTE             NOT NUMERIC)                OR
              (WANB-PACOTE             EQUAL ZEROS)                OR
              (WANB-DDD                NOT NUMERIC)                OR
              (WANB-NUMERO             NOT NUMERIC)                OR
              (WANB-OPERADORA          NOT NUMERIC)                OR
              (WANB-AMBIENTE           NOT EQUAL 'O' AND 'B')
               MOVE 'B'                TO WANB-AMBIENTE

               MOVE 01                 TO WANB-COD-RETORNO
      *
      *--- DADOS ENVIADOS AO MODULO GFCT6150 INCONSISTENTES
      *
               MOVE '1501'             TO GFCTG2-COD-MSG
               MOVE 1501               TO WANB-COD-MENSAGEM

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           PERFORM 1110-CONSISTIR-DADOS-CELULAR.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA CONSISTENCIA DOS DADOS DO CELULAR                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-CONSISTIR-DADOS-CELULAR    SECTION.
      *----------------------------------------------------------------*

           IF  WANB-DDD                EQUAL ZEROS AND
               WANB-NUMERO             EQUAL ZEROS AND
               WANB-OPERADORA          EQUAL ZEROS
               MOVE 59                 TO WANB-COD-RETORNO
      *
      *--- PACOTE CESTA CELULAR. NUMERO CELULAR OBRIGATORIO.
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1802'         TO GFCTG2-COD-MSG
                   MOVE 1802           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1596'         TO GFCTG2-COD-MSG
                   MOVE 1596           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO WANB-MENSAGEM
               MOVE SPACES              TO WANB-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WANB-DDD                EQUAL ZEROS
               MOVE 51                 TO WANB-COD-RETORNO
      *
      *--- DADOS ENVIADOS NO CODIGO DDD INVALIDOS
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1714'         TO GFCTG2-COD-MSG
                   MOVE 1714           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1504'         TO GFCTG2-COD-MSG
                   MOVE 1504           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO WANB-MENSAGEM
               MOVE SPACES              TO WANB-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  WANB-NUMERO             EQUAL ZEROS
               MOVE 52                  TO WANB-COD-RETORNO
      *
      *--- DADOS ENVIADOS NO NUMERO CELULAR INVALIDOS
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1715'         TO GFCTG2-COD-MSG
                   MOVE 1715           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1505'         TO GFCTG2-COD-MSG
                   MOVE 1505           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO WANB-MENSAGEM
               MOVE SPACES              TO WANB-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.


______*____IF__WANB-OPERADORA______*___EQUAL_ZEROS
______*______*_MOVE_53______*______*___TO_WANB-COD-RETORNO
______*
______*---_DADOS_ENVIADOS_NO_CODIGO_DA_OPERADORA_INVALIDOS
______*
______*______*_IF__WANB-AMBIENTE______*EQUAL_'O'
______*______*_____MOVE_'1716'______*__TO_GFCTG2-COD-MSG
______*______*_____MOVE_1716______*____TO_WANB-COD-MENSAGEM
______*______*_ELSE
______*______*_____MOVE_'1506'______*__TO_GFCTG2-COD-MSG
______*______*_____MOVE_1506______*____TO_WANB-COD-MENSAGEM
______*______*_END-IF

______*______*_PERFORM_9900-OBTER-DESC-MSG

______*______*_MOVE_GFCTG3-DESC-MSG_____TO_WANB-MENSAGEM
______*______*_MOVE_SPACES______*______*TO_WANB-SQLCA
______*______*_MOVE_12______*______*____TO_RETURN-CODE

______*______*_PERFORM_9999-FINALIZAR
______*____END-IF.

           IF  (WANB-FASE EQUAL '1' AND
                WANB-OPERADORA  NOT NUMERIC) OR
               (WANB-FASE NOT EQUAL '1' AND
                (WANB-OPERADORA  NOT NUMERIC OR
                 WANB-OPERADORA EQUAL ZEROS))
               MOVE 53                 TO WANB-COD-RETORNO

      *--- DADOS ENVIADOS NO CODIGO DA OPERADORA INVALIDOS

               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1716'         TO GFCTG2-COD-MSG
                   MOVE 1716           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1506'         TO GFCTG2-COD-MSG
                   MOVE 1506           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO WANB-MENSAGEM
               MOVE SPACES              TO WANB-SQLCA
               MOVE 12                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA OBTER A DATA DE PROCESSAMENTO.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-OBTER-DATA-PROCESSAMENTO   SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
             SELECT
                     DPROCM_ATUAL
               INTO
                    :GFCTB0A1.DPROCM-ATUAL
               FROM
                     DB2PRD.PARM_DATA_PROCM
              WHERE
                     CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 06                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A1
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1717'         TO GFCTG2-COD-MSG
                   MOVE 1717           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1507'         TO GFCTG2-COD-MSG
                   MOVE 1507           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           MOVE DPROCM-ATUAL OF GFCTB0A1(1:2) TO WRK-DIA-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(4:2) TO WRK-MES-PROC-A1.
           MOVE DPROCM-ATUAL OF GFCTB0A1(7:4) TO WRK-ANO-PROC-A1.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR O PACOTE                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTE-PACOTE            SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-PACOTE            TO CSERVC-TARIF      OF GFCTB0C6.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C6
                                          DFIM-PRMSS-PCOTE  OF GFCTB0C6.

BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0C6.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_ADSAO_PCOTE
              WHERE
                     CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                AND  DINIC_PRMSS_PCOTE  <= :GFCTB0C6.DINIC-PRMSS-PCOTE
                AND  DFIM_PRMSS_PCOTE  >= :GFCTB0C6.DFIM-PRMSS-PCOTE
BI0412          AND (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
BI0412               CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 36                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C6
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1739'         TO GFCTG2-COD-MSG
                   MOVE 1739           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1530'         TO GFCTG2-COD-MSG
                   MOVE 1530           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100 OR -305
               MOVE 37                 TO WANB-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA GFCTB0C6
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1740'         TO GFCTG2-COD-MSG
                   MOVE 1740           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1531'         TO GFCTG2-COD-MSG
                   MOVE 1531           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

BI0511     PERFORM 1310-ACESSAR-GFCTB0A2.

           IF  WRK-CESTA-CELULAR       EQUAL 'N'
               MOVE 54                 TO WANB-COD-RETORNO
      *
      *--- TARIFA PACOTE NAO EH CESTA CELULAR
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1795'         TO GFCTG2-COD-MSG
                   MOVE 1795           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1545'         TO GFCTG2-COD-MSG
                   MOVE 1545           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG     TO WANB-MENSAGEM
               MOVE SPACES              TO WANB-SQLCA
               MOVE 04                  TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI0511******************************************************************
BI0511*    ROTINA ACESSAR GFCTB0A2                                     *
BI0511******************************************************************
BI0511*----------------------------------------------------------------*
BI0511 1310-ACESSAR-GFCTB0A2           SECTION.
BI0511*----------------------------------------------------------------*
BI0511
BI0511     MOVE WANB-PACOTE    TO CSERVC-TARIF        OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL   OF GFCTB0A1
BI0511                         TO DINIC-VGCIA-TARIF   OF GFCTB0A2.
BI0511
BI0511     MOVE DPROCM-ATUAL   OF GFCTB0A1
BI0511                         TO DFIM-VGCIA-TARIF    OF GFCTB0A2.
BI0511
BI0511     EXEC SQL
BI0511          SELECT
BI0511               CINDCD_CREDT_CLULR
BI0511          INTO
BI0511              :GFCTB0A2.CINDCD-CREDT-CLULR
BI0511          FROM   DB2PRD.PARM_SERVC_TARIF
BI0511          WHERE  CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
BI0511            AND  DINIC_VGCIA_TARIF <= :GFCTB0A2.DINIC-VGCIA-TARIF
BI0511            AND  DFIM_VGCIA_TARIF  >= :GFCTB0A2.DFIM-VGCIA-TARIF
BI0511     END-EXEC.
BI0511
BI0511     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BI0511        (SQLWARN0                EQUAL 'W')
BI0511         MOVE 36                 TO WANB-COD-RETORNO
BI0511*
BI0511*--- ERRO NA TABELA GFCTB0A2
BI0511*
BI0511         IF  WANB-AMBIENTE       EQUAL 'O'
BI0511             MOVE '1722'         TO GFCTG2-COD-MSG
BI0511             MOVE 1722           TO WANB-COD-MENSAGEM
BI0511         ELSE
BI0511             MOVE '1530'         TO GFCTG2-COD-MSG
BI0511             MOVE 1530           TO WANB-COD-MENSAGEM
BI0511         END-IF
BI0511
BI0511         PERFORM 9900-OBTER-DESC-MSG
BI0511
BI0511         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
BI0511         MOVE 12                 TO RETURN-CODE
BI0511
BI0511         PERFORM 9999-FINALIZAR
BI0511     END-IF.
BI0511
BI0511     IF  CINDCD-CREDT-CLULR OF GFCTB0A2   EQUAL   'S'
BI0511         MOVE 'S'                TO WRK-CESTA-CELULAR
BI0511     ELSE
BI0511         MOVE 'N'                TO WRK-CESTA-CELULAR
BI0511     END-IF.
BI0511
BI0511*----------------------------------------------------------------*
BI0511 1310-99-FIM.                    EXIT.
BI0511*----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR A CONTA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-VALIDAR-CONTA              SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO LKW-CJUNC-DEPC.
           MOVE WANB-CONTA             TO LKW-CTA-CORR.

           MOVE 'SCCE9100'             TO WRK-MODULO.

           CALL WRK-MODULO             USING LKW-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 22                 TO WANB-COD-RETORNO
      *
      *--- MENSAGEM INTERNA NO PROGRAMA
      *
               MOVE 1392               TO WANB-COD-MENSAGEM

               MOVE WRK-MSG-SCCE9100   TO WANB-MENSAGEM

               MOVE SPACES             TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          EQUAL 99
               MOVE 22                 TO WANB-COD-RETORNO
      *
      *--- MENSAGEM INTERNA NO PROGRAMA
      *
               MOVE 1392               TO WANB-COD-MENSAGEM

               MOVE WRK-MSG-SCCE9100   TO WANB-MENSAGEM

               MOVE SPACES             TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  LKW-CD-RETORNO          NOT EQUAL ZEROS
               MOVE 03                 TO WANB-COD-RETORNO
      *
      *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
      *
               MOVE 1306               TO WANB-COD-MENSAGEM

               MOVE LKW-MENSAGEM       TO WANB-MENSAGEM

               MOVE SPACES             TO WANB-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA VALIDAR AGENCIA/CONTA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1500-CONSISTE-AGENCIA-CONTA     SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC OF CLIEV007.
           MOVE WANB-CONTA             TO CCTA-CLI    OF CLIEV007.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV007.CID-CLI
               FROM
                     DB2PRD.V01CTA_POUPANCA
              WHERE
                     CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 11                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV007
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1718'         TO GFCTG2-COD-MSG
                   MOVE 1718           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1508'         TO GFCTG2-COD-MSG
                   MOVE 1508           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 1510-ACESSAR-CLIEV008
           ELSE
               MOVE 2                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI OF CLIEV007
                                       TO CID-CLI OF CLIEV004

               PERFORM 1520-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV008                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       1510-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC OF CLIEV008.
           MOVE WANB-CONTA             TO CCTA-CLI    OF CLIEV008.

           EXEC SQL
             SELECT
                     CID_CLI
               INTO
                    :CLIEV008.CID-CLI
               FROM
                     DB2PRD.V01CTA_CORRENTE
              WHERE
                     CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
                AND  CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 12                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV008
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1719'         TO GFCTG2-COD-MSG
                   MOVE 1719           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1509'         TO GFCTG2-COD-MSG
                   MOVE 1509           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 19                 TO WANB-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA CLIEV008
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1724'         TO GFCTG2-COD-MSG
                   MOVE 1724           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1514'         TO GFCTG2-COD-MSG
                   MOVE 1514           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           ELSE
               MOVE 1                  TO WRK-TIPO-CONTA-CLIENTE
               MOVE CID-CLI OF CLIEV008
                                       TO CID-CLI OF CLIEV004

               PERFORM 1520-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR CLIEV004 PARA OBTER DADOS DO CLIENTE                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1520-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     CCGC_CPF,
                     CFLIAL_CGC,
                     CCTRL_CPF_CGC,
                     CTPO_PSSOA,
                     IPSSOA_COPLT
               INTO
                    :CLIEV004.CCGC-CPF,
                    :CLIEV004.CFLIAL-CGC,
                    :CLIEV004.CCTRL-CPF-CGC,
                    :CLIEV004.CTPO-PSSOA,
                    :CLIEV004.IPSSOA-COPLT
               FROM
                     DB2PRD.V01CLIENTE_AGENCIA
              WHERE
                     CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 13                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA CLIEV004
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1720'         TO GFCTG2-COD-MSG
                   MOVE 1720           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1510'         TO GFCTG2-COD-MSG
                   MOVE 1510           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 20                 TO WANB-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO NA TABELA CLIEV004
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1725'         TO GFCTG2-COD-MSG
                   MOVE 1725           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1515'         TO GFCTG2-COD-MSG
                   MOVE 1515           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           ELSE
ST25X6                IF  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                        EQUAL ZEROS
ST25X6                OR  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                        EQUAL SPACES
ST25X6                OR  CFLIAL-CGC-ST OF GFCTB0I0
ST25X6                                        EQUAL LOW-VALUES
ST25X6                    MOVE 'F'            TO WRK-TIPO-PESSOA-CLIENTE
ST25X6                ELSE
ST25X6                    MOVE 'J'            TO WRK-TIPO-PESSOA-CLIENTE
ST25X6                END-IF

               PERFORM 1521-OBTER-DADOS-GFCTB0D8
           END-IF.

      *----------------------------------------------------------------*
       1520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA TARIFA.                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       1521-OBTER-DADOS-GFCTB0D8       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
             SELECT
                     CPSSOA_SERVC_TARIF,
                     CTPO_CTA_DEB_TARIF
               INTO
                    :GFCTB0D8.CPSSOA-SERVC-TARIF,
                    :GFCTB0D8.CTPO-CTA-DEB-TARIF
               FROM
                     DB2PRD.SERVC_TARIF_PRINC
              WHERE
                     CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 14                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D8
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1721'         TO GFCTG2-COD-MSG
                   MOVE 1721           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1511'         TO GFCTG2-COD-MSG
                   MOVE 1511           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR CODIGO DA OPERADORA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       1600-VALIDAR-OPERADORA          SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-OPERADORA         TO COPER-FONE-MOVEL OF GFCTB0M4.
           MOVE WANB-DDD               TO CDDD-FONE-MOVEL  OF GFCTB0M4.

           EXEC SQL
             SELECT
                     ROPER_FONE_MOVEL
               INTO
                    :GFCTB0M4.ROPER-FONE-MOVEL
               FROM
                     DB2PRD.TOPER_FONE_CREDT
              WHERE
                     COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
                AND  CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 58                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M4
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1797'         TO GFCTG2-COD-MSG
                   MOVE 1797           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1547'         TO GFCTG2-COD-MSG
                   MOVE 1547           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 56                 TO WANB-COD-RETORNO
      *
      *--- DDD NAO CONDIZENTE COM O CODIGO DA OPERADORA
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1798'         TO GFCTG2-COD-MSG
                   MOVE 1798           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1548'         TO GFCTG2-COD-MSG
                   MOVE 1548           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR NUMERO DA CELULAR                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       1700-VALIDAR-CELULAR            SECTION.
      *----------------------------------------------------------------*

BI0710     INITIALIZE                     WAN7-ENTRADA
BI0710                                    WAN7-SAIDA
BI0710                                    WAN7-DADOS-ERRO.
BI0710
BI0710     MOVE 'O'                    TO WAN7-AMBIENTE.
BI0710     MOVE WANB-DDD               TO WAN7-CD-DDD.
BI0710     MOVE WANB-NUMERO            TO WAN7-NR-TELEFONE.
BI0710
BI0710     MOVE 'GFCT5307'         TO     WRK-MODULO.
BI0710
BI0710     CALL WRK-MODULO         USING  GFCTWAN7-REG.
BI0710
BI0710     IF WAN7-COD-RETORNO  NOT EQUAL  ZEROS
BI0710         PERFORM 9950-OBTEM-MSG-GFCT5307
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR DADOS OBTIDOS NO CURSOR 1 DA TABELA GFCTB0C6.        *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR-REGISTROS        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-TARIFA-VALIDA.

           IF  CTPO-CTA-DEB-TARIF OF GFCTB0D8
                                       EQUAL WRK-TIPO-CONTA-CLIENTE
               IF  CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '3' OR
                  (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '1' AND
                   WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'F') OR
                  (CPSSOA-SERVC-TARIF OF GFCTB0D8
                                       EQUAL '2' AND
                   WRK-TIPO-PESSOA-CLIENTE
                                       EQUAL 'J')
                   PERFORM 2700-CALCULAR-DATA-FIM

                   MOVE 014            TO WRK-PROX-AGRUPAMENTO

                   MOVE 'N'            TO WRK-FIM-AGRUPAMENTO

                   MOVE 'N'            TO WRK-AGRUPAMENTO-VALIDO

                   PERFORM 2100-IDENTIFICAR-PERMISSAO
                     UNTIL WRK-FIM-AGRUPAMENTO
                                       EQUAL 'S'
                        OR WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'S'

                   IF  WRK-AGRUPAMENTO-VALIDO         EQUAL 'S'
                       MOVE 'S'        TO WRK-TARIFA-VALIDA
                   END-IF
               END-IF
           END-IF.

           IF  WRK-TARIFA-VALIDA       EQUAL 'S'
               PERFORM 2400-VER-TARIFA-PACOTE-CAD

               PERFORM 2500-VER-TARIFA-COMP-CAD

               PERFORM 2010-VER-FASE-INS-B09

______*______*_IF_WANB-FASE_EQUAL_'3'
______*______*____PERFORM_2800-INSERIR-GFCTB009
______*______*_ELSE
______*______*____PERFORM_9955-FINALIZAR-FASE-OK
______*______*_END-IF

               IF  WANB-COD-RETORNO    EQUAL ZEROS

                   PERFORM 2015-VER-FASE-INS-BM2

______*______*_____IF_WANB-FASE_EQUAL_'3'
______*______*______*_PERFORM_3000-INSERIR-GFCTB0M2
______*______*_____END-IF

                   IF  WANB-COD-RETORNO
                                       EQUAL ZEROS
                       MOVE 35         TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TARIFA PACOTE DELULAR ADERIDA COM SUCESSO
      *
                       IF  WANB-AMBIENTE
                                       EQUAL 'O'
                           MOVE '1749' TO GFCTG2-COD-MSG
                           MOVE 1749   TO WANB-COD-MENSAGEM
                       ELSE
                           MOVE '1529' TO GFCTG2-COD-MSG
                           MOVE 1529   TO WANB-COD-MENSAGEM
                       END-IF

JUN10                  PERFORM 9800-OBTER-DESC-MSG-S-FIN
JUN10
JUN10                  IF  WANB-COD-RETORNO EQUAL 35
                           MOVE GFCTG3-DESC-MSG
                                       TO WANB-MENSAGEM
JUN10                      MOVE SPACES TO WANB-SQLCA

                           MOVE ZEROS  TO RETURN-CODE

JUN10                  ELSE

                           PERFORM 2020-VER-FASE-DEL-B09M2
______*______*______*______IF_WANB-FASE_EQUAL_'3'
JUN10_*______*______*______*__PERFORM_2950-EXCLUIR-GFCTB0M2
JUN10_*______*______*______*__PERFORM_2900-EXCLUIR-GFCTB009
______*______*______*______END-IF
JUN10                  END-IF
                   ELSE
                       PERFORM 2025-VER-FASE-DEL-B09

______*______*______*__IF_WANB-FASE_EQUAL_'3'
______*______*______*_____PERFORM_2900-EXCLUIR-GFCTB009
______*______*______*__END-IF
                   END-IF
               END-IF
           ELSE
               MOVE 41                TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - PACOTE NAO EH VALIDO PARA ADESAO
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1743'         TO GFCTG2-COD-MSG
                   MOVE 1743           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1534'         TO GFCTG2-COD-MSG
                   MOVE 1534           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *_2010-VER-FASE-INS-B09
      ******************************************************************
      *----------------------------------------------------------------*
       2010-VER-FASE-INS-B09      SECTION.
      *----------------------------------------------------------------*

           IF WANB-FASE EQUAL '3'
              PERFORM 2800-INSERIR-GFCTB009
           ELSE
              PERFORM 9955-FINALIZAR-FASE-OK
           END-IF.

      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *_2015-VER-FASE-INS-BM2
      ******************************************************************
      *----------------------------------------------------------------*
       2015-VER-FASE-INS-BM2      SECTION.
      *----------------------------------------------------------------*

           IF WANB-FASE EQUAL '3'
              PERFORM 3000-INSERIR-GFCTB0M2
           END-IF.

      *----------------------------------------------------------------*
       2015-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *_2020-VER-FASE-DEL-B09M2
      ******************************************************************
      *----------------------------------------------------------------*
       2020-VER-FASE-DEL-B09M2      SECTION.
      *----------------------------------------------------------------*

           IF WANB-FASE EQUAL '3'
              PERFORM 2950-EXCLUIR-GFCTB0M2
              PERFORM 2900-EXCLUIR-GFCTB009
           END-IF.

      *----------------------------------------------------------------*
       2020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *_2025-VER-FASE-DEL-B09
      ******************************************************************
      *----------------------------------------------------------------*
       2025-VER-FASE-DEL-B09      SECTION.
      *----------------------------------------------------------------*

           IF WANB-FASE EQUAL '3'
              PERFORM 2900-EXCLUIR-GFCTB009
           END-IF.

      *----------------------------------------------------------------*
       2025-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      ******************************************************************
      * IDENTIFICAR AGRUPAMENTO COM PERMISSAO DE ADESAO AO PACOTE.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-IDENTIFICAR-PERMISSAO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-EXISTE-AGRUPAMENTO.

           EVALUATE WRK-PROX-AGRUPAMENTO

               WHEN 004

                    IF  WRK-5527-CCGC-CPF
                                       NOT EQUAL ZEROS
                        PERFORM 2110-ACESSAR-GFCTB0I0

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 018       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 009

                    IF  WRK-1-VEZ-SEGMENTO
                                       EQUAL  'N'
                        PERFORM 2120-OBTER-SEG-GESTAO

                        MOVE 'S'       TO  WRK-1-VEZ-SEGMENTO
                    END-IF

                    IF  GFCTJM-CSGMTO-GSTAO-TARIF
                                       NOT EQUAL ZEROS
                        PERFORM 2130-ACESSAR-GFCTB0D2

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 'S'       TO WRK-FIM-AGRUPAMENTO
                    END-IF

               WHEN 014

                    PERFORM 2140-ACESSAR-GFCTB0C7

                    IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                        PERFORM 2300-OBTER-DADOS-GFCTB0C6
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 015       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 015

                    IF  WRK-1-VEZ-CNPJ-CPF
                                       EQUAL 'N'
                        PERFORM 2150-OBTER-CNPJ-CPF-POSTO

                        MOVE 'S'       TO WRK-1-VEZ-CNPJ-CPF
                    END-IF

                    IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                        PERFORM 2160-ACESSAR-GFCTB0D0

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 017       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 017

                    IF  WRK-5527-CPOSTO-SERVC
                                       NOT EQUAL ZEROS
                        PERFORM 2170-ACESSAR-GFCTB0D1

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
JUN10                   MOVE 020       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 018

                    IF  WRK-1-VEZ-UF-MUNIC
                                       EQUAL  'N'
                        PERFORM 2180-OBTER-UF-MUNICIPIO

                        MOVE 'S'       TO WRK-1-VEZ-UF-MUNIC
                    END-IF

                    IF  GFCTRK-CMUN-RURAL
                                       NOT EQUAL ZEROS
                        PERFORM 2190-ACESSAR-GFCTB0C9

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 019       TO WRK-PROX-AGRUPAMENTO
                    END-IF

               WHEN 019

                    IF  GFCTRK-CSGL-UF NOT EQUAL SPACES
                        PERFORM 2200-ACESSAR-GFCTB0D3

                        IF  WRK-EXISTE-AGRUPAMENTO
                                       EQUAL 'S'
                            PERFORM 2300-OBTER-DADOS-GFCTB0C6
                        END-IF
                    END-IF

                    IF  WRK-AGRUPAMENTO-VALIDO
                                       EQUAL 'N'
                        MOVE 009       TO WRK-PROX-AGRUPAMENTO
                    END-IF

JUN10          WHEN 020
JUN10
JUN10               IF  WRK-5527-CPOSTO-SERVC
JUN10                                  NOT EQUAL ZEROS
JUN10                   PERFORM 2170-ACESSAR-GFCTB0D1
JUN10
JUN10                   IF  WRK-EXISTE-AGRUPAMENTO
JUN10                                  EQUAL 'S'
JUN10                       PERFORM 2300-OBTER-DADOS-GFCTB0C6
JUN10                   END-IF
JUN10               END-IF
JUN10
JUN10               IF  WRK-AGRUPAMENTO-VALIDO
JUN10                                  EQUAL 'N'
JUN10                   MOVE 004       TO WRK-PROX-AGRUPAMENTO
JUN10               END-IF
JUN10
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO CLIENTE.*
      ******************************************************************
      *----------------------------------------------------------------*
       2110-ACESSAR-GFCTB0I0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0I0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0I0.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0I0.
ST2506     MOVE WRK-5527-CCGC-CPF      TO CCGC-CPF-ST       OF GFCTB0I0.
ST2506     MOVE WRK-5527-CFLIAL-CGC    TO CFLIAL-CGC-ST     OF GFCTB0I0.
ST2506     MOVE WRK-5527-CCTRL-CPF-CGC TO CCTRL-CPF-CGC-ST  OF GFCTB0I0.

           PERFORM 2111-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0I0.CSEQ-AGPTO-CTA,
                    :GFCTB0I0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.TPRMSS_GRP_CLI
              WHERE
                     CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                AND  CCGC_CPF           = :GFCTB0I0.CCGC-CPF
                AND  CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
                AND  CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 21                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0I0
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1726'         TO GFCTG2-COD-MSG
                   MOVE 1726           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1516'         TO GFCTG2-COD-MSG
                   MOVE 1516           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0I0
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0I0
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2111-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0I0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.TPRMSS_GRP_CLI
              WHERE
                     CSERVC_TARIF       = :GFCTB0I0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0I0.CAGPTO-CTA
                AND  CCGC_CPF           = :GFCTB0I0.CCGC-CPF
                AND  CFLIAL_CGC         = :GFCTB0I0.CFLIAL-CGC
                AND  CCTRL_CPF_CGC      = :GFCTB0I0.CCTRL-CPF-CGC
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0I0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 21                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0I0
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1726'         TO GFCTG2-COD-MSG
                   MOVE 1726           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1516'         TO GFCTG2-COD-MSG
                   MOVE 1516           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
                MOVE '01.01.0001'      TO DINIC-PRMSS-PCOTE OF GFCTB0I0
           END-IF.

      *----------------------------------------------------------------*
       2111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5534 PARA OBTER SEGMENTO GESTAO.            *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-OBTER-SEG-GESTAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTJM-AREA.

           INITIALIZE                  GFCTJM-AREA.

           MOVE WANB-AMBIENTE          TO GFCTJM-TPO-AMBIENTE.
           MOVE WANB-AGENCIA           TO GFCTJM-CJUNC-DEPDC.
           MOVE WANB-CONTA             TO GFCTJM-CCTA-CLI.

           MOVE 'GFCT5534'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTJM-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 23                 TO WANB-COD-RETORNO
      *
      *--- ERRO AO ACESSAR O MODULO GFCT5534
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1728'         TO GFCTG2-COD-MSG
                   MOVE 1728           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1518'         TO GFCTG2-COD-MSG
                   MOVE 1518           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      EQUAL 99
               MOVE 24                 TO WANB-COD-RETORNO
      *
      *--- REGISTRO NAO ENCONTRADO GFCT5534
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1729'         TO GFCTG2-COD-MSG
                   MOVE 1729           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1519'         TO GFCTG2-COD-MSG
                   MOVE 1519           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTJM-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTJM-CSGMTO-GSTAO-TARIF
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO SEGMENTO*
      ******************************************************************
      *----------------------------------------------------------------*
       2130-ACESSAR-GFCTB0D2           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D2.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D2.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D2.
           MOVE GFCTJM-CSGMTO-GSTAO-TARIF
                                       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0D2.

           PERFORM 2131-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D2.CSEQ-AGPTO-CTA,
                    :GFCTB0D2.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_SGMTO
              WHERE
                     CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                AND  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 25                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D2
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1730'         TO GFCTG2-COD-MSG
                   MOVE 1730           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1520'         TO GFCTG2-COD-MSG
                   MOVE 1520           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D2
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D2
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2131-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D2.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_SGMTO
              WHERE
                     CSERVC_TARIF       = :GFCTB0D2.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D2.CAGPTO-CTA
                AND  CSGMTO_GSTAO_TARIF = :GFCTB0D2.CSGMTO-GSTAO-TARIF
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D2.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 25                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D2
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1730'         TO GFCTG2-COD-MSG
                   MOVE 1730           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1520'         TO GFCTG2-COD-MSG
                   MOVE 1520           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D2
           END-IF.

      *----------------------------------------------------------------*
       2131-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO AGENCIA.*
      ******************************************************************
      *----------------------------------------------------------------*
       2140-ACESSAR-GFCTB0C7           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C7.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C7.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C7.
           MOVE WANB-AGENCIA           TO CDEPDC            OF GFCTB0C7.

           PERFORM 2141-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0C7.CSEQ-AGPTO-CTA,
                    :GFCTB0C7.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_DEPDC
              WHERE
                     CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0C7.CDEPDC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 26                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C7
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1731'         TO GFCTG2-COD-MSG
                   MOVE 1731           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1521'         TO GFCTG2-COD-MSG
                   MOVE 1521           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C7
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C7
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2141-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0C7.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_DEPDC
              WHERE
                     CSERVC_TARIF       = :GFCTB0C7.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C7.CAGPTO-CTA
                AND (CDEPDC             = 0
                 OR  CDEPDC             = :GFCTB0C7.CDEPDC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0C7.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 26                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C7
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1731'         TO GFCTG2-COD-MSG
                   MOVE 1731           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1521'         TO GFCTG2-COD-MSG
                   MOVE 1521           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C7
           END-IF.

      *----------------------------------------------------------------*
       2141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5527 PARA OBTER CPF/CNPJ/POSTO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2150-OBTER-CNPJ-CPF-POSTO       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-5527-AREA-SAIDA.

           INITIALIZE                  WRK-5527-AREA-SAIDA.

           MOVE 'GFCT6151'             TO WRK-5527-CHAMADOR.
           MOVE WANB-AGENCIA           TO WRK-5527-AGENCIA-CLIENTE.
           MOVE WANB-CONTA             TO WRK-5527-CONTA-CLIENTE.
           MOVE WANB-AMBIENTE          TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5527-AREA-ENTRADA
                                             WRK-5527-AREA-SAIDA
                                             WRK-IO-PCB
                                             WRK-ALT-PCB
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 16                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO AO ACESSAR O MODULO GFCT5527
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1723'         TO GFCTG2-COD-MSG
                   MOVE 1723           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1513'         TO GFCTG2-COD-MSG
                   MOVE 1513           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (WRK-5527-COD-RETORNO    NOT EQUAL ZEROS AND 02 AND 05)
               IF  WRK-5527-COD-RETORNO
                                       EQUAL 99
                   MOVE 38             TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO NAO ENCONTRADO GFCT5527
      *
                   IF  WANB-AMBIENTE   EQUAL 'O'
                       MOVE '1727'     TO GFCTG2-COD-MSG
                       MOVE 1727       TO WANB-COD-MENSAGEM
                   ELSE
                       MOVE '1517'     TO GFCTG2-COD-MSG
                       MOVE 1517       TO WANB-COD-MENSAGEM
                   END-IF

                   PERFORM 9900-OBTER-DESC-MSG

                   MOVE GFCTG3-DESC-MSG
                                       TO WANB-MENSAGEM
                   MOVE SPACES         TO WANB-SQLCA
                   MOVE 12             TO RETURN-CODE

                   PERFORM 9999-FINALIZAR
               END-IF
           END-IF.

           IF (WRK-5527-COD-RETORNO    EQUAL 02 OR 05)
               MOVE ZEROS              TO WRK-5527-CCGC-CPF
                                          WRK-5527-CFLIAL-CGC
                                          WRK-5527-CCTRL-CPF-CGC
           END-IF.

      *----------------------------------------------------------------*
       2150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PAB.    *
      ******************************************************************
      *----------------------------------------------------------------*
       2160-ACESSAR-GFCTB0D0           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D0.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D0.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D0.
           MOVE WANB-AGENCIA           TO CDEPDC            OF GFCTB0D0.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D0.

           PERFORM 2161-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D0.CSEQ-AGPTO-CTA,
                    :GFCTB0D0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PAB
              WHERE
                     CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                AND  CDEPDC             = :GFCTB0D0.CDEPDC
                AND (CPOSTO_SERVC       = 0
                 OR  CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 27                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D0
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1732'         TO GFCTG2-COD-MSG
                   MOVE 1732           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1522'         TO GFCTG2-COD-MSG
                   MOVE 1522           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D0
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D0
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2161-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D0.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PAB
              WHERE
                     CSERVC_TARIF       = :GFCTB0D0.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D0.CAGPTO-CTA
                AND  CDEPDC             = :GFCTB0D0.CDEPDC
                AND (CPOSTO_SERVC       = 0
                 OR  CPOSTO_SERVC       = :GFCTB0D0.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D0.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 27                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D0
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1732'         TO GFCTG2-COD-MSG
                   MOVE 1732           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1522'         TO GFCTG2-COD-MSG
                   MOVE 1522           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D0
           END-IF.

      *----------------------------------------------------------------*
       2161-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO PSTAL.  *
      ******************************************************************
      *----------------------------------------------------------------*
       2170-ACESSAR-GFCTB0D1           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D1.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D1.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D1.
           MOVE WANB-AGENCIA           TO CDEPDC            OF GFCTB0D1.

           MOVE WRK-5527-CPOSTO-SERVC  TO CPOSTO-SERVC      OF GFCTB0D1.

           PERFORM 2171-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D1.CSEQ-AGPTO-CTA,
                    :GFCTB0D1.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PSTAL
              WHERE
                     CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
JUN10           AND (CDEPDC             = 0
JUN10            OR  CDEPDC             = :GFCTB0D1.CDEPDC
JUN10           AND  CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 28                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D1
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1733'         TO GFCTG2-COD-MSG
                   MOVE 1733           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1523'         TO GFCTG2-COD-MSG
                   MOVE 1523           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D1
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D1
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2171-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D1.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_PSTAL
              WHERE
                     CSERVC_TARIF       = :GFCTB0D1.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D1.CAGPTO-CTA
JUN10           AND (CDEPDC             = 0
JUN10            OR  CDEPDC             = :GFCTB0D1.CDEPDC
JUN10           AND  CPOSTO_SERVC       = :GFCTB0D1.CPOSTO-SERVC)
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D1.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 28                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D1
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1733'         TO GFCTG2-COD-MSG
                   MOVE 1733           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1523'         TO GFCTG2-COD-MSG
                   MOVE 1523           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D1
           END-IF.

      *----------------------------------------------------------------*
       2171-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR MODULO GFCT5564 PARA OBTER UF E MUNICIPIO.             *
      ******************************************************************
      *----------------------------------------------------------------*
       2180-OBTER-UF-MUNICIPIO         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTRK-AREA.

           INITIALIZE GFCTRK-AREA.

           MOVE WANB-AMBIENTE          TO GFCTRK-TPO-AMBIENTE.
           MOVE WANB-AGENCIA           TO GFCTRK-CJUNC-DEPDC.

           MOVE 'GFCT5564'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTRK-AREA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 29                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO AO ACESSAR O MODULO GFCT5564
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1734'         TO GFCTG2-COD-MSG
                   MOVE 1734           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1524'         TO GFCTG2-COD-MSG
                   MOVE 1524           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE 12                 TO RETURN-CODE
               MOVE SPACES             TO WANB-SQLCA

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (GFCTRK-COD-RETORNO      EQUAL 01 OR 02 OR 99)
               MOVE 30                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO NAO ENCONTRADO GFCT5564
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1735'         TO GFCTG2-COD-MSG
                   MOVE 1735           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1525'         TO GFCTG2-COD-MSG
                   MOVE 1525           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTRK-COD-RETORNO      NOT EQUAL ZEROS
               MOVE ZEROS              TO GFCTRK-CMUN-RURAL
               MOVE SPACES             TO GFCTRK-CSGL-UF
           END-IF.

      *----------------------------------------------------------------*
       2180-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GP MUNICIPIO. *
      ******************************************************************
      *----------------------------------------------------------------*
       2190-ACESSAR-GFCTB0C9           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0C9.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C9.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0C9.
           MOVE GFCTRK-CMUN-RURAL      TO WRK-CAMPO-TIRA-SINAL-17.
           MOVE WRK-CAMPO-TIRA-SINAL-07 TO WRK-CAMPO-POE-SINAL-07-C.
           MOVE WRK-CAMPO-POE-SINAL-07 TO CMUN-IBGE         OF GFCTB0C9.

           PERFORM 2191-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0C9.CSEQ-AGPTO-CTA,
                    :GFCTB0C9.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_MUN
              WHERE
                     CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                AND  CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 31                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1736'         TO GFCTG2-COD-MSG
                   MOVE 1736           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1526'         TO GFCTG2-COD-MSG
                   MOVE 1526           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0C9
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0C9
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2190-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2191-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0C9.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_MUN
              WHERE
                     CSERVC_TARIF       = :GFCTB0C9.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C9.CAGPTO-CTA
                AND  CMUN_IBGE          = :GFCTB0C9.CMUN-IBGE
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0C9.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 31                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1736'         TO GFCTG2-COD-MSG
                   MOVE 1736           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1526'         TO GFCTG2-COD-MSG
                   MOVE 1526           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0C9
           END-IF.

      *----------------------------------------------------------------*
       2191-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR EXISTENCIA DE INFORMACOES DE PERMISSAO GRUPO UF.     *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-ACESSAR-GFCTB0D3           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF      OF GFCTB0D3.
           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0D3.
           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO DINIC-PRMSS-PCOTE OF GFCTB0D3.
           MOVE GFCTRK-CSGL-UF         TO CSGL-UF           OF GFCTB0D3.

           PERFORM 2201-OBTER-MAX-DATA-INICIO.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE
               INTO
                    :GFCTB0D3.CSEQ-AGPTO-CTA,
                    :GFCTB0D3.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_UF
              WHERE
                     CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA
                AND  CSGL_UF            = :GFCTB0D3.CSGL-UF
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 32                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D3
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1737'         TO GFCTG2-COD-MSG
                   MOVE 1737           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1527'         TO GFCTG2-COD-MSG
                   MOVE 1527           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               PERFORM 2600-CALCULAR-DATA-INI

               IF  WRK-AAAAMM-FIM-ADSAO-INDVD
                                       GREATER
                                             WRK-AAAAMM-INI-ADSAO-INDVD
                   MOVE CSEQ-AGPTO-CTA OF GFCTB0D3
                                       TO WRK-SEQ-AGPTO
                   MOVE DINIC-PRMSS-PCOTE OF GFCTB0D3
                                       TO WRK-DINIC-PRMSS-PCOTE

                   MOVE 'S'            TO WRK-EXISTE-AGRUPAMENTO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER A MAIOR DATA DE INICIO DA PERMISSAO                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2201-OBTER-MAX-DATA-INICIO      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                     MAX(DINIC_PRMSS_PCOTE)
               INTO
                    :GFCTB0D3.DINIC-PRMSS-PCOTE
               FROM
                     DB2PRD.PRMSS_GRP_UF
              WHERE
                     CSERVC_TARIF       = :GFCTB0D3.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0D3.CAGPTO-CTA
                AND  CSGL_UF            = :GFCTB0D3.CSGL-UF
                AND  DINIC_PRMSS_PCOTE <= :GFCTB0D3.DINIC-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -305) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 32                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0D3
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1737'         TO GFCTG2-COD-MSG
                   MOVE 1737           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1527'         TO GFCTG2-COD-MSG
                   MOVE 1527           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL -305
               MOVE '01.01.0001'       TO DINIC-PRMSS-PCOTE OF GFCTB0D3
           END-IF.

      *----------------------------------------------------------------*
       2201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DA PERMISSAO DE ADESAO AO PACOTE.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-DADOS-GFCTB0C6       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-PROX-AGRUPAMENTO   TO CAGPTO-CTA        OF GFCTB0C6.
           MOVE WRK-SEQ-AGPTO          TO CSEQ-AGPTO-CTA    OF GFCTB0C6.
           MOVE WRK-DINIC-PRMSS-PCOTE  TO DINIC-PRMSS-PCOTE OF GFCTB0C6.

BI0412     MOVE 'I'                    TO WRK-CINDCD-ADSAO-INDVD.
BI0412     MOVE 'A'                    TO WRK-CINDCD-ADSAO-AMBAS.

           EXEC SQL
             SELECT
                     CSEQ_AGPTO_CTA,
                     DINIC_PRMSS_PCOTE,
                     CINDCD_AGPTO_TOT
               INTO
                    :GFCTB0C6.CSEQ-AGPTO-CTA,
                    :GFCTB0C6.DINIC-PRMSS-PCOTE,
                    :GFCTB0C6.CINDCD-AGPTO-TOT
               FROM
                     DB2PRD.PRMSS_ADSAO_PCOTE
              WHERE
                     CSERVC_TARIF       = :GFCTB0C6.CSERVC-TARIF
                AND  CAGPTO_CTA         = :GFCTB0C6.CAGPTO-CTA
                AND  CSEQ_AGPTO_CTA     = :GFCTB0C6.CSEQ-AGPTO-CTA
                AND  DINIC_PRMSS_PCOTE  = :GFCTB0C6.DINIC-PRMSS-PCOTE
BI0412          AND (CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-INDVD OR
BI0412               CINDCD_ADSAO_INDVD = :WRK-CINDCD-ADSAO-AMBAS)
                AND  CIDTFD_PRMSS_INDVD
JUN10                                  IN ( 0, 2 )
                AND  DFIM_PRMSS_PCOTE  > :GFCTB0C6.DFIM-PRMSS-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 33                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C6
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1738'         TO GFCTG2-COD-MSG
                   MOVE 1738           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1528'         TO GFCTG2-COD-MSG
                   MOVE 1528           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 'S'                TO WRK-AGRUPAMENTO-VALIDO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR ADESAO A TARIFA PACORE                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-VER-TARIFA-PACOTE-CAD      SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC      OF GFCTB009.
           MOVE WANB-CONTA             TO CCTA-CLI         OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009.

           MOVE 0                      TO CINDCD-EXCL-REG  OF GFCTB009.

           EXEC SQL
             SELECT
                     DINIC_ADSAO_INDVD,
                     DFIM_ADSAO_INDVD
               INTO
                    :GFCTB009.DINIC-ADSAO-INDVD,
                    :GFCTB009.DFIM-ADSAO-INDVD
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE
              WHERE
                     CJUNC_DEPDC        = :GFCTB009.CJUNC-DEPDC
                AND  CCTA_CLI           = :GFCTB009.CCTA-CLI
                AND  CINDCD_EXCL_REG    = :GFCTB009.CINDCD-EXCL-REG
                AND  DFIM_ADSAO_INDVD  >  :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
JUN10          MOVE 40                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO NA TABELA DB2PRD.ADSAO_INDVD_PCOTE
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 39                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO JA EXISTE NA TABELA GFCTB009
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1741'         TO GFCTG2-COD-MSG
                   MOVE 1741           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1532'         TO GFCTG2-COD-MSG
                   MOVE 1532           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VALIDAR PACOTE.                                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-VER-TARIFA-COMP-CAD        SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CURSOR
                                          WRK-EXISTE-COMPONENTE.

           PERFORM 2520-ABRIR-CURSOR.

           PERFORM 2530-LER-CURSOR.

           PERFORM 2510-PROCESSAR-CURSOR
             UNTIL WRK-FIM-CURSOR      EQUAL 'S'
                OR WRK-EXISTE-COMPONENTE
                                       EQUAL 'S'.

           PERFORM 2540-FECHAR-CURSOR.

           IF  WRK-EXISTE-COMPONENTE   EQUAL 'S'
               MOVE 50                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TAR. COMP. JA CONTEMPLADA EM OUTRA TARIFA
      *--- PACOTE
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1745'         TO GFCTG2-COD-MSG
                   MOVE 1745           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1539'         TO GFCTG2-COD-MSG
                   MOVE 1539           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
               MOVE SPACES             TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CURSOR DA TABELA GFCTB029.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC      OF GFCTB009.
           MOVE WANB-CONTA             TO CCTA-CLI         OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009.

           EXEC SQL
             SELECT
                     DISTINCT (B.CTARIF_PCOTE)
               INTO
                    :GFCTB029.CTARIF-PCOTE
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE A,
                     DB2PRD.COMPS_TARIF_PCOTE B
              WHERE
                     A.CJUNC_DEPDC      = :GFCTB009.CJUNC-DEPDC
                AND  A.CCTA_CLI         = :GFCTB009.CCTA-CLI
                AND  A.CINDCD_EXCL_REG  = :GFCTB009.CINDCD-EXCL-REG
                AND  A.DFIM_ADSAO_INDVD
                                       >= :GFCTB009.DFIM-ADSAO-INDVD
                AND  A.CSERVC_TARIF     = B.CTARIF_PCOTE
                AND  B.CTARIF_COMPO_PCOTE
                                        = :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 46                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - ERRO NO DISTINCT DAS TABELAS GFCTB029 E
      *--- GFCTB009
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1746'         TO GFCTG2-COD-MSG
                   MOVE 1746           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1537'         TO GFCTG2-COD-MSG
                   MOVE 1537           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF (SQLCODE                 EQUAL ZEROS OR -811)
               MOVE 'S'                TO WRK-EXISTE-COMPONENTE
           END-IF.

           PERFORM 2530-LER-CURSOR.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ABRIR CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-PACOTE   TO CTARIF-PCOTE     OF GFCTB029.

           EXEC SQL
               OPEN  CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 44                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1747'         TO GFCTG2-COD-MSG
                   MOVE 1747           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1535'         TO GFCTG2-COD-MSG
                   MOVE 1535           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER   CURSOR DA TABELA GFCTB029.                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-LER-CURSOR                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH
                     CSR01-GFCTB029
               INTO
                    :GFCTB029.CTARIF-COMPO-PCOTE
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 45                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1748'         TO GFCTG2-COD-MSG
                   MOVE 1748           TO  WANB-COD-MENSAGEM
               ELSE
                   MOVE '1536'         TO GFCTG2-COD-MSG
                   MOVE 1536           TO  WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FECHAR CURSOR DA TABELA GFCTB029.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-FECHAR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE  CSR01-GFCTB029
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 47                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - TABELA GFCTB029 INDISPONIVEL
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1744'         TO GFCTG2-COD-MSG
                   MOVE 1744           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1538'         TO GFCTG2-COD-MSG
                   MOVE 1538           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE INICIO DA ADESAO INDIVIDUAL                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-CALCULAR-DATA-INI          SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-PROCM-ATUAL.
           MOVE WRK-MES-DB2            TO WRK-MES-PROCM-ATUAL.
           MOVE WRK-ANO-DB2            TO WRK-ANO-PROCM-ATUAL.

           MOVE DINIC-PRMSS-PCOTE OF GFCTB0C6
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-INI-PRMSS-PCOTE.
           MOVE WRK-MES-DB2            TO WRK-MES-INI-PRMSS-PCOTE.
           MOVE WRK-ANO-DB2            TO WRK-ANO-INI-PRMSS-PCOTE.

           IF  WRK-DT-INI-PRMSS-PCOTE  GREATER WRK-DT-PROCM-ATUAL
               MOVE WRK-DT-INI-PRMSS-PCOTE
                                       TO WRK-DT-INI-ADSAO-INDVD

               IF  WRK-DIA-INI-ADSAO-INDVD
                                       NOT EQUAL 01
                   MOVE 01             TO WRK-DIA-INI-ADSAO-INDVD

                   MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES
                   ADD  1              TO WRK-MES
                   MOVE WRK-MES        TO WRK-MES-INI-ADSAO-INDVD

                   IF  WRK-MES-INI-ADSAO-INDVD
                                       GREATER 12
                       MOVE 01         TO WRK-MES-INI-ADSAO-INDVD

                       MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO
                       ADD  1          TO WRK-ANO
                       MOVE WRK-ANO    TO WRK-ANO-INI-ADSAO-INDVD
                   END-IF
               END-IF
           ELSE
               MOVE WRK-DT-PROCM-ATUAL TO WRK-DT-INI-ADSAO-INDVD
               MOVE 01                 TO WRK-DIA-INI-ADSAO-INDVD

               MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES
               ADD  1                  TO WRK-MES
               MOVE WRK-MES            TO WRK-MES-INI-ADSAO-INDVD

               IF  WRK-MES-INI-ADSAO-INDVD
                                       GREATER 12
                   MOVE 01             TO WRK-MES-INI-ADSAO-INDVD

                   MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO
                   ADD  1              TO WRK-ANO
                   MOVE WRK-ANO        TO WRK-ANO-INI-ADSAO-INDVD
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CALCULAR A DATA DE FIM DA ADESAO INDIVIDUAL.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-CALCULAR-DATA-FIM          SECTION.
      *----------------------------------------------------------------*

           PERFORM 2720-OBTER-DADOS-GFCTB0A2.

           MOVE DFIM-VGCIA-TARIF OF GFCTB0A2
                                       TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-FIM-ADSAO-INDVD.
           MOVE WRK-MES-DB2            TO WRK-MES-FIM-ADSAO-INDVD.
           MOVE WRK-ANO-DB2            TO WRK-ANO-FIM-ADSAO-INDVD.

           IF (WRK-MES-FIM-ADSAO-INDVD EQUAL 01 OR 03 OR 05 OR 07 OR
                                             08 OR 10 OR 12) AND
               WRK-DIA-FIM-ADSAO-INDVD EQUAL 31
               NEXT SENTENCE
           ELSE
               IF (WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 04 OR 06 OR 09 OR 11) AND
                   WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 30
                   NEXT SENTENCE
               ELSE
                   IF  WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 02 AND
                       WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 29
                       NEXT SENTENCE
                   ELSE
                       MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO

                       DIVIDE WRK-ANO  BY 4
JUN10                                  GIVING WRK-INTEIRO
JUN10                                  REMAINDER WRK-RESTO

                       MOVE WRK-ANO    TO WRK-ANO-FIM-ADSAO-INDVD

                       IF  WRK-MES-FIM-ADSAO-INDVD
                                       EQUAL 02 AND
                           WRK-RESTO   NOT EQUAL ZEROS AND
                           WRK-DIA-FIM-ADSAO-INDVD
                                       EQUAL 28
                           NEXT SENTENCE
                       ELSE
                           PERFORM 2710-ACHAR-DATA-FIM
                       END-IF
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACHAR A DATA DE FIM DA ADESAO INDIVIDUAL.                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ACHAR-DATA-FIM             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MES-FIM-ADSAO-INDVD
                                       TO WRK-MES.
           SUBTRACT 1                  FROM WRK-MES.
           MOVE WRK-MES                TO WRK-MES-FIM-ADSAO-INDVD.

           EVALUATE WRK-MES-FIM-ADSAO-INDVD

           WHEN 00

                MOVE 31                TO WRK-DIA-FIM-ADSAO-INDVD
                MOVE 12                TO WRK-MES-FIM-ADSAO-INDVD

                MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO
                SUBTRACT 1             FROM WRK-ANO
                MOVE WRK-ANO           TO WRK-ANO-FIM-ADSAO-INDVD

           WHEN 01
           WHEN 03
           WHEN 05
           WHEN 07
           WHEN 08
           WHEN 10
           WHEN 12

                MOVE 31                TO WRK-DIA-FIM-ADSAO-INDVD

           WHEN 04
           WHEN 06
           WHEN 09
           WHEN 11

                MOVE 30                TO WRK-DIA-FIM-ADSAO-INDVD

           WHEN 02

                IF  WRK-RESTO          EQUAL ZEROS
                    MOVE 28            TO WRK-DIA-FIM-ADSAO-INDVD
                ELSE
                    MOVE 29            TO WRK-DIA-FIM-ADSAO-INDVD
                END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * OBTER DADOS DO PARAMETRO DA TARIFA.                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-OBTER-DADOS-GFCTB0A2       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0C6
                                       TO CSERVC-TARIF OF GFCTB0A2.

           EXEC SQL
             SELECT
                     MAX(DFIM_VGCIA_TARIF)
               INTO
                    :GFCTB0A2.DFIM-VGCIA-TARIF
               FROM
                     DB2PRD.PARM_SERVC_TARIF
              WHERE
                     CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 15                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0A2
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1722'         TO GFCTG2-COD-MSG
                   MOVE 1722           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1512'         TO GFCTG2-COD-MSG
                   MOVE 1512           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-INSERIR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SET :WRK-HINCL-REG      = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC       OF GFCTB009.
           MOVE WANB-CONTA             TO CCTA-CLI          OF GFCTB009.
           MOVE WANB-PACOTE            TO CSERVC-TARIF      OF GFCTB009.

           MOVE WRK-HINCL-REG          TO HINCL-REG         OF GFCTB009.

           MOVE 000006151              TO CFUNC-MANUT-INCL  OF GFCTB009.
           MOVE ZEROS                  TO CFUNC-MANUT       OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-INI-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-INI-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-INI-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DINIC-ADSAO-INDVD OF GFCTB009.

           MOVE '01.01.0001'           TO WRK-DATA-DB2.
           MOVE WRK-DIA-FIM-ADSAO-INDVD
                                       TO WRK-DIA-DB2.
           MOVE WRK-MES-FIM-ADSAO-INDVD
                                       TO WRK-MES-DB2.
           MOVE WRK-ANO-FIM-ADSAO-INDVD
                                       TO WRK-ANO-DB2.
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD  OF GFCTB009.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG        OF GFCTB009.
           MOVE ZEROS                  TO CINDCD-EXCL-REG   OF GFCTB009.

           EXEC SQL
             INSERT
               INTO
                      DB2PRD.ADSAO_INDVD_PCOTE
                     (CJUNC_DEPDC,
                      CCTA_CLI,
                      CSERVC_TARIF,
                      HINCL_REG,
                      CFUNC_MANUT_INCL,
                      CFUNC_MANUT,
                      DINIC_ADSAO_INDVD,
                      DFIM_ADSAO_INDVD,
                      HMANUT_REG,
                      CINDCD_EXCL_REG)
             VALUES
                    (:GFCTB009.CJUNC-DEPDC,
                     :GFCTB009.CCTA-CLI,
                     :GFCTB009.CSERVC-TARIF,
                     :WRK-HINCL-REG,
                     :GFCTB009.CFUNC-MANUT-INCL,
                     :GFCTB009.CFUNC-MANUT,
                     :GFCTB009.DINIC-ADSAO-INDVD,
                     :GFCTB009.DFIM-ADSAO-INDVD,
                     :GFCTB009.HMANUT-REG,
                     :GFCTB009.CINDCD-EXCL-REG)
           END-EXEC.

JUN10      IF (SQLCODE                 NOT EQUAL ZEROS AND -803) OR
              (SQLWARN0                EQUAL 'W')
JUN10          MOVE 40                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

JUN10      IF  SQLCODE                 EQUAL -803
               MOVE 39                 TO WANB-COD-RETORNO
      *
      *--- ADESAO AO PACOTE - REGISTRO JA EXISTE NA TABELA GFCTB009
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1741'         TO GFCTG2-COD-MSG
                   MOVE 1741           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1532'         TO GFCTG2-COD-MSG
                   MOVE 1532           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * REMOVER AS INFORMACOES PARA AREA DE SAIDA.                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-EXCLUIR-GFCTB009           SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CJUNC-DEPDC      OF GFCTB009.
           MOVE WANB-CONTA             TO CCTA-CLI         OF GFCTB009.
           MOVE WANB-PACOTE            TO CSERVC-TARIF     OF GFCTB009.

           MOVE WRK-HINCL-REG          TO HINCL-REG        OF GFCTB009.

           EXEC SQL
             DELETE
               FROM
                     DB2PRD.ADSAO_INDVD_PCOTE
              WHERE
                     CJUNC_DEPDC       = :GFCTB009.CJUNC-DEPDC
                AND  CCTA_CLI          = :GFCTB009.CCTA-CLI
                AND  CSERVC_TARIF      = :GFCTB009.CSERVC-TARIF
                AND  HINCL_REG         = :GFCTB009.HINCL-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
JUN10          MOVE 40                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0C9
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1742'         TO GFCTG2-COD-MSG
                   MOVE 1742           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1533'         TO GFCTG2-COD-MSG
                   MOVE 1533           TO WANB-COD-MENSAGEM
               END-IF

               PERFORM 9900-OBTER-DESC-MSG

               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

JUN10 ******************************************************************
JUN10 * REMOVER AS INFORMACOES PARA AREA DE SAIDA.                     *
JUN10 ******************************************************************
JUN10 *----------------------------------------------------------------*
JUN10  2950-EXCLUIR-GFCTB0M2           SECTION.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10      EXEC SQL
JUN10        DELETE
JUN10          FROM
JUN10                DB2PRD.TADSAO_INDVD_CLULR
JUN10         WHERE
JUN10                CAG_BCRIA         = :GFCTB0M2.CAG-BCRIA
JUN10           AND  CCTA_CLI          = :GFCTB0M2.CCTA-CLI
JUN10           AND  CSERVC_TARIF      = :GFCTB0M2.CSERVC-TARIF
JUN10           AND  HINCL_REG         = :WRK-HINCL-REG
JUN10           AND  NSEQ_REG_CLULR    = :GFCTB0M2.NSEQ-REG-CLULR
JUN10      END-EXEC.
JUN10
JUN10      IF (SQLCODE                 NOT EQUAL ZEROS) OR
JUN10         (SQLWARN0                EQUAL 'W')
JUN10          MOVE 57                 TO WANB-COD-RETORNO
JUN10 *
JUN10 *--- ERRO NA TABELA GFCTB0M2
JUN10 *
JUN10          IF  WANB-AMBIENTE       EQUAL 'O'
JUN10              MOVE '1794'         TO GFCTG2-COD-MSG
JUN10              MOVE 1794           TO WANB-COD-MENSAGEM
JUN10          ELSE
JUN10              MOVE '1542'         TO GFCTG2-COD-MSG
JUN10              MOVE 1542           TO WANB-COD-MENSAGEM
JUN10          END-IF
JUN10
JUN10          PERFORM 9900-OBTER-DESC-MSG
JUN10
JUN10          MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
JUN10
JUN10          MOVE 12                 TO RETURN-CODE
JUN10
JUN10          PERFORM 9999-FINALIZAR
JUN10      END-IF.
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  2950-99-FIM.                    EXIT.
JUN10 *----------------------------------------------------------------*
JUN10
      ******************************************************************
      * MOVIMENTAR AS INFORMACOES PARA AREA DE SAIDA.                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-INSERIR-GFCTB0M2           SECTION.
      *----------------------------------------------------------------*

           MOVE WANB-AGENCIA           TO CAG-BCRIA        OF GFCTB0M2.
           MOVE WANB-CONTA             TO CCTA-CLI         OF GFCTB0M2.
           MOVE WANB-PACOTE            TO CSERVC-TARIF     OF GFCTB0M2.

           MOVE 1                      TO NSEQ-REG-CLULR   OF GFCTB0M2.

           MOVE WRK-HINCL-REG          TO HINCL-REG        OF GFCTB009.

           MOVE WANB-OPERADORA         TO COPER-FONE-MOVEL OF GFCTB0M2.
           MOVE WANB-DDD               TO CDDD-FONE-MOVEL  OF GFCTB0M2.
022012     MOVE WANB-NUMERO            TO NLIN-TFONI       OF GFCTB0M2.

           MOVE 000006151              TO CFUNC-INCL-SIST  OF GFCTB0M2.

           MOVE ZEROS                  TO CFUNC-MANUT      OF GFCTB0M2.

           MOVE '0001-01-01-01.01.01.000001'
                                       TO HMANUT-REG       OF GFCTB0M2.

           EXEC SQL
             INSERT
               INTO
                      DB2PRD.TADSAO_INDVD_CLULR
                     (CAG_BCRIA,
                      CCTA_CLI,
                      CSERVC_TARIF,
                      HINCL_REG,
                      NSEQ_REG_CLULR,
                      COPER_FONE_MOVEL,
                      CDDD_FONE_MOVEL,
                      CFUNC_INCL_SIST,
                      CFUNC_MANUT,
022012                HMANUT_REG,
022012                NLIN_TFONI)
             VALUES
                    (:GFCTB0M2.CAG-BCRIA,
                     :GFCTB0M2.CCTA-CLI,
                     :GFCTB0M2.CSERVC-TARIF,
                     :WRK-HINCL-REG,
                     :GFCTB0M2.NSEQ-REG-CLULR,
                     :GFCTB0M2.COPER-FONE-MOVEL,
                     :GFCTB0M2.CDDD-FONE-MOVEL,
                     :GFCTB0M2.CFUNC-INCL-SIST,
                     :GFCTB0M2.CFUNC-MANUT,
022012               :GFCTB0M2.HMANUT-REG,
022012               :GFCTB0M2.NLIN-TFONI)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 57                 TO WANB-COD-RETORNO
      *
      *--- ERRO NA TABELA GFCTB0M2
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE '1794'         TO GFCTG2-COD-MSG
                   MOVE 1794           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE '1542'         TO GFCTG2-COD-MSG
                   MOVE 1542           TO WANB-COD-MENSAGEM
               END-IF

JUN10          PERFORM 9800-OBTER-DESC-MSG-S-FIN
JUN10
JUN10          IF  WANB-COD-RETORNO    EQUAL 57
                   MOVE GFCTG3-DESC-MSG
                                       TO WANB-MENSAGEM
022012             MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA

                   MOVE 12             TO RETURN-CODE
JUN10          END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * VERIFICAR PERMISSAO DE ADESAO GFCTB0N7
      ******************************************************************
      *----------------------------------------------------------------*
       3105-VERIFICA-PERMISSAO-ADESAO        SECTION.
      *----------------------------------------------------------------*
           MOVE WANB-PACOTE          TO WRK-TAR-CEL-5
           MOVE WRK-TAR-CEL          TO CSERVC-TARIF     OF GFCTB0N7.
           MOVE WANB-OPERADORA       TO WRK-OPE-CEL-9.
           MOVE WRK-OPE-CEL          TO COPER-FONE-MOVEL OF GFCTB0N7.
           MOVE WANB-DDD             TO WRK-DDD-CEL-3.
           MOVE WRK-DDD-CEL          TO CDDD-FONE-MOVEL  OF GFCTB0N7.
           MOVE DPROCM-ATUAL                             OF GFCTB0A1 TO
                DINIC-VGCIA-REG                          OF GFCTB0N7
                DFIM-VGCIA-REG                           OF GFCTB0N7.

           EXEC SQL
             SELECT
                   QMAX_DIG_MOVEL
             INTO
                   :GFCTB0N7.QMAX-DIG-MOVEL
             FROM   DB2PRD.TOPER_PRMSS_CLULR
             WHERE CSERVC_TARIF       = :GFCTB0N7.CSERVC-TARIF
             AND   COPER_FONE_MOVEL   = :GFCTB0N7.COPER-FONE-MOVEL
             AND   CDDD_FONE_MOVEL    = :GFCTB0N7.CDDD-FONE-MOVEL
             AND   DINIC_VGCIA_REG    <= :GFCTB0N7.DINIC-VGCIA-REG
             AND   DFIM_VGCIA_REG     >= :GFCTB0N7.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE 85                 TO WANB-COD-RETORNO
               MOVE '1856'             TO GFCTG2-COD-MSG
               MOVE 1856                TO WANB-COD-MENSAGEM
JUN10          PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 12             TO RETURN-CODE
               PERFORM 9999-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 81                 TO WANB-COD-RETORNO
               MOVE '1857'             TO GFCTG2-COD-MSG
               MOVE 1857                TO WANB-COD-MENSAGEM
JUN10          PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-FINALIZAR
            END-IF.

            IF (WRK-NUM-CEL-9     EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 8) OR
               (WRK-NUM-CEL-9 NOT EQUAL ZEROS     AND
                QMAX-DIG-MOVEL OF GFCTB0N7 = 9)
                NEXT SENTENCE
            ELSE
               MOVE 82                 TO WANB-COD-RETORNO
               MOVE '1858'             TO GFCTG2-COD-MSG
               MOVE 1858                TO WANB-COD-MENSAGEM
JUN10          PERFORM 9800-OBTER-DESC-MSG-S-FIN
               MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
022012         MOVE SQLCA(1:LENGTH OF WANB-SQLCA) TO WANB-SQLCA
               MOVE 4                  TO RETURN-CODE
               PERFORM 9999-FINALIZAR
            END-IF.

      *----------------------------------------------------------------*
       3105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*



JUN10 ******************************************************************
JUN10 * ROTINA P/ ACESSO AO MODULO DE DESCRICOES DE MSGS S/ FINALIZAR  *
JUN10 ******************************************************************
JUN10 *----------------------------------------------------------------*
JUN10  9800-OBTER-DESC-MSG-S-FIN       SECTION.
JUN10 *----------------------------------------------------------------*
JUN10
JUN10      MOVE SPACES                 TO GFCTG3-SAIDA.
JUN10
JUN10      INITIALIZE GFCTG3-SAIDA.
JUN10
JUN10      MOVE +100                   TO GFCTG2-LL.
JUN10      MOVE ZEROS                  TO GFCTG2-ZZ.
JUN10      MOVE 'GFCT6151'             TO GFCTG2-TRANSACAO
JUN10                                     GFCTG2-FUNCAO.
JUN10      MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
JUN10      MOVE WANB-AMBIENTE          TO GFCTG2-TIPO-PROC.
JUN10      MOVE 'N'                    TO GFCTG2-FIM.
JUN10
JUN10      MOVE 'GFCT5523'             TO WRK-MODULO.
JUN10
JUN10      CALL WRK-MODULO             USING GFCTG2-ENTRADA
JUN10                                        GFCTG3-SAIDA
JUN10                                        GFCT0M-AREA-ERROS.
JUN10
JUN10      IF  RETURN-CODE             NOT EQUAL ZEROS
JUN10          MOVE 02                 TO WANB-COD-RETORNO
JUN10 *
JUN10 *--- NAO FOI POSSIVEL OBTER A MENSAGEM NO MODULO GFCT5523
JUN10 *
JUN10          IF  WANB-AMBIENTE       EQUAL 'O'
JUN10              MOVE 1803           TO WANB-COD-MENSAGEM
JUN10          ELSE
JUN10              MOVE 1593           TO WANB-COD-MENSAGEM
JUN10          END-IF
JUN10
JUN10          MOVE WRK-MSG-1          TO WANB-MENSAGEM
JUN10
JUN10          MOVE 12                 TO RETURN-CODE
JUN10
JUN10          MOVE SPACES             TO WANB-SQLCA
JUN10
JUN10          MOVE 04                 TO RETURN-CODE
JUN10
JUN10          GO                      TO 9800-99-FIM
JUN10      END-IF.
JUN10
JUN10      IF  GFCTG3-ERRO             EQUAL 9
JUN10          MOVE 03                 TO WANB-COD-RETORNO
JUN10 *
JUN10 *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
JUN10 *
JUN10          IF  WANB-AMBIENTE       EQUAL 'O'
JUN10              MOVE 1804           TO WANB-COD-MENSAGEM
JUN10          ELSE
JUN10              MOVE 1594           TO WANB-COD-MENSAGEM
JUN10          END-IF
JUN10
JUN10          MOVE WRK-MSG-2          TO WANB-MENSAGEM
JUN10
JUN10          MOVE SPACES             TO WANB-SQLCA
JUN10
JUN10          MOVE 12                 TO RETURN-CODE
JUN10
JUN10          GO                      TO 9800-99-FIM
JUN10      END-IF.
JUN10
JUN10      IF  GFCTG3-ERRO             EQUAL 1
JUN10          MOVE 04                 TO WANB-COD-RETORNO
JUN10 *
JUN10 *--- MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS
JUN10 *
JUN10          IF  WANB-AMBIENTE       EQUAL 'O'
JUN10              MOVE 1805           TO WANB-COD-MENSAGEM
JUN10          ELSE
JUN10              MOVE 1595           TO WANB-COD-MENSAGEM
JUN10          END-IF
JUN10
JUN10          MOVE WRK-MSG-3          TO WANB-MENSAGEM
JUN10
JUN10          MOVE SPACES             TO WANB-SQLCA
JUN10
JUN10          MOVE 12                 TO RETURN-CODE
JUN10      END-IF.
JUN10
JUN10 *----------------------------------------------------------------*
JUN10  9800-99-FIM.                    EXIT.
JUN10 *----------------------------------------------------------------*
JUN10
      ******************************************************************
      * ROTINA PARA ACESSO AO MODULO DE DESCRICOES DE MENSAGENS.       *
      ******************************************************************
      *----------------------------------------------------------------*
       9900-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE 'GFCT6151'             TO GFCTG2-TRANSACAO
                                          GFCTG2-FUNCAO.
           MOVE '1'                    TO GFCTG2-FUNC-BDSCO.
           MOVE WANB-AMBIENTE          TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 02                 TO WANB-COD-RETORNO
      *
      *--- NAO FOI POSSIVEL OBTER A MENSAGEM NO MODULO GFCT5523
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE 1803           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE 1593           TO WANB-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-1          TO WANB-MENSAGEM

               MOVE 12                 TO RETURN-CODE

               MOVE SPACES             TO WANB-SQLCA

               MOVE 04                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 03                 TO WANB-COD-RETORNO
      *
      *--- PROBLEMA NO ACESSO A BASE DE MENSAGENS
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE 1804           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE 1594           TO WANB-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-2          TO WANB-MENSAGEM

               MOVE SPACES             TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 04                 TO WANB-COD-RETORNO
      *
      *--- MENSAGEM NAO ENCONTRADA NA BASE DE MENSAGENS
      *
               IF  WANB-AMBIENTE       EQUAL 'O'
                   MOVE 1805           TO WANB-COD-MENSAGEM
               ELSE
                   MOVE 1595           TO WANB-COD-MENSAGEM
               END-IF

               MOVE WRK-MSG-3          TO WANB-MENSAGEM

               MOVE SPACES             TO WANB-SQLCA

               MOVE 12                 TO RETURN-CODE

               PERFORM 9999-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       9900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
BI0710
BI0710******************************************************************
BI0710*    ROTINA PARA TRATAR RETORNO DO MODULO GFCT5307               *
BI0710******************************************************************
BI0710*----------------------------------------------------------------*
BI0710 9950-OBTEM-MSG-GFCT5307 SECTION.
BI0710*----------------------------------------------------------------*
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  4
BI0710         MOVE 'B'                TO WANB-AMBIENTE
BI0710
BI0710         MOVE 01                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- DADOS ENVIADOS AO MODULO GFCT5307 INCONSISTENTES
BI0710*
BI0710         MOVE '1501'             TO GFCTG2-COD-MSG
BI0710         MOVE 1501               TO WANB-COD-MENSAGEM
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE SPACES             TO WANB-SQLCA
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  7
BI0710         MOVE 40                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- ADESAO AO PACOTE - ERRO NA TABELA DB2PRD.ADSAO_INDVD_PCOTE
BI0710*
BI0710         IF  WANB-AMBIENTE       EQUAL 'O'
BI0710             MOVE '1742'         TO GFCTG2-COD-MSG
BI0710             MOVE 1742           TO WANB-COD-MENSAGEM
BI0710         ELSE
BI0710             MOVE '1533'         TO GFCTG2-COD-MSG
BI0710             MOVE 1533           TO WANB-COD-MENSAGEM
BI0710         END-IF
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  9
BI0710         MOVE 06                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- ERRO NA TABELA GFCTB0A1
BI0710*
BI0710         IF  WANB-AMBIENTE       EQUAL 'O'
BI0710             MOVE '1717'         TO GFCTG2-COD-MSG
BI0710             MOVE 1717           TO WANB-COD-MENSAGEM
BI0710         ELSE
BI0710             MOVE '1507'         TO GFCTG2-COD-MSG
BI0710             MOVE 1507           TO WANB-COD-MENSAGEM
BI0710         END-IF
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  11
BI0710         MOVE 36                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- ERRO NA TABELA GFCTB0C6
BI0710*
BI0710         IF  WANB-AMBIENTE       EQUAL 'O'
BI0710             MOVE '1739'         TO GFCTG2-COD-MSG
BI0710             MOVE 1739           TO WANB-COD-MENSAGEM
BI0710         ELSE
BI0710             MOVE '1530'         TO GFCTG2-COD-MSG
BI0710             MOVE 1530           TO WANB-COD-MENSAGEM
BI0710         END-IF
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  13
BI0710         MOVE 57                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- ERRO NA TABELA GFCTB0M2
BI0710*
BI0710         IF  WANB-AMBIENTE       EQUAL 'O'
BI0710             MOVE '1794'         TO GFCTG2-COD-MSG
BI0710             MOVE 1794           TO WANB-COD-MENSAGEM
BI0710         ELSE
BI0710             MOVE '1542'         TO GFCTG2-COD-MSG
BI0710             MOVE 1542           TO WANB-COD-MENSAGEM
BI0710         END-IF
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  15
BI0710         MOVE 55                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- CELULAR INFORMADO JA ADERIDO A UMA CESTA/CLIENTE
BI0710*
BI0710         IF  WANB-AMBIENTE       EQUAL 'O'
BI0710             MOVE '1796'         TO GFCTG2-COD-MSG
BI0710             MOVE 1796           TO WANB-COD-MENSAGEM
BI0710         ELSE
BI0710             MOVE '1546'         TO GFCTG2-COD-MSG
BI0710             MOVE 1546           TO WANB-COD-MENSAGEM
BI0710         END-IF
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710         MOVE 04                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710     IF  WAN7-COD-RETORNO  EQUAL  17
BI0710         MOVE 'B'                TO WANB-AMBIENTE
BI0710
BI0710         MOVE 01                 TO WANB-COD-RETORNO
BI0710*
BI0710*--- DADOS ENVIADOS AO MODULO GFCT5307 INCONSISTENTES
BI0710*
BI0710         MOVE '1501'             TO GFCTG2-COD-MSG
BI0710         MOVE 1501               TO WANB-COD-MENSAGEM
BI0710
BI0710         PERFORM 9900-OBTER-DESC-MSG
BI0710
BI0710         MOVE GFCTG3-DESC-MSG    TO WANB-MENSAGEM
BI0710         MOVE WAN7-SQLCA         TO WANB-SQLCA
BI0710         MOVE 12                 TO RETURN-CODE
BI0710
BI0710         PERFORM 9999-FINALIZAR
BI0710     END-IF.
BI0710
BI0710*----------------------------------------------------------------*
BI0710 9950-OBTEM-MSG-GFCT5307-9-FIM.                    EXIT.
BI0710*----------------------------------------------------------------*


      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       9955-FINALIZAR-FASE-OK                  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-DIA-INI-ADSAO-INDVD TO WRK-DATA-DMAB-D.
           MOVE WRK-MES-INI-ADSAO-INDVD TO WRK-DATA-DMAB-M.
           MOVE WRK-ANO-INI-ADSAO-INDVD TO WRK-DATA-DMAB-A.
           MOVE WRK-DATA-DMAB           TO WANB-DT-INIC.

           MOVE WRK-DIA-FIM-ADSAO-INDVD TO WRK-DATA-DMAB-D.
           MOVE WRK-MES-FIM-ADSAO-INDVD TO WRK-DATA-DMAB-M.
           MOVE WRK-ANO-FIM-ADSAO-INDVD TO WRK-DATA-DMAB-A.
           MOVE WRK-DATA-DMAB           TO WANB-DT-FIM.

           IF  WANB-FASE EQUAL '1'
               MOVE '1749' TO GFCTG2-COD-MSG
               MOVE 1749   TO WANB-COD-MENSAGEM
               MOVE 'FINAL FASE 1. CONSISTENCIA OK'
                    TO WANB-MENSAGEM
           ELSE
               MOVE '1749' TO GFCTG2-COD-MSG
               MOVE 1749   TO WANB-COD-MENSAGEM
               MOVE 'FINAL FASE 2. CONSISTENCIA OK'
                    TO WANB-MENSAGEM
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9955-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PARA FINALIZACAO DO PROGRAMA.                           *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
