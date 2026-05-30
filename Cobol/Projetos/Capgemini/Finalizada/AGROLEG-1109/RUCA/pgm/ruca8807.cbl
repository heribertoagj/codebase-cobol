      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    RUCA8807.
       AUTHOR.        WAGNER NUNES.

      *================================================================*
      *                   S I S   C O N S U L T O R I A                *
      *----------------------------------------------------------------*
      *                                                                *
      *      PROGRAMADOR  : WAGNER N.           - SIS                  *
      *      ANALISTA     : WAGNER N.           - SIS                  *
      *      DATA         : 05/10/2020                                 *
      *                                                                *
      *      OBJETIVO     :                                            *
      *        EFETIVA NAS BASES DE CONTRATO AS ALTERACOES DE COR0001  *
      *        E COR0005.                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
      *     BRAD0004 - CONVERTE BYPTE PARA BIT E BIT PARA BYTE         *
      *     BRAD0432 - CALCULO DE DIGITO                               *
      *     BRAD7600 - DATA/HORA                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *  BASES BD2:                                                    *
      *                                                                *
      *     RUCAB002 - DB2PRD.RUCA_MOVTO                               *
      *     RUCAB003 - DB2PRD.RUCA_CAD_ROOT                            *
      *     RUCAB004 - DB2PRD.RUCA_CAD_DEP                             *
      *     RUCAB012 - DB2PRD.TCTRL_ENVIO_MANUT                        *
      *     RUCAB015 - DB2PRD.TMANUT_CONTR_RECOR                       *
      *     RUCAB016 - DB2PRD.TMANUT_CORND_GEOGR                       *
      *     RUCAB017 - DB2PRD.TMANUT_PCELA_RECOR                       *
      *     RUECB025 - DB2PRD.PRORURA_I_CLPRORU                        *
      *     RUECB09N - DB2PRD.TCORND_GEOGR_CONTR                       *
      *     RUECB09M - DB2PRD.TCORND_GEOGR_CDULA                       *
      *     RUFIB002 - DB2PRD.CONTRATO_RURAL                           *
      *     RUFIB093 - DB2PRD.TRENEG_CONTR_RURAL                       *
      *     RUECV007 - DB2PRD.CEDULA_TEMPR                             *
      *                                                                *
      *================================================================*
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*
WP0522*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   MAIO/2022                                   *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.04                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP0522*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
STE001*                ALTERACAO - STEFANINI                           *
STE001*----------------------------------------------------------------*
STE001*                                                                *
STE001* ANALISTA REPONSAVEL: STEFANINI                                 *
STE001* DATA...............: MAIO / 2024                               *
STE001* OBJETIVOS..........: SICOR 5.08                                *
STE001*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: AGOSTO/2025                               *
.     * OBJETIVOS..........: FLASH - AGRO7-1628 - ALTERAÇÃO DA COR0001 *
.     *                      - SICOR 5.10                              *
7C5.10*================================================================*


      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************

      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** INICIO DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA TRATAMENTO DE ERRO ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           05  WRK-MSG-00              PIC  X(75)          VALUE
           '00 - MENSAGEM FORMATADA COM SUCESSO'.
           05  WRK-MSG-01              PIC  X(75)          VALUE
           '01 - CAMPO CHAVE COM CONTEUDO INVALIDO'.
           05  WRK-MSG-02              PIC  X(75)          VALUE
           '02 - CAMPO HORA DE INCLUSAO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-03              PIC  X(75)          VALUE
           '03 - CAMPO NUMERO DO ENVIO INVALIDO'.
           05  WRK-MSG-04              PIC  X(75)          VALUE
           '04 - CONTRATO NAO LOCALIZADO NA BASE DE MANUTENCOES'.
           05  WRK-MSG-05              PIC  X(75)          VALUE
           '05 - CONTRATO NAO LOCALIZADO NA BASE DE CONTROLE DE ENVIO'.
           05  WRK-MSG-06              PIC  X(75)          VALUE
           '06 - CONTRATO NAO LOCALIZADO NA BASE DE CADASTRO ROOT'.
           05  WRK-MSG-07              PIC  X(75)          VALUE
           '07 - CONTRATO NAO LOCALIZADO NA BASE CONTRATO RURAL'.
           05  WRK-MSG-99              PIC  X(75)          VALUE
           '99 - ERRO DE DB2'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DRESSCODE ***'.
      *----------------------------------------------------------------*

       01  WRK-S11-DRESS               PIC +9(11)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(03).
           05  WRK-N09-DRESS           PIC  9(09).
           05  WRK-X09-DRESS           REDEFINES
               WRK-N09-DRESS           PIC  X(09).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(04).
           05  WRK-N08-DRESS           PIC  9(08).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(05).
           05  WRK-N07-DRESS           PIC  9(07).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(09).
           05  WRK-N03-DRESS           PIC  9(03).
           05  WRK-X03-DRESS           REDEFINES
               WRK-N03-DRESS           PIC  X(03).

       01  WRK-S03V05-DRESS            PIC +9(03)V9(5)     VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S03V05-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-N03V02-DRESS        PIC  9(03)V9(2).
           05  FILLER                  PIC  X(03).

       01  WRK-S13V99-DRESS            PIC +9(13)V9(2)     VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-N13V99-DRESS        PIC  9(13)V9(2).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  IND-CHAR                    PIC  9(02) COMP-3   VALUE ZEROS.
       01  IND-PARC                    PIC  9(05) COMP-3   VALUE ZEROS.
       01  IND-LIN                     PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-IND                     PIC  9(05) COMP-3   VALUE ZEROS.

       01  ACU-NRO-PARC                PIC S9(07) COMP-3   VALUE ZEROS.
       01  ACU-B002-PARC               PIC S9(03) COMP-3   VALUE ZEROS.
       01  ACU-CPCELA-LIBRC            PIC S9(03) COMP-3   VALUE ZEROS.
       01  ACU-PERCENTUAL              PIC S9(01)V9(09)
                                                  COMP-3   VALUE ZEROS.
       01  ACU-NBASE-LEGAL-RENEG       PIC S9(05) COMP-3   VALUE ZEROS.
       01  ACU-VLMUTCOR                PIC S9(13)V9(02)
                                                  COMP-3   VALUE ZEROS.
       01  ACU-VLPROCOR                PIC S9(13)V9(02)
                                                  COMP-3   VALUE ZEROS.
       01  ACU-VLPRMUCOR               PIC S9(13)V9(02)
                                                  COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-NRO-PARC-ANT            PIC S9(03) COMP-3   VALUE ZEROS.
       01  WRK-CPCELA-LIBRC            PIC S9(03) COMP-3   VALUE ZEROS.
       01  WRK-CONTROLE-TAXA           PIC  9(05) COMP-3   VALUE 12672.
       01  WRK-ACHOU-PLANTIO           PIC  X(01)          VALUE SPACES.
       01  WRK-ACHOU-COLHEITA          PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR03-RUCAB017      PIC  X(01)          VALUE SPACES.
       01  WRK-LINHA-RUCAB004          PIC  X(292)         VALUE SPACES.
       01  WRK-DVCTO-PCELA-CONTR       PIC  X(10)          VALUE SPACES.

7C5.10 01  WRK-CSR08-FIM-1             PIC  X(01)          VALUE SPACES.

       01  WRK-CREFT-RECOR             PIC  X(11)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-CREFT-RECOR.
           05  FILLER                  PIC  X(02).
           05  WRK-CREFT-BACEN         PIC  9(09).

BVF001*
       01  WRK-TEM-MANUTENCAO          PIC  X(001) VALUE 'N'.
B0923  01  WRK-REDUC-ALT.
B0923      05  WRK-REDUC-TXT           PIC  X(40)          VALUE
B0923      'PERCENTUAL DE REDUCAO CAR (RESOL. 5.078)'.
B0923      05  WRK-REDUC-NUM           PIC  9(03)V99       VALUE ZEROS.
B0923      05  FILLER                  PIC  X(28)          VALUE SPACES.
      *

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAMENTO DE BIT-BYTE ***'.
      *----------------------------------------------------------------*

       01  WRK-B004-CONTROLE-AUX       PIC  X(08)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-B004-CONTROLE-AUX.
           05  WRK-B004-CONTROLE-1-AUX PIC  9(04)  COMP.
           05  WRK-B004-CONTROLE-2-AUX PIC  9(04)  COMP.
           05  WRK-B004-CONTROLE-3-AUX PIC  9(05)  COMP.

       01  WRK-CONTROLE-128            PIC  9(04)  COMP    VALUE 128.
       01  FILLER                      REDEFINES   WRK-CONTROLE-128.
           05  FILLER                  PIC  X(01).
           05  WRK-BIN-128             PIC  X(01).

       01  WRK-CONTROLE-AUX            PIC  9(04)  COMP    VALUE ZEROS.
       01  FILLER                      REDEFINES   WRK-CONTROLE-AUX.
           05  FILLER                  PIC  X(01).
           05  WRK-BIN-AUX             PIC  X(01).

       01  WRK-IDENTDC-240             PIC  9(03)  COMP-4  VALUE 240.
       01  FILLER                      REDEFINES   WRK-IDENTDC-240.
           05  FILLER                  PIC  X(01).
           05  WRK-IDENTDC-X1          PIC  X(01).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES PARA DATAS ***'.
      *----------------------------------------------------------------*

       01  WRK-TIMESTAMP.
           05  WRK-ANO-TIMES           PIC  9(04)          VALUE ZEROS.
           05  WRK-TR1-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-MES-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR2-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-DIA-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-TR3-TIMES           PIC  X(01)          VALUE '-'.
           05  WRK-HOR-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT1-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIN-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT2-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-SEG-TIMES           PIC  9(02)          VALUE ZEROS.
           05  WRK-PT3-TIMES           PIC  X(01)          VALUE '.'.
           05  WRK-MIL-TIMES           PIC  9(06)          VALUE ZEROS.

       01  WRK-DATA-ATUAL              PIC  9(08)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DATA-ATUAL.
           05  WRK-ANO-ATUAL           PIC  9(04).
           05  FILLER                  REDEFINES  WRK-ANO-ATUAL.
               07  WRK-SS-ATUAL        PIC  9(02).
               07  WRK-AA-ATUAL        PIC  9(02).
           05  WRK-MES-ATUAL           PIC  9(02).
           05  WRK-DIA-ATUAL           PIC  9(02).

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(02)          VALUE ZEROS.
           05  WRK-PT1-DB2             PIC  X(01)          VALUE '.'.
           05  WRK-MES-DB2             PIC  9(02)          VALUE ZEROS.
           05  WRK-PT2-DB2             PIC  X(01)          VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(04)          VALUE ZEROS.

       01  WRK-DT-JUL-ATUAL            PIC  9(07)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DT-JUL-ATUAL.
           05  WRK-SS-JUL-ATUAL        PIC  9(02).
           05  WRK-AADDD-JUL-ATUAL     PIC  9(05).

       01  WRK-DATA-AUX                PIC  9(08)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DATA-AUX.
           05  WRK-DIA-AUX             PIC  9(02).
           05  WRK-MES-AUX             PIC  9(02).
           05  WRK-ANO-AUX             PIC  9(04).
       01  FILLER                      REDEFINES  WRK-DATA-AUX.
           05  WRK-DATA-AUX-X          PIC  X(08).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS TABELA INTERNA PARCELA PGTO ***'.
      *----------------------------------------------------------------*

       01  TAB-TABELA-PARCELA.
           05  TAB-PARCELA     OCCURS  1000 TIMES.
               07  TAB-CCONTR          PIC S9(09)  COMP-3  VALUE ZEROS.
               07  TAB-HINCL-REG       PIC  X(26)          VALUE SPACES.
               07  TAB-CTPO-PCELA      PIC S9(01)  COMP-3  VALUE ZEROS.
               07  TAB-NPCELA-CONTR    PIC S9(03)  COMP-3  VALUE ZEROS.
               07  TAB-NATULZ-PCELA    PIC S9(03)  COMP-3  VALUE ZEROS.
               07  TAB-DVCTO-PCELA-CONTR
                                       PIC  X(10)          VALUE SPACES.
               07  TAB-VPCELA          PIC S9(13)V9(2)
                                                    COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS MOVIMENTO RUCAB002 ***'.
      *----------------------------------------------------------------*

       01  WRK-MVTO-B002.
           05  WRK-MOV-DATA            PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-MOV-HHMMSS          PIC  9(07)  COMP-3  VALUE ZEROS.
           05  WRK-MOV-CODFUNC         PIC  9(07)          VALUE ZEROS.
           05  WRK-MOV-CHAVE-CARTAO    PIC  9(02)          VALUE ZEROS.
           05  WRK-MOV-CODIFICACAO.
               10  WRK-MOV-CODIFI1     PIC  9(05)          VALUE ZEROS.
               10  WRK-MOV-CODIFI2     PIC  9(02)          VALUE ZEROS.
               10  WRK-MOV-CODIFI3     PIC  9(02)          VALUE ZEROS.
               10  WRK-MOV-CODIFI4     PIC  9(02)          VALUE ZEROS.
               10  WRK-MOV-CODIFI5     PIC  9(03)          VALUE ZEROS.
               10  WRK-MOV-CODIFI6     PIC  9(07)          VALUE ZEROS.
           05  WRK-MOV-CHAVE-LIB-PAG   PIC  9(01)          VALUE ZEROS.
           05  WRK-MOV-CHAVE-PARCELA   PIC  9(03)          VALUE ZEROS.
           05  WRK-MOV-DADOS-ENTRADA.
               10  WRK-MOV-CCONTR-TAXA PIC  9(09)          VALUE ZEROS.
               10  WRK-MOV-DIGITO-TAXA PIC  X(01)          VALUE SPACES.
               10  WRK-MOV-ORDEM-TAXA  PIC  9(03)          VALUE ZEROS.
               10  WRK-MOV-TAXA-JUROS  PIC  X(07)          VALUE SPACES.
               10  FILLER              REDEFINES  WRK-MOV-TAXA-JUROS.
                   15  WRK-MOV-TAXA-N02
                                       PIC  9(02).
                   15  WRK-MOV-TAXA-N03V02
                                       PIC  9(03)V9(2).
               10  WRK-MOV-DATA-JUROS  PIC  X(08)          VALUE SPACES.
               10  WRK-MOV-COD-JUROS   PIC  X(03)          VALUE SPACES.
               10  WRK-MOV-TAXA-CORRE  PIC  X(05)          VALUE SPACES.
               10  WRK-MOV-DATA-CORRE  PIC  X(08)          VALUE SPACES.
               10  WRK-MOV-COD-CORRE   PIC  X(03)          VALUE SPACES.
               10  WRK-MOV-TAXA-PROAG  PIC  X(05)          VALUE SPACES.
               10  WRK-MOV-DATA-PROAG  PIC  X(08)          VALUE SPACES.
               10  WRK-MOV-COD-PROAG   PIC  X(03)          VALUE SPACES.
               10  WRK-MOV-TAXA-ASSIS  PIC  X(05)          VALUE SPACES.
               10  WRK-MOV-DATA-ASSIS  PIC  X(08)          VALUE SPACES.
               10  WRK-MOV-COD-ASSIS   PIC  X(03)          VALUE SPACES.
               10  WRK-MOV-TAXA-IOF    PIC  X(05)          VALUE SPACES.
               10  WRK-MOV-DATA-IOF    PIC  X(08)          VALUE SPACES.
               10  WRK-MOV-COD-IOF     PIC  X(03)          VALUE SPACES.
               10  WRK-MOV-CORRECAO    PIC  X(01)          VALUE SPACES.
               10  WRK-MOV-SEQ-TAXA    PIC  X(02)          VALUE SPACES.
               10  WRK-MOV-CFUNC-TAXA  PIC  9(07)          VALUE ZEROS.
               10  WRK-MOV-SENHA       PIC  X(04)          VALUE SPACES.
               10  FILLER              PIC  X(110)         VALUE SPACES.
           05  WRK-MOV-DADOS-PGTO      REDEFINES  WRK-MOV-DADOS-ENTRADA.
               10  WRK-MOV-CCONTR-PARC PIC  9(09).
               10  WRK-MOV-DIGITO-PARC PIC  X(01).
               10  WRK-MOV-ORDEM-PARC  PIC  9(03).
               10  WRK-MOV-NRO-PARC    PIC  9(07).
               10  WRK-MOV-DIA-PARC    PIC  9(02).
               10  WRK-MOV-MES-PARC    PIC  9(02).
               10  WRK-MOV-ANO-PARC    PIC  9(04).
               10  WRK-MOV-SEQ-PARC    PIC  9(02).
               10  WRK-MOV-VLR-PARC    PIC  9(13)V99.
               10  WRK-MOV-CFUNC-PARC  PIC  9(07).
               10  FILLER              PIC  X(167).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD0004 ***'.
      *----------------------------------------------------------------*

       01  WRK-0004-TP-CONV            PIC  9(01)          VALUE ZEROS.
       01  WRK-0004-CPO-ENTRADA        PIC  X(40)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-0004-CPO-ENTRADA.
           05  WRK-0004-ENT-BYTE-1     PIC  X(01).
           05  FILLER                  PIC  X(39).
       01  FILLER                      REDEFINES  WRK-0004-CPO-ENTRADA.
           05  WRK-0004-ENT-5-BYTE     PIC  X(05).
           05  FILLER                  PIC  X(35).
       01  FILLER                      REDEFINES  WRK-0004-CPO-ENTRADA.
           05  WRK-0004-ENT-9-BITS     PIC  X(09).
           05  FILLER                  PIC  X(31).
       01  FILLER                      REDEFINES  WRK-0004-CPO-ENTRADA.
           05  WRK-0004-ENT-8-BITS     PIC  X(08).
           05  FILLER                  PIC  X(32).

       01  WRK-0004-CPO-SAIDA          PIC  X(40)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-0004-CPO-SAIDA.
           05  WRK-0004-SAI-5-BYTE     PIC  X(05).
           05  FILLER                  PIC  X(35).
       01  FILLER                      REDEFINES  WRK-0004-CPO-SAIDA.
           05  WRK-0004-SAI-BYTE-1     PIC  X(01).
           05  FILLER                  PIC  X(39).

       01  WRK-0004-BIT-BYTE           PIC  9(02)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD0432 ***'.
      *----------------------------------------------------------------*

       01  WRK-0432-CONTRATO           PIC  9(09)          VALUE ZEROS.
       01  WRK-0432-DIGITO             PIC  X(01)          VALUE SPACES.
       01  WRK-0432-TAMANHO            PIC  9(02)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DA BRAD7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-7600-DATA-HORA.
           05  WRK-7600-DT-JULIANA     PIC  9(05) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAMMDD      PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-DT-AAAAMMDD    PIC  9(09) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSS      PIC  9(07) COMP-3   VALUE ZEROS.
           05  WRK-7600-TI-HHMMSSMMMMMM
                                       PIC  9(13) COMP-3   VALUE ZEROS.
           05  WRK-7600-TIMESTAMP      PIC  X(20)          VALUE SPACES.
           05  FILLER                  REDEFINES  WRK-7600-TIMESTAMP.
               07  WRK-7600-ANO-TIMES  PIC  9(04).
               07  WRK-7600-MES-TIMES  PIC  9(02).
               07  WRK-7600-DIA-TIMES  PIC  9(02).
               07  WRK-7600-HOR-TIMES  PIC  9(02).
               07  WRK-7600-MIN-TIMES  PIC  9(02).
               07  WRK-7600-SEG-TIMES  PIC  9(02).
               07  WRK-7600-MIL-TIMES  PIC  9(06).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B002-CMUN-RURAL         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CORIGE-REC         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CESPCE-TITLO-GARNT PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CTPO-PROTR-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-VRECTA-BRUTA-EMPTO PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTEVENTO           PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTCONTAB           PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTACERT            PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTACERTO           PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CREFT-RECOR        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CESPCE-TITLO-GARNT PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-PROTR-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CORIGE-REC         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-VRECTA-BRUTA-EMPTO PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CMUN-RURAL         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-MAREA-TOT          PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-QUND-FINCD-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-QUND-PROD-PROVV    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DINIC-PLATI        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DFIM-PLATI         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DINIC-COLHE        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DFIM-COLHE         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-AGROP-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-INTGC-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CINDCD-UND-PRDTV   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-IGCAO-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-CTIVO-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CCICLO-PROD-RURAL  PIC S9(04)  COMP    VALUE ZEROS.
PRI499 01  WRK-B015-CCLIMT-RURAL-BACEN PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-VPCELA-LIBRC       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-VGARNT-RENDA-MIN   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-PJURO-ENCAR-FINCR  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DEVNTO             PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-EVNTO-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CFUNC-BDSCO-AUTRZ  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-PRISCO-FUNDO-CNSTT PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-PRISCO-TSORO-NACIO PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-PJURO-POS-ENCAR    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CTPO-ENCAR-COMPL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-NBASE-LEGAL-RENEG  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-DANO-BASE          PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CJUNC-DEPDC        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CSEQ-STUDO-NEGOC   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B016-MAREA-NAO-CTVDA    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B025-DLIBRC-OPER-ORCAM  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B025-DFIM-EVNTO-CDULA   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B09N-CUSUAR-MANUT       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B09N-HMANUT-REG         PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-B09N-CTPO-CORND-GEOGR   PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-B09M-CTPO-CORND-GEOGR   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B09M-CUSUAR-MANUT       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B09M-HMANUT-REG         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B093-NBASE-LEGAL-RENEG  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B093-DVCTO-RENEG-RURAL  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B093-CUSUAR-MANUT       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B093-HMANUT-REG         PIC S9(04)  COMP    VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT-NULL-15 PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CCICLO-CTVDA-NULL-15    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL-15     PIC S9(04)  COMP    VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT-NULL-02 PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CCICLO-CTVDA-NULL-02    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL-02     PIC S9(04)  COMP    VALUE ZEROS.

WP0522 01  WRK-CSCRET-TSORO-NULL-15    PIC S9(04)  COMP    VALUE ZEROS.

WP0522 01  WRK-CSCRET-TSORO-NULL-02    PIC S9(04)  COMP    VALUE ZEROS.
BVF001 01  WRK-PREDC-JURO-AMBTL-N      PIC S9(004) COMP    VALUE ZEROS.

STE001 01  WRK-CINDCD-CONFM-FLORS-N    PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-CINDCD-CONFM-IBAMA-N    PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-CINDCD-CONFM-CSVAO-N    PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-RREGLZ-IBAMA-N          PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-CNPJ-N                  PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-COUTRO-N                PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-RURAL-N                 PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-QUND-ANMAL-N            PIC S9(04)  COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB003
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB004
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB012
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB015
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB016
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB017
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB025
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09N
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09M
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB093
           END-EXEC.

BVF001     EXEC SQL
BVF001         INCLUDE RUCAB014
BVF001     END-EXEC.

STE002     EXEC SQL
STE002         INCLUDE RUECV007
STE002     END-EXEC.

7C5.10     EXEC SQL
.              INCLUDE RUCAB023
7C5.10     END-EXEC.

7C5.10     EXEC SQL
.              INCLUDE RUFIB094
7C5.10     END-EXEC.

           EXEC SQL
               DECLARE  CSR01-RUECB025  CURSOR WITH HOLD FOR
                SELECT  CJUNC_DEPDC       ,
                        CCDULA            ,
                        CPCELA_LIBRC      ,
                        DLIBRC_OPER_ORCAM ,
                        RTPO_OPER_ORCAM   ,
                        DFIM_EVNTO_CDULA
                  FROM  DB2PRD.ORCAM_CDULA_RURAL
                 WHERE  CJUNC_DEPDC   = :RUECB025.CJUNC-DEPDC
                   AND  CCDULA        = :RUECB025.CCDULA
                   AND  CPCELA_LIBRC  > :RUECB025.CPCELA-LIBRC
           END-EXEC.

           EXEC SQL
               DECLARE  CSR02-RUCAB004 CURSOR WITH HOLD FOR
                SELECT  B004_CH_CONTR            ,
                        B004_CH_TIPO             ,
                        B004_CH_NUMERO           ,
                        B004_ESPECIE             ,
                        B004_CONTROLE            ,
                        B004_DTEVENTO            ,
                        B004_CODLANC             ,
                        B004_DTCONTAB            ,
                        B004_DTACERT             ,
                        B004_IDENTDC             ,
                        B004_DEBCRED             ,
                        B004_VLPRIORI            ,
                        B004_VLPRICOR            ,
                        B004_VLMUTORI            ,
                        B004_VLMUTCOR            ,
                        B004_VLPROORI            ,
                        B004_VLPROCOR            ,
                        B004_VLPRMUORI           ,
                        B004_VLPRMUCOR           ,
                        B004_VLAUXIL             ,
                        B004_JURORIG             ,
                        B004_JURCORRI            ,
                        B004_CORRORIG            ,
                        B004_CORRCORRI           ,
                        B004_PROAORIG            ,
                        B004_PROACORR            ,
                        B004_ASSIORIG            ,
                        B004_ASSICORR            ,
                        B004_LAUDORIG            ,
                        B004_LAUDCORR            ,
                        B004_MEDIORIG            ,
                        B004_MEDICORR            ,
                        B004_REMUORIG            ,
                        B004_REMUCORR            ,
                        B004_IOFORIG             ,
                        B004_IOFCORR             ,
                        B004_AGDEBCRED           ,
                        B004_RZDEBCRED           ,
                        B004_CTDEBCRED           ,
                        B004_DTMORA              ,
                        B004_LANCMORA            ,
                        B004_DBCRMORA            ,
                        B004_NUMMORA             ,
                        B004_CODCRS              ,
                        B004_DTACERTO            ,
                        B004_CODACERT
                  FROM  DB2PRD.RUCA_CAD_DEP
                 WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                   AND  B004_CH_TIPO       = :RUCAB004.B004-CH-TIPO
                   AND  B004_CH_NUMERO    >= :RUCAB004.B004-CH-NUMERO
                 ORDER  BY B004_CH_NUMERO
           END-EXEC.

           EXEC SQL
              DECLARE  CSR03-RUCAB017 CURSOR WITH HOLD FOR
               SELECT  CCONTR                     ,
                       HINCL_REG                  ,
                       CTPO_PCELA                 ,
                       NPCELA_CONTR               ,
                       NATULZ_PCELA               ,
                       DVCTO_PCELA_CONTR          ,
                       VPCELA
                 FROM  DB2PRD.TMANUT_PCELA_RECOR
                WHERE  CCONTR        = :RUCAB017.CCONTR
                  AND  HINCL_REG     = :RUCAB017.HINCL-REG
                  AND  CTPO_PCELA    = :RUCAB017.CTPO-PCELA
                  AND  NPCELA_CONTR >= :RUCAB017.NPCELA-CONTR
                  AND  NATULZ_PCELA >= :RUCAB017.NATULZ-PCELA
                ORDER  BY NPCELA_CONTR      ASC,
                          DVCTO_PCELA_CONTR ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR04-RUCAB016 CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       HINCL_REG         ,
                       CSEQ_MATR_RURAL   ,
                       NSEQ_GLEBA_IMOV   ,
                       NCORND_GEOGR_IMOV ,
                       MLATTD_IMOV_RURAL ,
                       MLOGTD_IMOV_RURAL ,
                       MALTTD_IMOV_RURAL ,
                       MAREA_NAO_CTVDA   ,
STEFAB                 VALUE(CTPO_CORND_GEOGR, 2)
                 FROM  DB2PRD.TMANUT_CORND_GEOGR
                WHERE  CCONTR             = :RUCAB016.CCONTR
                  AND  HINCL_REG          = :RUCAB016.HINCL-REG
                  AND  CSEQ_MATR_RURAL   >= :RUCAB016.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUCAB016.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUCAB016.NCORND-GEOGR-IMOV
                ORDER  BY  CSEQ_MATR_RURAL    ASC ,
                           NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR05-RUECB09N CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       CSEQ_MATR_RURAL   ,
                       NSEQ_GLEBA_IMOV   ,
                       NCORND_GEOGR_IMOV ,
                       MLATTD_IMOV_RURAL ,
                       MLOGTD_IMOV_RURAL ,
                       MALTTD_IMOV_RURAL ,
                       CUSUAR_INCL       ,
                       HINCL             ,
                       CUSUAR_MANUT      ,
                       HMANUT_REG
                 FROM  DB2PRD.TCORND_GEOGR_CONTR
                WHERE  CCONTR             = :RUECB09N.CCONTR
                  AND  CSEQ_MATR_RURAL    = :RUECB09N.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUECB09N.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUECB09N.NCORND-GEOGR-IMOV
                ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR06-RUECB09M CURSOR WITH HOLD FOR
               SELECT  CJUNC_DEPDC        ,
                       CCDULA             ,
                       CSEQ_MATR_RURAL    ,
                       NSEQ_GLEBA_IMOV    ,
                       NCORND_GEOGR_IMOV  ,
                       MLATTD_IMOV_RURAL  ,
                       MLOGTD_IMOV_RURAL  ,
                       MALTTD_IMOV_RURAL  ,
                       CUSUAR_INCL        ,
                       HINCL              ,
                       CUSUAR_MANUT       ,
                       HMANUT_REG
                 FROM  DB2PRD.TCORND_GEOGR_CDULA
                WHERE  CJUNC_DEPDC        = :RUECB09M.CJUNC-DEPDC
                  AND  CCDULA             = :RUECB09M.CCDULA
                  AND  CSEQ_MATR_RURAL    = :RUECB09M.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUECB09M.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUECB09M.NCORND-GEOGR-IMOV
                ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

7C5.10     EXEC SQL
.             DECLARE  CSR07-RUCAB023 CURSOR WITH HOLD FOR
.              SELECT  CCONTR             ,
.                      COUTRO_TPO_CONFM
.                FROM  DB2PRD.TCONTR_RECOR_TPO_CONFM
.               WHERE  CCONTR             = :RUCAB023.CCONTR
.                 AND  HINCL_REG          = :RUCAB023.HINCL-REG
.               ORDER  BY  COUTRO_TPO_CONFM   ASC
.          END-EXEC.
.
.          EXEC SQL
.             DECLARE  CSR08-RUFIB094 CURSOR WITH HOLD FOR
.              SELECT  CCONTR             ,
.                      COUTRO_TPO_CONFM
.                FROM  DB2PRD.TCONTR_RURAL_OUTRO_CONFM
.               WHERE  CCONTR             = :RUFIB094.CCONTR
.               ORDER  BY  COUTRO_TPO_CONFM   ASC
7C5.10     END-EXEC.

BVF001     EXEC SQL
.              DECLARE  CSR01-RUCAB014    CURSOR FOR
.               SELECT  CCONTR              ,
.                       HINCL_REG           ,
.                       NMANUT_EFETU        ,
.                       RCPO_MANUT_RECOR    ,
.                       DMANUT              ,
.                       CFUNC_BDSCO
.                 FROM  DB2PRD.TLOG_MANUT_CONTR
.                WHERE  CCONTR            = :RUCAB014.CCONTR
.                  AND  HINCL_REG        >= :RUCAB014.HINCL-REG
.                  AND  NMANUT_EFETU     >= :RUCAB014.NMANUT-EFETU
.                  AND  DMANUT            = CURRENT DATE
.                ORDER  BY HINCL_REG DESC
BVF001     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************

       01  COMU-AREA-RUCA8807.
           05  COMU-8807-ENTRADA.
               07  COMU-8807-CHAVE     PIC  9(09).
               07  COMU-8807-CTRLNAV.
                   10  COMU-8807-ANO-CTRLNAV
                                       PIC  9(04).
                   10  COMU-8807-MES-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8807-DIA-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8807-HOR-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8807-MIN-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8807-SEG-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8807-MIL-CTRLNAV
                                       PIC  9(06).
               07  COMU-8807-NENVIO    PIC  9(05).
               07  COMU-8807-CODMSG    PIC  X(09).
           05  COMU-8807-SAIDA.
               07  COMU-8807-COD-RETORNO
                                       PIC  9(02).
               07  COMU-8807-MENSAGEM  PIC  X(75).
               07  COMU-8807-ERRO-AREA PIC  X(107).
               07  COMU-8807-SQLCA     PIC  X(136).

      ******************************************************************
       PROCEDURE                      DIVISION USING COMU-AREA-RUCA8807.
      ******************************************************************

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*
           PERFORM  1000-CONSISTIR-ENTRADA
           PERFORM  5000-PROCESSAR-DADOS
           PERFORM  8000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-CONSISTIR-ENTRADA          SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  COMU-8807-COD-RETORNO
           MOVE  WRK-MSG-00            TO  COMU-8807-MENSAGEM
           MOVE  SPACES                TO  COMU-8807-ERRO-AREA
                                           COMU-8807-SQLCA

           IF  COMU-8807-CHAVE         IS  NUMERIC  AND
               COMU-8807-CHAVE    GREATER  ZEROS
               PERFORM  1005-CALCULAR-DIG-CHAVE
           ELSE
               MOVE  01                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-01        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8807-CTRLNAV       IS  NUMERIC  AND
               COMU-8807-CTRLNAV  GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  02                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-02        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8807-NENVIO        IS  NUMERIC  AND
               COMU-8807-NENVIO   GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  03                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-03        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1005-CALCULAR-DIG-CHAVE         SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8807-CHAVE       TO  WRK-0432-CONTRATO
           MOVE  SPACES                TO  WRK-0432-DIGITO
           MOVE  09                    TO  WRK-0432-TAMANHO

           CALL  'BRAD0432'         USING  WRK-0432-CONTRATO
                                           WRK-0432-DIGITO
                                           WRK-0432-TAMANHO.

      *----------------------------------------------------------------*
       1005-99-FIM.  EXIT.
      *----------------------------------------------------------------*
BVF001
      *----------------------------------------------------------------*
       4150-PROCURAR-CONTR-MANUT       SECTION.
      *----------------------------------------------------------------*
           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB014
           MOVE  'N'                   TO  WRK-TEM-MANUTENCAO

           PERFORM  4155-OPEN-CSR01-RUCAB014
           PERFORM  4160-FETCH-CSR01-RUCAB014

           PERFORM
             UNTIL  SQLCODE             EQUAL  +100
                OR  WRK-TEM-MANUTENCAO  EQUAL  'S'

                PERFORM  4160-FETCH-CSR01-RUCAB014
           END-PERFORM.

           PERFORM  4165-CLOSE-CSR01-RUCAB014
      *    CORRIGE O CAMPO DE DESCRIÃ-Ã-O NA TABELA.
           IF WRK-TEM-MANUTENCAO EQUAL 'S'
              MOVE WRK-REDUC-NUM       TO PREDC-JURO-AMBTL OF RUFIB002
           END-IF.

      *----------------------------------------------------------------*
       4150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4155-OPEN-CSR01-RUCAB014        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUFIB002 TO  CCONTR           OF RUCAB014
           MOVE  '1900-01-01-00.00.00.000000'
                                       TO  HINCL-REG        OF RUCAB014
           MOVE  ZEROS                 TO  NMANUT-EFETU     OF RUCAB014

           EXEC SQL
                OPEN CSR01-RUCAB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       4155-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4160-FETCH-CSR01-RUCAB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB014
                 INTO  :RUCAB014.CCONTR              ,
                       :RUCAB014.HINCL-REG           ,
                       :RUCAB014.NMANUT-EFETU        ,
                       :RUCAB014.RCPO-MANUT-RECOR    ,
                       :RUCAB014.DMANUT              ,
                       :RUCAB014.CFUNC-BDSCO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 8000-FINALIZAR
           END-IF.

           IF SQLCODE EQUAL ZEROS
              MOVE RCPO-MANUT-RECOR-TEXT OF RUCAB014
                                         (1:LENGTH OF WRK-REDUC-ALT)
                                        TO WRK-REDUC-ALT
              IF WRK-REDUC-TXT          EQUAL
                             'PERCENTUAL DE REDUCAO CAR (RESOL. 5.078)'
                 MOVE 'S'               TO WRK-TEM-MANUTENCAO

              END-IF
           END-IF.

      *----------------------------------------------------------------*
       4160-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4165-CLOSE-CSR01-RUCAB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TLOG_MANUT_CONTR' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               PERFORM 8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       4165-99-FIM.  EXIT.
      *----------------------------------------------------------------*
BVF001
      *----------------------------------------------------------------*
       5000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-OBTER-DATA-HORA
           PERFORM  5010-SELECT-RUCAB015
           PERFORM  5020-SELECT-RUCAB012
           PERFORM  5030-SELECT-RUFIB002
           PERFORM  5025-SELECT-RUCAB003
           PERFORM  5035-ATUALIZAR-CONTRATO

           IF  WRK-B015-DINIC-PLATI       EQUAL  -1  AND
               WRK-B015-DFIM-PLATI        EQUAL  -1  AND
               WRK-B015-DINIC-COLHE       EQUAL  -1  AND
               WRK-B015-DFIM-COLHE        EQUAL  -1
               CONTINUE
           ELSE
               PERFORM  5050-ATUALIZAR-PLATI-COLH
           END-IF.

           IF  WRK-B015-PJURO-ENCAR-FINCR EQUAL  -1  AND
               WRK-B015-DEVNTO            EQUAL  -1  AND
               WRK-B015-CTPO-EVNTO-RURAL  EQUAL  -1
               CONTINUE
           ELSE
               PERFORM  5115-ATUALIZAR-TAXA
           END-IF.

           PERFORM  5175-TRATAR-PARC-PGTO
           PERFORM  5270-TRATAR-COORDENADAS.
7C5.10     IF COMU-8807-CODMSG EQUAL 'COR0001R1'
.             PERFORM  5360-TRATAR-CONFORMIDADE
7C5.10     END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5005-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-7600-DATA-HORA

           CALL 'BRAD7600'          USING  WRK-7600-DATA-HORA

           MOVE  WRK-7600-DT-AAAAMMDD  TO  WRK-S11-DRESS
           MOVE  WRK-N08-DRESS         TO  WRK-DATA-ATUAL
           MOVE  WRK-SS-ATUAL          TO  WRK-SS-JUL-ATUAL
           MOVE  WRK-7600-DT-JULIANA   TO  WRK-AADDD-JUL-ATUAL

           MOVE  WRK-7600-ANO-TIMES    TO  WRK-ANO-TIMES
           MOVE  WRK-7600-MES-TIMES    TO  WRK-MES-TIMES
           MOVE  WRK-7600-DIA-TIMES    TO  WRK-DIA-TIMES
           MOVE  WRK-7600-HOR-TIMES    TO  WRK-HOR-TIMES
           MOVE  WRK-7600-MIN-TIMES    TO  WRK-MIN-TIMES
           MOVE  WRK-7600-SEG-TIMES    TO  WRK-SEG-TIMES
           MOVE  WRK-7600-MIL-TIMES    TO  WRK-MIL-TIMES.

      *----------------------------------------------------------------*
       5005-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-SELECT-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8807-CHAVE       TO  CCONTR           OF RUCAB015
           MOVE  COMU-8807-ANO-CTRLNAV TO  WRK-ANO-TIMES
           MOVE  COMU-8807-MES-CTRLNAV TO  WRK-MES-TIMES
           MOVE  COMU-8807-DIA-CTRLNAV TO  WRK-DIA-TIMES
           MOVE  COMU-8807-HOR-CTRLNAV TO  WRK-HOR-TIMES
           MOVE  COMU-8807-MIN-CTRLNAV TO  WRK-MIN-TIMES
           MOVE  COMU-8807-SEG-CTRLNAV TO  WRK-SEG-TIMES
           MOVE  COMU-8807-MIL-CTRLNAV TO  WRK-MIL-TIMES
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB015
           EXEC SQL
                SELECT
                        CCONTR                        ,
                        HINCL_REG                     ,
                        CSIT_ALT_REG                  ,
                        VFINAN                        ,
                        CESPCE_TITLO_GARNT            ,
                        CTPO_PROTR_RURAL              ,
                        CORIGE_REC                    ,
                        VRECTA_BRUTA_EMPTO            ,
                        CMUN_RURAL                    ,
                        MAREA_TOT                     ,
                        QUND_FINCD_RURAL              ,
                        CPRODT_RURAL                  ,
                        QUND_PROD_PROVV               ,
                        DINIC_PLATI                   ,
                        DFIM_PLATI                    ,
                        DINIC_COLHE                   ,
                        DFIM_COLHE                    ,
                        CTPO_AGROP_RURAL              ,
                        CTPO_INTGC_RURAL              ,
                        CINDCD_UND_PRDTV              ,
                        CTPO_IGCAO_RURAL              ,
                        CTPO_CTIVO_RURAL              ,
                        CCICLO_PROD_RURAL             ,
                        VPCELA_LIBRC                  ,
                        VGARNT_RENDA_MIN              ,
                        PJURO_ENCAR_FINCR             ,
                        DEVNTO                        ,
                        CTPO_EVNTO_RURAL              ,
                        CFUNC_BDSCO_AUTRZ             ,
                        PRISCO_FUNDO_CNSTT            ,
                        PRISCO_TSORO_NACIO            ,
                        PJURO_POS_ENCAR               ,
                        CTPO_ENCAR_COMPL              ,
                        NBASE_LEGAL_RENEG             ,
                        DANO_BASE                     ,
                        CJUNC_DEPDC                   ,
                        CSEQ_STUDO_NEGOC              ,
                        DVCTO_CONTR                   ,
                        DMOVTO_RECOR                  ,
                        CFUNC_ATULZ                   ,
WP1021                  CTPO_SOLO_AGCLT               ,
WP1021                  CCICLO_CTVDA                  ,
WP1021                  CMANUT_SOLO                   ,
WP0522                  CSCRET_TSORO_NACIO            ,
PRI499                  CCLIMT_RURAL_BACEN            ,
STE001**                COUTRO_TRATO_SOLO             ,
STE002            VALUE(COUTRO_TRATO_SOLO,  0)        ,
STE002            VALUE(CINDCD_CONFM_FLORS, 9)        ,
STE002            VALUE(CINDCD_CONFM_IBAMA, 9)        ,
STE002            VALUE(CINDCD_CONFM_CSVAO, 9)        ,
STE002            VALUE(RREGLZ_IBAMA, '********************'),
STEFAB            VALUE(CCNPJ_INSTC_EXIGB, 0),
STEFAB            VALUE(CFLIAL_CNPJ_EXIGB, 0),
STEFAB            VALUE(CCTRL_CNPJ_EXIGB, 0),
STEFAB            VALUE(CINDCD_CONFM_RURAL, 9),
STEFAB            VALUE(COUTRO_TPO_CONFM, 0),
STEFA1            VALUE(QUND_ANMAL, 0)
                  INTO
                       :RUCAB015.CCONTR               ,
                       :RUCAB015.HINCL-REG            ,
                       :RUCAB015.CSIT-ALT-REG         ,
                       :RUCAB015.VFINAN               ,
                       :RUCAB015.CESPCE-TITLO-GARNT
                         :WRK-B015-CESPCE-TITLO-GARNT ,
                       :RUCAB015.CTPO-PROTR-RURAL
                         :WRK-B015-CTPO-PROTR-RURAL   ,
                       :RUCAB015.CORIGE-REC
                         :WRK-B015-CORIGE-REC         ,
                       :RUCAB015.VRECTA-BRUTA-EMPTO
                         :WRK-B015-VRECTA-BRUTA-EMPTO ,
                       :RUCAB015.CMUN-RURAL
                         :WRK-B015-CMUN-RURAL         ,
                       :RUCAB015.MAREA-TOT
                         :WRK-B015-MAREA-TOT          ,
                       :RUCAB015.QUND-FINCD-RURAL
                         :WRK-B015-QUND-FINCD-RURAL   ,
                       :RUCAB015.CPRODT-RURAL
                         :WRK-B015-CPRODT-RURAL       ,
                       :RUCAB015.QUND-PROD-PROVV
                         :WRK-B015-QUND-PROD-PROVV    ,
                       :RUCAB015.DINIC-PLATI
                         :WRK-B015-DINIC-PLATI        ,
                       :RUCAB015.DFIM-PLATI
                         :WRK-B015-DFIM-PLATI         ,
                       :RUCAB015.DINIC-COLHE
                         :WRK-B015-DINIC-COLHE        ,
                       :RUCAB015.DFIM-COLHE
                         :WRK-B015-DFIM-COLHE         ,
                       :RUCAB015.CTPO-AGROP-RURAL
                         :WRK-B015-CTPO-AGROP-RURAL   ,
                       :RUCAB015.CTPO-INTGC-RURAL
                         :WRK-B015-CTPO-INTGC-RURAL   ,
                       :RUCAB015.CINDCD-UND-PRDTV
                         :WRK-B015-CINDCD-UND-PRDTV   ,
                       :RUCAB015.CTPO-IGCAO-RURAL
                         :WRK-B015-CTPO-IGCAO-RURAL   ,
                       :RUCAB015.CTPO-CTIVO-RURAL
                         :WRK-B015-CTPO-CTIVO-RURAL   ,
                       :RUCAB015.CCICLO-PROD-RURAL
                         :WRK-B015-CCICLO-PROD-RURAL  ,
                       :RUCAB015.VPCELA-LIBRC
                         :WRK-B015-VPCELA-LIBRC       ,
                       :RUCAB015.VGARNT-RENDA-MIN
                         :WRK-B015-VGARNT-RENDA-MIN   ,
                       :RUCAB015.PJURO-ENCAR-FINCR
                         :WRK-B015-PJURO-ENCAR-FINCR  ,
                       :RUCAB015.DEVNTO
                         :WRK-B015-DEVNTO             ,
                       :RUCAB015.CTPO-EVNTO-RURAL
                         :WRK-B015-CTPO-EVNTO-RURAL   ,
                       :RUCAB015.CFUNC-BDSCO-AUTRZ
                         :WRK-B015-CFUNC-BDSCO-AUTRZ  ,
                       :RUCAB015.PRISCO-FUNDO-CNSTT
                         :WRK-B015-PRISCO-FUNDO-CNSTT ,
                       :RUCAB015.PRISCO-TSORO-NACIO
                         :WRK-B015-PRISCO-TSORO-NACIO ,
                       :RUCAB015.PJURO-POS-ENCAR
                         :WRK-B015-PJURO-POS-ENCAR    ,
                       :RUCAB015.CTPO-ENCAR-COMPL
                         :WRK-B015-CTPO-ENCAR-COMPL   ,
                       :RUCAB015.NBASE-LEGAL-RENEG
                         :WRK-B015-NBASE-LEGAL-RENEG  ,
                       :RUCAB015.DANO-BASE
                         :WRK-B015-DANO-BASE          ,
                       :RUCAB015.CJUNC-DEPDC
                         :WRK-B015-CJUNC-DEPDC        ,
                       :RUCAB015.CSEQ-STUDO-NEGOC
                         :WRK-B015-CSEQ-STUDO-NEGOC   ,
                       :RUCAB015.DVCTO-CONTR          ,
                       :RUCAB015.DMOVTO-RECOR         ,
                       :RUCAB015.CFUNC-ATULZ          ,
WP1021                 :RUCAB015.CTPO-SOLO-AGCLT
WP1021                   :WRK-CTPO-SOLO-AGCLT-NULL-15 ,
WP1021                 :RUCAB015.CCICLO-CTVDA
WP1021                   :WRK-CCICLO-CTVDA-NULL-15    ,
WP1021                 :RUCAB015.CMANUT-SOLO
WP1021                   :WRK-CMANUT-SOLO-NULL-15     ,
WP0522                 :RUCAB015.CSCRET-TSORO-NACIO
WP0522                   :WRK-CSCRET-TSORO-NULL-15    ,
PRI499                 :RUCAB015.CCLIMT-RURAL-BACEN
PRI499                   :WRK-B015-CCLIMT-RURAL-BACEN ,
STE001                 :RUCAB015.COUTRO-TRATO-SOLO    ,
STE002                 :RUCAB015.CINDCD-CONFM-FLORS   ,
STE002                 :RUCAB015.CINDCD-CONFM-IBAMA   ,
STE002                 :RUCAB015.CINDCD-CONFM-CSVAO   ,
STE002                 :RUCAB015.RREGLZ-IBAMA,
STEFAB                 :RUCAB015.CCNPJ-INSTC-EXIGB,
STEFAB                 :RUCAB015.CFLIAL-CNPJ-EXIGB,
STEFAB                 :RUCAB015.CCTRL-CNPJ-EXIGB,
STEFAB                 :RUCAB015.CINDCD-CONFM-RURAL,
STEFAB                 :RUCAB015.COUTRO-TPO-CONFM,
STEFA1                 :RUCAB015.QUND-ANMAL
                  FROM  DB2PRD.TMANUT_CONTR_RECOR
                 WHERE  CCONTR             = :RUCAB015.CCONTR
                   AND  HINCL_REG          = :RUCAB015.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CONTR_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  04                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-04        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           ELSE
               PERFORM  5015-TRATAR-NULOS-B015
           END-IF.

      *----------------------------------------------------------------*
       5010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5015-TRATAR-NULOS-B015          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-B015-CESPCE-TITLO-GARNT   EQUAL  -1
               MOVE  ZEROS             TO CESPCE-TITLO-GARNT OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-PROTR-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-PROTR-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CORIGE-REC           EQUAL  -1
               MOVE  ZEROS             TO CORIGE-REC         OF RUCAB015
           END-IF.

           IF  WRK-B015-VRECTA-BRUTA-EMPTO   EQUAL  -1
               MOVE  ZEROS             TO VRECTA-BRUTA-EMPTO OF RUCAB015
           END-IF.

           IF  WRK-B015-CMUN-RURAL           EQUAL  -1
               MOVE  ZEROS             TO CMUN-RURAL         OF RUCAB015
           END-IF.

           IF  WRK-B015-MAREA-TOT            EQUAL  -1
               MOVE  ZEROS             TO MAREA-TOT          OF RUCAB015
           END-IF.

           IF  WRK-B015-QUND-FINCD-RURAL     EQUAL  -1
               MOVE  ZEROS             TO QUND-FINCD-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CPRODT-RURAL         EQUAL  -1
               MOVE  ZEROS             TO CPRODT-RURAL       OF RUCAB015
           END-IF.

           IF  WRK-B015-QUND-PROD-PROVV      EQUAL  -1
               MOVE  ZEROS             TO QUND-PROD-PROVV    OF RUCAB015
           END-IF.

           IF  WRK-B015-DINIC-PLATI          EQUAL  -1
               MOVE  SPACES            TO DINIC-PLATI        OF RUCAB015
           END-IF.

           IF  WRK-B015-DFIM-PLATI           EQUAL  -1
               MOVE  SPACES            TO DFIM-PLATI         OF RUCAB015
           END-IF.

           IF  WRK-B015-DINIC-COLHE          EQUAL  -1
               MOVE  SPACES            TO DINIC-COLHE        OF RUCAB015
           END-IF.

           IF  WRK-B015-DFIM-COLHE           EQUAL  -1
               MOVE  SPACES            TO DFIM-COLHE         OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-AGROP-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-AGROP-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-INTGC-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-INTGC-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CINDCD-UND-PRDTV     EQUAL  -1
               MOVE  ZEROS             TO CINDCD-UND-PRDTV   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-IGCAO-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-IGCAO-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-CTIVO-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-CTIVO-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CCICLO-PROD-RURAL    EQUAL  -1
               MOVE  ZEROS             TO CCICLO-PROD-RURAL  OF RUCAB015
           END-IF.

PRI499     IF  WRK-B015-CCLIMT-RURAL-BACEN   EQUAL  -1
PRI499         MOVE  ZEROS             TO CCLIMT-RURAL-BACEN OF RUCAB015
PRI499     END-IF.

           IF  WRK-B015-VPCELA-LIBRC         EQUAL  -1
               MOVE  ZEROS             TO VPCELA-LIBRC       OF RUCAB015
           END-IF.

           IF  WRK-B015-VGARNT-RENDA-MIN     EQUAL  -1
               MOVE  ZEROS             TO VGARNT-RENDA-MIN   OF RUCAB015
           END-IF.

           IF  WRK-B015-PJURO-ENCAR-FINCR    EQUAL  -1
               MOVE  ZEROS             TO PJURO-ENCAR-FINCR  OF RUCAB015
           END-IF.

           IF  WRK-B015-DEVNTO               EQUAL  -1
               MOVE  SPACES            TO DEVNTO             OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-EVNTO-RURAL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-EVNTO-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CFUNC-BDSCO-AUTRZ    EQUAL  -1
               MOVE  ZEROS             TO CFUNC-BDSCO-AUTRZ  OF RUCAB015
           END-IF.

           IF  WRK-B015-PRISCO-FUNDO-CNSTT   EQUAL  -1
               MOVE  ZEROS             TO PRISCO-FUNDO-CNSTT OF RUCAB015
           END-IF.

           IF  WRK-B015-PRISCO-TSORO-NACIO   EQUAL  -1
               MOVE  ZEROS             TO PRISCO-TSORO-NACIO OF RUCAB015
           END-IF.

           IF  WRK-B015-PJURO-POS-ENCAR      EQUAL  -1
               MOVE  ZEROS             TO PJURO-POS-ENCAR    OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-ENCAR-COMPL     EQUAL  -1
               MOVE  ZEROS             TO CTPO-ENCAR-COMPL   OF RUCAB015
           END-IF.

           IF  WRK-B015-NBASE-LEGAL-RENEG    EQUAL  -1
               MOVE  ZEROS             TO NBASE-LEGAL-RENEG  OF RUCAB015
           END-IF.

           IF  WRK-B015-DANO-BASE            EQUAL  -1
               MOVE  ZEROS             TO DANO-BASE          OF RUCAB015
           END-IF.

           IF  WRK-B015-CJUNC-DEPDC          EQUAL  -1
               MOVE  ZEROS             TO CJUNC-DEPDC        OF RUCAB015
           END-IF.

           IF  WRK-B015-CSEQ-STUDO-NEGOC     EQUAL  -1
               MOVE  ZEROS             TO CSEQ-STUDO-NEGOC   OF RUCAB015
           END-IF.

WP1021     IF  WRK-CTPO-SOLO-AGCLT-NULL-15   EQUAL  -1
WP1021         MOVE  ZEROS             TO CTPO-SOLO-AGCLT    OF RUCAB015
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CCICLO-CTVDA-NULL-15      EQUAL  -1
WP1021         MOVE  ZEROS             TO CCICLO-CTVDA       OF RUCAB015
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CMANUT-SOLO-NULL-15       EQUAL  -1
WP1021         MOVE  ZEROS             TO CMANUT-SOLO        OF RUCAB015
WP1021     END-IF.
WP1021
WP0522     IF  WRK-CSCRET-TSORO-NULL-15      EQUAL  -1
WP0522         MOVE  ZEROS             TO CSCRET-TSORO-NACIO OF RUCAB015
WP0522     END-IF.
WP0522
      *----------------------------------------------------------------*
       5015-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-SELECT-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB012
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB012
           MOVE  COMU-8807-NENVIO      TO  NENVIO-RECOR     OF RUCAB012

           EXEC SQL
                SELECT
                        CCONTR                         ,
                        HINCL_REG                      ,
                        NENVIO_RECOR                   ,
                        CREFT_RECOR
                INTO
                       :RUCAB012.CCONTR                ,
                       :RUCAB012.HINCL-REG             ,
                       :RUCAB012.NENVIO-RECOR          ,
                       :RUCAB012.CREFT-RECOR
                         :WRK-B012-CREFT-RECOR
                  FROM  DB2PRD.TCTRL_ENVIO_MANUT
                 WHERE  CCONTR             = :RUCAB012.CCONTR
                   AND  HINCL_REG          = :RUCAB012.HINCL-REG
                   AND  NENVIO_RECOR       = :RUCAB012.NENVIO-RECOR

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  05                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-05        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           ELSE
               IF  WRK-B012-CREFT-RECOR  EQUAL  -1
                   MOVE  SPACES        TO  CREFT-RECOR      OF RUCAB012
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5025-SELECT-RUCAB003            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  B003-CH-CONTR    OF RUCAB003

           EXEC SQL
                SELECT
                        B003_CONTROL               ,
                        B003_NUMDEPEN
                INTO
                       :RUCAB003.B003-CONTROL      ,
                       :RUCAB003.B003-NUMDEPEN
                  FROM  DB2PRD.RUCA_CAD_ROOT
                 WHERE  B003_CH_CONTR    = :RUCAB003.B003-CH-CONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_ROOT'    TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  06                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-06        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           ELSE
               IF  WRK-B012-CREFT-RECOR  EQUAL  -1
                   MOVE  SPACES        TO  CREFT-RECOR      OF RUCAB012
               END-IF

               MOVE  B003-NUMDEPEN     TO  ACU-NRO-PARC
           END-IF.

      *----------------------------------------------------------------*
       5025-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5030-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUFIB002

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        CCDULA                        ,
                        DCDULA                        ,
                        MAREA_TOT                     ,
                        DVCTO_CONTR                   ,
                        VFINAN                        ,
                        QUND_FINCD_RURAL              ,
                        QUND_PROD_PROVV               ,
                        VREC_PPRIO                    ,
                        CJUNC_DEPDC                   ,
                        CMUN_RURAL                    ,
                        CORIGE_REC                    ,
                        CPRODT_RURAL                  ,
                        CESPCE_TITLO_GARNT            ,
                        CTPO_PROTR_RURAL              ,
                        CREFT_BACEN                   ,
                        DANO_BASE_NEGOC               ,
                        CDEPDC_NEGOC                  ,
                        CSEQ_STUDO_NEGOC              ,
                        CTPO_AGROP_RURAL              ,
                        CTPO_INTGC_RURAL              ,
                        CINDCD_UND_PRDTV              ,
                        CTPO_IGCAO_RURAL              ,
                        CTPO_CTIVO_RURAL              ,
                        CCICLO_PROD_RURAL             ,
                        CREFT_RECOR                   ,
                        VRECTA_BRUTA_EMPTO            ,
WP1021                  CTPO_SOLO_AGCLT               ,
WP1021                  CCICLO_CTVDA                  ,
WP1021                  CMANUT_SOLO                   ,
WP0522                  CSCRET_TSORO_NACIO            ,
PRI499                  CCLIMT_RURAL_BACEN            ,
BVF001                  PREDC_JURO_AMBTL              ,
STE001                  CINDCD_CONFM_FLORS            ,
STE001                  CINDCD_CONFM_IBAMA            ,
STE001                  CINDCD_CONFM_CSVAO            ,
STE001                  RREGLZ_IBAMA
                  INTO
                       :RUFIB002.CCONTR               ,
                       :RUFIB002.CCDULA               ,
                       :RUFIB002.DCDULA               ,
                       :RUFIB002.MAREA-TOT            ,
                       :RUFIB002.DVCTO-CONTR          ,
                       :RUFIB002.VFINAN               ,
                       :RUFIB002.QUND-FINCD-RURAL     ,
                       :RUFIB002.QUND-PROD-PROVV      ,
                       :RUFIB002.VREC-PPRIO           ,
                       :RUFIB002.CJUNC-DEPDC          ,
                       :RUFIB002.CMUN-RURAL
                         :WRK-B002-CMUN-RURAL         ,
                       :RUFIB002.CORIGE-REC
                         :WRK-B002-CORIGE-REC         ,
                       :RUFIB002.CPRODT-RURAL
                         :WRK-B002-CPRODT-RURAL       ,
                       :RUFIB002.CESPCE-TITLO-GARNT
                         :WRK-B002-CESPCE-TITLO-GARNT ,
                       :RUFIB002.CTPO-PROTR-RURAL
                         :WRK-B002-CTPO-PROTR-RURAL   ,
                       :RUFIB002.CREFT-BACEN          ,
                       :RUFIB002.DANO-BASE-NEGOC      ,
                       :RUFIB002.CDEPDC-NEGOC         ,
                       :RUFIB002.CSEQ-STUDO-NEGOC     ,
                       :RUFIB002.CTPO-AGROP-RURAL     ,
                       :RUFIB002.CTPO-INTGC-RURAL     ,
                       :RUFIB002.CINDCD-UND-PRDTV     ,
                       :RUFIB002.CTPO-IGCAO-RURAL     ,
                       :RUFIB002.CTPO-CTIVO-RURAL     ,
                       :RUFIB002.CCICLO-PROD-RURAL    ,
                       :RUFIB002.CREFT-RECOR          ,
                       :RUFIB002.VRECTA-BRUTA-EMPTO
                         :WRK-B002-VRECTA-BRUTA-EMPTO,
WP1021                 :RUFIB002.CTPO-SOLO-AGCLT
WP1021                   :WRK-CTPO-SOLO-AGCLT-NULL-02     ,
WP1021                 :RUFIB002.CCICLO-CTVDA
WP1021                   :WRK-CCICLO-CTVDA-NULL-02        ,
WP1021                 :RUFIB002.CMANUT-SOLO
WP1021                   :WRK-CMANUT-SOLO-NULL-02         ,
WP0522                 :RUFIB002.CSCRET-TSORO-NACIO
WP0522                   :WRK-CSCRET-TSORO-NULL-02        ,
PRI499                 :RUFIB002.CCLIMT-RURAL-BACEN       ,
BVF001                  :RUFIB002.PREDC-JURO-AMBTL
BVF001                       :WRK-PREDC-JURO-AMBTL-N      ,
STE001                 :RUFIB002.CINDCD-CONFM-FLORS
STE001                       :WRK-CINDCD-CONFM-FLORS-N    ,
STE001                 :RUFIB002.CINDCD-CONFM-IBAMA
STE001                       :WRK-CINDCD-CONFM-IBAMA-N    ,
STE001                 :RUFIB002.CINDCD-CONFM-CSVAO
STE001                       :WRK-CINDCD-CONFM-CSVAO-N    ,
STE001                 :RUFIB002.RREGLZ-IBAMA
STE001                       :WRK-RREGLZ-IBAMA-N
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CCONTR  = :RUFIB002.CCONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  07                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-07        TO  COMU-8807-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B002-CMUN-RURAL     EQUAL  -1
               MOVE  ZEROS             TO  CMUN-RURAL       OF RUFIB002
           END-IF.

           IF  WRK-B002-CORIGE-REC     EQUAL  -1
               MOVE  ZEROS             TO  CORIGE-REC       OF RUFIB002
           END-IF.

           IF  WRK-B002-CPRODT-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CPRODT-RURAL     OF RUFIB002
           END-IF.

           IF  WRK-B002-CESPCE-TITLO-GARNT EQUAL  -1
               MOVE  ZEROS             TO  CESPCE-TITLO-GARNT
                                                            OF RUFIB002
           END-IF.

           IF  WRK-B002-CTPO-PROTR-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CTPO-PROTR-RURAL OF RUFIB002
           END-IF.

           IF  WRK-B002-VRECTA-BRUTA-EMPTO
                                       EQUAL  -1
               MOVE  ZEROS             TO  VRECTA-BRUTA-EMPTO
                                                            OF RUFIB002
           END-IF.

WP1021     IF  WRK-CTPO-SOLO-AGCLT-NULL-02   EQUAL  -1
WP1021         MOVE  ZEROS             TO CTPO-SOLO-AGCLT    OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CCICLO-CTVDA-NULL-02      EQUAL  -1
WP1021         MOVE  ZEROS             TO CCICLO-CTVDA       OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CMANUT-SOLO-NULL-02       EQUAL  -1
WP1021         MOVE  ZEROS             TO CMANUT-SOLO        OF RUFIB002
WP1021     END-IF.
WP0522
WP0522     IF  WRK-CSCRET-TSORO-NULL-02      EQUAL  -1
WP0522         MOVE  ZEROS             TO CSCRET-TSORO-NACIO OF RUFIB002
WP0522     END-IF.
WP0522
BVF001     IF (WRK-PREDC-JURO-AMBTL-N  LESS ZEROS)
BVF001        MOVE ZEROS               TO PREDC-JURO-AMBTL
BVF001                                 OF RUFIB002
BVF001     END-IF.
STE001     IF  WRK-CINDCD-CONFM-FLORS-N      EQUAL  -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-FLORS OF RUFIB002
STE001     END-IF.
STE001     IF  WRK-CINDCD-CONFM-IBAMA-N      EQUAL  -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-IBAMA OF RUFIB002
STE001     END-IF.
STE001     IF  WRK-CINDCD-CONFM-CSVAO-N      EQUAL  -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-CSVAO OF RUFIB002
STE001     END-IF.
STE001     IF  WRK-RREGLZ-IBAMA-N            EQUAL  -1
STE001         MOVE  ZEROS             TO RREGLZ-IBAMA-LEN   OF RUFIB002
STE001         MOVE  SPACES            TO RREGLZ-IBAMA-TEXT  OF RUFIB002
STE001     END-IF.

      *----------------------------------------------------------------*
       5030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5035-ATUALIZAR-CONTRATO         SECTION.
      *----------------------------------------------------------------*

BVF001     PERFORM  4150-PROCURAR-CONTR-MANUT
           PERFORM  5040-MOVER-CPO-RUFIB002
           PERFORM  5045-UPDATE-RUFIB002
STE002     PERFORM  5046-UPDATE-RUECV007.

      *----------------------------------------------------------------*
       5035-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5040-MOVER-CPO-RUFIB002         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-B015-MAREA-TOT            EQUAL  -1
               CONTINUE
           ELSE
               MOVE  MAREA-TOT         OF RUCAB015
                                       TO MAREA-TOT          OF RUFIB002
           END-IF.

           IF  DVCTO-CONTR  OF RUCAB015  EQUAL  '01.01.0001'
               CONTINUE
           ELSE
               MOVE  DVCTO-CONTR       OF RUCAB015
                                       TO DVCTO-CONTR        OF RUFIB002
           END-IF.

           MOVE  VFINAN                OF RUCAB015
                                       TO VFINAN             OF RUFIB002

           IF  WRK-B015-QUND-FINCD-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  QUND-FINCD-RURAL  OF RUCAB015
                                       TO QUND-FINCD-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B015-QUND-PROD-PROVV      EQUAL  -1
               CONTINUE
           ELSE
               MOVE  QUND-PROD-PROVV   OF RUCAB015
                                       TO QUND-PROD-PROVV    OF RUFIB002
           END-IF.

           IF  WRK-B015-CMUN-RURAL           EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CMUN-RURAL        OF RUCAB015
                                       TO CMUN-RURAL         OF RUFIB002
           END-IF.

           IF  WRK-B015-CORIGE-REC           EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CORIGE-REC        OF RUCAB015
                                       TO CORIGE-REC         OF RUFIB002
           END-IF.

           IF  WRK-B015-CPRODT-RURAL         EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CPRODT-RURAL      OF RUCAB015
                                       TO CPRODT-RURAL       OF RUFIB002
           END-IF.

           IF  WRK-B015-CESPCE-TITLO-GARNT   EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CESPCE-TITLO-GARNT OF RUCAB015
                                       TO CESPCE-TITLO-GARNT OF RUFIB002
           END-IF.

           IF  WRK-B015-CTPO-PROTR-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CTPO-PROTR-RURAL  OF RUCAB015
                                       TO CTPO-PROTR-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B012-CREFT-RECOR           EQUAL  -1
               CONTINUE
           ELSE
              MOVE  CREFT-RECOR        OF RUCAB012
                                       TO CREFT-RECOR        OF RUFIB002
                                          WRK-CREFT-RECOR
              MOVE  WRK-CREFT-BACEN    TO CREFT-BACEN        OF RUFIB002
           END-IF.

           IF ( WRK-B015-DANO-BASE           EQUAL  -1     OR
                WRK-B015-CJUNC-DEPDC         EQUAL  -1     OR
                WRK-B015-CSEQ-STUDO-NEGOC    EQUAL  -1        ) OR
              ( DANO-BASE        OF RUCAB015 EQUAL  4130   AND
                CJUNC-DEPDC      OF RUCAB015 EQUAL  04130  AND
                CSEQ-STUDO-NEGOC OF RUCAB015 EQUAL  000000001 )
               CONTINUE
           ELSE
               MOVE  DANO-BASE         OF RUCAB015
                                       TO DANO-BASE-NEGOC    OF RUFIB002
               MOVE  CJUNC-DEPDC       OF RUCAB015
                                       TO CDEPDC-NEGOC       OF RUFIB002
               MOVE  CSEQ-STUDO-NEGOC  OF RUCAB015
                                       TO CSEQ-STUDO-NEGOC   OF RUFIB002
           END-IF.

           IF  WRK-B015-CTPO-AGROP-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CTPO-AGROP-RURAL  OF RUCAB015
                                       TO CTPO-AGROP-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B015-CTPO-INTGC-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CTPO-INTGC-RURAL  OF RUCAB015
                                       TO CTPO-INTGC-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B015-CINDCD-UND-PRDTV     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CINDCD-UND-PRDTV  OF RUCAB015
                                       TO CINDCD-UND-PRDTV   OF RUFIB002
           END-IF.

           IF  WRK-B015-CTPO-IGCAO-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CTPO-IGCAO-RURAL  OF RUCAB015
                                       TO CTPO-IGCAO-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B015-CTPO-CTIVO-RURAL     EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CTPO-CTIVO-RURAL  OF RUCAB015
                                       TO CTPO-CTIVO-RURAL   OF RUFIB002
           END-IF.

           IF  WRK-B015-CCICLO-PROD-RURAL    EQUAL  -1
               CONTINUE
           ELSE
               MOVE  CCICLO-PROD-RURAL OF RUCAB015
                                       TO CCICLO-PROD-RURAL  OF RUFIB002
           END-IF.

PRI499     IF  WRK-B015-CCLIMT-RURAL-BACEN   EQUAL  -1
PRI499         CONTINUE
PRI499     ELSE
PRI499         MOVE  CCLIMT-RURAL-BACEN OF RUCAB015
PRI499                                 TO CCLIMT-RURAL-BACEN OF RUFIB002
PRI499     END-IF.

           IF  WRK-B015-VRECTA-BRUTA-EMPTO   EQUAL  -1
               CONTINUE
           ELSE
               MOVE  VRECTA-BRUTA-EMPTO OF RUCAB015
                                       TO VRECTA-BRUTA-EMPTO OF RUFIB002
           END-IF.

WP1021     IF  WRK-CTPO-SOLO-AGCLT-NULL-15   EQUAL  -1
WP1021         CONTINUE
WP1021     ELSE
WP1021         MOVE  ZEROS             TO WRK-CTPO-SOLO-AGCLT-NULL-02
WP1021         MOVE  CTPO-SOLO-AGCLT   OF RUCAB015
WP1021                                 TO CTPO-SOLO-AGCLT    OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CCICLO-CTVDA-NULL-15      EQUAL  -1
WP1021         CONTINUE
WP1021     ELSE
WP1021         MOVE  ZEROS             TO WRK-CCICLO-CTVDA-NULL-02
WP1021         MOVE  CCICLO-CTVDA      OF RUCAB015
WP1021                                 TO CCICLO-CTVDA       OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CMANUT-SOLO-NULL-15       EQUAL  -1
WP1021         CONTINUE
WP1021     ELSE
WP1021         MOVE  ZEROS             TO WRK-CMANUT-SOLO-NULL-02
WP1021         MOVE  CMANUT-SOLO       OF RUCAB015
WP1021                                 TO CMANUT-SOLO        OF RUFIB002
WP1021     END-IF.
WP0522
WP0522     IF  WRK-CSCRET-TSORO-NULL-15      EQUAL  -1
WP0522         CONTINUE
WP0522     ELSE
WP0522         MOVE  ZEROS             TO WRK-CSCRET-TSORO-NULL-02
WP0522         MOVE  CSCRET-TSORO-NACIO OF RUCAB015
WP0522                                 TO CSCRET-TSORO-NACIO OF RUFIB002
WP0522     END-IF.
WP0522
STE002     MOVE COUTRO-TRATO-SOLO      OF RUCAB015
.                                      TO COUTRO-TRATO-SOLO
.                                      OF RUECV007
.
.          MOVE CINDCD-CONFM-FLORS     OF RUCAB015
.                                      TO CINDCD-CONFM-FLORS
.                                      OF RUFIB002
.
.          MOVE CINDCD-CONFM-IBAMA     OF RUCAB015
.                                      TO CINDCD-CONFM-IBAMA
.                                      OF RUFIB002
.
.          MOVE CINDCD-CONFM-CSVAO     OF RUCAB015
.                                      TO CINDCD-CONFM-CSVAO
.                                      OF RUFIB002
.
.          MOVE RREGLZ-IBAMA           OF RUCAB015
.                                      TO RREGLZ-IBAMA
.                                      OF RUFIB002
.
STEFAB     MOVE CCNPJ-INSTC-EXIGB      OF RUCAB015
STEFAB                                 TO CCNPJ-INSTC-EXIGB
STEFAB                                 OF RUFIB002
STEFAB     MOVE CFLIAL-CNPJ-EXIGB      OF RUCAB015
STEFAB                                 TO CFLIAL-CNPJ-EXIGB
STEFAB                                 OF RUFIB002
STEFAB     MOVE CCTRL-CNPJ-EXIGB       OF RUCAB015
STEFAB                                 TO CCTRL-CNPJ-EXIGB
STEFAB                                 OF RUFIB002
STEFAB     MOVE COUTRO-TPO-CONFM       OF RUCAB015
STEFAB                                 TO COUTRO-TPO-CONFM
STEFAB                                 OF RUFIB002
STEFAB     MOVE CINDCD-CONFM-RURAL     OF RUCAB015
STEFAB                                 TO CINDCD-CONFM-RURAL
STEFAB                                 OF RUFIB002
STEFA1     MOVE QUND-ANMAL             OF RUCAB015
STEFA1                                 TO QUND-ANMAL
STEFA1                                 OF RUFIB002
.
STEFAB     IF  CCNPJ-INSTC-EXIGB       OF RUCAB015
STEFAB                                 EQUAL ZEROS AND
STEFAB         CFLIAL-CNPJ-EXIGB       OF RUCAB015
STEFAB                                 EQUAL ZEROS AND
STEFAB         CCTRL-CNPJ-EXIGB        OF RUCAB015
STEFAB                                 EQUAL ZEROS
STEFAB         MOVE -1                 TO WRK-CNPJ-N
STEFAB     END-IF
.
STEFAB     IF  COUTRO-TPO-CONFM        OF RUCAB015
STEFAB                                 EQUAL ZEROS
STEFAB         MOVE -1                 TO WRK-COUTRO-N
STEFAB     END-IF
.
STEFAB     IF  CINDCD-CONFM-RURAL      OF RUCAB015
STEFAB                                 EQUAL ZEROS
STEFAB         MOVE -1                 TO WRK-RURAL-N
STEFAB     END-IF
.
STEFA1     IF  QUND-ANMAL              OF RUCAB015
STEFA1                                 EQUAL ZEROS
STEFA1         MOVE -1                 TO WRK-QUND-ANMAL-N
STEFA1     ELSE
STEFA1         MOVE ZEROS              TO WRK-QUND-ANMAL-N
STEFA1     END-IF
.
.          MOVE ZEROS                  TO WRK-CINDCD-CONFM-FLORS-N
.                                         WRK-CINDCD-CONFM-IBAMA-N
.                                         WRK-CINDCD-CONFM-CSVAO-N
.                                         WRK-RREGLZ-IBAMA-N.
STE002
      *----------------------------------------------------------------*
       5040-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5045-UPDATE-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              UPDATE  DB2PRD.CONTRATO_RURAL

               SET MAREA_TOT          = :RUFIB002.MAREA-TOT            ,
                   DVCTO_CONTR        = :RUFIB002.DVCTO-CONTR          ,
                   VFINAN             = :RUFIB002.VFINAN               ,
                   QUND_FINCD_RURAL   = :RUFIB002.QUND-FINCD-RURAL     ,
                   QUND_PROD_PROVV    = :RUFIB002.QUND-PROD-PROVV      ,
                   CMUN_RURAL         = :RUFIB002.CMUN-RURAL
                                          :WRK-B002-CMUN-RURAL         ,
                   CORIGE_REC         = :RUFIB002.CORIGE-REC
                                          :WRK-B002-CORIGE-REC         ,
                   CPRODT_RURAL       = :RUFIB002.CPRODT-RURAL
                                          :WRK-B002-CPRODT-RURAL       ,
                   CESPCE_TITLO_GARNT = :RUFIB002.CESPCE-TITLO-GARNT
                                          :WRK-B002-CESPCE-TITLO-GARNT ,
                   CTPO_PROTR_RURAL   = :RUFIB002.CTPO-PROTR-RURAL
                                          :WRK-B002-CTPO-PROTR-RURAL   ,
                   CREFT_BACEN        = :RUFIB002.CREFT-BACEN          ,
                   DANO_BASE_NEGOC    = :RUFIB002.DANO-BASE-NEGOC      ,
                   CDEPDC_NEGOC       = :RUFIB002.CDEPDC-NEGOC         ,
                   CSEQ_STUDO_NEGOC   = :RUFIB002.CSEQ-STUDO-NEGOC     ,
                   CTPO_AGROP_RURAL   = :RUFIB002.CTPO-AGROP-RURAL     ,
                   CTPO_INTGC_RURAL   = :RUFIB002.CTPO-INTGC-RURAL     ,
                   CINDCD_UND_PRDTV   = :RUFIB002.CINDCD-UND-PRDTV     ,
                   CTPO_IGCAO_RURAL   = :RUFIB002.CTPO-IGCAO-RURAL     ,
                   CTPO_CTIVO_RURAL   = :RUFIB002.CTPO-CTIVO-RURAL     ,
                   CCICLO_PROD_RURAL  = :RUFIB002.CCICLO-PROD-RURAL    ,
                   CREFT_RECOR        = :RUFIB002.CREFT-RECOR          ,
                   VRECTA_BRUTA_EMPTO = :RUFIB002.VRECTA-BRUTA-EMPTO
                                          :WRK-B002-VRECTA-BRUTA-EMPTO ,
WP1021             CTPO_SOLO_AGCLT    = :RUFIB002.CTPO-SOLO-AGCLT
WP1021                                    :WRK-CTPO-SOLO-AGCLT-NULL-15 ,
WP1021             CCICLO_CTVDA       = :RUFIB002.CCICLO-CTVDA
WP1021                                    :WRK-CCICLO-CTVDA-NULL-15    ,
WP1021             CMANUT_SOLO        = :RUFIB002.CMANUT-SOLO
WP1021                                    :WRK-CMANUT-SOLO-NULL-15     ,
WP0522             CSCRET_TSORO_NACIO = :RUFIB002.CSCRET-TSORO-NACIO
                                          :WRK-CSCRET-TSORO-NULL-15    ,
PRI499             CCLIMT_RURAL_BACEN = :RUFIB002.CCLIMT-RURAL-BACEN   ,
BVF001             PREDC_JURO_AMBTL   = :RUFIB002.PREDC-JURO-AMBTL
BVF001                                    :WRK-PREDC-JURO-AMBTL-N,
STE001             CINDCD_CONFM_FLORS = :RUFIB002.CINDCD-CONFM-FLORS
STE001                                     :WRK-CINDCD-CONFM-FLORS-N,
STE001             CINDCD_CONFM_IBAMA = :RUFIB002.CINDCD-CONFM-IBAMA
STE001                                     :WRK-CINDCD-CONFM-IBAMA-N,
STE001             CINDCD_CONFM_CSVAO = :RUFIB002.CINDCD-CONFM-CSVAO
STE001                                     :WRK-CINDCD-CONFM-CSVAO-N,
STE001             RREGLZ_IBAMA       = :RUFIB002.RREGLZ-IBAMA
STE001                                     :WRK-RREGLZ-IBAMA-N,
STEFAB             CCNPJ_INSTC_EXIGB  = :RUFIB002.CCNPJ-INSTC-EXIGB
STEFAB                                     :WRK-CNPJ-N,
STEFAB             CFLIAL_CNPJ_EXIGB  = :RUFIB002.CFLIAL-CNPJ-EXIGB
STEFAB                                     :WRK-CNPJ-N,
STEFAB             CCTRL_CNPJ_EXIGB   = :RUFIB002.CCTRL-CNPJ-EXIGB
STEFAB                                     :WRK-CNPJ-N,
STEFAB             CINDCD_CONFM_RURAL = :RUFIB002.CINDCD-CONFM-RURAL
STEFAB                                     :WRK-RURAL-N,
STEFAB             COUTRO_TPO_CONFM   = :RUFIB002.COUTRO-TPO-CONFM
STEFAB                                     :WRK-COUTRO-N,
STEFA1             QUND_ANMAL         = :RUFIB002.QUND-ANMAL
STEFA1                                     :WRK-QUND-ANMAL-N
             WHERE CCONTR  = :RUFIB002.CCONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0008'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5045-99-FIM.  EXIT.
      *----------------------------------------------------------------*

STE002*----------------------------------------------------------------*
.      5046-UPDATE-RUECV007            SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE CCDULA                 OF RUFIB002
.                                      TO CCDULA     OF RUECV007
.          MOVE CJUNC-DEPDC            OF RUFIB002
.                                      TO CJUNC-DEPDC
.                                      OF RUECV007
.          EXEC SQL
.            UPDATE  DB2PRD.V01CEDULA_TEMPR
.              SET
.                   COUTRO_TRATO_SOLO = :RUECV007.COUTRO-TRATO-SOLO
.            WHERE
.                  CJUNC_DEPDC        = :RUECV007.CJUNC-DEPDC AND
.                  CCDULA             = :RUECV007.CCDULA
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'             )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'CEDULA_TEMPR'     TO  ERR-DBD-TAB
.              MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
.              MOVE SQLCODE            TO  ERR-SQL-CODE
.              MOVE '0009'             TO  ERR-LOCAL
.              MOVE SPACES             TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5046-99-FIM.  EXIT.
STE002*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5050-ATUALIZAR-PLATI-COLH       SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-ACHOU-PLANTIO
                                           WRK-ACHOU-COLHEITA

           PERFORM  5080-OPEN-CSR01-RUECB025
           PERFORM  5085-FETCH-CSR01-RUECB025

           PERFORM
             UNTIL ( WRK-ACHOU-PLANTIO   EQUAL  'S'
               AND   WRK-ACHOU-COLHEITA  EQUAL  'S' )
                OR   SQLCODE             EQUAL  +100

                PERFORM  5055-PROCURAR-PLANTIO-COLHEITA
                PERFORM  5085-FETCH-CSR01-RUECB025

           END-PERFORM.

           PERFORM  5090-CLOSE-CSR01-RUECB025

           IF  WRK-ACHOU-PLANTIO    EQUAL  'N'  AND
               WRK-B015-DINIC-PLATI EQUAL  ZEROS
               PERFORM  5070-INCLUIR-PLANTIO
           END-IF.

           IF  WRK-ACHOU-COLHEITA   EQUAL  'N'  AND
               WRK-B015-DINIC-COLHE EQUAL  ZEROS
               PERFORM  5075-INCLUIR-COLHEITA
           END-IF.

      *----------------------------------------------------------------*
       5050-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5055-PROCURAR-PLANTIO-COLHEITA  SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-CHAR              FROM  1  BY 1
             UNTIL  IND-CHAR           GREATER  34
                OR  WRK-B015-DINIC-PLATI EQUAL  -1
                OR  WRK-ACHOU-PLANTIO    EQUAL  'S'

                IF  RTPO-OPER-ORCAM OF RUECB025 (IND-CHAR:07)
                                       EQUAL 'PLANTIO'
                    PERFORM  5060-ATUALIZAR-PLANTIO

                    MOVE  'S'          TO  WRK-ACHOU-PLANTIO
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-CHAR              FROM  1  BY 1
             UNTIL  IND-CHAR           GREATER  33
                OR  WRK-B015-DINIC-COLHE EQUAL  -1
                OR  WRK-ACHOU-COLHEITA   EQUAL  'S'

                IF  RTPO-OPER-ORCAM OF RUECB025 (IND-CHAR:08)
                                       EQUAL 'COLHEITA'
                    PERFORM  5065-ATUALIZAR-COLHEITA

                    MOVE  'S'          TO  WRK-ACHOU-COLHEITA
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       5055-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5060-ATUALIZAR-PLANTIO          SECTION.
      *----------------------------------------------------------------*

           IF  DINIC-PLATI OF RUCAB015  EQUAL  '01.01.0001'
               PERFORM  5095-DELETE-RUECB025
           ELSE
               MOVE  DINIC-PLATI       OF  RUCAB015
                                       TO  DLIBRC-OPER-ORCAM OF RUECB025
               MOVE  ZEROS             TO  WRK-B025-DLIBRC-OPER-ORCAM

               IF  WRK-B015-DFIM-PLATI  EQUAL  -1
                   MOVE  SPACES        TO  DFIM-EVNTO-CDULA  OF RUECB025
                   MOVE  -1            TO  WRK-B025-DFIM-EVNTO-CDULA
               ELSE
                   MOVE  DFIM-PLATI    OF  RUCAB015
                                       TO  DFIM-EVNTO-CDULA  OF RUECB025
                   MOVE  ZEROS         TO  WRK-B025-DFIM-EVNTO-CDULA
               END-IF

               PERFORM  5100-UPDATE-RUECB025
           END-IF.

      *----------------------------------------------------------------*
       5060-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5065-ATUALIZAR-COLHEITA         SECTION.
      *----------------------------------------------------------------*

           IF  DINIC-COLHE OF RUCAB015 EQUAL  '01.01.0001'
               PERFORM  5095-DELETE-RUECB025
           ELSE
               MOVE  DINIC-COLHE       OF  RUCAB015
                                       TO  DLIBRC-OPER-ORCAM OF RUECB025
               MOVE  ZEROS             TO  WRK-B025-DLIBRC-OPER-ORCAM

               IF  WRK-B015-DFIM-COLHE  EQUAL  -1
                   MOVE  SPACES        TO  DFIM-EVNTO-CDULA  OF RUECB025
                   MOVE  -1            TO  WRK-B025-DFIM-EVNTO-CDULA
               ELSE
                   MOVE  DFIM-COLHE    OF  RUCAB015
                                       TO  DFIM-EVNTO-CDULA  OF RUECB025
                   MOVE  ZEROS         TO  WRK-B025-DFIM-EVNTO-CDULA
               END-IF

               PERFORM  5100-UPDATE-RUECB025
           END-IF.

      *----------------------------------------------------------------*
       5065-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5070-INCLUIR-PLANTIO            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5110-SELECT-MAX-RUECB025

           MOVE  WRK-CPCELA-LIBRC      TO  CPCELA-LIBRC      OF RUECB025
           MOVE  'PLANTIO'             TO  RTPO-OPER-ORCAM   OF RUECB025
           MOVE  3                     TO  CTPO-LIBRC-ORCAM  OF RUECB025

           MOVE  DINIC-PLATI           OF  RUCAB015
                                       TO  DLIBRC-OPER-ORCAM OF RUECB025
           MOVE  ZEROS                 TO  WRK-B025-DLIBRC-OPER-ORCAM

           IF  WRK-B015-DFIM-PLATI  EQUAL  -1
               MOVE  SPACES            TO  DFIM-EVNTO-CDULA  OF RUECB025
               MOVE  -1                TO  WRK-B025-DFIM-EVNTO-CDULA
           ELSE
               MOVE  DFIM-PLATI        OF  RUCAB015
                                       TO  DFIM-EVNTO-CDULA  OF RUECB025
               MOVE  ZEROS             TO  WRK-B025-DFIM-EVNTO-CDULA
           END-IF.

           MOVE  VFINAN                OF  RUCAB015
                                       TO  VFINCD-OPER-ORCAM OF RUECB025
           MOVE  VREC-PPRIO            OF  RUFIB002
                                       TO  VPCELA-REC-PPRIO  OF RUECB025
           MOVE  ZEROS                 TO  CTPO-INCON-ORCAM  OF RUECB025
           MOVE  ZEROS                 TO  CINDCD-LIBRC-PCELA
                                                             OF RUECB025

           PERFORM  5105-INSERT-RUECB025.

      *----------------------------------------------------------------*
       5070-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5075-INCLUIR-COLHEITA           SECTION.
      *----------------------------------------------------------------*

           PERFORM  5110-SELECT-MAX-RUECB025

           MOVE  WRK-CPCELA-LIBRC      TO  CPCELA-LIBRC      OF RUECB025
           MOVE  'COLHEITA'            TO  RTPO-OPER-ORCAM   OF RUECB025
           MOVE  3                     TO  CTPO-LIBRC-ORCAM  OF RUECB025

           MOVE  DINIC-COLHE           OF  RUCAB015
                                       TO  DLIBRC-OPER-ORCAM OF RUECB025
           MOVE  ZEROS                 TO  WRK-B025-DLIBRC-OPER-ORCAM

           IF  WRK-B015-DFIM-COLHE  EQUAL  -1
               MOVE  SPACES            TO  DFIM-EVNTO-CDULA  OF RUECB025
               MOVE  -1                TO  WRK-B025-DFIM-EVNTO-CDULA
           ELSE
               MOVE  DFIM-COLHE        OF  RUCAB015
                                       TO  DFIM-EVNTO-CDULA  OF RUECB025
               MOVE  ZEROS             TO  WRK-B025-DFIM-EVNTO-CDULA
           END-IF.

           MOVE  VFINAN                OF  RUCAB015
                                       TO  VFINCD-OPER-ORCAM OF RUECB025
           MOVE  VREC-PPRIO            OF  RUFIB002
                                       TO  VPCELA-REC-PPRIO  OF RUECB025
           MOVE  ZEROS                 TO  CTPO-INCON-ORCAM  OF RUECB025
           MOVE  ZEROS                 TO  CINDCD-LIBRC-PCELA
                                                             OF RUECB025

           PERFORM  5105-INSERT-RUECB025.

      *----------------------------------------------------------------*
       5075-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5080-OPEN-CSR01-RUECB025        SECTION.
      *----------------------------------------------------------------*

           MOVE  CJUNC-DEPDC OF RUFIB002
                                       TO  CJUNC-DEPDC  OF RUECB025
           MOVE  CCDULA      OF RUFIB002
                                       TO  CCDULA       OF RUECB025
           MOVE  ZEROS                 TO  CPCELA-LIBRC OF RUECB025

           EXEC SQL
                OPEN CSR01-RUECB025
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0010'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5080-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5085-FETCH-CSR01-RUECB025       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUECB025
                 INTO  :RUECB025.CJUNC-DEPDC          ,
                       :RUECB025.CCDULA               ,
                       :RUECB025.CPCELA-LIBRC         ,
                       :RUECB025.DLIBRC-OPER-ORCAM
                         :WRK-B025-DLIBRC-OPER-ORCAM  ,
                       :RUECB025.RTPO-OPER-ORCAM      ,
                       :RUECB025.DFIM-EVNTO-CDULA
                         :WRK-B025-DFIM-EVNTO-CDULA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0011'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B025-DLIBRC-OPER-ORCAM  EQUAL  -1
                   MOVE  SPACES        TO  DLIBRC-OPER-ORCAM OF RUECB025
               END-IF

               IF  WRK-B025-DFIM-EVNTO-CDULA   EQUAL  -1
                   MOVE  SPACES        TO  DFIM-EVNTO-CDULA  OF RUECB025
               END-IF
           ELSE
               MOVE  SPACES            TO  DLIBRC-OPER-ORCAM OF RUECB025
                                           RTPO-OPER-ORCAM   OF RUECB025
                                           DFIM-EVNTO-CDULA  OF RUECB025
           END-IF.

      *----------------------------------------------------------------*
       5085-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5090-CLOSE-CSR01-RUECB025       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUECB025
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0012'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5090-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5095-DELETE-RUECB025            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              DELETE
                FROM  DB2PRD.ORCAM_CDULA_RURAL
               WHERE  CJUNC_DEPDC   = :RUECB025.CJUNC-DEPDC
                 AND  CCDULA        = :RUECB025.CCDULA
                 AND  CPCELA_LIBRC  = :RUECB025.CPCELA-LIBRC
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0013'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5095-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-UPDATE-RUECB025            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              UPDATE DB2PRD.ORCAM_CDULA_RURAL

                 SET DLIBRC_OPER_ORCAM  = :RUECB025.DLIBRC-OPER-ORCAM
                                            :WRK-B025-DLIBRC-OPER-ORCAM,
                     DFIM_EVNTO_CDULA   = :RUECB025.DFIM-EVNTO-CDULA
                                            :WRK-B025-DFIM-EVNTO-CDULA

               WHERE CJUNC_DEPDC   = :RUECB025.CJUNC-DEPDC
                 AND CCDULA        = :RUECB025.CCDULA
                 AND CPCELA_LIBRC  = :RUECB025.CPCELA-LIBRC
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0014'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5105-INSERT-RUECB025            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.ORCAM_CDULA_RURAL
                     (  CJUNC_DEPDC                  ,
                        CCDULA                       ,
                        CPCELA_LIBRC                 ,
                        RTPO_OPER_ORCAM              ,
                        CTPO_LIBRC_ORCAM             ,
                        DLIBRC_OPER_ORCAM            ,
                        VFINCD_OPER_ORCAM            ,
                        VPCELA_REC_PPRIO             ,
                        CTPO_INCON_ORCAM             ,
                        CINDCD_LIBRC_PCELA           ,
                        DFIM_EVNTO_CDULA             )
                VALUES
                     ( :RUECB025.CJUNC-DEPDC         ,
                       :RUECB025.CCDULA              ,
                       :RUECB025.CPCELA-LIBRC        ,
                       :RUECB025.RTPO-OPER-ORCAM     ,
                       :RUECB025.CTPO-LIBRC-ORCAM    ,
                       :RUECB025.DLIBRC-OPER-ORCAM
                         :WRK-B025-DLIBRC-OPER-ORCAM ,
                       :RUECB025.VFINCD-OPER-ORCAM   ,
                       :RUECB025.VPCELA-REC-PPRIO    ,
                       :RUECB025.CTPO-INCON-ORCAM    ,
                       :RUECB025.CINDCD-LIBRC-PCELA  ,
                       :RUECB025.DFIM-EVNTO-CDULA
                         :WRK-B025-DFIM-EVNTO-CDULA  )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0015'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5105-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-SELECT-MAX-RUECB025        SECTION.
      *----------------------------------------------------------------*

           MOVE  CJUNC-DEPDC OF RUFIB002
                                       TO  CJUNC-DEPDC      OF RUECB025
           MOVE  CCDULA      OF RUFIB002
                                       TO  CCDULA           OF RUECB025

           EXEC SQL
              SELECT
                      MAX(CPCELA_LIBRC)
                INTO
                     :RUECB025.CPCELA-LIBRC
                FROM  DB2PRD.ORCAM_CDULA_RURAL
               WHERE  CJUNC_DEPDC   = :RUECB025.CJUNC-DEPDC
                 AND  CCDULA        = :RUECB025.CCDULA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -305 ) OR
              ( SQLWARN0            EQUAL  'W'                     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0016'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               MOVE  CPCELA-LIBRC      OF  RUECB025
                                       TO  ACU-CPCELA-LIBRC
               ADD   1                 TO  ACU-CPCELA-LIBRC
               MOVE  ACU-CPCELA-LIBRC  TO  WRK-CPCELA-LIBRC
           ELSE
               MOVE  1                 TO  WRK-CPCELA-LIBRC
           END-IF.

      *----------------------------------------------------------------*
       5110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5115-ATUALIZAR-TAXA             SECTION.
      *----------------------------------------------------------------*

           PERFORM  5120-OBTER-TAXA
           PERFORM  5130-MOVER-CPO-NOVA-TAXA
           PERFORM  5135-INSERT-RUCAB004

           MOVE  B003-CONTROL          OF  RUCAB003 (1:1)
                                       TO  WRK-BIN-AUX

           IF  WRK-CONTROLE-AUX  NOT LESS  WRK-CONTROLE-128
               PERFORM  5155-ATUALIZAR-CONTROLE-TAXA
           END-IF.

           PERFORM  5160-UPDATE-RUCAB003
           PERFORM  5165-MOVER-CPO-MVTO-TAXA
           PERFORM  5170-INSERT-RUCAB002.

      *----------------------------------------------------------------*
       5115-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5120-OBTER-TAXA                 SECTION.
      *----------------------------------------------------------------*

           PERFORM  5140-OPEN-CSR02-RUCAB004
           PERFORM  5145-FETCH-CSR02-RUCAB004

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

               IF  B004-DEBCRED OF RUCAB004 EQUAL '8'
                   PERFORM  5125-VERIFICAR-LCTO
               END-IF

               PERFORM  5145-FETCH-CSR02-RUCAB004
           END-PERFORM.

           PERFORM  5150-CLOSE-CSR02-RUCAB004.

           IF  WRK-IDENTDC-240      EQUAL  255
               CONTINUE
           ELSE
               ADD  1                  TO  WRK-IDENTDC-240
           END-IF.

      *----------------------------------------------------------------*
       5120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5125-VERIFICAR-LCTO             SECTION.
      *----------------------------------------------------------------*

            IF DEVNTO OF RUCAB015   EQUAL  B004-DTEVENTO OF RUCAB004 AND
               B004-CODLANC       GREATER  199                       AND
               B004-CODLANC          LESS  300                       AND
               B004-IDENTDC       GREATER  WRK-IDENTDC-X1
               MOVE  B004-IDENTDC      TO  WRK-IDENTDC-X1
            END-IF.

      *----------------------------------------------------------------*
       5125-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5130-MOVER-CPO-NOVA-TAXA        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  B004-CH-CONTR    OF RUCAB004
           MOVE  100                   TO  B004-CH-TIPO     OF RUCAB004
           ADD   1                     TO  ACU-NRO-PARC
           MOVE  ACU-NRO-PARC          TO  B004-CH-NUMERO   OF RUCAB004
           MOVE  SPACES                TO  B004-ESPECIE     OF RUCAB004
           MOVE  WRK-CONTROLE-TAXA     TO  WRK-B004-CONTROLE-1-AUX
           MOVE  ZEROS                 TO  WRK-B004-CONTROLE-2-AUX
           MOVE  ZEROS                 TO  WRK-B004-CONTROLE-3-AUX
           MOVE  WRK-B004-CONTROLE-AUX TO  B004-CONTROLE    OF RUCAB004
           MOVE  DEVNTO                OF  RUCAB015
                                       TO  B004-DTEVENTO    OF RUCAB004
           MOVE  ZEROS                 TO  WRK-B004-DTEVENTO
           MOVE  CTPO-EVNTO-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  B004-CODLANC     OF RUCAB004
           MOVE  SPACES                TO  B004-DTCONTAB    OF RUCAB004
           MOVE  -1                    TO  WRK-B004-DTCONTAB
           MOVE  SPACES                TO  B004-DTACERT     OF RUCAB004
           MOVE  -1                    TO  WRK-B004-DTACERT
           MOVE  WRK-IDENTDC-X1        TO  B004-IDENTDC     OF RUCAB004
           MOVE  '8'                   TO  B004-DEBCRED     OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRIORI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRICOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLMUTORI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLMUTCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPROORI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPROCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRMUORI   OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRMUCOR   OF RUCAB004
           MOVE  PJURO-ENCAR-FINCR     OF  RUCAB015
                                       TO  WRK-S03V05-DRESS
           MOVE  WRK-N03V02-DRESS      TO  B004-VLAUXIL     OF RUCAB004
           MOVE  ZEROS                 TO  B004-JURORIG     OF RUCAB004
           MOVE  ZEROS                 TO  B004-JURCORRI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CORRORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CORRCORRI   OF RUCAB004
           MOVE  ZEROS                 TO  B004-PROAORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-PROACORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-ASSIORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-ASSICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-LAUDORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-LAUDCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-MEDIORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-MEDICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-REMUORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-REMUCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-IOFORIG     OF RUCAB004
           MOVE  ZEROS                 TO  B004-IOFCORR     OF RUCAB004
           MOVE  ZEROS                 TO  B004-AGDEBCRED   OF RUCAB004
           MOVE  ZEROS                 TO  B004-RZDEBCRED   OF RUCAB004
           MOVE  ZEROS                 TO  B004-CTDEBCRED   OF RUCAB004
           MOVE  ZEROS                 TO  B004-DTMORA      OF RUCAB004
           MOVE  ZEROS                 TO  B004-LANCMORA    OF RUCAB004
           MOVE  ZEROS                 TO  B004-DBCRMORA    OF RUCAB004
           MOVE  ZEROS                 TO  B004-NUMMORA     OF RUCAB004
           MOVE  ZEROS                 TO  B004-CODCRS      OF RUCAB004
           MOVE  WRK-DIA-ATUAL         TO  WRK-DIA-DB2
           MOVE  '.'                   TO  WRK-PT1-DB2
           MOVE  WRK-MES-ATUAL         TO  WRK-MES-DB2
           MOVE  '.'                   TO  WRK-PT2-DB2
           MOVE  WRK-ANO-ATUAL         TO  WRK-ANO-DB2
           MOVE  WRK-DATA-DB2          TO  B004-DTACERTO    OF RUCAB004
           MOVE  ZEROS                 TO  WRK-B004-DTACERTO
           MOVE  66                    TO  B004-CODACERT    OF RUCAB004.

      *----------------------------------------------------------------*
       5130-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5135-INSERT-RUCAB004            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.RUCA_CAD_DEP

                     (  B004_CH_CONTR            ,
                        B004_CH_TIPO             ,
                        B004_CH_NUMERO           ,
                        B004_ESPECIE             ,
                        B004_CONTROLE            ,
                        B004_DTEVENTO            ,
                        B004_CODLANC             ,
                        B004_DTCONTAB            ,
                        B004_DTACERT             ,
                        B004_IDENTDC             ,
                        B004_DEBCRED             ,
                        B004_VLPRIORI            ,
                        B004_VLPRICOR            ,
                        B004_VLMUTORI            ,
                        B004_VLMUTCOR            ,
                        B004_VLPROORI            ,
                        B004_VLPROCOR            ,
                        B004_VLPRMUORI           ,
                        B004_VLPRMUCOR           ,
                        B004_VLAUXIL             ,
                        B004_JURORIG             ,
                        B004_JURCORRI            ,
                        B004_CORRORIG            ,
                        B004_CORRCORRI           ,
                        B004_PROAORIG            ,
                        B004_PROACORR            ,
                        B004_ASSIORIG            ,
                        B004_ASSICORR            ,
                        B004_LAUDORIG            ,
                        B004_LAUDCORR            ,
                        B004_MEDIORIG            ,
                        B004_MEDICORR            ,
                        B004_REMUORIG            ,
                        B004_REMUCORR            ,
                        B004_IOFORIG             ,
                        B004_IOFCORR             ,
                        B004_AGDEBCRED           ,
                        B004_RZDEBCRED           ,
                        B004_CTDEBCRED           ,
                        B004_DTMORA              ,
                        B004_LANCMORA            ,
                        B004_DBCRMORA            ,
                        B004_NUMMORA             ,
                        B004_CODCRS              ,
                        B004_DTACERTO            ,
                        B004_CODACERT            )
                VALUES
                     ( :RUCAB004.B004-CH-CONTR   ,
                       :RUCAB004.B004-CH-TIPO    ,
                       :RUCAB004.B004-CH-NUMERO  ,
                       :RUCAB004.B004-ESPECIE    ,
                       :RUCAB004.B004-CONTROLE   ,
                       :RUCAB004.B004-DTEVENTO
                         :WRK-B004-DTEVENTO      ,
                       :RUCAB004.B004-CODLANC    ,
                       :RUCAB004.B004-DTCONTAB
                         :WRK-B004-DTCONTAB      ,
                       :RUCAB004.B004-DTACERT
                         :WRK-B004-DTACERT       ,
                       :RUCAB004.B004-IDENTDC    ,
                       :RUCAB004.B004-DEBCRED    ,
                       :RUCAB004.B004-VLPRIORI   ,
                       :RUCAB004.B004-VLPRICOR   ,
                       :RUCAB004.B004-VLMUTORI   ,
                       :RUCAB004.B004-VLMUTCOR   ,
                       :RUCAB004.B004-VLPROORI   ,
                       :RUCAB004.B004-VLPROCOR   ,
                       :RUCAB004.B004-VLPRMUORI  ,
                       :RUCAB004.B004-VLPRMUCOR  ,
                       :RUCAB004.B004-VLAUXIL    ,
                       :RUCAB004.B004-JURORIG    ,
                       :RUCAB004.B004-JURCORRI   ,
                       :RUCAB004.B004-CORRORIG   ,
                       :RUCAB004.B004-CORRCORRI  ,
                       :RUCAB004.B004-PROAORIG   ,
                       :RUCAB004.B004-PROACORR   ,
                       :RUCAB004.B004-ASSIORIG   ,
                       :RUCAB004.B004-ASSICORR   ,
                       :RUCAB004.B004-LAUDORIG   ,
                       :RUCAB004.B004-LAUDCORR   ,
                       :RUCAB004.B004-MEDIORIG   ,
                       :RUCAB004.B004-MEDICORR   ,
                       :RUCAB004.B004-REMUORIG   ,
                       :RUCAB004.B004-REMUCORR   ,
                       :RUCAB004.B004-IOFORIG    ,
                       :RUCAB004.B004-IOFCORR    ,
                       :RUCAB004.B004-AGDEBCRED  ,
                       :RUCAB004.B004-RZDEBCRED  ,
                       :RUCAB004.B004-CTDEBCRED  ,
                       :RUCAB004.B004-DTMORA     ,
                       :RUCAB004.B004-LANCMORA   ,
                       :RUCAB004.B004-DBCRMORA   ,
                       :RUCAB004.B004-NUMMORA    ,
                       :RUCAB004.B004-CODCRS     ,
                       :RUCAB004.B004-DTACERTO
                         :WRK-B004-DTACERTO      ,
                       :RUCAB004.B004-CODACERT   )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0017'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5135-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5140-OPEN-CSR02-RUCAB004        SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8807-CHAVE       TO  B004-CH-CONTR    OF RUCAB004
           MOVE  100                   TO  B004-CH-TIPO     OF RUCAB004
           MOVE  ZEROS                 TO  B004-CH-NUMERO   OF RUCAB004

           EXEC SQL
                OPEN CSR02-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0018'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5140-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5145-FETCH-CSR02-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUCAB004
                 INTO  :RUCAB004.B004-CH-CONTR   ,
                       :RUCAB004.B004-CH-TIPO    ,
                       :RUCAB004.B004-CH-NUMERO  ,
                       :RUCAB004.B004-ESPECIE    ,
                       :RUCAB004.B004-CONTROLE   ,
                       :RUCAB004.B004-DTEVENTO
                         :WRK-B004-DTEVENTO      ,
                       :RUCAB004.B004-CODLANC    ,
                       :RUCAB004.B004-DTCONTAB
                         :WRK-B004-DTCONTAB      ,
                       :RUCAB004.B004-DTACERT
                         :WRK-B004-DTACERT       ,
                       :RUCAB004.B004-IDENTDC    ,
                       :RUCAB004.B004-DEBCRED    ,
                       :RUCAB004.B004-VLPRIORI   ,
                       :RUCAB004.B004-VLPRICOR   ,
                       :RUCAB004.B004-VLMUTORI   ,
                       :RUCAB004.B004-VLMUTCOR   ,
                       :RUCAB004.B004-VLPROORI   ,
                       :RUCAB004.B004-VLPROCOR   ,
                       :RUCAB004.B004-VLPRMUORI  ,
                       :RUCAB004.B004-VLPRMUCOR  ,
                       :RUCAB004.B004-VLAUXIL    ,
                       :RUCAB004.B004-JURORIG    ,
                       :RUCAB004.B004-JURCORRI   ,
                       :RUCAB004.B004-CORRORIG   ,
                       :RUCAB004.B004-CORRCORRI  ,
                       :RUCAB004.B004-PROAORIG   ,
                       :RUCAB004.B004-PROACORR   ,
                       :RUCAB004.B004-ASSIORIG   ,
                       :RUCAB004.B004-ASSICORR   ,
                       :RUCAB004.B004-LAUDORIG   ,
                       :RUCAB004.B004-LAUDCORR   ,
                       :RUCAB004.B004-MEDIORIG   ,
                       :RUCAB004.B004-MEDICORR   ,
                       :RUCAB004.B004-REMUORIG   ,
                       :RUCAB004.B004-REMUCORR   ,
                       :RUCAB004.B004-IOFORIG    ,
                       :RUCAB004.B004-IOFCORR    ,
                       :RUCAB004.B004-AGDEBCRED  ,
                       :RUCAB004.B004-RZDEBCRED  ,
                       :RUCAB004.B004-CTDEBCRED  ,
                       :RUCAB004.B004-DTMORA     ,
                       :RUCAB004.B004-LANCMORA   ,
                       :RUCAB004.B004-DBCRMORA   ,
                       :RUCAB004.B004-NUMMORA    ,
                       :RUCAB004.B004-CODCRS     ,
                       :RUCAB004.B004-DTACERTO
                         :WRK-B004-DTACERTO      ,
                       :RUCAB004.B004-CODACERT
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0019'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B004-DTEVENTO   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTEVENTO    OF RUCAB004
               END-IF

               IF  WRK-B004-DTCONTAB   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTCONTAB    OF RUCAB004
               END-IF

               IF  WRK-B004-DTACERT    EQUAL  -1
                   MOVE  SPACES        TO  B004-DTACERT     OF RUCAB004
               END-IF

               IF  WRK-B004-DTACERTO   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTACERTO    OF RUCAB004
               END-IF
           ELSE
               MOVE  SPACES            TO  B004-DTEVENTO    OF RUCAB004
                                           B004-DTCONTAB    OF RUCAB004
                                           B004-DTACERT     OF RUCAB004
                                           B004-DTACERTO    OF RUCAB004
               MOVE  ZEROS             TO  B004-CODLANC     OF RUCAB004
                                           B004-DEBCRED     OF RUCAB004
                                           B004-VLAUXIL     OF RUCAB004
           END-IF.

      *----------------------------------------------------------------*
       5145-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5150-CLOSE-CSR02-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0020'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5155-ATUALIZAR-CONTROLE-TAXA    SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  WRK-0004-CPO-ENTRADA
           MOVE  SPACES                TO  WRK-0004-CPO-SAIDA
           MOVE  SPACES                TO  WRK-0004-ENT-BYTE-1
           MOVE  SPACES                TO  WRK-0004-SAI-BYTE-1

           MOVE  B003-CONTROL          OF  RUCAB003 (1:1)
                                       TO  WRK-0004-ENT-BYTE-1
           MOVE  1                     TO  WRK-0004-TP-CONV
           MOVE  1                     TO  WRK-0004-BIT-BYTE

           CALL  'BRAD0004'         USING  WRK-0004-TP-CONV
                                           WRK-0004-ENT-BYTE-1
                                           WRK-0004-CPO-SAIDA
                                           WRK-0004-BIT-BYTE

           MOVE  WRK-0004-CPO-SAIDA    TO  WRK-0004-CPO-ENTRADA
           MOVE  '0'                   TO  WRK-0004-ENT-8-BITS(1:1)
           MOVE  '1'                   TO  WRK-0004-ENT-8-BITS(7:1)
           MOVE  '1'                   TO  WRK-0004-ENT-8-BITS(8:1)
           MOVE  2                     TO  WRK-0004-TP-CONV
           MOVE  8                     TO  WRK-0004-BIT-BYTE

           CALL  'BRAD0004'         USING  WRK-0004-TP-CONV
                                           WRK-0004-ENT-8-BITS
                                           WRK-0004-SAI-BYTE-1
                                           WRK-0004-BIT-BYTE

           MOVE  WRK-0004-SAI-BYTE-1   TO B003-CONTROL OF RUCAB003(1:1).

      *----------------------------------------------------------------*
       5155-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5160-UPDATE-RUCAB003            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  B003-CH-CONTR    OF RUCAB003
           MOVE  ACU-NRO-PARC          TO  B003-NUMDEPEN    OF RUCAB003

           EXEC SQL
                UPDATE  DB2PRD.RUCA_CAD_ROOT

                   SET  B003_CONTROL   = :RUCAB003.B003-CONTROL    ,
                        B003_NUMDEPEN  = :RUCAB003.B003-NUMDEPEN

                 WHERE  B003_CH_CONTR    = :RUCAB003.B003-CH-CONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_ROOT'    TO  ERR-DBD-TAB
               MOVE  'UPDATE'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0021'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5160-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5165-MOVER-CPO-MVTO-TAXA        SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  WRK-MVTO-B002
           MOVE  WRK-DT-JUL-ATUAL      TO  WRK-MOV-DATA
           MOVE  WRK-7600-TI-HHMMSS    TO  WRK-MOV-HHMMSS
           MOVE  CFUNC-ATULZ           OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CODFUNC
           MOVE  66                    TO  WRK-MOV-CHAVE-CARTAO
           MOVE  ZEROS                 TO  WRK-MOV-CODIFI1
                                           WRK-MOV-CODIFI2
                                           WRK-MOV-CODIFI3
                                           WRK-MOV-CODIFI4
                                           WRK-MOV-CODIFI5
           MOVE  CCONTR                OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CODIFI6
           MOVE  ZEROS                 TO  WRK-MOV-CHAVE-LIB-PAG
                                           WRK-MOV-CHAVE-PARCELA
                                           ACU-B002-PARC

           MOVE  CCONTR                OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N09-DRESS         TO  WRK-MOV-CCONTR-TAXA
           MOVE  WRK-0432-DIGITO       TO  WRK-MOV-DIGITO-TAXA
           MOVE  ZEROS                 TO  WRK-MOV-ORDEM-TAXA
           MOVE  PJURO-ENCAR-FINCR     OF  RUCAB015
                                       TO  WRK-S03V05-DRESS
           MOVE  ZEROS                 TO  WRK-MOV-TAXA-N02
           MOVE  WRK-N03V02-DRESS      TO  WRK-MOV-TAXA-N03V02
           MOVE  DEVNTO                OF  RUCAB015
                                       TO  WRK-DATA-DB2
           MOVE  WRK-DIA-DB2           TO  WRK-DIA-AUX
           MOVE  WRK-MES-DB2           TO  WRK-MES-AUX
           MOVE  WRK-ANO-DB2           TO  WRK-ANO-AUX
           MOVE  WRK-DATA-AUX-X        TO  WRK-MOV-DATA-JUROS
           MOVE  CTPO-EVNTO-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-X03-DRESS         TO  WRK-MOV-COD-JUROS
           MOVE  ZEROS                 TO  WRK-MOV-TAXA-CORRE
                                           WRK-MOV-DATA-CORRE
                                           WRK-MOV-COD-CORRE
                                           WRK-MOV-TAXA-PROAG
                                           WRK-MOV-DATA-PROAG
                                           WRK-MOV-COD-PROAG
                                           WRK-MOV-TAXA-ASSIS
                                           WRK-MOV-DATA-ASSIS
                                           WRK-MOV-COD-ASSIS
                                           WRK-MOV-TAXA-IOF
                                           WRK-MOV-DATA-IOF
                                           WRK-MOV-COD-IOF
           MOVE  SPACES                TO  WRK-MOV-CORRECAO
           MOVE  CFUNC-BDSCO-AUTRZ     OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CFUNC-TAXA
           MOVE  SPACES                TO  WRK-MOV-SENHA.

      *----------------------------------------------------------------*
       5165-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5170-INSERT-RUCAB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-MOV-DATA          TO  B002-DATA        OF RUCAB002
           MOVE  WRK-MOV-HHMMSS        TO  B002-HORA        OF RUCAB002
           MOVE  WRK-MOV-CODFUNC       TO  B002-COD-FUNC    OF RUCAB002
           MOVE  WRK-MOV-CHAVE-CARTAO  TO  B002-CARTAO      OF RUCAB002
           MOVE  WRK-MOV-CODIFICACAO   TO  B002-CODIFIC     OF RUCAB002
           MOVE  WRK-MOV-CHAVE-LIB-PAG TO  B002-LIB-PAG     OF RUCAB002
           MOVE  WRK-MOV-CHAVE-PARCELA TO  B002-PARCELA     OF RUCAB002
           MOVE  WRK-MOV-DADOS-ENTRADA TO  B002-DADOS-ENTRD OF RUCAB002

           EXEC SQL
                INSERT  INTO DB2PRD.RUCA_MOVTO
                     (  B002_DATA         ,
                        B002_HORA         ,
                        B002_COD_FUNC     ,
                        B002_CARTAO       ,
                        B002_CODIFIC      ,
                        B002_LIB_PAG      ,
                        B002_PARCELA      ,
                        B002_DADOS_ENTRD  )
                VALUES
                     ( :B002-DATA         ,
                       :B002-HORA         ,
                       :B002-COD-FUNC     ,
                       :B002-CARTAO       ,
                       :B002-CODIFIC      ,
                       :B002-LIB-PAG      ,
                       :B002-PARCELA      ,
                       :B002-DADOS-ENTRD  )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND -803 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_MOVTO'       TO  ERR-DBD-TAB
               MOVE  'INSERT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0022'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  -803  AND
               ACU-B002-PARC         LESS   999
               ADD  1                  TO  ACU-B002-PARC
               MOVE  ACU-B002-PARC     TO  WRK-S11-DRESS
               MOVE  WRK-N03-DRESS     TO  WRK-MOV-CHAVE-PARCELA
               GO                      TO  5170-INSERT-RUCAB002
           END-IF.

      *----------------------------------------------------------------*
       5170-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5175-TRATAR-PARC-PGTO           SECTION.
      *----------------------------------------------------------------*

           MOVE  6                     TO  CTPO-PCELA       OF RUCAB017

           PERFORM  5180-OPEN-CSR03-RUCAB017
           PERFORM  5185-FETCH-CSR03-RUCAB017

           PERFORM
             UNTIL  WRK-FIM-CSR03-RUCAB017   EQUAL  'S'

                PERFORM  5195-SELECT-RUCAB004

                IF  SQLCODE         EQUAL  ZEROS
                    INITIALIZE             TAB-TABELA-PARCELA
                    MOVE  ZEROS        TO  IND-PARC
                    MOVE  NPCELA-CONTR OF  RUCAB017
                                       TO  WRK-NRO-PARC-ANT

                    PERFORM
                      UNTIL  WRK-FIM-CSR03-RUCAB017   EQUAL  'S'
                         OR  NPCELA-CONTR OF RUCAB017 NOT EQUAL
                                                       WRK-NRO-PARC-ANT
                         PERFORM  5200-INCLUIR-NOVA-PARC-PGTO
                         PERFORM  5185-FETCH-CSR03-RUCAB017
                    END-PERFORM

                    PERFORM  5245-ATUALIZAR-PARC-PGTO
                ELSE
                    PERFORM  5185-FETCH-CSR03-RUCAB017
                END-IF
           END-PERFORM.

           PERFORM  5190-CLOSE-CSR03-RUCAB017.

      *----------------------------------------------------------------*
       5175-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5180-OPEN-CSR03-RUCAB017        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB017
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB017
           MOVE  ZEROS                 TO  NPCELA-CONTR     OF RUCAB017
           MOVE  ZEROS                 TO  NATULZ-PCELA     OF RUCAB017
           MOVE  'N'                   TO  WRK-FIM-CSR03-RUCAB017

           EXEC SQL
                OPEN  CSR03-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0023'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5180-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5185-FETCH-CSR03-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUCAB017
                INTO   :RUCAB017.CCONTR            ,
                       :RUCAB017.HINCL-REG         ,
                       :RUCAB017.CTPO-PCELA        ,
                       :RUCAB017.NPCELA-CONTR      ,
                       :RUCAB017.NATULZ-PCELA      ,
                       :RUCAB017.DVCTO-PCELA-CONTR ,
                       :RUCAB017.VPCELA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0024'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR03-RUCAB017
           END-IF.

      *----------------------------------------------------------------*
       5185-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5190-CLOSE-CSR03-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0025'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5190-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5195-SELECT-RUCAB004            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR   OF RUCAB017  TO  B004-CH-CONTR  OF RUCAB004
           MOVE  100                   TO  B004-CH-TIPO   OF RUCAB004
           MOVE  NPCELA-CONTR          OF  RUCAB017
                                       TO  B004-CH-NUMERO OF RUCAB004

           EXEC SQL
                SELECT  B004_CH_CONTR            ,
                        B004_CH_TIPO             ,
                        B004_CH_NUMERO           ,
                        B004_ESPECIE             ,
                        B004_CONTROLE            ,
                        B004_DTEVENTO            ,
                        B004_CODLANC             ,
                        B004_DTCONTAB            ,
                        B004_DTACERT             ,
                        B004_IDENTDC             ,
                        B004_DEBCRED             ,
                        B004_VLPRIORI            ,
                        B004_VLPRICOR            ,
                        B004_VLMUTORI            ,
                        B004_VLMUTCOR            ,
                        B004_VLPROORI            ,
                        B004_VLPROCOR            ,
                        B004_VLPRMUORI           ,
                        B004_VLPRMUCOR           ,
                        B004_VLAUXIL             ,
                        B004_JURORIG             ,
                        B004_JURCORRI            ,
                        B004_CORRORIG            ,
                        B004_CORRCORRI           ,
                        B004_PROAORIG            ,
                        B004_PROACORR            ,
                        B004_ASSIORIG            ,
                        B004_ASSICORR            ,
                        B004_LAUDORIG            ,
                        B004_LAUDCORR            ,
                        B004_MEDIORIG            ,
                        B004_MEDICORR            ,
                        B004_REMUORIG            ,
                        B004_REMUCORR            ,
                        B004_IOFORIG             ,
                        B004_IOFCORR             ,
                        B004_AGDEBCRED           ,
                        B004_RZDEBCRED           ,
                        B004_CTDEBCRED           ,
                        B004_DTMORA              ,
                        B004_LANCMORA            ,
                        B004_DBCRMORA            ,
                        B004_NUMMORA             ,
                        B004_CODCRS              ,
                        B004_DTACERTO            ,
                        B004_CODACERT
                  INTO
                       :RUCAB004.B004-CH-CONTR   ,
                       :RUCAB004.B004-CH-TIPO    ,
                       :RUCAB004.B004-CH-NUMERO  ,
                       :RUCAB004.B004-ESPECIE    ,
                       :RUCAB004.B004-CONTROLE   ,
                       :RUCAB004.B004-DTEVENTO
                         :WRK-B004-DTEVENTO      ,
                       :RUCAB004.B004-CODLANC    ,
                       :RUCAB004.B004-DTCONTAB
                         :WRK-B004-DTCONTAB      ,
                       :RUCAB004.B004-DTACERT
                         :WRK-B004-DTACERT       ,
                       :RUCAB004.B004-IDENTDC    ,
                       :RUCAB004.B004-DEBCRED    ,
                       :RUCAB004.B004-VLPRIORI   ,
                       :RUCAB004.B004-VLPRICOR   ,
                       :RUCAB004.B004-VLMUTORI   ,
                       :RUCAB004.B004-VLMUTCOR   ,
                       :RUCAB004.B004-VLPROORI   ,
                       :RUCAB004.B004-VLPROCOR   ,
                       :RUCAB004.B004-VLPRMUORI  ,
                       :RUCAB004.B004-VLPRMUCOR  ,
                       :RUCAB004.B004-VLAUXIL    ,
                       :RUCAB004.B004-JURORIG    ,
                       :RUCAB004.B004-JURCORRI   ,
                       :RUCAB004.B004-CORRORIG   ,
                       :RUCAB004.B004-CORRCORRI  ,
                       :RUCAB004.B004-PROAORIG   ,
                       :RUCAB004.B004-PROACORR   ,
                       :RUCAB004.B004-ASSIORIG   ,
                       :RUCAB004.B004-ASSICORR   ,
                       :RUCAB004.B004-LAUDORIG   ,
                       :RUCAB004.B004-LAUDCORR   ,
                       :RUCAB004.B004-MEDIORIG   ,
                       :RUCAB004.B004-MEDICORR   ,
                       :RUCAB004.B004-REMUORIG   ,
                       :RUCAB004.B004-REMUCORR   ,
                       :RUCAB004.B004-IOFORIG    ,
                       :RUCAB004.B004-IOFCORR    ,
                       :RUCAB004.B004-AGDEBCRED  ,
                       :RUCAB004.B004-RZDEBCRED  ,
                       :RUCAB004.B004-CTDEBCRED  ,
                       :RUCAB004.B004-DTMORA     ,
                       :RUCAB004.B004-LANCMORA   ,
                       :RUCAB004.B004-DBCRMORA   ,
                       :RUCAB004.B004-NUMMORA    ,
                       :RUCAB004.B004-CODCRS     ,
                       :RUCAB004.B004-DTACERTO
                         :WRK-B004-DTACERTO      ,
                       :RUCAB004.B004-CODACERT

                  FROM  DB2PRD.RUCA_CAD_DEP
                 WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                   AND  B004_CH_TIPO       = :RUCAB004.B004-CH-TIPO
                   AND  B004_CH_NUMERO     = :RUCAB004.B004-CH-NUMERO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0026'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               IF  WRK-B004-DTEVENTO   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTEVENTO    OF RUCAB004
               END-IF

               IF  WRK-B004-DTCONTAB   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTCONTAB    OF RUCAB004
               END-IF

               IF  WRK-B004-DTACERT    EQUAL  -1
                   MOVE  SPACES        TO  B004-DTACERT     OF RUCAB004
               END-IF

               IF  WRK-B004-DTACERTO   EQUAL  -1
                   MOVE  SPACES        TO  B004-DTACERTO    OF RUCAB004
               END-IF
           ELSE
               MOVE  RUCAB004          TO  WRK-LINHA-RUCAB004
           END-IF.

      *----------------------------------------------------------------*
       5195-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-INCLUIR-NOVA-PARC-PGTO     SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-LINHA-RUCAB004    TO  RUCAB004

           PERFORM  5205-ATUALIZA-CTRL-NOVA-PARC
           PERFORM  5210-MOVER-CPO-NOVA-PGTO
           PERFORM  5135-INSERT-RUCAB004
           PERFORM  5215-MOVER-CPO-MVTO-PARC
           PERFORM  5170-INSERT-RUCAB002
           PERFORM  5220-INCLUIR-PRORROG
           PERFORM  5235-CARREGAR-TABELA-PARC
           PERFORM  5240-DELETE-RUCAB017.

      *----------------------------------------------------------------*
       5200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5205-ATUALIZA-CTRL-NOVA-PARC    SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  WRK-0004-CPO-ENTRADA
           MOVE  SPACES                TO  WRK-0004-CPO-SAIDA
           MOVE  SPACES                TO  WRK-0004-ENT-5-BYTE
           MOVE  SPACES                TO  WRK-0004-SAI-5-BYTE

           MOVE  B004-CONTROLE         OF  RUCAB004 (1: 5)
                                       TO  WRK-0004-ENT-5-BYTE
           MOVE  1                     TO  WRK-0004-TP-CONV
           MOVE  5                     TO  WRK-0004-BIT-BYTE

           CALL  'BRAD0004'         USING  WRK-0004-TP-CONV
                                           WRK-0004-ENT-5-BYTE
                                           WRK-0004-CPO-SAIDA
                                           WRK-0004-BIT-BYTE

           MOVE  WRK-0004-CPO-SAIDA    TO  WRK-0004-CPO-ENTRADA
           MOVE  '001100010'           TO  WRK-0004-ENT-9-BITS
           MOVE  2                     TO  WRK-0004-TP-CONV
           MOVE  40                    TO  WRK-0004-BIT-BYTE

           CALL  'BRAD0004'         USING  WRK-0004-TP-CONV
                                           WRK-0004-CPO-ENTRADA
                                           WRK-0004-SAI-5-BYTE
                                           WRK-0004-BIT-BYTE

           MOVE  WRK-0004-SAI-5-BYTE   TO  B004-CONTROLE
                                                    OF RUCAB004 (1: 5).

      *----------------------------------------------------------------*
       5205-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5210-MOVER-CPO-NOVA-PGTO        SECTION.
      *----------------------------------------------------------------*

           IF  VPCELA OF RUCAB017   EQUAL  B004-VLPRICOR    OF RUCAB004
               MOVE  B004-VLMUTCOR     OF  RUCAB004
                                       TO  ACU-VLMUTCOR
               MOVE  B004-VLPROCOR     OF  RUCAB004
                                       TO  ACU-VLPROCOR
               MOVE  B004-VLPRMUCOR    OF  RUCAB004
                                       TO  ACU-VLPRMUCOR
           ELSE
               COMPUTE  ACU-PERCENTUAL  = VPCELA            OF RUCAB017
                                        / B004-VLPRICOR     OF RUCAB004

               COMPUTE  ACU-VLMUTCOR    = B004-VLMUTCOR     OF RUCAB004
                                        * ACU-PERCENTUAL

               COMPUTE  ACU-VLPROCOR    = B004-VLPROCOR     OF RUCAB004
                                        * ACU-PERCENTUAL

               COMPUTE  ACU-VLPRMUCOR   = B004-VLPRMUCOR    OF RUCAB004
                                        * ACU-PERCENTUAL
           END-IF.

           MOVE  CCONTR    OF RUCAB015 TO  B004-CH-CONTR    OF RUCAB004
           MOVE  100                   TO  B004-CH-TIPO     OF RUCAB004
           ADD   1                     TO  ACU-NRO-PARC
           MOVE  ACU-NRO-PARC          TO  B004-CH-NUMERO   OF RUCAB004

           IF  B004-DTEVENTO OF  RUCAB004  EQUAL  SPACES
               MOVE  SPACES            TO  B004-DTACERT     OF RUCAB004
               MOVE  -1                TO  WRK-B004-DTACERT
           ELSE
               MOVE  B004-DTEVENTO     OF  RUCAB004
                                       TO  B004-DTACERT     OF RUCAB004
               MOVE  ZEROS             TO  WRK-B004-DTACERT
           END-IF.

           MOVE  DVCTO-PCELA-CONTR     OF  RUCAB017
                                       TO  B004-DTEVENTO    OF RUCAB004
                                           WRK-DVCTO-PCELA-CONTR
           MOVE  ZEROS                 TO  WRK-B004-DTEVENTO

           IF  B004-CODLANC OF RUCAB004 EQUAL ZEROS
               MOVE  15                TO  B004-CODLANC     OF RUCAB004
           END-IF.

           MOVE  SPACES                TO  B004-DTCONTAB    OF RUCAB004
           MOVE  -1                    TO  WRK-B004-DTCONTAB
           MOVE  '6'                   TO  B004-DEBCRED     OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRIORI    OF RUCAB004
           MOVE  VPCELA OF RUCAB017    TO  B004-VLPRICOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLMUTORI    OF RUCAB004
           MOVE  ACU-VLMUTCOR          TO  B004-VLMUTCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPROORI    OF RUCAB004
           MOVE  ACU-VLPROCOR          TO  B004-VLPROCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRMUORI   OF RUCAB004
           MOVE  ACU-VLPRMUCOR         TO  B004-VLPRMUCOR   OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLAUXIL     OF RUCAB004
           MOVE  ZEROS                 TO  B004-JURORIG     OF RUCAB004
           MOVE  ZEROS                 TO  B004-JURCORRI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CORRORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CORRCORRI   OF RUCAB004
           MOVE  ZEROS                 TO  B004-PROAORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-PROACORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-ASSIORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-ASSICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-LAUDORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-LAUDCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-MEDIORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-MEDICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-REMUORIG    OF RUCAB004
           MOVE  ZEROS                 TO  B004-REMUCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-IOFORIG     OF RUCAB004
           MOVE  ZEROS                 TO  B004-IOFCORR     OF RUCAB004
           MOVE  WRK-DIA-ATUAL         TO  WRK-DIA-DB2
           MOVE  '.'                   TO  WRK-PT1-DB2
           MOVE  WRK-MES-ATUAL         TO  WRK-MES-DB2
           MOVE  '.'                   TO  WRK-PT2-DB2
           MOVE  WRK-ANO-ATUAL         TO  WRK-ANO-DB2
           MOVE  WRK-DATA-DB2          TO  B004-DTACERTO    OF RUCAB004
           MOVE  ZEROS                 TO  WRK-B004-DTACERTO
           MOVE  69                    TO  B004-CODACERT    OF RUCAB004.

      *----------------------------------------------------------------*
       5210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5215-MOVER-CPO-MVTO-PARC        SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO  WRK-MVTO-B002
           MOVE  WRK-DT-JUL-ATUAL      TO  WRK-MOV-DATA
           MOVE  WRK-7600-TI-HHMMSS    TO  WRK-MOV-HHMMSS
           MOVE  CFUNC-ATULZ           OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CODFUNC
           MOVE  69                    TO  WRK-MOV-CHAVE-CARTAO
           MOVE  ZEROS                 TO  WRK-MOV-CODIFI1
                                           WRK-MOV-CODIFI2
                                           WRK-MOV-CODIFI3
                                           WRK-MOV-CODIFI4
                                           WRK-MOV-CODIFI5
           MOVE  CCONTR                OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CODIFI6
           MOVE  ZEROS                 TO  WRK-MOV-CHAVE-LIB-PAG
                                           WRK-MOV-CHAVE-PARCELA
                                           ACU-B002-PARC
           MOVE  CCONTR                OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N09-DRESS         TO  WRK-MOV-CCONTR-PARC
           MOVE  WRK-0432-DIGITO       TO  WRK-MOV-DIGITO-PARC
           MOVE  ZEROS                 TO  WRK-MOV-ORDEM-PARC
           MOVE  NPCELA-CONTR          OF  RUCAB017
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  WRK-MOV-NRO-PARC
           MOVE  DVCTO-PCELA-CONTR     OF  RUCAB017
                                       TO  WRK-DATA-DB2
           MOVE  WRK-DIA-DB2           TO  WRK-MOV-DIA-PARC
           MOVE  WRK-MES-DB2           TO  WRK-MOV-MES-PARC
           MOVE  WRK-ANO-DB2           TO  WRK-MOV-ANO-PARC
           MOVE  ZEROS                 TO  WRK-MOV-SEQ-PARC
           MOVE  VPCELA                OF  RUCAB017
                                       TO  WRK-S13V99-DRESS
           MOVE  WRK-N13V99-DRESS      TO  WRK-MOV-VLR-PARC
           MOVE  CFUNC-ATULZ           OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N07-DRESS         TO  WRK-MOV-CFUNC-PARC.

      *----------------------------------------------------------------*
       5215-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5220-INCLUIR-PRORROG            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5225-SELECT-MAX-RUFIB093
           PERFORM  5005-OBTER-DATA-HORA

           MOVE  CCONTR                OF  RUCAB015
                                       TO  CCONTR            OF RUFIB093
           MOVE  NBASE-LEGAL-RENEG     OF  RUCAB015
                                       TO  NBASE-LEGAL-RENEG OF RUFIB093
           MOVE  ZEROS                 TO  WRK-B093-NBASE-LEGAL-RENEG
           MOVE  DVCTO-PCELA-CONTR     OF  RUCAB017
                                       TO  DRENEG-CONTR-RURAL
                                                             OF RUFIB093
           MOVE  VPCELA                OF  RUCAB017
                                       TO  VRENEG-CONTR-RURAL
                                                             OF RUFIB093
           MOVE  B004-DTACERT          OF  RUCAB004
                                       TO  DVCTO-RENEG-RURAL OF RUFIB093
           MOVE  ZEROS                 TO  WRK-B093-DVCTO-RENEG-RURAL
           MOVE  'RUCA8807'            TO  CUSUAR-INCL       OF RUFIB093
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG         OF RUFIB093
           MOVE  SPACES                TO  CUSUAR-MANUT      OF RUFIB093
           MOVE  -1                    TO  WRK-B093-CUSUAR-MANUT
           MOVE  SPACES                TO  HMANUT-REG        OF RUFIB093
           MOVE  -1                    TO  WRK-B093-HMANUT-REG

           PERFORM  5230-INSERT-RUFIB093.

      *----------------------------------------------------------------*
       5220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5225-SELECT-MAX-RUFIB093        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUCAB015    TO  CCONTR           OF RUFIB093
           MOVE  ZEROS                 TO  NRENEG-CONTR-RURAL
                                                            OF RUFIB093

           EXEC SQL
              SELECT
                      MAX(NRENEG_CONTR_RURAL)
                INTO
                     :RUFIB093.NRENEG-CONTR-RURAL
                FROM  DB2PRD.TRENEG_CONTR_RURAL
               WHERE  CCONTR              = :RUFIB093.CCONTR
                 AND  NRENEG_CONTR_RURAL >= :RUFIB093.NRENEG-CONTR-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -305 ) OR
              ( SQLWARN0            EQUAL  'W'                     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TRENEG_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0027'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               MOVE  NRENEG-CONTR-RURAL OF RUFIB093
                                       TO  ACU-NBASE-LEGAL-RENEG
               ADD   1                 TO  ACU-NBASE-LEGAL-RENEG
               MOVE  ACU-NBASE-LEGAL-RENEG
                                       TO  NRENEG-CONTR-RURAL
                                                            OF RUFIB093
           ELSE
               MOVE  1                 TO  NRENEG-CONTR-RURAL
                                                            OF RUFIB093
           END-IF.

      *----------------------------------------------------------------*
       5225-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5230-INSERT-RUFIB093            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.TRENEG_CONTR_RURAL
                     (  CCONTR                       ,
                        NRENEG_CONTR_RURAL           ,
                        NBASE_LEGAL_RENEG            ,
                        DRENEG_CONTR_RURAL           ,
                        VRENEG_CONTR_RURAL           ,
                        DVCTO_RENEG_RURAL            ,
                        CUSUAR_INCL                  ,
                        HINCL_REG                    ,
                        CUSUAR_MANUT                 ,
                        HMANUT_REG                   )
                VALUES
                     ( :RUFIB093.CCONTR              ,
                       :RUFIB093.NRENEG-CONTR-RURAL  ,
                       :RUFIB093.NBASE-LEGAL-RENEG
                         :WRK-B093-NBASE-LEGAL-RENEG ,
                       :RUFIB093.DRENEG-CONTR-RURAL  ,
                       :RUFIB093.VRENEG-CONTR-RURAL  ,
                       :RUFIB093.DVCTO-RENEG-RURAL
                         :WRK-B093-DVCTO-RENEG-RURAL ,
                       :RUFIB093.CUSUAR-INCL         ,
                       :RUFIB093.HINCL-REG           ,
                       :RUFIB093.CUSUAR-MANUT
                          :WRK-B093-CUSUAR-MANUT     ,
                       :RUFIB093.HMANUT-REG
                          :WRK-B093-HMANUT-REG       )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND -803 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TRENEG_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0028'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5230-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5235-CARREGAR-TABELA-PARC       SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO  IND-PARC

           MOVE  CCONTR                OF  RUCAB017
                                       TO  TAB-CCONTR        (IND-PARC)
           MOVE  HINCL-REG             OF  RUCAB017
                                       TO  TAB-HINCL-REG     (IND-PARC)
           MOVE  CTPO-PCELA            OF  RUCAB017
                                       TO  TAB-CTPO-PCELA    (IND-PARC)
           MOVE  NPCELA-CONTR          OF  RUCAB017
                                       TO  TAB-NPCELA-CONTR  (IND-PARC)
           MOVE  ACU-NRO-PARC          TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  TAB-NATULZ-PCELA  (IND-PARC)
           MOVE  DVCTO-PCELA-CONTR     OF  RUCAB017
                                       TO  TAB-DVCTO-PCELA-CONTR
                                                             (IND-PARC)
           MOVE  VPCELA                OF  RUCAB017
                                       TO  TAB-VPCELA        (IND-PARC).

      *----------------------------------------------------------------*
       5235-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5240-DELETE-RUCAB017            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                DELETE
                  FROM  DB2PRD.TMANUT_PCELA_RECOR
                 WHERE  CCONTR             = :RUCAB017.CCONTR
                   AND  HINCL_REG          = :RUCAB017.HINCL-REG
                   AND  CTPO_PCELA         = :RUCAB017.CTPO-PCELA
                   AND  NPCELA_CONTR       = :RUCAB017.NPCELA-CONTR
                   AND  NATULZ_PCELA       = :RUCAB017.NATULZ-PCELA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0029'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5240-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5245-ATUALIZAR-PARC-PGTO        SECTION.
      *----------------------------------------------------------------*

           PERFORM  5250-MOVER-CPO-ATULZ-PGTO
           PERFORM  5255-UPDATE-RUCAB004
           PERFORM  5160-UPDATE-RUCAB003

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  IND-PARC

             PERFORM  5260-MOVER-CPO-RUCAB017
             PERFORM  5265-INSERT-CPO-RUCAB017
           END-PERFORM.

      *----------------------------------------------------------------*
       5245-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5250-MOVER-CPO-ATULZ-PGTO       SECTION.
      *----------------------------------------------------------------*

           MOVE  WRK-LINHA-RUCAB004    TO  RUCAB004

           MOVE  WRK-DVCTO-PCELA-CONTR TO  B004-DTACERT     OF RUCAB004
           MOVE  ZEROS                 TO  WRK-B004-DTACERT
           MOVE  ZEROS                 TO  B004-VLPRICOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLMUTCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPROCOR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLPRMUCOR   OF RUCAB004
           MOVE  ZEROS                 TO  B004-VLAUXIL     OF RUCAB004
           MOVE  ZEROS                 TO  B004-JURCORRI    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CORRCORRI   OF RUCAB004
           MOVE  ZEROS                 TO  B004-PROACORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-ASSICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-LAUDCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-MEDICORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-REMUCORR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-IOFCORR     OF RUCAB004
           MOVE  WRK-DIA-ATUAL         TO  WRK-DIA-DB2
           MOVE  '.'                   TO  WRK-PT1-DB2
           MOVE  WRK-MES-ATUAL         TO  WRK-MES-DB2
           MOVE  '.'                   TO  WRK-PT2-DB2
           MOVE  WRK-ANO-ATUAL         TO  WRK-ANO-DB2
           MOVE  WRK-DATA-DB2          TO  B004-DTACERTO    OF RUCAB004
           MOVE  ZEROS                 TO  WRK-B004-DTACERTO
           MOVE  69                    TO  B004-CODACERT    OF RUCAB004.

      *----------------------------------------------------------------*
       5250-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5255-UPDATE-RUCAB004            SECTION.
      *----------------------------------------------------------------*

           IF  B004-DTACERT  OF RUCAB004   EQUAL  SPACES
               MOVE  -1                TO  WRK-B004-DTACERT
           END-IF.

           IF  B004-DTACERTO OF RUCAB004   EQUAL  SPACES
               MOVE  -1                TO  WRK-B004-DTACERTO
           END-IF.

           EXEC SQL
                UPDATE  DB2PRD.RUCA_CAD_DEP

                   SET  B004_DTACERT     = :RUCAB004.B004-DTACERT
                                              :WRK-B004-DTACERT     ,
                        B004_VLPRICOR    = :RUCAB004.B004-VLPRICOR  ,
                        B004_VLMUTCOR    = :RUCAB004.B004-VLMUTCOR  ,
                        B004_VLPROCOR    = :RUCAB004.B004-VLPROCOR  ,
                        B004_VLPRMUCOR   = :RUCAB004.B004-VLPRMUCOR ,
                        B004_VLAUXIL     = :RUCAB004.B004-VLAUXIL   ,
                        B004_JURCORRI    = :RUCAB004.B004-JURCORRI  ,
                        B004_CORRCORRI   = :RUCAB004.B004-CORRCORRI ,
                        B004_PROACORR    = :RUCAB004.B004-PROACORR  ,
                        B004_ASSICORR    = :RUCAB004.B004-ASSICORR  ,
                        B004_LAUDCORR    = :RUCAB004.B004-LAUDCORR  ,
                        B004_MEDICORR    = :RUCAB004.B004-MEDICORR  ,
                        B004_REMUCORR    = :RUCAB004.B004-REMUCORR  ,
                        B004_IOFCORR     = :RUCAB004.B004-IOFCORR   ,
                        B004_DTACERTO    = :RUCAB004.B004-DTACERTO
                                              :WRK-B004-DTACERTO    ,
                        B004_CODACERT    = :RUCAB004.B004-CODACERT

                WHERE   B004_CH_CONTR    = :RUCAB004.B004-CH-CONTR
                  AND   B004_CH_TIPO     = :RUCAB004.B004-CH-TIPO
                  AND   B004_CH_NUMERO   = :RUCAB004.B004-CH-NUMERO
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'UPDATE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0030'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5255-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5260-MOVER-CPO-RUCAB017         SECTION.
      *----------------------------------------------------------------*

           MOVE  TAB-CCONTR            (IND-LIN)
                                       TO  CCONTR            OF RUCAB017
           MOVE  TAB-HINCL-REG         (IND-LIN)
                                       TO  HINCL-REG         OF RUCAB017
           MOVE  TAB-CTPO-PCELA        (IND-LIN)
                                       TO  CTPO-PCELA        OF RUCAB017
           MOVE  TAB-NPCELA-CONTR      (IND-LIN)
                                       TO  NPCELA-CONTR      OF RUCAB017
           MOVE  TAB-NATULZ-PCELA      (IND-LIN)
                                       TO  NATULZ-PCELA      OF RUCAB017
           MOVE  TAB-DVCTO-PCELA-CONTR (IND-LIN)
                                       TO  DVCTO-PCELA-CONTR OF RUCAB017
           MOVE  TAB-VPCELA            (IND-LIN)
                                       TO  VPCELA           OF RUCAB017.

      *----------------------------------------------------------------*
       5260-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5265-INSERT-CPO-RUCAB017        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.TMANUT_PCELA_RECOR
                     (  CCONTR                     ,
                        HINCL_REG                  ,
                        CTPO_PCELA                 ,
                        NPCELA_CONTR               ,
                        NATULZ_PCELA               ,
                        DVCTO_PCELA_CONTR          ,
                        VPCELA                     )
                VALUES
                     ( :RUCAB017.CCONTR            ,
                       :RUCAB017.HINCL-REG         ,
                       :RUCAB017.CTPO-PCELA        ,
                       :RUCAB017.NPCELA-CONTR      ,
                       :RUCAB017.NATULZ-PCELA      ,
                       :RUCAB017.DVCTO-PCELA-CONTR ,
                       :RUCAB017.VPCELA            )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0031'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5265-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5270-TRATAR-COORDENADAS         SECTION.
      *----------------------------------------------------------------*

           PERFORM  5275-OPEN-CSR04-RUCAB016
           PERFORM  5280-FETCH-CSR04-RUCAB016

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

             PERFORM  5290-EXCLUIR-COORDENADA

             PERFORM
               UNTIL  SQLCODE       EQUAL  +100
                  OR  CCONTR           OF RUCAB016
                                NOT EQUAL CCONTR           OF RUECB09N
                  OR  CSEQ-MATR-RURAL  OF RUCAB016
                                NOT EQUAL CSEQ-MATR-RURAL  OF RUECB09N

                  IF  NSEQ-GLEBA-IMOV OF RUCAB016  GREATER  ZEROS
                      PERFORM  5335-INSERIR-COORDENADAS
                  END-IF

                  PERFORM  5280-FETCH-CSR04-RUCAB016
             END-PERFORM
           END-PERFORM.

           PERFORM  5285-CLOSE-CSR04-RUCAB016.

      *----------------------------------------------------------------*
       5270-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5275-OPEN-CSR04-RUCAB016        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB015
                                       TO  CCONTR            OF RUCAB016
           MOVE  HINCL-REG             OF  RUCAB015
                                       TO  HINCL-REG         OF RUCAB016
           MOVE  ZEROS                 TO  CSEQ-MATR-RURAL   OF RUCAB016
                                           NSEQ-GLEBA-IMOV   OF RUCAB016
                                           NCORND-GEOGR-IMOV OF RUCAB016

           EXEC SQL
                OPEN CSR04-RUCAB016
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0032'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5275-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5280-FETCH-CSR04-RUCAB016       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR04-RUCAB016
                INTO   :RUCAB016.CCONTR            ,
                       :RUCAB016.HINCL-REG         ,
                       :RUCAB016.CSEQ-MATR-RURAL   ,
                       :RUCAB016.NSEQ-GLEBA-IMOV   ,
                       :RUCAB016.NCORND-GEOGR-IMOV ,
                       :RUCAB016.MLATTD-IMOV-RURAL ,
                       :RUCAB016.MLOGTD-IMOV-RURAL ,
                       :RUCAB016.MALTTD-IMOV-RURAL ,
                       :RUCAB016.MAREA-NAO-CTVDA
                         :WRK-B016-MAREA-NAO-CTVDA ,
STEFAB                 :RUCAB016.CTPO-CORND-GEOGR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0033'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  ZEROS             TO  MAREA-NAO-CTVDA OF RUCAB016
           ELSE
               IF  WRK-B016-MAREA-NAO-CTVDA  EQUAL  -1
                   MOVE  ZEROS         TO  MAREA-NAO-CTVDA OF RUCAB016
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5280-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5285-CLOSE-CSR04-RUCAB016       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR04-RUCAB016
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0034'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5285-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5290-EXCLUIR-COORDENADA         SECTION.
      *----------------------------------------------------------------*

           PERFORM  5295-OPEN-CSR05-RUECB09N
           PERFORM  5300-FETCH-CSR05-RUECB09N

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

             PERFORM  5310-DELETE-RUECB09N
             PERFORM  5300-FETCH-CSR05-RUECB09N

           END-PERFORM.

           PERFORM  5305-CLOSE-CSR05-RUECB09N

      *----------------------------------------------------------------*

           PERFORM  5315-OPEN-CSR06-RUECB09M
           PERFORM  5320-FETCH-CSR06-RUECB09M

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

             PERFORM  5330-DELETE-RUECB09M
             PERFORM  5320-FETCH-CSR06-RUECB09M

           END-PERFORM.

           PERFORM  5325-CLOSE-CSR06-RUECB09M.

      *----------------------------------------------------------------*
       5290-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5295-OPEN-CSR05-RUECB09N        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB016
                                       TO  CCONTR            OF RUECB09N
           MOVE  CSEQ-MATR-RURAL       OF  RUCAB016
                                       TO  CSEQ-MATR-RURAL   OF RUECB09N
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUECB09N
                                           NCORND-GEOGR-IMOV OF RUECB09N

           EXEC SQL
                OPEN CSR05-RUECB09N
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0035'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5295-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5300-FETCH-CSR05-RUECB09N       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR05-RUECB09N
                INTO   :RUECB09N.CCONTR            ,
                       :RUECB09N.CSEQ-MATR-RURAL   ,
                       :RUECB09N.NSEQ-GLEBA-IMOV   ,
                       :RUECB09N.NCORND-GEOGR-IMOV ,
                       :RUECB09N.MLATTD-IMOV-RURAL ,
                       :RUECB09N.MLOGTD-IMOV-RURAL ,
                       :RUECB09N.MALTTD-IMOV-RURAL ,
                       :RUECB09N.CUSUAR-INCL       ,
                       :RUECB09N.HINCL             ,
                       :RUECB09N.CUSUAR-MANUT
                         :WRK-B09N-CUSUAR-MANUT    ,
                       :RUECB09N.HMANUT-REG
                         :WRK-B09N-HMANUT-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0036'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               CONTINUE
           ELSE
               IF  WRK-B09N-CUSUAR-MANUT  EQUAL  -1
                   MOVE  SPACES        TO  CUSUAR-MANUT     OF RUECB09N
               END-IF

               IF  WRK-B09N-HMANUT-REG    EQUAL  -1
                   MOVE  SPACES        TO  HMANUT-REG       OF RUECB09N
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5300-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5305-CLOSE-CSR05-RUECB09N       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR05-RUECB09N
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0037'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5305-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5310-DELETE-RUECB09N            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                DELETE
                 FROM  DB2PRD.TCORND_GEOGR_CONTR
                WHERE  CCONTR             = :RUECB09N.CCONTR
                  AND  CSEQ_MATR_RURAL    = :RUECB09N.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV    = :RUECB09N.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV  = :RUECB09N.NCORND-GEOGR-IMOV
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0038'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5310-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5315-OPEN-CSR06-RUECB09M        SECTION.
      *----------------------------------------------------------------*

           MOVE  CJUNC-DEPDC           OF  RUFIB002
                                       TO  CJUNC-DEPDC       OF RUECB09M
           MOVE  CCDULA                OF  RUFIB002
                                       TO  CCDULA            OF RUECB09M
           MOVE  CSEQ-MATR-RURAL       OF  RUCAB016
                                       TO  CSEQ-MATR-RURAL   OF RUECB09M
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUECB09M
                                           NCORND-GEOGR-IMOV OF RUECB09M

           EXEC SQL
                OPEN CSR06-RUECB09M
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CDULA'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0039'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5315-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5320-FETCH-CSR06-RUECB09M       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR06-RUECB09M
                INTO   :RUECB09M.CJUNC-DEPDC       ,
                       :RUECB09M.CCDULA            ,
                       :RUECB09M.CSEQ-MATR-RURAL   ,
                       :RUECB09M.NSEQ-GLEBA-IMOV   ,
                       :RUECB09M.NCORND-GEOGR-IMOV ,
                       :RUECB09M.MLATTD-IMOV-RURAL ,
                       :RUECB09M.MLOGTD-IMOV-RURAL ,
                       :RUECB09M.MALTTD-IMOV-RURAL ,
                       :RUECB09M.CUSUAR-INCL       ,
                       :RUECB09M.HINCL             ,
                       :RUECB09M.CUSUAR-MANUT
                         :WRK-B09M-CUSUAR-MANUT    ,
                       :RUECB09M.HMANUT-REG
                         :WRK-B09M-HMANUT-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CDULA'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0040'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               CONTINUE
           ELSE
               IF  WRK-B09M-CUSUAR-MANUT  EQUAL  -1
                   MOVE  SPACES        TO  CUSUAR-MANUT     OF RUECB09M
               END-IF

               IF  WRK-B09M-HMANUT-REG    EQUAL  -1
                   MOVE  SPACES        TO  HMANUT-REG       OF RUECB09M
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5320-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5325-CLOSE-CSR06-RUECB09M       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR06-RUECB09M
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CDULA'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0041'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5325-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5330-DELETE-RUECB09M            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                DELETE
                 FROM  DB2PRD.TCORND_GEOGR_CDULA
                WHERE  CJUNC_DEPDC        = :RUECB09M.CJUNC-DEPDC
                  AND  CCDULA             = :RUECB09M.CCDULA
                  AND  CSEQ_MATR_RURAL    = :RUECB09M.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV    = :RUECB09M.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV  = :RUECB09M.NCORND-GEOGR-IMOV
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CDULA'
                                       TO  ERR-DBD-TAB
               MOVE 'DELETE'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0042'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5330-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5335-INSERIR-COORDENADAS        SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-OBTER-DATA-HORA
           PERFORM  5340-MOVER-COORDENAS
           PERFORM  5345-INSERT-RUECB09N
           PERFORM  5350-INSERT-RUECB09M.

      *----------------------------------------------------------------*
       5335-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5340-MOVER-COORDENAS            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB016
                                       TO  CCONTR            OF RUECB09N

           MOVE  CJUNC-DEPDC           OF  RUFIB002
                                       TO  CJUNC-DEPDC       OF RUECB09M

           MOVE  CCDULA                OF  RUFIB002
                                       TO  CCDULA            OF RUECB09M

           MOVE  CSEQ-MATR-RURAL       OF  RUCAB016
                                       TO  CSEQ-MATR-RURAL   OF RUECB09N
                                           CSEQ-MATR-RURAL   OF RUECB09M

           MOVE  NSEQ-GLEBA-IMOV       OF  RUCAB016
                                       TO  NSEQ-GLEBA-IMOV   OF RUECB09N
                                           NSEQ-GLEBA-IMOV   OF RUECB09M

           MOVE  NCORND-GEOGR-IMOV     OF  RUCAB016
                                       TO  NCORND-GEOGR-IMOV OF RUECB09N
                                           NCORND-GEOGR-IMOV OF RUECB09M

           MOVE  MLATTD-IMOV-RURAL     OF  RUCAB016
                                       TO  MLATTD-IMOV-RURAL OF RUECB09N
                                           MLATTD-IMOV-RURAL OF RUECB09M

           MOVE  MLOGTD-IMOV-RURAL     OF  RUCAB016
                                       TO  MLOGTD-IMOV-RURAL OF RUECB09N
                                           MLOGTD-IMOV-RURAL OF RUECB09M

           MOVE  MALTTD-IMOV-RURAL     OF  RUCAB016
                                       TO  MALTTD-IMOV-RURAL OF RUECB09N
                                           MALTTD-IMOV-RURAL OF RUECB09M

           MOVE  CFUNC-ATULZ           OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-X09-DRESS         TO  CUSUAR-INCL       OF RUECB09N
                                           CUSUAR-INCL       OF RUECB09M

           MOVE  WRK-TIMESTAMP         TO  HINCL             OF RUECB09N
                                           HINCL             OF RUECB09M

           MOVE  SPACES                TO  CUSUAR-MANUT      OF RUECB09N
                                           CUSUAR-MANUT      OF RUECB09M

           MOVE  -1                    TO  WRK-B09N-CUSUAR-MANUT
                                           WRK-B09M-CUSUAR-MANUT

           MOVE  SPACES                TO  HMANUT-REG        OF RUECB09N
                                           HMANUT-REG        OF RUECB09M

           MOVE  -1                    TO  WRK-B09N-HMANUT-REG
                                           WRK-B09M-HMANUT-REG.

STEFAB     MOVE  CTPO-CORND-GEOGR      OF  RUCAB016
.                                      TO  CTPO-CORND-GEOGR OF RUECB09N
.                                          CTPO-CORND-GEOGR OF RUECB09M
.
.          IF  CTPO-CORND-GEOGR        OF RUCAB016
.                                      NOT LESS ZEROS
.              MOVE 0                  TO WRK-B09N-CTPO-CORND-GEOGR
.                                         WRK-B09M-CTPO-CORND-GEOGR
.          ELSE
.              MOVE -1                 TO WRK-B09N-CTPO-CORND-GEOGR
.                                         WRK-B09M-CTPO-CORND-GEOGR
STEFAB     END-IF.

      *----------------------------------------------------------------*
       5340-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5345-INSERT-RUECB09N            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.TCORND_GEOGR_CONTR
                     (  CCONTR                     ,
                        CSEQ_MATR_RURAL            ,
                        NSEQ_GLEBA_IMOV            ,
                        NCORND_GEOGR_IMOV          ,
                        MLATTD_IMOV_RURAL          ,
                        MLOGTD_IMOV_RURAL          ,
                        MALTTD_IMOV_RURAL          ,
                        CUSUAR_INCL                ,
                        HINCL                      ,
                        CUSUAR_MANUT               ,
                        HMANUT_REG                 ,
STEFAB                  CTPO_CORND_GEOGR           )
                VALUES
                     ( :RUECB09N.CCONTR            ,
                       :RUECB09N.CSEQ-MATR-RURAL   ,
                       :RUECB09N.NSEQ-GLEBA-IMOV   ,
                       :RUECB09N.NCORND-GEOGR-IMOV ,
                       :RUECB09N.MLATTD-IMOV-RURAL ,
                       :RUECB09N.MLOGTD-IMOV-RURAL ,
                       :RUECB09N.MALTTD-IMOV-RURAL ,
                       :RUECB09N.CUSUAR-INCL       ,
                       :RUECB09N.HINCL             ,
                       :RUECB09N.CUSUAR-MANUT
                         :WRK-B09N-CUSUAR-MANUT    ,
                       :RUECB09N.HMANUT-REG
                         :WRK-B09N-HMANUT-REG      ,
STEFAB                 :RUECB09N.CTPO-CORND-GEOGR
STEFAB                   :WRK-B09N-CTPO-CORND-GEOGR)
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0043'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5345-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5350-INSERT-RUECB09M            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.TCORND_GEOGR_CDULA
                     (  CJUNC_DEPDC                ,
                        CCDULA                     ,
                        CSEQ_MATR_RURAL            ,
                        NSEQ_GLEBA_IMOV            ,
                        NCORND_GEOGR_IMOV          ,
                        MLATTD_IMOV_RURAL          ,
                        MLOGTD_IMOV_RURAL          ,
                        MALTTD_IMOV_RURAL          ,
                        CUSUAR_INCL                ,
                        HINCL                      ,
                        CUSUAR_MANUT               ,
                        HMANUT_REG                 ,
STEFAB                  CTPO_CORND_GEOGR           )
                VALUES
                     ( :RUECB09M.CJUNC-DEPDC       ,
                       :RUECB09M.CCDULA            ,
                       :RUECB09M.CSEQ-MATR-RURAL   ,
                       :RUECB09M.NSEQ-GLEBA-IMOV   ,
                       :RUECB09M.NCORND-GEOGR-IMOV ,
                       :RUECB09M.MLATTD-IMOV-RURAL ,
                       :RUECB09M.MLOGTD-IMOV-RURAL ,
                       :RUECB09M.MALTTD-IMOV-RURAL ,
                       :RUECB09M.CUSUAR-INCL       ,
                       :RUECB09M.HINCL             ,
                       :RUECB09M.CUSUAR-MANUT
                         :WRK-B09M-CUSUAR-MANUT    ,
                       :RUECB09M.HMANUT-REG
                         :WRK-B09M-HMANUT-REG      ,
STEFAB                 :RUECB09M.CTPO-CORND-GEOGR
STEFAB                   :WRK-B09M-CTPO-CORND-GEOGR)
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND -530 AND -803 ) OR
              ( SQLWARN0            EQUAL  'W'                     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CDULA'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0044'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5350-99-FIM.  EXIT.
      *----------------------------------------------------------------*
7C5.10*----------------------------------------------------------------*
.      5360-TRATAR-CONFORMIDADE        SECTION.
.     *----------------------------------------------------------------*
.
.          PERFORM  5365-OPEN-CSR07-RUCAB023
.          PERFORM  5370-FETCH-CSR07-RUCAB023
.
.          PERFORM
.            UNTIL  SQLCODE         EQUAL  +100
.
.            PERFORM  5400-EXCLUIR-RUFIB094
.
.            PERFORM
.              UNTIL  SQLCODE       EQUAL  +100
.
.                 PERFORM  5435-INSERIR-CONFORMIDADE
.
.                 PERFORM  5370-FETCH-CSR07-RUCAB023
.            END-PERFORM
.          END-PERFORM.
.
.          PERFORM  5375-CLOSE-CSR07-RUCAB023.
.
.     *----------------------------------------------------------------*
.      5360-99-FIM.  EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5365-OPEN-CSR07-RUCAB023        SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE COMU-8807-CHAVE        TO CCONTR    OF RUCAB023
.          MOVE WRK-TIMESTAMP          TO HINCL-REG OF RUCAB023
.
.
.          EXEC SQL
.               OPEN CSR07-RUCAB023
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RECOR_TPO_CONFM   '
.                                      TO  ERR-DBD-TAB
.              MOVE 'OPEN'             TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5365'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5365-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5370-FETCH-CSR07-RUCAB023       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               FETCH CSR07-RUCAB023
.                INTO :RUCAB023.CCONTR
.                    ,:RUCAB023.COUTRO-TPO-CONFM
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'             )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RURAL_OUTRO_CONFM '
.                                      TO  ERR-DBD-TAB
.              MOVE 'FETCH'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5370'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF (SQLCODE                 EQUAL +100)
.              MOVE 'S'                TO WRK-CSR08-FIM-1
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5370-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*


7C5.10*----------------------------------------------------------------*
.      5375-CLOSE-CSR07-RUCAB023       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               CLOSE CSR07-RUCAB023
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RECOR_TPO_CONFM   '
.                                      TO  ERR-DBD-TAB
.              MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5040'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5375-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5400-EXCLUIR-RUFIB094           SECTION.
.     *----------------------------------------------------------------*
.
.          INITIALIZE                  RUFIB094
.                                      REPLACING ALPHANUMERIC BY SPACES
.                                                     NUMERIC BY ZEROS
.
.          MOVE COMU-8807-CHAVE        TO CCONTR OF RUFIB094
.
.          PERFORM 5401-OPEN-CSR08-RUFIB094
.
.          MOVE 'N'                    TO WRK-CSR08-FIM-1
.
.          PERFORM 5402-FETCH-CSR08-RUFIB094
.
.          PERFORM VARYING WRK-IND     FROM 1 BY 1
.                 UNTIL WRK-CSR08-FIM-1    EQUAL 'S'
.
.                  PERFORM 5403-DELETE-CSR08-RUFIB094
.                  PERFORM 5402-FETCH-CSR08-RUFIB094
.          END-PERFORM
.
.          PERFORM 5404-CLOSE-CSR08-RUFIB094.
.
.     *----------------------------------------------------------------*
.      5400-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      5401-OPEN-CSR08-RUFIB094        SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               OPEN CSR08-RUFIB094
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RURAL_OUTRO_CONFM '
.                                      TO  ERR-DBD-TAB
.              MOVE 'OPEN'             TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5401'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5401-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5402-FETCH-CSR08-RUFIB094       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               FETCH CSR08-RUFIB094
.                INTO :RUFIB094.CCONTR
.                    ,:RUFIB094.COUTRO-TPO-CONFM
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'             )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RURAL_OUTRO_CONFM '
.                                      TO  ERR-DBD-TAB
.              MOVE 'FETCH'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5402'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF (SQLCODE                 EQUAL +100)
.              MOVE 'S'                TO WRK-CSR08-FIM-1
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5402-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5403-DELETE-CSR08-RUFIB094      SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               DELETE FROM DB2PRD.TCONTR_RURAL_OUTRO_CONFM
.               WHERE CCONTR           = :RUFIB094.CCONTR
.               AND   COUTRO_TPO_CONFM = :RUFIB094.COUTRO-TPO-CONFM
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'            )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RURAL_OUTRO_CONFM '
.                                      TO  ERR-DBD-TAB
.              MOVE 'DELETE'           TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5403'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5403-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      5404-CLOSE-CSR08-RUFIB094       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               CLOSE CSR08-RUFIB094
.          END-EXEC
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'RURAL_OUTRO_CONFM '
.                                      TO  ERR-DBD-TAB
.              MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '5040'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8807-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5404-99-FIM.                    EXIT.
7C5.10*----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5435-INSERIR-CONFORMIDADE       SECTION.
      *----------------------------------------------------------------*

           PERFORM  5440-MOVER-CONFORMIDADE
           PERFORM  5445-INSERT-RUFIB094.

      *----------------------------------------------------------------*
       5435-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5440-MOVER-CONFORMIDADE         SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR               OF  RUCAB023
                                      TO  CCONTR            OF RUFIB094.

           MOVE  COUTRO-TPO-CONFM     OF  RUCAB023
                                      TO  COUTRO-TPO-CONFM  OF RUFIB094.

      *----------------------------------------------------------------*
       5440-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5445-INSERT-RUFIB094            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                INSERT  INTO DB2PRD.TCONTR_RURAL_OUTRO_CONFM
                     (  CCONTR                     ,
                        COUTRO_TPO_CONFM           )
                VALUES
                     ( :RUFIB094.CCONTR            ,
                       :RUFIB094.COUTRO-TPO-CONFM )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RURAL_OUTRO_CONFM '
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '5445'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8807-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8807-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8807-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8807-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5445-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*
