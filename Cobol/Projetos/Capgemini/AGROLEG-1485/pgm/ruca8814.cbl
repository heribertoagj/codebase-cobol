      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    RUCA8814.
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
      *        FORMATAR MENSAGEM COR0005 DE ALTERACAO.                 *
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
      *     MESU9032 - OBTER DADOS DA DEPENDENCIA                      *
      *     BRAD7600 - DATA/HORA                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *  BASES BD2:                                                    *
      *                                                                *
      *     RUCAB012 - DB2PRD.TCTRL_ENVIO_MANUT                        *
      *     RUCAB013 - DB2PRD.TENVIO_RETOR_MANUT                       *
      *     RUCAB015 - DB2PRD.TMANUT_CONTR_RECOR                       *
      *     RUCAB016 - DB2PRD.TMANUT_CORND_GEOGR                       *
      *     RUCAB017 - DB2PRD.TMANUT_PCELA_RECOR                       *
BRQ001*     RUCAB018 - DB2PRD.TMANUT_PPRIE_IMOV_RECOR                  *
      *     RUCAB019 - DB2PRD.TSIT_MANUT_RECOR                         *
      *     RUECB040 - DB2PRD.MATR_CONTR_RURAL                         *
      *     RUECB09N - DB2PRD.TCORND_GEOGR_CONTR                       *
      *     RUFIB002 - DB2PRD.CONTRATO_RURAL                           *
      *     RUFIB092 - DB2PRD.TINFO_COOP_CONTR                         *
      *     RUFIB093 - DB2PRD.TRENEG_CONTR_RURAL                       *
      *     RURCB000 - DB2PRD.ORIGEM_RECURSO                           *
      *     RURCB00A - DB2PRD.TBASE_LEGAL_RENEG                        *
      *     RURCB028 - DB2PRD.PLANO_REFIN                              *
WP1122*     RURCB097 - DB2PRD.TMOTVO_DCLAS_OPER                        *
WP1122*     RUFIB044 - DB2PRD.TMOTVO_DCLAS_CONTR                       *
CG0523*     TRAGB013 - DB2PRD.PROCS_TRAG_CONCL                         *
      *                                                                *
      *================================================================*
WP1122*================================================================*
.     *  ANALISTA......:   WIPRO - CLAUDIO PIAZZA                      *
.     *  DATA..........:   NOVEMBRO/2022                               *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.05 - MVP1                *
.     *                    INCLUSAO DO GRUPO DE DESCLASSIFICACAO 15    *
WP1122*================================================================*
NAV506*  ANALISTA......:   NAVA                                        *
NAV506*  DATA..........:   MAIO/2023                                   *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*                    FORMATACAO DOS CAMPOS                       *
NAV506*================================================================*
CG0523*================================================================*
CG0523*    C A P G E M I N I                                           *
CG0523*----------------------------------------------------------------*
CG0523*    ANALISTA....:   RICARDO PEREIRA DA SILVA                    *
CG0523*    DATA........:   05/2023                                     *
CG0523*    PROJETO.....:   ALTERACAO DE CNPJ DO BANCO SICOR            *
CG0523*    OBJETIVO....:   VALIDACAO DE TRAG                           *
CG0523*================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
MJ1223*================================================================*
MJ0224*    DS - BRADESCO                                               *
MJ0224*----------------------------------------------------------------*
MJ0224*    ANALISTA....:   MILTON JANUARIO                             *
MJ0224*    DATA........:   02/2024                                     *
MJ0224*    PROJETO.....:   SICOR WEB                                   *
MJ0224*    OBJETIVO....:   ENVIAR SEMPRE A DATA DE VENCIMENTO ORIGNAL  *
MJ0224*                    DA OPERAÇÃO                                 *
MJ0224*================================================================*
STE001*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  STEFANINI                                    *
.     *    DATA........:  MAIO / 2024                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.08                                   *
.     *    PESQUISA....:  STE001                                       *
STE001*================================================================*
7C5.10*================================================================*
.     * ANALISTA REPONSAVEL: EDSON/NIKSON                              *
.     * DATA...............: AGOSTO/2025                               *
.     * OBJETIVOS..........: FLASH - AGRO7-1553 - ALTERAÇÃO DA COR0005 *
.     *                      - SICOR 5.10                              *
7C5.10*================================================================*
CAPG05*================================================================*
CAPG05*  ANALISTA......:   CAPGEMINI                                   *
CAPG05*  DATA..........:   05/2026                                     *
CAPG05*----------------------------------------------------------------*
CAPG05*  OBJETIVOS.....:   AGROLEG-1485 - COR0005 SICOR 5.12           *
CAPG05*                    EXCLUSAO CERTIF/RASTREAB/ENERGIA MSG        *
CAPG05*                    EXCLUSAO INDR CONF ALERTA SUSTENTABILIDADE  *
CAPG05*================================================================*
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
           '01 - CAMPO USUARIO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-02              PIC  X(75)          VALUE
           '02 - CAMPO CHAVE COM CONTEUDO INVALIDO'.
           05  WRK-MSG-03              PIC  X(75)          VALUE
           '03 - CAMPO HORA DE INCLUSAO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-04              PIC  X(75)          VALUE
           '04 - CONTRATO NAO LOCALIZADO NA BASE CONTRATO RURAL'.
           05  WRK-MSG-05              PIC  X(75)          VALUE
           '05 - CONTRATO NAO LOCALIZADO NA BASE DE MANUTENCOES'.
           05  WRK-MSG-06              PIC  X(75)          VALUE
           '06 - ORIGEM DO RECURSO NAO LOCALIZADA'.
           05  WRK-MSG-07              PIC  X(75)          VALUE
           '07 - FONTE DO RECURSO NAO LOCALIZADA'.
           05  WRK-MSG-08              PIC  X(75)          VALUE
           '08 - AGENCIA NAO ENCONTRADO NO MESU'.
           05  WRK-MSG-99              PIC  X(75)          VALUE
           '99 - ERRO DE DB2'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DRESSCODE ***'.
      *----------------------------------------------------------------*

       01  WRK-S11-DRESS               PIC +9(11)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-X11-DRESS           PIC  X(11).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(03).
           05  WRK-N09-DRESS           PIC  9(09).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(04).
           05  WRK-N08-DRESS           PIC  9(08).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(07).
           05  WRK-N05-DRESS           PIC  9(05).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(08).
           05  WRK-N04-DRESS           PIC  9(04).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(09).
           05  WRK-N03-DRESS           PIC  9(03).
           05  WRK-X03-DRESS           REDEFINES
               WRK-N03-DRESS           PIC  X(03).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(10).
           05  WRK-N02-DRESS           PIC  9(02).

       01  WRK-S13V99-DRESS            PIC +9(13)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-X15-DRESS           PIC  X(15).
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(11).
           05  WRK-N05V-DRESS          PIC  9(05).

STEFAB 01  WRK-SJUR-S                  PIC  9(03)V99       VALUE ZEROS.
.      01  FILLER                      REDEFINES  WRK-SJUR-S.
STEFAB     05  WRK-SJUR-X              PIC  X(05).

STEFA1*x1  WRK-CCNPJ-INSTC-EXIG        PIC +9(08)          VALUE ZEROS.
.     *x1  FILLER                      REDEFINES  WRK-CCNPJ-INSTC-EXIG.
.     *    x5  FILLER                  PIC  X(01).
STEFA1*    x5  WRK-CCNPJ-INSTC-EXIG-N  PIC  X(08).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

7c2511*01  ACU-TAM                     PIC  9(04) COMP-3   VALUE ZEROS.
7c2511 01  ACU-TAM                     PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-POSICAO                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-QTD-GLEBA               PIC  9(05) COMP-3   VALUE ZEROS.

       01  ACU-VPCELA                  PIC S9(13)V99
                                                  COMP-3   VALUE ZEROS.
       01  ACU-MAREA-NAO-CTVDA         PIC S9(09)V99
                                                  COMP-3   VALUE ZEROS.
       01  ACU-NENVIO-RECOR            PIC S9(05) COMP-3   VALUE ZEROS.
BRQ001 01  ACU-QTD-PROPT               PIC S9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-CSEQ-MATR-RURAL-ANT     PIC S9(03) COMP-3   VALUE ZEROS.
       01  WRK-NSEQ-GLEBA-IMOV-ANT     PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-NENVIO-RECOR            PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-DVCTO-PCELA-CONTR-ANT   PIC  X(10)          VALUE SPACES.
       01  WRK-FIM-CSR04-RUECB040      PIC  X(01)          VALUE SPACES.
       01  WRK-TEM-GLEBA               PIC  X(01)          VALUE SPACES.
       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.
CG0523 01  WRK-TRAG                    PIC  X(01)          VALUE SPACES.

       01  WRK-NUMCTRLIF.
           05 WRK-NUMCTRLIF-FIXO1      PIC  X(008)         VALUE
              '0237RUC1'.
           05 WRK-NUMCTRLIF-ANO        PIC  9(004)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-MES        PIC  9(002)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-DIA        PIC  9(002)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-FIXO2      PIC  X(004)         VALUE '0001'.

7C5.10 01  WRK-CNPJ-AUX                PIC  X(14)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-CNPJ-AUX.
7C5.10     05  WRK-NUM-CNPJ-AUX        PIC  X(08).
           05  WRK-FILIAL-CNPJ-AUX     PIC  9(04).
           05  WRK-CTRL-CNPJ-AUX       PIC  9(02).

7C5.10 01  WRK-CPF-AUX                 PIC  X(11)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-CPF-AUX.
           05  WRK-NUM-CPF-AUX         PIC  9(09).
           05  WRK-CTRL-CPF-AUX        PIC  9(02).

       01  WRK-MSGEM-RURAL             PIC  X(32000)       VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-MSGEM-RURAL.
           05 FILLER                   PIC  X(00009).
           05 WRK-NUMCTRLIF-MSGEM      PIC  X(00020).
           05 FILLER                   PIC  X(31971).

       01  WRK-SEQ-GLEBA-X19.
           05  FILLER                  PIC  9(11)          VALUE ZEROS.
           05  WRK-SEQ-MATR-N03        PIC  9(03)          VALUE ZEROS.
           05  WRK-SEQ-GLEBA-N05       PIC  9(05)          VALUE ZEROS.

       01  WRK-MLATTD-IMOV-RURAL-X.
           05  WRK-MLATTD-IMOV-RURAL-N PIC S9(02)V9(11)    VALUE ZEROS.

       01  WRK-MLOGTD-IMOV-RURAL-X.
           05  WRK-MLOGTD-IMOV-RURAL-N PIC S9(03)V9(11)    VALUE ZEROS.

       01  WRK-MALTTD-IMOV-RURAL-X.
           05  WRK-MALTTD-IMOV-RURAL-N PIC S9(04)V9(02)    VALUE ZEROS.

       01  WRK-MAREA-NAO-CTVDA-X.
           05  WRK-MAREA-NAO-CTVDA-N   PIC S9(09)V9(02)    VALUE ZEROS.

BRQ001 01  WRK-PIC9-3V99-CS            PIC +9(03)V99       VALUE ZEROS.
BRQ001 01  FILLER                      REDEFINES WRK-PIC9-3V99-CS.
BRQ001     05 FILLER                   PIC  X(01).
BRQ001     05 WRK-PIC9-3V99-SS         PIC  9(03)V99.

BRQ001 01  WRK-COD9-NUM-S      PIC +9(9)        VALUE ZEROS.
BRQ001 01  FILLER              REDEFINES WRK-COD9-NUM-S.
BRQ001     05 FILLER           PIC  X(1).
BRQ001     05 FILLER           PIC  9(1).
BRQ001     05 WRK-COD9-NUM8-S  PIC  9(8).
BRQ001     05 WRK-COD9-NUM8-XS REDEFINES  WRK-COD9-NUM8-S
BRQ001                         PIC  X(8).

BRQ001 01  WRK-COD9-CPF        PIC +9(9)        VALUE ZEROS.
BRQ001 01  FILLER              REDEFINES WRK-COD9-CPF.
BRQ001     05 FILLER           PIC  9(1).
BRQ001     05 WRK-COD9-CPF-9   PIC  9(9).

BRQ001 01  WRK-CNPJBASE-PROPT  PIC  X(11)       VALUE SPACES.
BRQ001 01  FILLER              REDEFINES WRK-CNPJBASE-PROPT.
BRQ001     05 WRK-CNPJ-BAS-PRO       PIC  X(8).
BRQ001     05 WRK-CNPJ-BAS-PRO-ZEROS PIC  X(3).

BRQ001 01  WRK-COD2-NUM-S      PIC +9(2)        VALUE ZEROS.
BRQ001 01  FILLER              REDEFINES WRK-COD2-NUM-S.
BRQ001     05 FILLER           PIC  X(1).
BRQ001     05 WRK-COD2-NUM     PIC  9(2).

BRQ001 01  WRK-EDIT-CPF.
BRQ001     05 WRK-EDIT-NCPF        PIC  9(09) VALUE ZEROS.
BRQ001     05 WRK-EDIT-CCPF        PIC  9(02) VALUE ZEROS.

STEFAB*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(50)          VALUE
.          '*** DADOS SALVOS DA TELA RUCAT954 ***'.
.     *----------------------------------------------------------------*
.
.      01  WRK-DADOS-NAV-T954.
.          05  WRK-954-CTRLNAV         PIC  X(20)          VALUE SPACES.
.          05  WRK-954-CHAVE           PIC  9(09)          VALUE ZEROS.
.          05  WRK-954-CTRLALT         PIC  X(01)          VALUE SPACES.
.          05  WRK-954-CAMPOS-ANT      PIC  X(57)          VALUE SPACES.
.          05  WRK-954-CAMPOS-ATU.
.              07  FILLER              PIC  X(04)          VALUE SPACES.
.              07  WRK-954-VLRGAR-ATU  PIC  9(12)V99       VALUE ZEROS.
.              07  FILLER              PIC  X(01)          VALUE SPACES.
.              07  WRK-954-TXJURO-ATU  PIC  9(03)V99       VALUE ZEROS.
.              07  WRK-954-DTTAXA-ATU  PIC  X(08)          VALUE SPACES.
.              07  WRK-954-CDTAXA-ATU  PIC  9(03)          VALUE ZEROS.
.              07  WRK-954-CODFUN-ATU  PIC  9(07)          VALUE ZEROS.
.              07  FILLER              PIC  X(01)          VALUE SPACES.
.              07  WRK-954-PERCON-ATU  PIC  9(03)V99       VALUE ZEROS.
.              07  FILLER              PIC  X(01)          VALUE SPACES.
.              07  WRK-954-JURPOS-ATU  PIC  9(03)V99       VALUE ZEROS.
.              07  FILLER              PIC  X(01)          VALUE SPACES.
.              07  WRK-954-PERSTN-ATU  PIC  9(03)V99       VALUE ZEROS.
.              07  FILLER              PIC  X(01)          VALUE SPACES.
.              07  WRK-954-PREDUC-ATU  PIC  9(03)V99       VALUE ZEROS.
.          05  WRK-954-STA-CPO-ALT.
.              07  WRK-954-STA-VLRGAR  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-TXJURO  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-DTTAXA  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-CDTAXA  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-CODFUN  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-PERCON  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-JURPOS  PIC  X(01)          VALUE SPACES.
.              07  WRK-954-STA-PERSTN  PIC  X(01)          VALUE SPACES.
STEFAB         07  WRK-954-STA-PREDUC  PIC  X(01)         VALUE SPACES.

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
           05  WRK-MES-ATUAL           PIC  9(02).
           05  WRK-DIA-ATUAL           PIC  9(02).

       01  WRK-DATA-INV                PIC  9(08)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-DATA-INV.
           05  WRK-ANO-INV             PIC  9(04).
           05  WRK-MES-INV             PIC  9(02).
           05  WRK-DIA-INV             PIC  9(02).
       01  FILLER                      REDEFINES  WRK-DATA-INV.
           05  WRK-DATA-INV-X          PIC  X(08).

MJ0224 01  WRK-DVCTO-ORIGN-CONTR-AUX   PIC  X(08)          VALUE SPACES.
MJ0224 01  WRK-DVCTO-CONTR-AUX         PIC  X(08)          VALUE SPACES.
      *
       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-MES-DB2             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(04)          VALUE ZEROS.

CG0523 01  WRK-DATA-CONTRATO.
CG0523     05  WRK-ANO-CONTRATO        PIC  9(04)          VALUE ZEROS.
CG0523     05  WRK-MES-CONTRATO        PIC  9(02)          VALUE ZEROS.
CG0523     05  WRK-DIA-CONTRATO        PIC  9(02)          VALUE ZEROS.
CG0523 01  WRK-DATA-TRAG.
CG0523     05  WRK-ANO-TRAG            PIC  9(04)          VALUE ZEROS.
CG0523     05  WRK-MES-TRAG            PIC  9(02)          VALUE ZEROS.
CG0523     05  WRK-DIA-TRAG            PIC  9(02)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREAS DO MODULO MESU9032 ***'.
      *----------------------------------------------------------------*

       01  WRK-9032-DADOS.
           03  WRK-9032-ENTRADA.
               05  WRK-9032-NOME-PGM   PIC X(08)           VALUE SPACES.
               05  WRK-9032-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-9032-AGENCIA    PIC 9(05)           VALUE ZEROS.
           03  WRK-9032-SAIDAS.
               05  WRK-9032-NOME       PIC X(20)           VALUE SPACES.
               05  WRK-9032-ENDERECO   PIC X(30)           VALUE SPACES.
               05  WRK-9032-CIDADE     PIC X(25)           VALUE SPACES.
               05  WRK-9032-ESTADO     PIC X(02)           VALUE SPACES.
               05  WRK-9032-CEP        PIC 9(05)           VALUE ZEROS.
               05  WRK-9032-REGIO-DIRET
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-9032-JUNC-REGIO PIC 9(05)           VALUE ZEROS.
               05  WRK-9032-NOVOCEP    PIC 9(09)           VALUE ZEROS.
               05  WRK-9032-NOME-GER   PIC X(30)           VALUE SPACES.
               05  WRK-9032-DT-INAUGUR PIC X(10)           VALUE SPACES.
               05  WRK-9032-DT-ENCRESU PIC X(10)           VALUE SPACES.
               05  WRK-9032-LOJA       PIC 9(05)           VALUE ZEROS.
               05  FILLER              REDEFINES  WRK-9032-LOJA.
                   07  FILLER          PIC X(01).
                   07  WRK-9032-FILIAL PIC 9(04).
               05  WRK-9032-CONTROLE   PIC X(02)           VALUE SPACES.
               05  WRK-9032-CONTROLE-N REDEFINES
                   WRK-9032-CONTROLE   PIC 9(02).

       01  WRK-9032-BRAD7100           PIC X(107)          VALUE SPACES.
       01  WRK-9032-SQLCA              PIC X(136)          VALUE SPACES.
       01  WRK-9032-AMBIENTE           PIC X(01)           VALUE SPACES.
       01  WRK-9032-IO-PCB             PIC X(32)           VALUE SPACES.
       01  WRK-9032-ALT-PCB            PIC X(32)           VALUE SPACES.

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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE FORMATACAO P/ COR0005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUCA67'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B002-CFNALD-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CATVDD-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CORIGE-REC         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CTPO-BNEFC-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-QPROTV-ADQUI       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CCTRL-RQUIS-FINCR  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CUNIC-MSGEM        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CREFT-RECOR        PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CORIGE-REC         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B015-NBASE-LEGAL-RENEG  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B016-MAREA-NAO-CTVDA    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B019-CMSGEM-RECOR       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B092-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B093-NBASE-LEGAL-RENEG  PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CCETFC-ORGNC-EMPTO-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-EURL-CETFC-ORGNC-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CRSTRB-EMPTO-RURAL-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-EURL-RSTRB-EMPTO-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-PENERG-RENOV-EMPTO-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CNIRF-IMOV-RURAL-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CCCIR-NULL              PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CREG-AMBTL-RURAL-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-PPRESV-AMBTL-PPRIE-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-COUTGA-AGUA-IMOV-NULL   PIC S9(04)  COMP    VALUE ZEROS.
WP0522 01  WRK-CSCRET-TSORO-NULL       PIC S9(04)  COMP    VALUE ZEROS.
STEFAB 01  WRK-WRESUL-CONS-MDULO-NULL  PIC S9(04)  COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB012
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB013
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

BRQ001     EXEC SQL
BRQ001         INCLUDE RUCAB018
BRQ001     END-EXEC.

           EXEC SQL
               INCLUDE RUCAB019
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB040
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09N
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

WP1122     EXEC SQL
-              INCLUDE RUFIB044
WP1122     END-EXEC.

CG0523     EXEC SQL
CG0523         INCLUDE TRAGB013
CG0523     END-EXEC.

           EXEC SQL
               INCLUDE RUFIB092
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB093
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB000
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB00A
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB028
           END-EXEC.

STEFAB     EXEC SQL
.              INCLUDE RUECB048
STEFAB     END-EXEC.

WP1122     EXEC SQL
-              INCLUDE RURCB097
WP1122     END-EXEC.

           EXEC SQL
              DECLARE  CSR01-RUCAB017 CURSOR WITH HOLD FOR
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
                  AND  NPCELA_CONTR  = :RUCAB017.NPCELA-CONTR
                  AND  NATULZ_PCELA >= :RUCAB017.NATULZ-PCELA
                ORDER  BY DVCTO_PCELA_CONTR ASC
           END-EXEC.

WP1122     EXEC SQL
-             DECLARE  CSR02-RUFIB044-JN CURSOR WITH HOLD FOR
-              SELECT  A.CMOTVO_DCLAS_OPER  ,
-                      A.DDCLAS_OPER_CREDT  ,
-                      A.VPRINC_DCLAS_CONTR ,
-                      B.CMOTVO_DCLAS_BACEN
-                FROM  DB2PRD.TMOTVO_DCLAS_CONTR A ,
-                      DB2PRD.TMOTVO_DCLAS_OPER  B
-               WHERE  CCONTR              = :RUFIB044.CCONTR
-                 AND  A.CMOTVO_DCLAS_OPER = B.CMOTVO_DCLAS_OPER
WP1122     END-EXEC.

           EXEC SQL
              DECLARE  CSR03-RUFIB092 CURSOR WITH HOLD FOR
               SELECT  CTPO_PSSOA         ,
                       CCNPJ_CPF_COOP     ,
                       CFLIAL_CNPJ_COOP   ,
                       CCTRL_CPF_COOP     ,
                       VPCELA_COOP_RURAL  ,
                       CPROG_RURAL
                 FROM  DB2PRD.TINFO_COOP_CONTR
                WHERE  CCONTR             = :RUFIB092.CCONTR
                  AND  NID_COOP_RURAL    >= :RUFIB092.NID-COOP-RURAL
           END-EXEC.

           EXEC SQL
              DECLARE  CSR04-RUECB040 CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       CSEQ_MATR_RURAL
                 FROM  DB2PRD.MATR_CONTR_RURAL
                WHERE  CCONTR             = :RUECB040.CCONTR
                  AND  CSEQ_MATR_RURAL   >= :RUECB040.CSEQ-MATR-RURAL
                ORDER  BY CSEQ_MATR_RURAL DESC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR05-RUCAB016 CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       HINCL_REG         ,
                       CSEQ_MATR_RURAL   ,
                       NSEQ_GLEBA_IMOV   ,
                       NCORND_GEOGR_IMOV ,
                       MLATTD_IMOV_RURAL ,
                       MLOGTD_IMOV_RURAL ,
                       MALTTD_IMOV_RURAL ,
                       MAREA_NAO_CTVDA
                 FROM  DB2PRD.TMANUT_CORND_GEOGR
                WHERE  CCONTR             = :RUCAB016.CCONTR
                  AND  HINCL_REG          = :RUCAB016.HINCL-REG
                  AND  CSEQ_MATR_RURAL    = :RUCAB016.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUCAB016.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUCAB016.NCORND-GEOGR-IMOV
                ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR06-RUECB09N CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       CSEQ_MATR_RURAL   ,
                       NSEQ_GLEBA_IMOV   ,
                       NCORND_GEOGR_IMOV ,
                       MLATTD_IMOV_RURAL ,
                       MLOGTD_IMOV_RURAL ,
                       MALTTD_IMOV_RURAL
                 FROM  DB2PRD.TCORND_GEOGR_CONTR
                WHERE  CCONTR             = :RUECB09N.CCONTR
                  AND  CSEQ_MATR_RURAL    = :RUECB09N.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUECB09N.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUECB09N.NCORND-GEOGR-IMOV
                ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

BRQ001     EXEC SQL
BRQ001        DECLARE  CSR07-RUCAB018 CURSOR WITH HOLD FOR
BRQ001         SELECT  CCNPJ_CPF         ,
BRQ001                 CFLIAL_CNPJ       ,
BRQ001                 CCTRL_CNPJ_CPF    ,
BRQ001                 CTPO_PSSOA        ,
BRQ001                 CNIRF_IMOV_RURAL  ,
BRQ001                 CCCIR             ,
BRQ001                 CREG_AMBTL_RURAL  ,
BRQ001                 PPRESV_AMBTL_PPRIE,
BRQ001                 COUTGA_AGUA_IMOV
BRQ001           FROM  DB2PRD.TMANUT_PPRIE_IMOV_RECOR
BRQ001          WHERE  CCONTR             = :RUCAB018.CCONTR
BRQ001            AND  HINCL_REG          = :RUCAB018.HINCL-REG
BRQ001     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************

       01  COMU-AREA-RUCA8814.
           05  COMU-8814-ENTRADA.
               07  COMU-8814-USUARIO   PIC  9(07).
               07  COMU-8814-CHAVE     PIC  9(09).
               07  COMU-8814-CTRLNAV.
                   10  COMU-8814-ANO-CTRLNAV
                                       PIC  9(04).
                   10  COMU-8814-MES-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8814-DIA-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8814-HOR-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8814-MIN-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8814-SEG-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8814-MIL-CTRLNAV
                                       PIC  9(06).
STEFAB         07  COMU-8814-TERMINAL  PIC  X(08).
           05  COMU-8814-SAIDA.
               07  COMU-8814-COD-RETORNO
                                       PIC  9(02).
               07  COMU-8814-MENSAGEM  PIC  X(75).
               07  COMU-8814-ERRO-AREA PIC  X(107).
               07  COMU-8814-SQLCA     PIC  X(136).

      ******************************************************************
       PROCEDURE                      DIVISION USING COMU-AREA-RUCA8814.
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

           MOVE  ZEROS                 TO  COMU-8814-COD-RETORNO
                                           ACU-NENVIO-RECOR
           MOVE  WRK-MSG-00            TO  COMU-8814-MENSAGEM
           MOVE  SPACES                TO  COMU-8814-ERRO-AREA
                                           COMU-8814-SQLCA

           IF  COMU-8814-USUARIO       IS  NUMERIC  AND
               COMU-8814-USUARIO  GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  01                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-01        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8814-CHAVE         IS  NUMERIC  AND
               COMU-8814-CHAVE    GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  02                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-02        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8814-CTRLNAV       IS  NUMERIC  AND
               COMU-8814-CTRLNAV  GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  03                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-03        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-SELECT-RUFIB002
           PERFORM  5010-SELECT-RUCAB015
           PERFORM  5015-FORMATAR-MSG
           PERFORM  5225-SALVAR-MENSAGEM.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5005-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8814-CHAVE       TO  CCONTR           OF RUFIB002

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        DVCTO_CONTR                   ,
                        VFINAN                        ,
CG0523                  DINCL                         ,
                        CJUNC_DEPDC                   ,
CG0523                  CCTA_CORR                     ,
                        CFNALD_RURAL                  ,
                        CATVDD_RURAL                  ,
                        CORIGE_REC                    ,
                        CPRODT_RURAL                  ,
                        CREFT_BACEN                   ,
                        CMODLD_RURAL                  ,
                        CREFT_RECOR                   ,
                        CTPO_BNEFC_RURAL              ,
                        QPROTV_ADQUI                  ,
MJ0224                  DVCTO_ORIGN_CONTR
                  INTO
                       :RUFIB002.CCONTR               ,
                       :RUFIB002.DVCTO-CONTR          ,
                       :RUFIB002.VFINAN               ,
CG0523                 :RUFIB002.DINCL                ,
                       :RUFIB002.CJUNC-DEPDC          ,
CG0523                 :RUFIB002.CCTA-CORR            ,
                       :RUFIB002.CFNALD-RURAL
                         :WRK-B002-CFNALD-RURAL       ,
                       :RUFIB002.CATVDD-RURAL
                         :WRK-B002-CATVDD-RURAL       ,
                       :RUFIB002.CORIGE-REC
                         :WRK-B002-CORIGE-REC         ,
                       :RUFIB002.CPRODT-RURAL
                         :WRK-B002-CPRODT-RURAL       ,
                       :RUFIB002.CREFT-BACEN          ,
                       :RUFIB002.CMODLD-RURAL         ,
                       :RUFIB002.CREFT-RECOR          ,
                       :RUFIB002.CTPO-BNEFC-RURAL
                         :WRK-B002-CTPO-BNEFC-RURAL   ,
                       :RUFIB002.QPROTV-ADQUI
                         :WRK-B002-QPROTV-ADQUI       ,
MJ0224                 :RUFIB002.DVCTO-ORIGN-CONTR
                  FROM  DB2PRD.CONTRATO_RURAL
                 WHERE  CCONTR  = :RUFIB002.CCONTR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  04                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-04        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B002-CFNALD-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CFNALD-RURAL     OF RUFIB002
           END-IF.

           IF  WRK-B002-CATVDD-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CATVDD-RURAL     OF RUFIB002
           END-IF.

           IF  WRK-B002-CORIGE-REC     EQUAL  -1
               MOVE  ZEROS             TO  CORIGE-REC       OF RUFIB002
           END-IF.

           IF  WRK-B002-CPRODT-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CPRODT-RURAL     OF RUFIB002
           END-IF.

           IF  WRK-B002-CTPO-BNEFC-RURAL
                                       EQUAL  -1
               MOVE  ZEROS             TO  CTPO-BNEFC-RURAL OF RUFIB002
           END-IF.

           IF  WRK-B002-QPROTV-ADQUI   EQUAL  -1
               MOVE  ZEROS             TO  QPROTV-ADQUI     OF RUFIB002
           END-IF.

CG0523     MOVE DINCL OF RUFIB002      TO WRK-DATA-DB2
CG0523     MOVE WRK-DIA-DB2            TO WRK-DIA-CONTRATO
CG0523     MOVE WRK-MES-DB2            TO WRK-MES-CONTRATO
CG0523     MOVE WRK-ANO-DB2            TO WRK-ANO-CONTRATO.

      *----------------------------------------------------------------*
       5005-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-SELECT-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8814-CHAVE       TO  CCONTR           OF RUCAB015
           MOVE  COMU-8814-ANO-CTRLNAV TO  WRK-ANO-TIMES
           MOVE  COMU-8814-MES-CTRLNAV TO  WRK-MES-TIMES
           MOVE  COMU-8814-DIA-CTRLNAV TO  WRK-DIA-TIMES
           MOVE  COMU-8814-HOR-CTRLNAV TO  WRK-HOR-TIMES
           MOVE  COMU-8814-MIN-CTRLNAV TO  WRK-MIN-TIMES
           MOVE  COMU-8814-SEG-CTRLNAV TO  WRK-SEG-TIMES
           MOVE  COMU-8814-MIL-CTRLNAV TO  WRK-MIL-TIMES
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB015

           EXEC SQL
                SELECT
                        CCONTR                        ,
                        HINCL_REG                     ,
                        VFINAN                        ,
                        CORIGE_REC                    ,
                        CPRODT_RURAL                  ,
                        NBASE_LEGAL_RENEG             ,
                        DVCTO_CONTR                   ,
BRQ001                  CCETFC_ORGNC_EMPTO            ,
BRQ001                  EURL_CETFC_ORGNC              ,
BRQ001                  CRSTRB_EMPTO_RURAL            ,
BRQ001                  EURL_RSTRB_EMPTO              ,
BRQ001                  PENERG_RENOV_EMPTO            ,
WP0522                  CSCRET_TSORO_NACIO            ,
STEFAB                  VALUE(CCNPJ_INSTC_EXIGB, 0)
                  INTO
                       :RUCAB015.CCONTR               ,
                       :RUCAB015.HINCL-REG            ,
                       :RUCAB015.VFINAN               ,
                       :RUCAB015.CORIGE-REC
                         :WRK-B015-CORIGE-REC         ,
                       :RUCAB015.CPRODT-RURAL
                         :WRK-B015-CPRODT-RURAL       ,
                       :RUCAB015.NBASE-LEGAL-RENEG
                         :WRK-B015-NBASE-LEGAL-RENEG  ,
                       :RUCAB015.DVCTO-CONTR          ,
BRQ001                 :RUCAB015.CCETFC-ORGNC-EMPTO
BRQ001                   :WRK-CCETFC-ORGNC-EMPTO-NULL ,
BRQ001                 :RUCAB015.EURL-CETFC-ORGNC
BRQ001                   :WRK-EURL-CETFC-ORGNC-NULL   ,
BRQ001                 :RUCAB015.CRSTRB-EMPTO-RURAL
BRQ001                   :WRK-CRSTRB-EMPTO-RURAL-NULL ,
BRQ001                 :RUCAB015.EURL-RSTRB-EMPTO
BRQ001                   :WRK-EURL-RSTRB-EMPTO-NULL   ,
BRQ001                 :RUCAB015.PENERG-RENOV-EMPTO
BRQ001                   :WRK-PENERG-RENOV-EMPTO-NULL ,
WP0522                 :RUCAB015.CSCRET-TSORO-NACIO
WP0522                   :WRK-CSCRET-TSORO-NULL,
STEFAB                 :RUCAB015.CCNPJ-INSTC-EXIGB
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
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  05                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-05        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B015-CORIGE-REC           EQUAL  -1
               MOVE  CORIGE-REC        OF RUFIB002
                                       TO CORIGE-REC         OF RUCAB015
           END-IF.

           IF  WRK-B015-CPRODT-RURAL         EQUAL  -1
               MOVE  CPRODT-RURAL      OF RUFIB002
                                       TO CPRODT-RURAL       OF RUCAB015
           END-IF.

           IF  WRK-B015-NBASE-LEGAL-RENEG    EQUAL  -1
               MOVE  ZEROS             TO NBASE-LEGAL-RENEG  OF RUCAB015
           END-IF.

BRQ001     IF  WRK-CCETFC-ORGNC-EMPTO-NULL   EQUAL  -1
BRQ001         MOVE  SPACES            TO CCETFC-ORGNC-EMPTO OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-EURL-CETFC-ORGNC-NULL     EQUAL  -1
BRQ001         MOVE  256               TO EURL-CETFC-ORGNC-LEN
BRQ001                                                      OF RUCAB015
BRQ001         MOVE  SPACES            TO EURL-CETFC-ORGNC-TEXT
BRQ001                                                      OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-CRSTRB-EMPTO-RURAL-NULL   EQUAL  -1
BRQ001         MOVE  SPACES            TO CRSTRB-EMPTO-RURAL OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-EURL-RSTRB-EMPTO-NULL     EQUAL  -1
BRQ001         MOVE  256               TO EURL-RSTRB-EMPTO-LEN
BRQ001                                                      OF RUCAB015
BRQ001         MOVE  SPACES            TO EURL-RSTRB-EMPTO-TEXT
BRQ001                                                      OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-PENERG-RENOV-EMPTO-NULL   EQUAL  -1
BRQ001         MOVE  ZEROS             TO PENERG-RENOV-EMPTO OF RUCAB015
BRQ001     END-IF.
BRQ001
WP0522     IF  WRK-CSCRET-TSORO-NULL         EQUAL  -1
WP0522         MOVE  ZEROS             TO CSCRET-TSORO-NACIO OF RUCAB015
WP0522     END-IF.
WP0522
      *----------------------------------------------------------------*
       5010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5015-FORMATAR-MSG               SECTION.
      *----------------------------------------------------------------*

           MOVE  'COR0005'             TO  RUCA67-CODMSG

           PERFORM  5020-OBTER-DATA-HORA

           MOVE  WRK-ANO-ATUAL         TO  WRK-NUMCTRLIF-ANO
           MOVE  WRK-MES-ATUAL         TO  WRK-NUMCTRLIF-MES
           MOVE  WRK-DIA-ATUAL         TO  WRK-NUMCTRLIF-DIA
           MOVE  WRK-NUMCTRLIF         TO  RUCA67-NUMCTRLIF

CG0523     PERFORM  5021-OBTER-AGENCIA-ORIGINAL

           PERFORM  5025-CHAMAR-MESU9032

7C5.10     MOVE '60746948'             TO  WRK-NUM-CNPJ-AUX
           MOVE  WRK-9032-FILIAL       TO  WRK-FILIAL-CNPJ-AUX
           MOVE  WRK-9032-CONTROLE-N   TO  WRK-CTRL-CNPJ-AUX
           MOVE  WRK-CNPJ-AUX          TO  RUCA67-CNPJ-ENT-RESPONS

           PERFORM  5030-OBTER-CREFT-RECOR

      ******F  DVCTO-CONTR OF RUCAB015  EQUAL  '01.01.0001'
      **********OVE  DVCTO-CONTR       OF  RUFIB002
MJ0224     IF (DVCTO-CONTR OF RUCAB015 NOT EQUAL '01.01.0001')       AND
MJ0224        (DVCTO-CONTR OF RUCAB015 NOT EQUAL SPACES)             AND
MJ0224        (DVCTO-CONTR OF RUCAB015 NOT EQUAL LOW-VALUES)
               MOVE  DVCTO-CONTR       OF  RUCAB015
                                       TO  WRK-DATA-DB2
               MOVE  WRK-DIA-DB2       TO  WRK-DIA-INV
               MOVE  WRK-MES-DB2       TO  WRK-MES-INV
               MOVE  WRK-ANO-DB2       TO  WRK-ANO-INV
               MOVE  WRK-DATA-INV      TO  RUCA67-DTVENC
MJ0224     ELSE
MJ0224     IF (DVCTO-ORIGN-CONTR OF RUFIB002 NOT EQUAL '01.01.0001') AND
MJ0224        (DVCTO-ORIGN-CONTR OF RUFIB002 NOT EQUAL SPACES)       AND
MJ0224        (DVCTO-ORIGN-CONTR OF RUFIB002 NOT EQUAL LOW-VALUES)
MJ0224         STRING DVCTO-ORIGN-CONTR OF RUFIB002(7:4)
MJ0224                DVCTO-ORIGN-CONTR OF RUFIB002(4:2)
MJ0224                DVCTO-ORIGN-CONTR OF RUFIB002(1:2)
MJ0224                DELIMITED BY SIZE INTO WRK-DVCTO-ORIGN-CONTR-AUX
MJ0224         STRING DVCTO-CONTR       OF RUFIB002(7:4)
MJ0224                DVCTO-CONTR       OF RUFIB002(4:2)
MJ0224                DVCTO-CONTR       OF RUFIB002(1:2)
MJ0224                DELIMITED BY SIZE INTO WRK-DVCTO-CONTR-AUX
MJ0224         IF  WRK-DVCTO-CONTR-AUX  GREATER
MJ0224                                  WRK-DVCTO-ORIGN-CONTR-AUX
MJ0224             MOVE DVCTO-CONTR    OF  RUFIB002
MJ0224                                 TO  WRK-DATA-DB2
MJ0224             MOVE WRK-DIA-DB2    TO  WRK-DIA-INV
MJ0224             MOVE WRK-MES-DB2    TO  WRK-MES-INV
MJ0224             MOVE WRK-ANO-DB2    TO  WRK-ANO-INV
MJ0224             MOVE WRK-DATA-INV   TO  RUCA67-DTVENC
MJ0224         ELSE
MJ0224             MOVE DVCTO-ORIGN-CONTR  OF RUFIB002
MJ0224                                 TO  WRK-DATA-DB2
MJ0224             MOVE WRK-DIA-DB2    TO  WRK-DIA-INV
MJ0224             MOVE WRK-MES-DB2    TO  WRK-MES-INV
MJ0224             MOVE WRK-ANO-DB2    TO  WRK-ANO-INV
MJ0224             MOVE WRK-DATA-INV   TO  RUCA67-DTVENC
MJ0224         END-IF
MJ0224     ELSE
MJ0224             MOVE DVCTO-CONTR    OF  RUFIB002
MJ0224                                 TO  WRK-DATA-DB2
MJ0224             MOVE WRK-DIA-DB2    TO  WRK-DIA-INV
MJ0224             MOVE WRK-MES-DB2    TO  WRK-MES-INV
MJ0224             MOVE WRK-ANO-DB2    TO  WRK-ANO-INV
MJ0224             MOVE WRK-DATA-INV   TO  RUCA67-DTVENC
MJ0224      END-IF
           END-IF.

           IF  NBASE-LEGAL-RENEG OF RUCAB015  GREATER   ZEROS
               MOVE  NBASE-LEGAL-RENEG OF  RUCAB015
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X03-DRESS     TO  RUCA67-CBASE-LEGAL-PRO
           ELSE
               PERFORM  5045-OBTER-MOTIVO-PRORROG
           END-IF.

           MOVE  ALL '*'               TO  RUCA67-TP-INSTNTO-CRED
CAPG05*    -MOVE  'N'                   TO  RUCA67-IND-CONF-ALRT-SUST.

           MOVE  1                     TO  ACU-POSICAO
           MOVE  RUCA67-COR0005-1      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-1)
           ADD   LENGTH OF RUCA67-COR0005-1
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  RUCA67-COR0005-2      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-2)
           ADD   LENGTH OF RUCA67-COR0005-2
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  '001'                 TO  RUCA67-NRO-ORDEM-DEST

           IF  WRK-B002-QPROTV-ADQUI   EQUAL  -1
               MOVE  ALL '*'           TO  RUCA67-VLR-PROD-OBTD
           ELSE
               MOVE  QPROTV-ADQUI      OF  RUFIB002
                                       TO  WRK-S13V99-DRESS
               MOVE  ZEROS             TO  RUCA67-VLR-PROD-OBTD(1:04)
               MOVE  WRK-X15-DRESS     TO  RUCA67-VLR-PROD-OBTD(5:15)
           END-IF.

           MOVE  RUCA67-COR0005-3      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-3)
           ADD   LENGTH OF RUCA67-COR0005-3
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           PERFORM  5060-TRATAR-GRUPO-LIBERACAO


      *----------------------------------------------------------------*

           PERFORM  5105-TRATAR-GRUPO-COOPERADO

      *----------------------------------------------------------------*

           PERFORM  5125-SELECT-RURCB000
           PERFORM  5130-SELECT-RURCB028

           IF CPLANO-REFIN-RECOR OF RURCB028 EQUAL SPACES
               MOVE  ALL '*'           TO  RUCA67-TP-FNTE-REC
           ELSE
               MOVE  CPLANO-REFIN-RECOR OF  RURCB028
                                       TO  RUCA67-TP-FNTE-REC
           END-IF.

           MOVE  RUCA67-COR0005-6A     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-6A)
           ADD   LENGTH OF RUCA67-COR0005-6A
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           PERFORM  5135-FORMATAR-GRUPO-GLEBA

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA67-PERC-POSFIX
           MOVE  '09'                  TO  RUCA67-TPENCARGO-COMPL
           MOVE  ALL '*'               TO  RUCA67-AREA-X
           MOVE  ALL '*'               TO  RUCA67-QTD-ITEM-FINCD-X
           MOVE  ALL '*'               TO  RUCA67-PERC-CST-EFT-TOTAL

           MOVE  RUCA67-COR0005-9      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-9)
           ADD   LENGTH OF RUCA67-COR0005-9
                                       TO  ACU-TAM
                                           ACU-POSICAO

BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     PERFORM  5250-FORMATAR-GRUPO-PROPT.
BRQ001

BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     PERFORM  5260-FORMATAR-GRUPO-CORGN.
BRQ001

WP1122*----------------------------------------------------------------*
-
WP1122     PERFORM  5085-TRATAR-GRUPO-DESCLASS

7COMMP*** MONTAR GRUPO SICOR 5.07 - INICIO
.
.          MOVE '****'                 TO RUCA67-COD-PROGRAMA
.                                         RUCA67-SUB-PROGRAMA
.          MOVE RUCA67-COR0005-DEST    TO WRK-MSGEM-RURAL(ACU-POSICAO:
.                                         LENGTH OF RUCA67-COR0005-DEST)
.
.          ADD LENGTH OF RUCA67-COR0005-DEST
.                                      TO ACU-TAM
.                                         ACU-POSICAO.
.
7COMMP*** MONTAR GRUPO SICOR 5.07 - FIM

STEFAB     PERFORM 6020-DADOS-ENCARGOS-FINANC

STE001***  PercJurosEncargoFinanc
STEFAB*** XMOVE ALL '*'                TO RUCA67-PERCJUROSENCARGOFINANC
STE001***  PercBonusCAR
STE001     MOVE ALL '*'                TO RUCA67-PERCBONUSCAR
STE001     MOVE RUCA67-COR0005-JUROS   TO WRK-MSGEM-RURAL(ACU-POSICAO:
STE001                                    LENGTH OF
STE001                                    RUCA67-COR0005-JUROS)
STE001     ADD  LENGTH                 OF RUCA67-COR0005-JUROS
STE001                                 TO ACU-TAM
STE001                                    ACU-POSICAO.

STEFAB*** MONTAR GRUPO SICOR 5.09 - INICIO
.
.     ***  NumParcl
.          MOVE ALL '*'                TO RUCAC5-NUM-PARCL-X
.     ***  DtPrvPgto
.          MOVE ALL '*'                TO RUCAC5-DT-PRV-PAGTO
.     ***  VlrPrincipalParcL
.          MOVE ALL '*'                TO RUCAC5-VLR-PRINC-X
.          MOVE RUCAC5-COR0005-16A     TO WRK-MSGEM-RURAL(ACU-POSICAO:
.                                         LENGTH OF
.                                         RUCAC5-COR0005-16A)
.          ADD  LENGTH                 OF RUCAC5-COR0005-16A
.                                      TO ACU-TAM
.                                         ACU-POSICAO.
.
.     ***  CNPJBaseInstExigldd
.     *   xIF  CCNPJ-INSTC-EXIGB       OF RUCAB015
.     *                                GREATER ZEROS
.     *       xMOVE CCNPJ-INSTC-EXIGB  OF RUCAB015
.     *                                TO WRK-CCNPJ-INSTC-EXIG
.     *       xMOVE WRK-CCNPJ-INSTC-EXIG-N
.     *                               xTO RUCAC5-CNPJ-BASE-INST-EXIGIB
.     *   xELSE
.     *       xMOVE ALL '*'           xTO RUCAC5-CNPJ-BASE-INST-EXIGIB
.     *   xEND-IF
.
.          MOVE ALL '*'                TO RUCAC5-CNPJ-BASE-INST-EXIGIB
.
.          MOVE RUCAC5-COR0005-CNPJ    TO WRK-MSGEM-RURAL(ACU-POSICAO:
.                                         LENGTH OF
.                                         RUCAC5-COR0005-CNPJ)
.          ADD  LENGTH                 OF RUCAC5-COR0005-CNPJ
.                                      TO ACU-TAM
.                                         ACU-POSICAO.
.
7C5.10     MOVE ALL '*'                TO RUCA67-COR0005-BONUS-SUST
.
.          MOVE RUCA67-COR0005-BONUS-SUST
.                                        TO WRK-MSGEM-RURAL(ACU-POSICAO:
.                                         LENGTH OF
.                                         RUCA67-COR0005-BONUS-SUST)
.          ADD  LENGTH                 OF RUCA67-COR0005-BONUS-SUST
.                                      TO ACU-TAM
7C5.10                                    ACU-POSICAO.

7C5.10     MOVE ALL '*'                TO RUCA67-GP-COR0005-CONFDD.
.
.          MOVE RUCA67-COR0005-16B     TO WRK-MSGEM-RURAL(ACU-POSICAO:
.                                         LENGTH OF
.                                         RUCA67-COR0005-16B)
.          ADD  LENGTH                 OF RUCA67-COR0005-16B
.                                      TO ACU-TAM
7C5.10                                    ACU-POSICAO.
.
STEFAB*** MONTAR GRUPO SICOR 5.09 - FIM
           MOVE  WRK-DATA-ATUAL        TO  RUCA67-DTMOVTO

NAV506     MOVE  RUCA67-COR0005-17     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
NAV506                                     LENGTH OF RUCA67-COR0005-17)
NAV506     ADD   LENGTH OF RUCA67-COR0005-17
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5015-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-OBTER-DATA-HORA            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-7600-DATA-HORA

           CALL 'BRAD7600'          USING  WRK-7600-DATA-HORA

           MOVE  WRK-7600-DT-AAAAMMDD  TO  WRK-S11-DRESS
           MOVE  WRK-N08-DRESS         TO  WRK-DATA-ATUAL
           MOVE  WRK-7600-ANO-TIMES    TO  WRK-ANO-TIMES
           MOVE  WRK-7600-MES-TIMES    TO  WRK-MES-TIMES
           MOVE  WRK-7600-DIA-TIMES    TO  WRK-DIA-TIMES
           MOVE  WRK-7600-HOR-TIMES    TO  WRK-HOR-TIMES
           MOVE  WRK-7600-MIN-TIMES    TO  WRK-MIN-TIMES
           MOVE  WRK-7600-SEG-TIMES    TO  WRK-SEG-TIMES
           MOVE  WRK-7600-MIL-TIMES    TO  WRK-MIL-TIMES.

      *----------------------------------------------------------------*
       5020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

CG0523*----------------------------------------------------------------*
CG0523 5021-OBTER-AGENCIA-ORIGINAL SECTION.
CG0523*----------------------------------------------------------------*
CG0523
CG0523     MOVE 237                   TO  CBCO-DSTNA        OF TRAGB013
CG0523     MOVE CJUNC-DEPDC OF RUFIB002
CG0523                                TO  CAG-BCRIA-DSTNO   OF TRAGB013
CG0523     MOVE 07                    TO  CGRP-CTBIL-ATUAL  OF TRAGB013
CG0523     MOVE 05                    TO  CSGRP-CTBIL       OF TRAGB013
CG0523     MOVE CCTA-CORR   OF RUFIB002
CG0523                                TO  CCTA-MOVTC-DSTNO  OF TRAGB013
CG0523
CG0523     EXEC SQL
CG0523         SELECT
CG0523               CAG_BCRIA,
CG0523               DCTBIL_PREVT
CG0523         INTO
CG0523               :TRAGB013.CAG-BCRIA,
CG0523               :TRAGB013.DCTBIL-PREVT
CG0523         FROM  DB2PRD.PROCS_TRAG_CONCL
CG0523         WHERE CBCO_DSTNA          =  :TRAGB013.CBCO-DSTNA
CG0523          AND  CAG_BCRIA_DSTNO     =  :TRAGB013.CAG-BCRIA-DSTNO
CG0523          AND  CGRP_CTBIL_ATUAL    =  :TRAGB013.CGRP-CTBIL-ATUAL
CG0523          AND  CSGRP_CTBIL         =  :TRAGB013.CSGRP-CTBIL
CG0523          AND  CCTA_MOVTC_DSTNO    =  :TRAGB013.CCTA-MOVTC-DSTNO
                AND  DCAD_PREVT          >  '01.01.0001'
                FETCH FIRST 1 ROWS ONLY
CG0523     END-EXEC.
CG0523
CG0523     IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -811) OR
CG0523        ( SQLWARN0            EQUAL  'W'            )
CG0523         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
CG0523         MOVE 'PROCS_TRAG_CONCL'
CG0523                                 TO  ERR-DBD-TAB
CG0523         MOVE 'SELECT'           TO  ERR-FUN-COMANDO
CG0523         MOVE  SQLCODE           TO  ERR-SQL-CODE
CG0523         MOVE '0033'             TO  ERR-LOCAL
CG0523         MOVE  SPACES            TO  ERR-SEGM
CG0523         MOVE  99                TO  COMU-8814-COD-RETORNO
CG0523         MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
CG0523         MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
CG0523         MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
CG0523         PERFORM  8000-FINALIZAR
CG0523     END-IF.
CG0523
CG0523     IF  SQLCODE              EQUAL  +100
CG0523         MOVE  'N'            TO  WRK-TRAG
CG0523     END-IF.
CG0523
CG0523     MOVE DCTBIL-PREVT OF TRAGB013 TO WRK-DATA-DB2
CG0523     MOVE WRK-DIA-DB2              TO WRK-DIA-TRAG
CG0523     MOVE WRK-MES-DB2              TO WRK-MES-TRAG
CG0523     MOVE WRK-ANO-DB2              TO WRK-ANO-TRAG.
CG0523
CG0523*----------------------------------------------------------------*
CG0523 5021-99-FIM.  EXIT.
CG0523*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5025-CHAMAR-MESU9032            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      WRK-9032-DADOS
                                           WRK-9032-BRAD7100
                                           WRK-9032-SQLCA
                                           WRK-9032-AMBIENTE
                                           WRK-9032-IO-PCB
                                           WRK-9032-ALT-PCB

           MOVE  'MESU9032'            TO  WRK-MODULO

CG0523     IF WRK-TRAG = 'N'
CG0523        MOVE CJUNC-DEPDC         OF  RUFIB002
CG0523                                 TO  WRK-S11-DRESS
CG0523     ELSE
CG0523        IF WRK-DATA-CONTRATO     >=  WRK-DATA-TRAG
CG0523           MOVE CJUNC-DEPDC      OF  RUFIB002
CG0523                                 TO  WRK-S11-DRESS
CG0523        ELSE
CG0523           MOVE CAG-BCRIA        OF  TRAGB013
CG0523                                 TO  WRK-S11-DRESS
CG0523        END-IF
CG0523     END-IF.

           MOVE  WRK-N05-DRESS         TO  WRK-9032-AGENCIA
           MOVE  'O'                   TO  WRK-9032-AMBIENTE

           CALL  WRK-MODULO         USING  WRK-9032-DADOS
                                           WRK-9032-BRAD7100
                                           WRK-9032-SQLCA
                                           WRK-9032-AMBIENTE
                                           WRK-9032-IO-PCB
                                           WRK-9032-ALT-PCB

           EVALUATE  WRK-9032-COD-RETORNO

           WHEN  ZEROS
                 CONTINUE

           WHEN  01
                 MOVE  08              TO  COMU-8814-COD-RETORNO
                 MOVE  WRK-MSG-08      TO  COMU-8814-MENSAGEM
                 PERFORM  8000-FINALIZAR

           WHEN  OTHER
                 MOVE  99              TO  COMU-8814-COD-RETORNO
                 MOVE  WRK-9032-BRAD7100
                                       TO  COMU-8814-ERRO-AREA
                 MOVE  WRK-9032-SQLCA  TO  COMU-8814-SQLCA
                 MOVE  WRK-MSG-99      TO  COMU-8814-MENSAGEM
                 PERFORM  8000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       5025-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5030-OBTER-CREFT-RECOR          SECTION.
      *----------------------------------------------------------------*

           PERFORM  5035-SELECT-MAX-RUCAB012

           IF  SQLCODE              EQUAL  ZEROS
               PERFORM  5040-SELECT-RUCAB012
           END-IF.

           IF  CREFT-RECOR OF RUCAB012  EQUAL  SPACES
               IF  CREFT-RECOR OF RUFIB002 IS       NUMERIC  AND
                   CREFT-RECOR OF RUFIB002 GREATER  ZEROS
                   MOVE  CREFT-RECOR OF RUFIB002
                                       TO  RUCA67-NUMREF-BCCOROR
               ELSE
                   MOVE  CREFT-BACEN OF RUFIB002
                                       TO  WRK-S11-DRESS
                   MOVE  WRK-X11-DRESS TO  RUCA67-NUMREF-BCCOROR
               END-IF
           ELSE
               MOVE  CREFT-RECOR OF RUCAB012
                                       TO  RUCA67-NUMREF-BCCOROR
           END-IF.

      *----------------------------------------------------------------*
       5030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5035-SELECT-MAX-RUCAB012        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB012
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB012

           EXEC SQL
               SELECT
                       MAX(NENVIO_RECOR)
                 INTO
                      :RUCAB012.NENVIO-RECOR
                 FROM  DB2PRD.TCTRL_ENVIO_MANUT
                WHERE  CCONTR    = :RUCAB012.CCONTR
                  AND  HINCL_REG = :RUCAB012.HINCL-REG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -305 ) OR
              ( SQLWARN0            EQUAL  'W'                     )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100 OR -305
               MOVE  ZEROS             TO  NENVIO-RECOR     OF RUCAB012
               MOVE  SPACES            TO  CREFT-RECOR      OF RUCAB012
           END-IF.

      *----------------------------------------------------------------*
       5035-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5040-SELECT-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT
                       CREFT_RECOR
                 INTO
                      :RUCAB012.CREFT-RECOR
                        :WRK-B012-CREFT-RECOR
                 FROM  DB2PRD.TCTRL_ENVIO_MANUT
                WHERE  CCONTR       = :RUCAB012.CCONTR
                  AND  HINCL_REG    = :RUCAB012.HINCL-REG
                  AND  NENVIO_RECOR = :RUCAB012.NENVIO-RECOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  SPACES            TO  CREFT-RECOR      OF RUCAB012
           ELSE
               IF  WRK-B012-CREFT-RECOR    EQUAL  -1
                   MOVE  SPACES        TO  CREFT-RECOR      OF RUCAB012
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5040-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5045-OBTER-MOTIVO-PRORROG       SECTION.
      *----------------------------------------------------------------*

           PERFORM  5050-SELECT-RUFIB093

           IF  SQLCODE              EQUAL  ZEROS
               PERFORM  5055-SELECT-RURCB00A
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               MOVE CBASE-LEGAL-RENEG  OF  RURCB00A (3:3)
                                       TO  RUCA67-CBASE-LEGAL-PRO
           ELSE
               MOVE  ALL '*'           TO  RUCA67-DTVENC-X
                                           RUCA67-CBASE-LEGAL-PRO
           END-IF.

      *----------------------------------------------------------------*
       5045-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5050-SELECT-RUFIB093            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUFIB093

           EXEC SQL
               SELECT
                       NRENEG_CONTR_RURAL           ,
                       NBASE_LEGAL_RENEG            ,
                       DRENEG_CONTR_RURAL
                 INTO
                      :RUFIB093.NRENEG-CONTR-RURAL  ,
                      :RUFIB093.NBASE-LEGAL-RENEG
                        :WRK-B093-NBASE-LEGAL-RENEG ,
                      :RUFIB093.DRENEG-CONTR-RURAL

                 FROM  DB2PRD.TRENEG_CONTR_RURAL
                WHERE  CCONTR          = :RUFIB093.CCONTR
                ORDER  BY NRENEG_CONTR_RURAL DESC
                FETCH  FIRST 1 ROW ONLY
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TRENEG_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B093-NBASE-LEGAL-RENEG  EQUAL -1
                   MOVE ZEROS          TO NBASE-LEGAL-RENEG OF RUFIB093
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5050-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5055-SELECT-RURCB00A            SECTION.
      *----------------------------------------------------------------*

           MOVE  NBASE-LEGAL-RENEG     OF  RUFIB093
                                       TO  NBASE-LEGAL-RENEG OF RURCB00A

           EXEC SQL
               SELECT
                       CBASE_LEGAL_RENEG
                 INTO
                      :RURCB00A.CBASE-LEGAL-RENEG
                 FROM  DB2PRD.TBASE_LEGAL_RENEG
                WHERE  NBASE_LEGAL_RENEG = :RURCB00A.NBASE-LEGAL-RENEG
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 ) OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TBASE_LEGAL_RENEG'
                                       TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0006'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5055-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5060-TRATAR-GRUPO-LIBERACAO     SECTION.
      *----------------------------------------------------------------*

           PERFORM  5070-OPEN-CSR01-RUCAB017
           PERFORM  5075-FETCH-CSR01-RUCAB017

           MOVE  DVCTO-PCELA-CONTR     OF  RUCAB017
                                       TO  WRK-DVCTO-PCELA-CONTR-ANT
           MOVE  ZEROS                 TO  ACU-VPCELA

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

                IF  DVCTO-PCELA-CONTR OF RUCAB017
                                NOT EQUAL  WRK-DVCTO-PCELA-CONTR-ANT
                    PERFORM  5065-MOVER-LIBERACAO

                    MOVE  DVCTO-PCELA-CONTR OF  RUCAB017
                                       TO  WRK-DVCTO-PCELA-CONTR-ANT
                    MOVE  ZEROS        TO  ACU-VPCELA
                END-IF

                ADD VPCELA OF RUCAB017 TO  ACU-VPCELA

                PERFORM  5075-FETCH-CSR01-RUCAB017
           END-PERFORM.

           PERFORM  5080-CLOSE-CSR01-RUCAB017

           IF  WRK-DVCTO-PCELA-CONTR-ANT EQUAL  SPACES
               MOVE  ALL '*'           TO  RUCA67-DTLIBRC-RECUR
                                           RUCA67-VLR-LIBRC-RECUR

               MOVE  RUCA67-COR0005-4  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-4)
               ADD   LENGTH OF RUCA67-COR0005-4
                                       TO  ACU-TAM
                                           ACU-POSICAO
           ELSE
               PERFORM  5065-MOVER-LIBERACAO
           END-IF.

      *----------------------------------------------------------------*
       5060-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5065-MOVER-LIBERACAO            SECTION.
      *----------------------------------------------------------------*

           MOVE  ACU-VPCELA            TO  WRK-S13V99-DRESS
           MOVE  ZEROS                 TO  RUCA67-VLR-LIBRC-RECUR(1: 04)
           MOVE  WRK-X15-DRESS         TO  RUCA67-VLR-LIBRC-RECUR(5: 15)

           MOVE  WRK-DVCTO-PCELA-CONTR-ANT
                                       TO  WRK-DATA-DB2
           MOVE  WRK-DIA-DB2           TO  WRK-DIA-INV
           MOVE  WRK-MES-DB2           TO  WRK-MES-INV
           MOVE  WRK-ANO-DB2           TO  WRK-ANO-INV
           MOVE  WRK-DATA-INV-X        TO  RUCA67-DTLIBRC-RECUR

           MOVE  RUCA67-COR0005-4      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-4)
           ADD   LENGTH OF RUCA67-COR0005-4
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5065-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5070-OPEN-CSR01-RUCAB017        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB017
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB017
           MOVE  2                     TO  CTPO-PCELA       OF RUCAB017
           MOVE  ZEROS                 TO  NPCELA-CONTR     OF RUCAB017
           MOVE  ZEROS                 TO  NATULZ-PCELA     OF RUCAB017

           EXEC SQL
                OPEN  CSR01-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5070-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5075-FETCH-CSR01-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB017
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
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  SPACES            TO  DVCTO-PCELA-CONTR OF RUCAB017
           END-IF.

      *----------------------------------------------------------------*
       5075-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5080-CLOSE-CSR01-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5080-99-FIM.  EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
WP1122 5085-TRATAR-GRUPO-DESCLASS      SECTION.
-     *----------------------------------------------------------------*
-
-          PERFORM  5090-OPEN-CSR02-RUFIB044-JN
-          PERFORM  5095-FETCH-CSR02-RUFIB044-JN
-
************F  SQLCODE              EQUAL  +100
-              MOVE  ALL '*'           TO  RUCA67-DT-DESCLASS
-                                          RUCA67-MTV-DESCLASS
-
-              MOVE  RUCA67-COR0005-15 TO  WRK-MSGEM-RURAL(ACU-POSICAO:
-                                          LENGTH OF RUCA67-COR0005-15)
-              ADD   LENGTH OF RUCA67-COR0005-15
-                                      TO  ACU-TAM
-                                          ACU-POSICAO
NAV506
NAV506         MOVE  ALL '*'           TO  RUCA67-DT-DESCLASS-PAR
NAV506                                     RUCA67-VLR-DESCLASS-PAR
NAV506                                     RUCA67-MTV-DESCLASS-PAR
NAV506
NAV506         MOVE  RUCA67-COR0005-16 TO  WRK-MSGEM-RURAL(ACU-POSICAO:
NAV506                                     LENGTH OF RUCA67-COR0005-16)
NAV506         ADD   LENGTH OF RUCA67-COR0005-16
NAV506                                 TO  ACU-TAM
NAV506                                     ACU-POSICAO
************ND-IF.
-
-***********ERFORM
-*************NTIL  SQLCODE         EQUAL  +100
-
-**************OVE  DDCLAS-OPER-CREDT OF RUFIB044
-***************************************O  WRK-DATA-DB2
-**************OVE  WRK-DIA-DB2        TO  WRK-DIA-INV
-**************OVE  WRK-MES-DB2        TO  WRK-MES-INV
-**************OVE  WRK-ANO-DB2        TO  WRK-ANO-INV
-**************OVE  WRK-DATA-INV-X     TO  RUCA67-DT-DESCLASS
-
-**************OVE  CMOTVO-DCLAS-BACEN OF  RURCB097
-***************************************O  RUCA67-MTV-DESCLASS
-
-**************OVE  RUCA67-COR0005-15  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
-*******************************************ENGTH OF RUCA67-COR0005-15)
-**************DD   LENGTH OF RUCA67-COR0005-15
-***************************************O  ACU-TAM
-******                                    ACU-POSICAO
-
-*************ERFORM  5095-FETCH-CSR02-RUFIB044-JN
-***********ND-PERFORM.
-
-          PERFORM  5100-CLOSE-CSR02-RUFIB044-JN.
-
-     *----------------------------------------------------------------*
-      5085-99-FIM.  EXIT.
-     *----------------------------------------------------------------*
-
-     *----------------------------------------------------------------*
-      5090-OPEN-CSR02-RUFIB044-JN     SECTION.
-     *----------------------------------------------------------------*
-
-          MOVE  CCONTR OF RUCAB015    TO  CCONTR          OF  RUFIB044
-
-          EXEC SQL
-               OPEN  CSR02-RUFIB044-JN
-          END-EXEC.
-
-          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
-             ( SQLWARN0            EQUAL  'W'    )
-              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO  ERR-DBD-TAB
-              MOVE 'OPEN'             TO  ERR-FUN-COMANDO
-              MOVE  SQLCODE           TO  ERR-SQL-CODE
-              MOVE '0010'             TO  ERR-LOCAL
-              MOVE  SPACES            TO  ERR-SEGM
-              MOVE  99                TO  COMU-8814-COD-RETORNO
-              MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
-              MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
-              MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
-              PERFORM  8000-FINALIZAR
-          END-IF.
-
-     *----------------------------------------------------------------*
-      5090-99-FIM.  EXIT.
-     *----------------------------------------------------------------*
-
-     *----------------------------------------------------------------*
-      5095-FETCH-CSR02-RUFIB044-JN    SECTION.
-     *----------------------------------------------------------------*
-
-          EXEC SQL
-               FETCH  CSR02-RUFIB044-JN
-               INTO   :RUFIB044.CMOTVO-DCLAS-OPER  ,
-                      :RUFIB044.DDCLAS-OPER-CREDT  ,
-                      :RUFIB044.VPRINC-DCLAS-CONTR ,
-                      :RURCB097.CMOTVO-DCLAS-BACEN
-          END-EXEC.
-
-          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
-             ( SQLWARN0            EQUAL  'W'             )
-              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO  ERR-DBD-TAB
-              MOVE 'FETCH'            TO  ERR-FUN-COMANDO
-              MOVE  SQLCODE           TO  ERR-SQL-CODE
-              MOVE '0011'             TO  ERR-LOCAL
-              MOVE  SPACES            TO  ERR-SEGM
-              MOVE  99                TO  COMU-8814-COD-RETORNO
-              MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
-              MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
-              MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
-              PERFORM  8000-FINALIZAR
-          END-IF.
-
-          IF  SQLCODE              EQUAL  +100
-              MOVE  SPACES            TO  DVCTO-PCELA-CONTR OF RUCAB017
-          END-IF.
-
-     *----------------------------------------------------------------*
-      5095-99-FIM.  EXIT.
-     *----------------------------------------------------------------*
-
-     *----------------------------------------------------------------*
-      5100-CLOSE-CSR02-RUFIB044-JN    SECTION.
-     *----------------------------------------------------------------*
-
-          EXEC SQL
-               CLOSE CSR02-RUFIB044-JN
-          END-EXEC.
-
-          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
-             ( SQLWARN0            EQUAL  'W'    )
-              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO  ERR-DBD-TAB
-              MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
-              MOVE  SQLCODE           TO  ERR-SQL-CODE
-              MOVE '0012'             TO  ERR-LOCAL
-              MOVE  SPACES            TO  ERR-SEGM
-              MOVE  99                TO  COMU-8814-COD-RETORNO
-              MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
-              MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
-              MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
-              PERFORM  8000-FINALIZAR
-          END-IF.
-
-     *----------------------------------------------------------------*
-      5100-99-FIM.  EXIT.
WP1122*----------------------------------------------------------------*


      *----------------------------------------------------------------*
       5105-TRATAR-GRUPO-COOPERADO     SECTION.
      *----------------------------------------------------------------*

           PERFORM  5110-OPEN-CSR03-RUFIB092
           PERFORM  5115-FETCH-CSR03-RUFIB092

           IF  SQLCODE              EQUAL  +100
               MOVE  ALL '*'           TO  RUCA67-TPSSOA-COOP
7C5.10                                     RUCA67-CNPJ-CPF-COOP
                                           RUCA67-VLR-PCELA-COOP
                                           RUCA67-CODPROG-LINCRED

               MOVE  RUCA67-COR0005-6  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-6)
               ADD   LENGTH OF RUCA67-COR0005-6
                                       TO  ACU-TAM
                                           ACU-POSICAO
           END-IF.

           PERFORM
             UNTIL  SQLCODE         EQUAL  +100

                IF  CTPO-PSSOA OF RUFIB092  EQUAL  1
                    MOVE  'F'          TO  RUCA67-TPSSOA-COOP
                    MOVE  CCNPJ-CPF-COOP   OF RUFIB092
                                       TO  WRK-S11-DRESS
                    MOVE  WRK-N09-DRESS
                                       TO  WRK-NUM-CPF-AUX
                    MOVE  CCTRL-CPF-COOP   OF RUFIB092
                                       TO  WRK-S11-DRESS
                    MOVE  WRK-N02-DRESS
                                       TO  WRK-CTRL-CPF-AUX
                    MOVE  WRK-CPF-AUX  TO  RUCA67-CNPJ-CPF-COOP
                ELSE
                    MOVE  'J'          TO  RUCA67-TPSSOA-COOP
                    MOVE  CCNPJ-CPF-COOP   OF RUFIB092
                                       TO  WRK-S11-DRESS
                    MOVE  WRK-N08-DRESS
                                       TO  WRK-NUM-CNPJ-AUX
                    MOVE  CFLIAL-CNPJ-COOP OF RUFIB092
                                       TO  WRK-S11-DRESS
                    MOVE  WRK-N04-DRESS
                                       TO  WRK-FILIAL-CNPJ-AUX
                    MOVE  CCTRL-CPF-COOP   OF RUFIB092
                                       TO  WRK-S11-DRESS
                    MOVE  WRK-N02-DRESS
                                       TO  WRK-CTRL-CNPJ-AUX
                    MOVE  WRK-CNPJ-AUX TO  RUCA67-CNPJ-CPF-COOP
                END-IF

                MOVE  VPCELA-COOP-RURAL    OF RUFIB092
                                       TO  WRK-S13V99-DRESS
                MOVE  ZEROS            TO  RUCA67-VLR-PCELA-COOP(1:04)
                MOVE  WRK-X15-DRESS    TO  RUCA67-VLR-PCELA-COOP(5:15)

                IF CPROG-RURAL OF RUFIB092 EQUAL  ZEROS OR SPACES
                   MOVE  '0999'        TO  RUCA67-CODPROG-LINCRED
                ELSE
                   MOVE  CPROG-RURAL   OF  RUFIB092
                                       TO  RUCA67-CODPROG-LINCRED
                END-IF

                MOVE  RUCA67-COR0005-6 TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-6)
                ADD   LENGTH OF RUCA67-COR0005-6
                                       TO  ACU-TAM
                                           ACU-POSICAO
                PERFORM  5115-FETCH-CSR03-RUFIB092

           END-PERFORM.

           PERFORM  5120-CLOSE-CSR03-RUFIB092.

      *----------------------------------------------------------------*
       5105-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-OPEN-CSR03-RUFIB092        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUCAB015    TO  CCONTR           OF RUFIB092
           MOVE  ZEROS                 TO  NID-COOP-RURAL   OF RUFIB092

           EXEC SQL
                OPEN CSR03-RUFIB092
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TINFO_COOP_CONTR' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0013'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5115-FETCH-CSR03-RUFIB092       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUFIB092
                INTO   :RUFIB092.CTPO-PSSOA        ,
                       :RUFIB092.CCNPJ-CPF-COOP    ,
                       :RUFIB092.CFLIAL-CNPJ-COOP  ,
                       :RUFIB092.CCTRL-CPF-COOP    ,
                       :RUFIB092.VPCELA-COOP-RURAL ,
                       :RUFIB092.CPROG-RURAL
                         :WRK-B092-CPRODT-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TINFO_COOP_CONTR' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0014'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               IF  WRK-B092-CPRODT-RURAL  EQUAL  -1
                   MOVE  SPACES        TO  CPROG-RURAL      OF RUFIB092
               END-IF
           ELSE
               MOVE  SPACES            TO  CPROG-RURAL      OF RUFIB092
           END-IF.

      *----------------------------------------------------------------*
       5115-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5120-CLOSE-CSR03-RUFIB092       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUFIB092
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TINFO_COOP_CONTR' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0015'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5120-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5125-SELECT-RURCB000            SECTION.
      *----------------------------------------------------------------*

           MOVE  CORIGE-REC            OF  RUCAB015
                                       TO  CORIGE-REC       OF RURCB000

           EXEC SQL
                SELECT
                        CPLANO_REFIN_RURAL
                  INTO
                        :RURCB000.CPLANO-REFIN-RURAL
                  FROM  DB2PRD.ORIGEM_RECURSO
                 WHERE  CORIGE_REC  = :RURCB000.CORIGE-REC

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORIGEM_RECURSO'   TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0016'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  06                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-06        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5125-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5130-SELECT-RURCB028            SECTION.
      *----------------------------------------------------------------*

           MOVE  CPLANO-REFIN-RURAL    OF  RURCB000
                                       TO  CPLANO-REFIN     OF RURCB028

           EXEC SQL
                SELECT
                        CPLANO_REFIN_RECOR
                  INTO
                        :RURCB028.CPLANO-REFIN-RECOR
                  FROM  DB2PRD.PLANO_REFIN
                 WHERE  CPLANO_REFIN     = :RURCB028.CPLANO-REFIN

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'PLANO_REFIN'      TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0017'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  07                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-07        TO  COMU-8814-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       5130-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       5135-FORMATAR-GRUPO-GLEBA       SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  WRK-CSEQ-MATR-RURAL-ANT
                                           ACU-QTD-GLEBA

STEFAB** DEFINIDO NO DIA 23/9/2024 QUE AO ENVIAR COR005, NAO ENVIAR
STEFAB** COORDENDADAS - SEJA ELA, CULTIVADAS OU NAO

STEFAB     PERFORM 5175-TRATAR-SEM-GLEBA.

STEFAB*   xPERFORM  5140-PROCURAR-GLEBA

STEFAB*   xIF  ACU-QTD-GLEBA        EQUAL  ZEROS
.     *      xxPERFORM  5175-TRATAR-SEM-GLEBA
STEFAB*   xEND-IF.

      *----------------------------------------------------------------*
       5135-99-FIM.  EXIT.
      *----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5136-TRATAR-GLEBA-EXCLUIDA      S3CTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*    M0VE RUCA67-COR0005-8B TO WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                 LENGTH OF RUCA67-COR0005-8B)
7C2510*   XADD LENGTH OF RUCA67-COR0005-8B TO ACU-TAM
7C2510*                                       ACU-POSICAO
STE001*
7C2510*    M0VE ALL '*'           TO RUCA67-LAT-PONTO-EXCL
7C2510*    M0VE ALL '*'           TO RUCA67-LONG-PONTO-EXCL
7C2510*    M0VE ALL '*'           TO RUCA67-ALTT-PONTO-EXCL
7C2510*    M0VE RUCA67-COR0005-8BB TO WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                 LENGTH OF RUCA67-COR0005-8BB)
7C2510*   XADD LENGTH OF RUCA67-COR0005-8BB TO ACU-TAM
7C2510*                                       ACU-POSICAO.
.     *----------------------------------------------------------------*
7C2510*5136-99-FIM.  EXIT.
STE001*----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5140-PROCURAR-GLEBA             SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*    P3RFORM  5180-OPEN-CSR04-RUECB040
7C2510*    P3RFORM  5185-FETCH-CSR04-RUECB040
7C2510*
7C2510*    P3RFORM
7C2510*      UNTIL  WRK-FIM-CSR04-RUECB040  EQUAL  'S'
7C2510*
7C2510*         P3RFORM  5145-OBTER-PONTOS-GEO
7C2510*
7C2510*         1F  WRK-TEM-GLEBA   EQUAL  'S'
7C2510*             P3RFORM  5170-TRATAR-AREA-NAO-CULT
7C2510*         END-IF
7C2510*
7C2510*         P3RFORM  5185-FETCH-CSR04-RUECB040
7C2510*    END-PERFORM.
7C2510*
7C2510*    P3RFORM  5190-CLOSE-CSR04-RUECB040.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5140-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5145-OBTER-PONTOS-GEO           SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*    M0VE  ZEROS                 TO  WRK-NSEQ-GLEBA-IMOV-ANT
7C2510*    M0VE  'N'                   TO  WRK-TEM-GLEBA
7C2510*
7C2510*    P3RFORM  5195-OPEN-CSR05-RUCAB016
7C2510*    P3RFORM  5200-FETCH-CSR05-RUCAB016
7C2510*
7C2510*    1F  SQLCODE              EQUAL  ZEROS
7C2510*        P3RFORM  5150-OBTER-COORD-COM-ALT
7C2510*    3LSE
7C2510*        P3RFORM  5160-OBTER-COORD-SEM-ALT
7C2510*    END-IF.
7C2510*
7C2510*    P3RFORM  5205-CLOSE-CSR05-RUCAB016.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5145-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*
7C2510*----------------------------------------------------------------*
7C2510*5150-OBTER-COORD-COM-ALT        SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*   XPERFORM
7C2510*      UNTIL  SQLCODE     EQUAL  +100
7C2510*
7C2510*        XIF  NSEQ-GLEBA-IMOV OF RUCAB016  GREATER  ZEROS
7C2510*            xPERFORM  5155-FORMATAR-COORD-ALT
7C2510*         END-IF
7C2510*
7C2510*        XPERFORM  5200-FETCH-CSR05-RUCAB016
7C2510*    END-PERFORM.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5150-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5155-FORMATAR-COORD-ALT         SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*   XIF  WRK-CSEQ-MATR-RURAL-ANT NOT EQUAL
7C2510*                                  CSEQ-MATR-RURAL OF RUCAB016  OR
7C2510*        WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
7C2510*                                  NSEQ-GLEBA-IMOV OF RUCAB016
7C2510*
7C2510*       XIF  WRK-NSEQ-GLEBA-IMOV-ANT GREATER  ZEROS
7C2510*           XPERFORM  5170-TRATAR-AREA-NAO-CULT
7C2510*        END-IF
7C2510*
7C2510*       XMOVE  CSEQ-MATR-RURAL   OF  RUCAB016
7C2510*                                TO  WRK-CSEQ-MATR-RURAL-ANT
7C2510*                                    WRK-S11-DRESS
7C2510*       XMOVE  WRK-N03-DRESS     TO  WRK-SEQ-MATR-N03
7C2510*       XMOVE  NSEQ-GLEBA-IMOV   OF  RUCAB016
7C2510*                                TO  WRK-NSEQ-GLEBA-IMOV-ANT
7C2510*                                    WRK-S11-DRESS
7C2510*       XMOVE  WRK-N05-DRESS     TO  WRK-SEQ-GLEBA-N05
7C2510*       XMOVE  WRK-SEQ-GLEBA-X19 TO  RUCA67-IDENTC-GLEBA
7C2510*
7C2510*       XMOVE  RUCA67-COR0005-7  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA67-COR0005-7)
7C2510*       XADD   LENGTH OF RUCA67-COR0005-7
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
7C2510*    END-IF.
7C2510*
7C2510*   XMOVE  MLATTD-IMOV-RURAL     OF  RUCAB016
7C2510*                                TO  WRK-MLATTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MLATTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-LAT-PONTO
7C2510*
7C2510*   XMOVE  MLOGTD-IMOV-RURAL     OF  RUCAB016
7C2510*                                TO  WRK-MLOGTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MLOGTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-LONG-PONTO
7C2510*
7C2510*   XMOVE  MALTTD-IMOV-RURAL     OF  RUCAB016
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-ALTT-PONTO
7C2510*
7C2510*   XADD   MAREA-NAO-CTVDA       OF  RUCAB016
7C2510*                                TO  ACU-MAREA-NAO-CTVDA
7C2510*
7C2510*   XMOVE  RUCA67-COR0005-8      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA67-COR0005-8)
7C2510*   XADD   LENGTH OF RUCA67-COR0005-8
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
7C2510*
7C2510*   XMOVE  'S'                   TO  WRK-TEM-GLEBA
7C2510*
7C2510*   XADD   1                     TO  ACU-QTD-GLEBA.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5155-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5160-OBTER-COORD-SEM-ALT        SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*   XPERFORM  5210-OPEN-CSR06-RUECB09N
7C2510*   XPERFORM  5215-FETCH-CSR06-RUECB09N
7C2510*
7C2510*   XPERFORM
7C2510*      UNTIL  SQLCODE    EQUAL  +100
7C2510*
7C2510*        XPERFORM  5165-FORMATAR-COORD-SEM-ALT
7C2510*        XPERFORM  5215-FETCH-CSR06-RUECB09N
7C2510*    END-PERFORM.
7C2510*
7C2510*   XPERFORM  5220-CLOSE-CSR06-RUECB09N.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5160-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5165-FORMATAR-COORD-SEM-ALT     SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*   XIF  WRK-CSEQ-MATR-RURAL-ANT NOT EQUAL
7C2510*                                  CSEQ-MATR-RURAL OF RUECB09N  OR
7C2510*        WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
7C2510*                                  NSEQ-GLEBA-IMOV OF  RUECB09N
7C2510*
7C2510*       XIF  WRK-NSEQ-GLEBA-IMOV-ANT GREATER  ZEROS
7C2510*           XPERFORM  5170-TRATAR-AREA-NAO-CULT
7C2510*        END-IF
7C2510*
7C2510*       XMOVE  CSEQ-MATR-RURAL   OF  RUECB09N
7C2510*                                TO  WRK-CSEQ-MATR-RURAL-ANT
7C2510*                                    WRK-S11-DRESS
7C2510*       XMOVE  WRK-N03-DRESS     TO  WRK-SEQ-MATR-N03
7C2510*       XMOVE  NSEQ-GLEBA-IMOV   OF  RUECB09N
7C2510*                                TO  WRK-NSEQ-GLEBA-IMOV-ANT
7C2510*                                    WRK-S11-DRESS
7C2510*       XMOVE  WRK-N05-DRESS     TO  WRK-SEQ-GLEBA-N05
7C2510*       XMOVE  WRK-SEQ-GLEBA-X19 TO  RUCA67-IDENTC-GLEBA
7C2510*
7C2510*       XMOVE  RUCA67-COR0005-7  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA67-COR0005-7)
7C2510*       XADD   LENGTH OF RUCA67-COR0005-7
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
7C2510*    END-IF.
7C2510*
7C2510*   XMOVE  MLATTD-IMOV-RURAL     OF  RUECB09N
7C2510*                                TO  WRK-MLATTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MLATTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-LAT-PONTO
7C2510*
7C2510*   XMOVE  MLOGTD-IMOV-RURAL     OF  RUECB09N
7C2510*                                TO  WRK-MLOGTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MLOGTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-LONG-PONTO
7C2510*
7C2510*   XMOVE  MALTTD-IMOV-RURAL     OF  RUECB09N
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA67-ALTT-PONTO
7C2510*
7C2510*   XMOVE  RUCA67-COR0005-8      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA67-COR0005-8)
7C2510*   XADD   LENGTH OF RUCA67-COR0005-8
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
7C2510*
7C2510*   XMOVE  'S'                   TO  WRK-TEM-GLEBA
7C2510*
7C2510*   XADD   1                     TO  ACU-QTD-GLEBA.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5165-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*

7C2510*----------------------------------------------------------------*
7C2510*5170-TRATAR-AREA-NAO-CULT       SECTION.
7C2510*----------------------------------------------------------------*
7C2510*
7C2510*   XMOVE  ACU-MAREA-NAO-CTVDA   TO  WRK-MAREA-NAO-CTVDA-N
7C2510*   XMOVE  WRK-MAREA-NAO-CTVDA-X TO  RUCA67-AREA-NAO-CULT
7C2510*
7C2510*   XMOVE  ALL '*'               TO  RUCA67-COR0005-8A
7C2510*   XMOVE  RUCA67-COR0005-8A     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA67-COR0005-8A)
7C2510*   XADD   LENGTH OF RUCA67-COR0005-8A
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
7C2510*
7C2510*   XMOVE  ZEROS                 TO  ACU-MAREA-NAO-CTVDA.
7C2510*
7C2510***  TRATAMENTO GLEBA EXCLUIDA
ST2510*    P3RFORM  5136-TRATAR-GLEBA-EXCLUIDA.
7C2510*
7C2510*----------------------------------------------------------------*
7C2510*5170-99-FIM.  EXIT.
7C2510*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5175-TRATAR-SEM-GLEBA           SECTION.
      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA67-IDENTC-GLEBA.
           MOVE  ALL '*'               TO  RUCA67-AREA-NAO-CULT.

           MOVE  RUCA67-COR0005-7      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-7)
           ADD   LENGTH OF RUCA67-COR0005-7
                                       TO  ACU-TAM
                                           ACU-POSICAO

7C2510*    M0VE  ALL '*'               TO  RUCA67-LAT-PONTO
7C2510*    M0VE  ALL '*'               TO  RUCA67-LONG-PONTO
7C2510*    M0VE  ALL '*'               TO  RUCA67-ALTT-PONTO
7c2510     MOVE  ALL '*'               to  RUCA67-WKTPART
           MOVE  RUCA67-COR0005-8      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA67-COR0005-8)
           ADD   LENGTH OF RUCA67-COR0005-8
                                       TO  ACU-TAM
                                           ACU-POSICAO.


7c2510*   xMOVE  RUCA67-COR0005-8A     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7c2510*                                    LENGTH OF RUCA67-COR0005-8A)
7c2510*   xADD   LENGTH OF RUCA67-COR0005-8A
7c2510*                                TO  ACU-TAM
7c2510*                                    ACU-POSICAO.

STE001***  TRATAMENTO GLEBA EXCLUIDA
7c2510*   xPERFORM  5136-TRATAR-GLEBA-EXCLUIDA.

      *----------------------------------------------------------------*
       5175-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5180-OPEN-CSR04-RUECB040        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUCAB015    TO  CCONTR           OF RUECB040
           MOVE  ZEROS                 TO  CSEQ-MATR-RURAL  OF RUECB040
           MOVE  'N'                   TO  WRK-FIM-CSR04-RUECB040

           EXEC SQL
                OPEN CSR04-RUECB040
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0018'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5180-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5185-FETCH-CSR04-RUECB040       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR04-RUECB040
                INTO   :RUECB040.CCONTR            ,
                       :RUECB040.CSEQ-MATR-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0019'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR04-RUECB040
           END-IF.

      *----------------------------------------------------------------*
       5185-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5190-CLOSE-CSR04-RUECB040       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR04-RUECB040
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0020'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5190-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5195-OPEN-CSR05-RUCAB016        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB015
                                       TO  CCONTR            OF RUCAB016
           MOVE  HINCL-REG             OF  RUCAB015
                                       TO  HINCL-REG         OF RUCAB016
           MOVE  CSEQ-MATR-RURAL       OF  RUECB040
                                       TO  CSEQ-MATR-RURAL   OF RUCAB016
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUCAB016
                                           NCORND-GEOGR-IMOV OF RUCAB016

           EXEC SQL
                OPEN CSR05-RUCAB016
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0021'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5195-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-FETCH-CSR05-RUCAB016       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR05-RUCAB016
                INTO   :RUCAB016.CCONTR            ,
                       :RUCAB016.HINCL-REG         ,
                       :RUCAB016.CSEQ-MATR-RURAL   ,
                       :RUCAB016.NSEQ-GLEBA-IMOV   ,
                       :RUCAB016.NCORND-GEOGR-IMOV ,
                       :RUCAB016.MLATTD-IMOV-RURAL ,
                       :RUCAB016.MLOGTD-IMOV-RURAL ,
                       :RUCAB016.MALTTD-IMOV-RURAL ,
                       :RUCAB016.MAREA-NAO-CTVDA
                         :WRK-B016-MAREA-NAO-CTVDA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0022'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
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
       5200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5205-CLOSE-CSR05-RUCAB016       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR05-RUCAB016
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0023'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5205-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5210-OPEN-CSR06-RUECB09N        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUECB040
                                       TO  CCONTR            OF RUECB09N
           MOVE  CSEQ-MATR-RURAL       OF  RUECB040
                                       TO  CSEQ-MATR-RURAL   OF RUECB09N
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUECB09N
                                           NCORND-GEOGR-IMOV OF RUECB09N

           EXEC SQL
                OPEN CSR06-RUECB09N
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0024'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5215-FETCH-CSR06-RUECB09N       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR06-RUECB09N
                INTO   :RUECB09N.CCONTR            ,
                       :RUECB09N.CSEQ-MATR-RURAL   ,
                       :RUECB09N.NSEQ-GLEBA-IMOV   ,
                       :RUECB09N.NCORND-GEOGR-IMOV ,
                       :RUECB09N.MLATTD-IMOV-RURAL ,
                       :RUECB09N.MLOGTD-IMOV-RURAL ,
                       :RUECB09N.MALTTD-IMOV-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0025'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5215-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5220-CLOSE-CSR06-RUECB09N       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR06-RUECB09N
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCORND_GEOGR_CONTR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0026'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5225-SALVAR-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5020-OBTER-DATA-HORA
           PERFORM  5230-INSERT-RUCAB012
           PERFORM  5235-INSERT-RUCAB019
           PERFORM  5240-INSERT-RUCAB013.

      *----------------------------------------------------------------*
       5225-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5230-INSERT-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB012
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB012
           MOVE  NENVIO-RECOR OF RUCAB012
                                       TO  ACU-NENVIO-RECOR
           ADD   1                     TO  ACU-NENVIO-RECOR
           MOVE  ACU-NENVIO-RECOR      TO  WRK-NENVIO-RECOR
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB012
           MOVE  SPACES                TO  CCTRL-RQUIS-FINCR
                                                            OF RUCAB012
           MOVE  -1                    TO  WRK-B012-CCTRL-RQUIS-FINCR
           MOVE  SPACES                TO  CUNIC-MSGEM      OF RUCAB012
           MOVE  -1                    TO  WRK-B012-CUNIC-MSGEM

           IF  CREFT-RECOR OF RUCAB012  EQUAL  SPACES
               IF  CREFT-RECOR OF RUFIB002 IS       NUMERIC  AND
                   CREFT-RECOR OF RUFIB002 GREATER  ZEROS
                   MOVE  CREFT-RECOR OF RUFIB002
                                       TO  CREFT-RECOR      OF RUCAB012
               ELSE
                   MOVE  CREFT-BACEN   OF  RUFIB002
                                       TO  WRK-S11-DRESS
                   MOVE  WRK-X11-DRESS TO  CREFT-RECOR  OF RUCAB012
               END-IF
           END-IF.

           MOVE  ZEROS                 TO  WRK-B012-CREFT-RECOR
           MOVE  WRK-TIMESTAMP         TO  HINCL            OF RUCAB012
           MOVE  COMU-8814-USUARIO     TO  CFUNC-BDSCO      OF RUCAB012

           EXEC SQL
                INSERT  INTO DB2PRD.TCTRL_ENVIO_MANUT
                     (  CCONTR                         ,
                        HINCL_REG                      ,
                        NENVIO_RECOR                   ,
                        CCTRL_RQUIS_FINCR              ,
                        CUNIC_MSGEM                    ,
                        CREFT_RECOR                    ,
                        HINCL                          ,
                        CFUNC_BDSCO                    )
                VALUES
                     ( :RUCAB012.CCONTR                ,
                       :RUCAB012.HINCL-REG             ,
                       :RUCAB012.NENVIO-RECOR          ,
                       :RUCAB012.CCTRL-RQUIS-FINCR
                         :WRK-B012-CCTRL-RQUIS-FINCR   ,
                       :RUCAB012.CUNIC-MSGEM
                         :WRK-B012-CUNIC-MSGEM         ,
                       :RUCAB012.CREFT-RECOR
                         :WRK-B012-CREFT-RECOR         ,
                       :RUCAB012.HINCL                 ,
                       :RUCAB012.CFUNC-BDSCO           )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCTRL_ENVIO_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0027'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5230-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5235-INSERT-RUCAB019            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB012 TO  CCONTR           OF RUCAB019
           MOVE  HINCL-REG OF RUCAB012 TO  HINCL-REG        OF RUCAB019
           MOVE  NENVIO-RECOR          OF  RUCAB012
                                       TO  NENVIO-RECOR     OF RUCAB019
           MOVE  1                     TO  NREG-MSGEM-RECOR OF RUCAB019
           MOVE  1                     TO  CSIT-MSGEM       OF RUCAB019
           MOVE  SPACES                TO  CMSGEM-RECOR     OF RUCAB019
           MOVE  -1                    TO  WRK-B019-CMSGEM-RECOR

           EXEC SQL
                INSERT  INTO DB2PRD.TSIT_MANUT_RECOR
                     (  CCONTR                         ,
                        HINCL_REG                      ,
                        NENVIO_RECOR                   ,
                        NREG_MSGEM_RECOR               ,
                        CSIT_MSGEM                     ,
                        CMSGEM_RECOR                   )
                VALUES
                     ( :RUCAB019.CCONTR                ,
                       :RUCAB019.HINCL-REG             ,
                       :RUCAB019.NENVIO-RECOR          ,
                       :RUCAB019.NREG-MSGEM-RECOR      ,
                       :RUCAB019.CSIT-MSGEM            ,
                       :RUCAB019.CMSGEM-RECOR
                         :WRK-B019-CMSGEM-RECOR        )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TSIT_MANUT_RECOR' TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0028'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5235-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5240-INSERT-RUCAB013            SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB019 TO  CCONTR           OF RUCAB013
           MOVE  HINCL-REG OF RUCAB019 TO  HINCL-REG        OF RUCAB013
           MOVE  NENVIO-RECOR          OF  RUCAB019
                                       TO  NENVIO-RECOR     OF RUCAB013
           MOVE  NREG-MSGEM-RECOR OF RUCAB019
                                       TO  NREG-MSGEM-RECOR OF RUCAB013
           MOVE  1                     TO  CTPO-CONTD-MSGEM OF RUCAB013
           MOVE  WRK-MES-TIMES         TO  CPTCAO-TBELA     OF RUCAB013

           MOVE  ACU-TAM               TO  WCONTD-MSGEM-RURAL-LEN
                                                            OF RUCAB013
           MOVE  WRK-MSGEM-RURAL       TO  WCONTD-MSGEM-RURAL-TEXT
                                                            OF RUCAB013
           MOVE  WRK-TIMESTAMP         TO  HMOVTO-MSGEM-RECOR
                                                            OF RUCAB013

           EXEC SQL
                INSERT  INTO DB2PRD.TENVIO_RETOR_MANUT
                     (  CCONTR                         ,
                        HINCL_REG                      ,
                        NENVIO_RECOR                   ,
                        NREG_MSGEM_RECOR               ,
                        CTPO_CONTD_MSGEM               ,
                        CPTCAO_TBELA                   ,
                        WCONTD_MSGEM_RURAL             ,
                        HMOVTO_MSGEM_RECOR             )
                VALUES
                     ( :RUCAB013.CCONTR                ,
                       :RUCAB013.HINCL-REG             ,
                       :RUCAB013.NENVIO-RECOR          ,
                       :RUCAB013.NREG-MSGEM-RECOR      ,
                       :RUCAB013.CTPO-CONTD-MSGEM      ,
                       :RUCAB013.CPTCAO-TBELA          ,
                       :RUCAB013.WCONTD-MSGEM-RURAL    ,
                       :RUCAB013.HMOVTO-MSGEM-RECOR    )
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND  -803 )  OR
              ( SQLWARN0            EQUAL  'W'              )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RETOR_MANUT'
                                       TO  ERR-DBD-TAB
               MOVE 'INSERT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0029'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8814-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5240-99-FIM.  EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001 5250-FORMATAR-GRUPO-PROPT       SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     PERFORM 5251-OPEN-CSR07-RUCAB018.
BRQ001     PERFORM 5252-FETCH-CSR07-RUCAB018 UNTIL SQLCODE EQUAL +100.
BRQ001     PERFORM 5253-CLOSE-CSR07-RUCAB018.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5250-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5251-OPEN-CSR07-RUCAB018        SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE  CCONTR    OF RUCAB015 TO  CCONTR      OF RUCAB018.
BRQ001     MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG   OF RUCAB018.
BRQ001
BRQ001     EXEC SQL
BRQ001          OPEN  CSR07-RUCAB018
BRQ001     END-EXEC.
BRQ001
BRQ001     IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
BRQ001        ( SQLWARN0            EQUAL  'W'    )
BRQ001         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BRQ001         MOVE 'TMANUT_PPRIE_IMOV_'
BRQ001                                 TO  ERR-DBD-TAB
BRQ001         MOVE 'OPEN'             TO  ERR-FUN-COMANDO
BRQ001         MOVE  SQLCODE           TO  ERR-SQL-CODE
BRQ001         MOVE '0030'             TO  ERR-LOCAL
BRQ001         MOVE  SPACES            TO  ERR-SEGM
BRQ001         MOVE  99                TO  COMU-8814-COD-RETORNO
BRQ001         MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
BRQ001         MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
BRQ001         MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
BRQ001         PERFORM  8000-FINALIZAR
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5251-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5252-FETCH-CSR07-RUCAB018       SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     EXEC SQL
BRQ001          FETCH  CSR07-RUCAB018
BRQ001          INTO   :RUCAB018.CCNPJ-CPF         ,
BRQ001                 :RUCAB018.CFLIAL-CNPJ       ,
BRQ001                 :RUCAB018.CCTRL-CNPJ-CPF    ,
BRQ001                 :RUCAB018.CTPO-PSSOA        ,
BRQ001                 :RUCAB018.CNIRF-IMOV-RURAL
BRQ001                   :WRK-CNIRF-IMOV-RURAL-NULL,
BRQ001                 :RUCAB018.CCCIR
BRQ001                   :WRK-CCCIR-NULL           ,
BRQ001                 :RUCAB018.CREG-AMBTL-RURAL
BRQ001                   :WRK-CREG-AMBTL-RURAL-NULL,
BRQ001                 :RUCAB018.PPRESV-AMBTL-PPRIE
BRQ001                   :WRK-PPRESV-AMBTL-PPRIE-NULL,
BRQ001                 :RUCAB018.COUTGA-AGUA-IMOV
BRQ001                   :WRK-COUTGA-AGUA-IMOV-NULL
BRQ001     END-EXEC.
BRQ001
BRQ001     IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
BRQ001        ( SQLWARN0            EQUAL  'W'             )
BRQ001         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BRQ001         MOVE 'TMANUT_PCELA_RECOR'
BRQ001                                 TO  ERR-DBD-TAB
BRQ001         MOVE 'FETCH'            TO  ERR-FUN-COMANDO
BRQ001         MOVE  SQLCODE           TO  ERR-SQL-CODE
BRQ001         MOVE '0031'             TO  ERR-LOCAL
BRQ001         MOVE  SPACES            TO  ERR-SEGM
BRQ001         MOVE  99                TO  COMU-8814-COD-RETORNO
BRQ001         MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
BRQ001         MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
BRQ001         MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
BRQ001         PERFORM  8000-FINALIZAR
BRQ001     END-IF.
BRQ001
BRQ001     IF  SQLCODE              EQUAL  +100
BRQ001         IF ACU-QTD-PROPT     EQUAL ZEROS
BRQ001            MOVE ALL '*'      TO RUCA67-GP-COR0005-PROPT
BRQ001            MOVE RUCA67-COR0005-10
BRQ001                              TO WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                              LENGTH OF RUCA67-COR0005-10)
BRQ001            ADD  LENGTH OF RUCA67-COR0005-10
BRQ001                              TO ACU-TAM
BRQ001                                 ACU-POSICAO
BRQ001         END-IF
BRQ001         GO TO 5252-99-FIM
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-CNIRF-IMOV-RURAL-NULL LESS ZEROS
BRQ001         MOVE SPACES             TO CNIRF-IMOV-RURAL OF RUCAB018
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-CCCIR-NULL          LESS ZEROS
BRQ001         MOVE SPACES             TO CCCIR            OF RUCAB018
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-CREG-AMBTL-RURAL-NULL LESS ZEROS
BRQ001         MOVE SPACES             TO CREG-AMBTL-RURAL OF RUCAB018
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-PPRESV-AMBTL-PPRIE-NULL LESS ZEROS
BRQ001         MOVE ZEROS              TO PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-COUTGA-AGUA-IMOV-NULL   LESS ZEROS
BRQ001         MOVE SPACES             TO COUTGA-AGUA-IMOV OF RUCAB018
BRQ001     END-IF.
BRQ001
BRQ001     ADD 1                       TO ACU-QTD-PROPT.
BRQ001
BRQ001     PERFORM 5254-TRATA-DADOS-PROPT.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5252-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5253-CLOSE-CSR07-RUCAB018       SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     EXEC SQL
BRQ001          CLOSE CSR07-RUCAB018
BRQ001     END-EXEC.
BRQ001
BRQ001     IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
BRQ001        ( SQLWARN0            EQUAL  'W'    )
BRQ001         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
BRQ001         MOVE 'TMANUT_PPRIE_IMOV_'
BRQ001                                 TO  ERR-DBD-TAB
BRQ001         MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
BRQ001         MOVE  SQLCODE           TO  ERR-SQL-CODE
BRQ001         MOVE '0032'             TO  ERR-LOCAL
BRQ001         MOVE  SPACES            TO  ERR-SEGM
BRQ001         MOVE  99                TO  COMU-8814-COD-RETORNO
BRQ001         MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
BRQ001         MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
BRQ001         MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
BRQ001         PERFORM  8000-FINALIZAR
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5253-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5254-TRATA-DADOS-PROPT          SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     IF CTPO-PSSOA OF RUCAB018   EQUAL 1
BRQ001        MOVE 'F'                 TO RUCA67-TPSSOA-PROPT
BRQ001        MOVE CCNPJ-CPF OF RUCAB018
BRQ001                                 TO WRK-COD9-CPF
BRQ001        MOVE CCTRL-CNPJ-CPF OF RUCAB018
BRQ001                                 TO WRK-COD2-NUM-S
BRQ001        MOVE WRK-COD9-CPF-9      TO WRK-EDIT-NCPF
BRQ001        MOVE WRK-COD2-NUM        TO WRK-EDIT-CCPF
BRQ001        MOVE WRK-EDIT-CPF        TO RUCA67-CNPJ-CPF-PROPT
BRQ001     ELSE
BRQ001        MOVE 'J'                 TO RUCA67-TPSSOA-PROPT
BRQ001        MOVE CCNPJ-CPF OF RUCAB018
BRQ001                                 TO WRK-COD9-NUM-S
BRQ001        MOVE WRK-COD9-NUM8-XS    TO WRK-CNPJ-BAS-PRO
BRQ001        MOVE '000'               TO WRK-CNPJ-BAS-PRO-ZEROS
BRQ001        MOVE WRK-CNPJBASE-PROPT  TO RUCA67-CNPJ-CPF-PROPT
BRQ001     END-IF.
BRQ001
BRQ001     IF CNIRF-IMOV-RURAL OF RUCAB018 EQUAL SPACES OR LOW-VALUES
7C5.10        MOVE ALL '*'             TO RUCA67-CIB
BRQ001     ELSE
BRQ001        MOVE CNIRF-IMOV-RURAL OF RUCAB018(2:8)
7C5.10                                 TO RUCA67-CIB
BRQ001     END-IF.
BRQ001
BRQ001     IF CCCIR            OF RUCAB018 EQUAL SPACES OR LOW-VALUES
BRQ001        MOVE ALL '*'             TO RUCA67-COD-SNCR
BRQ001     ELSE
BRQ001        MOVE CCCIR            OF RUCAB018
BRQ001                                 TO RUCA67-COD-SNCR
BRQ001     END-IF.
BRQ001
BRQ001     IF CREG-AMBTL-RURAL OF RUCAB018 EQUAL SPACES OR LOW-VALUES
BRQ001        MOVE ALL '*'             TO RUCA67-NUM-REG-CAR
BRQ001     ELSE
BRQ001        MOVE CREG-AMBTL-RURAL OF RUCAB018
BRQ001                                 TO RUCA67-NUM-REG-CAR
BRQ001     END-IF.
BRQ001
BRQ001     IF COUTGA-AGUA-IMOV OF RUCAB018 EQUAL SPACES OR LOW-VALUES
BRQ001        MOVE ALL '*'             TO RUCA67-COUTGA-AGUA
BRQ001     ELSE
BRQ001        MOVE COUTGA-AGUA-IMOV OF RUCAB018
BRQ001                                 TO RUCA67-COUTGA-AGUA
BRQ001     END-IF.
BRQ001
BRQ001     IF PPRESV-AMBTL-PPRIE OF RUCAB018 EQUAL ZEROS
BRQ001        MOVE ALL '*'            TO RUCA67-PPRESV-AMBTL
BRQ001     ELSE
BRQ001        MOVE PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001                                 TO WRK-PIC9-3V99-CS
BRQ001        MOVE WRK-PIC9-3V99-SS    TO RUCA67-PPRESV-AMBTL-N
BRQ001     END-IF.
BRQ001
BRQ001     MOVE RUCA67-COR0005-10
BRQ001                              TO WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                              LENGTH OF RUCA67-COR0005-10)
BRQ001     ADD  LENGTH OF RUCA67-COR0005-10
BRQ001                              TO ACU-TAM
BRQ001                                 ACU-POSICAO.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5254-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5260-FORMATAR-GRUPO-CORGN       SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
CAPG05*    -IF CCETFC-ORGNC-EMPTO OF RUCAB015 EQUAL SPACES
CAPG05*       -MOVE ALL '*'             TO RUCA67-GP-COR0005-CORGN
CAPG05*    -ELSE
CAPG05*       -MOVE CCETFC-ORGNC-EMPTO OF RUCAB015
CAPG05*                                TO RUCA67-CCETFC-ORGNC
CAPG05*       -MOVE EURL-CETFC-ORGNC-TEXT OF RUCAB015(1:255)
CAPG05*                                TO RUCA67-EURL-CETFC-ORGNC
CAPG05*    END-IF.
CAPG05*
CAPG05*    -MOVE RUCA67-COR0005-11
CAPG05*                             TO WRK-MSGEM-RURAL(ACU-POSICAO:
CAPG05*                             LENGTH OF RUCA67-COR0005-11)
CAPG05*    -ADD  LENGTH OF RUCA67-COR0005-11
CAPG05*                             TO ACU-TAM
CAPG05*                                ACU-POSICAO
CAPG05*
CAPG05*    -IF CRSTRB-EMPTO-RURAL OF RUCAB015 EQUAL SPACES
CAPG05*       -MOVE ALL '*'             TO RUCA67-GP-COR0005-RASTB
CAPG05*    -ELSE
CAPG05*       -MOVE CRSTRB-EMPTO-RURAL OF RUCAB015
CAPG05*                                TO RUCA67-CRSTRB-EMPTO
CAPG05*       -MOVE EURL-RSTRB-EMPTO-TEXT OF RUCAB015(1:255)
CAPG05*                                TO RUCA67-EURL-RSTRB
CAPG05*    END-IF.
CAPG05*
CAPG05*    -MOVE RUCA67-COR0005-12
CAPG05*                             TO WRK-MSGEM-RURAL(ACU-POSICAO:
CAPG05*                             LENGTH OF RUCA67-COR0005-12)
CAPG05*    -ADD  LENGTH OF RUCA67-COR0005-12
CAPG05*                             TO ACU-TAM
CAPG05*                                ACU-POSICAO
CAPG05*
CAPG05*    -IF PENERG-RENOV-EMPTO OF RUCAB015 EQUAL ZEROS
CAPG05*       -MOVE ALL '*'             TO RUCA67-PENERG-RENOV
CAPG05*    -ELSE
CAPG05*       -MOVE PENERG-RENOV-EMPTO OF RUCAB015
CAPG05*                                TO WRK-PIC9-3V99-CS
CAPG05*       -MOVE WRK-PIC9-3V99-SS    TO RUCA67-PENERG-RENOV-N
CAPG05*    END-IF.
CAPG05*
CAPG05*    -MOVE RUCA67-COR0005-13
CAPG05*                             TO WRK-MSGEM-RURAL(ACU-POSICAO:
CAPG05*                             LENGTH OF RUCA67-COR0005-13)
CAPG05*    -ADD  LENGTH OF RUCA67-COR0005-13
CAPG05*                             TO ACU-TAM
CAPG05*                                ACU-POSICAO.
WP0522
WP0522     IF CSCRET-TSORO-NACIO OF RUCAB015 EQUAL ZEROS
WP0522        MOVE ALL '*'             TO RUCA67-COD-STN-EQULZC
WP0522     ELSE
WP0522        MOVE CSCRET-TSORO-NACIO OF RUCAB015
WP0522                                 TO RUCA67-COD-STN-EQULZC
WP0522     END-IF.
WP0522
WP0522     MOVE RUCA67-COR0005-14
WP0522                              TO WRK-MSGEM-RURAL(ACU-POSICAO:
WP0522                              LENGTH OF RUCA67-COR0005-14)
WP0522     ADD  LENGTH OF RUCA67-COR0005-14
WP0522                              TO ACU-TAM
WP0522                                 ACU-POSICAO.
WP0522
BRQ001*----------------------------------------------------------------*
BRQ001 5260-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*

STEFAB*----------------------------------------------------------------*
.      6000-SELECT-RUECB048            SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE  COMU-8814-TERMINAL    TO  CTERM           OF RUECB048
.
.          EXEC SQL
.               SELECT
.                       CTERM                       ,
.                       CIDTFD_MDULO                ,
.                       CSEQ_CTRL_MDULO             ,
.                       WRESUL_CONS_MDULO
.                 INTO
.                       :RUECB048.CTERM             ,
.                       :RUECB048.CIDTFD-MDULO      ,
.                       :RUECB048.CSEQ-CTRL-MDULO   ,
.                       :RUECB048.WRESUL-CONS-MDULO
.                         :WRK-WRESUL-CONS-MDULO-NULL
.                 FROM  DB2PRD.TAPOIO_UTILZ_MDULO
.                WHERE  CTERM              = :RUECB048.CTERM
.                  AND  CIDTFD_MDULO       = :RUECB048.CIDTFD-MDULO
.                  AND  CSEQ_CTRL_MDULO    = :RUECB048.CSEQ-CTRL-MDULO
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND  +100 )  OR
.             ( SQLWARN0            EQUAL  'W'              )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TAPOIO_UTILZ_MDULO'
.                                      TO  ERR-DBD-TAB
.              MOVE 'SELECT'           TO  ERR-FUN-COMANDO
.              MOVE SQLCODE            TO  ERR-SQL-CODE
.              MOVE '0033'             TO  ERR-LOCAL
.              MOVE SPACES             TO  ERR-SEGM
.              MOVE  99                TO  COMU-8814-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8814-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8814-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8814-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF  SQLCODE              EQUAL  ZEROS
.              IF  WRK-WRESUL-CONS-MDULO-NULL  EQUAL  -1
.                  MOVE  ZEROS         TO  WRESUL-CONS-MDULO-LEN
.                                                           OF RUECB048
.                  MOVE  SPACES        TO  WRESUL-CONS-MDULO-TEXT
.                                                           OF RUECB048
.              END-IF
.          ELSE
.              MOVE  ZEROS             TO  WRESUL-CONS-MDULO-LEN
.                                                           OF RUECB048
.              MOVE  SPACES            TO  WRESUL-CONS-MDULO-TEXT
.                                                           OF RUECB048
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6000-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6020-DADOS-ENCARGOS-FINANC      SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE  954                   TO  CIDTFD-MDULO     OF RUECB048
.          MOVE  10000                 TO  CSEQ-CTRL-MDULO  OF RUECB048
.
.          PERFORM  6000-SELECT-RUECB048
.
.          MOVE WRESUL-CONS-MDULO-TEXT OF RUECB048
.                                (1: WRESUL-CONS-MDULO-LEN OF RUECB048)
.                                      TO  WRK-DADOS-NAV-T954
.
.
.          IF  WRK-954-STA-TXJURO      EQUAL  'S'
.              MOVE WRK-954-TXJURO-ATU TO WRK-SJUR-S
.              MOVE WRK-SJUR-X         TO RUCA67-PERCJUROSENCARGOFINANC
.          ELSE
.              MOVE ALL '*'            TO RUCA67-PERCJUROSENCARGOFINANC
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6020-99-FIM.  EXIT.
STEFAB*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*
