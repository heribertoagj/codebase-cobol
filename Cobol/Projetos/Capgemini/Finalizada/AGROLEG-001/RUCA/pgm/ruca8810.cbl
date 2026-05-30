      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID.    RUCA8810.
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
      *        FORMATAR MENSAGEM COR0001 DE ALTERACAO.                 *
      *                                                                *
      *----------------------------------------------------------------*
      *  MODULOS CHAMADOS:                                             *
      *                                                                *
      *     MESU9032 - OBTER DADOS DA DEPENDENCIA                      *
      *     RUEC8824 - OBTER COD. SUBPROGRAMA E OBRIGATORIEDADE GLEBA  *
      *     CMRU307C - CONSULTA BENEFICIARIO                           *
      *     RUEC8800 - OBTER SITEMA PRODUTIVO                          *
PRI005*     RUCA8516 - VERIFICAR CONTRATO LIQUIDADO                    *
      *     BRAD7600 - DATA/HORA                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *  BASES BD2:                                                    *
      *                                                                *
      *     RURCB003 - DB2PRD.FNALD_RURAL                              *
      *     RUCAB004 - DB2PRD.RUCA_CAD_DEP                             *
      *     RUCAB012 - DB2PRD.TCTRL_ENVIO_MANUT                        *
      *     RUCAB013 - DB2PRD.TENVIO_RETOR_MANUT                       *
      *     RUCAB015 - DB2PRD.TMANUT_CONTR_RECOR                       *
      *     RUCAB016 - DB2PRD.TMANUT_CORND_GEOGR                       *
      *     RUCAB017 - DB2PRD.TMANUT_PCELA_RECOR                       *
      *     RUCAB018 - DB2PRD.TMANUT_PPRIE_IMOV_RECOR                  *
      *     RUCAB019 - DB2PRD.TSIT_MANUT_RECOR                         *
      *     RUFIB002 - DB2PRD.CONTRATO_RURAL                           *
      *     RURCB000 - DB2PRD.ORIGEM_RECURSO                           *
      *     RURCB006 - DB2PRD.PRODUTO_RURAL                            *
      *     RURCB018 - DB2PRD.ESPCE_TITLO_GARNT                        *
      *     RURCB022 - DB2PRD.TPO_PROTR_RURAL                          *
      *     RURCB028 - DB2PRD.PLANO_REFIN                              *
      *     RUECB014 - DB2PRD.PESSOA_CONTR_RURAL                       *
      *     RUECB025 - DB2PRD.ORCAM_CDULA_RURAL                        *
      *     RUECB040 - DB2PRD.MATR_CONTR_RURAL                         *
      *     RUECB09G - DB2PRD.TPRODT_RURAL_CSCDO                       *
      *     RUECB09J - DB2PRD.TAPOLC_SEGUR_OBRIG                       *
      *     RUECB09N - DB2PRD.TCORND_GEOGR_CONTR                       *
CG0523*     TRAGB013 - DB2PRD.PROCS_TRAG_CONCL                         *
      *                                                                *
      *================================================================*
ONEDA *================================================================*
ONEDA *                      A L T E R A C A O                         *
ONEDA *================================================================*
ONEDA *    ANALISTA....:   ADILSON ONEDA    -   BRADESCO               *
ONEDA *    DATA........:   JULHO/2021                                  *
ONEDA *    OBJETIVO....:   NOTICIAS SICOR DE 07/05/2021                *
ONEDA *                    NAO ENVIAR AREA PARA OPERACOES COM COORDENA-*
ONEDA *                    DAS GEODESICAS                              *
ONEDA *================================================================*
BRQ001*----------------------------------------------------------------*
BRQ001*               U L T I M A  A L T E R A C A O                   *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*      DATA         : JUN/2021                                   *
BRQ001*                                                                *
BRQ001*      OBJETIVOS    : INCLUSAO DOS NOVOS CAMPOS PARA ENVIO DA    *
BRQ001*                     MENSAGEM COR0001.                          *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*
MJ0522*================================================================*
.     *  ANALISTA......:   MILTON JANUARIO          - DS               *
.     *  DATA..........:   MAIO/2022                                   *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   NAO ENVIAR DATAS DE PLANTIO E COLHEITA NA   *
.     *                    COR0001 QUANDO O PRODUTO FOR PERENE         *
MJ0522*================================================================*
WP1122*================================================================*
.     *  ANALISTA......:   WIPRO                    - WIPRO            *
.     *  DATA..........:   MAIO/2022                                   *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   RECOMPILAR - SICOR 5.05                     *
WP1122*================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
PRI005*                ALTERACAO - PRIME                               *
PRI005*----------------------------------------------------------------*
PRI005*                                                                *
PRI005* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI005* DATA...............: 31/01/2023                                *
PRI005* OBJETIVOS..........: MANUTENCAO DE CONTRATO LIQUIDADO          *
PRI005*================================================================*
CG0523*================================================================*
CG0523*    C A P G E M I N I                                           *
CG0523*----------------------------------------------------------------*
CG0523*    ANALISTA....:   RICARDO PEREIRA DA SILVA                    *
CG0523*    DATA........:   05/2023                                     *
CG0523*    PROJETO.....:   ALTERACAO DE CNPJ DO BANCO SICOR            *
CG0523*    OBJETIVO....:   VALIDACAO DE TRAG                           *
CG0523*================================================================*
MJ1223*================================================================*
MJ1223*    DS - BRADESCO                                               *
MJ1223*----------------------------------------------------------------*
MJ1223*    ANALISTA....:   MILTON JANUARIO                             *
MJ1223*    DATA........:   12/2023                                     *
MJ1223*    PROJETO.....:   NOTICIAS SICOR 11/12/2023                   *
MJ1223*    OBJETIVO....:   ENVIO DAS DATAS DE INICIO E FIM DE COLHEITA *
MJ1223*                    PARA CUSTEIO DE CULTURAS PERENES            *
MJ1223*================================================================*
MJ1223*================================================================*
MJ0224*    DS - BRADESCO                                               *
MJ0224*----------------------------------------------------------------*
MJ0224*    ANALISTA....:   MILTON JANUARIO                             *
MJ0224*    DATA........:   02/2024                                     *
MJ0224*    PROJETO.....:   SICOR WEB                                   *
MJ0224*    OBJETIVO....:-  ENVIAR SEMPRE A DATA DE VENCIMENTO ORIGNAL  *
MJ0224*                    DA OPERAÇÃO                                 *
MJ0224*                 -  TRAG AGENCIA                                *
MJ0224*                                                                *
MJ0224*================================================================*
STE001*================================================================*
.     *  ANALISTA......:   STEFANINI                - STEFANINI        *
.     *  DATA..........:   MAIO/2024                                   *
.     *----------------------------------------------------------------*
.     *  OBJETIVOS.....:   RECOMPILAR - SICOR 5.08                     *
.     *                    INCLUSAO DE OUTRAS PRATICAS DE MANEJO NO    *
.     *                    SISTEMA DE PRODUCAO                         *
STE001*================================================================*
WI0824*================================================================*
WI0824*  ANALISTA......:   M.KAZUO                  - WIPRO            *
WI0824*  DATA..........:   AGO/2024                                    *
WI0824*----------------------------------------------------------------*
WI0824*  OBJETIVOS.....:   ATENDER INC0550338                          *
WI0824*                    AJUSTE NO ENVIO QUANDO ADITAMENTO PARA O    *
WI0824*                    CONTRATO 9192362                            *
WI0824*================================================================*
MJ0824*================================================================*
MJ0824*  ANALISTA......:   MILTON JANUARIO          - BRADESCO         *
MJ0824*  DATA..........:   AGOSTO/2024                                 *
MJ0824*----------------------------------------------------------------*
MJ0824*  OBJETIVOS.....:   NOTICIAS SICOR 24/07/2024 - ENVIO DE DATA   *
MJ0824*                    DE PLANTIO PARA CULTURA PERENE              *
MJ0824*================================================================*
WI0924*================================================================*
WI0924*  ANALISTA......:   M.KAZUO                  - WIPRO            *
WI0924*  DATA..........:   SET/2024                                    *
WI0924*----------------------------------------------------------------*
WI0924*  OBJETIVOS.....:   ATENDER INC0571401                          *
WI0924*                    AJUSTE NO ENVIO QUANDO OCORRER O ADITAMENTO *
WI0924*                    PARA O CONTRATO 9195977                     *
WI0924*================================================================*
WIP924*================================================================*
WIP924*  ANALISTA......:   M.KAZUO                  - WIPRO            *
WIP924*  DATA..........:   SET/2024                                    *
WIP924*----------------------------------------------------------------*
WIP924*  OBJETIVOS.....:   ATENDER INC0610938                          *
WIP924*                    AJUSTE NO ENVIO QUANDO OCORRER O ADITAMENTO *
WIP924*                    PARA O CONTRATO 9197016. FILIAL E CONTROLE  *
WIP924*                    DA AGENCIA TRAGUEADA.                       *
WIP924*================================================================*
WI1024*================================================================*
WI1024*  ANALISTA......:   M.KAZUO                  - WIPRO            *
WI1024*  DATA..........:   OUT/2024                                    *
WI1024*----------------------------------------------------------------*
WI1024*  OBJETIVOS.....:   ATENDER INC0632831                          *
WI1024*                    AJUSTE NO ENVIO QUANDO OCORRER O ADITAMENTO *
WI1024*                    PARA O CONTRATO 9190457. FILIAL E CONTROLE  *
WI1024*                    DA AGENCIA TRAGUEADA.                       *
WI1024*================================================================*

STE002*================================================================*
  .   *  ANALISTA......:   STEFANINI                - STEFANINI        *
  .   *  DATA..........:   FEVEREIRO / 2025                            *
  .   *----------------------------------------------------------------*
  .   *  PROJETO.......:   UNIDADE ANIMAL                              *
  .   *----------------------------------------------------------------*
  .   *  OBJETIVOS.....:   AJUSTE ENVIO COR01                          *
STE002*================================================================*
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
           '01 - CAMPO USUARIO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-02              PIC  X(75)          VALUE
           '02 - CAMPO CHAVE COM CONTEUDO INVALIDO'.
           05  WRK-MSG-03              PIC  X(75)          VALUE
           '03 - CAMPO HORA DE INCLUSAO COM CONTEUDO INVALIDO'.
           05  WRK-MSG-04              PIC  X(75)          VALUE
           '04 - CODIGO DO SUBPROGRAMA NAO ENCONTRADO'.
           05  WRK-MSG-05              PIC  X(75)          VALUE
           '05 - CONTRATO NAO LOCALIZADO NA BASE CONTRATO RURAL'.
           05  WRK-MSG-06              PIC  X(75)          VALUE
           '06 - CONTRATO NAO LOCALIZADO NA BASE DE MANUTENCOES'.
           05  WRK-MSG-07              PIC  X(75)          VALUE
           '07 - TIPO DE TITULO NAO LOCALIZADO'.
           05  WRK-MSG-08              PIC  X(75)          VALUE
           '08 - ORIGEM DO RECURSO NAO LOCALIZADA'.
           05  WRK-MSG-09              PIC  X(75)          VALUE
           '09 - FONTE DO RECURSO NAO LOCALIZADA'.
           05  WRK-MSG-11              PIC  X(75)          VALUE
           '11 - PRODUTO BACEN NAO LOCALIZADO'.
           05  WRK-MSG-12              PIC  X(75)          VALUE
           '12 - DATAS PLANTIO E COLHEITA SAO OBRIGATORIOS PARA CULTURAS
      -    ' NAO PERENES'.
           05  WRK-MSG-13              PIC  X(75)          VALUE
MJ0824     '13 - DATA DE PLANTIO OBRIGATORIA PARA CUSTEIO DE CULTURAS PE
MJ0824-    'RENES'.
           05  WRK-MSG-14              PIC  X(75)          VALUE
           '14 - DATA DE COLHEITA OBRIGATORIO PARA CUSTEIO DE CULTURAS P
      -    'ERENES'.
           05  WRK-MSG-15              PIC  X(75)          VALUE
           '15 - AGENCIA NAO ENCONTRADO NO MESU'.
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
           05  WRK-X08-DRESS           REDEFINES
               WRK-N08-DRESS           PIC  X(08).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(06).
           05  WRK-N06-DRESS           PIC  9(06).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(07).
           05  WRK-N05-DRESS           PIC  9(05).
           05  WRK-X05-DRESS           REDEFINES
               WRK-N05-DRESS           PIC  X(05).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(08).
           05  WRK-N04-DRESS           PIC  9(04).
           05  WRK-X04-DRESS           REDEFINES
               WRK-N04-DRESS           PIC  X(04).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(09).
           05  WRK-N03-DRESS           PIC  9(03).
           05  WRK-X03-DRESS           REDEFINES
               WRK-N03-DRESS           PIC  X(03).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(10).
           05  WRK-N02-DRESS           PIC  9(02).
           05  WRK-X02-DRESS           REDEFINES
               WRK-N02-DRESS           PIC  X(02).
       01  FILLER                      REDEFINES  WRK-S11-DRESS.
           05  FILLER                  PIC  X(11).
           05  WRK-N01-DRESS           PIC  9(01).

       01  WRK-S13V99-DRESS            PIC +9(13)V99      VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(01).
           05  WRK-X15-DRESS           PIC  X(15).
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(02).
           05  WRK-N12V99-DRESS        PIC  9(12)V99.
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(05).
           05  WRK-N09V99-DRESS        PIC  9(09)V99.
       01  FILLER                      REDEFINES  WRK-S13V99-DRESS.
           05  FILLER                  PIC  X(11).
           05  WRK-N03V99-DRESS        PIC  9(03)V99.

STE001 01  WRK-N01-DRESS-X.
STE001     05  WRK-N01-DRESS-9         PIC  9(01) VALUE ZEROS.

       01  WRK-S03V99-DRESS            PIC S9(03)V99       VALUE ZEROS.
       01  WRK-S05-DRESS               REDEFINES
           WRK-S03V99-DRESS            PIC S9(05).

       01  WRK-TEM-MANUTENCAO          PIC  X(001) VALUE 'N'.
B0923  01  WRK-REDUC-ALT.
B0923      05  WRK-REDUC-TXT           PIC  X(40)          VALUE
B0923      'PERCENTUAL DE REDUCAO CAR (RESOL. 5.078)'.
B0923      05  WRK-REDUC-X.
B0923          10  WRK-REDUC-NUM       PIC  9(03)V99       VALUE ZEROS.
B0923      05  FILLER                  PIC  X(28)          VALUE SPACES.

STEFA1 01  WRK-S9-2                PIC  9(02)    VALUE ZEROS.
.      01  FILLER REDEFINES WRK-S9-2.
STEFA1     05 WRK-9-2              PIC  X(02).



      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  IND-PARC                    PIC  9(05) COMP-3   VALUE ZEROS.
PRI005 01  WRK-IND-LIN                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  IND-LIN                     PIC  9(05) COMP-3   VALUE ZEROS.
       01  IND-LIN2                    PIC  9(05) COMP-3   VALUE ZEROS.
       01  IND-AUX                     PIC  9(05) COMP-3   VALUE ZEROS.
       01  IND-CHAR                    PIC  9(02) COMP-3   VALUE ZEROS.

7C2511*01  ACU-TAM                     PIC  9(04) COMP-3   VALUE ZEROS.
7C2511 01  ACU-TAM                     PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-POSICAO                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-NEW-POS                 PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-QTD-GLEBA               PIC  9(05) COMP-3   VALUE ZEROS.
7C2511*01  ACU-QTD-GLEBA-E             PIC  9(05) COMP-3   VALUE ZEROS.
       01  ACU-MAREA-NAO-CTVDA         PIC S9(09)V99
                                                  COMP-3   VALUE ZEROS.
       01  ACU-NENVIO-RECOR            PIC S9(05) COMP-3   VALUE ZEROS.
       01  ACU-NREG-MSGEM-RECOR        PIC S9(05) COMP-3   VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

7C5.10 01  WRK-CRUR240G                PIC  X(08) VALUE 'CRUR240G'.
STE001 01  WRK-INDICADOR-TELA          PIC  X(01) VALUE SPACES.
STE001 01  WRK-INDICADOR-TELA-X        PIC  X(01) VALUE SPACES.
       01  WRK-CSEQ-MATR-RURAL-ANT     PIC S9(03) COMP-3   VALUE ZEROS.
STEFA1 01  WRK-PRIMEIRA-VEZ            PIC  X(01)          VALUE SPACES.
       01  WRK-NSEQ-GLEBA-IMOV-ANT     PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-NENVIO-RECOR            PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-NREG-MSGEM-RECOR        PIC S9(05) COMP-3   VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(08)          VALUE SPACES.
PRI005 01  WRK-RUCA8516                PIC  X(08)          VALUE
PRI005                                                      'RUCA8516'.
       01  WRK-TEM-CONTINUACAO         PIC  X(01)          VALUE SPACES.
       01  WRK-ACHOU-PLANTIO           PIC  X(01)          VALUE SPACES.
       01  WRK-ACHOU-COLHEITA          PIC  X(01)          VALUE SPACES.
       01  WRK-ACHOU-TAXA              PIC  X(01)          VALUE SPACES.
CG0523 01  WRK-TRAG                    PIC  X(01)          VALUE SPACES.
       01  WRK-TEM-GLEBA               PIC  X(01)          VALUE SPACES.
STEFAB 01  WRK-GLEBA-7B                PIC  X(01)          VALUE SPACES.
STEFA2 01  WRK-GRAVOU                  PIC  X(01)          VALUE SPACES.
       01  WRK-REG-AUX                 PIC  X(27)          VALUE SPACES.
       01  WRK-FIM-CSR01               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR02               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR03               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR04               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR05               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR06               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR07               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR08               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR09               PIC  X(01)          VALUE SPACES.
       01  WRK-FIM-CSR10               PIC  X(01)          VALUE SPACES.
PRI005 01  WRK-FIM-CSR11               PIC  X(01)          VALUE SPACES.
STEFAB 01  WRK-FIM-CSR14               PIC  X(01)          VALUE SPACES.
STEFAB 01  WRK-FIM-CSR15               PIC  X(01)          VALUE SPACES.
7C5.10 01  WRK-FIM-CSR16               PIC  X(01)          VALUE SPACES.

       01  WRK-TAB16-RUCAB016.
           10 WRK-TAB16-CCONTR             PIC S9(9)V      COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-HINCL-REG          PIC X(26)       VALUE SPACES.
           10 WRK-TAB16-CSEQ-MATR-RURAL    PIC S9(3)V      COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-NSEQ-GLEBA-IMOV    PIC S9(5)V      COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-NCORND-GEOGR-IMOV  PIC S9(5)V      COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-MLATTD-IMOV-RURAL  PIC S9(3)V9(11) COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-MLOGTD-IMOV-RURAL  PIC S9(3)V9(11) COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-MALTTD-IMOV-RURAL  PIC S9(5)V9(2)  COMP-3
                                                           VALUE ZEROS.
           10 WRK-TAB16-MAREA-NAO-CTVDA    PIC S9(9)V9(2)  COMP-3
                                                           VALUE ZEROS.
STEFA1 01  WRK-TAB9N-RUECB09N.
.          10 WRK-TAB9N-CCONTR             PIC S9(9)V      COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-CSEQ-MATR-RURAL    PIC S9(3)V      COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-NSEQ-GLEBA-IMOV    PIC S9(5)V      COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-NCORND-GEOGR-IMOV  PIC S9(5)V      COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-MLATTD-IMOV-RURAL  PIC S9(3)V9(11) COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-MLOGTD-IMOV-RURAL  PIC S9(3)V9(11) COMP-3
.                                                          VALUE ZEROS.
.          10 WRK-TAB9N-MALTTD-IMOV-RURAL  PIC S9(5)V9(2)  COMP-3
.                                                          VALUE ZEROS.
.     *    X0 WRK-TAB9N-MAREA-NAO-CTVDA    PIC S9(9)V9(2)  COMP-3
STEFA1*                                                    XALUE ZEROS.
       01  WRK-NUMCTRLIF.
           05 WRK-NUMCTRLIF-FIXO1      PIC  X(008)         VALUE
              '0237RUC1'.
           05 WRK-NUMCTRLIF-ANO        PIC  9(004)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-MES        PIC  9(002)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-DIA        PIC  9(002)         VALUE ZEROS.
           05 WRK-NUMCTRLIF-FIXO2      PIC  X(004)         VALUE '0001'.

7C5.10 01  WRK-CNPJ-AUX                PIC  X(14)          VALUE SPACES.
       01  FILLER                      REDEFINES  WRK-CNPJ-AUX.
           05  WRK-NUM-CNPJ-AUX        PIC  9(08).
           05  WRK-FILIAL-CNPJ-AUX     PIC  9(04).
           05  WRK-CTRL-CNPJ-AUX       PIC  9(02).

       01  WRK-CPF-AUX                 PIC  9(11)          VALUE ZEROS.
       01  FILLER                      REDEFINES  WRK-CPF-AUX.
           05  WRK-NUM-CPF-AUX         PIC  9(09).
           05  WRK-CTRL-CPF-AUX        PIC  9(02).
       01  WRK-CPF-AUX-X               REDEFINES
           WRK-CPF-AUX                 PIC  X(11).

       01  WRK-CEDULA-RECOR            PIC  X(17)          VALUE SPACES.
       01  FILLER   REDEFINES  WRK-CEDULA-RECOR.
           05  WRK-CJUNC-DEPDC         PIC  9(05).
           05  WRK-CCDULA              PIC  9(12).

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
7C2510*    05  WRK-MLATTD-IMOV-RURAL-N PIC S9(02)V9(11)    VALUE ZEROS.
           05  WRK-MLATTD-IMOV-RURAL-N PIC S9(03)V9(11)    VALUE ZEROS.

7C2510 01  FILLER      REDEFINES WRK-MLATTD-IMOV-RURAL-X.
7C2510     05  WRK-MLATTD-IMOV-9V7     PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(3).

       01  WRK-MLOGTD-IMOV-RURAL-X.
           05  WRK-MLOGTD-IMOV-RURAL-N PIC S9(03)V9(11)    VALUE ZEROS.

7C2510 01  FILLER      REDEFINES WRK-MLOGTD-IMOV-RURAL-X.
7C2510     05  WRK-MLOGTD-IMOV-9V7     PIC S9(03)V9(07).
7C2510     05  FILLER                  PIC X(3).


       01  WRK-MALTTD-IMOV-RURAL-X.
           05  WRK-MALTTD-IMOV-RURAL-N PIC S9(04)V9(02)    VALUE ZEROS.

       01  WRK-MAREA-NAO-CTVDA-X.
           05  WRK-MAREA-NAO-CTVDA-N   PIC S9(09)V9(02)    VALUE ZEROS.

       01  WRK-CONTROLE-AUX            PIC  9(04) COMP     VALUE 128.
       01  FILLER                      REDEFINES  WRK-CONTROLE-AUX.
           05  FILLER                  PIC  X(01).
           05  WRK-BYTE-LIGADO         PIC  X(01).

BRQ001 01  WRK-PIC9-3V99-CS            PIC +9(03)V99       VALUE ZEROS.
BRQ001 01  FILLER                      REDEFINES  WRK-PIC9-3V99-CS.
BRQ001     05  FILLER                  PIC  X(01).
BRQ001     05  WRK-PIC9-3V99-SS        PIC  9(03)V99.

7C5.10 01  WRK-S9-5-AUX                 PIC +9(005)         VALUE ZEROS.
.      01  FILLER                      REDEFINES WRK-S9-5-AUX.
.          05 FILLER                   PIC 9(001).
7C5.10     05 WRK-9-5-AUX              PIC 9(005).

STEFA1 01  WRK-9-8-AUX                 PIC 9(008)          VALUE ZEROS.
.      01  FILLER                      REDEFINES WRK-9-8-AUX.
STEFA1     05 WRK-X-8-AUX              PIC X(008).

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
      *
       01  WRK-DVCTO-ORIGN-CONTR-AUX   PIC  X(08)          VALUE SPACES.
       01  WRK-DVCTO-CONTR-AUX         PIC  X(08)          VALUE SPACES.
      *
       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-MES-DB2             PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(01)          VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(04)          VALUE ZEROS.

       01  WRK-DATA-INI-PLANTIO.
           05  WRK-ANO-INI-PLANTIO     PIC  9(04)          VALUE ZEROS.
           05  WRK-MES-INI-PLANTIO     PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA-INI-PLANTIO     PIC  9(02)          VALUE ZEROS.

       01  WRK-DATA-FIM-PLANTIO.
           05  WRK-ANO-FIM-PLANTIO     PIC  9(04)          VALUE ZEROS.
           05  WRK-MES-FIM-PLANTIO     PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA-FIM-PLANTIO     PIC  9(02)          VALUE ZEROS.

       01  WRK-DATA-INI-COLHEITA.
           05  WRK-ANO-INI-COLHEITA    PIC  9(04)          VALUE ZEROS.
           05  WRK-MES-INI-COLHEITA    PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA-INI-COLHEITA    PIC  9(02)          VALUE ZEROS.

       01  WRK-DATA-FIM-COLHEITA.
           05  WRK-ANO-FIM-COLHEITA    PIC  9(04)          VALUE ZEROS.
           05  WRK-MES-FIM-COLHEITA    PIC  9(02)          VALUE ZEROS.
           05  WRK-DIA-FIM-COLHEITA    PIC  9(02)          VALUE ZEROS.

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
           '*** TABELA AUXILIAR DE PARCELAS ***'.
      *----------------------------------------------------------------*

       01  TAB-PARCELA.
           07  TAB-LINHA               OCCURS 1000 TIMES.
               10  TAB-DATA            PIC  9(08)          VALUE ZEROS.
               10  TAB-VALOR           PIC  X(19)          VALUE SPACES.

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

PRI005*----------------------------------------------------------------*
PRI005 01  FILLER                      PIC  X(50)          VALUE
PRI005     '*** AREAS DA POOL0004 ***'.
PRI005*----------------------------------------------------------------*
PRI005
PRI005*---------------------------------------------------------------*
PRI005*  AREA PARA TRANSFORMAR O CPO CONTROLE DO DB (BYTE1) DE BIT'S  *
PRI005*  PARA BYTES, PARA POSTERIOR TESTE DO BIT 2, SE ESTA DESLIGADO.*
PRI005*---------------------------------------------------------------*
PRI005 01  WRK-CONVERSAO              PIC 9(01)  VALUE ZEROS.
PRI005 01  WRK-BIT-BYTE               PIC 9(02)  VALUE ZEROS.
PRI005 01  WRK-CONTROLE               PIC X(08)  VALUE SPACES.
PRI005
PRI005 01  WRK-CONVERTIDO.
PRI005     03  FILLER                 PIC X(02)  VALUE SPACES.
PRI005     03  WRK-BIT2               PIC X(01)  VALUE SPACES.
PRI005     03  FILLER                 PIC X(37)  VALUE SPACES.
PRI005
PRI005 01  WRK-MSG-ERRO-PL04.
PRI005     03  FILLER                  PIC X(47)  VALUE
PRI005     '*** ERRO NO ACESSO A POOL0004 - RETURN-CODE => '.
PRI005     03  WRK-RETURN-CODE-PL04    PIC 9(02)  VALUE ZEROS.
PRI005     03  FILLER                  PIC X(14)  VALUE
PRI005     ' <=  LOCAL => '.
PRI005     03  WRK-LOCAL-PL04          PIC X(08)  VALUE SPACES.
PRI005     03  FILLER                  PIC X(04)  VALUE ' ***'.

7C5.10*----------------------------------------------------------------*
.      01  FILLER                      PIC  X(032)         VALUE
.          '*** AREA DO MODULO CRUR240G  ***'.
.     *----------------------------------------------------------------*
.
.      01  WRK-AREA-CRUR240G.
.          COPY CRURW00W.
7C5.10     COPY CRURWPGI.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO RUEC8824 ***'.
      *---------------------------------------------------------------*

       COPY 'RUECW824'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO CMRU307C ***'.
      *----------------------------------------------------------------*

       01  WRK-AREA-CMRU307C.
           COPY CMRUW09I.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DO MODULO RUEC8800 ***'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC58'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE FORMATACAO P/ MODULO RCOR5005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUCA66'.

PRI005*----------------------------------------------------------------*
PRI005 01  FILLER                      PIC  X(50)         VALUE
PRI005     '***  AREA PARA O PROGRAMA RUCA8516'.
PRI005*----------------------------------------------------------------*
PRI005
PRI005 COPY I#RUCAA3.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** AREAS PARA TRATAR NULO ***'.
      *----------------------------------------------------------------*

       01  WRK-B002-CMUN-RURAL         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CFNALD-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CATVDD-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CORIGE-REC         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CUND-MEDD-FINCD    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CESPCE-TITLO-GARNT PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CTPO-GARNT         PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CTPO-PROTR-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-VRECTA-BRUTA-EMPTO
                                       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CDECLR-AGCLT-FAMLR PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B002-CTPO-BNEFC-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B004-DTEVENTO           PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B006-CTEMPO-VIDA-CULTR  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CCTRL-RQUIS-FINCR  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B012-CUNIC-MSGEM        PIC S9(04)  COMP    VALUE ZEROS.
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
STE001 01  WRK-B015-COUTRO-TRATO-SOLO  PIC S9(04)  COMP    VALUE ZEROS.
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
BRQ001 01  WRK-CCETFC-ORGNC-EMPTO-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-EURL-CETFC-ORGNC-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-CRSTRB-EMPTO-RURAL-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-EURL-RSTRB-EMPTO-NULL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-PENERG-RENOV-EMPTO-NULL PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CTPO-SOLO-AGCLT-NULL    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CCICLO-CTVDA-NULL       PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL        PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CTPO-SEGUR-CONTT-NULL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B016-MAREA-NAO-CTVDA    PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B018-CNIRF-IMOV-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B018-CCCIR              PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B018-CREG-AMBTL-RURAL   PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-PPRESV-AMBTL-PPRIE-NULL PIC S9(04)  COMP    VALUE ZEROS.
BRQ001 01  WRK-COUTGA-AGUA-IMOV-NULL   PIC S9(04)  COMP    VALUE ZEROS.
STEFA1 01  WRK-COUTRO-TPO-CONFM-NULL   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B019-CMSGEM-RECOR       PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B025-DLIBRC-OPER-ORCAM  PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B025-DFIM-EVNTO-CDULA   PIC S9(04)  COMP    VALUE ZEROS.
       01  WRK-B09G-CPRODT-RURAL       PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-B002-PREDC-JURO-AMBTL   PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-B002-CINDCD-CONFM-FLO   PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-B002-CINDCD-CONFM-IBA   PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-B002-CINDCD-CONFM-CSV   PIC S9(04)  COMP    VALUE ZEROS.
STE001 01  WRK-B002-RREGLZ-IBAMA       PIC S9(04)  COMP    VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT         PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CTPO-SOLO-AGCLT.
WP1021     05  WRK-CTPO-SOLO-AGCLT-X   PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

WP1021 01  WRK-CCICLO-CTVDA            PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CCICLO-CTVDA.
WP1021     05  WRK-CCICLO-CTVDA-X      PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

WP1021 01  WRK-CMANUT-SOLO             PIC  9(02)+         VALUE ZEROS.
WP1021 01  FILLER   REDEFINES  WRK-CMANUT-SOLO.
WP1021     05  WRK-CMANUT-SOLO-X       PIC  X(02).
WP1021     05  FILLER                  PIC  X(01).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(50)          VALUE
           '*** TABELAS DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB004
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

           EXEC SQL
               INCLUDE RUCAB018
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB019
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB000
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB003
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB006
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB018
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB022
           END-EXEC.

           EXEC SQL
               INCLUDE RURCB028
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB014
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB025
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB040
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09G
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09J
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09N
           END-EXEC.

CG0523     EXEC SQL
CG0523         INCLUDE TRAGB013
CG0523     END-EXEC.

MILTON     EXEC SQL
MILTON         INCLUDE RUCAB014
MILTON     END-EXEC.

7C0825     EXEC SQL
.               INCLUDE RUCAB023
7C0825     END-EXEC.

           EXEC SQL
              DECLARE  CSR01-RUCAB004 CURSOR WITH HOLD FOR
               SELECT  B004_CH_CONTR     ,
                       B004_CH_TIPO      ,
                       B004_CH_NUMERO    ,
                       B004_CODLANC      ,
                       B004_DEBCRED      ,
                       B004_VLAUXIL
                 FROM  DB2PRD.RUCA_CAD_DEP
                WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                  AND  B004_CH_TIPO       = :RUCAB004.B004-CH-TIPO
                  AND  B004_CH_NUMERO    >= :RUCAB004.B004-CH-NUMERO
                ORDER  BY  B004_CH_NUMERO DESC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR02-RUECB014 CURSOR WITH HOLD FOR
               SELECT  CTPO_RESP_PSSOA  ,
                       CCGC_CPF         ,
                       CFLIAL_CGC       ,
                       CCTRL_CPF_CGC
                 FROM  DB2PRD.PESSOA_CONTR_RURAL
                WHERE  CCONTR             = :RUECB014.CCONTR
                  AND  CSEQ_PSSOA        >= :RUECB014.CSEQ-PSSOA
           END-EXEC.

           EXEC SQL
              DECLARE  CSR03-RUECB040 CURSOR WITH HOLD FOR
               SELECT  CCONTR            ,
                       CSEQ_MATR_RURAL
                 FROM  DB2PRD.MATR_CONTR_RURAL
                WHERE  CCONTR             = :RUECB040.CCONTR
                  AND  CSEQ_MATR_RURAL   >= :RUECB040.CSEQ-MATR-RURAL
STEFAB          ORDER  BY CSEQ_MATR_RURAL ASC
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
                       MAREA_NAO_CTVDA
                 FROM  DB2PRD.TMANUT_CORND_GEOGR
                WHERE  CCONTR             = :RUCAB016.CCONTR
                  AND  HINCL_REG          = :RUCAB016.HINCL-REG
                  AND  CSEQ_MATR_RURAL    = :RUCAB016.CSEQ-MATR-RURAL
                  AND  NSEQ_GLEBA_IMOV   >= :RUCAB016.NSEQ-GLEBA-IMOV
                  AND  NCORND_GEOGR_IMOV >= :RUCAB016.NCORND-GEOGR-IMOV
STEFAB            AND (CTPO_CORND_GEOGR   = 2 OR
STEFAB                 CTPO_CORND_GEOGR   IS NULL)
STEFAB          ORDER  BY  NSEQ_GLEBA_IMOV   ASC ,
                           NCORND_GEOGR_IMOV ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR05-RUECB09N CURSOR WITH HOLD FOR
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
STEFAB            AND (CTPO_CORND_GEOGR   = 2 OR
STEFAB                 CTPO_CORND_GEOGR   IS NULL)
STEFAB          ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
                           NCORND_GEOGR_IMOV  ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR06-RUECB09G CURSOR WITH HOLD FOR
               SELECT  NPRODT_RURAL_CSCDO ,
                       CPRODT_RURAL
                 FROM  DB2PRD.TPRODT_RURAL_CSCDO
                WHERE  CCONTR_CREDT_RURAL = :RUECB09G.CCONTR-CREDT-RURAL
                ORDER  BY CPRODT_RURAL ASC
           END-EXEC.

           EXEC SQL
              DECLARE  CSR07-RUCAB018 CURSOR WITH HOLD FOR
               SELECT  CCONTR                     ,
                       HINCL_REG                  ,
                       NPPRIE_IMOV                ,
                       CCNPJ_CPF                  ,
                       CFLIAL_CNPJ                ,
                       CCTRL_CNPJ_CPF             ,
                       CTPO_PSSOA                 ,
                       CNIRF_IMOV_RURAL           ,
                       CCCIR                      ,
                       CREG_AMBTL_RURAL           ,
BRQ001                 PPRESV_AMBTL_PPRIE         ,
BRQ001                 COUTGA_AGUA_IMOV

                 FROM  DB2PRD.TMANUT_PPRIE_IMOV_RECOR
                WHERE  CCONTR        = :RUCAB018.CCONTR
                  AND  HINCL_REG     = :RUCAB018.HINCL-REG
                  AND  NPPRIE_IMOV  >= :RUCAB018.NPPRIE-IMOV
                ORDER  BY NPPRIE_IMOV ASC
           END-EXEC.

           EXEC SQL
               DECLARE  CSR08-RUCAB004 CURSOR WITH HOLD FOR
                SELECT  B004_CH_CONTR     ,
                        B004_CH_TIPO      ,
                        B004_CH_NUMERO    ,
                        B004_CONTROLE     ,
                        B004_DTEVENTO     ,
                        B004_CODLANC      ,
                        B004_DEBCRED      ,
                        B004_VLPRICOR
                  FROM  DB2PRD.RUCA_CAD_DEP
                 WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
                   AND  B004_CH_TIPO      >= :RUCAB004.B004-CH-TIPO
                   AND  B004_CH_NUMERO    >= :RUCAB004.B004-CH-NUMERO
                 ORDER  BY  B004_CH_CONTR ,
                            B004_CH_TIPO  ,
                            B004_CH_NUMERO
           END-EXEC.

           EXEC SQL
              DECLARE  CSR09-RUCAB017 CURSOR WITH HOLD FOR
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
                ORDER  BY NPCELA_CONTR ASC
           END-EXEC.

           EXEC SQL
               DECLARE  CSR10-RUECB025  CURSOR WITH HOLD FOR
                SELECT  DLIBRC_OPER_ORCAM ,
                        RTPO_OPER_ORCAM   ,
                        DFIM_EVNTO_CDULA
                  FROM  DB2PRD.ORCAM_CDULA_RURAL
                 WHERE  CJUNC_DEPDC        = :RUECB025.CJUNC-DEPDC
                 AND    CCDULA             = :RUECB025.CCDULA
                 AND    CPCELA_LIBRC       > :RUECB025.CPCELA-LIBRC
           END-EXEC.

PRI005     EXEC SQL
PRI005         DECLARE  CSR11-RUCAB004 CURSOR WITH HOLD FOR
PRI005          SELECT  B004_CH_CONTR     ,
PRI005                  B004_CH_TIPO      ,
PRI005                  B004_CH_NUMERO    ,
PRI005                  B004_CONTROLE     ,
PRI005                  B004_DTEVENTO     ,
PRI005                  B004_CODLANC      ,
PRI005                  B004_DEBCRED      ,
PRI005                  B004_VLPRIORI     ,
PRI005                  B004_VLPRICOR
PRI005            FROM  DB2PRD.RUCA_CAD_DEP
PRI005           WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
PRI005*            AND  B004_DEBCRED       = :WRK-AUX-DEBCRED
PRI005             AND  B004_DEBCRED       = '6'
PRI005           ORDER  BY  B004_CH_CONTR
PRI005     END-EXEC.

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

STEFAB     EXEC SQL
.             DECLARE  CSR14-RUCAB016 CURSOR WITH HOLD FOR
.              SELECT  CCONTR            ,
.                      HINCL_REG         ,
.                      CSEQ_MATR_RURAL   ,
.                      NSEQ_GLEBA_IMOV   ,
.                      NCORND_GEOGR_IMOV ,
.                      MLATTD_IMOV_RURAL ,
.                      MLOGTD_IMOV_RURAL ,
.                      MALTTD_IMOV_RURAL ,
.                      MAREA_NAO_CTVDA
.                FROM  DB2PRD.TMANUT_CORND_GEOGR
.               WHERE  CCONTR             = :WRK-TAB16-CCONTR
.                 AND  HINCL_REG          = :WRK-TAB16-HINCL-REG
.                 AND  CSEQ_MATR_RURAL    = :WRK-TAB16-CSEQ-MATR-RURAL
.                 AND  NSEQ_GLEBA_IMOV    = :WRK-TAB16-NSEQ-GLEBA-IMOV
.                 AND  NCORND_GEOGR_IMOV >= :WRK-TAB16-NCORND-GEOGR-IMOV
.                 AND  CTPO_CORND_GEOGR   = 1
.               ORDER  BY  NSEQ_GLEBA_IMOV    ASC ,
.                          NCORND_GEOGR_IMOV  ASC
.          END-EXEC.
.
.          EXEC SQL
.             DECLARE  CSR15-RUECB09N CURSOR WITH HOLD FOR
.              SELECT  CCONTR            ,
.                      CSEQ_MATR_RURAL   ,
.                      NSEQ_GLEBA_IMOV   ,
.                      NCORND_GEOGR_IMOV ,
.                      MLATTD_IMOV_RURAL ,
.                      MLOGTD_IMOV_RURAL ,
.                      MALTTD_IMOV_RURAL
.                FROM  DB2PRD.TCORND_GEOGR_CONTR
.               WHERE  CCONTR             = :WRK-TAB9N-CCONTR
.                 AND  CSEQ_MATR_RURAL    = :WRK-TAB9N-CSEQ-MATR-RURAL
.                 AND  NSEQ_GLEBA_IMOV    = :WRK-TAB9N-NSEQ-GLEBA-IMOV
.                 AND  NCORND_GEOGR_IMOV >= :WRK-TAB9N-NCORND-GEOGR-IMOV
.                 AND  CTPO_CORND_GEOGR   = 1
STEFAB            ORDER BY NSEQ_GLEBA_IMOV    ASC ,
.                          NCORND_GEOGR_IMOV  ASC
STEFAB     END-EXEC.

7C5.10     EXEC SQL
.             DECLARE  CSR16-RUCAB023 CURSOR WITH HOLD FOR
.              SELECT  CCONTR
.                     ,COUTRO_TPO_CONFM
.                FROM  DB2PRD.TCONTR_RECOR_TPO_CONFM
.               WHERE  CCONTR     = :RUCAB023.CCONTR
.               AND    HINCL_REG  = :RUCAB023.HINCL-REG
.               ORDER  BY COUTRO_TPO_CONFM ASC
7C5.10     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *----------------------------------------------------------------*

7C2510 01  WRK-MASK-COORD-X.
7C2510     05  WRK-MASK-COORD          PIC -999,9999999    VALUE ZEROS.

7C2510 01  FILLER.
7C2510     05 WRK-QTD-CULTVD           PIC 9(04) COMP-3    VALUE ZEROS.
7C2510     05 WRK-PONTO-CULTVD         OCCURS 999.
7C2510        10 WRK-CULTVD-LON        PIC X(12)           VALUE SPACES.
7C2510        10 FILLER                PIC X(01)           VALUE SPACES.
7C2510        10 WRK-CULTVD-LAT        PIC X(12)           VALUE SPACES.
7C2510     05 WRK-QTD-NCULTVD          PIC 9(04) COMP-3    VALUE ZEROS.
7C2510     05 WRK-PONTO-NCULTVD        OCCURS 999.
7C2510        10 WRK-NCULTVD-LON       PIC X(12)           VALUE SPACES.
7C2510        10 FILLER                PIC X(01)           VALUE SPACES.
7C2510        10 WRK-NCULTVD-LAT       PIC X(12)           VALUE SPACES.

7C2510 01  WRK-NUM-POLIGN              PIC 9(01) COMP-3    VALUE ZEROS.
7C2510 01  WRK-TAM-POLIGN              PIC 9(05) COMP-3    VALUE ZEROS.
7C2510 01  WRK-POLIGN                  PIC X(10000)        VALUE SPACES.
7C2510 01  IND-1                       PIC 9(04) COMP-3    VALUE ZEROS.
      ******************************************************************
       LINKAGE                         SECTION.
      ******************************************************************

       01  COMU-AREA-RUCA8810.
           05  COMU-8810-ENTRADA.
               07  COMU-8810-USUARIO   PIC  9(07).
               07  COMU-8810-CHAVE     PIC  9(09).
               07  COMU-8810-CTRLNAV.
                   10  COMU-8810-ANO-CTRLNAV
                                       PIC  9(04).
                   10  COMU-8810-MES-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8810-DIA-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8810-HOR-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8810-MIN-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8810-SEG-CTRLNAV
                                       PIC  9(02).
                   10  COMU-8810-MIL-CTRLNAV
                                       PIC  9(06).
           05  COMU-8810-SAIDA.
               07  COMU-8810-COD-RETORNO
                                       PIC  9(02).
               07  COMU-8810-MENSAGEM  PIC  X(75).
               07  COMU-8810-ERRO-AREA PIC  X(107).
               07  COMU-8810-SQLCA     PIC  X(136).

      ******************************************************************
       PROCEDURE                      DIVISION USING COMU-AREA-RUCA8810.
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

           MOVE  ZEROS                 TO  COMU-8810-COD-RETORNO
                                           ACU-NENVIO-RECOR
                                           ACU-NREG-MSGEM-RECOR
           MOVE  WRK-MSG-00            TO  COMU-8810-MENSAGEM
           MOVE  SPACES                TO  COMU-8810-ERRO-AREA
                                           COMU-8810-SQLCA

           IF  COMU-8810-USUARIO       IS  NUMERIC  AND
               COMU-8810-USUARIO  GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  01                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-01        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8810-CHAVE         IS  NUMERIC  AND
               COMU-8810-CHAVE    GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  02                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-02        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  COMU-8810-CTRLNAV       IS  NUMERIC  AND
               COMU-8810-CTRLNAV  GREATER  ZEROS
               CONTINUE
           ELSE
               MOVE  03                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-03        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-PROCESSAR-DADOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5005-OBTER-DADOS

           PERFORM  5400-SALVAR-MENSAGEM

           PERFORM  5420-TRATAR-CONTINUACAO
             UNTIL  WRK-TEM-CONTINUACAO  EQUAL  'N'.

      *----------------------------------------------------------------*
       5000-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5005-OBTER-DADOS                SECTION.
      *----------------------------------------------------------------*

           PERFORM  5010-SELECT-RUFIB002
           PERFORM  5015-OBTER-TAXA
           PERFORM  5035-SELECT-RUCAB015
           PERFORM  5050-FORMATAR-MSG.

      *----------------------------------------------------------------*
       5005-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-SELECT-RUFIB002            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8810-CHAVE       TO  CCONTR           OF RUFIB002

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
                        DINCL                         ,
                        CJUNC_DEPDC                   ,
CG0523                  CCTA_CORR                     ,
                        CMUN_RURAL                    ,
                        CFNALD_RURAL                  ,
                        CATVDD_RURAL                  ,
                        CORIGE_REC                    ,
                        CPRODT_RURAL                  ,
                        CUND_MEDD_FINCD               ,
                        CESPCE_TITLO_GARNT            ,
                        CTPO_GARNT                    ,
                        CTPO_PROTR_RURAL              ,
                        CREFT_BACEN                   ,
                        DANO_BASE_NEGOC               ,
                        CDEPDC_NEGOC                  ,
                        CSEQ_STUDO_NEGOC              ,
                        DIMPRE_CONTR_RURAL            ,
                        CCTRO_CUSTO                   ,
                        CCESTA_SAFRA_RURAL            ,
                        CMODLD_RURAL                  ,
                        CVAR_PRODT_RURAL              ,
                        CINDCD_ZNMTO                  ,
                        CINDCD_CSCIO                  ,
                        CTPO_AGROP_RURAL              ,
                        CTPO_INTGC_RURAL              ,
                        CINDCD_UND_PRDTV              ,
                        CTPO_IGCAO_RURAL              ,
                        CTPO_CTIVO_RURAL              ,
                        CCICLO_PROD_RURAL             ,
                        CREFT_RECOR                   ,
                        VRECTA_BRUTA_EMPTO            ,
                        CTPO_BNEFC_RURAL              ,
                        CDECLR_AGCLT_FAMLR            ,
PRI499                  CCLIMT_RURAL_BACEN            ,
MJ0224                  DVCTO_ORIGN_CONTR             ,
STE001                  PREDC_JURO_AMBTL              ,
.                       CINDCD_CONFM_FLORS            ,
.                       CINDCD_CONFM_IBAMA            ,
.                       CINDCD_CONFM_CSVAO            ,
STE001                  RREGLZ_IBAMA                  ,
STEFA1                  VALUE(CINDCD_CONFM_RURAL, '9'),
STEFA1                  COUTRO_TPO_CONFM              ,
STEFA1                  VALUE(CCNPJ_INSTC_EXIGB, 0)
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
                       :RUFIB002.DINCL                ,
                       :RUFIB002.CJUNC-DEPDC          ,
CG0523                 :RUFIB002.CCTA-CORR            ,
                       :RUFIB002.CMUN-RURAL
                         :WRK-B002-CMUN-RURAL         ,
                       :RUFIB002.CFNALD-RURAL
                         :WRK-B002-CFNALD-RURAL       ,
                       :RUFIB002.CATVDD-RURAL
                         :WRK-B002-CATVDD-RURAL       ,
                       :RUFIB002.CORIGE-REC
                         :WRK-B002-CORIGE-REC         ,
                       :RUFIB002.CPRODT-RURAL
                         :WRK-B002-CPRODT-RURAL       ,
                       :RUFIB002.CUND-MEDD-FINCD
                         :WRK-B002-CUND-MEDD-FINCD    ,
                       :RUFIB002.CESPCE-TITLO-GARNT
                         :WRK-B002-CESPCE-TITLO-GARNT ,
                       :RUFIB002.CTPO-GARNT
                         :WRK-B002-CTPO-GARNT         ,
                       :RUFIB002.CTPO-PROTR-RURAL
                         :WRK-B002-CTPO-PROTR-RURAL   ,
                       :RUFIB002.CREFT-BACEN          ,
                       :RUFIB002.DANO-BASE-NEGOC      ,
                       :RUFIB002.CDEPDC-NEGOC         ,
                       :RUFIB002.CSEQ-STUDO-NEGOC     ,
                       :RUFIB002.DIMPRE-CONTR-RURAL   ,
                       :RUFIB002.CCTRO-CUSTO          ,
                       :RUFIB002.CCESTA-SAFRA-RURAL   ,
                       :RUFIB002.CMODLD-RURAL         ,
                       :RUFIB002.CVAR-PRODT-RURAL     ,
                       :RUFIB002.CINDCD-ZNMTO         ,
                       :RUFIB002.CINDCD-CSCIO         ,
                       :RUFIB002.CTPO-AGROP-RURAL     ,
                       :RUFIB002.CTPO-INTGC-RURAL     ,
                       :RUFIB002.CINDCD-UND-PRDTV     ,
                       :RUFIB002.CTPO-IGCAO-RURAL     ,
                       :RUFIB002.CTPO-CTIVO-RURAL     ,
                       :RUFIB002.CCICLO-PROD-RURAL    ,
                       :RUFIB002.CREFT-RECOR          ,
                       :RUFIB002.VRECTA-BRUTA-EMPTO
                         :WRK-B002-VRECTA-BRUTA-EMPTO ,
                       :RUFIB002.CTPO-BNEFC-RURAL
                         :WRK-B002-CTPO-BNEFC-RURAL   ,
                       :RUFIB002.CDECLR-AGCLT-FAMLR
                         :WRK-B002-CDECLR-AGCLT-FAMLR ,
PRI499                 :RUFIB002.CCLIMT-RURAL-BACEN   ,
MJ0224                 :RUFIB002.DVCTO-ORIGN-CONTR    ,
STE001                 :RUFIB002.PREDC-JURO-AMBTL
.                        :WRK-B002-PREDC-JURO-AMBTL   ,
.                      :RUFIB002.CINDCD-CONFM-FLORS
.                        :WRK-B002-CINDCD-CONFM-FLO   ,
.                      :RUFIB002.CINDCD-CONFM-IBAMA
.                        :WRK-B002-CINDCD-CONFM-IBA   ,
.                      :RUFIB002.CINDCD-CONFM-CSVAO
.                        :WRK-B002-CINDCD-CONFM-CSV   ,
STE001                 :RUFIB002.RREGLZ-IBAMA
STE001                   :WRK-B002-RREGLZ-IBAMA,
STEFA1                 :RUFIB002.CINDCD-CONFM-RURAL,
STEFA1                 :RUFIB002.COUTRO-TPO-CONFM
STEFA1                   :WRK-COUTRO-TPO-CONFM-NULL,
STEFA1                 :RUFIB002.CCNPJ-INSTC-EXIGB
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
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  05                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-05        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B002-CMUN-RURAL     EQUAL  -1
               MOVE  ZEROS             TO  CMUN-RURAL       OF RUFIB002
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

           IF  WRK-B002-CUND-MEDD-FINCD  EQUAL  -1
               MOVE  ZEROS             TO  CUND-MEDD-FINCD  OF RUFIB002
           END-IF.

           IF  WRK-B002-CESPCE-TITLO-GARNT EQUAL  -1
               MOVE  ZEROS             TO  CESPCE-TITLO-GARNT
                                                            OF RUFIB002
           END-IF.

           IF  WRK-B002-CTPO-GARNT     EQUAL  -1
               MOVE  ZEROS             TO  CTPO-GARNT       OF RUFIB002
           END-IF.

           IF  WRK-B002-CTPO-PROTR-RURAL   EQUAL  -1
               MOVE  ZEROS             TO  CTPO-PROTR-RURAL OF RUFIB002
           END-IF.

           IF  WRK-B002-VRECTA-BRUTA-EMPTO
                                       EQUAL  -1
               MOVE  ZEROS             TO  VRECTA-BRUTA-EMPTO
                                                            OF RUFIB002
           END-IF.

           IF  WRK-B002-CDECLR-AGCLT-FAMLR
                                       EQUAL  -1
               MOVE  SPACES            TO  CDECLR-AGCLT-FAMLR
                                                            OF RUFIB002
           END-IF.

           IF  WRK-B002-CTPO-BNEFC-RURAL
                                       EQUAL  -1
               MOVE  ZEROS             TO  CTPO-BNEFC-RURAL OF RUFIB002
           END-IF.

STE001     IF  WRK-B002-PREDC-JURO-AMBTL
.                                      EQUAL  -1
.              MOVE  ZEROS             TO  PREDC-JURO-AMBTL OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-CINDCD-CONFM-FLO
.                                      EQUAL  -1
.              MOVE  ZEROS          TO  CINDCD-CONFM-FLORS  OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-CINDCD-CONFM-IBA
.                                      EQUAL  -1
.              MOVE  ZEROS          TO  CINDCD-CONFM-IBAMA  OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-CINDCD-CONFM-CSV
.                                      EQUAL  -1
.              MOVE  ZEROS          TO  CINDCD-CONFM-CSVAO  OF RUFIB002
.          END-IF.
.
STE001     IF  WRK-B002-RREGLZ-IBAMA
.                                      EQUAL  -1
.              MOVE  ZEROS          TO  RREGLZ-IBAMA-LEN    OF RUFIB002
.              MOVE  SPACES         TO  RREGLZ-IBAMA-TEXT   OF RUFIB002
STE001     END-IF.

      ******************************************************************
      *  DEVIDO OS CONTRATOS DO CRUR ESTAREM SENDO TOMBADOS SEM        *
      *  TRAZER O CAMPO CTPO-BNEFC-RURAL, ESTAMOS FORMATANDO FIXO 1    *
      *  PARA QUE SEJA POSSIVEL ENVIAR A COR0001 SEM REJEICAO POR PARTE*
      *  DO BACEN. REGRA DEFINIDA JUNTO AOS GESTORES EM REUNIAO        *
      *  REALIZADA EM 10/12/2020.                                      *
      ******************************************************************
           IF  CTPO-BNEFC-RURAL OF RUFIB002 EQUAL ZEROS
               MOVE  1                 TO  CTPO-BNEFC-RURAL OF RUFIB002
           END-IF.

CG0523     MOVE DINCL OF RUFIB002      TO WRK-DATA-DB2
CG0523     MOVE WRK-DIA-DB2            TO WRK-DIA-CONTRATO
CG0523     MOVE WRK-MES-DB2            TO WRK-MES-CONTRATO
CG0523     MOVE WRK-ANO-DB2            TO WRK-ANO-CONTRATO.

      *----------------------------------------------------------------*
       5010-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5015-OBTER-TAXA                 SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-ACHOU-TAXA

           PERFORM  5020-OPEN-CSR01-RUCAB004
           PERFORM  5025-FETCH-CSR01-RUCAB004

           PERFORM
             UNTIL  WRK-ACHOU-TAXA  EQUAL  'S'
               OR   WRK-FIM-CSR01   EQUAL  'S'

               IF  B004-CODLANC OF RUCAB004 GREATER 199  AND
                   B004-CODLANC OF RUCAB004 LESS    300  AND
                   B004-DEBCRED OF RUCAB004 EQUAL   '8'
                   MOVE  'S'           TO  WRK-ACHOU-TAXA
               ELSE
                   PERFORM  5025-FETCH-CSR01-RUCAB004
               END-IF
           END-PERFORM.

           PERFORM  5030-CLOSE-CSR01-RUCAB004.

      *----------------------------------------------------------------*
       5015-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-OPEN-CSR01-RUCAB004        SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8810-CHAVE       TO  B004-CH-CONTR    OF RUCAB004
           MOVE  100                   TO  B004-CH-TIPO     OF RUCAB004
           MOVE  ZEROS                 TO  B004-CH-NUMERO   OF RUCAB004
           MOVE  'N'                   TO  WRK-FIM-CSR01

           EXEC SQL
                OPEN CSR01-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0002'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5020-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5025-FETCH-CSR01-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR01-RUCAB004
                INTO   :RUCAB004.B004-CH-CONTR     ,
                       :RUCAB004.B004-CH-TIPO      ,
                       :RUCAB004.B004-CH-NUMERO    ,
                       :RUCAB004.B004-CODLANC      ,
                       :RUCAB004.B004-DEBCRED      ,
                       :RUCAB004.B004-VLAUXIL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0003'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  ZEROS             TO  B004-VLAUXIL     OF RUCAB004
               MOVE  'S'               TO  WRK-FIM-CSR01
           END-IF.

      *----------------------------------------------------------------*
       5025-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5030-CLOSE-CSR01-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0004'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5030-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5035-SELECT-RUCAB015            SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8810-CHAVE       TO  CCONTR           OF RUCAB015
           MOVE  COMU-8810-ANO-CTRLNAV TO  WRK-ANO-TIMES
           MOVE  COMU-8810-MES-CTRLNAV TO  WRK-MES-TIMES
           MOVE  COMU-8810-DIA-CTRLNAV TO  WRK-DIA-TIMES
           MOVE  COMU-8810-HOR-CTRLNAV TO  WRK-HOR-TIMES
           MOVE  COMU-8810-MIN-CTRLNAV TO  WRK-MIN-TIMES
           MOVE  COMU-8810-SEG-CTRLNAV TO  WRK-SEG-TIMES
           MOVE  COMU-8810-MIL-CTRLNAV TO  WRK-MIL-TIMES
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
BRQ001                  CCETFC_ORGNC_EMPTO            ,
BRQ001                  EURL_CETFC_ORGNC              ,
BRQ001                  CRSTRB_EMPTO_RURAL            ,
BRQ001                  EURL_RSTRB_EMPTO              ,
BRQ001                  PENERG_RENOV_EMPTO            ,
WP1021                  CTPO_SOLO_AGCLT               ,
WP1021                  CCICLO_CTVDA                  ,
WP1021                  CMANUT_SOLO                   ,
WP1021                  CTPO_SEGUR_CONTT              ,
PRI499                  CCLIMT_RURAL_BACEN            ,
STE001                  COUTRO_TRATO_SOLO             ,
.                 VALUE(CINDCD_CONFM_FLORS, 0)        ,
.                 VALUE(CINDCD_CONFM_IBAMA, 0)        ,
.                 VALUE(CINDCD_CONFM_CSVAO, 0)        ,
STE001            VALUE(RREGLZ_IBAMA, '')             ,
STEFA1            VALUE(CINDCD_CONFM_RURAL, '9')      ,
STEFA1                  COUTRO_TPO_CONFM              ,
STEFA1            VALUE(CCNPJ_INSTC_EXIGB, 0)         ,
STE002            VALUE(QUND_ANMAL, 0)
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
BRQ001                 :RUCAB015.CCETFC-ORGNC-EMPTO
BRQ001                   :WRK-CCETFC-ORGNC-EMPTO-NULL     ,
BRQ001                 :RUCAB015.EURL-CETFC-ORGNC
BRQ001                   :WRK-EURL-CETFC-ORGNC-NULL       ,
BRQ001                 :RUCAB015.CRSTRB-EMPTO-RURAL
BRQ001                   :WRK-CRSTRB-EMPTO-RURAL-NULL     ,
BRQ001                 :RUCAB015.EURL-RSTRB-EMPTO
BRQ001                   :WRK-EURL-RSTRB-EMPTO-NULL       ,
BRQ001                 :RUCAB015.PENERG-RENOV-EMPTO
BRQ001                   :WRK-PENERG-RENOV-EMPTO-NULL     ,
WP1021                 :RUCAB015.CTPO-SOLO-AGCLT
WP1021                   :WRK-CTPO-SOLO-AGCLT-NULL        ,
WP1021                 :RUCAB015.CCICLO-CTVDA
WP1021                   :WRK-CCICLO-CTVDA-NULL           ,
WP1021                 :RUCAB015.CMANUT-SOLO
WP1021                   :WRK-CMANUT-SOLO-NULL            ,
WP1021                 :RUCAB015.CTPO-SEGUR-CONTT
WP1021                   :WRK-CTPO-SEGUR-CONTT-NULL       ,
PRI499                 :RUCAB015.CCLIMT-RURAL-BACEN
PRI499                   :WRK-B015-CCLIMT-RURAL-BACEN     ,
STE001                 :RUCAB015.COUTRO-TRATO-SOLO
STE001                   :WRK-B015-COUTRO-TRATO-SOLO      ,
.                      :RUCAB015.CINDCD-CONFM-FLORS       ,
.                      :RUCAB015.CINDCD-CONFM-IBAMA       ,
.                      :RUCAB015.CINDCD-CONFM-CSVAO       ,
STE001                 :RUCAB015.RREGLZ-IBAMA             ,
STEFA1                 :RUCAB015.CINDCD-CONFM-RURAL       ,
STEFA1                 :RUCAB015.COUTRO-TPO-CONFM
STEFA1                    :WRK-COUTRO-TPO-CONFM-NULL      ,
STEFA1                 :RUCAB015.CCNPJ-INSTC-EXIGB        ,
STE002                 :RUCAB015.QUND-ANMAL
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
               MOVE '0005'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  06                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-06        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           ELSE
               PERFORM  5040-TRATAR-NULOS-B015
           END-IF.

      *----------------------------------------------------------------*
       5035-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5040-TRATAR-NULOS-B015          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-B015-CESPCE-TITLO-GARNT   EQUAL  -1
               MOVE  CESPCE-TITLO-GARNT OF RUFIB002
                                       TO CESPCE-TITLO-GARNT OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-PROTR-RURAL     EQUAL  -1
               MOVE  CTPO-PROTR-RURAL  OF RUFIB002
                                       TO CTPO-PROTR-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CORIGE-REC           EQUAL  -1
               MOVE  CORIGE-REC        OF RUFIB002
                                       TO CORIGE-REC         OF RUCAB015
           END-IF.

           IF  WRK-B015-VRECTA-BRUTA-EMPTO   EQUAL  -1
               MOVE  VRECTA-BRUTA-EMPTO OF RUFIB002
                                       TO VRECTA-BRUTA-EMPTO OF RUCAB015
           END-IF.

           IF  WRK-B015-CMUN-RURAL           EQUAL  -1
               MOVE  CMUN-RURAL        OF RUFIB002
                                       TO CMUN-RURAL         OF RUCAB015
           END-IF.

           IF  WRK-B015-MAREA-TOT            EQUAL  -1
               MOVE  MAREA-TOT         OF RUFIB002
                                       TO MAREA-TOT          OF RUCAB015
           END-IF.

           IF  WRK-B015-QUND-FINCD-RURAL     EQUAL  -1
               MOVE  QUND-FINCD-RURAL  OF RUFIB002
                                       TO QUND-FINCD-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CPRODT-RURAL         EQUAL  -1
               MOVE  CPRODT-RURAL      OF RUFIB002
                                       TO CPRODT-RURAL       OF RUCAB015
           END-IF.

           IF  WRK-B015-QUND-PROD-PROVV      EQUAL  -1
               MOVE  QUND-PROD-PROVV   OF RUFIB002
                                       TO QUND-PROD-PROVV    OF RUCAB015
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
               MOVE  CTPO-AGROP-RURAL  OF RUFIB002
                                       TO CTPO-AGROP-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-INTGC-RURAL     EQUAL  -1
               MOVE  CTPO-INTGC-RURAL  OF RUFIB002
                                       TO CTPO-INTGC-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CINDCD-UND-PRDTV     EQUAL  -1
               MOVE  CINDCD-UND-PRDTV  OF RUFIB002
                                       TO CINDCD-UND-PRDTV   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-IGCAO-RURAL     EQUAL  -1
               MOVE  CTPO-IGCAO-RURAL  OF RUFIB002
                                       TO CTPO-IGCAO-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CTPO-CTIVO-RURAL     EQUAL  -1
               MOVE  CTPO-CTIVO-RURAL  OF RUFIB002
                                       TO CTPO-CTIVO-RURAL   OF RUCAB015
           END-IF.

           IF  WRK-B015-CCICLO-PROD-RURAL    EQUAL  -1
               MOVE  CCICLO-PROD-RURAL OF RUFIB002
                                       TO CCICLO-PROD-RURAL  OF RUCAB015
           END-IF.

PRI499     IF  WRK-B015-CCLIMT-RURAL-BACEN   EQUAL  -1
PRI499         MOVE CCLIMT-RURAL-BACEN OF RUFIB002
PRI499                                 TO CCLIMT-RURAL-BACEN OF RUCAB015
PRI499     END-IF.

PRI499     IF  WRK-B015-COUTRO-TRATO-SOLO    EQUAL  -1
STE001         MOVE ZEROS
STE001                                 TO COUTRO-TRATO-SOLO  OF RUCAB015
STE001     END-IF.

           IF  WRK-B015-VPCELA-LIBRC         EQUAL  -1
               MOVE  ZEROS             TO VPCELA-LIBRC       OF RUCAB015
           END-IF.

           IF  WRK-B015-VGARNT-RENDA-MIN     EQUAL  -1
               MOVE  ZEROS             TO VGARNT-RENDA-MIN   OF RUCAB015
           END-IF.

           IF  WRK-B015-PJURO-ENCAR-FINCR    EQUAL  -1
               MOVE  B004-VLAUXIL      OF  RUCAB004
                                       TO  WRK-S13V99-DRESS
               MOVE  WRK-N03V99-DRESS  TO  PJURO-ENCAR-FINCR OF RUCAB015
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
               MOVE  DANO-BASE-NEGOC   OF RUFIB002
                                       TO DANO-BASE          OF RUCAB015
           END-IF.

           IF  WRK-B015-CJUNC-DEPDC          EQUAL  -1
               MOVE  CDEPDC-NEGOC      OF RUFIB002
                                       TO CJUNC-DEPDC        OF RUCAB015
           END-IF.

           IF  WRK-B015-CSEQ-STUDO-NEGOC     EQUAL  -1
               MOVE  CSEQ-STUDO-NEGOC  OF RUFIB002
                                       TO CSEQ-STUDO-NEGOC   OF RUCAB015
           END-IF.

BRQ001     IF  WRK-CCETFC-ORGNC-EMPTO-NULL   EQUAL  -1
BRQ001         MOVE  SPACES            TO CCETFC-ORGNC-EMPTO OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-EURL-CETFC-ORGNC-NULL     EQUAL  -1
BRQ001         MOVE  256               TO  EURL-CETFC-ORGNC-LEN
BRQ001                                                      OF RUCAB015
BRQ001         MOVE  SPACES            TO  EURL-CETFC-ORGNC-TEXT
BRQ001                                                      OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-CRSTRB-EMPTO-RURAL-NULL   EQUAL  -1
BRQ001         MOVE  SPACES            TO CRSTRB-EMPTO-RURAL OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-EURL-RSTRB-EMPTO-NULL     EQUAL  -1
BRQ001         MOVE  256               TO  EURL-RSTRB-EMPTO-LEN
BRQ001                                                      OF RUCAB015
BRQ001         MOVE  SPACES            TO  EURL-RSTRB-EMPTO-TEXT
BRQ001                                                      OF RUCAB015
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-PENERG-RENOV-EMPTO-NULL   EQUAL  -1
BRQ001         MOVE  ZEROS             TO PENERG-RENOV-EMPTO OF RUCAB015
BRQ001     END-IF.
BRQ001
WP1021     IF  WRK-CTPO-SOLO-AGCLT-NULL      EQUAL  -1
WP1021         MOVE  ZEROS             TO CTPO-SOLO-AGCLT    OF RUCAB015
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CCICLO-CTVDA-NULL         EQUAL  -1
WP1021         MOVE  ZEROS             TO CCICLO-CTVDA       OF RUCAB015
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CMANUT-SOLO-NULL          EQUAL  -1
WP1021         MOVE  ZEROS             TO CMANUT-SOLO        OF RUCAB015
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CTPO-SEGUR-CONTT-NULL     EQUAL  -1
WP1021         MOVE  ZEROS             TO CTPO-SEGUR-CONTT   OF RUCAB015
WP1021     END-IF.
WP1021
      *----------------------------------------------------------------*
       5040-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5050-FORMATAR-MSG               SECTION.
      *----------------------------------------------------------------*

           MOVE  'COR0001'             TO  RUCA66-CODMSG

           PERFORM  5055-OBTER-DATA-HORA

           MOVE  WRK-ANO-ATUAL         TO  WRK-NUMCTRLIF-ANO
           MOVE  WRK-MES-ATUAL         TO  WRK-NUMCTRLIF-MES
           MOVE  WRK-DIA-ATUAL         TO  WRK-NUMCTRLIF-DIA
           MOVE  WRK-NUMCTRLIF         TO  RUCA66-NUMCTRLIF

CG0523     PERFORM  5056-OBTER-AGENCIA-ORIGINAL

           PERFORM  5060-CHAMAR-MESU9032

WIP924     IF  COMU-8810-CHAVE EQUAL 9197016
WIP924         MOVE 2083               TO  WRK-9032-FILIAL
WIP924         MOVE 76                 TO  WRK-9032-CONTROLE-N
WIP924     END-IF.
WIP924
WI1024     IF  COMU-8810-CHAVE EQUAL 9190457
WI1024         MOVE 2794               TO  WRK-9032-FILIAL
WI1024         MOVE 78                 TO  WRK-9032-CONTROLE-N
WI1024     END-IF.
WI1024
WI1124     IF  COMU-8810-CHAVE EQUAL 9196903
WI1124         MOVE 6941               TO  WRK-9032-FILIAL
WI1124         MOVE 06                 TO  WRK-9032-CONTROLE-N
WI1124     END-IF.
WI1124
WI1124     IF  COMU-8810-CHAVE EQUAL 9198392
WI1124         MOVE 1721               TO  WRK-9032-FILIAL
WI1124         MOVE 69                 TO  WRK-9032-CONTROLE-N
WI1124     END-IF.
WI1124
           MOVE   60746948             TO  WRK-NUM-CNPJ-AUX
           MOVE  WRK-9032-FILIAL       TO  WRK-FILIAL-CNPJ-AUX
           MOVE  WRK-9032-CONTROLE-N   TO  WRK-CTRL-CNPJ-AUX
           MOVE  WRK-CNPJ-AUX          TO  RUCA66-CNPJ-ENT-RESPONS

           MOVE  ALL '*'               TO  RUCA66-CNPJ-PART

           IF  CREFT-RECOR OF RUFIB002 IS       NUMERIC  AND
               CREFT-RECOR OF RUFIB002 GREATER  ZEROS
               MOVE  CREFT-RECOR OF RUFIB002
                                       TO  RUCA66-NUMREF-BCCOROR
           ELSE
               MOVE  CREFT-BACEN OF RUFIB002
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X11-DRESS     TO  RUCA66-NUMREF-BCCOROR
           END-IF.

           IF  DIMPRE-CONTR-RURAL OF RUFIB002  EQUAL  '01.01.0001'
               MOVE  DCDULA            OF  RUFIB002
                                       TO  WRK-DATA-DB2
               MOVE  WRK-DIA-DB2       TO  WRK-DIA-INV
               MOVE  WRK-MES-DB2       TO  WRK-MES-INV
               MOVE  WRK-ANO-DB2       TO  WRK-ANO-INV
               MOVE  WRK-DATA-INV      TO  RUCA66-DTEMS
           ELSE
               MOVE  DIMPRE-CONTR-RURAL
                                       OF  RUFIB002
                                       TO  WRK-DATA-DB2
               MOVE  WRK-DIA-DB2       TO  WRK-DIA-INV
               MOVE  WRK-MES-DB2       TO  WRK-MES-INV
               MOVE  WRK-ANO-DB2       TO  WRK-ANO-INV
               MOVE  WRK-DATA-INV      TO  RUCA66-DTEMS
           END-IF.

WI0824     IF  COMU-8810-CHAVE EQUAL 9192362
WI0824         MOVE 20230927           TO  RUCA66-DTEMS
WI0824     END-IF.
MJ0924
MJ0924     PERFORM 5052-DE-PARA-ENV-BACEN
MJ0924
      *    *F  DVCTO-CONTR OF  RUCAB015  EQUAL  '01.01.0001'
MJ0224     IF (DVCTO-CONTR OF RUCAB015 NOT EQUAL '01.01.0001')       AND
MJ0224        (DVCTO-CONTR OF RUCAB015 NOT EQUAL SPACES)             AND
MJ0224        (DVCTO-CONTR OF RUCAB015 NOT EQUAL LOW-VALUES)
               MOVE  DVCTO-CONTR       OF  RUCAB015
                                       TO  WRK-DATA-DB2
               MOVE  WRK-DIA-DB2       TO  WRK-DIA-INV
               MOVE  WRK-MES-DB2       TO  WRK-MES-INV
               MOVE  WRK-ANO-DB2       TO  WRK-ANO-INV
               MOVE  WRK-DATA-INV      TO  RUCA66-DTVENC
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
MJ0224             MOVE WRK-DATA-INV   TO  RUCA66-DTVENC
MJ0224         ELSE
MJ0224             MOVE DVCTO-ORIGN-CONTR  OF RUFIB002
MJ0224                                 TO  WRK-DATA-DB2
MJ0224             MOVE WRK-DIA-DB2    TO  WRK-DIA-INV
MJ0224             MOVE WRK-MES-DB2    TO  WRK-MES-INV
MJ0224             MOVE WRK-ANO-DB2    TO  WRK-ANO-INV
MJ0224             MOVE WRK-DATA-INV   TO  RUCA66-DTVENC
MJ0224         END-IF
MJ0224     ELSE
MJ0224             MOVE DVCTO-CONTR    OF  RUFIB002
MJ0224                                 TO  WRK-DATA-DB2
MJ0224             MOVE WRK-DIA-DB2    TO  WRK-DIA-INV
MJ0224             MOVE WRK-MES-DB2    TO  WRK-MES-INV
MJ0224             MOVE WRK-ANO-DB2    TO  WRK-ANO-INV
MJ0224             MOVE WRK-DATA-INV   TO  RUCA66-DTVENC
MJ0224      END-IF
           END-IF.

           IF  NBASE-LEGAL-RENEG OF RUCAB015  GREATER  ZEROS
               MOVE  NBASE-LEGAL-RENEG OF RUCAB015
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X03-DRESS     TO  RUCA66-CBASE-LEGAL-PRO
           ELSE
               MOVE  ALL '*'           TO  RUCA66-CBASE-LEGAL-PRO
           END-IF.

           MOVE  CJUNC-DEPDC           OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  WRK-CJUNC-DEPDC
           MOVE  CCDULA                OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N09-DRESS         TO  WRK-CCDULA
           MOVE  WRK-CEDULA-RECOR      TO  RUCA66-NUMCEDL-CRED-RURALIF

           PERFORM  5065-SELECT-RURCB018

           MOVE  CTPO-GARNT-RECOR      OF  RURCB018
                                       TO  WRK-S11-DRESS
           MOVE  WRK-X02-DRESS         TO  RUCA66-TP-INSTNTO-CRED

MJ0924* SOLICITADO PELO DEF EM 02/09/24 O ENVIO DO VALOR ABAIXO PARA O
MJ0924* CONTRATO 9185111. O CONTRATO FOI LIQUIDADO ANTECIPADAMENTE
MJ0924* ANTES DA LIBERACAO DA ULTIMA PARCELA. VALOR MOVIMENTADO APENAS
MJ0924* PARA ENVIO AO BACEN
MJ0924     IF  COMU-8810-CHAVE      EQUAL  9185111
MJ0924         MOVE 30000,00           TO  VFINAN OF  RUCAB015
MJ0924     END-IF.

           MOVE  VFINAN                OF  RUCAB015
                                       TO  WRK-S13V99-DRESS
           MOVE  ZEROS                 TO  RUCA66-VLR-TOTOP(01:04)
           MOVE  WRK-X15-DRESS         TO  RUCA66-VLR-TOTOP(05:15)

           PERFORM  5070-SELECT-RURCB022

           MOVE  CCATEG-PROTR-RECOR    OF  RURCB022
                                       TO  RUCA66-TP-CATG-EMIT

           MOVE  RUCA66-COR0001-1      TO  WRK-MSGEM-RURAL

           ADD   LENGTH OF RUCA66-COR0001-1
                                       TO  ACU-TAM

           COMPUTE  ACU-POSICAO         =  ACU-TAM  +  1

      *----------------------------------------------------------------*

           PERFORM  5075-FORMATAR-GRUPO-BENFCRIO

      *----------------------------------------------------------------*

           PERFORM  5105-FORMATAR-GRUPO-DESTC

      *----------------------------------------------------------------*
STE001
MILTON     PERFORM 5500-PROCURAR-CONTR-MANUT
.          IF WRK-REDUC-X             IS NUMERIC AND
              WRK-REDUC-NUM           GREATER ZEROS
.             MOVE  WRK-REDUC-X       TO  RUCA66-PERC-BONUS-CAR
.          ELSE
.             MOVE  ALL '*'           TO  RUCA66-PERC-BONUS-CAR
.          END-IF.
.
.
STEFA1     IF RUEC58-CNPJ-BASE-EXIGLDD
.                                     GREATER  ZEROS
.             MOVE RUEC58-CNPJ-BASE-EXIGLDD
.                                     TO  WRK-9-8-AUX
.             MOVE WRK-X-8-AUX        TO  RUCA66-CNPJ-BASE-EXIGLDD
.          ELSE
.             MOVE  ALL '*'           TO  RUCA66-CNPJ-BASE-EXIGLDD
STEFA1     END-IF.
7C5.10     MOVE  ALL '*'              TO  RUCA66-PERC-BONUS-SUST-X
.
.          MOVE  RUCA66-COR0001-21A    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21A)
.          ADD   LENGTH OF RUCA66-COR0001-21A
.                                      TO  ACU-TAM
.                                          ACU-POSICAO.
.
.          PERFORM 6300-OBTER-TIPO-CONFORMIDADE
.
.          MOVE  ALL '*'        TO  RUCA66-GP-COR0001-LIBRC.
.          MOVE  RUCA66-COR0001-21C    TO
.                                      WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21C)
.          ADD   LENGTH OF RUCA66-COR0001-21C
.                                          TO ACU-TAM
.                                             ACU-POSICAO.
.
.          MOVE  ALL '*'        TO  RUCA66-GP-COR0001-COOP.

.          MOVE  RUCA66-COR0001-21D    TO
.                                      WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21D)
.          ADD   LENGTH OF RUCA66-COR0001-21D
.                                          TO ACU-TAM
7C5.10                                        ACU-POSICAO.
.
STE001*----------------------------------------------------------------*
           MOVE  WRK-DATA-ATUAL        TO  RUCA66-DTMOVTO

WP1021     MOVE  RUCA66-COR0001-22     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
WP1021                                     LENGTH OF RUCA66-COR0001-22)
WP1021     ADD   LENGTH OF RUCA66-COR0001-22
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5050-99-FIM.  EXIT.
      *----------------------------------------------------------------*

STE001*----------------------------------------------------------------*
.      5051-TRATAR-DE-PARA             SECTION.
.     *----------------------------------------------------------------*
.          EVALUATE WRK-INDICADOR-TELA
.              WHEN '1'
.                 MOVE 'S'             TO WRK-INDICADOR-TELA-X
.              WHEN '2'
.                 MOVE 'N'             TO WRK-INDICADOR-TELA-X
.              WHEN OTHER
.                 MOVE '*'             TO WRK-INDICADOR-TELA-X
.          END-EVALUATE.
.     *----------------------------------------------------------------*
.      5051-99-FIM.  EXIT.
STE001*----------------------------------------------------------------*
MJ0924
..    *----------------------------------------------------------------*
..     5052-DE-PARA-ENV-BACEN          SECTION.
..    *----------------------------------------------------------------*
.. ***** TRATAMENTO PARA CONTRATOS QUE TOMBARAM COM A DATA DE
.. ***** IMPRESSAO DIFERENTE DO ENVIADO AO BACEN
.. *****
WI0824     EVALUATE COMU-8810-CHAVE
               WHEN 9210005  MOVE 20240828 TO RUCA66-DTEMS
               WHEN 9208487  MOVE 20240821 TO RUCA66-DTEMS
               WHEN 9208458  MOVE 20240821 TO RUCA66-DTEMS
               WHEN 9208202  MOVE 20240819 TO RUCA66-DTEMS
               WHEN 9207717  MOVE 20240814 TO RUCA66-DTEMS
               WHEN 9207799  MOVE 20240813 TO RUCA66-DTEMS
               WHEN 9207479  MOVE 20240805 TO RUCA66-DTEMS
               WHEN 9208693  MOVE 20240805 TO RUCA66-DTEMS
               WHEN 9206123  MOVE 20240729 TO RUCA66-DTEMS
               WHEN 9205872  MOVE 20240729 TO RUCA66-DTEMS
               WHEN 9206573  MOVE 20240726 TO RUCA66-DTEMS
               WHEN 9206437  MOVE 20240726 TO RUCA66-DTEMS
               WHEN 9204489  MOVE 20240625 TO RUCA66-DTEMS
               WHEN 9204625  MOVE 20240625 TO RUCA66-DTEMS
               WHEN 9204134  MOVE 20240617 TO RUCA66-DTEMS
               WHEN 9204132  MOVE 20240614 TO RUCA66-DTEMS
               WHEN 9204175  MOVE 20240617 TO RUCA66-DTEMS
               WHEN 9203262  MOVE 20240510 TO RUCA66-DTEMS
               WHEN 9201373  MOVE 20240305 TO RUCA66-DTEMS
               WHEN 9201060  MOVE 20240228 TO RUCA66-DTEMS
               WHEN 9201132  MOVE 20240226 TO RUCA66-DTEMS
               WHEN 9199717  MOVE 20240115 TO RUCA66-DTEMS
               WHEN 9199379  MOVE 20231226 TO RUCA66-DTEMS
               WHEN 9199186  MOVE 20231220 TO RUCA66-DTEMS
               WHEN 9198559  MOVE 20231204 TO RUCA66-DTEMS
               WHEN 9198046  MOVE 20231204 TO RUCA66-DTEMS
               WHEN 9197871  MOVE 20231130 TO RUCA66-DTEMS
               WHEN 9197227  MOVE 20231129 TO RUCA66-DTEMS
               WHEN 9197562  MOVE 20231129 TO RUCA66-DTEMS
               WHEN 9197786  MOVE 20231129 TO RUCA66-DTEMS
               WHEN 9197619  MOVE 20231128 TO RUCA66-DTEMS
               WHEN 9195970  MOVE 20231108 TO RUCA66-DTEMS
               WHEN 9195863  MOVE 20231109 TO RUCA66-DTEMS
               WHEN 9195209  MOVE 20231101 TO RUCA66-DTEMS
               WHEN 9196243  MOVE 20231031 TO RUCA66-DTEMS
               WHEN 9194873  MOVE 20231027 TO RUCA66-DTEMS
               WHEN 9195918  MOVE 20231026 TO RUCA66-DTEMS
               WHEN 9195080  MOVE 20231025 TO RUCA66-DTEMS
               WHEN 9194814  MOVE 20231025 TO RUCA66-DTEMS
               WHEN 9194101  MOVE 20231011 TO RUCA66-DTEMS
               WHEN 9194175  MOVE 20231009 TO RUCA66-DTEMS
               WHEN 9192362  MOVE 20230927 TO RUCA66-DTEMS
               WHEN 9193702  MOVE 20230926 TO RUCA66-DTEMS
               WHEN 9192109  MOVE 20230922 TO RUCA66-DTEMS
               WHEN 9193031  MOVE 20230921 TO RUCA66-DTEMS
               WHEN 9192237  MOVE 20230920 TO RUCA66-DTEMS
               WHEN 9191827  MOVE 20230919 TO RUCA66-DTEMS
               WHEN 9192442  MOVE 20230918 TO RUCA66-DTEMS
               WHEN 9191910  MOVE 20230913 TO RUCA66-DTEMS
               WHEN 9191252  MOVE 20230913 TO RUCA66-DTEMS
               WHEN 9192187  MOVE 20230915 TO RUCA66-DTEMS
               WHEN 9193125  MOVE 20230906 TO RUCA66-DTEMS
               WHEN 9191773  MOVE 20230901 TO RUCA66-DTEMS
               WHEN 9191297  MOVE 20230829 TO RUCA66-DTEMS
               WHEN 9189011  MOVE 20230823 TO RUCA66-DTEMS
               WHEN 9190315  MOVE 20230829 TO RUCA66-DTEMS
               WHEN 9189724  MOVE 20230823 TO RUCA66-DTEMS
               WHEN 9190777  MOVE 20230818 TO RUCA66-DTEMS
               WHEN 9189479  MOVE 20230817 TO RUCA66-DTEMS
               WHEN 9190422  MOVE 20230825 TO RUCA66-DTEMS
               WHEN 9188217  MOVE 20230818 TO RUCA66-DTEMS
               WHEN 9188034  MOVE 20230810 TO RUCA66-DTEMS
               WHEN 9187085  MOVE 20230807 TO RUCA66-DTEMS
               WHEN 9189700  MOVE 20230808 TO RUCA66-DTEMS
               WHEN 9187576  MOVE 20230804 TO RUCA66-DTEMS
               WHEN 9185325  MOVE 20230713 TO RUCA66-DTEMS
               WHEN 9184053  MOVE 20230510 TO RUCA66-DTEMS
               WHEN 9182473  MOVE 20230313 TO RUCA66-DTEMS
               WHEN 9182701  MOVE 20230313 TO RUCA66-DTEMS
               WHEN 9181708  MOVE 20230214 TO RUCA66-DTEMS
               WHEN 9181127  MOVE 20230131 TO RUCA66-DTEMS
               WHEN 9181085  MOVE 20230130 TO RUCA66-DTEMS
               WHEN 9181287  MOVE 20230126 TO RUCA66-DTEMS
               WHEN 9180251  MOVE 20221228 TO RUCA66-DTEMS
               WHEN 9180476  MOVE 20230103 TO RUCA66-DTEMS
               WHEN 9179335  MOVE 20221129 TO RUCA66-DTEMS
               WHEN 9178043  MOVE 20221027 TO RUCA66-DTEMS
               WHEN 9178194  MOVE 20221026 TO RUCA66-DTEMS
               WHEN 9178256  MOVE 20221018 TO RUCA66-DTEMS
               WHEN 9176308  MOVE 20221014 TO RUCA66-DTEMS
               WHEN 9176701  MOVE 20221014 TO RUCA66-DTEMS
               WHEN 9175991  MOVE 20221010 TO RUCA66-DTEMS
               WHEN 9176260  MOVE 20221011 TO RUCA66-DTEMS
               WHEN 9175570  MOVE 20221005 TO RUCA66-DTEMS
               WHEN 9174946  MOVE 20220930 TO RUCA66-DTEMS
               WHEN 9175037  MOVE 20220928 TO RUCA66-DTEMS
               WHEN 9177084  MOVE 20220927 TO RUCA66-DTEMS
               WHEN 9177082  MOVE 20220926 TO RUCA66-DTEMS
               WHEN 9174499  MOVE 20220921 TO RUCA66-DTEMS
               WHEN 9174430  MOVE 20220912 TO RUCA66-DTEMS
               WHEN 9172873  MOVE 20220902 TO RUCA66-DTEMS
               WHEN 9172578  MOVE 20220906 TO RUCA66-DTEMS
               WHEN 9171754  MOVE 20220826 TO RUCA66-DTEMS
               WHEN 9171882  MOVE 20220824 TO RUCA66-DTEMS
               WHEN 9172013  MOVE 20220822 TO RUCA66-DTEMS
               WHEN 9170674  MOVE 20220822 TO RUCA66-DTEMS
               WHEN 9170407  MOVE 20220822 TO RUCA66-DTEMS
               WHEN 9170555  MOVE 20220823 TO RUCA66-DTEMS
               WHEN 9170623  MOVE 20220816 TO RUCA66-DTEMS
               WHEN 9172812  MOVE 20220908 TO RUCA66-DTEMS
               WHEN 9169573  MOVE 20220811 TO RUCA66-DTEMS
               WHEN 9170466  MOVE 20220811 TO RUCA66-DTEMS
               WHEN 9169669  MOVE 20220809 TO RUCA66-DTEMS
               WHEN 9169043  MOVE 20220803 TO RUCA66-DTEMS
               WHEN 9170276  MOVE 20220802 TO RUCA66-DTEMS
               WHEN 9169305  MOVE 20220729 TO RUCA66-DTEMS
               WHEN 9169069  MOVE 20220804 TO RUCA66-DTEMS
               WHEN 9168509  MOVE 20220726 TO RUCA66-DTEMS
               WHEN 9168983  MOVE 20220729 TO RUCA66-DTEMS
               WHEN 9169189  MOVE 20220729 TO RUCA66-DTEMS
               WHEN 9168534  MOVE 20220720 TO RUCA66-DTEMS
               WHEN 9168352  MOVE 20220719 TO RUCA66-DTEMS
               WHEN 9168347  MOVE 20220715 TO RUCA66-DTEMS
               WHEN 9167948  MOVE 20220624 TO RUCA66-DTEMS
               WHEN 9167255  MOVE 20220613 TO RUCA66-DTEMS
               WHEN 9167190  MOVE 20220613 TO RUCA66-DTEMS
               WHEN 9167068  MOVE 20220610 TO RUCA66-DTEMS
               WHEN 9166991  MOVE 20220602 TO RUCA66-DTEMS
               WHEN 9166559  MOVE 20220525 TO RUCA66-DTEMS
               WHEN 9166325  MOVE 20220523 TO RUCA66-DTEMS
               WHEN 9164855  MOVE 20220324 TO RUCA66-DTEMS
               WHEN 9164651  MOVE 20220314 TO RUCA66-DTEMS
               WHEN 9164650  MOVE 20220311 TO RUCA66-DTEMS
               WHEN 9164274  MOVE 20220221 TO RUCA66-DTEMS
               WHEN 9164391  MOVE 20220216 TO RUCA66-DTEMS
               WHEN 9164390  MOVE 20220216 TO RUCA66-DTEMS
               WHEN OTHER CONTINUE
WI0824     END-EVALUATE.
..    *----------------------------------------------------------------*
..     5052-99-FIM.  EXIT.
MJ0924*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5055-OBTER-DATA-HORA            SECTION.
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
       5055-99-FIM.  EXIT.
      *----------------------------------------------------------------*

CG0523*----------------------------------------------------------------*
CG0523 5056-OBTER-AGENCIA-ORIGINAL SECTION.
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
CG0523         MOVE '0006'             TO  ERR-LOCAL
CG0523         MOVE  SPACES            TO  ERR-SEGM
CG0523         MOVE  99                TO  COMU-8810-COD-RETORNO
CG0523         MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
CG0523         MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
CG0523         MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
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
CG0523 5056-99-FIM.  EXIT.
CG0523*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5060-CHAMAR-MESU9032            SECTION.
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
                 MOVE  15              TO  COMU-8810-COD-RETORNO
                 MOVE  WRK-MSG-15      TO  COMU-8810-MENSAGEM
                 PERFORM  8000-FINALIZAR

           WHEN  OTHER
                 MOVE  99              TO  COMU-8810-COD-RETORNO
                 MOVE  WRK-9032-BRAD7100
                                       TO  COMU-8810-ERRO-AREA
                 MOVE  WRK-9032-SQLCA  TO  COMU-8810-SQLCA
                 MOVE  WRK-MSG-99      TO  COMU-8810-MENSAGEM
                 PERFORM  8000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       5060-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5065-SELECT-RURCB018            SECTION.
      *----------------------------------------------------------------*

           MOVE  CESPCE-TITLO-GARNT    OF RUCAB015
                                       TO CESPCE-TITLO-GARNT OF RURCB018

           EXEC SQL
               SELECT
                       CTPO_GARNT_RECOR
                 INTO
                      :RURCB018.CTPO-GARNT-RECOR
                 FROM  DB2PRD.ESPCE_TITLO_GARNT
                WHERE  CESPCE_TITLO_GARNT = :RURCB018.CESPCE-TITLO-GARNT
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ESPCE_TITLO_GARNT'
                                       TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0007'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  07                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-07        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5065-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5070-SELECT-RURCB022            SECTION.
      *----------------------------------------------------------------*

           MOVE  CTPO-PROTR-RURAL      OF  RUCAB015
                                       TO  CTPO-PROTR-RURAL OF RURCB022

           EXEC SQL
               SELECT
                       CCATEG_PROTR_RECOR
                 INTO
                      :RURCB022.CCATEG-PROTR-RECOR
                 FROM  DB2PRD.TPO_PROTR_RURAL
                WHERE  CTPO_PROTR_RURAL   = :RURCB022.CTPO-PROTR-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TPO_PROTR_RURAL'  TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0008'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE                EQUAL  +100
               MOVE '5555'             TO CCATEG-PROTR-RECOR OF RURCB022
           END-IF.

      *----------------------------------------------------------------*
       5070-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5075-FORMATAR-GRUPO-BENFCRIO    SECTION.
      *----------------------------------------------------------------*

           PERFORM  5085-OBTER-BNEFC-RURAL
           PERFORM  5090-OPEN-CSR02-RUECB014
           PERFORM  5095-FETCH-CSR02-RUECB014

           PERFORM
             UNTIL  WRK-FIM-CSR02   EQUAL  'S'

                IF  CTPO-RESP-PSSOA OF RUECB014  EQUAL  'T'
                    PERFORM  5080-MOVER-BENEFICIARIO
                END-IF

                PERFORM  5095-FETCH-CSR02-RUECB014

           END-PERFORM.

           PERFORM  5100-CLOSE-CSR02-RUECB014.

      *----------------------------------------------------------------*
       5075-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5080-MOVER-BENEFICIARIO         SECTION.
      *----------------------------------------------------------------*

           IF  CFLIAL-CGC OF RUECB014  EQUAL  ZEROS
               MOVE  'F'               TO  RUCA66-TP-PESSOA-EMIT
               MOVE  CCGC-CPF          OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N09-DRESS     TO  WRK-NUM-CPF-AUX
               MOVE  CCTRL-CPF-CGC     OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N02-DRESS     TO  WRK-CTRL-CPF-AUX
7C5.10         MOVE  WRK-CPF-AUX-X     TO  RUCA66-CNPJ-CPF-EMIT
           ELSE
               MOVE  'J'               TO  RUCA66-TP-PESSOA-EMIT
               MOVE  CCGC-CPF          OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N08-DRESS     TO  WRK-NUM-CNPJ-AUX
               MOVE  CFLIAL-CGC        OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N04-DRESS     TO  WRK-FILIAL-CNPJ-AUX
               MOVE  CCTRL-CPF-CGC     OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N02-DRESS     TO  WRK-CTRL-CNPJ-AUX
               MOVE  WRK-CNPJ-AUX      TO  RUCA66-CNPJ-CPF-EMIT
           END-IF.

           IF  CDECLR-AGCLT-FAMLR      OF  RUFIB002
                                       EQUAL  SPACES
               MOVE ALL '*'            TO  RUCA66-CAF-BENF
           ELSE
               MOVE CDECLR-AGCLT-FAMLR OF  RUFIB002
                                       TO  RUCA66-CAF-BENF
           END-IF.

           MOVE  CMRUW09I-S-CTPO-BNEFC-BACEN
                                       TO  RUCA66-TPO-BENF

           MOVE  RUCA66-COR0001-2      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-2)

           ADD  LENGTH OF RUCA66-COR0001-2
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5080-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5085-OBTER-BNEFC-RURAL          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      CMRUW09I-REGISTRO
                                           CMRUW09I-S-ERRO-AREA
                                           CMRUW09I-S-SQLCA

           MOVE CTPO-BNEFC-RURAL       OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N02-DRESS         TO  CMRUW09I-E-CTPO-BNEFC-RURAL
           MOVE 'CMRU307C'             TO  WRK-MODULO

           CALL WRK-MODULO          USING  WRK-AREA-CMRU307C

           EVALUATE  CMRUW09I-COD-RETORNO
               WHEN  ZEROS
                     MOVE  CMRUW09I-S-CTPO-BNEFC-BACEN
                                       TO  RUCA66-TPO-BENF
               WHEN  04
               WHEN  08
                     MOVE  ALL '*'     TO  RUCA66-TPO-BENF
                                           CMRUW09I-S-CTPO-BNEFC-BACEN

               WHEN  OTHER
                     MOVE  WRK-MSG-99  TO  COMU-8810-MENSAGEM
                     MOVE  99          TO  COMU-8810-COD-RETORNO
                     MOVE  CMRUW09I-S-ERRO-AREA
                                       TO  COMU-8810-ERRO-AREA
                     MOVE  CMRUW09I-S-SQLCA
                                       TO  COMU-8810-SQLCA
                     PERFORM  8000-FINALIZAR
           END-EVALUATE.

      *----------------------------------------------------------------*
       5085-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5090-OPEN-CSR02-RUECB014        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUCAB015    TO  CCONTR          OF RUECB014
           MOVE  ZEROS                 TO  CSEQ-PSSOA      OF RUECB014
           MOVE  'N'                   TO  WRK-FIM-CSR02

           EXEC SQL
                OPEN CSR02-RUECB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'PESSOA_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0009'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5090-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5095-FETCH-CSR02-RUECB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR02-RUECB014
                 INTO  :RUECB014.CTPO-RESP-PSSOA  ,
                       :RUECB014.CCGC-CPF         ,
                       :RUECB014.CFLIAL-CGC       ,
                       :RUECB014.CCTRL-CPF-CGC
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'PESSOA_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0010'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR02
           END-IF.

      *----------------------------------------------------------------*
       5095-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-CLOSE-CSR02-RUECB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR02-RUECB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'PESSOA_CONTR_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0011'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5100-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5105-FORMATAR-GRUPO-DESTC       SECTION.
      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-3      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-3)
           ADD   LENGTH OF RUCA66-COR0001-3
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           PERFORM  5110-FORMATAR-GRUPO-RENEGC

      *----------------------------------------------------------------*

           PERFORM  5115-SELECT-RURCB000

           IF  CPROG-RURAL OF RURCB000  EQUAL  SPACES
               MOVE  ALL '*'           TO  RUCA66-CODPROG-LINHA-CRED
           ELSE
               MOVE  CPROG-RURAL       OF  RURCB000
                                       TO  RUCA66-CODPROG-LINHA-CRED
           END-IF.

7C5.10     PERFORM  5355-FORMATAR-COD-SUBPROGRAMA


           PERFORM  5120-SELECT-RURCB028

           MOVE  CPLANO-REFIN-RECOR    OF  RURCB028
                                       TO  RUCA66-TP-FNTE-REC

           MOVE  CMUN-RURAL            OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N06-DRESS         TO  RUCA66-COD-MUNIC

           MOVE  RUCA66-COR0001-5      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-5)
           ADD   LENGTH OF RUCA66-COR0001-5
                                       TO  ACU-TAM
                                           ACU-POSICAO

           COMPUTE  ACU-NEW-POS         = (ACU-POSICAO - 6)

      *----------------------------------------------------------------*

           PERFORM  5130-FORMATAR-GRUPO-GLEBA.

      *--> QUANDO AS GLEBAS SAO INFORMADAS, MOVIMENTA '*' PARA O
      *    MUNICIPIO RURAL. MUNICIPIO RURAL SO PODE SER ENVIADO
      *    QUANDO NAO FOREM INFORMADAS AS COORDENADAS GEOGRAFICOS.

7C2510*   XIF  RUCA66-IDENTC-GLEBA(1:1)  EQUAL  '*'
7C2510*       XCONTINUE
7C2510*   XELSE
7C2510*      XMOVE '******'            TO  WRK-MSGEM-RURAL
7C2510*                                                (ACU-NEW-POS: 6)
7C2510*    END-IF.

      *----------------------------------------------------------------*

           PERFORM  5220-FORMATAR-PROD-CONSCD

      *----------------------------------------------------------------*

           PERFORM  5245-CHAMAR-RUEC8800

           MOVE  RUEC58-EMPREEND       TO  RUCA66-COD-EMPNMNT
           MOVE  RUEC58-SIST-PROD      TO  RUCA66-COD-SIST-PRODC
           MOVE  VFINAN                OF  RUCAB015
                                       TO  WRK-S13V99-DRESS
           MOVE  ZEROS                 TO  RUCA66-VLR-PARCL-CRED(01:04)
           MOVE  WRK-X15-DRESS         TO  RUCA66-VLR-PARCL-CRED(05:15)

           IF  VREC-PPRIO OF RUFIB002  EQUAL  ZEROS
WI0924      IF COMU-8810-CHAVE EQUAL 9195977
WI0924         MOVE '0000000000023127341'
WI0924                                 TO  RUCA66-VLR-PARCL-REC-PROP
WI0924      ELSE
               MOVE  ALL '*'           TO  RUCA66-VLR-PARCL-REC-PROP
WI0924      END-IF
           ELSE
               MOVE  VREC-PPRIO        OF  RUFIB002
                                       TO  WRK-S13V99-DRESS
               MOVE  ZEROS             TO  RUCA66-VLR-PARCL-REC-PROP
                                                                 (01:04)
               MOVE  WRK-X15-DRESS     TO  RUCA66-VLR-PARCL-REC-PROP
                                                                 (05:15)
           END-IF.

           IF  VGARNT-RENDA-MIN OF RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-VLR-PARCL-SERVICOS
           ELSE
               MOVE  VGARNT-RENDA-MIN  OF RUCAB015
                                       TO  WRK-S13V99-DRESS
               MOVE  ZEROS             TO  RUCA66-VLR-PARCL-SERVICOS
                                                                 (01:04)
               MOVE  WRK-X15-DRESS     TO  RUCA66-VLR-PARCL-SERVICOS
                                                                 (05:15)
           END-IF.

           MOVE  ALL '*'               TO  RUCA66-ALQ-PROAGRO

           MOVE  PJURO-ENCAR-FINCR     OF  RUCAB015
                                       TO  RUCA66-PERC-JUROS-ENCARFIN

           IF  CTPO-ENCAR-COMPL OF RUCAB015  EQUAL  ZEROS
               MOVE  9                 TO  RUCA66-TP-ENCARG-FIN-COMPL
           ELSE
               MOVE  CTPO-ENCAR-COMPL  OF  RUCAB015
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N02-DRESS     TO  RUCA66-TP-ENCARG-FIN-COMPL
           END-IF.

           IF  PRISCO-TSORO-NACIO  OF  RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-PERC-RSC-STN
           ELSE
               MOVE  PRISCO-TSORO-NACIO   OF  RUCAB015
                                       TO  WRK-S03V99-DRESS
               MOVE  WRK-S05-DRESS     TO  RUCA66-PERC-RSC-STN-S5
           END-IF.

           IF  PRISCO-FUNDO-CNSTT  OF  RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-PERC-RSC-FUND-CONSCL
           ELSE
               MOVE  PRISCO-FUNDO-CNSTT   OF  RUCAB015
                                       TO  WRK-S03V99-DRESS
               MOVE  WRK-S05-DRESS     TO RUCA66-PERC-RSC-FUND-CONSCL-S5
           END-IF.

           MOVE  ALL '*'               TO  RUCA66-PERC-CST-EFT-TOTAL

           IF  MAREA-TOT OF  RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-AREA-X
           ELSE
               MOVE  MAREA-TOT         OF  RUCAB015
                                       TO  WRK-S13V99-DRESS
               MOVE  WRK-N09V99-DRESS
                                       TO  RUCA66-AREA-N
           END-IF.
ONEDA ***  **---------------------------------------
ONEDA ***  -> NAO ENVIA AREA QDO EXIGE COORDENADAS GEODESICAS
ONEDA      IF RUCA66-IDENTC-GLEBA(1:1) NOT EQUAL '*'
ONEDA         MOVE  ALL '*'        TO RUCA66-AREA-X
ONEDA      END-IF.
ONEDA ***  **---------------------------------------

STE002     IF  (QUND-FINCD-RURAL OF  RUCAB015  EQUAL  ZEROS AND
STE002          QUND-ANMAL       OF  RUCAB015  EQUAL  ZEROS)
               MOVE  ALL '*'           TO  RUCA66-QTD-ITEM-FINCD-X
           ELSE
STEFA4         PERFORM 5111-FORMATAR-CAMPO-QTD
           END-IF

           IF  QUND-PROD-PROVV OF  RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-QTD-PRV-PRODC-X
           ELSE
               MOVE  QUND-PROD-PROVV   OF  RUCAB015
                                       TO  WRK-S13V99-DRESS
               MOVE  WRK-N12V99-DRESS  TO  RUCA66-QTD-PRV-PRODC
           END-IF.

           MOVE  RUCA66-COR0001-9      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-9)
           ADD   LENGTH OF RUCA66-COR0001-9
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           PERFORM  5260-FORMATAR-PROPRIETARIO

      *----------------------------------------------------------------*

           PERFORM  5295-VERIFICAR-SEGURO

           IF  CSIT-REG-RURAL OF RUECB09J  EQUAL 'A'
               MOVE  3                 TO  RUCA66-TP-GAR-EMPNMNT
           ELSE
               MOVE  9                 TO  RUCA66-TP-GAR-EMPNMNT
           END-IF.

WI0921     IF  COMU-8810-CHAVE         EQUAL 9153679
WI0921         MOVE  3                 TO  RUCA66-TP-GAR-EMPNMNT
WI0921     END-IF.

WP1021     IF  WRK-CTPO-SEGUR-CONTT-NULL
WP1021                                 EQUAL  -1
WP1021         CONTINUE
WP1021     ELSE
WP1021         MOVE  CTPO-SEGUR-CONTT   OF RUCAB015
WP1021                                 TO RUCA66-TP-GAR-EMPNMNT
WP1021     END-IF.

           MOVE  ALL '*'               TO  RUCA66-CNPJBASE-INST-MUTNTE
           MOVE  ALL '*'               TO  RUCA66-NUMREF-BCCOR-INVESTMT
           MOVE  ALL '*'               TO  RUCA66-VLR-PARCL-INVESTMT

           IF  VRECTA-BRUTA-EMPTO OF RUCAB015  EQUAL  ZEROS
               MOVE  ALL '*'           TO  RUCA66-VLR-RECBRUTA-ESPRDEMP
           ELSE
               MOVE  VRECTA-BRUTA-EMPTO OF RUCAB015
                                       TO  WRK-S13V99-DRESS
               MOVE  ZEROS             TO
                                 RUCA66-VLR-RECBRUTA-ESPRDEMP(01:04)
               MOVE  WRK-X15-DRESS     TO
                                 RUCA66-VLR-RECBRUTA-ESPRDEMP(05:15)
           END-IF.

           MOVE  RUCA66-COR0001-11     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-11)
           ADD   LENGTH OF RUCA66-COR0001-11
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           PERFORM  5305-FORMATAR-PARC-PGTO

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*

           PERFORM  5360-FORMATAR-PLANTIO-COLHEITA

           IF  WRK-B015-DINIC-PLATI  EQUAL  -1  AND
               WRK-B015-DFIM-PLATI   EQUAL  -1
               CONTINUE
           ELSE
               IF  DINIC-PLATI OF RUCAB015 EQUAL SPACES OR '01.01.0001'
                   MOVE  ALL '*'       TO  RUCA66-DT-INI-PLANT
                                           RUCA66-DT-FIM-PLANT
               ELSE
                   MOVE  DINIC-PLATI   OF  RUCAB015
                                       TO  WRK-DATA-DB2
                   MOVE  WRK-DIA-DB2   TO  WRK-DIA-INV
                   MOVE  WRK-MES-DB2   TO  WRK-MES-INV
                   MOVE  WRK-ANO-DB2   TO  WRK-ANO-INV
                   MOVE  WRK-DATA-INV-X
                                       TO  RUCA66-DT-INI-PLANT

                   IF  DFIM-PLATI OF RUCAB015 EQUAL  SPACES
                                                 OR  '01.01.0001'
                       MOVE  ALL '*'   TO  RUCA66-DT-FIM-PLANT
                   ELSE
                       MOVE  DFIM-PLATI OF  RUCAB015
                                       TO  WRK-DATA-DB2
                       MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-INV
                       MOVE  WRK-MES-DB2
                                       TO  WRK-MES-INV
                       MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-INV
                       MOVE  WRK-DATA-INV-X
                                       TO  RUCA66-DT-FIM-PLANT
                   END-IF
               END-IF
           END-IF.

           IF  WRK-B015-DINIC-COLHE  EQUAL  -1  AND
               WRK-B015-DFIM-COLHE   EQUAL  -1
               CONTINUE
           ELSE
               IF  DINIC-COLHE OF RUCAB015 EQUAL SPACES OR '01.01.0001'
                   MOVE  ALL '*'       TO  RUCA66-DT-INI-COLHT
                                           RUCA66-DT-FIM-COLHT
               ELSE
                   MOVE  DINIC-COLHE   OF  RUCAB015
                                       TO  WRK-DATA-DB2
                   MOVE  WRK-DIA-DB2   TO  WRK-DIA-INV
                   MOVE  WRK-MES-DB2   TO  WRK-MES-INV
                   MOVE  WRK-ANO-DB2   TO  WRK-ANO-INV
                   MOVE  WRK-DATA-INV-X
                                       TO  RUCA66-DT-INI-COLHT

                   IF  DFIM-COLHE OF RUCAB015 EQUAL SPACES
                                                 OR '01.01.0001'
                       MOVE  ALL '*'   TO  RUCA66-DT-FIM-COLHT
                   ELSE
                       MOVE  DFIM-COLHE OF  RUCAB015
                                       TO  WRK-DATA-DB2
                       MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-INV
                       MOVE  WRK-MES-DB2
                                       TO  WRK-MES-INV
                       MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-INV
                       MOVE  WRK-DATA-INV-X
                                       TO  RUCA66-DT-FIM-COLHT
                   END-IF
               END-IF
           END-IF.

MJ0522******F CTEMPO-VIDA-CULTR     OF RURCB006 IS NUMERIC AND
MJ0522********CTEMPO-VIDA-CULTR     OF RURCB006 EQUAL 1 AND
MJ0522********CFNALD-RECOR OF RURCB003          EQUAL '00002'
MJ0522*********OVE ALL '*'             TO RUCA66-DT-INI-PLANT
MJ0522*********OVE ALL '*'             TO RUCA66-DT-FIM-PLANT
MJ1223*********OVE ALL '*'             TO RUCA66-DT-INI-COLHT
MJ1223*********OVE ALL '*'             TO RUCA66-DT-FIM-COLHT
MJ0522******ND-IF.

           MOVE  RUCA66-COR0001-15     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-15)
           ADD   LENGTH OF RUCA66-COR0001-15
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-NUM-REF-BCCO-DESCL
                                           RUCA66-VLR-DESCLASCD
BRQ001                                     RUCA66-MOTV-DESCLASSCOR

           MOVE  RUCA66-COR0001-16     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-16)
           ADD   LENGTH OF RUCA66-COR0001-16
                                       TO  ACU-TAM
                                           ACU-POSICAO.

BRQ001*----------------------------------------------------------------*
BRQ001     MOVE  ALL '*'               TO  RUCA66-COD-STN-EQULZC
BRQ001
BRQ001     MOVE  RUCA66-COR0001-17     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-17)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-17
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.

BRQ001*----------------------------------------------------------------*
BRQ001     PERFORM 5106-MONTA-CERTIFICACAO.
BRQ001
WP1021     PERFORM 5107-MONTA-ZARC.
WP1021
      *----------------------------------------------------------------*
       5105-99-FIM.  EXIT.
      *----------------------------------------------------------------*

BRQ001*----------------------------------------------------------------*
BRQ001 5106-MONTA-CERTIFICACAO         SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     IF  WRK-CCETFC-ORGNC-EMPTO-NULL LESS ZEROS
BRQ001         MOVE  ALL '*'       TO  RUCA66-CODCERTIFCORGNCOEMPNMNT
BRQ001                                 RUCA66-URLCERTIFCORGNCOEMPNMNT
BRQ001     ELSE
BRQ001         MOVE  CCETFC-ORGNC-EMPTO OF RUCAB015
BRQ001                             TO RUCA66-CODCERTIFCORGNCOEMPNMNT
BRQ001         MOVE  EURL-CETFC-ORGNC-TEXT OF RUCAB015(1:255)
BRQ001                             TO RUCA66-URLCERTIFCORGNCOEMPNMNT
BRQ001     END-IF.
BRQ001
BRQ001     MOVE  RUCA66-COR0001-18     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-18)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-18
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001     IF  WRK-CRSTRB-EMPTO-RURAL-NULL LESS ZEROS
BRQ001         MOVE  ALL '*'       TO  RUCA66-CODRASTLDDEMPNMNT
BRQ001                                 RUCA66-URLCODRASTLDDEMPNMNT
BRQ001     ELSE
BRQ001         MOVE  CRSTRB-EMPTO-RURAL OF RUCAB015
BRQ001                             TO RUCA66-CODRASTLDDEMPNMNT
BRQ001         MOVE  EURL-RSTRB-EMPTO-TEXT OF RUCAB015(1:255)
BRQ001                             TO RUCA66-URLCODRASTLDDEMPNMNT
BRQ001     END-IF.
BRQ001
BRQ001     MOVE  RUCA66-COR0001-19     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-19)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-19
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001     IF  WRK-PENERG-RENOV-EMPTO-NULL LESS ZEROS
BRQ001         MOVE  ALL '*'       TO  RUCA66-COR0001-20
BRQ001     ELSE
BRQ001         MOVE  PENERG-RENOV-EMPTO OF RUCAB015
BRQ001                             TO WRK-PIC9-3V99-CS
BRQ001         MOVE  WRK-PIC9-3V99-SS  TO RUCA66-PERCENERRENOVVLEMPNMNT
BRQ001     END-IF.
BRQ001
BRQ001     MOVE  RUCA66-COR0001-20     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-20)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-20
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 5106-99-FIM.  EXIT.
BRQ001*----------------------------------------------------------------*

WP1021*----------------------------------------------------------------*
WP1021 5107-MONTA-ZARC                 SECTION.
WP1021*----------------------------------------------------------------*
WR1021
WP1021     IF (CTPO-SOLO-AGCLT OF RUCAB015   EQUAL  ZEROS)  OR
WP1021        (CCICLO-CTVDA    OF RUCAB015   EQUAL  ZEROS)  OR
WP1021        (CMANUT-SOLO     OF RUCAB015   EQUAL  ZEROS)
WP1021         MOVE  ALL '*'       TO  RUCA66-COR0001-21
WP1021     ELSE
WP1021         MOVE  CTPO-SOLO-AGCLT OF RUCAB015
WP1021                             TO   WRK-CTPO-SOLO-AGCLT
WP1021         MOVE  CCICLO-CTVDA    OF RUCAB015
WP1021                             TO   WRK-CCICLO-CTVDA
WP1021         MOVE  CMANUT-SOLO     OF RUCAB015
WP1021                             TO   WRK-CMANUT-SOLO
WP1021         MOVE  WRK-CTPO-SOLO-AGCLT-X  TO RUCA66-TIPO-SOLO
WP1021         MOVE  WRK-CCICLO-CTVDA-X     TO RUCA66-CICLO-CULTURA
WP1021         MOVE  WRK-CMANUT-SOLO-X      TO RUCA66-TIPO-MANEJO
WP1021     END-IF.
WP1021
WP1021     MOVE  RUCA66-COR0001-21     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
WP1021                                     LENGTH OF RUCA66-COR0001-21)
WP1021     ADD   LENGTH OF RUCA66-COR0001-21
WP1021                                 TO  ACU-TAM
WP1021                                     ACU-POSICAO.
WP1021
WP1021*----------------------------------------------------------------*
WP1021 5107-99-FIM.  EXIT.
WP1021*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5110-FORMATAR-GRUPO-RENEGC      SECTION.
      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-NUMREF-BCCOR-RENEGC
           MOVE  ALL '*'               TO  RUCA66-VLR-RENEGC
           MOVE  ALL '*'               TO  RUCA66-BASE-LEGAL-RENEGC

           MOVE  RUCA66-COR0001-4      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-4)

           ADD  LENGTH OF RUCA66-COR0001-4
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5110-99-FIM.  EXIT.
      *----------------------------------------------------------------*

STEFA4*----------------------------------------------------------------*
.      5111-FORMATAR-CAMPO-QTD         SECTION.
.     *----------------------------------------------------------------*
.
.          IF (QUND-FINCD-RURAL        OF  RUCAB015
.                                      GREATER ZEROS AND
.              QUND-ANMAL              OF  RUCAB015
.                                      GREATER ZEROS)
.              MOVE  QUND-ANMAL        OF  RUCAB015
.                                      TO  WRK-S13V99-DRESS
.              MOVE  WRK-N09V99-DRESS  TO  RUCA66-QTD-ITEM-FINCD-N
.              GO                      TO  5111-99-FIM
.          END-IF
.
.          IF  (QUND-FINCD-RURAL       OF  RUCAB015
.                                      GREATER ZEROS AND
.               QUND-ANMAL             OF  RUCAB015
.                                      EQUAL  ZEROS)
.              MOVE  QUND-FINCD-RURAL  OF  RUCAB015
.                                      TO  WRK-S13V99-DRESS
.              MOVE  WRK-N09V99-DRESS  TO  RUCA66-QTD-ITEM-FINCD-N
.          ELSE
.           IF (QUND-FINCD-RURAL       OF  RUCAB015
.                                      EQUAL ZEROS AND
.               QUND-ANMAL             OF  RUCAB015
.                                      GREATER  ZEROS)
.              MOVE  QUND-ANMAL        OF  RUCAB015
.                                      TO  WRK-S13V99-DRESS
.              MOVE  WRK-N09V99-DRESS  TO  RUCA66-QTD-ITEM-FINCD-N
.           END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      5111-99-FIM.                    EXIT.
STEFA4*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5115-SELECT-RURCB000            SECTION.
      *----------------------------------------------------------------*

           MOVE  CORIGE-REC            OF  RUCAB015
                                       TO  CORIGE-REC       OF RURCB000

           EXEC SQL
                SELECT
                        CPLANO_REFIN_RURAL           ,
                        CPROG_RURAL
                  INTO
                        :RURCB000.CPLANO-REFIN-RURAL ,
                        :RURCB000.CPROG-RURAL
                  FROM  DB2PRD.ORIGEM_RECURSO
                 WHERE  CORIGE_REC  = :RURCB000.CORIGE-REC

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORIGEM_RECURSO'   TO  ERR-DBD-TAB
               MOVE  'SELECT'          TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0012'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  08                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-08        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5115-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5120-SELECT-RURCB028            SECTION.
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
               MOVE '0013'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  09                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-09        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *---------------------------------------------------------------*
       5120-99-FIM.  EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       5130-FORMATAR-GRUPO-GLEBA       SECTION.
      *----------------------------------------------------------------*

           MOVE  'S'                   TO  WRK-FIM-CSR03
                                           WRK-FIM-CSR04
STEFAB                                     WRK-FIM-CSR14
                                           WRK-FIM-CSR05
STEFAB                                     WRK-FIM-CSR15
           MOVE  'N'                   TO  WRK-TEM-CONTINUACAO
           MOVE  ZEROS                 TO  WRK-CSEQ-MATR-RURAL-ANT
STEFA4     MOVE  ZEROS                 TO  ACU-QTD-GLEBA
7C2511*   XMOVE  ZEROS                 TO  ACU-QTD-GLEBA-E
STEFA1     MOVE  'S'                   TO  WRK-PRIMEIRA-VEZ

           PERFORM  5135-PROCURAR-GLEBA.

STEFA1*   XIF  ACU-QTD-GLEBA        EQUAL  ZEROS
.     *       XPERFORM  5170-TRATAR-SEM-GLEBA
.     *       XPERFORM  5165-TRATAR-AREA-NAO-CULT
.     *       XPERFORM  6170-TRATAR-SEM-GLEBA
STEFA1*   XEND-IF.

STEFAB*   XIF  ACU-QTD-GLEBA-E      EQUAL  ZEROS
.     *       XPERFORM  5165-TRATAR-AREA-NAO-CULT
.     *       XPERFORM  6170-TRATAR-SEM-GLEBA
STEFAB*   XEND-IF.

7C2511*   XIF  ACU-QTD-GLEBA        EQUAL  ZEROS AND
7C2511*        ACU-QTD-GLEBA-E      EQUAL  ZEROS
7C2511     IF  ACU-QTD-GLEBA        EQUAL  ZEROS
.              IF  RUCA66-IDENTC-GLEBA
.                                   EQUAL SPACES OR LOW-VALUES
.                  PERFORM  5170-TRATAR-SEM-GLEBA
.                  PERFORM  6170-TRATAR-SEM-GLEBA
.              END-IF
STEFA4     END-IF.
      *
      *----------------------------------------------------------------*
       5130-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5135-PROCURAR-GLEBA             SECTION.
      *----------------------------------------------------------------*

           IF   WRK-FIM-CSR03          EQUAL  'S'
                PERFORM  5175-OPEN-CSR03-RUECB040
                PERFORM  5180-FETCH-CSR03-RUECB040
           END-IF.

           MOVE  ZEROS                 TO  ACU-QTD-GLEBA
7C2511*   XMOVE  ZEROS                 TO  ACU-QTD-GLEBA-E

           PERFORM
7C2511*      UNTIL  ACU-QTD-GLEBA   EQUAL  90
7C2511       UNTIL  ACU-QTD-GLEBA   EQUAL  350
7C2511          OR  WRK-NUM-POLIGN  GREATER 1
                OR  WRK-FIM-CSR03   EQUAL  'S'

STEFAB** BUSCAR GLEBAS DA AREA CULTIVADA

                PERFORM  5140-OBTER-PONTOS-GEO

STEFA1*        XIF  ACU-QTD-GLEBA        EQUAL  ZEROS
.     *            XPERFORM  5170-TRATAR-SEM-GLEBA
STEFA1*        XEND-IF

                IF  WRK-FIM-CSR04        EQUAL  'S'  AND
                    WRK-FIM-CSR05        EQUAL  'S'
                    IF WRK-NUM-POLIGN    LESS 2
7C2510                 PERFORM  5141-MONTAR-POLIGONO
                    END-IF

                    PERFORM  5180-FETCH-CSR03-RUECB040
                END-IF
           END-PERFORM.

           IF  WRK-FIM-CSR03        EQUAL  'S'
               PERFORM  5185-CLOSE-CSR03-RUECB040
               MOVE  'N'               TO  WRK-TEM-CONTINUACAO
           ELSE
7C2510         MOVE ZEROS              TO  WRK-NUM-POLIGN
               MOVE  'S'               TO  WRK-TEM-CONTINUACAO
           END-IF.

      *----------------------------------------------------------------*
       5135-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5140-OBTER-PONTOS-GEO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FIM-CSR04        EQUAL  'S'
               PERFORM  5190-OPEN-CSR04-RUCAB016
               PERFORM  5195-FETCH-CSR04-RUCAB016
               MOVE  ZEROS             TO  WRK-NSEQ-GLEBA-IMOV-ANT
           END-IF.

           MOVE  'N'                   TO  WRK-TEM-GLEBA

           IF  WRK-FIM-CSR04        EQUAL  'N'
               PERFORM  5145-OBTER-COORD-COM-ALT
           ELSE
               PERFORM  5155-OBTER-COORD-SEM-ALT
           END-IF.

           IF  WRK-FIM-CSR04    EQUAL  'S'
               PERFORM  5200-CLOSE-CSR04-RUCAB016
           END-IF.

      *----------------------------------------------------------------*
       5140-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5141-MONTAR-POLIGONO            SECTION.
      *----------------------------------------------------------------*


           IF WRK-QTD-CULTVD           EQUAL ZEROS
              MOVE ALL '*'             TO WRK-POLIGN
              GO                       TO 5141-99-FIM
           END-IF.

           MOVE SPACE                  TO WRK-POLIGN
           MOVE 1                      TO WRK-TAM-POLIGN

           MOVE 'POLYGON(('            TO WRK-POLIGN(WRK-TAM-POLIGN:9)
           ADD 9                       TO WRK-TAM-POLIGN

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-QTD-CULTVD
              IF IND-1                 GREATER 1
                 MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
                 ADD 1                 TO WRK-TAM-POLIGN
              END-IF

              MOVE WRK-PONTO-CULTVD(IND-1)
                                       TO WRK-POLIGN
                                         (WRK-TAM-POLIGN: LENGTH OF
                                          WRK-PONTO-CULTVD(IND-1))

              ADD LENGTH               OF WRK-PONTO-CULTVD(IND-1)
                                       TO WRK-TAM-POLIGN
           END-PERFORM.

           MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1)
           ADD 1                       TO WRK-TAM-POLIGN

           IF WRK-QTD-NCULTVD          GREATER ZEROS
              MOVE ',('                TO WRK-POLIGN(WRK-TAM-POLIGN:2)
              ADD 2                    TO WRK-TAM-POLIGN
           END-IF.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-QTD-NCULTVD
              IF IND-1                 GREATER 1
                 MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
                 ADD 1                 TO WRK-TAM-POLIGN
              END-IF

              MOVE WRK-PONTO-NCULTVD(IND-1)
                                       TO WRK-POLIGN
                                         (WRK-TAM-POLIGN:LENGTH OF
                                          WRK-PONTO-NCULTVD(IND-1))

              ADD LENGTH               OF WRK-PONTO-NCULTVD(IND-1)
                                       TO WRK-TAM-POLIGN
           END-PERFORM.

           IF WRK-QTD-NCULTVD          GREATER ZEROS
              MOVE ')'                 TO WRK-POLIGN(WRK-TAM-POLIGN:1)
              ADD 1                    TO WRK-TAM-POLIGN
           END-IF.

           MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1)
           ADD 1                       TO WRK-TAM-POLIGN.

           MOVE  WRK-POLIGN            TO  RUCA66-WKTPART
           MOVE  RUCA66-COR0001-7      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-7)
           ADD   LENGTH                OF  RUCA66-COR0001-7
                                       TO  ACU-TAM
                                           ACU-POSICAO.

           PERFORM                      VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-QTD-CULTVD
              MOVE SPACES              TO WRK-PONTO-CULTVD(IND-1)
           END-PERFORM

           PERFORM                      VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER WRK-QTD-NCULTVD
              MOVE SPACES              TO WRK-PONTO-NCULTVD(IND-1)
           END-PERFORM


           MOVE ZEROS                  TO WRK-QTD-CULTVD
                                          WRK-QTD-NCULTVD.
7C2510     ADD 1                       TO WRK-NUM-POLIGN.

      *----------------------------------------------------------------*
       5141-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5145-OBTER-COORD-COM-ALT        SECTION.
      *----------------------------------------------------------------*

           PERFORM
             UNTIL  WRK-FIM-CSR04   EQUAL  'S'
7C2511*         OR  ACU-QTD-GLEBA   EQUAL  90
7C2511          OR  ACU-QTD-GLEBA   EQUAL  350
7C2511          OR  WRK-NUM-POLIGN  GREATER 1

                IF  NSEQ-GLEBA-IMOV OF RUCAB016  GREATER  ZEROS
                    PERFORM  5150-FORMATAR-COORD-ALT

7C2511*            XIF  ACU-QTD-GLEBA  LESS  90
7C2511              IF  ACU-QTD-GLEBA  LESS  350
                        PERFORM  5195-FETCH-CSR04-RUCAB016
                    END-IF
                ELSE
                    PERFORM  5195-FETCH-CSR04-RUCAB016
                END-IF
           END-PERFORM.

STEFA4*   XIF  WRK-FIM-CSR04           EQUAL 'S'
STEFA4*        _IF ACU-QTD-GLEBA-E      EQUAL ZEROS
.     *          XPERFORM  5165-TRATAR-AREA-NAO-CULT
.     *          XMOVE 'S'             TO WRK-FIM-CSR14
.     *          XMOVE 'S'             TO WRK-FIM-CSR15
.     *          XPERFORM  6140-OBTER-PONTOS-GEO
STEFA4*        _END-IF
STEFA4*   XEND-IF.

STEFA4     IF  WRK-FIM-CSR04           EQUAL 'S'
.              IF ACU-QTD-GLEBA        EQUAL ZEROS
.                 PERFORM 5170-TRATAR-SEM-GLEBA
.                 PERFORM 6170-TRATAR-SEM-GLEBA
.              ELSE
.                 PERFORM  5165-TRATAR-AREA-NAO-CULT
.                 MOVE 'S'             TO WRK-FIM-CSR14
.                 MOVE 'S'             TO WRK-FIM-CSR15
.                 PERFORM  6140-OBTER-PONTOS-GEO
.              END-IF
STEFA4     END-IF.

      *----------------------------------------------------------------*
       5145-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5150-FORMATAR-COORD-ALT         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CSEQ-MATR-RURAL-ANT NOT EQUAL
                                         CSEQ-MATR-RURAL OF RUCAB016  OR
               WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
                                         NSEQ-GLEBA-IMOV OF RUCAB016

STEFA2*       *XF  WRK-CSEQ-MATR-RURAL-ANT GREATER  ZEROS AND
STEFA2*           *XRK-PRIMEIRA-VEZ        EQUAL 'N'
STEFA2*           *XOVE  'N'               TO  WRK-GLEBA-7B
STEFA2*           *XERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA2*           *XOVE 'S'                TO WRK-FIM-CSR14
STEFA2*           *XOVE 'S'                TO WRK-FIM-CSR15
STEFA2*           *XERFORM  6140-OBTER-PONTOS-GEO
STEFA2*       *XLSE
STEFA2*           *XOVE 'N'                TO WRK-PRIMEIRA-VEZ
STEFA2*       *XND-IF

STEFA3         IF  WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
STEFA3                                     NSEQ-GLEBA-IMOV OF RUCAB016
STEFA3                                     AND
STEFA3             WRK-PRIMEIRA-VEZ        EQUAL 'N'
STEFA3             MOVE  'N'               TO  WRK-GLEBA-7B
STEFA3             PERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA3             MOVE 'S'                TO WRK-FIM-CSR14
STEFA3             MOVE 'S'                TO WRK-FIM-CSR15
STEFA3             PERFORM  6140-OBTER-PONTOS-GEO
7C2510             PERFORM  5141-MONTAR-POLIGONO
STEFA3         ELSE
STEFA3             MOVE 'N'                TO WRK-PRIMEIRA-VEZ
7C2511*  RETIRANDO CODIGO MUNICIPIO QUANDO POSSUI COORDENADAS
7C2511             MOVE ALL '*' TO WRK-MSGEM-RURAL(ACU-POSICAO - 6: 6)
STEFA3         END-IF


               MOVE  CSEQ-MATR-RURAL   OF  RUCAB016
                                       TO  WRK-CSEQ-MATR-RURAL-ANT
                                           WRK-S11-DRESS
               MOVE  WRK-N03-DRESS     TO  WRK-SEQ-MATR-N03
               MOVE  NSEQ-GLEBA-IMOV   OF  RUCAB016
                                       TO  WRK-NSEQ-GLEBA-IMOV-ANT
                                           WRK-S11-DRESS
               MOVE  WRK-N05-DRESS     TO  WRK-SEQ-GLEBA-N05
               MOVE  WRK-SEQ-GLEBA-X19 TO  RUCA66-IDENTC-GLEBA
7C2510         MOVE  ALL '*'           TO  RUCA66-AREA-NCULTVD

7C2510         IF WRK-NUM-POLIGN       LESS 2
7C2510            MOVE  RUCA66-COR0001-6
7C2510                                 TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510                                     LENGTH OF RUCA66-COR0001-6)
7C2510            ADD LENGTH OF RUCA66-COR0001-6
7C2510                                 TO  ACU-TAM
7C2510                                     ACU-POSICAO
               END-IF
           END-IF.

7C2510     ADD 1                       TO  WRK-QTD-CULTVD
           MOVE  MLATTD-IMOV-RURAL     OF  RUCAB016
                                       TO  WRK-MLATTD-IMOV-RURAL-N
7C2510*                                TO  RUCA66-LAT-PONTO
7C2510     IF WRK-MLATTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)
7C2510        MOVE '-'                 TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)
           END-IF
7C2510     INSPECT WRK-CULTVD-LAT(WRK-QTD-CULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

           MOVE  MLOGTD-IMOV-RURAL     OF  RUCAB016
                                       TO  WRK-MLOGTD-IMOV-RURAL-N

7C2510     IF WRK-MLOGTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)
7C2510        MOVE '-'                 TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)
           END-IF
7C2510     INSPECT WRK-CULTVD-LON(WRK-QTD-CULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

7C2510*                                TO  RUCA66-LONG-PONTO

7C2510*   XMOVE  MALTTD-IMOV-RURAL     OF  RUCAB016
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*   XMOVE  WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-ALTT-PONTO

7C2510*   XADD   MAREA-NAO-CTVDA       OF  RUCAB016
7C2510*                                TO  ACU-MAREA-NAO-CTVDA

7C2510*   XMOVE  RUCA66-COR0001-7      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO

           MOVE  'S'                   TO  WRK-TEM-GLEBA

           ADD   1                     TO  ACU-QTD-GLEBA.

      *----------------------------------------------------------------*
       5150-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5155-OBTER-COORD-SEM-ALT        SECTION.
      *----------------------------------------------------------------*

           IF   WRK-FIM-CSR05       EQUAL  'S'
                PERFORM  5205-OPEN-CSR05-RUECB09N
                PERFORM  5210-FETCH-CSR05-RUECB09N
                MOVE  ZEROS            TO  WRK-NSEQ-GLEBA-IMOV-ANT
           END-IF.

           PERFORM
             UNTIL  WRK-FIM-CSR05   EQUAL  'S'
7C2511*         OR  ACU-QTD-GLEBA   EQUAL  90
7C2511          OR  ACU-QTD-GLEBA   EQUAL  350
7C2511          OR  WRK-NUM-POLIGN  GREATER 1

                PERFORM  5160-FORMATAR-COORD-SEM-ALT

7C2511*        XIF  ACU-QTD-GLEBA    LESS  90
7C2511          IF  ACU-QTD-GLEBA    LESS  350
7C2511          OR  WRK-NUM-POLIGN   GREATER 1
                    PERFORM  5210-FETCH-CSR05-RUECB09N
                END-IF
           END-PERFORM.

           IF  WRK-FIM-CSR05    EQUAL  'S'
               PERFORM  5215-CLOSE-CSR05-RUECB09N
STEFA2*        XF  WRK-CSEQ-MATR-RURAL-ANT GREATER  ZEROS AND
STEFA2*           XACU-QTD-GLEBA           EQUAL ZEROS
STEFA2*        _IF  ACU-QTD-GLEBA-E         EQUAL ZEROS
STEFA2         IF  ACU-QTD-GLEBA
STEFA2                          EQUAL  ZEROS
STEFA2             PERFORM  5170-TRATAR-SEM-GLEBA
STEFA2*           XPERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA2             PERFORM  6170-TRATAR-SEM-GLEBA
STEFA2         ELSE
STEFA2             PERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA2             MOVE 'S'     TO WRK-FIM-CSR14
STEFA2             MOVE 'S'     TO WRK-FIM-CSR15
STEFA2             PERFORM  6140-OBTER-PONTOS-GEO
STEFA2         END-IF
           END-IF.

      *----------------------------------------------------------------*
       5155-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5160-FORMATAR-COORD-SEM-ALT     SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CSEQ-MATR-RURAL-ANT NOT EQUAL
                                         CSEQ-MATR-RURAL OF RUECB09N  OR
               WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
                                         NSEQ-GLEBA-IMOV OF  RUECB09N


STEFA2*        _IF  WRK-CSEQ-MATR-RURAL-ANT GREATER  ZEROS AND
STEFA2*           *WRK-PRIMEIRA-VEZ        EQUAL 'N'
STEFA2*           *OVE  'N'               TO  WRK-GLEBA-7B
STEFA2*           *ERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA2*           *OVE 'S'                TO WRK-FIM-CSR14
STEFA2*           *OVE 'S'                TO WRK-FIM-CSR15
STEFA2*           *ERFORM  6140-OBTER-PONTOS-GEO
STEFA2*       *LSE
STEFA2*           *OVE 'N'                TO WRK-PRIMEIRA-VEZ
STEFA2*       *ND-IF

STEFA3         IF  WRK-NSEQ-GLEBA-IMOV-ANT NOT EQUAL
STEFA3                                     NSEQ-GLEBA-IMOV OF RUCAB016
STEFA3                                     AND
STEFA3             WRK-PRIMEIRA-VEZ        EQUAL 'N'
STEFA3             MOVE  'N'               TO  WRK-GLEBA-7B
STEFA3             PERFORM  5165-TRATAR-AREA-NAO-CULT
STEFA3             MOVE 'S'                TO WRK-FIM-CSR14
STEFA3             MOVE 'S'                TO WRK-FIM-CSR15
STEFA3             PERFORM  6140-OBTER-PONTOS-GEO
7C2510             PERFORM  5141-MONTAR-POLIGONO
STEFA3         ELSE
7C2511*  RETIRANDO CODIGO MUNICIPIO QUANDO POSSUI COORDENADAS
7C2511             MOVE ALL '*' TO WRK-MSGEM-RURAL(ACU-POSICAO - 6: 6)
STEFA3             MOVE 'N'                TO WRK-PRIMEIRA-VEZ
STEFA3         END-IF

               MOVE  CSEQ-MATR-RURAL   OF  RUECB09N
                                       TO  WRK-CSEQ-MATR-RURAL-ANT
                                           WRK-S11-DRESS
               MOVE  WRK-N03-DRESS     TO  WRK-SEQ-MATR-N03
               MOVE  NSEQ-GLEBA-IMOV   OF  RUECB09N
                                       TO  WRK-NSEQ-GLEBA-IMOV-ANT
                                           WRK-S11-DRESS
               MOVE  WRK-N05-DRESS     TO  WRK-SEQ-GLEBA-N05
               MOVE  WRK-SEQ-GLEBA-X19 TO  RUCA66-IDENTC-GLEBA

7C2510         MOVE  ALL '*'           TO  RUCA66-AREA-NCULTVD
7C2510         IF WRK-NUM-POLIGN       LESS 2
                  MOVE  RUCA66-COR0001-6
                                       TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-6)
                  ADD LENGTH OF RUCA66-COR0001-6
                                       TO  ACU-TAM
                                           ACU-POSICAO
               END-IF
           END-IF.

           ADD 1                       TO  WRK-QTD-CULTVD
           MOVE  MLATTD-IMOV-RURAL     OF  RUECB09N
                                       TO  WRK-MLATTD-IMOV-RURAL-N
7C2510*                                TO  RUCA66-LAT-PONTO
7C2510     IF WRK-MLATTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)
7C2510        MOVE '-'                 TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
7C2510                                    (WRK-QTD-CULTVD)
7C2510     END-IF
7C2510     INSPECT WRK-CULTVD-LAT(WRK-QTD-CULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

           MOVE  MLOGTD-IMOV-RURAL     OF  RUECB09N
                                       TO  WRK-MLOGTD-IMOV-RURAL-N
7C2510     IF WRK-MLOGTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)
7C2510        MOVE '-'                 TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
7C2510                                    (WRK-QTD-CULTVD)
           END-IF
7C2510     INSPECT WRK-CULTVD-LON(WRK-QTD-CULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

7C2510*                                TO  RUCA66-LONG-PONTO

7C2510*    M0VE  MALTTD-IMOV-RURAL     OF  RUECB09N
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*    M0VE  WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-ALTT-PONTO

7C2510*   XMOVE  RUCA66-COR0001-7      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO

           MOVE  'S'                   TO  WRK-TEM-GLEBA

           ADD   1                     TO  ACU-QTD-GLEBA.

      *----------------------------------------------------------------*
       5160-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5165-TRATAR-AREA-NAO-CULT       SECTION.
      *----------------------------------------------------------------*

7C2510*   XMOVE  ACU-MAREA-NAO-CTVDA   TO  WRK-MAREA-NAO-CTVDA-N
7C2510*   XMOVE  WRK-MAREA-NAO-CTVDA-X TO  RUCA66-AREA-NAO-CULT

7C2510*   XMOVE  ALL '*'               TO  RUCA66-COR0001-7A
7C2510*   XMOVE  RUCA66-COR0001-7A     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7A)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7A
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO.

           MOVE  ZEROS                 TO  ACU-MAREA-NAO-CTVDA.

STE001* --- TRATAR GLEBA EXCLUIDA
STE001*   XPERFORM  5131-FORMATAR-GRUPO-GLEBA-E.

      *----------------------------------------------------------------*
       5165-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5170-TRATAR-SEM-GLEBA           SECTION.
      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-IDENTC-GLEBA
                                           RUCA66-AREA-NCULTVD
           MOVE  RUCA66-COR0001-6      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-6)
           ADD   LENGTH OF RUCA66-COR0001-6
                                       TO  ACU-TAM
                                           ACU-POSICAO

7C2510*   XM0VE  ALL '*'               TO  RUCA66-LAT-PONTO
7C2510*    M0VE  ALL '*'               TO  RUCA66-LONG-PONTO
7C2510*    M0VE  ALL '*'               TO  RUCA66-ALTT-PONTO
7C2510     MOVE  ALL '*'               TO  RUCA66-WKTPART

           MOVE  RUCA66-COR0001-7      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-7)
           ADD   LENGTH OF RUCA66-COR0001-7
                                       TO  ACU-TAM
                                           ACU-POSICAO.

7C2510*   XMOVE  ALL '*'               TO  RUCA66-AREA-NAO-CULT
7C2510*
7C2510*   XMOVE  RUCA66-COR0001-7A     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7A)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7A
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO.

      *----------------------------------------------------------------*
       5170-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5175-OPEN-CSR03-RUECB040        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR OF RUCAB015    TO  CCONTR           OF RUECB040
           MOVE  ZEROS                 TO  CSEQ-MATR-RURAL  OF RUECB040
           MOVE  'N'                   TO  WRK-FIM-CSR03

           EXEC SQL
                OPEN CSR03-RUECB040
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0015'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5175-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5180-FETCH-CSR03-RUECB040       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR03-RUECB040
                INTO   :RUECB040.CCONTR            ,
                       :RUECB040.CSEQ-MATR-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0016'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR03
           END-IF.

      *----------------------------------------------------------------*
       5180-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5185-CLOSE-CSR03-RUECB040       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR03-RUECB040
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'MATR_CONTR_RURAL' TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0017'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5185-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5190-OPEN-CSR04-RUCAB016        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB015
                                       TO  CCONTR            OF RUCAB016
           MOVE  HINCL-REG             OF  RUCAB015
                                       TO  HINCL-REG         OF RUCAB016
           MOVE  CSEQ-MATR-RURAL       OF  RUECB040
                                       TO  CSEQ-MATR-RURAL   OF RUCAB016
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUCAB016
                                           NCORND-GEOGR-IMOV OF RUCAB016
           MOVE  'N'                   TO  WRK-FIM-CSR04

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
               MOVE '0018'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5190-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5195-FETCH-CSR04-RUCAB016       SECTION.
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
                         :WRK-B016-MAREA-NAO-CTVDA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_CORND_GEOGR'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0019'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  ZEROS             TO  MAREA-NAO-CTVDA OF RUCAB016
               MOVE  'S'               TO  WRK-FIM-CSR04
           ELSE
               IF  WRK-B016-MAREA-NAO-CTVDA  EQUAL  -1
                   MOVE  ZEROS         TO  MAREA-NAO-CTVDA OF RUCAB016
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5195-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-CLOSE-CSR04-RUCAB016       SECTION.
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
               MOVE '0020'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5200-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5205-OPEN-CSR05-RUECB09N        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUECB040
                                       TO  CCONTR            OF RUECB09N
           MOVE  CSEQ-MATR-RURAL       OF  RUECB040
                                       TO  CSEQ-MATR-RURAL   OF RUECB09N
           MOVE  ZEROS                 TO  NSEQ-GLEBA-IMOV   OF RUECB09N
                                           NCORND-GEOGR-IMOV OF RUECB09N
           MOVE  'N'                   TO  WRK-FIM-CSR05


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
               MOVE '0021'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5205-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5210-FETCH-CSR05-RUECB09N       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR05-RUECB09N
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
               MOVE '0022'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR05
           END-IF.

      *----------------------------------------------------------------*
       5210-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5215-CLOSE-CSR05-RUECB09N       SECTION.
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
               MOVE '0023'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5215-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5220-FORMATAR-PROD-CONSCD       SECTION.
      *----------------------------------------------------------------*

           PERFORM  5225-OPEN-CSR06-RUECB09G
           PERFORM  5230-FETCH-CSR06-RUECB09G

           IF  WRK-FIM-CSR06        EQUAL  'S'
               MOVE  ALL '*'           TO RUCA66-COD-PRODT-CONSCD

               MOVE  RUCA66-COR0001-8  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-8)
               ADD   LENGTH OF RUCA66-COR0001-8
                                       TO  ACU-TAM
                                           ACU-POSICAO
           END-IF.

           PERFORM
             UNTIL  WRK-FIM-CSR06   EQUAL  'S'

             MOVE  CPRODT-RURAL        OF  RUECB09G
                                       TO  CPRODT-RURAL  OF  RURCB006
             PERFORM  5240-SELECT-RURCB006

             MOVE  CPRODT-BACEN        OF  RURCB006
                                       TO  WRK-X05-DRESS
             MOVE  WRK-X04-DRESS       TO  RUCA66-COD-PRODT-CONSCD

             MOVE  RUCA66-COR0001-8    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-8)
             ADD   LENGTH OF RUCA66-COR0001-8
                                       TO  ACU-TAM
                                           ACU-POSICAO

             PERFORM  5230-FETCH-CSR06-RUECB09G

           END-PERFORM.

           PERFORM  5235-CLOSE-CSR06-RUECB09G.

      *----------------------------------------------------------------*
       5220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5225-OPEN-CSR06-RUECB09G        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR                OF  RUCAB015
                                       TO  CCONTR-CREDT-RURAL
                                                             OF RUECB09G
           MOVE  'N'                   TO  WRK-FIM-CSR06

           EXEC SQL
                OPEN CSR06-RUECB09G
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL-CSCDO'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0024'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5225-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5230-FETCH-CSR06-RUECB09G       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR06-RUECB09G
                INTO   :RUECB09G.NPRODT-RURAL-CSCDO ,
                       :RUECB09G.CPRODT-RURAL
                          :WRK-B09G-CPRODT-RURAL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0025'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR06
           ELSE
               IF  WRK-B09G-CPRODT-RURAL  EQUAL -1
                   MOVE  ZEROS         TO  CPRODT-RURAL  OF RUECB09G
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5230-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5235-CLOSE-CSR06-RUECB09G       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR06-RUECB09G
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_CSCDO'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0026'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5235-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5240-SELECT-RURCB006            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                SELECT
                        CPRODT_RURAL               ,
                        CPRODT_BACEN               ,
                        CTEMPO_VIDA_CULTR
                  INTO
                        :RURCB006.CPRODT-RURAL     ,
                        :RURCB006.CPRODT-BACEN     ,
                        :RURCB006.CTEMPO-VIDA-CULTR
                          :WRK-B006-CTEMPO-VIDA-CULTR
                  FROM  DB2PRD.PRODUTO_RURAL
                 WHERE  CPRODT_RURAL  = :RURCB006.CPRODT-RURAL

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'            )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'PRODUTO_RURAL'    TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0027'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  11                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-11        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  WRK-B006-CTEMPO-VIDA-CULTR  EQUAL  -1
               MOVE  ZEROS             TO  CTEMPO-VIDA-CULTR OF RURCB006
           END-IF.

      *----------------------------------------------------------------*
       5240-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5245-CHAMAR-RUEC8800            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      RUEC58-AREA

           MOVE  'RUCA8810'            TO  RUEC58-PGM-CHAMADOR
           MOVE  CCONTR                OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N09-DRESS         TO  RUEC58-CONTRATO
           MOVE  CFNALD-RURAL          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUEC58-FINALIDADE
           MOVE  CATVDD-RURAL          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUEC58-ATIVIDADE
           MOVE  CMODLD-RURAL          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-MODALIDADE
           MOVE  CPRODT-RURAL          OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUEC58-PRODUTO
           MOVE  CVAR-PRODT-RURAL      OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-VARIEDADE
           MOVE  CCESTA-SAFRA-RURAL    OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-CESTA-SAFRA
           MOVE  CINDCD-ZNMTO          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N01-DRESS         TO  RUEC58-ZONEAMENTO
           MOVE  CINDCD-CSCIO          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N01-DRESS         TO  RUEC58-CONSORCIO
           MOVE  CTPO-AGROP-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-TIPO-AGROP
           MOVE  CTPO-INTGC-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-TIPO-INTEG
           MOVE  CINDCD-UND-PRDTV      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N01-DRESS         TO  RUEC58-GRAO-SEMENTE
           MOVE  CTPO-IGCAO-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-TIPO-IRRIG
           MOVE  CTPO-CTIVO-RURAL      OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-TIPO-CULTIVO
           MOVE  CCICLO-PROD-RURAL     OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N05-DRESS         TO  RUEC58-CICLO-PROD
PRI499     MOVE  CCLIMT-RURAL-BACEN    OF  RUCAB015
PRI499                                 TO  WRK-S11-DRESS
PRI499     MOVE  WRK-N02-DRESS         TO  RUEC58-CLIMA-PROD
STE001     MOVE  COUTRO-TRATO-SOLO     OF  RUCAB015
.                                      TO  WRK-S11-DRESS
STE001     MOVE  WRK-N05-DRESS         TO  RUEC58-COUTRO-TRATO-SOLO
.
.          MOVE  PREDC-JURO-AMBTL      OF  RUFIB002
.                                      TO  WRK-S13V99-DRESS
.          MOVE  WRK-N03V99-DRESS      TO  RUEC58-PREDC-JURO-AMBTL
.
.          MOVE  CINDCD-CONFM-FLORS    OF  RUCAB015
.                                      TO  WRK-S11-DRESS
.          MOVE  WRK-N01-DRESS         TO  WRK-N01-DRESS-9
.          MOVE  WRK-N01-DRESS-X       TO  RUEC58-CINDCD-CONFM-FLORS
.          MOVE  CINDCD-CONFM-IBAMA    OF  RUCAB015
.                                      TO  WRK-S11-DRESS
.          MOVE  WRK-N01-DRESS         TO  WRK-N01-DRESS-9
.          MOVE  WRK-N01-DRESS-X       TO  RUEC58-CINDCD-CONFM-IBAMA
.          MOVE  CINDCD-CONFM-CSVAO    OF  RUCAB015
.                                      TO  WRK-S11-DRESS
.          MOVE  WRK-N01-DRESS         TO  WRK-N01-DRESS-9
.          MOVE  WRK-N01-DRESS-X       TO  RUEC58-CINDCD-CONFM-CSVAO
.          MOVE  RREGLZ-IBAMA          OF  RUCAB015(1:LENGTH OF
.                                          RUEC58-RREGLZ-IBAMA)
STE001                                 TO  RUEC58-RREGLZ-IBAMA
STEFA1     MOVE  CINDCD-CONFM-RURAL    OF  RUCAB015
STEFA1                                 TO  RUEC58-IND-RCONFDD-IM-RURAL

STEFA1     IF  WRK-COUTRO-TPO-CONFM-NULL
.                                      NOT EQUAL ZEROS
.              MOVE ZEROS              TO COUTRO-TPO-CONFM OF  RUCAB015
STEFA1     END-IF

STEFA1     MOVE  COUTRO-TPO-CONFM      OF  RUCAB015
STEFA1                                 TO  WRK-S11-DRESS
STEFA1     MOVE  WRK-N05-DRESS         TO  RUEC58-TP-CONFDD-OTR
STEFA1     MOVE  CCNPJ-INSTC-EXIGB     OF  RUCAB015
STEFA1                                 TO  WRK-S11-DRESS
STEFA1     MOVE  WRK-N08-DRESS         TO  RUEC58-CNPJ-BASE-EXIGLDD
           MOVE  DCDULA                OF  RUFIB002
                                       TO  RUEC58-DCDULA
           MOVE  'RUEC8800'            TO  WRK-MODULO

           CALL  WRK-MODULO         USING   RUEC58-AREA

           IF  RUEC58-COD-RET       EQUAL  99
               MOVE RUEC58-COD-RET     TO  COMU-8810-COD-RETORNO
               MOVE RUEC58-ERRO-AREA   TO  COMU-8810-ERRO-AREA
               MOVE RUEC58-SQLCA       TO  COMU-8810-SQLCA
               MOVE RUEC58-MENSAGEM(1:75)
                                       TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  RUEC58-COD-RET   NOT EQUAL ZEROS AND 04 AND 05
               MOVE RUEC58-COD-RET     TO  COMU-8810-COD-RETORNO
               MOVE RUEC58-MENSAGEM(1:75)
                                       TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5245-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5260-FORMATAR-PROPRIETARIO      SECTION.
      *----------------------------------------------------------------*

           PERFORM  5280-OPEN-CSR07-RUCAB018
           PERFORM  5285-FETCH-CSR07-RUCAB018

           IF  WRK-FIM-CSR07        EQUAL  'S'
               PERFORM  5265-OBTER-TITULAR-CONTR
           END-IF.

           PERFORM
             UNTIL  WRK-FIM-CSR07   EQUAL  'S'

             PERFORM  5275-MOVER-PROPRIETARIO
             PERFORM  5285-FETCH-CSR07-RUCAB018

           END-PERFORM.

           PERFORM  5290-CLOSE-CSR07-RUCAB018.

      *----------------------------------------------------------------*
       5260-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5265-OBTER-TITULAR-CONTR        SECTION.
      *----------------------------------------------------------------*

           PERFORM  5090-OPEN-CSR02-RUECB014
           PERFORM  5095-FETCH-CSR02-RUECB014

           PERFORM
             UNTIL  WRK-FIM-CSR02   EQUAL  'S'

                IF  CTPO-RESP-PSSOA OF RUECB014  EQUAL  'T'
                    PERFORM  5270-MOVER-PROPR-TITU
                    MOVE  'S'          TO  WRK-FIM-CSR02
                ELSE
                    PERFORM  5095-FETCH-CSR02-RUECB014
                END-IF
           END-PERFORM.

           PERFORM  5100-CLOSE-CSR02-RUECB014.

      *----------------------------------------------------------------*
       5265-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5270-MOVER-PROPR-TITU           SECTION.
      *----------------------------------------------------------------*

           IF  CFLIAL-CGC OF RUECB014  EQUAL  ZEROS
               MOVE  'F'               TO  RUCA66-TP-PESSOA-PROPT-F
               MOVE  CCGC-CPF          OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N09-DRESS     TO  WRK-NUM-CPF-AUX
               MOVE  CCTRL-CPF-CGC     OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N02-DRESS     TO  WRK-CTRL-CPF-AUX
               MOVE  WRK-CPF-AUX-X     TO  RUCA66-CPFPROPT
7C5.10         MOVE  ALL '*'           TO  RUCA66-CIB-F
               MOVE  ALL '*'           TO  RUCA66-CCIR-F
               MOVE  ALL '*'           TO  RUCA66-NRO-CAR-F
BRQ001         MOVE  ALL '*'           TO  RUCA66-NRO-OUT-F
BRQ001         MOVE  ALL '*'           TO  RUCA66-PERC-PRESC-F
               MOVE  RUCA66-COR0001-10-F
                                       TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-10-F)
               ADD   LENGTH OF RUCA66-COR0001-10-F
                                       TO  ACU-TAM
                                           ACU-POSICAO
           ELSE
               MOVE  'J'               TO  RUCA66-TP-PESSOA-PROPT-J
               MOVE  CCGC-CPF          OF  RUECB014
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X08-DRESS     TO  RUCA66-CNPJBASE-PROPT
7C5.10         MOVE  ALL '*'           TO  RUCA66-CIB-J
               MOVE  ALL '*'           TO  RUCA66-CCIR-J
               MOVE  ALL '*'           TO  RUCA66-NRO-CAR-J
BRQ001         MOVE  ALL '*'           TO  RUCA66-NRO-OUT-J
BRQ001         MOVE  ALL '*'           TO  RUCA66-PERC-PRESC-J
               MOVE  RUCA66-COR0001-10-J
                                       TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-10-J)
               ADD   LENGTH OF RUCA66-COR0001-10-J
                                       TO  ACU-TAM
                                           ACU-POSICAO
           END-IF.

      *----------------------------------------------------------------*
       5270-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5275-MOVER-PROPRIETARIO         SECTION.
      *----------------------------------------------------------------*

           IF  CFLIAL-CNPJ OF RUCAB018  EQUAL  ZEROS
               MOVE  'F'               TO  RUCA66-TP-PESSOA-PROPT-F
               MOVE  CCNPJ-CPF         OF  RUCAB018
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N09-DRESS     TO  WRK-NUM-CPF-AUX
               MOVE  CCTRL-CNPJ-CPF    OF  RUCAB018
                                       TO  WRK-S11-DRESS
               MOVE  WRK-N02-DRESS     TO  WRK-CTRL-CPF-AUX
               MOVE  WRK-CPF-AUX-X     TO  RUCA66-CPFPROPT

               IF  CNIRF-IMOV-RURAL OF RUCAB018  EQUAL SPACES
7C5.10             MOVE  ALL '*'       TO  RUCA66-CIB-F
               ELSE
STEFAB             MOVE  CNIRF-IMOV-RURAL  OF  RUCAB018(2:8)
7C5.10                                 TO  RUCA66-CIB-F
               END-IF

               IF  CCCIR OF RUCAB018   EQUAL  SPACES OR LOW-VALUES
                   MOVE  ALL '*'       TO  RUCA66-CCIR-F
               ELSE
                   MOVE  CCCIR         OF  RUCAB018
                                       TO  RUCA66-CCIR-F
               END-IF

               IF  CREG-AMBTL-RURAL OF RUCAB018
                                    EQUAL  SPACES OR LOW-VALUES
                   MOVE  ALL '*'      TO  RUCA66-NRO-CAR-F
               ELSE
                   MOVE  CREG-AMBTL-RURAL OF RUCAB018
                                      TO  RUCA66-NRO-CAR-F
               END-IF

BRQ001         IF  COUTGA-AGUA-IMOV OF RUCAB018
BRQ001                              EQUAL  SPACES OR LOW-VALUES
BRQ001             MOVE  ALL '*'      TO  RUCA66-NRO-OUT-F
BRQ001         ELSE
BRQ001             MOVE  COUTGA-AGUA-IMOV OF RUCAB018
BRQ001                                TO  RUCA66-NRO-OUT-F
BRQ001         END-IF

BRQ001         IF  PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001                              EQUAL  ZEROS
BRQ001             MOVE  ALL '*'      TO  RUCA66-PERC-PRESC-F
BRQ001         ELSE
BRQ001             MOVE  PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001                                TO  WRK-PIC9-3V99-CS
BRQ001             MOVE WRK-PIC9-3V99-SS TO RUCA66-PERC-PRESC-F-N
BRQ001         END-IF

               MOVE  RUCA66-COR0001-10-F
                                       TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-10-F)
               ADD   LENGTH OF RUCA66-COR0001-10-F
                                       TO  ACU-TAM
                                           ACU-POSICAO
           ELSE
               MOVE  'J'               TO  RUCA66-TP-PESSOA-PROPT-J
               MOVE  CCNPJ-CPF         OF  RUCAB018
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X08-DRESS     TO  RUCA66-CNPJBASE-PROPT

               IF  CNIRF-IMOV-RURAL OF RUCAB018  EQUAL SPACES
7C5.10             MOVE  ALL '*'       TO  RUCA66-CIB-J
               ELSE
STEFAB             MOVE  CNIRF-IMOV-RURAL  OF  RUCAB018(2:8)
7C5.10                                 TO  RUCA66-CIB-J
               END-IF

               IF  CCCIR OF RUCAB018   EQUAL  SPACES OR LOW-VALUES
                   MOVE  ALL '*'       TO  RUCA66-CCIR-J
               ELSE
                   MOVE  CCCIR         OF  RUCAB018
                                       TO  RUCA66-CCIR-J
               END-IF

               IF  CREG-AMBTL-RURAL OF RUCAB018
                                    EQUAL  SPACES OR LOW-VALUES
                   MOVE  ALL '*'      TO  RUCA66-NRO-CAR-J
               ELSE
                   MOVE  CREG-AMBTL-RURAL OF RUCAB018
                                      TO  RUCA66-NRO-CAR-J
               END-IF

BRQ001         IF  COUTGA-AGUA-IMOV OF RUCAB018
BRQ001                              EQUAL  SPACES OR LOW-VALUES
BRQ001             MOVE  ALL '*'      TO  RUCA66-NRO-OUT-J
BRQ001         ELSE
BRQ001             MOVE  COUTGA-AGUA-IMOV OF RUCAB018
BRQ001                                TO  RUCA66-NRO-OUT-J
BRQ001         END-IF

BRQ001         IF  PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001                              EQUAL  ZEROS
BRQ001             MOVE  ALL '*'      TO  RUCA66-PERC-PRESC-J
BRQ001         ELSE
BRQ001             MOVE  PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001                                TO  WRK-PIC9-3V99-CS
BRQ001             MOVE WRK-PIC9-3V99-SS TO RUCA66-PERC-PRESC-J-N
BRQ001         END-IF

               MOVE  RUCA66-COR0001-10-J
                                       TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-10-J)
               ADD   LENGTH OF RUCA66-COR0001-10-J
                                       TO  ACU-TAM
                                           ACU-POSICAO
           END-IF.

      *----------------------------------------------------------------*
       5275-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5280-OPEN-CSR07-RUCAB018        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB018
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB018
           MOVE  ZEROS                 TO  NPPRIE-IMOV      OF RUCAB018
           MOVE  'N'                   TO  WRK-FIM-CSR07

           EXEC SQL
                OPEN  CSR07-RUCAB018
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PPRIE_IMOV'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0029'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5280-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5285-FETCH-CSR07-RUCAB018       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR07-RUCAB018
                INTO   :RUCAB018.CCONTR            ,
                       :RUCAB018.HINCL-REG         ,
                       :RUCAB018.NPPRIE-IMOV       ,
                       :RUCAB018.CCNPJ-CPF         ,
                       :RUCAB018.CFLIAL-CNPJ       ,
                       :RUCAB018.CCTRL-CNPJ-CPF    ,
                       :RUCAB018.CTPO-PSSOA        ,
                       :RUCAB018.CNIRF-IMOV-RURAL
                         :WRK-B018-CNIRF-IMOV-RURAL,
                       :RUCAB018.CCCIR
                         :WRK-B018-CCCIR           ,
                       :RUCAB018.CREG-AMBTL-RURAL
                         :WRK-B018-CREG-AMBTL-RURAL,
BRQ001                 :RUCAB018.PPRESV-AMBTL-PPRIE
BRQ001                   :WRK-PPRESV-AMBTL-PPRIE-NULL,
BRQ001                 :RUCAB018.COUTGA-AGUA-IMOV
BRQ001                   :WRK-COUTGA-AGUA-IMOV-NULL
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PPRIE_IMOV'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0030'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR07
           ELSE
               IF  WRK-B018-CNIRF-IMOV-RURAL  EQUAL  -1
                   MOVE  SPACES        TO  CNIRF-IMOV-RURAL OF RUCAB018
               END-IF

               IF  WRK-B018-CCCIR             EQUAL  -1
                   MOVE  SPACES        TO  CCCIR            OF RUCAB018
               END-IF

               IF  WRK-B018-CREG-AMBTL-RURAL  EQUAL  -1
                   MOVE  SPACES        TO  CREG-AMBTL-RURAL OF RUCAB018
               END-IF

BRQ001         IF  WRK-PPRESV-AMBTL-PPRIE-NULL EQUAL  -1
BRQ001             MOVE  ZEROS         TO PPRESV-AMBTL-PPRIE OF RUCAB018
BRQ001         END-IF
BRQ001
BRQ001         IF  WRK-COUTGA-AGUA-IMOV-NULL   EQUAL  -1
BRQ001             MOVE  SPACES        TO COUTGA-AGUA-IMOV   OF RUCAB018
BRQ001         END-IF
           END-IF.

      *----------------------------------------------------------------*
       5285-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5290-CLOSE-CSR07-RUCAB018       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR07-RUCAB018
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PPRIE_IMOV'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0031'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5290-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5295-VERIFICAR-SEGURO           SECTION.
      *----------------------------------------------------------------*

           MOVE  CJUNC-DEPDC OF RUFIB002
                                       TO CJUNC-DEPDC       OF RUECB09J
           MOVE  CCDULA      OF RUFIB002
                                       TO CCDULA            OF RUECB09J

           EXEC SQL
               SELECT
                      CSIT_REG_RURAL
                 INTO
                     :RUECB09J.CSIT-REG-RURAL
               FROM   DB2PRD.TAPOLC_SEGUR_OBRIG
               WHERE  CJUNC_DEPDC      = :RUECB09J.CJUNC-DEPDC
                 AND  CCDULA           = :RUECB09J.CCDULA
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS AND +100 AND -811)  OR
              ( SQLWARN0            EQUAL  'W'                    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TAPOLC_SEGUR_OBRIG'
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0032'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE SPACES             TO  CSIT-REG-RURAL   OF RUECB09J
           END-IF.

      *----------------------------------------------------------------*
       5295-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5305-FORMATAR-PARC-PGTO         SECTION.
      *----------------------------------------------------------------*

           MOVE  ZEROS                 TO  IND-PARC

           PERFORM  5325-OPEN-CSR08-RUCAB004
           PERFORM  5330-FETCH-CSR08-RUCAB004

           PERFORM
             UNTIL  WRK-FIM-CSR08   EQUAL  'S'

               IF  B004-CODLANC  OF RUCAB004   LESS   23         AND
                   B004-DEBCRED  OF RUCAB004   EQUAL '6'         AND
                   B004-VLPRICOR OF RUCAB004 GREATER  ZEROS      AND
                   B004-CONTROLE OF RUCAB004 (1:1)
                                                LESS  WRK-BYTE-LIGADO
                   PERFORM  5310-VERIFICAR-ALT-PARC-PGTO
               END-IF

               PERFORM  5330-FETCH-CSR08-RUCAB004
           END-PERFORM.

           PERFORM  5335-CLOSE-CSR08-RUCAB004
           PERFORM  5315-CLASSIFICAR-TAB-PARC
           PERFORM  5320-GERAR-NOVA-PARC-PGTO.

      *----------------------------------------------------------------*
       5305-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5310-VERIFICAR-ALT-PARC-PGTO    SECTION.
      *----------------------------------------------------------------*

           PERFORM  5340-OPEN-CSR09-RUCAB017
           PERFORM  5345-FETCH-CSR09-RUCAB017

           IF  WRK-FIM-CSR09        EQUAL  'S'
               IF  IND-PARC          LESS  1000
                   ADD   1             TO  IND-PARC
                   MOVE  B004-DTEVENTO OF  RUCAB004
                                       TO  WRK-DATA-DB2
                   MOVE  WRK-DIA-DB2   TO  WRK-DIA-INV
                   MOVE  WRK-MES-DB2   TO  WRK-MES-INV
                   MOVE  WRK-ANO-DB2   TO  WRK-ANO-INV
                   MOVE  WRK-DATA-INV  TO  TAB-DATA (IND-PARC)
                   MOVE  B004-VLPRICOR OF  RUCAB004
                                       TO  WRK-S13V99-DRESS
                   MOVE  ZEROS         TO  TAB-VALOR(IND-PARC) (01:04)
                   MOVE  WRK-X15-DRESS TO  TAB-VALOR(IND-PARC) (05:15)
               END-IF
           END-IF.

           PERFORM
             UNTIL  WRK-FIM-CSR09   EQUAL  'S'
                OR  IND-PARC      GREATER  999
               ADD  1                  TO  IND-PARC
               MOVE  DVCTO-PCELA-CONTR OF  RUCAB017
                                       TO  WRK-DATA-DB2
               MOVE  WRK-DIA-DB2       TO  WRK-DIA-INV
               MOVE  WRK-MES-DB2       TO  WRK-MES-INV
               MOVE  WRK-ANO-DB2       TO  WRK-ANO-INV
               MOVE  WRK-DATA-INV      TO  TAB-DATA (IND-PARC)
               MOVE  VPCELA            OF  RUCAB017
                                       TO  WRK-S13V99-DRESS
               MOVE  ZEROS             TO  TAB-VALOR(IND-PARC) (01:04)
               MOVE  WRK-X15-DRESS     TO  TAB-VALOR(IND-PARC) (05:15)

               PERFORM  5345-FETCH-CSR09-RUCAB017
           END-PERFORM.

           PERFORM  5350-CLOSE-CSR09-RUCAB017.

      *----------------------------------------------------------------*
       5310-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5315-CLASSIFICAR-TAB-PARC       SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  IND-PARC

             COMPUTE  IND-AUX           =  IND-LIN + 1

             PERFORM
             VARYING  IND-LIN2       FROM  IND-AUX BY 1
               UNTIL  IND-LIN2    GREATER  IND-PARC

                  IF  TAB-DATA (IND-LIN) GREATER TAB-DATA (IND-LIN2)
                      MOVE  TAB-LINHA (IND-LIN)
                                       TO  WRK-REG-AUX
                      MOVE  TAB-LINHA (IND-LIN2)
                                       TO  TAB-LINHA (IND-LIN)
                      MOVE  WRK-REG-AUX
                                       TO  TAB-LINHA (IND-LIN2)
                  END-IF
             END-PERFORM
           END-PERFORM.

      *----------------------------------------------------------------*
       5315-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5320-GERAR-NOVA-PARC-PGTO       SECTION.
      *----------------------------------------------------------------*

PRI005     PERFORM  5322-CONTRATO-LIQUIDADO
PRI005     IF  RUCAA3-S-CONTR-LIQUIDADO
PRI005                                 EQUAL  'SIM'
PRI005         MOVE ALL '*'            TO  RUCA66-GP-COR0001-PARCL
PRI005         PERFORM 5323-OBTER-CRONO-PAGTO
PRI005     END-IF.

           PERFORM
           VARYING  IND-LIN          FROM  1 BY 1
             UNTIL  IND-LIN       GREATER  IND-PARC

              MOVE  IND-LIN            TO  WRK-S11-DRESS
              MOVE  WRK-N03-DRESS      TO  RUCA66-NUM-PARCL
              MOVE  TAB-DATA (IND-LIN) TO  RUCA66-DT-PRVPGTO
              MOVE  TAB-VALOR(IND-LIN) TO  RUCA66-VLR-PRINC-PARCL


              MOVE  RUCA66-COR0001-12  TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-12)
              ADD   LENGTH OF RUCA66-COR0001-12
                                       TO  ACU-TAM
                                           ACU-POSICAO
           END-PERFORM.

      *----------------------------------------------------------------*
       5320-99-FIM.  EXIT.
      *----------------------------------------------------------------*

PRI005*----------------------------------------------------------------*
PRI005 5322-CONTRATO-LIQUIDADO         SECTION.
PRI005*----------------------------------------------------------------*
PRI005* *** SE SALDO DEVEDOR MAIOR QUE ZEROS CONTRATO NAO LIQUIDADO
PRI005     INITIALIZE                      RUCAA3-REGISTRO
PRI005     MOVE  COMU-8810-CHAVE       TO  RUCAA3-E-CONTR
PRI005     CALL  WRK-RUCA8516          USING  RUCAA3-REGISTRO
PRI005                                        ERRO-AREA
PRI005                                        SQLCA.
PRI005     EVALUATE  RUCAA3-COD-RETORNO
PRI005     WHEN  00
PRI005           CONTINUE
PRI005     WHEN  99
PRI005           PERFORM 8000-FINALIZAR
PRI005     WHEN  OTHER
PRI005           MOVE 'APL'            TO  ERR-TIPO-ACESSO
PRI005           MOVE  RUCAA3-MSG-RETORNO(1:75)
PRI005                                 TO ERR-TEXTO
PRI005           PERFORM 8000-FINALIZAR
PRI005     END-EVALUATE.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5322-99-FIM.                    EXIT.
PRI005*----------------------------------------------------------------*

PRI005*----------------------------------------------------------------*
PRI005 5323-OBTER-CRONO-PAGTO          SECTION.
PRI005*----------------------------------------------------------------*
PRI005
PRI005     MOVE ZEROS                  TO WRK-IND-LIN.
PRI005     PERFORM  5336-OPEN-CSR11-RUCAB004.
PRI005     PERFORM  5337-FETCH-CSR11-RUCAB004.
PRI005
PRI005     PERFORM
PRI005       UNTIL  WRK-FIM-CSR11      EQUAL  'S'
PRI005
PRI005         MOVE B004-CONTROLE      TO  WRK-CONTROLE
PRI005         MOVE 1                  TO  WRK-CONVERSAO
PRI005                                     WRK-BIT-BYTE
PRI005
PRI005         CALL 'POOL0004'         USING  WRK-CONVERSAO
PRI005                                        WRK-CONTROLE(1:1)
PRI005                                        WRK-CONVERTIDO
PRI005                                        WRK-BIT-BYTE
PRI005
PRI005         IF  RETURN-CODE         NOT EQUAL ZEROS
PRI005             MOVE   'APL'        TO  ERR-TIPO-ACESSO
PRI005             MOVE   RETURN-CODE  TO  WRK-RETURN-CODE-PL04
PRI005             MOVE   '** 04 **'   TO  WRK-LOCAL-PL04
PRI005             MOVE   WRK-MSG-ERRO-PL04
PRI005                                 TO  ERR-TEXTO
PRI005             PERFORM 8000-FINALIZAR
PRI005         END-IF
PRI005
PRI005         IF  WRK-BIT2   NOT EQUAL     '1'
PRI005             ADD 1               TO WRK-IND-LIN
PRI005             MOVE  WRK-IND-LIN   TO WRK-S11-DRESS
PRI005             MOVE  WRK-N03-DRESS TO RUCA66-NUM-PARCL
PRI005             MOVE  B004-DTEVENTO OF RUCAB004
PRI005                                 TO WRK-DATA-DB2
PRI005             MOVE  WRK-DIA-DB2   TO WRK-DIA-INV
PRI005             MOVE  WRK-MES-DB2   TO WRK-MES-INV
PRI005             MOVE  WRK-ANO-DB2   TO WRK-ANO-INV
PRI005             MOVE  WRK-DATA-INV  TO RUCA66-DT-PRVPGTO
PRI005             MOVE  B004-VLPRIORI OF RUCAB004
PRI005                                 TO WRK-S13V99-DRESS
PRI005             MOVE  ZEROS         TO RUCA66-VLR-PRINC-PARCL(01:04)
PRI005             MOVE  WRK-X15-DRESS TO RUCA66-VLR-PRINC-PARCL(05:15)
PRI005
PRI005             MOVE  RUCA66-COR0001-12
PRI005                                 TO WRK-MSGEM-RURAL(ACU-POSICAO:
PRI005                                 LENGTH OF RUCA66-COR0001-12)
PRI005             ADD   LENGTH OF RUCA66-COR0001-12
PRI005                                 TO ACU-TAM
PRI005                                    ACU-POSICAO
PRI005         END-IF
PRI005
PRI005         PERFORM  5337-FETCH-CSR11-RUCAB004
PRI005     END-PERFORM.
PRI005
PRI005     PERFORM  5338-CLOSE-CSR11-RUCAB004.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5323-99-FIM.                    EXIT.
PRI005*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5325-OPEN-CSR08-RUCAB004        SECTION.
      *----------------------------------------------------------------*

           MOVE  COMU-8810-CHAVE       TO  B004-CH-CONTR    OF RUCAB004
           MOVE  ZEROS                 TO  B004-CH-TIPO     OF RUCAB004
           MOVE  1                     TO  B004-CH-NUMERO   OF RUCAB004
           MOVE  'N'                   TO  WRK-FIM-CSR08

           EXEC SQL
                OPEN CSR08-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0033'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5325-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5330-FETCH-CSR08-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR08-RUCAB004
                INTO   :RUCAB004.B004-CH-CONTR     ,
                       :RUCAB004.B004-CH-TIPO      ,
                       :RUCAB004.B004-CH-NUMERO    ,
                       :RUCAB004.B004-CONTROLE     ,
                       :RUCAB004.B004-DTEVENTO
                          :WRK-B004-DTEVENTO       ,
                       :RUCAB004.B004-CODLANC      ,
                       :RUCAB004.B004-DEBCRED      ,
                       :RUCAB004.B004-VLPRICOR
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0034'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE               EQUAL  ZEROS
               IF  WRK-B004-DTEVENTO EQUAL  -1
                   MOVE  ZEROS         TO  B004-DTEVENTO    OF RUCAB004
               END-IF
           ELSE
               MOVE  ZEROS             TO  B004-CH-NUMERO   OF RUCAB004
                                           B004-DTEVENTO    OF RUCAB004
                                           B004-CODLANC     OF RUCAB004
                                           B004-VLPRICOR    OF RUCAB004
               MOVE  'S'               TO  WRK-FIM-CSR08
           END-IF.

      *----------------------------------------------------------------*
       5330-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5335-CLOSE-CSR08-RUCAB004       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR08-RUCAB004
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0035'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5335-99-FIM.  EXIT.
      *----------------------------------------------------------------*

PRI005*----------------------------------------------------------------*
PRI005 5336-OPEN-CSR11-RUCAB004        SECTION.
PRI005*----------------------------------------------------------------*
PRI005
PRI005     MOVE  COMU-8810-CHAVE       TO  B004-CH-CONTR    OF RUCAB004
PRI005     MOVE  'N'                   TO  WRK-FIM-CSR11
PRI005
PRI005     EXEC SQL
PRI005          OPEN CSR11-RUCAB004
PRI005     END-EXEC.
PRI005
PRI005     IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
PRI005        ( SQLWARN0            EQUAL  'W'    )
PRI005         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PRI005         MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
PRI005         MOVE 'OPEN'             TO  ERR-FUN-COMANDO
PRI005         MOVE  SQLCODE           TO  ERR-SQL-CODE
PRI005         MOVE '0046'             TO  ERR-LOCAL
PRI005         MOVE  SPACES            TO  ERR-SEGM
PRI005         MOVE  99                TO  COMU-8810-COD-RETORNO
PRI005         MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
PRI005         MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
PRI005         MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
PRI005         PERFORM  8000-FINALIZAR
PRI005     END-IF.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5336-99-FIM.  EXIT.
PRI005*----------------------------------------------------------------*
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5337-FETCH-CSR11-RUCAB004       SECTION.
PRI005*----------------------------------------------------------------*
PRI005
PRI005     EXEC SQL
PRI005          FETCH  CSR11-RUCAB004
PRI005          INTO   :RUCAB004.B004-CH-CONTR     ,
PRI005                 :RUCAB004.B004-CH-TIPO      ,
PRI005                 :RUCAB004.B004-CH-NUMERO    ,
PRI005                 :RUCAB004.B004-CONTROLE     ,
PRI005                 :RUCAB004.B004-DTEVENTO
PRI005                    :WRK-B004-DTEVENTO       ,
PRI005                 :RUCAB004.B004-CODLANC      ,
PRI005                 :RUCAB004.B004-DEBCRED      ,
PRI005                 :RUCAB004.B004-VLPRIORI     ,
PRI005                 :RUCAB004.B004-VLPRICOR
PRI005     END-EXEC.
PRI005
PRI005     IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
PRI005        ( SQLWARN0            EQUAL  'W'             )
PRI005         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PRI005         MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
PRI005         MOVE 'FETCH'            TO  ERR-FUN-COMANDO
PRI005         MOVE  SQLCODE           TO  ERR-SQL-CODE
PRI005         MOVE '0047'             TO  ERR-LOCAL
PRI005         MOVE  SPACES            TO  ERR-SEGM
PRI005         MOVE  99                TO  COMU-8810-COD-RETORNO
PRI005         MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
PRI005         MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
PRI005         MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
PRI005         PERFORM  8000-FINALIZAR
PRI005     END-IF.
PRI005
PRI005     IF  SQLCODE               EQUAL  ZEROS
PRI005         IF  WRK-B004-DTEVENTO EQUAL  -1
PRI005             MOVE  ZEROS         TO  B004-DTEVENTO    OF RUCAB004
PRI005         END-IF
PRI005     ELSE
PRI005         MOVE  ZEROS             TO  B004-CH-NUMERO   OF RUCAB004
PRI005                                     B004-DTEVENTO    OF RUCAB004
PRI005                                     B004-CODLANC     OF RUCAB004
PRI005                                     B004-VLPRIORI    OF RUCAB004
PRI005                                     B004-VLPRICOR    OF RUCAB004
PRI005         MOVE  'S'               TO  WRK-FIM-CSR11
PRI005     END-IF.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5337-99-FIM.  EXIT.
PRI005*----------------------------------------------------------------*
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5338-CLOSE-CSR11-RUCAB004       SECTION.
PRI005*----------------------------------------------------------------*
PRI005
PRI005     EXEC SQL
PRI005          CLOSE CSR11-RUCAB004
PRI005     END-EXEC.
PRI005
PRI005     IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
PRI005        ( SQLWARN0            EQUAL  'W'    )
PRI005         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
PRI005         MOVE 'RUCA_CAD_DEP'     TO  ERR-DBD-TAB
PRI005         MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
PRI005         MOVE  SQLCODE           TO  ERR-SQL-CODE
PRI005         MOVE '0048'             TO  ERR-LOCAL
PRI005         MOVE  SPACES            TO  ERR-SEGM
PRI005         MOVE  99                TO  COMU-8810-COD-RETORNO
PRI005         MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
PRI005         MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
PRI005         MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
PRI005         PERFORM  8000-FINALIZAR
PRI005     END-IF.
PRI005
PRI005*----------------------------------------------------------------*
PRI005 5338-99-FIM.  EXIT.
PRI005*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5340-OPEN-CSR09-RUCAB017        SECTION.
      *----------------------------------------------------------------*

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB017
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB017
           MOVE  6                     TO  CTPO-PCELA       OF RUCAB017
           MOVE  B004-CH-NUMERO        OF  RUCAB004
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  NPCELA-CONTR     OF RUCAB017
           MOVE  ZEROS                 TO  NATULZ-PCELA     OF RUCAB017
           MOVE  'N'                   TO  WRK-FIM-CSR09

           EXEC SQL
                OPEN  CSR09-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0036'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5340-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5345-FETCH-CSR09-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR09-RUCAB017
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
               MOVE '0037'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  'S'               TO  WRK-FIM-CSR09
           END-IF.

      *----------------------------------------------------------------*
       5345-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5350-CLOSE-CSR09-RUCAB017       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR09-RUCAB017
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TMANUT_PCELA_RECOR'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0038'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5350-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5355-FORMATAR-COD-SUBPROGRAMA   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      RUECW824-REGISTRO

           MOVE  CORIGE-REC            OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUECW824-E-CORIGE-REC
           MOVE  CFNALD-RURAL          OF  RUFIB002
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUECW824-E-CFNALD-RURAL
           MOVE  CPRODT-RURAL          OF  RUCAB015
                                       TO  WRK-S11-DRESS
           MOVE  WRK-N03-DRESS         TO  RUECW824-E-CPRODT-RURAL
           MOVE  SPACES                TO  RUECW824-E-RESERVA

           MOVE 'RUEC8824'             TO  WRK-MODULO.

           CALL  WRK-MODULO        USING   RUECW824-COPYBOOK
                                           ERRO-AREA
                                           SQLCA.

           IF  RUECW824-COD-RETORNO    NOT EQUAL ZEROS
               MOVE  04                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-04        TO  COMU-8810-MENSAGEM
               PERFORM  8000-FINALIZAR
           END-IF.

           MOVE RUECW824-S-SUB-PROG-RURAL
                                       TO  RUCA66-CODSUB-PROG.

      *----------------------------------------------------------------*
       5355-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5360-FORMATAR-PLANTIO-COLHEITA  SECTION.
      *----------------------------------------------------------------*

           PERFORM  5365-SELECT-RURCB003

           IF ( CFNALD-RURAL OF RUFIB002     EQUAL 4       )  OR
              ( CATVDD-RURAL OF RUFIB002 NOT EQUAL 1 AND 5 )  OR
              ( CFNALD-RECOR OF RURCB003 NOT EQUAL '00002' )
               MOVE  ALL '*'           TO  RUCA66-DT-INI-PLANT
               MOVE  ALL '*'           TO  RUCA66-DT-FIM-PLANT
               MOVE  ALL '*'           TO  RUCA66-DT-INI-COLHT
               MOVE  ALL '*'           TO  RUCA66-DT-FIM-COLHT
           ELSE
               PERFORM  5370-OBTER-PLANTIO-COLHEITA
           END-IF.

      *----------------------------------------------------------------*
       5360-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5365-SELECT-RURCB003            SECTION.
      *----------------------------------------------------------------*

           MOVE  CFNALD-RURAL OF RUFIB002
                                       TO  CFNALD-RURAL     OF RURCB003

           EXEC SQL
                SELECT
                        CFNALD_RECOR
                  INTO
                        :RURCB003.CFNALD-RECOR
                  FROM  DB2PRD.FNALD_RURAL
                 WHERE  CFNALD_RURAL = :RURCB003.CFNALD-RURAL

           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'FNALD_RURAL'      TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0039'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  +100
               MOVE  SPACES            TO  CFNALD-RECOR     OF RURCB003
           END-IF.

      *----------------------------------------------------------------*
       5365-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5370-OBTER-PLANTIO-COLHEITA     SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-ACHOU-PLANTIO
                                           WRK-ACHOU-COLHEITA

           PERFORM  5380-OPEN-CSR10-RUECB025
           PERFORM  5385-FETCH-CSR10-RUECB025

           PERFORM
             UNTIL ( WRK-ACHOU-PLANTIO   EQUAL  'S'
               AND   WRK-ACHOU-COLHEITA  EQUAL  'S' )
                OR ( WRK-FIM-CSR10       EQUAL  'S' )

             PERFORM  5375-PROCURAR-PLANTIO-COLHEITA
             PERFORM  5385-FETCH-CSR10-RUECB025

           END-PERFORM.

           PERFORM  5390-CLOSE-CSR10-RUECB025
           PERFORM  5395-CHECAR-PROD-PERENE-PLANT

           IF  WRK-ACHOU-PLANTIO   EQUAL  'S'
               IF  WRK-DATA-INI-PLANTIO  EQUAL  ZEROS
                   MOVE  ALL '*'       TO  RUCA66-DT-INI-PLANT
                                           RUCA66-DT-FIM-PLANT
               ELSE
                   MOVE  WRK-DATA-INI-PLANTIO
                                       TO  RUCA66-DT-INI-PLANT

                   IF  WRK-DATA-FIM-PLANTIO  EQUAL  ZEROS
                       MOVE  ALL '*'   TO  RUCA66-DT-FIM-PLANT
                   ELSE
                       MOVE  WRK-DATA-FIM-PLANTIO
                                       TO  RUCA66-DT-FIM-PLANT
                   END-IF
               END-IF
           ELSE
               MOVE  ALL '*'           TO  RUCA66-DT-INI-PLANT
                                           RUCA66-DT-FIM-PLANT
           END-IF.

           IF  WRK-ACHOU-COLHEITA   EQUAL  'S'
               IF  WRK-DATA-INI-COLHEITA  EQUAL  ZEROS
                   MOVE  ALL '*'       TO  RUCA66-DT-INI-COLHT
                                           RUCA66-DT-FIM-COLHT
               ELSE
                   MOVE  WRK-DATA-INI-COLHEITA
                                       TO  RUCA66-DT-INI-COLHT

                   IF  WRK-DATA-FIM-COLHEITA  EQUAL  ZEROS
                       MOVE  ALL '*'   TO  RUCA66-DT-FIM-COLHT
                   ELSE
                       MOVE  WRK-DATA-FIM-COLHEITA
                                       TO  RUCA66-DT-FIM-COLHT
                   END-IF
               END-IF
           ELSE
               MOVE  ALL '*'           TO  RUCA66-DT-INI-COLHT
                                           RUCA66-DT-FIM-COLHT
           END-IF.

      *----------------------------------------------------------------*
       5370-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5375-PROCURAR-PLANTIO-COLHEITA  SECTION.
      *----------------------------------------------------------------*

           PERFORM
           VARYING  IND-CHAR            FROM  1  BY 1
             UNTIL  IND-CHAR         GREATER  34
                OR  WRK-ACHOU-PLANTIO  EQUAL  'S'

                IF  RTPO-OPER-ORCAM OF RUECB025 (IND-CHAR:07)
                                       EQUAL 'PLANTIO'
                    IF  DLIBRC-OPER-ORCAM OF RUECB025  EQUAL  SPACES
                        MOVE  ZEROS    TO  WRK-DATA-INI-PLANTIO
                    ELSE
                        MOVE  DLIBRC-OPER-ORCAM OF RUECB025
                                       TO  WRK-DATA-DB2
                        MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-INI-PLANTIO
                        MOVE  WRK-MES-DB2
                                       TO  WRK-MES-INI-PLANTIO
                        MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-INI-PLANTIO
                    END-IF

                    IF  DFIM-EVNTO-CDULA OF RUECB025  EQUAL  SPACES
                        MOVE  ZEROS    TO  WRK-DATA-FIM-PLANTIO
                    ELSE
                        MOVE  DFIM-EVNTO-CDULA  OF RUECB025
                                       TO  WRK-DATA-DB2
                        MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-FIM-PLANTIO
                        MOVE  WRK-MES-DB2
                                       TO  WRK-MES-FIM-PLANTIO
                        MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-FIM-PLANTIO
                    END-IF

                    MOVE  'S'          TO  WRK-ACHOU-PLANTIO
                END-IF
           END-PERFORM.

           PERFORM
           VARYING  IND-CHAR            FROM  1  BY 1
             UNTIL  IND-CHAR         GREATER  33
                OR  WRK-ACHOU-COLHEITA EQUAL  'S'

                IF  RTPO-OPER-ORCAM OF RUECB025 (IND-CHAR:08)
                                       EQUAL 'COLHEITA'
                    IF  DLIBRC-OPER-ORCAM OF RUECB025  EQUAL  SPACES
                        MOVE  ZEROS    TO  WRK-DATA-INI-COLHEITA
                    ELSE
                        MOVE  DLIBRC-OPER-ORCAM OF RUECB025
                                       TO  WRK-DATA-DB2
                        MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-INI-COLHEITA
                        MOVE  WRK-MES-DB2
                                       TO  WRK-MES-INI-COLHEITA
                        MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-INI-COLHEITA
                    END-IF

                    IF  DFIM-EVNTO-CDULA OF RUECB025  EQUAL  SPACES
                        MOVE  ZEROS    TO  WRK-DATA-FIM-COLHEITA
                    ELSE
                        MOVE  DFIM-EVNTO-CDULA  OF RUECB025
                                       TO  WRK-DATA-DB2
                        MOVE  WRK-DIA-DB2
                                       TO  WRK-DIA-FIM-COLHEITA
                        MOVE  WRK-MES-DB2
                                       TO  WRK-MES-FIM-COLHEITA
                        MOVE  WRK-ANO-DB2
                                       TO  WRK-ANO-FIM-COLHEITA
                    END-IF

                    MOVE  'S'          TO  WRK-ACHOU-COLHEITA
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       5375-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5380-OPEN-CSR10-RUECB025        SECTION.
      *----------------------------------------------------------------*

           MOVE  CJUNC-DEPDC OF RUFIB002
                                       TO  CJUNC-DEPDC  OF RUECB025
           MOVE  CCDULA      OF RUFIB002
                                       TO  CCDULA       OF RUECB025
           MOVE  ZEROS                 TO  CPCELA-LIBRC OF RUECB025
           MOVE  'N'                   TO  WRK-FIM-CSR10

           EXEC SQL
                OPEN CSR10-RUECB025
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0040'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5380-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5385-FETCH-CSR10-RUECB025       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR10-RUECB025
                 INTO  :RUECB025.DLIBRC-OPER-ORCAM
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
               MOVE '0041'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
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
               MOVE  'S'               TO  WRK-FIM-CSR10
               MOVE  SPACES            TO  DLIBRC-OPER-ORCAM OF RUECB025
                                           RTPO-OPER-ORCAM   OF RUECB025
                                           DFIM-EVNTO-CDULA  OF RUECB025
           END-IF.

      *----------------------------------------------------------------*
       5385-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5390-CLOSE-CSR10-RUECB025       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR10-RUECB025
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'ORCAM_CDULA_RURAL'
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '0042'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5390-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5395-CHECAR-PROD-PERENE-PLANT   SECTION.
      *----------------------------------------------------------------*

           MOVE  CPRODT-RURAL OF RUCAB015
                                       TO  CPRODT-RURAL     OF RURCB006

           PERFORM  5240-SELECT-RURCB006


           IF  CTEMPO-VIDA-CULTR OF RURCB006   EQUAL  ZEROS
               IF ( DINIC-PLATI OF RUCAB015 EQUAL  '01.01.0001' ) OR
                  ( WRK-B015-DINIC-PLATI    EQUAL  -1             AND
                    WRK-ACHOU-PLANTIO       EQUAL  'N'          )
                    MOVE  12           TO  COMU-8810-COD-RETORNO
                    MOVE  WRK-MSG-12   TO  COMU-8810-MENSAGEM
                    PERFORM  8000-FINALIZAR
               END-IF

               IF ( DINIC-COLHE OF RUCAB015 EQUAL '01.01.0001' ) OR
                  ( WRK-B015-DINIC-COLHE    EQUAL  -1            AND
                    WRK-ACHOU-COLHEITA      EQUAL  'N'         )
                    MOVE  12           TO  COMU-8810-COD-RETORNO
                    MOVE  WRK-MSG-12   TO  COMU-8810-MENSAGEM
                    PERFORM  8000-FINALIZAR
               END-IF
           ELSE
               IF ( DINIC-PLATI OF RUCAB015 EQUAL  '01.01.0001' ) OR
                  ( WRK-B015-DINIC-PLATI    EQUAL  -1             AND
                    WRK-ACHOU-PLANTIO       EQUAL  'N'          )
                    MOVE  13           TO  COMU-8810-COD-RETORNO
MJ0824              MOVE  WRK-MSG-13   TO  COMU-8810-MENSAGEM
                    PERFORM  8000-FINALIZAR
               END-IF

               IF ( DINIC-COLHE OF RUCAB015 EQUAL '01.01.0001' ) OR
                  ( WRK-B015-DINIC-COLHE    EQUAL  -1            AND
                    WRK-ACHOU-COLHEITA      EQUAL  'N'         )
                    MOVE  14           TO  COMU-8810-COD-RETORNO
                    MOVE  WRK-MSG-14   TO  COMU-8810-MENSAGEM
                    PERFORM  8000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       5395-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5400-SALVAR-MENSAGEM            SECTION.
      *----------------------------------------------------------------*

           PERFORM  5055-OBTER-DATA-HORA

           IF  ACU-NENVIO-RECOR     EQUAL  ZEROS
               PERFORM  5405-INSERT-RUCAB012
           END-IF.

           PERFORM  5410-INSERT-RUCAB019
           PERFORM  5415-INSERT-RUCAB013.

      *----------------------------------------------------------------*
       5400-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5405-INSERT-RUCAB012            SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO  ACU-NENVIO-RECOR

           MOVE  CCONTR    OF RUCAB015 TO  CCONTR           OF RUCAB012
           MOVE  HINCL-REG OF RUCAB015 TO  HINCL-REG        OF RUCAB012
           MOVE  ACU-NENVIO-RECOR      TO  WRK-NENVIO-RECOR
           MOVE  WRK-NENVIO-RECOR      TO  NENVIO-RECOR     OF RUCAB012
           MOVE  SPACES                TO  CCTRL-RQUIS-FINCR
                                                            OF RUCAB012
           MOVE  -1                    TO  WRK-B012-CCTRL-RQUIS-FINCR
           MOVE  SPACES                TO  CUNIC-MSGEM      OF RUCAB012
           MOVE  -1                    TO  WRK-B012-CUNIC-MSGEM

           IF  CREFT-RECOR OF RUFIB002 IS       NUMERIC  AND
               CREFT-RECOR OF RUFIB002 GREATER  ZEROS
               MOVE  CREFT-RECOR OF RUFIB002
                                       TO  CREFT-RECOR      OF RUCAB012
           ELSE
               MOVE  CREFT-BACEN OF RUFIB002
                                       TO  WRK-S11-DRESS
               MOVE  WRK-X11-DRESS     TO  CREFT-RECOR      OF RUCAB012
           END-IF.

           MOVE  ZEROS                 TO  WRK-B012-CREFT-RECOR

           MOVE  WRK-TIMESTAMP         TO  HINCL            OF RUCAB012
           MOVE  COMU-8810-USUARIO     TO  CFUNC-BDSCO      OF RUCAB012

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
               MOVE '0043'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5405-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5410-INSERT-RUCAB019            SECTION.
      *----------------------------------------------------------------*

           ADD   1                     TO  ACU-NREG-MSGEM-RECOR

           MOVE  CCONTR    OF RUCAB012 TO  CCONTR           OF RUCAB019
           MOVE  HINCL-REG OF RUCAB012 TO  HINCL-REG        OF RUCAB019
           MOVE  NENVIO-RECOR          OF  RUCAB012
                                       TO  NENVIO-RECOR     OF RUCAB019
           MOVE  ACU-NREG-MSGEM-RECOR  TO  WRK-NREG-MSGEM-RECOR
           MOVE  WRK-NREG-MSGEM-RECOR  TO  NREG-MSGEM-RECOR OF RUCAB019
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
               MOVE '0044'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5410-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5415-INSERT-RUCAB013            SECTION.
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
               MOVE '0045'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       5415-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5420-TRATAR-CONTINUACAO         SECTION.
      *----------------------------------------------------------------*

           PERFORM  5425-FORMATAR-MSG-CONT
           PERFORM  5410-INSERT-RUCAB019
           PERFORM  5415-INSERT-RUCAB013.

      *----------------------------------------------------------------*
       5420-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5425-FORMATAR-MSG-CONT          SECTION.
      *----------------------------------------------------------------*
 
           MOVE 1                      TO  ACU-POSICAO
           MOVE ZEROS                  TO  ACU-TAM
           MOVE SPACES                 TO  WRK-MSGEM-RURAL

           MOVE  RUCA66-COR0001-1      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-1)
           ADD   LENGTH OF RUCA66-COR0001-1
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-GP-COR0001-EMIT
           MOVE  RUCA66-COR0001-2      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-2)
           ADD   LENGTH OF RUCA66-COR0001-2
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-3      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-3)
           ADD   LENGTH OF RUCA66-COR0001-3
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-GP-COR0001-RENEGC
           MOVE  RUCA66-COR0001-4      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-4)
           ADD   LENGTH OF RUCA66-COR0001-4
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

7C2510*   -MOVE  ALL '*'               TO  RUCA66-CODPROG-LINHA-CRED
7C2510*   -MOVE  ALL '*'               TO  RUCA66-TP-FNTE-REC
           MOVE  RUCA66-COR0001-5      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-5)
           ADD   LENGTH OF RUCA66-COR0001-5
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*
7C2510     MOVE  RUCA66-COR0001-6      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510                                     LENGTH OF RUCA66-COR0001-6)
7C2510     ADD LENGTH OF RUCA66-COR0001-6
7C2510                                 TO  ACU-TAM
7C2510                                     ACU-POSICAO

           PERFORM  5135-PROCURAR-GLEBA

           IF  RUCA66-IDENTC-GLEBA(1:1)  EQUAL  '*'
ONEDA          MOVE  ALL '*'           TO RUCA66-AREA-X
           ELSE
              MOVE '******'            TO  WRK-MSGEM-RURAL
                                                       (ACU-NEW-POS: 6)
           END-IF.

      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-8      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-8)
           ADD   LENGTH OF RUCA66-COR0001-8
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-9      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-9)
           ADD   LENGTH OF RUCA66-COR0001-9
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-GP-COR0001-PROP-F
           MOVE  RUCA66-COR0001-10-F   TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-10-F)
           ADD   LENGTH OF RUCA66-COR0001-10-F
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-11      TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                          LENGTH OF RUCA66-COR0001-11)
           ADD   LENGTH OF RUCA66-COR0001-11
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-GP-COR0001-PARCL
           MOVE  RUCA66-COR0001-12     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-12)
           ADD   LENGTH OF RUCA66-COR0001-12
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  RUCA66-COR0001-15     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-15)
           ADD   LENGTH OF RUCA66-COR0001-15
                                       TO  ACU-TAM
                                           ACU-POSICAO

      *----------------------------------------------------------------*

           MOVE  ALL '*'               TO  RUCA66-GP-COR0001-DESCLASS
                                       OF  RUCA66-GP-COR0001-DESCLASS
           MOVE  RUCA66-COR0001-16     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
                                           LENGTH OF RUCA66-COR0001-16)
           ADD   LENGTH OF RUCA66-COR0001-16
                                       TO  ACU-TAM
                                           ACU-POSICAO.

BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE  RUCA66-COR0001-17     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-17)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-17
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE  RUCA66-COR0001-18     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-18)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-18
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE  RUCA66-COR0001-19     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-19)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-19
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE  RUCA66-COR0001-20     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
BRQ001                                     LENGTH OF RUCA66-COR0001-20)
BRQ001     ADD   LENGTH OF RUCA66-COR0001-20
BRQ001                                 TO  ACU-TAM
BRQ001                                     ACU-POSICAO.
BRQ001
      *----------------------------------------------------------------*

7C2512     MOVE  RUCA66-COR0001-21     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2512                                     LENGTH OF RUCA66-COR0001-21)
7C2512     ADD   LENGTH OF RUCA66-COR0001-21
7C2512                                 TO  ACU-TAM
7C2512                                     ACU-POSICAO.
7C2512
7C2512     MOVE  RUCA66-COR0001-21A    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2512                                    LENGTH OF RUCA66-COR0001-21A)
7C2512     ADD   LENGTH OF RUCA66-COR0001-21A
7C2512                                 TO  ACU-TAM
7C2512                                     ACU-POSICAO.
7C2512
7C2512     MOVE  ALL '*'               TO  RUCA66-GP-COR0001-CONFDD
7C2512     MOVE  RUCA66-COR0001-21B    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2512                                    LENGTH OF RUCA66-COR0001-21B)
7C2512     ADD   LENGTH OF RUCA66-COR0001-21B
7C2512                                 TO  ACU-TAM
7C2512                                     ACU-POSICAO.
7C2512
7C2512     MOVE  RUCA66-COR0001-21C    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2512                                    LENGTH OF RUCA66-COR0001-21C)
7C2512     ADD   LENGTH OF RUCA66-COR0001-21C
7C2512                                 TO  ACU-TAM
7C2512                                     ACU-POSICAO.
7C2512
7C2512     MOVE  RUCA66-COR0001-21D    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2512                                    LENGTH OF RUCA66-COR0001-21D)
7C2512     ADD   LENGTH OF RUCA66-COR0001-21D
7C2512                                 TO  ACU-TAM
7C2512                                     ACU-POSICAO.

      *----------------------------------------------------------------*

WP1021     MOVE  RUCA66-COR0001-22     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
WP1021                                     LENGTH OF RUCA66-COR0001-22)
WP1021     ADD   LENGTH OF RUCA66-COR0001-22
                                       TO  ACU-TAM
                                           ACU-POSICAO.

      *----------------------------------------------------------------*
       5425-99-FIM.  EXIT.
      *----------------------------------------------------------------*
BVF001
      *----------------------------------------------------------------*
       5500-PROCURAR-CONTR-MANUT       SECTION.
      *----------------------------------------------------------------*

           MOVE  'N'                   TO  WRK-TEM-MANUTENCAO

           PERFORM  5510-OPEN-CSR01-RUCAB014
           PERFORM  5520-FETCH-CSR01-RUCAB014

           PERFORM
             UNTIL  SQLCODE             EQUAL  +100
                OR  WRK-TEM-MANUTENCAO  EQUAL  'S'

                PERFORM  5520-FETCH-CSR01-RUCAB014
           END-PERFORM.

           PERFORM  5530-CLOSE-CSR01-RUCAB014
      *
           IF WRK-TEM-MANUTENCAO NOT EQUAL 'S'
              MOVE ZEROS               TO WRK-REDUC-NUM
           END-IF.

      *----------------------------------------------------------------*
       5500-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5510-OPEN-CSR01-RUCAB014        SECTION.
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
              MOVE  'N'                TO  WRK-TEM-MANUTENCAO
           END-IF.

      *----------------------------------------------------------------*
       5510-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5520-FETCH-CSR01-RUCAB014       SECTION.
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
              MOVE  'N'                TO  WRK-TEM-MANUTENCAO
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
       5520-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5530-CLOSE-CSR01-RUCAB014       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-RUCAB014
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
              MOVE  'N'                TO  WRK-TEM-MANUTENCAO
           END-IF.

      *----------------------------------------------------------------*
       5530-99-FIM.  EXIT.
      *----------------------------------------------------------------*
BVF001
STEFAB*----------------------------------------------------------------*
.      6140-OBTER-PONTOS-GEO           SECTION.
.     *----------------------------------------------------------------*
.
.     **** ENVIAR PRIMEIRO A AREA DO NAO CULTIVADO
.
.          IF  WRK-FIM-CSR14        EQUAL  'S'
.              PERFORM  6190-OPEN-CSR14-RUCAB016
.              PERFORM  6195-FETCH-CSR14-RUCAB016
.          END-IF.
.
.          MOVE  'N'                   TO  WRK-TEM-GLEBA
.
.          IF  WRK-FIM-CSR14        EQUAL  'N'
.              PERFORM  6145-OBTER-COORD-COM-ALT
.          ELSE
.              PERFORM  6155-OBTER-COORD-SEM-ALT
.          END-IF.
.
.          IF  WRK-FIM-CSR14           EQUAL  'S'
.              PERFORM  6200-CLOSE-CSR14-RUCAB016
7C2511*       XIF  ACU-QTD-GLEBA-E     EQUAL  ZEROS
7C2511         IF  ACU-QTD-GLEBA       EQUAL  ZEROS
STEFA2             IF WRK-GRAVOU       EQUAL 'N'
.                     PERFORM  6170-TRATAR-SEM-GLEBA
STEFA2             END-IF
STEFAB         END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6140-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6145-OBTER-COORD-COM-ALT        SECTION.
.     *----------------------------------------------------------------*
.
7C2510*   XMOVE  RUCA66-COR0001-7B     TO
7C2510*                                WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                LENGTH OF RUCA66-COR0001-7B)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7B
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
.
.          MOVE 'S'                    TO WRK-GLEBA-7B
.
.          PERFORM
.            UNTIL  WRK-FIM-CSR14   EQUAL  'S'
7C2511*         OR  ACU-QTD-GLEBA-E EQUAL  90
7C2511          OR  ACU-QTD-GLEBA   EQUAL  350
.
STEFA1          IF  WRK-TAB16-CSEQ-MATR-RURAL GREATER ZEROS
.                   PERFORM  6150-FORMATAR-COORD-ALT
.
7C2511*            XIF  ACU-QTD-GLEBA-E LESS  90
7C2511              IF  ACU-QTD-GLEBA   LESS  350
.                       PERFORM  6195-FETCH-CSR14-RUCAB016
.                   END-IF
.               ELSE
.                   PERFORM  6195-FETCH-CSR14-RUCAB016
.               END-IF
.          END-PERFORM.
.
STEFAB     IF  WRK-FIM-CSR14           EQUAL 'S'
.              PERFORM  5165-TRATAR-AREA-NAO-CULT
.              MOVE 'S'                TO WRK-FIM-CSR14
.              MOVE 'S'                TO WRK-FIM-CSR15
7C2511*       XPERFORM  6140-OBTER-PONTOS-GEO
STEFAB     END-IF.
.     *----------------------------------------------------------------*
.      6145-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6150-FORMATAR-COORD-ALT         SECTION.
.     *----------------------------------------------------------------*
.
           ADD 1                       TO WRK-QTD-NCULTVD
           MOVE WRK-TAB16-MLATTD-IMOV-RURAL
                                       TO  WRK-MLATTD-IMOV-RURAL-N
7C2510     IF WRK-MLATTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-NCULTVD-LAT
7C2510                                    (WRK-QTD-NCULTVD)
7C2510        MOVE '-'                 TO  WRK-NCULTVD-LAT
7C2510                                    (WRK-QTD-NCULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLATTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO  WRK-NCULTVD-LAT
7C2510                                    (WRK-QTD-NCULTVD)
7C2510     END-IF
7C2510     INSPECT WRK-NCULTVD-LAT(WRK-QTD-NCULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

.          MOVE WRK-TAB16-MLOGTD-IMOV-RURAL
.                                      TO  WRK-MLOGTD-IMOV-RURAL-N

7C2510     IF WRK-MLOGTD-IMOV-RURAL-N  LESS ZEROS
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO WRK-NCULTVD-LON
7C2510                                    (WRK-QTD-NCULTVD)
7C2510        MOVE '-'                 TO WRK-NCULTVD-LON
7C2510                                    (WRK-QTD-NCULTVD)(1:1)
7C2510     ELSE
7C2510        MOVE WRK-MLOGTD-IMOV-9V7 TO WRK-MASK-COORD
7C2510        MOVE WRK-MASK-COORD-X    TO WRK-NCULTVD-LON
7C2510                                    (WRK-QTD-NCULTVD)
7C2510     END-IF
7C2510     INSPECT WRK-NCULTVD-LON(WRK-QTD-NCULTVD)
7C2510                                 REPLACING ALL ','
7C2510                                 BY '.'

7C2510*    M0VE WRK-TAB16-MALTTD-IMOV-RURAL
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*    M0VE WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-ALTT-PONTO-EXCL
.
7C2510*   XMOVE  RUCA66-COR0001-7BB    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                LENGTH OF RUCA66-COR0001-7BB)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7BB
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO.
.          MOVE  'S'                   TO  WRK-TEM-GLEBA
.
7C2510*   XADD   1                     TO  ACU-QTD-GLEBA-E.
.          ADD   1                     TO  ACU-QTD-GLEBA.
.     *----------------------------------------------------------------*
.      6150-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6155-OBTER-COORD-SEM-ALT        SECTION.
.     *----------------------------------------------------------------*
.
.          IF   WRK-FIM-CSR15       EQUAL  'S'
.               PERFORM  6205-OPEN-CSR15-RUECB09N
.               PERFORM  6210-FETCH-CSR15-RUECB09N
.          END-IF.
.
.     *   XIF  WRK-GLEBA-7B         EQUAL 'N' AND
STEFA2     IF  WRK-FIM-CSR15        EQUAL 'N'
7C2510         CONTINUE
7C2510*       XMOVE  RUCA66-COR0001-7B     TO
7C2510*                                WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                LENGTH OF RUCA66-COR0001-7B)
7C2510*       XADD   LENGTH OF RUCA66-COR0001-7B
7C2510*                             TO ACU-TAM
7C2510*                                ACU-POSICAO
STEFA2*       XMOVE 'S'             TO WRK-GLEBA-7B
.          END-IF
.
.          PERFORM
.            UNTIL  WRK-FIM-CSR15   EQUAL  'S'
7C2511*         OR  ACU-QTD-GLEBA-E EQUAL  90
7C2511          OR  ACU-QTD-GLEBA   EQUAL  350
.
.               PERFORM  6160-FORMATAR-COORD-SEM-ALT
.
7C2511*        XIF  ACU-QTD-GLEBA-E  LESS  90
7C2511          IF  ACU-QTD-GLEBA    LESS  350
.                   PERFORM  6210-FETCH-CSR15-RUECB09N
.               END-IF
.          END-PERFORM.
.
.          IF  WRK-FIM-CSR15    EQUAL  'S'
.              PERFORM  6215-CLOSE-CSR15-RUECB09N
7C2511*       XIF  ACU-QTD-GLEBA-E      EQUAL  ZEROS
7C2511         IF  ACU-QTD-GLEBA        EQUAL  ZEROS
.                  PERFORM  6170-TRATAR-SEM-GLEBA
STEFA2             MOVE 'S'             TO WRK-GRAVOU
STEFA2         ELSE
STEFA2             MOVE 'N'             TO WRK-GRAVOU
STEFAB         END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6155-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6160-FORMATAR-COORD-SEM-ALT     SECTION.
.     *----------------------------------------------------------------*
.
7C2510*    M0VE WRK-TAB9N-MLATTD-IMOV-RURAL
7C2510*                                TO  WRK-MLATTD-IMOV-RURAL-N
7C2510*    M0VE WRK-MLATTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-LAT-PONTO-EXCL
7C2510*    M0VE WRK-TAB9N-MLOGTD-IMOV-RURAL
7C2510*                                TO  WRK-MLOGTD-IMOV-RURAL-N
7C2510*    M0VE WRK-MLOGTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-LONG-PONTO-EXCL
7C2510*    M0VE WRK-TAB9N-MALTTD-IMOV-RURAL
7C2510*                                TO  WRK-MALTTD-IMOV-RURAL-N
7C2510*    M0VE WRK-MALTTD-IMOV-RURAL-X
7C2510*                                TO  RUCA66-ALTT-PONTO-EXCL
.
7C2510*   XMOVE  RUCA66-COR0001-7BB    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                LENGTH OF RUCA66-COR0001-7BB)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7BB
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO.
.          MOVE  'S'                   TO  WRK-TEM-GLEBA
.
7C2511*   XADD   1                     TO  ACU-QTD-GLEBA-E.
7C2511     ADD   1                     TO  ACU-QTD-GLEBA.
.
.     *----------------------------------------------------------------*
.      6160-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6170-TRATAR-SEM-GLEBA           SECTION.
.     *----------------------------------------------------------------*
.
STAFA2*   XIF  WRK-GLEBA-7B            EQUAL 'N'
7C2510*   XMOVE  RUCA66-COR0001-7B     TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7B)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7B
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO
.
7C2510*   XMOVE  ALL '*'               TO  RUCA66-LAT-PONTO-EXCL
7C2510*   XMOVE  ALL '*'               TO  RUCA66-LONG-PONTO-EXCL
7C2510*   XMOVE  ALL '*'               TO  RUCA66-ALTT-PONTO-EXCL
7C2510     MOVE  ALL '*'               TO  RUCA66-WKTPART.
.
7C2510*   XMOVE  RUCA66-COR0001-7BB    TO  WRK-MSGEM-RURAL(ACU-POSICAO:
7C2510*                                    LENGTH OF RUCA66-COR0001-7BB)
7C2510*   XADD   LENGTH OF RUCA66-COR0001-7BB
7C2510*                                TO  ACU-TAM
7C2510*                                    ACU-POSICAO.
STEFA2*   XEND-IF.
.
.     *----------------------------------------------------------------*
.      6170-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6190-OPEN-CSR14-RUCAB016        SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE  CCONTR                OF  RUCAB015
.     *                                TO  CCONTR            OF RUCAB016
.                                      TO  WRK-TAB16-CCONTR
.          MOVE  HINCL-REG             OF  RUCAB015
.     *                                TO  HINCL-REG         OF RUCAB016
.                                      TO  WRK-TAB16-HINCL-REG
.          MOVE  WRK-CSEQ-MATR-RURAL-ANT
.     *                                TO  CSEQ-MATR-RURAL   OF RUCAB016
.                                      TO  WRK-TAB16-CSEQ-MATR-RURAL
.          MOVE  WRK-NSEQ-GLEBA-IMOV-ANT
.     *                                TO  NSEQ-GLEBA-IMOV   OF RUCAB016
.                                      TO  WRK-TAB16-NSEQ-GLEBA-IMOV
.          MOVE  ZEROS                 TO  WRK-TAB16-NCORND-GEOGR-IMOV
.          MOVE  'N'                   TO  WRK-FIM-CSR14
.
.          EXEC SQL
.               OPEN CSR14-RUCAB016
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS AND -502 )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TMANUT_CORND_GEOGR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'OPEN'             TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0018'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF SQLCODE                  EQUAL -502
.             CONTINUE
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6190-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6195-FETCH-CSR14-RUCAB016       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               FETCH  CSR14-RUCAB016
.               INTO   :WRK-TAB16-CCONTR           ,
.                      :WRK-TAB16-HINCL-REG        ,
.                      :WRK-TAB16-CSEQ-MATR-RURAL  ,
.                      :WRK-TAB16-NSEQ-GLEBA-IMOV  ,
.                      :WRK-TAB16-NCORND-GEOGR-IMOV ,
.                      :WRK-TAB16-MLATTD-IMOV-RURAL ,
.                      :WRK-TAB16-MLOGTD-IMOV-RURAL ,
.                      :WRK-TAB16-MALTTD-IMOV-RURAL ,
.                      :WRK-TAB16-MAREA-NAO-CTVDA
.                        :WRK-B016-MAREA-NAO-CTVDA
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'             )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TMANUT_CORND_GEOGR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'FETCH'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0019'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF  SQLCODE              EQUAL  +100
.              MOVE  ZEROS             TO  WRK-TAB16-MAREA-NAO-CTVDA
.              MOVE  'S'               TO  WRK-FIM-CSR14
.          ELSE
.              IF  WRK-B016-MAREA-NAO-CTVDA  EQUAL  -1
.                  MOVE  ZEROS         TO  WRK-TAB16-MAREA-NAO-CTVDA
.              END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6195-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6200-CLOSE-CSR14-RUCAB016       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               CLOSE CSR14-RUCAB016
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TMANUT_CORND_GEOGR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0020'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6200-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6205-OPEN-CSR15-RUECB09N        SECTION.
.     *----------------------------------------------------------------*
.
.          MOVE  CCONTR                OF  RUECB040
.                                      TO  WRK-TAB9N-CCONTR
.          MOVE  WRK-CSEQ-MATR-RURAL-ANT
.                                      TO  WRK-TAB9N-CSEQ-MATR-RURAL
.          MOVE  WRK-NSEQ-GLEBA-IMOV-ANT
.                                      TO  WRK-TAB9N-NSEQ-GLEBA-IMOV
.          MOVE  ZEROS                 TO  WRK-TAB9N-NCORND-GEOGR-IMOV
.          MOVE  'N'                   TO  WRK-FIM-CSR15
.
.
.          EXEC SQL
.               OPEN CSR15-RUECB09N
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_CONTR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'OPEN'             TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0021'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6205-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6210-FETCH-CSR15-RUECB09N       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               FETCH  CSR15-RUECB09N
.               INTO   :WRK-TAB9N-CCONTR           ,
.                      :WRK-TAB9N-CSEQ-MATR-RURAL  ,
.                      :WRK-TAB9N-NSEQ-GLEBA-IMOV  ,
.                      :WRK-TAB9N-NCORND-GEOGR-IMOV ,
.                      :WRK-TAB9N-MLATTD-IMOV-RURAL ,
.                      :WRK-TAB9N-MLOGTD-IMOV-RURAL ,
.                      :WRK-TAB9N-MALTTD-IMOV-RURAL
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
.             ( SQLWARN0            EQUAL  'W'             )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_CONTR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'FETCH'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0022'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.          IF  SQLCODE              EQUAL  +100
.              MOVE  'S'               TO  WRK-FIM-CSR15
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6210-99-FIM.  EXIT.
.     *----------------------------------------------------------------*
.
.     *----------------------------------------------------------------*
.      6215-CLOSE-CSR15-RUECB09N       SECTION.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.               CLOSE CSR15-RUECB09N
.          END-EXEC.
.
.          IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
.             ( SQLWARN0            EQUAL  'W'    )
.              MOVE 'DB2'              TO  ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_CONTR'
.                                      TO  ERR-DBD-TAB
.              MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
.              MOVE  SQLCODE           TO  ERR-SQL-CODE
.              MOVE '0023'             TO  ERR-LOCAL
.              MOVE  SPACES            TO  ERR-SEGM
.              MOVE  99                TO  COMU-8810-COD-RETORNO
.              MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
.              MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
.              MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
.              PERFORM  8000-FINALIZAR
.          END-IF.
.
.     *----------------------------------------------------------------*
.      6215-99-FIM.  EXIT.
STEFAB*----------------------------------------------------------------*

7C5.10*----------------------------------------------------------------*
.      6300-OBTER-TIPO-CONFORMIDADE    SECTION.
.     *----------------------------------------------------------------*
.
.          PERFORM  6310-OPEN-CSR16-RUCAB023
.          PERFORM  6320-FETCH-CSR16-RUCAB023
.
.          IF  WRK-FIM-CSR16        EQUAL 'N'
.              MOVE  RUCA66-COR0001-21B    TO
.                                      WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21B)
.              ADD   LENGTH OF RUCA66-COR0001-21B
.                                          TO ACU-TAM
.                                             ACU-POSICAO
.          ELSE
.              MOVE  ALL '*'        TO  RUCA66-GP-COR0001-CONFDD
.              PERFORM  6330-CLOSE-CSR16-RUCAB023
.              MOVE  RUCA66-COR0001-21B    TO
.                                      WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21B)
.              ADD   LENGTH OF RUCA66-COR0001-21B
.                                          TO ACU-TAM
.                                             ACU-POSICAO
.              GO TO 6300-99-FIM
.          END-IF
.
.          PERFORM
.            UNTIL ( WRK-FIM-CSR16       EQUAL  'S' )
.
.              PERFORM  6320-FETCH-CSR16-RUCAB023
.              IF  WRK-FIM-CSR16        EQUAL 'N'
.                 MOVE  RUCA66-COR0001-21B    TO
.                                      WRK-MSGEM-RURAL(ACU-POSICAO:
.                                      LENGTH OF RUCA66-COR0001-21B)
.                 ADD   LENGTH OF RUCA66-COR0001-21B
.                                             TO ACU-TAM
.                                                ACU-POSICAO
.              END-IF
.          END-PERFORM.
.
.          PERFORM  6330-CLOSE-CSR16-RUCAB023.
.
.
.     *----------------------------------------------------------------*
.      6300-99-FIM.  EXIT.
7C5.10*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6310-OPEN-CSR16-RUCAB023        SECTION.
      *----------------------------------------------------------------*

           MOVE '-'                    TO WRK-TR1-TIMES
                                          WRK-TR2-TIMES
                                          WRK-TR3-TIMES
           MOVE '.'                    TO WRK-PT1-TIMES
                                          WRK-PT2-TIMES
                                          WRK-PT3-TIMES
           MOVE  COMU-8810-CHAVE       TO  CCONTR           OF RUCAB023
           MOVE  COMU-8810-ANO-CTRLNAV TO  WRK-ANO-TIMES
           MOVE  COMU-8810-MES-CTRLNAV TO  WRK-MES-TIMES
           MOVE  COMU-8810-DIA-CTRLNAV TO  WRK-DIA-TIMES
           MOVE  COMU-8810-HOR-CTRLNAV TO  WRK-HOR-TIMES
           MOVE  COMU-8810-MIN-CTRLNAV TO  WRK-MIN-TIMES
           MOVE  COMU-8810-SEG-CTRLNAV TO  WRK-SEG-TIMES
           MOVE  COMU-8810-MIL-CTRLNAV TO  WRK-MIL-TIMES
           MOVE  WRK-TIMESTAMP         TO  HINCL-REG        OF RUCAB023
           MOVE  'N'                   TO  WRK-FIM-CSR16


           EXEC SQL
                OPEN CSR16-RUCAB023
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RECOR_TPO_CONFM  '
                                       TO  ERR-DBD-TAB
               MOVE 'OPEN'             TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '6310'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       6310-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6320-FETCH-CSR16-RUCAB023       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                FETCH  CSR16-RUCAB023
                 INTO  :RUCAB023.CCONTR  ,
                       :RUCAB023.COUTRO-TPO-CONFM
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  AND +100 )  OR
              ( SQLWARN0            EQUAL  'W'             )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RECOR_TPO_CONFM  '
                                       TO  ERR-DBD-TAB
               MOVE 'FETCH'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '6320'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

           IF  SQLCODE              EQUAL  ZEROS
               MOVE COUTRO-TPO-CONFM  OF RUCAB023 TO WRK-S9-5-AUX
               PERFORM  6340-BUSCA-TP-CONF
           ELSE
               MOVE  'S'               TO  WRK-FIM-CSR16
               MOVE  ZEROS             TO  COUTRO-TPO-CONFM  OF RUCAB023
           END-IF.

      *----------------------------------------------------------------*
       6320-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6330-CLOSE-CSR16-RUCAB023       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR16-RUCAB023
           END-EXEC.

           IF ( SQLCODE         NOT EQUAL  ZEROS  )  OR
              ( SQLWARN0            EQUAL  'W'    )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RECOR_TPO_CONFM  '
                                       TO  ERR-DBD-TAB
               MOVE 'CLOSE'            TO  ERR-FUN-COMANDO
               MOVE  SQLCODE           TO  ERR-SQL-CODE
               MOVE '6330'             TO  ERR-LOCAL
               MOVE  SPACES            TO  ERR-SEGM
               MOVE  99                TO  COMU-8810-COD-RETORNO
               MOVE  WRK-MSG-99        TO  COMU-8810-MENSAGEM
               MOVE  ERRO-AREA         TO  COMU-8810-ERRO-AREA
               MOVE  SQLCA(1:136)      TO  COMU-8810-SQLCA
               PERFORM  8000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       6330-99-FIM.  EXIT.
      *----------------------------------------------------------------*
.     *---------------------------------------------------------------*
.      6340-BUSCA-TP-CONF              SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE SPACES                 TO  CRURWPGI-E-ROUTRO-CONFM-BACEN
.                                          CRURWPGI-E-CSIT-REG-RURAL
.          MOVE 1                      TO  CRURWPGI-E-MAX-OCORR
.          MOVE 'I'                    TO  CRURWPGI-INDICADOR-PAGINACAO
.          MOVE WRK-9-5-AUX            TO  CRURWPGI-E-COUTRO-TPO-CONFM
.
.          CALL  WRK-CRUR240G          USING WRK-AREA-CRUR240G.
.
.          IF CRURWPGI-E-COUTRO-TPO-CONFM EQUAL
.             CRURWPGI-S-COUTRO-TPO-CONFM(1)
.             IF CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.                                         NUMERIC
.                MOVE CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.                              TO RUCA66-TP-CONFDD
.             END-IF
.          END-IF.
.
.     *---------------------------------------------------------------*
.      6340-99-FIM.                    EXIT.
.     *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.  EXIT.
      *----------------------------------------------------------------*
