      *================================================================*
      * VERSAO 4.06- IMPLANTACAO PREVISTA JULHO/2015                   *
      *================================================================*
      * INCLUSAO DO CAMPO SUBPROGRAMA                                  *
      *================================================================*
      * VERSAO 4.0 - IMPLANTACAO PREVISTA OUTUBRO/2013                 *
      *================================================================*
      * VERSAO COM QUEBRA DE MSGEM                                     *
      * REPETINDO EM CADA QUEBRA APENAS AS INFORMACOES FIXAS           *
      * GRUPOS DE REPETICAO PREENCHIDOS APENAS NA PRIMEIRA SEQUENCIA   *
      * NAS QUEBRAS PREENCHIDOS COM '*'                                *
      *================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
NAV506*  ANALISTA......:   NAVA                                        *
NAV506*  DATA..........:   MAIO/2023                                   *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*                    RECOMPILAR ALTERACAO I#RUEC65               *
NAV506*================================================================*
STE001*================================================================*
STE001*  ANALISTA......:   STEFANINI                                   *
STE001*  DATA..........:   MAIO/2024                                   *
STE001*----------------------------------------------------------------*
STE001*  OBJETIVO......:   ADEQUACOES SICOR 5.08                       *
STE001*                    RECOMPILAR ALTERACAO I#RUEC66               *
STE001*================================================================*

       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8810.
       AUTHOR.     BSI TECNOLOGIA.
      *================================================================*
      *                    B S I  T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8810                                    *
      *    ANALISTA....:   BSI TECNOLOGIA                              *
      *    DATA........:   08/2012                                     *
      *    PROJETO.....:   PWT 12/9931    2012/81536/5-01              *
      *                    REFORMULACAO MENSAGERIA BACEN - RECOR       *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR SISMSG COR0001 P/ ENVIO SISTEMA RCOR  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC65 - AREA DE RETORNO DO MODULO RUEC8810               *
      *    I#RCOR10 - AREA DE RETORNO DO MODULO RCOR5005               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *    RUEC8810 - FORMATA/ENVIA SISMSG COR0001                     *
      *    RUEC8822 - FORMATA GRUPO FORNECEDORES                       *
      *                                                                *
      *    TABELA                                                      *
      *    TENVIO_RECOR_TEMPR                        RUECB09A          *
      *    THIST_ENVIO_RECOR                         RUECB09B          *
      *    TPCELA_ENVIO_RECOR                        RUECB09D          *
      *    TPSSOA_ENVIO_RECOR                        RUECB09E          *
      *    TPRODT_RURAL_RECOR                        RUECB09F          *
      *    TCONTR_ENVIO_RECOR                        RUECB099          *
      *    CONTRATO_RURAL                            RUFIB002          *
      *    PRODUTO_RURAL                             RURCB006          *
      *    V01ORCAM_CDULA                            RUECV025          *
      *    V01ORCAMENTO_RURAL                        RUIAV007          *
      *    V01PCELA_ORCAM                            RUIAV008          *
      *    V01TPO_OPER_ORCAM                         RUIAV005          *
      *    V01MUNICIPIO_RURAL                        RURCV014          *
      *    FNALD_RURAL                               RURCB003          *
      *    CEDULA_TEMPR                              RUECB007          *
      *    TMOTVO_DCLAS_CONTR                        RUFIB044          *
      *    TMOTVO_DCLAS_OPER                         RURCB097          *
      *================================================================*
      *                                                                *
      * BSI TECNOLOGIA.                                                *
      *                                                                *
      * ALTERACAO: BS0250  - ARQUIVO FORNECEDORES VERSAO 4.0           *
      *                      ALTERAR MOVAS CONDICOES DO CSIT-MSGEM     *
      * DATA.....: OUTUBRO/2013                                        *
      * ANALISTA.: DANILO KIOROGLO                                     *
      *                                                                *
      *================================================================*
      *                                                                *
      * BSI TECNOLOGIA.                                                *
      *                                                                *
      * ALTERACAO: BS0000  - SICOR - ALTERACAO EXTRAORDINARIA NO       *
      *                      CATALOGO DE MENSAGEM.                     *
      * DATA.....: DEZEMBRO/2013                                       *
      * ANALISTA.: DANILO KIOROGLO                                     *
      *                                                                *
      *================================================================*
EG0615*                                                                *
EG0615* BSI TECNOLOGIA.                                                *
EG0615*                                                                *
EG0615* ALTERACAO: EG0615  - SICOR - VERSAO 4.06                       *
EG0615*                      INCLUSAO CAMPO SUBPROGRAMA                *
EG0615* DATA.....: JULHO/2015                                          *
EG0615* ANALISTA.: EVANDRO GUIMARAES                                   *
EG0615*                                                                *
EG0615*================================================================*
PA1215*                                                                *
PA1215* PATRICK C SILVEIRA - BSI                                       *
PA1215*                                                                *
PA1215* ALTERACAO: PA1215  - ALTERAR FORMATACAO DO GRUPO GLEBA         *
PA1215*                                                                *
PA1215* DATA.....: DEZEMBRO/2015                                       *
PA1215* ANALISTA.: EVANDRO GUIMARAES                                   *
PA1215*                                                                *
PA1215*================================================================*
BS0216*                                                                *
BS0216* BSI TECNOLOGIA.                                                *
BS0216*                                                                *
BS0216* ALTERACAO: BS0216  - SICOR - VERSAO 4.07                       *
BS0216* DATA.....: FEVEREIRO/2016                                      *
BS0216* ANALISTA.: EDSON MATSUMOTO                                     *
BS0216*                                                                *
EG0916*================================================================*
EG0916*                      A L T E R A C A O                         *
EG0916*================================================================*
EG0916*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
EG0916*    DATA........:   SETEMBRO/2016                               *
EG0916*    OBJETIVO....:   ADAPTACAO CATALOGO 4.08                     *
EG0916*================================================================*
BR1117*================================================================*
BR1117*                      A L T E R A C A O                         *
BR1117*================================================================*
BR1117*    ANALISTA....:   EDSON MATSUMOTO         - BRQ SOLUCOES      *
BR1117*    DATA........:   NOVEMBRO/2017                               *
BR1117*    OBJETIVO....:   ADAPTACAO CATALOGO 4.10                     *
BR1117*================================================================*
BR1117*    ANALISTA....:   EDSON MATSUMOTO          - BRQ SOLUCOES INF *
BR1117*    DATA........:   NOVEMBRO/2017                               *
BR1117*----------------------------------------------------------------*
BR1117*    OBJETIVO....:  TRATAR NRO. CAR                              *
BR1117*----------------------------------------------------------------*
BR1811*================================================================*
BR1811* ALTERACAO..........: COORDENADAS GEODESICAS                    *
BR1811* ANALISTA REPONSAVEL: PATRICK SILVEIRA                          *
BR1811* DATA...............: 11/2018                                   *
BR1811* OBJETIVOS..........: ENVIAR MENSAGEM DE CONTINUACAO            *
BR1811*================================================================*
BR1117*================================================================*
BR1117*                      A L T E R A C A O                         *
BR1117*================================================================*
BR1117*    ANALISTA....:   RODOLFO LUIZ DE OLIVEIRA SILVA - CAPGEMINI  *
BR1117*    DATA........:   OUTUBRO/2019                                *
BR1117*    OBJETIVO....:   ADAPTACAO CATALOGO 4.14                     *
BR1117*================================================================*
BR1117*    OBJETIVO....:  TRATAR NRO. CAR                              *
BR1117*----------------------------------------------------------------*
WMM   *                    ALTERACAO - PRIME                           *
WMM   *----------------------------------------------------------------*
WMM   *  ANALISTA PRIME:   WALTER MESSAS                               *
WMM   *  DATA..........:   26/12/2019                                  *
WMM   *----------------------------------------------------------------*
WMM   *  OBJETIVO......:   ENCARGOS FINANCEIROS COMPLEMENTARES         *
WMM   *                    ALTERACAO REGRA DE ENVIO SICOR              *
WMM   *================================================================*
7C0120*================================================================*
.     *                      A L T E R A C A O                         *
.     *================================================================*
.     *    ANALISTA....:   EDSON MATSUMOTO         - 7COMM             *
.     *    DATA........:   JANEIRO/2020                                *
.     *    OBJETIVO....:   ALTERAR A FORMA DE ENVIO DE CONTINUACAO,    *
.     *                    DEVERA  AGUARDAR O RETORNO DO SICOR PARA    *
.     *                    ENVIO DA PROXIMA COR0001 SERA FEITO NO      *
.     *                    RUEC8804                                    *
7C0120*================================================================*
QT1120*                    ALTERACAO - QINTESS                         *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA QINTESS: KELLY CHUCATA                               *
QT1120*  DATA...........:  13/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO......:   ADEQUAR PARA SICOR 5.01                     *
QT1120*    ALTERACOES NA COR0005:                                      *
QT1120*                    - RETIRADA DO CAMPO "PERCENTUAL DESCLASSIFI *
QT1120*                                         CACAO"                 *
QT1120*================================================================*
WI0421*                   A L T E R A C A O                            *
WI0421*================================================================*
WI0421*                                                                *
WI0421*     ANALISTA : MILTON JANUARIO         - WIPRO                 *
WI0421*     DATA     : ABRIL / 2021                                    *
WI0421*     OBJETIVO : IN5624443 - PERMITIR INCLUIR OS CAMPOS          *
WI0421*                CAMPOS QUANTIDADE E ÁREA PARA O EMPREENDIMENTO  *
WI0421*                BACEN  23271300193002                           *
WI0421*================================================================*
BRQ001*----------------------------------------------------------------*
BRQ001*               U L T I M A  A L T E R A C A O                   *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*      DATA         : MAIO/2021                                  *
BRQ001*                                                                *
BRQ001*      OBJETIVOS    : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
BRQ001*                     V5.02 NA MENSAGEM COR0001                  *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
ONEDA *                      A L T E R A C A O                         *
ONEDA *================================================================*
ONEDA *    ANALISTA....:   ADILSON ONEDA    -   BRADESCO               *
ONEDA *    DATA........:   JULHO/2021                                  *
ONEDA *    OBJETIVO....:   NOTICIAS SICOR DE 07/05/2021                *
ONEDA *                    NAO ENVIAR AREA PARA OPERACOES COM COORDENA-*
ONEDA *                    DAS GEODESICAS                              *
ONEDA *================================================================*
WP1021*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2021                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
.     *                    FORMATACAO DOS CAMPOS                       *
WP1021*================================================================*
MJ0522*================================================================*
MJ0522*  ANALISTA......:   MILTON JANUARIO        - DS                 *
MJ0522*  DATA..........:   MAIO/2022                                   *
MJ0522*----------------------------------------------------------------*
MJ0522*  OBJETIVO......:   NAO ENVIAR DATAS DE PLANTIO E COLHEITA PARA *
MJ0522*                    OS PRODUTOS PERENE                          *
MJ0522*================================================================*
WIP001*================================================================*
.     *  ANALISTA......:   WIPRO                                       *
.     *  DATA..........:   OUTUBRO/2022                                *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES SICOR 5.05 - MVP1                *
.     *                    FORMATACAO DOS CAMPOS                       *
WIP001*================================================================*
MJ1223*================================================================*
MJ1223*  ANALISTA......:   MILTON JANUARIO - DS-BRADESCO               *
MJ1223*  DATA..........:   DEZEMBRO/2023                               *
MJ1223*----------------------------------------------------------------*
MJ1223*  OBJETIVO......:   ENVIO DAS DATAS DE INICIO E FIM DE COLHEITA *
MJ1223*                    FORMATACAO DOS CAMPOS                       *
MJ1223*================================================================*
MJ0824*================================================================*
MJ0824*  ANALISTA......:   MILTON JANUARIO - DS-BRADESCO               *
MJ0824*  DATA..........:   AGOSTO/2024                                 *
MJ0824*----------------------------------------------------------------*
MJ0824*  OBJETIVO......:   NOTICIAS SICOR 24/07/2024 - ENVIO DE DATA   *
MJ0824*                    DE PLANTIO PARA CULTURA PERENE              *
MJ0824*================================================================*
BR1117*================================================================*
STE002*================================================================*
.     *  ANALISTA......:   STEFANINI                                   *
.     *  DATA..........:   FEVEREIRO / 2024                            *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   PROJETO UNIDADE ANIMAL                      *
.     *                                                                *
.     *================================================================*
STE002*================================================================*
050825*  ANALISTA......:   7COMM - SQUAD FLASH                         *
.     *  DATA..........:   AGOSTO / 2025                               *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES DE LAYOUT PARA SICOR 5.10        *
.     *                    TRATAMENTO PARA FORMATACAO DO CPF           *
050825*                    TRATAMENTO PARA CNPJ ALFANUMERICO           *
      *================================================================*
211025*  ANALISTA......:   7COMM - SQUAD FLASH - AGRO7-2978            *
.     *  DATA..........:   OUTUBRO / 2025                              *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ADEQUACOES DE LAYOUT PARA SICOR 5.11        *
211025*                    NOVA MONTAGEM DO GRUPO GLEBA                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

       CONFIGURATION                   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** INICIO DA WORKING RUEC8810 ***'.
      *---------------------------------------------------------------*
       77  FILLER                      PIC X(079)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *---------------------------------------------------------------*

       77  WRK-SQLCA                   PIC X(136) VALUE SPACES.

       77  WRK-TAM                 PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-POSICAO             PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-NEW-POS             PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-NEW-POS2            PIC  9(05) COMP-3 VALUE ZEROS.
       77  WRK-1                   PIC  9(05) COMP-3 VALUE 1.

AC0513 77  WRK-TAM2                PIC  9(05) COMP-3 VALUE ZEROS.
AC0513 77  WRK-POSICAO2            PIC  9(05) COMP-3 VALUE ZEROS.

       77  WRK-CONTINUA            PIC  9(05) COMP-3 VALUE ZEROS.

       77  WRK-QTDREG-B09A         PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-PRIM-ENVIO          PIC  X(01) VALUE 'S'.
       77  WRK-PRIM-NUMCTRLIF      PIC  X(20) VALUE SPACES.
       77  WRK-PRIM-B09B           PIC  X(01) VALUE 'S'.

       77  WRK-999                 PIC S9(05) COMP-3 VALUE 999.
211025*77  WRK-50                  PIC S9(05) COMP-3 VALUE 50.
211025 77  WRK-60                  PIC S9(05) COMP-3 VALUE 60.
AC0813 77  WRK-ZERO                PIC S9(03) COMP-3 VALUE ZEROS.
       77  WRK-IND1                PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND2                PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND3                PIC S9(05) COMP-3 VALUE ZEROS.
BR1217 77  WRK-IND4                PIC S9(05) COMP-3 VALUE ZEROS.
211025 77  WRK-IND5                PIC  9(03) COMP-3 VALUE ZEROS.
BR1217 77  WRK-IND-TAB             PIC S9(05) COMP-3 VALUE ZEROS.
BR1217 77  WRK-IND-INI             PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND-B09A            PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND-B09D            PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND-B09E            PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-IND-B09F            PIC S9(05) COMP-3 VALUE ZEROS.
050825 77  WRK-IND-B094            PIC S9(05) COMP-3 VALUE ZEROS.
       77  WRK-CODE100-B09A        PIC  X(01) VALUE 'N'.
       77  WRK-CODE100-B09D        PIC  X(01) VALUE 'N'.
       77  WRK-CODE100-B09E        PIC  X(01) VALUE 'N'.
       77  WRK-CODE100-B09F        PIC  X(01) VALUE 'N'.
050825 77  WRK-CODE100-B094        PIC  X(01) VALUE 'N'.
       77  WRK-ACHOU-B09F          PIC  X(01) VALUE 'N'.
       77  WRK-MODULO-RCOR5005     PIC  X(08) VALUE 'RCOR5005'.
EG0615 77  WRK-MODULO              PIC  X(08) VALUE 'RUEC8824'.
CAT408 77  WRK-MODULO-CMRU307C     PIC  X(08) VALUE 'CMRU307C'.
STEFAB 01  WRK-CRUR240G            PIC  X(08) VALUE 'CRUR240G'.
       77  WRK-TAMANHO-SISMSG      PIC S9(4)  COMP VALUE ZEROS.
       77  WRK-MAX-NREG      PIC S9(05) COMP-3 VALUE ZEROS.

BR4.12 01  WRK-BENEF               PIC X(03)   VALUE SPACES.

STE001 01  WRK-S9-3V99             PIC +9(03)V99 VALUE ZEROS.
STE001 01  FILLER REDEFINES WRK-S9-3V99.
STE001     05 FILLER               PIC  X(01).
STE001     05 WRK-9-3V99           PIC  9(03)V99.
STE001
STEFA1 01  WRK-S9-5A               PIC +9(05)    VALUE ZEROS.
.      01  FILLER REDEFINES WRK-S9-5A.
.          05 FILLER               PIC  X(01).
STEFA1     05 WRK-9-5A             PIC  9(05).

STEFA1 01  WRK-S9-2A               PIC  9(02)    VALUE ZEROS.
.      01  FILLER REDEFINES WRK-S9-2A.
STEFA1     05 WRK-9-2A             PIC  X(02).
STE001
EG0617*---------------------------------------------------------------*
EG0617 01  FILLER                      PIC  X(32)         VALUE
EG0617     '**** AUXILIARES DE DATA      ***'.
EG0617*----------------------------------------------------------------*
EG0617
EG0617 01  FILLER.
EG0617     05 WRK-66-DT-INI            PIC  X(008) VALUE SPACES.
EG0617     05 WRK-66-DT-FIM            PIC  X(008) VALUE SPACES.
EG0617
BR1217*---------------------------------------------------------------*
BR1217 01  FILLER                      PIC  X(32) VALUE
BR1217     '* TAB. INTERNA PARA PROPRIETARIO'.
BR1217*---------------------------------------------------------------*
BR1217
BR1217 01  WRK-TABELA.
BR1217     05  WRK-TAB-PROPRIETARIO OCCURS 100 TIMES.
BR1217         10  WRK-CTPO-PSSOA      PIC S9(01)    COMP-3 VALUE ZEROS.
BR1217         10  WRK-CCNPJ-CPF       PIC S9(09)    COMP-3 VALUE ZEROS.
BR1217         10  WRK-CFLIAL-CNPJ     PIC S9(05)    COMP-3 VALUE ZEROS.
BR1217         10  WRK-CCTRL-CNPJ-CPF  PIC S9(02)    COMP-3 VALUE ZEROS.
BR1217         10  WRK-CNIRF-IMOV      PIC  X(09)          VALUE SPACES.
BR1217         10  WRK-CCCIR           PIC  X(13)          VALUE SPACES.
BR1217         10  WRK-CREG-AMBTL      PIC  X(41)          VALUE SPACES.
BRQ001         10  WRK-PPRESV-AMBTL-PPRIE PIC  9(03)V99    VALUE ZEROS.
BRQ001         10  WRK-COUTGA-AGUA-IMOV   PIC  X(30)       VALUE SPACES.
BRQ001         10  WRK-DVALDD-OUTGA-AGUA  PIC  X(10)       VALUE SPACES.
BR1217
EG1216*----------------------------------------------------------------*
EG1216 01  FILLER                      PIC  X(32)         VALUE
EG1216     '**** AREA DE NULIDADE        ***'.
EG1216*----------------------------------------------------------------*
EG1216
EG1216 01  FILLER.
EG1216     10 WRK-V07-NUMERO-DAP-N     PIC S9(04) COMP    VALUE ZEROS.
BR1218     10 WRK-B07-VEGF-N           PIC S9(04) COMP    VALUE ZEROS.
BR1218     10 WRK-B02-VEGF-N           PIC S9(04) COMP    VALUE ZEROS.
EG1216
BR1218*----------------------------------------------------------------*
BR1218 01  FILLER                      PIC  X(32)         VALUE
BR1218     '** AUX. EXIGE COORDENADAS    ***'.
BR1218*----------------------------------------------------------------*
BR1218
BR1218 77  WRK-EXIGE-AREA              PIC X(01)    VALUE SPACES.
BR1218
       01  WRK-VLR1.
           05 FILLER            PIC  X(04) VALUE ZEROS.
           05 WRK-VLR15-PARC    PIC  9(13)V99 VALUE ZEROS.

       01  WRK-VLR2.
           05 FILLER            PIC  X(02) VALUE ZEROS.
           05 WRK-VLR17-PARC    PIC  9(15)V99 VALUE ZEROS.

STE001 01  WRK-VLR5             PIC +9(03)V99 VALUE ZEROS.
STE001 01  FILLER               REDEFINES WRK-VLR5.
STE001     05 FILLER            PIC  X(01).
STE001     05 WRK-VLR5-CHAR     PIC  9(03)V99.
STE001 01  WRK-VLR5-CHAR-1.
STE001     05 WRK-VLR5-CHAR-2   PIC  9(03)V99 VALUE ZEROS.

       01  WRK-VLR15            PIC S9(13)V99 VALUE ZEROS.
       01  FILLER               REDEFINES WRK-VLR15.
           05 WRK-VLR15-CHAR    PIC  9(13)V99.

       01  WRK-VLR17            PIC S9(15)V99        VALUE ZEROS.
       01  FILLER               REDEFINES WRK-VLR17.
           05 WRK-VLR17-CHAR    PIC  9(15)V99.


       01  WRK-JUROS            PIC S9(3)V9(05) VALUE ZEROS.
       01  FILLER        REDEFINES   WRK-JUROS.
           05 WRK-JUR5-NUM   PIC S9(3)V99.
           05 FILLER         PIC  9(3).

       01  WRK-QTD14            PIC S9(12)V99       VALUE ZEROS.
       01  FILLER               REDEFINES WRK-QTD14.
           05 WRK-QTD14-NUM     PIC  9(12)V99.

BR1217 01  WRK-COD9-NUM-S      PIC +9(9)        VALUE ZEROS.
BR1217 01  FILLER              REDEFINES WRK-COD9-NUM-S.
BR1217     05 FILLER           PIC  X(1).
BR1217     05 FILLER           PIC  9(1).
BR1217     05 WRK-COD9-NUM8-S  PIC  9(8).
BR1217     05 WRK-COD9-NUM8-XS REDEFINES  WRK-COD9-NUM8-S
BR1217                         PIC  X(8).

BR1217 01  WRK-COD9-CPF        PIC +9(9)        VALUE ZEROS.
BR1217 01  FILLER              REDEFINES WRK-COD9-CPF.
BR1217     05 FILLER           PIC  9(1).
BR1217     05 WRK-COD9-CPF-9   PIC  9(9).

       01  WRK-COD9-NUM        PIC  9(9)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD9-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD9-NUM8    PIC  9(8).
           05 WRK-COD9-NUM8-X  REDEFINES  WRK-COD9-NUM8
                               PIC  X(8).

050825 01  WRK-TEM-TPCONFDD    PIC  X(1)        VALUE SPACES.
.
.      01  WRK-AUX-2-X.
.          05 WRK-AUX-2-N      PIC  9(2)        VALUE ZEROS.
.
.      01  WRK-AUX-9-X.
050825     05 WRK-AUX-9-N      PIC  9(9)        VALUE ZEROS.

       01  WRK-COD5-NUM        PIC  9(5)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD5-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD5-NUM4    PIC  9(4).

       01  WRK-COD5-CHAR       PIC  X(5)        VALUE SPACES.
       01  FILLER              REDEFINES WRK-COD5-CHAR.
           05 FILLER           PIC  X(1).
           05 WRK-COD5-CHAR4   PIC  X(4).

BR1217 01  WRK-COD2-NUM-S      PIC +9(2)        VALUE ZEROS.
BR1217 01  FILLER              REDEFINES WRK-COD2-NUM-S.
BR1217     05 FILLER           PIC  X(1).
BR1217     05 WRK-COD2-NUM     PIC  9(2).

       01  WRK-COD3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-COD3-NUM.
           05 FILLER           PIC  9(1).
           05 WRK-COD3-NUM2    PIC  9(2).

       01  WRK-AUX3-NUM        PIC  9(3)        VALUE ZEROS.
       01  FILLER              REDEFINES WRK-AUX3-NUM.
           05 FILLER           PIC  9(2).
           05 WRK-AUX3-NUM1    PIC  9(1).

       01  WRK-QAREA-COMS       PIC S9(09)V99     VALUE ZEROS.
       01  FILLER              REDEFINES WRK-QAREA-COMS.
           05 WRK-QAREA-SEMS    PIC  9(09)V99.

       01  WRK-EDIT-CNPJ.
           05 WRK-EDIT-NCNPJ       PIC  9(08) VALUE ZEROS.
           05 WRK-EDIT-FCNPJ       PIC  9(04) VALUE ZEROS.
           05 WRK-EDIT-CCNPJ       PIC  9(02) VALUE ZEROS.
       01  WRK-EDIT-CNPJ-R  REDEFINES  WRK-EDIT-CNPJ  PIC 9(14).

       01  WRK-EDIT-CPF.
           05 WRK-EDIT-NCPF        PIC  9(09) VALUE ZEROS.
           05 WRK-EDIT-CCPF        PIC  9(02) VALUE ZEROS.

       01  WRK-SEQ-CSINAL          PIC  S9(05) VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-SEQ-CSINAL.
           05 WRK-SEQ-SSINAL       PIC   9(05).

       01  WRK-SEQ-NUM5            PIC   9(05) VALUE ZEROS.
       01  FILLER                  REDEFINES WRK-SEQ-NUM5.
           05 FILLER               PIC   9(02).
           05 WRK-SEQ-NUM3         PIC   9(03).

211025 01  WRK-MASK-COORD-X.
211025     05  WRK-MASK-COORD          PIC -999,9999999    VALUE ZEROS.

211025********** TABELA INTERNA PARA COORDENADAS *********
  .    01  FILLER.
  .        05 WRK-QTD-CULTVD           PIC 9(03) COMP-3    VALUE ZEROS.
  .        05 WRK-PONTO-CULTVD         OCCURS 999.
  .           10 WRK-CULTVD-LON        PIC X(12)           VALUE SPACES.
  .           10 FILLER                PIC X(01)           VALUE SPACES.
  .           10 WRK-CULTVD-LAT        PIC X(12)           VALUE SPACES.
  .        05 WRK-QTD-NCULTVD          PIC 9(03) COMP-3    VALUE ZEROS.
  .        05 WRK-PONTO-NCULTVD        OCCURS 999.
  .           10 WRK-NCULTVD-LON       PIC X(12)           VALUE SPACES.
  .           10 FILLER                PIC X(01)           VALUE SPACES.
  .           10 WRK-NCULTVD-LAT       PIC X(12)           VALUE SPACES.
  .
  .    01  WRK-NUM-POLIGN              PIC 9(01) COMP-3    VALUE ZEROS.
  .    01  WRK-TAM-POLIGN              PIC 9(05) COMP-3    VALUE ZEROS.
  .    01  WRK-POLIGN                  PIC X(10000)        VALUE SPACES.
211025 01  IND-1                       PIC 9(03) COMP-3    VALUE ZEROS.

       01  WRK-NUMCTRLIF.
           05 WRK-NUMCTRLIF-FIXO1  PIC X(008)  VALUE '0237RUEC'.
           05 WRK-NUMCTRLIF-ANO    PIC 9(004)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-MES    PIC 9(002)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-DIA    PIC 9(002)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-FIXO2  PIC X(004)  VALUE '0001'.

       77  WRK-ENVIO-LEN            PIC S9(004) COMP VALUE ZEROS.

       01  WRK-ENVIO-SIS            PIC X(32000)    VALUE SPACES.
       01  WRK-ENVIO-SIS-R          REDEFINES WRK-ENVIO-SIS.
           05 FILLER                PIC X(00009).
           05 WRK-ENVIO-NUMCTRLIF   PIC X(00020).
           05 FILLER                PIC X(31971).

       01  WRK-B09A-SIS            PIC X(32000)     VALUE SPACES.
       01  WRK-B09A-SIS-R          REDEFINES WRK-B09A-SIS.
           05 FILLER               PIC X(00009).
           05 WRK-B09A-NUMCTRLIF   PIC X(00020).
           05 FILLER               PIC X(31971).

AC0513 01  WRK-B09A-SIS2           PIC X(32000)     VALUE SPACES.
AC0513 01  WRK-B09A-SIS-R2         REDEFINES WRK-B09A-SIS2.
AC0513     05 FILLER               PIC X(00009).
AC0513     05 WRK-B09A-NUMCTRLI2   PIC X(00020).
AC0513     05 FILLER               PIC X(31971).

       01  WRK-TIMESTAMP-B09A      PIC X(26) VALUE SPACES.

       01  WRK-TIMESTAMP           PIC X(26) VALUE SPACES.
       01  WRK-FORMA-DATA          REDEFINES WRK-TIMESTAMP.
           05 WRK-FORMA-AAAA       PIC  9(04).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MM         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-DD         PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-HORA       PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MIN        PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-SEG        PIC  9(02).
           05 FILLER               PIC  X(01).
           05 WRK-FORMA-MILE       PIC  9(06).

       01  WRK-DATA.
           05 WRK-DD          PIC  9(02) VALUE ZEROS.
           05 FILLER          PIC  X(01) VALUE '.' .
           05 WRK-MM          PIC  9(02) VALUE ZEROS.
           05 FILLER          PIC  X(01) VALUE '.' .
           05 WRK-AAAA        PIC  9(04) VALUE ZEROS.

       01  WRK-EDIT-DATA.
           05 WRK-EDIT-AAAA        PIC  9(04) VALUE ZEROS.
           05 WRK-EDIT-MM          PIC  9(02) VALUE ZEROS.
           05 WRK-EDIT-DD          PIC  9(02) VALUE ZEROS.
       01  WRK-EDIT-DATA-R  REDEFINES  WRK-EDIT-DATA  PIC 9(8).

BR1217 01  WRK-CNPJ-CPF-ANT.
BR1217     05  WRK-CCNPJ-CPF-ANT       PIC S9(09)    COMP-3 VALUE ZEROS.
BR1217     05  WRK-CFLIAL-CNPJ-ANT     PIC S9(05)    COMP-3 VALUE ZEROS.
BR1217     05  WRK-CCTRL-CNPJ-CPF-ANT  PIC S9(02)    COMP-3 VALUE ZEROS.

       01  WRK-QTDREG-B097         PIC S9(05) COMP-3 VALUE ZEROS.

BR1217 01  WRK-ATUALIZA            PIC   X(01)             VALUE SPACES.
PA1215 01  WRK-FIM-B40             PIC   X(01)             VALUE SPACES.
BS0216 01  WRK-FIM-V25             PIC   X(01)             VALUE SPACES.
BS0216 01  WRK-ACHOU-V25-COLHT     PIC   X(01)             VALUE SPACES.
BS0216 01  WRK-ACHOU-V25-PLANT     PIC   X(01)             VALUE SPACES.
BS0216 01  WRK-FIM-RUIA            PIC   X(01)             VALUE SPACES.
BS0216 01  WRK-ACHOU-RUIA          PIC   X(01)             VALUE SPACES.
PA1215 01  WRK-FIM-B9N             PIC   X(01)             VALUE SPACES.
STEFAB 01  WRK-FIM-B9N-E           PIC   X(01)             VALUE SPACES.
PA1215
BS0216 01  WRK-LOG-PONTO-GRUPO.
BS0216     05 WRK-LOG-PONTO        PIC  S9(03)V9(11)       VALUE ZEROS.
211025 01  FILLER      REDEFINES WRK-LOG-PONTO-GRUPO.
211025     05  WRK-LOG-PONTO-9V7       PIC S9(03)V9(07).
211025     05  FILLER                  PIC X(4).
BS0216
BS0216 01  WRK-LAT-PONTO-GRUPO.
BS0216     05 WRK-LAT-PONTO        PIC  S9(03)V9(11)       VALUE ZEROS.
211025 01  FILLER      REDEFINES WRK-LAT-PONTO-GRUPO.
211025     05  WRK-LAT-PONTO-9V7       PIC S9(03)V9(07).
211025     05  FILLER                  PIC X(4).
BS0216
BRQ001 01  WRK-PPRESV-CS            PIC +9(03)V99    VALUE ZEROS.
BRQ001 01  FILLER                   REDEFINES WRK-PPRESV-CS.
BRQ001     05 FILLER                PIC  X(01).
BRQ001     05 WRK-PPRESV-SS         PIC  9(03)V99.

BR1811*----------------------------------------------------------------*
BR1811 01  FILLER                      PIC  X(050)         VALUE
BR1811     '*** TRATAMENTO DE CONTINUACAO ***'.
BR1811*----------------------------------------------------------------*
BR1811 01  WRK-FINAL                   PIC X(01)         VALUE SPACES.
BR1811 01  WRK-PRIMEIRO-IMOV           PIC X(01)         VALUE SPACES.
BR1811 01  WRK-PRIMEIRA-COORD          PIC X(01)         VALUE SPACES.
STEFAB 01  WRK-PRIMEIRA-COORD-E        PIC X(01)         VALUE SPACES.
BR1811 01  WRK-QTD-COORD               PIC 9(05) COMP-3  VALUE ZEROS.
BR1811 01  WRK-CONTA-ENVIO             PIC S9(05) COMP-3 VALUE ZEROS.
BR1811
BR1811 01  WRK-TIMES-1S                PIC X(26)         VALUE SPACES.
BR1811 01  WRK-TIMESTAMP-1S            PIC X(26)         VALUE SPACES.
BR1811
BR1811 01  WRK-AREA-POOL7600.
BR1811     03  WRK-PO-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
BR1811     03  WRK-PO-TIMESTAMP        PIC X(20)         VALUE SPACES.
BR1811
CAT408*----------------------------------------------------------------*
CAT408 01  FILLER                      PIC  X(050)         VALUE
CAT408     '*** AREA DO MODULO CMRU307C  ***'.
CAT408*----------------------------------------------------------------*
CAT408
CAT408 01  WRK-AREA-CMRU307C.
CAT408     COPY CMRUW09I.
CAT408
      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA DE FORMATACAO P/ MODULO RCOR5005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RUEC66'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA MODULO RCOR5005 ***'.
      *---------------------------------------------------------------*

       COPY 'I#RCOR10'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA CHAMADA BRAD7600   ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
           03  WRK-TI-TIMESTAMP        PIC X(20)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA FORMATACAO DE ERRO ***'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

STEFAB*----------------------------------------------------------------*
STEFAB 01  FILLER                      PIC  X(032)         VALUE
STEFAB     '*** AREA DO MODULO CRUR240G  ***'.
STEFAB*----------------------------------------------------------------*
STEFAB
STEFAB 01  WRK-AREA-CRUR240G.
STEFAB     COPY CRURW00W.
STEFAB     COPY CRURWPGI.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(079)          VALUE
           '*** AREA PARA TABELAS DB2 ***'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09A
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09B
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09D
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09E
           END-EXEC.

           EXEC SQL
               INCLUDE RUECB09F
           END-EXEC.


           EXEC SQL
               INCLUDE RUECB099
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

050825     EXEC SQL
.              INCLUDE RUFIB094
050825     END-EXEC.

           EXEC SQL
               INCLUDE RURCB006
           END-EXEC.

BS0216     EXEC SQL
BS0216         INCLUDE RUECV025
BS0216     END-EXEC.
BS0216
PA1215     EXEC SQL
PA1215         INCLUDE RUECB040
PA1215     END-EXEC.
PA1215
PA1215     EXEC SQL
PA1215         INCLUDE RUECB09N
PA1215     END-EXEC.
PA1215
BS0216     EXEC SQL
BS0216         INCLUDE RUIAV007
BS0216     END-EXEC.
BS0216
BS0216     EXEC SQL
BS0216         INCLUDE RUIAV008
BS0216     END-EXEC.
BS0216
BS0216     EXEC SQL
BS0216         INCLUDE RUIAV005
BS0216     END-EXEC.
BS0216
BS0216     EXEC SQL
BS0216         INCLUDE RURCV014
BS0216     END-EXEC.
BS0216
BS0216     EXEC SQL
BS0216         INCLUDE RURCB003
BS0216     END-EXEC.
BS0216
BS0216     EXEC SQL
BS0216         INCLUDE RUECB007
BS0216     END-EXEC.
BS0216
BR4.11*   -EXEC SQL
BR4.11*       -INCLUDE RUFIB044
BR4.11*   -END-EXEC.

BR4.11*   -EXEC SQL
BR4.11*       -INCLUDE RURCB097
BR4.11*   -END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE NULIDADE      ***'.
      *----------------------------------------------------------------*
       01  WRK-B09A-NULL.
           05 WRK-B09A-CSIT-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B09A-HINCL-NULL  PIC S9(04) COMP VALUE +0.

       01  WRK-B09B-NULL.
           05 WRK-B09B-CMSGEM-NULL      PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-HINCL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CCTRL-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUNIC-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CREFT-NULL       PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CSIT-NULL        PIC S9(04) COMP VALUE +0.
           05 WRK-B09B-CUSUAR-NULL      PIC S9(04) COMP VALUE +0.

       01  WRK-V025-NULL.
           05 WRK-DLIBRC-OP-ORCAM-NULL  PIC S9(04) COMP VALUE +0.
EG0617     05 WRK-DFIM-EVNTO-CDULA-NULL PIC S9(04) COMP VALUE +0.

       01  WRK-B09D-NULL.
           05 WRK-B09D-DVCTO-NULL PIC S9(04) COMP VALUE +0.

       01  WRK-B09E-NULL.
           05 WRK-B09E-RESP-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-PSSOA-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CCNPJ-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CFLIAL-NULL PIC S9(04) COMP VALUE +0.
           05 WRK-B09E-CCTRL-NULL  PIC S9(04) COMP VALUE +0.
EG0617     05 WRK-B040-CNIRF-IMOV-N PIC S9(4) COMP VALUE +0.
EG0617     05 WRK-B040-CCCIR-N     PIC S9(04) COMP VALUE +0.
EG0617     05 WRK-B040-CREG-AMBTL-N PIC S9(4) COMP VALUE +0.

       01  WRK-NULL-B09F.
           05 WRK-B09F-CPRODT-NULL PIC S9(04) COMP VALUE +0.

       01  WRK-B099-NULL.
           05 WRK-B099-DCDULA-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DVCTO-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-VFINAN       PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CCATEG-NULL  PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CPROG-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-CMUN-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-LIBRC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-REC-NULL     PIC S9(04) COMP VALUE +0.
           05 WRK-B099-FINCD-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-PROD-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DINIC-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B099-DFIM-NULL    PIC S9(04) COMP VALUE +0.
           05 WRK-B099-GARNT-NULL   PIC S9(04) COMP VALUE +0.
EG0615
EG0615 01  WRK-B002-NULL.
EG0615     05 WRK-N-CORIGE-REC         PIC S9(04) COMP     VALUE ZEROS.
EG0615     05 WRK-N-CFNALD-RURAL       PIC S9(04) COMP     VALUE ZEROS.
EG0615     05 WRK-N-CPRODT-RURAL       PIC S9(04) COMP     VALUE ZEROS.
BS0216     05 WRK-N-MEDD-PROVV         PIC S9(04) COMP     VALUE ZEROS.
BS0216     05 WRK-N-CMUN-RURAL         PIC S9(04) COMP     VALUE ZEROS.
BS0216     05 WRK-N-CATVDD-RURAL       PIC S9(04) COMP     VALUE ZEROS.
STE001     05 WRK-PREDC-JURO-AMBTL-NULL PIC S9(04) COMP VALUE ZEROS.
STE001     05 WRK-CINDCD-CONFM-FLORS-NULL PIC S9(04) COMP VALUE ZEROS.
STE001     05 WRK-CINDCD-CONFM-IBAMA-NULL PIC S9(04) COMP VALUE ZEROS.
STE001     05 WRK-CINDCD-CONFM-CSVAO-NULL PIC S9(04) COMP VALUE ZEROS.
BS0216
BS0216 01  WRK-B006-NULL.
BS0216     05 WRK-N-CTEMPO-VIDA        PIC S9(04) COMP     VALUE ZEROS.
BS0216
BRQ001 01  WRK-PPRESV-AMBTL-PPRIE-N    PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-COUTGA-AGUA-IMOV-N      PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-DVALDD-OUTGA-AGUA-N     PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-CCETFC-ORGNC-EMPTO-N    PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-EURL-CETFC-ORGNC-N      PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-CRSTRB-EMPTO-RURAL-N    PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-EURL-RSTRB-EMPTO-N      PIC S9(04) COMP VALUE ZEROS.
BRQ001 01  WRK-PENERG-RENOV-EMPTO-N    PIC S9(04) COMP VALUE ZEROS.

WP1021 01  WRK-CTPO-SOLO-AGCLT-NULL    PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CCICLO-CTVDA-NULL       PIC S9(04)  COMP    VALUE ZEROS.
WP1021 01  WRK-CMANUT-SOLO-NULL        PIC S9(04)  COMP    VALUE ZEROS.
STEFA1 01  WRK-COUTRO-NULL             PIC S9(04)  COMP    VALUE ZEROS.

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

EG0615*---------------------------------------------------------------*
EG0615 01  FILLER                      PIC X(032)          VALUE
EG0615     '*** AREA DO MODULO RUEC8824  ***'.
EG0615*---------------------------------------------------------------*
EG0615
EG0615 COPY 'RUECW824'.
EG0615
EG1215*----------------------------------------------------------------*
EG1215 01  FILLER                      PIC  X(032)         VALUE
EG1215     '*** AREA DO MODULO RUEC7941  ***'.
EG1215*----------------------------------------------------------------*
EG1215
EG1215     COPY 'RUECWGEO'.
EG1215
EG1215 01  FILLER.
EG1215     03 WRK-RUEC7941             PIC X(008)    VALUE 'RUEC7941'.
EG1215     03 WRK-SEQ-GLEBA-X19.
EG0517        05 FILLER                PIC 9(011)    VALUE ZEROS.
EG0517        05 WRK-SEQ-MATR-N03      PIC 9(003)    VALUE ZEROS.
EG0517        05 WRK-SEQ-GLEBA-N05     PIC 9(005)    VALUE ZEROS.
EG1215     03 ACU-GLEBA                PIC 9(009)    COMP-3 VALUE ZEROS.
EG1215
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*  DEFINICAO CURSOR CSR01-RUECB09E    *'.
      *----------------------------------------------------------------*

EG0717***  PALIATIVO: DEIXAR DE ENVIAR DADOS NIRF, CCIR E CAR AO
EG0717***  BACEN
           EXEC SQL
           DECLARE CSR01-RUECB09E-JN CURSOR  WITH HOLD  FOR
EG0717     SELECT  DISTINCT
EG0717             A.CCONTR_CREDT_RURAL,
EG0617             A.NENVIO_RECOR,
EG0617             A.CTPO_RESP_PSSOA,
EG0617             A.CTPO_PSSOA,
EG0617             A.CCNPJ_CPF,
EG0617             A.CFLIAL_CNPJ,
EG0617             A.CCTRL_CNPJ_CPF,
BR1217             B.CNIRF_IMOV_RURAL,
BR1217             B.CCCIR,
BR1217             B.CREG_AMBTL_RURAL,
BRQ001             B.PPRESV_AMBTL_PPRIE,
BRQ001             B.COUTGA_AGUA_IMOV,
BRQ001             B.DVALDD_OUTGA_AGUA
EG0617     FROM  DB2PRD.TPSSOA_ENVIO_RECOR          A
EG0617     LEFT  OUTER JOIN DB2PRD.MATR_CONTR_RURAL B
EG0617     ON    B.CCONTR             = A.CCONTR
EG0617     AND   B.CSEQ_MATR_RURAL    = A.CSEQ_MATR_RURAL
EG0617     WHERE A.CCONTR_CREDT_RURAL = :RUECB09E.CCONTR-CREDT-RURAL
EG0617     AND   A.NENVIO_RECOR       = :RUECB09E.NENVIO-RECOR
EG0617     AND   A.CTPO_RESP_PSSOA    = :RUECB09E.CTPO-RESP-PSSOA
BR1217     ORDER BY A.CTPO_PSSOA,
BR1217              A.CCNPJ_CPF,
BR1217              A.CFLIAL_CNPJ,
BR1217              A.CCTRL_CNPJ_CPF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*  DEFINICAO CURSOR CSR01-RUECB09D    *'.
      *----------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB09D CURSOR  WITH HOLD  FOR
           SELECT  CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NPCELA_CONTR,
                   DVCTO_PCELA_CONTR,
                   VPCELA_RURAL
           FROM  DB2PRD.TPCELA_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09D.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09D.NENVIO-RECOR
           ORDER BY  CCONTR_CREDT_RURAL,
                     NENVIO_RECOR
           END-EXEC.


      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*  DEFINICAO CURSOR CSR01-RUECB09F    *'.
      *----------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB09F CURSOR  WITH HOLD  FOR
           SELECT  CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NPRODT_RURAL_RECOR,
                   CPRODT_RURAL
           FROM  DB2PRD.TPRODT_RURAL_RECOR
           WHERE CCONTR_CREDT_RURAL = :RUECB09F.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09F.NENVIO-RECOR
           ORDER BY  CCONTR_CREDT_RURAL,
                     NENVIO_RECOR
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)           VALUE
           '*  DEFINICAO CURSOR CSR01-RUECB09A    *'.
      *----------------------------------------------------------------*

           EXEC SQL
           DECLARE CSR01-RUECB09A CURSOR  WITH HOLD  FOR
           SELECT  CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NREG_MSGEM_RECOR,
                   CSIT_MSGEM,
                   HINCL,
                   WCONTD_MSGEM_RURAL
           FROM  DB2PRD.TENVIO_RECOR_TEMPR
           WHERE CCONTR_CREDT_RURAL   = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB09A.NENVIO-RECOR
           AND   HINCL                = :RUECB09A.HINCL
           ORDER BY  CCONTR_CREDT_RURAL,
                     NENVIO_RECOR,
                     NREG_MSGEM_RECOR
           END-EXEC.

PA1215*----------------------------------------------------------------*
PA1215 01  FILLER                      PIC X(50)           VALUE
PA1215     '*  DEFINICAO CURSOR CSR01-RUECB040    *'.
PA1215*----------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
EG0517         DECLARE  CSR01-RUECB040-JN CURSOR WITH HOLD FOR
EG0517            SELECT  A.CCONTR
EG0517                  , A.CSEQ_MATR_RURAL
EG0517                  , VALUE ( B.NSEQ_GLEBA_IMOV , 0 )
EG0517                  , A.CMUN_RURAL
EG0517                  , VALUE ( MIN (B.NCORND_GEOGR_IMOV) , 0 )
EG0517              FROM  DB2PRD.MATR_CONTR_RURAL   A
EG0517              LEFT  OUTER JOIN
EG0517                    DB2PRD.TCORND_GEOGR_CONTR B
EG0517                ON  A.CCONTR          = B.CCONTR
EG0517               AND  A.CSEQ_MATR_RURAL = B.CSEQ_MATR_RURAL
EG0517             WHERE  A.CCONTR           = :RUECB040.CCONTR
STEFAB               AND (B.CTPO_CORND_GEOGR = 2 OR
STEFAB                    B.CTPO_CORND_GEOGR IS NULL)
EG0517          GROUP BY  A.CCONTR
EG0517                  , A.CSEQ_MATR_RURAL
EG0517                  , B.NSEQ_GLEBA_IMOV
EG0517                  , A.CMUN_RURAL
EG0517          ORDER BY  A.CCONTR
EG0517                  , A.CSEQ_MATR_RURAL
EG0517                  , B.NSEQ_GLEBA_IMOV
EG0517                  , A.CMUN_RURAL
PA1215     END-EXEC.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 01  FILLER                      PIC X(50)           VALUE
PA1215     '*  DEFINICAO CURSOR CSR01-RUECB09N    *'.
PA1215*----------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
PA1215         DECLARE  CSR01-RUECB09N  CURSOR WITH HOLD FOR
EG0517          SELECT  NCORND_GEOGR_IMOV
EG0517                , MLOGTD_IMOV_RURAL
PA1215                , MLATTD_IMOV_RURAL
PA1215                , MALTTD_IMOV_RURAL
PA1215            FROM  DB2PRD.TCORND_GEOGR_CONTR
PA1215           WHERE  CCONTR           = :RUECB09N.CCONTR
PA1215             AND  CSEQ_MATR_RURAL  = :RUECB09N.CSEQ-MATR-RURAL
EG0517             AND  NSEQ_GLEBA_IMOV  = :RUECB09N.NSEQ-GLEBA-IMOV
STEFAB             AND (CTPO_CORND_GEOGR = 2 OR
STEFAB                  CTPO_CORND_GEOGR IS NULL)
PA1215        ORDER BY  CCONTR
PA1215                , CSEQ_MATR_RURAL
EG0517                , NSEQ_GLEBA_IMOV
PA1215                , NCORND_GEOGR_IMOV
PA1215     END-EXEC.
PA1215
STEFAB
.     *----------------------------------------------------------------*
.      01  FILLER                      PIC X(50)           VALUE
.          '*  DEFINICAO CURSOR CSR02-RUECB09N    *'.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.              DECLARE  CSR02-RUECB09N  CURSOR WITH HOLD FOR
.               SELECT  NCORND_GEOGR_IMOV
.                     , MLOGTD_IMOV_RURAL
.                     , MLATTD_IMOV_RURAL
.                     , MALTTD_IMOV_RURAL
.                 FROM  DB2PRD.TCORND_GEOGR_CONTR
.                WHERE  CCONTR           = :RUECB09N.CCONTR
.                  AND  CSEQ_MATR_RURAL  = :RUECB09N.CSEQ-MATR-RURAL
.                  AND  NSEQ_GLEBA_IMOV  = :RUECB09N.NSEQ-GLEBA-IMOV
.                  AND  CTPO_CORND_GEOGR = 1
.             ORDER BY  CCONTR
.                     , CSEQ_MATR_RURAL
.                     , NSEQ_GLEBA_IMOV
.                     , NCORND_GEOGR_IMOV
.          END-EXEC.
STEFAB
BS0216*----------------------------------------------------------------*
BS0216 01  FILLER                      PIC X(50)           VALUE
BS0216     '*  DEFINICAO CURSOR CSR01-RUECV025    *'.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216         DECLARE  CSR01-RUECV025  CURSOR WITH HOLD FOR
BS0216          SELECT  DLIBRC_OPER_ORCAM,
BS0216                  RTPO_OPER_ORCAM
RE0517          ,       DFIM_EVNTO_CDULA
BS0216            FROM  DB2PRD.V01ORCAM_CDULA
BS0216           WHERE CJUNC_DEPDC          = :RUECV025.CJUNC-DEPDC
BS0216           AND   CCDULA               = :RUECV025.CCDULA
BS0216           AND   CPCELA_LIBRC         <> :WRK-ZERO
BS0216     END-EXEC.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 01  FILLER                      PIC X(50)           VALUE
BS0216     '*  DEFINICAO CURSOR CSR01-RUIAV008-JN        *'.
BS0216*----------------------------------------------------------------*
BS0216     EXEC SQL
BS0216        DECLARE  CSR01-RUIAV008-JN CURSOR FOR
BS0216        SELECT
BS0216             A.CTPO_OPER_ORCAM,
BS0216             A.DINIC_VGCIA_PCELA,
BS0216             A.DFIM_VGCIA,
BS0216             A.VPCELA_LIBRC,
BS0216             B.RTPO_OPER_ORCAM
BS0216        FROM DB2PRD.V01PCELA_ORCAM    A,
BS0216             DB2PRD.V01TPO_OPER_ORCAM B
BS0216        WHERE
BS0216             A.CPRODT_RURAL      = :RUIAV008.CPRODT-RURAL     AND
BS0216             A.CPROJ_RURAL       = :RUIAV008.CPROJ-RURAL      AND
BS0216             A.QMIN_FAIXA_PROTV  = :RUIAV008.QMIN-FAIXA-PROTV AND
BS0216             A.DINIC_VGCIA       = :RUIAV008.DINIC-VGCIA      AND
BS0216             A.CTPO_OPER_ORCAM   = B.CTPO_OPER_ORCAM
BS0216        ORDER BY
BS0216             A.DINIC_VGCIA_PCELA,
BS0216             A.CTPO_OPER_ORCAM
BS0216     END-EXEC.
BR4.11*----------------------------------------------------------------*
BR4.11*01  FILLER                      PIC X(50)           VALUE
BR4.11*    '*  DEFINICAO CURSOR CSR01-RUFIB044           *'.
BR4.11*----------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*      -DECLARE  CSR01-RUFIB044    CURSOR FOR
BR4.11*      -SELECT
BR4.11*           -CMOTVO_DCLAS_OPER,
BR4.11*           -DDCLAS_OPER_CREDT,
BR4.11*           -VPRINC_DCLAS_CONTR
BR4.11*      -FROM DB2PRD.TMOTVO_DCLAS_CONTR
BR4.11*      -WHERE
BR4.11*           -CCONTR              = :RUFIB044.CCONTR           AND
BR4.11*           -NMOTVO_DCLAS_CONTR  > :RUFIB044.NMOTVO-DCLAS-CONTR
BR4.11*      -ORDER BY
BR4.11*           -DDCLAS_OPER_CREDT
BR4.11*   -END-EXEC.
BR4.11*

050825*----------------------------------------------------------------*
.      01  FILLER                      PIC X(50)           VALUE
.          '*  DEFINICAO CURSOR CSR01-RUFIB094    *'.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.             DECLARE CSR01-RUFIB094 CURSOR  WITH HOLD  FOR
               SELECT COUTRO_TPO_CONFM
.                FROM DB2PRD.TCONTR_RURAL_OUTRO_CONFM
.               WHERE CCONTR             = :RUFIB094.CCONTR
.               ORDER BY CCONTR,
.                        COUTRO_TPO_CONFM
050825     END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER   PIC X(32) VALUE '* FIM DA WORKING RUEC8810 *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       COPY 'I#RUEC65'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC65-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                 SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           MOVE ZEROS              TO RUEC65-COD-RET.
           MOVE ZEROS              TO WRK-TAM.
           MOVE ZEROS              TO WRK-TAM2.
           MOVE ZEROS              TO WRK-CONTINUA.

           MOVE SPACES             TO WRK-B09A-SIS.
           MOVE SPACES             TO WRK-B09A-SIS2.

           PERFORM  1000-PROCESSAR.

BR1811     PERFORM UNTIL WRK-FINAL EQUAL 'S'
BR1811         PERFORM 5000-TRATAR-CONTINUACAO
BR1811         PERFORM 4000-INSERT-B09A
BR1811     END-PERFORM.
BR1811
           PERFORM  9000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAR    SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'  USING  WRK-DATA-HORA.

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

EG1215     PERFORM 2100-COORDENADA-OBRIGATORIA.
EG1215
BR1604     MOVE ZEROS          TO     WRK-CONTA-ENVIO
           IF  RUEC65-COD-RET  EQUAL  ZEROS
               IF RUEC65-FUNCAO   EQUAL  'F'
                  PERFORM 3000-OBTEM-DADOS-B099
                  PERFORM 3100-FORMATA-SIS1
                  PERFORM 4000-INSERT-B09A
                  PERFORM 4100-ENVIO
                  GO TO 1000-99-FIM
               END-IF
               IF RUEC65-FUNCAO   EQUAL  'R'
                  PERFORM 4010-SELMAX-B09A
                  PERFORM 3025-OBTEM-HINCL-B09A
                  PERFORM 4100-ENVIO
                  GO TO 1000-99-FIM
               END-IF
               IF RUEC65-FUNCAO   EQUAL  'A'
                  PERFORM 3000-OBTEM-DADOS-B099
                  PERFORM 3100-FORMATA-SIS1
                  PERFORM 4000-INSERT-B09A
                  PERFORM 4100-ENVIO
                  GO TO 1000-99-FIM
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       1000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA    SECTION.
      *---------------------------------------------------------------*

           IF  RUEC65-PGM-CHAMADOR EQUAL SPACES
               MOVE 01             TO RUEC65-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                   TO RUEC65-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC65-CCONTR       NOT   NUMERIC)
           OR (RUEC65-CCONTR       EQUAL ZEROS)
               MOVE 01             TO RUEC65-COD-RET
               MOVE 'CONTRATO INVALIDO'
                                   TO RUEC65-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF (RUEC65-NENVIO-RECOR NOT   NUMERIC)
           OR (RUEC65-NENVIO-RECOR EQUAL ZEROS)
               MOVE 01             TO RUEC65-COD-RET
               MOVE 'NENVIO-RECOR INVALIDO'
                                   TO RUEC65-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  (RUEC65-FUNCAO  NOT EQUAL 'F')
           AND (RUEC65-FUNCAO  NOT EQUAL 'R')
           AND (RUEC65-FUNCAO  NOT EQUAL 'A')
                MOVE 01             TO RUEC65-COD-RET
                MOVE 'FUNCAO DIFERENTE DE F OU R OU A'
                                    TO RUEC65-MENSAGEM
                GO  TO  2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

EG1215*------------------------------------------------------------*
EG1215 2100-COORDENADA-OBRIGATORIA     SECTION.
EG1215*------------------------------------------------------------*
EG1215
EG1215     INITIALIZE RUECWGEO-REGISTRO.
EG1215
EG1215     MOVE RUEC65-CCONTR          TO RUECWGEO-E-CCONTR.
EG1215
EG1215     MOVE 'RUEC7941'             TO WRK-RUEC7941
EG1215     CALL WRK-RUEC7941           USING RUECWGEO-COPYBOOK
EG1215                                       ERRO-AREA
EG1215                                       SQLCA.
EG1215
EG1215     IF  RUECWGEO-COD-RETORNO    NOT EQUAL ZEROS
EG1215         MOVE 'N'                TO RUECWGEO-COORD-OBRIG
EG1215     END-IF.
EG1215
EG1215*------------------------------------------------------------*
EG1215 2100-99-FIM.                    EXIT.
EG1215*------------------------------------------------------------*
EG1215
      *---------------------------------------------------------------*
       3000-OBTEM-DADOS-B099       SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB099.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB099.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  CCNPJ_ENTID_RESP,
                  CFLIAL_CNPJ_ENTID,
                  CCTRL_CNPJ_ENTID,
                  CCNPJ_PARTC,
                  CFLIAL_CNPJ_PARTC,
                  CCTRL_CNPJ_PARTC,
                  CSIT_OPER_RURAL,
                  DCDULA,
                  DVCTO_CONTR,
                  CCDULA_RECOR,
                  CTPO_INSTT_CREDT,
                  VFINAN,
                  CCATEG_PROTR_RECOR,
                  CPROG_RURAL,
                  CTPO_FONTE_RECOR,
                  CMUN_REFT_BACEN,
                  CEMPTO_BACEN,
                  CSIST_PROD,
                  VPCELA_LIBRC,
                  VPCELA_REC_PPRIO,
                  PALIQT_PROAGRO,
                  PJURO_ENCAR_FINCR,
                  CTPO_ENCAR_COMPL,
                  CTSORO_NACIO,
                  PRISCO_TSORO_NACIO,
                  PRISCO_FUNDO_CNSTT,
                  QAREA_RECOR,
                  QUND_FINCD_RURAL,
                  CUND_MEDD_FINCD,
                  QUND_PROD_PROVV,
                  CUND_MEDD_PROVV,
                  DINIC_SAFRA,
                  DFIM_SAFRA,
                  CTPO_GARNT_RECOR,
                  VRECTA_BRUTA_RECOR,
                  DMOVTO_RECOR
           INTO   :RUECB099.CCONTR-CREDT-RURAL,
                  :RUECB099.NENVIO-RECOR,
                  :RUECB099.CCNPJ-ENTID-RESP,
                  :RUECB099.CFLIAL-CNPJ-ENTID,
                  :RUECB099.CCTRL-CNPJ-ENTID,
                  :RUECB099.CCNPJ-PARTC,
                  :RUECB099.CFLIAL-CNPJ-PARTC,
                  :RUECB099.CCTRL-CNPJ-PARTC,
                  :RUECB099.CSIT-OPER-RURAL,
                  :RUECB099.DCDULA
                       :WRK-B099-DCDULA-NULL,
                  :RUECB099.DVCTO-CONTR
                       :WRK-B099-DVCTO-NULL,
                  :RUECB099.CCDULA-RECOR,
                  :RUECB099.CTPO-INSTT-CREDT,
                  :RUECB099.VFINAN
                       :WRK-B099-VFINAN,
                  :RUECB099.CCATEG-PROTR-RECOR
                       :WRK-B099-CCATEG-NULL,
                  :RUECB099.CPROG-RURAL
                       :WRK-B099-CPROG-NULL,
                  :RUECB099.CTPO-FONTE-RECOR,
                  :RUECB099.CMUN-REFT-BACEN
                       :WRK-B099-CMUN-NULL,
                  :RUECB099.CEMPTO-BACEN,
                  :RUECB099.CSIST-PROD,
                  :RUECB099.VPCELA-LIBRC
                       :WRK-B099-LIBRC-NULL,
                  :RUECB099.VPCELA-REC-PPRIO
                       :WRK-B099-REC-NULL,
                  :RUECB099.PALIQT-PROAGRO,
                  :RUECB099.PJURO-ENCAR-FINCR,
                  :RUECB099.CTPO-ENCAR-COMPL,
                  :RUECB099.CTSORO-NACIO,
                  :RUECB099.PRISCO-TSORO-NACIO,
                  :RUECB099.PRISCO-FUNDO-CNSTT,
                  :RUECB099.QAREA-RECOR,
                  :RUECB099.QUND-FINCD-RURAL
                       :WRK-B099-FINCD-NULL,
                  :RUECB099.CUND-MEDD-FINCD,
                  :RUECB099.QUND-PROD-PROVV
                       :WRK-B099-PROD-NULL,
                  :RUECB099.CUND-MEDD-PROVV,
                  :RUECB099.DINIC-SAFRA
                       :WRK-B099-DINIC-NULL,
                  :RUECB099.DFIM-SAFRA
                       :WRK-B099-DFIM-NULL,
                  :RUECB099.CTPO-GARNT-RECOR
                       :WRK-B099-GARNT-NULL,
                  :RUECB099.VRECTA-BRUTA-RECOR,
                  :RUECB099.DMOVTO-RECOR
           FROM  DB2PRD.TCONTR_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL   = :RUECB099.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB099.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3000'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 03                   TO RUEC65-COD-RET
               MOVE 'CONTRATO/NENVIO-RCOR NAO ENCONTRADO B099'
                                         TO RUEC65-MENSAGEM
               MOVE SPACES               TO RUEC65-SQLCA
               MOVE SPACES               TO RUEC65-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3001-VER-NULOS-B099
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       3001-VER-NULOS-B099            SECTION.
      *----------------------------------------------------------------*

           IF WRK-B099-DCDULA-NULL  LESS ZEROS
              MOVE SPACES TO DCDULA              OF RUECB099
           END-IF.

           IF WRK-B099-DVCTO-NULL   LESS ZEROS
              MOVE SPACES TO  DVCTO-CONTR        OF RUECB099
           END-IF.

           IF WRK-B099-VFINAN LESS ZEROS
              MOVE ZEROS  TO VFINAN              OF RUECB099
           END-IF.

           IF WRK-B099-CCATEG-NULL LESS ZEROS
              MOVE SPACES TO CCATEG-PROTR-RECOR  OF RUECB099
           END-IF.

           IF WRK-B099-CPROG-NULL    LESS ZEROS
              MOVE SPACES TO CPROG-RURAL         OF RUECB099
           END-IF.

           IF WRK-B099-CMUN-NULL LESS ZEROS
              MOVE ZEROS  TO CMUN-REFT-BACEN     OF RUECB099
           END-IF.

           IF WRK-B099-LIBRC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-LIBRC        OF RUECB099
           END-IF.

           IF WRK-B099-REC-NULL LESS ZEROS
              MOVE ZEROS  TO VPCELA-REC-PPRIO    OF RUECB099
           END-IF.

           IF WRK-B099-FINCD-NULL LESS ZEROS
              MOVE ZEROS  TO QUND-FINCD-RURAL    OF RUECB099
           END-IF.

           IF WRK-B099-PROD-NULL  LESS ZEROS
              MOVE ZEROS  TO QUND-PROD-PROVV     OF RUECB099
           END-IF.

           IF WRK-B099-DINIC-NULL LESS ZEROS
              MOVE ZEROS  TO DINIC-SAFRA         OF RUECB099
           END-IF.

           IF WRK-B099-DFIM-NULL LESS ZEROS
              MOVE ZEROS  TO DFIM-SAFRA          OF RUECB099
           END-IF.

           IF WRK-B099-GARNT-NULL LESS ZEROS
              MOVE ZEROS  TO CTPO-GARNT-RECOR    OF RUECB099
           END-IF.

      *----------------------------------------------------------------*
       3001-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3011-OPEN-CSR01-RUECB09E    SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09E.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09E.

           EXEC SQL
              OPEN      CSR01-RUECB09E-JN
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
              ( SQLWARN0       EQUAL  'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO   ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR' TO   ERR-DBD-TAB
               MOVE 'OPEN    '           TO   ERR-FUN-COMANDO
               MOVE SQLCODE              TO   ERR-SQL-CODE
               MOVE '3011'               TO   ERR-LOCAL
               MOVE SPACES               TO   ERR-SEGM
               PERFORM   9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3011-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3012-SELECT-RUECB09E SECTION.
      *---------------------------------------------------------------*
           EXEC SQL
              FETCH CSR01-RUECB09E-JN INTO
                 :RUECB09E.CCONTR-CREDT-RURAL,
                 :RUECB09E.NENVIO-RECOR,
                 :RUECB09E.CTPO-RESP-PSSOA
                      :WRK-B09E-RESP-NULL,
                 :RUECB09E.CTPO-PSSOA
                      :WRK-B09E-PSSOA-NULL,
                 :RUECB09E.CCNPJ-CPF
                      :WRK-B09E-CCNPJ-NULL,
                 :RUECB09E.CFLIAL-CNPJ
                      :WRK-B09E-CFLIAL-NULL,
                 :RUECB09E.CCTRL-CNPJ-CPF
                      :WRK-B09E-CCTRL-NULL
EG0617        ,  :RUECB040.CNIRF-IMOV-RURAL
EG0617                :WRK-B040-CNIRF-IMOV-N
EG0617        ,  :RUECB040.CCCIR
EG0617                :WRK-B040-CCCIR-N
EG0617        ,  :RUECB040.CREG-AMBTL-RURAL
EG0617                :WRK-B040-CREG-AMBTL-N
BRQ001        ,  :RUECB040.PPRESV-AMBTL-PPRIE
BRQ001                :WRK-PPRESV-AMBTL-PPRIE-N
BRQ001        ,  :RUECB040.COUTGA-AGUA-IMOV
BRQ001                :WRK-COUTGA-AGUA-IMOV-N
BRQ001        ,  :RUECB040.DVALDD-OUTGA-AGUA
BRQ001                :WRK-DVALDD-OUTGA-AGUA-N
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3012'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-CODE100-B09E
EG0617        GO TO 3012-99-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3013-VER-NULOS-B09E
           END-IF.

      *----------------------------------------------------------------*
       3012-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3013-VER-NULOS-B09E       SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09E-RESP-NULL    LESS ZEROS
              MOVE SPACES TO CTPO-RESP-PSSOA OF RUECB09E
           END-IF.

           IF WRK-B09E-PSSOA-NULL      LESS ZEROS
              MOVE ZEROS  TO CTPO-PSSOA   OF RUECB09E
           END-IF.

           IF WRK-B09E-CCNPJ-NULL LESS ZEROS
              MOVE ZEROS  TO CCNPJ-CPF    OF RUECB09E
           END-IF.

           IF WRK-B09E-CFLIAL-NULL LESS ZEROS
              MOVE ZEROS  TO CFLIAL-CNPJ  OF RUECB09E
           END-IF.

           IF WRK-B09E-CCTRL-NULL      LESS ZEROS
              MOVE ZEROS  TO CCTRL-CNPJ-CPF OF RUECB09E
           END-IF.

EG0617     IF WRK-B040-CNIRF-IMOV-N    LESS ZEROS
EG0617        MOVE SPACES              TO CNIRF-IMOV-RURAL OF RUECB040
EG0617     END-IF.
EG0617
EG0617     IF WRK-B040-CCCIR-N         LESS ZEROS
EG0617        MOVE SPACES              TO CCCIR            OF RUECB040
EG0617     END-IF.
EG0617
EG0617     IF WRK-B040-CREG-AMBTL-N    LESS ZEROS
EG0617        MOVE SPACES              TO CREG-AMBTL-RURAL OF RUECB040
EG0617     END-IF.
EG0617
BRQ001     IF WRK-PPRESV-AMBTL-PPRIE-N LESS ZEROS
BRQ001        MOVE ZEROS               TO PPRESV-AMBTL-PPRIE OF RUECB040
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-COUTGA-AGUA-IMOV-N   LESS ZEROS
BRQ001        MOVE SPACES              TO COUTGA-AGUA-IMOV   OF RUECB040
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-DVALDD-OUTGA-AGUA-N  LESS ZEROS
BRQ001        MOVE SPACES              TO DVALDD-OUTGA-AGUA OF RUECB040
BRQ001     END-IF.
BRQ001
      *----------------------------------------------------------------*
       3013-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3014-CLOSE-CSR01-RUECB09E    SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              CLOSE     CSR01-RUECB09E-JN
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
              ( SQLWARN0       EQUAL    'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3014'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3014-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3021-OPEN-CSR01-RUECB09D    SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09D.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09D.

           EXEC SQL
              OPEN      CSR01-RUECB09D
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
              ( SQLWARN0       EQUAL  'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR' TO   ERR-DBD-TAB
               MOVE 'OPEN    '           TO   ERR-FUN-COMANDO
               MOVE SQLCODE              TO   ERR-SQL-CODE
               MOVE '3021'               TO   ERR-LOCAL
               MOVE SPACES               TO   ERR-SEGM
               PERFORM   9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3021-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3022-SELECT-RUECB09D SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB09D INTO
                 :RUECB09D.CCONTR-CREDT-RURAL,
                 :RUECB09D.NENVIO-RECOR,
                 :RUECB09D.NPCELA-CONTR,
                 :RUECB09D.DVCTO-PCELA-CONTR
                      :WRK-B09D-DVCTO-NULL,
                 :RUECB09D.VPCELA-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO  RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO  RUEC65-MENSAGEM
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3022'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-CODE100-B09D
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3023-VER-NULOS-B09D
           END-IF.

      *----------------------------------------------------------------*
       3022-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3023-VER-NULOS-B09D       SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09D-DVCTO-NULL   LESS ZEROS
              MOVE SPACES TO DVCTO-PCELA-CONTR  OF RUECB09D
           END-IF.

      *----------------------------------------------------------------*
       3023-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3024-CLOSE-CSR01-RUECB09D    SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              CLOSE     CSR01-RUECB09D
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
              ( SQLWARN0       EQUAL    'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TPCELA_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3024'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3024-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3025-OBTEM-HINCL-B09A       SECTION.
      *---------------------------------------------------------------*

           IF (SQLCODE       EQUAL  -305)
               MOVE 05                   TO RUEC65-COD-RET
               MOVE 'NAO EXISTE CONTRATO/NENVIO PARA SER REENVIADO'
                                         TO RUEC65-MENSAGEM
               MOVE SPACES               TO RUEC65-SQLCA
               MOVE SPACES               TO RUEC65-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.

           MOVE NREG-MSGEM-RECOR      TO WRK-MAX-NREG.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  NREG_MSGEM_RECOR,
                  CSIT_MSGEM,
                  HINCL,
                  WCONTD_MSGEM_RURAL
           INTO   :RUECB09A.CCONTR-CREDT-RURAL,
                  :RUECB09A.NENVIO-RECOR,
                  :RUECB09A.NREG-MSGEM-RECOR,
                  :RUECB09A.CSIT-MSGEM
                       :WRK-B09A-CSIT-NULL,
                  :RUECB09A.HINCL
                       :WRK-B09A-HINCL-NULL,
                  :RUECB09A.WCONTD-MSGEM-RURAL
           FROM  DB2PRD.TENVIO_RECOR_TEMPR
           WHERE CCONTR_CREDT_RURAL   = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB09A.NENVIO-RECOR
           AND   NREG_MSGEM_RECOR     = :RUECB09A.NREG-MSGEM-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3025'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 06                   TO RUEC65-COD-RET
               MOVE 'CONTRATO/NENVIO-RCOR NAO ENCONTRADO B09A'
                                         TO RUEC65-MENSAGEM
               MOVE SPACES               TO RUEC65-SQLCA
               MOVE SPACES               TO RUEC65-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3026-VER-NULOS-B09A
               MOVE HINCL OF RUECB09A TO WRK-TIMESTAMP-B09A
           END-IF.

      *---------------------------------------------------------------*
       3025-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
      * VER NULIDADE DOS CAMPOS                                        *
      *----------------------------------------------------------------*
       3026-VER-NULOS-B09A            SECTION.
      *----------------------------------------------------------------*

           IF WRK-B09A-CSIT-NULL    LESS ZEROS
              MOVE ZEROS  TO CSIT-MSGEM          OF RUECB09A
           END-IF.

           IF WRK-B09A-HINCL-NULL   LESS ZEROS
              MOVE SPACES TO  HINCL              OF RUECB09A
           END-IF.

      *----------------------------------------------------------------*
       3026-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3027-OBTEM-DADOS-B002       SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR OF RUFIB002.

           EXEC SQL
           SELECT CCDULA,
                  CJUNC_DEPDC,
                  CTPO_CDULA_RECOR
EG0615          , CORIGE_REC
EG0615          , CFNALD_RURAL
EG0615          , CPRODT_RURAL
BS0216          , CCTRO_CUSTO
BS0216          , CUND_MEDD_PROVV
BS0216          , QUND_PROD_PROVV
BS0216          , CMUN_RURAL
BS0216          , CATVDD_RURAL
201609          , IFNULL ( CTPO_BNEFC_RURAL , 0 )
EG1216          , CDECLR_AGCLT_FAMLR
BRQ001          , CCETFC_ORGNC_EMPTO
BRQ001          , EURL_CETFC_ORGNC
BRQ001          , CRSTRB_EMPTO_RURAL
BRQ001          , EURL_RSTRB_EMPTO
BRQ001          , PENERG_RENOV_EMPTO
WP1021          , CTPO_SOLO_AGCLT
WP1021          , CCICLO_CTVDA
WP1021          , CMANUT_SOLO
STE001          , PREDC_JURO_AMBTL
STE001          , CINDCD_CONFM_FLORS
STE001          , CINDCD_CONFM_IBAMA
STE001          , CINDCD_CONFM_CSVAO
STEFA1          , VALUE(CINDCD_CONFM_RURAL, '9')
STEFA1          , COUTRO_TPO_CONFM
STEFA1          , VALUE(CCNPJ_INSTC_EXIGB, 0)
STE002          , VALUE(QUND_ANMAL, 0)
           INTO   :RUFIB002.CCDULA,
                  :RUFIB002.CJUNC-DEPDC,
                  :RUFIB002.CTPO-CDULA-RECOR
EG0615          , :RUFIB002.CORIGE-REC   :WRK-N-CORIGE-REC
EG0615          , :RUFIB002.CFNALD-RURAL :WRK-N-CFNALD-RURAL
EG0615          , :RUFIB002.CPRODT-RURAL :WRK-N-CPRODT-RURAL
BS0216          , :RUFIB002.CCTRO-CUSTO
BS0216          , :RUFIB002.CUND-MEDD-PROVV :WRK-N-MEDD-PROVV
BS0216          , :RUFIB002.QUND-PROD-PROVV
BS0216          , :RUFIB002.CMUN-RURAL   :WRK-N-CMUN-RURAL
BS0216          , :RUFIB002.CATVDD-RURAL :WRK-N-CATVDD-RURAL
201609          , :RUFIB002.CTPO-BNEFC-RURAL
EG1216          , :RUFIB002.CDECLR-AGCLT-FAMLR :WRK-V07-NUMERO-DAP-N
BRQ001          , :RUFIB002.CCETFC-ORGNC-EMPTO :WRK-CCETFC-ORGNC-EMPTO-N
BRQ001          , :RUFIB002.EURL-CETFC-ORGNC   :WRK-EURL-CETFC-ORGNC-N
BRQ001          , :RUFIB002.CRSTRB-EMPTO-RURAL :WRK-CRSTRB-EMPTO-RURAL-N
BRQ001          , :RUFIB002.EURL-RSTRB-EMPTO   :WRK-EURL-RSTRB-EMPTO-N
BRQ001          , :RUFIB002.PENERG-RENOV-EMPTO :WRK-PENERG-RENOV-EMPTO-N
WP1021          , :RUFIB002.CTPO-SOLO-AGCLT
WP1021               :WRK-CTPO-SOLO-AGCLT-NULL
WP1021          , :RUFIB002.CCICLO-CTVDA
WP1021               :WRK-CCICLO-CTVDA-NULL
WP1021          , :RUFIB002.CMANUT-SOLO
WP1021               :WRK-CMANUT-SOLO-NULL
STE001          , :RUFIB002.PREDC-JURO-AMBTL
STE001               :WRK-PREDC-JURO-AMBTL-NULL
STE001          , :RUFIB002.CINDCD-CONFM-FLORS
STE001               :WRK-CINDCD-CONFM-FLORS-NULL
STE001          , :RUFIB002.CINDCD-CONFM-IBAMA
STE001               :WRK-CINDCD-CONFM-IBAMA-NULL
STE001          , :RUFIB002.CINDCD-CONFM-CSVAO
STE001               :WRK-CINDCD-CONFM-CSVAO-NULL
STEFA1          , :RUFIB002.CINDCD-CONFM-RURAL
STEFA1          , :RUFIB002.COUTRO-TPO-CONFM
STEFA1               :WRK-COUTRO-NULL
STEFA1          , :RUFIB002.CCNPJ-INSTC-EXIGB
STE002          , :RUFIB002.QUND-ANMAL
           FROM  DB2PRD.CONTRATO_RURAL
           WHERE CCONTR               = :RUFIB002.CCONTR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'     TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3027'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 07                   TO RUEC65-COD-RET
               MOVE 'CONTRATO NAO ENCONTRADO B002'
                                         TO RUEC65-MENSAGEM
               MOVE SPACES               TO RUEC65-SQLCA
               MOVE SPACES               TO RUEC65-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

EG0615     IF  WRK-N-CORIGE-REC        NOT EQUAL ZEROS
EG0615         MOVE  ZEROS             TO  CORIGE-REC   OF RUFIB002
EG0615     END-IF.
EG0615
EG0615     IF  WRK-N-CFNALD-RURAL      NOT EQUAL ZEROS
EG0615         MOVE  ZEROS             TO  CFNALD-RURAL OF RUFIB002
EG0615     END-IF.
EG0615
EG0615     IF  WRK-N-CPRODT-RURAL      NOT EQUAL ZEROS
EG0615         MOVE  ZEROS             TO  CPRODT-RURAL OF RUFIB002
EG0615     END-IF.
EG0615
BS0216     IF  WRK-N-MEDD-PROVV        NOT EQUAL ZEROS
BS0216         MOVE  ZEROS          TO  CUND-MEDD-PROVV OF RUFIB002
BS0216     END-IF.
BS0216
BS0216     IF  WRK-N-CMUN-RURAL        NOT EQUAL ZEROS
BS0216         MOVE  ZEROS          TO  CMUN-RURAL      OF RUFIB002
BS0216     END-IF.
BS0216
BS0216     IF  WRK-N-CATVDD-RURAL      NOT EQUAL ZEROS
BS0216         MOVE  ZEROS          TO  CATVDD-RURAL    OF RUFIB002
BS0216     END-IF.
EG1216
EG1216     IF  WRK-V07-NUMERO-DAP-N    NOT EQUAL ZEROS
EG1216         MOVE SPACES             TO CDECLR-AGCLT-FAMLR OF RUFIB002
EG1216     END-IF.
EG1216
BRQ001     IF WRK-CCETFC-ORGNC-EMPTO-N NOT EQUAL ZEROS
BRQ001        MOVE SPACES              TO CCETFC-ORGNC-EMPTO OF RUFIB002
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-EURL-CETFC-ORGNC-N   NOT EQUAL ZEROS
BRQ001        MOVE ZEROS               TO EURL-CETFC-ORGNC-LEN
BRQ001                                                       OF RUFIB002
BRQ001        MOVE SPACES              TO EURL-CETFC-ORGNC-TEXT
BRQ001                                                       OF RUFIB002
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-CRSTRB-EMPTO-RURAL-N NOT EQUAL ZEROS
BRQ001        MOVE SPACES              TO CRSTRB-EMPTO-RURAL OF RUFIB002
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-EURL-RSTRB-EMPTO-N   NOT EQUAL ZEROS
BRQ001        MOVE ZEROS               TO EURL-RSTRB-EMPTO-LEN
BRQ001                                                       OF RUFIB002
BRQ001        MOVE SPACES              TO EURL-RSTRB-EMPTO-TEXT
BRQ001                                                       OF RUFIB002
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-PENERG-RENOV-EMPTO-N NOT EQUAL ZEROS
BRQ001        MOVE ZEROS               TO PENERG-RENOV-EMPTO OF RUFIB002
BRQ001     END-IF.
BRQ001
WP1021     IF  WRK-CTPO-SOLO-AGCLT-NULL      EQUAL  -1
WP1021         MOVE  ZEROS             TO CTPO-SOLO-AGCLT    OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CCICLO-CTVDA-NULL         EQUAL  -1
WP1021         MOVE  ZEROS             TO CCICLO-CTVDA       OF RUFIB002
WP1021     END-IF.
WP1021
WP1021     IF  WRK-CMANUT-SOLO-NULL          EQUAL  -1
WP1021         MOVE  ZEROS             TO CMANUT-SOLO        OF RUFIB002
WP1021     END-IF.
WP1021
STE001     IF  WRK-PREDC-JURO-AMBTL-NULL EQUAL -1
STE001         MOVE  ZEROS             TO PREDC-JURO-AMBTL   OF RUFIB002
STE001     END-IF.
STE001
STE001     IF  WRK-CINDCD-CONFM-FLORS-NULL EQUAL -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-FLORS OF RUFIB002
STE001     END-IF.
STE001
STE001     IF  WRK-CINDCD-CONFM-IBAMA-NULL EQUAL -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-IBAMA OF RUFIB002
STE001     END-IF.
STE001
STE001     IF  WRK-CINDCD-CONFM-CSVAO-NULL EQUAL -1
STE001         MOVE  ZEROS             TO CINDCD-CONFM-CSVAO OF RUFIB002
STE001     END-IF.

STEFA1     IF  WRK-COUTRO-NULL         NOT EQUAL ZEROS
STEFA1*       XMOVE  ZEROS             TO COUTRO-TPO-CONFM
STEFA1*                               XOF RUFIB002
STEFA1         CONTINUE
STEFA1     END-IF.
STE001
BR1218     MOVE CCONTR      OF RUFIB002 TO CCONTR      OF RUECB007
BR1218     MOVE CJUNC-DEPDC OF RUFIB002 TO CJUNC-DEPDC OF RUECB007
BR1218     MOVE CCDULA      OF RUFIB002 TO CCDULA      OF RUECB007
BR1218
BR1218     EXEC SQL
BR1218       SELECT VEGF
BR1218       INTO   :RUECB007.VEGF     :WRK-B07-VEGF-N
BR1218       FROM   DB2PRD.CEDULA_TEMPR
BR1218       WHERE  CCONTR             = :RUECB007.CCONTR
BR1218       AND    CJUNC_DEPDC        = :RUECB007.CJUNC-DEPDC
BR1218       AND    CCDULA             = :RUECB007.CCDULA
BR1218     END-EXEC.
BR1218
BR1218     IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
BR1218        (SQLWARN0      EQUAL 'W')
BR1218         MOVE 99                   TO RUEC65-COD-RET
BR1218         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR1218         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR1218         MOVE 'CEDULA_TEMPR'       TO ERR-DBD-TAB
BR1218         MOVE 'SELECT'             TO ERR-FUN-COMANDO
BR1218         MOVE SQLCODE              TO ERR-SQL-CODE
BR1218         MOVE '327B'               TO ERR-LOCAL
BR1218         MOVE SPACES               TO ERR-SEGM
BR1218         PERFORM 9000-ROTINA-FIM
BR1218     END-IF.
BR1218
BR1218     IF (SQLCODE                 EQUAL  +100)
BR1218         MOVE ZEROS              TO VEGF               OF RUECB007
BR1218     END-IF.
BR1218
BR1218     IF  WRK-B07-VEGF-N          NOT EQUAL ZEROS
BR1218         MOVE ZEROS              TO VEGF               OF RUECB007
BR1218     END-IF.
BR1218
      *---------------------------------------------------------------*
       3027-99-FIM.                EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3028-OPEN-CSR01-RUECB09F    SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09F.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09F.

           EXEC SQL
              OPEN      CSR01-RUECB09F
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
              ( SQLWARN0       EQUAL    'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO   ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR' TO   ERR-DBD-TAB
               MOVE 'OPEN    '           TO   ERR-FUN-COMANDO
               MOVE SQLCODE              TO   ERR-SQL-CODE
               MOVE '3028'               TO   ERR-LOCAL
               MOVE SPACES               TO   ERR-SEGM
               PERFORM   9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3028-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3029-SELECT-RUECB09F SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              FETCH CSR01-RUECB09F INTO
                 :RUECB09F.CCONTR-CREDT-RURAL,
                 :RUECB09F.NENVIO-RECOR,
                 :RUECB09F.NPRODT-RURAL-RECOR,
                 :RUECB09F.CPRODT-RURAL
                      :WRK-B09F-CPRODT-NULL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3029'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-CODE100-B09F
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 3030-VER-NULOS-B09F
           END-IF.

      *----------------------------------------------------------------*
       3029-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3030-VER-NULOS-B09F       SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09F-CPRODT-NULL  LESS ZEROS
              MOVE ZEROS  TO CPRODT-RURAL    OF RUECB09F
           END-IF.

      *----------------------------------------------------------------*
       3030-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3031-CLOSE-CSR01-RUECB09F    SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              CLOSE     CSR01-RUECB09F
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
              ( SQLWARN0       EQUAL    'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TPRODT_RURAL_RECOR' TO ERR-DBD-TAB
               MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '3031'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3031-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3032-SELECT-B006            SECTION.
      *---------------------------------------------------------------*

           MOVE CPRODT-RURAL OF RUECB09F TO CPRODT-RURAL OF RURCB006.

           EXEC SQL
           SELECT CPRODT_RURAL,
                  CPRODT_BACEN
           INTO   :RURCB006.CPRODT-RURAL,
                  :RURCB006.CPRODT-BACEN
           FROM DB2PRD.PRODUTO_RURAL
           WHERE CPRODT_RURAL       = :RURCB006.CPRODT-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'PRODUTO_RURAL'      TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '3032'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF (SQLCODE       EQUAL  +100)
               MOVE 08                   TO RUEC65-COD-RET
               MOVE 'PRODUTO NAO ENCONTRADO B096'
                                         TO RUEC65-MENSAGEM
               MOVE SPACES               TO RUEC65-SQLCA
               MOVE SPACES               TO RUEC65-ERRO-AREA
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3032-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-FORMATA-SIS1           SECTION.
      *---------------------------------------------------------------*

           MOVE 'COR0001'          TO RUEC66-CODMSG.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE WRK-FORMA-AAAA TO WRK-NUMCTRLIF-ANO.
           MOVE WRK-FORMA-MM   TO WRK-NUMCTRLIF-MES.
           MOVE WRK-FORMA-DD   TO WRK-NUMCTRLIF-DIA.

           MOVE WRK-NUMCTRLIF      TO RUEC66-NUMCTRLIF.

      * FORMATAR CNPJ
           MOVE CCNPJ-ENTID-RESP   TO WRK-COD9-NUM.
           MOVE CFLIAL-CNPJ-ENTID  TO WRK-COD5-NUM.
           MOVE CCTRL-CNPJ-ENTID   TO WRK-COD3-NUM.
           MOVE WRK-COD9-NUM8      TO WRK-EDIT-NCNPJ.
           MOVE WRK-COD5-NUM4      TO WRK-EDIT-FCNPJ.
           MOVE WRK-COD3-NUM2      TO WRK-EDIT-CCNPJ.
050825     MOVE WRK-EDIT-CNPJ      TO RUEC66-CNPJ-ENT-RESPONS.

           MOVE ALL '*'            TO RUEC66-CNPJ-PART.

           IF RUEC65-FUNCAO  EQUAL  'A'
              PERFORM 4320-SELECT-B09B
              MOVE CREFT-RECOR OF RUECB09B  TO RUEC66-NUMREF-BCCOROR
           ELSE
              MOVE ALL '*'            TO RUEC66-NUMREF-BCCOROR
           END-IF.

           PERFORM 3027-OBTEM-DADOS-B002.
CAT408****-MOVE CTPO-CDULA-RECOR   OF RUFIB002
CAT408****-                        TO WRK-COD3-CHAR.
CAT408****-MOVE WRK-COD3-CHAR1     TO RUEC66-TP-CEDLCOR.

STE001     MOVE  PREDC-JURO-AMBTL   OF RUFIB002
STE001           TO  WRK-S9-3V99
STE001     MOVE  '*'
STE001           TO  RUEC66-PERC-BONUS-CAR
050825**  -CAMPOS DE TIPO DE CONFORMIDADE SERA TRATADOS EM OUTRO PONTO
050825**  -MOVE  CINDCD-CONFM-FLORS OF RUFIB002
050825**        -TO  WRK-S9-1
STE001*   -MOVE  CINDCD-CONFM-FLORS OF RUFIB002
STE001*         -TO  WRK-S9-1
STE001*   -MOVE  WRK-9-1
STE001*         -TO  WRK-9-1-R
STE001*   -MOVE  WRK-9-1-X
STE001*         -TO  RUEC66-IND-RCONFDD-FLORPUBL
STE001*   -MOVE  CINDCD-CONFM-IBAMA OF RUFIB002
STE001*         -TO  WRK-S9-1
STE001*   -MOVE  WRK-9-1
STE001*         -TO  WRK-9-1-R
STE001*   -MOVE  WRK-9-1-X
STE001*         -TO  RUEC66-IND-RCONFDD-IBAMA
STE001*   -MOVE  CINDCD-CONFM-CSVAO OF RUFIB002
STE001*         -TO  WRK-S9-1
STE001*   -MOVE  WRK-9-1
STE001*         -TO  WRK-9-1-R
STE001*   -MOVE  WRK-9-1-X
STE001*         -TO  RUEC66-IND-RCONFDD-UC


      * FORMATAR DATA   PARA AAAAMMDD
           MOVE DCDULA  OF RUECB099  TO WRK-DATA.
           MOVE WRK-DD             TO WRK-EDIT-DD.
           MOVE WRK-MM             TO WRK-EDIT-MM.
           MOVE WRK-AAAA           TO WRK-EDIT-AAAA.
           MOVE WRK-EDIT-DATA-R    TO RUEC66-DTEMS.

           MOVE DVCTO-CONTR  OF RUECB099  TO WRK-DATA.
           MOVE WRK-DD             TO WRK-EDIT-DD.
           MOVE WRK-MM             TO WRK-EDIT-MM.
           MOVE WRK-AAAA           TO WRK-EDIT-AAAA.
           MOVE WRK-EDIT-DATA-R    TO RUEC66-DTVENC.

BR4.11     MOVE ALL '*'            TO RUEC66-CBASE-LEGAL-PRO.

           MOVE CCDULA-RECOR       TO RUEC66-NUMCEDL-CRED-RURALIF.

           MOVE CTPO-INSTT-CREDT   TO RUEC66-TP-INSTNTO-CRED.

CAT408****-MOVE ALL '*'            TO RUEC66-CNPJ-BASEIF-SUBEMPRT.

CAT408****-MOVE ALL '*'            TO RUEC66-NUMREF-BCCOR-SUBEMPRT.

      * FORMATA VALOR
           MOVE VFINAN  OF RUECB099  TO WRK-VLR15.
           MOVE WRK-VLR15-CHAR     TO WRK-VLR15-PARC.
           MOVE WRK-VLR1           TO RUEC66-VLR-TOTOP.

           MOVE CCATEG-PROTR-RECOR TO RUEC66-TP-CATG-EMIT.

           MOVE RUEC66-COR0001-1  TO WRK-B09A-SIS.
           ADD  LENGTH OF RUEC66-COR0001-1 TO WRK-TAM.

           COMPUTE WRK-POSICAO = WRK-TAM + WRK-1.

AC0513     MOVE RUEC66-COR0001-1  TO WRK-B09A-SIS2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-1 TO WRK-TAM2.
AC0513
AC0513     COMPUTE WRK-POSICAO2 = WRK-TAM2 + WRK-1.

      * FORMATA GRUPO EMIT
           MOVE 'N'              TO  WRK-CODE100-B09E.
           MOVE 1     TO WRK-IND1.
           PERFORM 3110-FORMATA-GRUPO-EMIT
                   UNTIL WRK-IND1     GREATER  WRK-999.

      * FORMATA GRUPO DESTCFIN
           MOVE 1     TO WRK-IND1.
           PERFORM 3120-FORMATA-GRUPO-DESTCFIN
211025*           -UNTIL WRK-IND1 GREATER  WRK-50.
211025             UNTIL WRK-IND1 GREATER  WRK-60.

EG0615* FORMATA SUBPROGRAMA
EG0615*   -PERFORM 3155-FORMATA-SUBPROG.
EG0615*

BR4.11* FORMATAR GRUPO LIBERACAO
BR4.11*   -MOVE 1 TO WRK-IND2.
BR4.11*   -PERFORM 3129-FORMATA-GRUPO-LIBRC
BR4.11*           -UNTIL WRK-IND2 GREATER  WRK-30.

      * FORMATA PRODUTIVIDADE / ANO MES PLANTIO E COLHEITA
BS0216     IF CCTRO-CUSTO OF RUFIB002  EQUAL 'MASS'
BS0216         PERFORM 3132-ACESSA-FUMAGEIRA
BS0216     ELSE
BS0216         PERFORM 3131-ACESSA-RUECV025
BS0216     END-IF

      * CONCLUI E CONCATENA GRUPO COR0001-15
201609     IF WRK-CONTINUA  EQUAL  ZEROS
201609        MOVE RUEC66-COR0001-15 TO
201609             WRK-B09A-SIS(WRK-POSICAO:
201609                          LENGTH OF RUEC66-COR0001-15)
201609        ADD LENGTH OF RUEC66-COR0001-15 TO WRK-TAM
201609        ADD LENGTH OF RUEC66-COR0001-15 TO WRK-POSICAO
201609     END-IF.
201609
201609     IF WRK-CONTINUA  GREATER  ZEROS
201609        MOVE RUEC66-COR0001-15 TO
201609             WRK-B09A-SIS2(WRK-POSICAO2:
201609                           LENGTH OF RUEC66-COR0001-15)
201609        ADD LENGTH OF RUEC66-COR0001-15 TO WRK-TAM2
201609        ADD LENGTH OF RUEC66-COR0001-15 TO WRK-POSICAO2
201609     END-IF.
201609
BR4.11* FORMATA GRUPO DE DESCLASSIFICACAO
BR4.11****-PERFORM 3500-FORMATA-GRUPO-DCLAS
201609
201609* CONCLUI E CONCATENA GRUPO COR0001-16
BR4.11*   -IF WRK-CONTINUA  EQUAL  ZEROS
BR4.11*      -MOVE RUEC66-COR0001-16 TO
BR4.11*            WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                         LENGTH OF RUEC66-COR0001-16)
BR4.11*      -ADD LENGTH OF RUEC66-COR0001-16 TO WRK-TAM
BR4.11*      -ADD LENGTH OF RUEC66-COR0001-16 TO WRK-POSICAO
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -IF WRK-CONTINUA  GREATER  ZEROS
BR4.11*      -MOVE RUEC66-COR0001-16 TO
BR4.11*            WRK-B09A-SIS2(WRK-POSICAO2:
BR4.11*                          LENGTH OF RUEC66-COR0001-16)
BR4.11*      -ADD LENGTH OF RUEC66-COR0001-16 TO WRK-TAM2
BR4.11*      -ADD LENGTH OF RUEC66-COR0001-16 TO WRK-POSICAO2
BR4.11*   -END-IF.
BR4.11*
QT1120* FORMATA GRUPO DE DESCLASSIFICACAO
QT1120     MOVE ALL '*'              TO RUEC66-GP-COR0001-DESCLASS
QT1120
QT1120
QT1120*    CONCATENA GRUPO COR0001-16
QT1120     IF WRK-CONTINUA  EQUAL  ZEROS
QT1120        MOVE RUEC66-COR0001-16 TO
QT1120             WRK-B09A-SIS(WRK-POSICAO:
QT1120                          LENGTH OF RUEC66-COR0001-16)
QT1120        ADD LENGTH OF RUEC66-COR0001-16 TO WRK-TAM
QT1120        ADD LENGTH OF RUEC66-COR0001-16 TO WRK-POSICAO
QT1120     END-IF.
QT1120
QT1120     IF WRK-CONTINUA  GREATER  ZEROS
QT1120        MOVE RUEC66-COR0001-16 TO
QT1120             WRK-B09A-SIS2(WRK-POSICAO2:
QT1120                           LENGTH OF RUEC66-COR0001-16)
QT1120        ADD LENGTH OF RUEC66-COR0001-16 TO WRK-TAM2
QT1120        ADD LENGTH OF RUEC66-COR0001-16 TO WRK-POSICAO2
QT1120     END-IF.
QT1120*
BRQ001* FORMATA CODIGO STN EQUALIZACAO
BRQ001     MOVE ALL '*'              TO RUEC66-COD-STN-EQULZC
BRQ001
BRQ001* FORMATA GRUPO DE CERTIFICACAO ORGANICA
BRQ001     IF (CCETFC-ORGNC-EMPTO    OF RUFIB002
BRQ001                               NOT EQUAL SPACES AND LOW-VALUES)
BRQ001                               AND
BRQ001        (EURL-CETFC-ORGNC-TEXT OF RUFIB002
BRQ001                               NOT EQUAL SPACES AND LOW-VALUES)
BRQ001         MOVE CCETFC-ORGNC-EMPTO OF RUFIB002
BRQ001                               TO RUEC66-CODCERTIFCORGNCOEMPNMNT
BRQ001         MOVE EURL-CETFC-ORGNC-TEXT OF RUFIB002(1:255)
BRQ001                               TO RUEC66-URLCERTIFCORGNCOEMPNMNT
BRQ001     ELSE
BRQ001         MOVE ALL '*'          TO RUEC66-GP-COR0001-CERTIFCOR
BRQ001     END-IF.
BRQ001
BRQ001* FORMATA GRUPO RASTREABILIDADE EMPREENDIMENTO
BRQ001     IF (CRSTRB-EMPTO-RURAL    OF RUFIB002
BRQ001                               NOT EQUAL SPACES AND LOW-VALUES)
BRQ001                               AND
BRQ001        (EURL-RSTRB-EMPTO-TEXT OF RUFIB002
BRQ001                               NOT EQUAL SPACES AND LOW-VALUES)
BRQ001         MOVE CRSTRB-EMPTO-RURAL OF RUFIB002
BRQ001                               TO RUEC66-CODRASTLDDEMPNMNT
BRQ001         MOVE EURL-RSTRB-EMPTO-TEXT OF RUFIB002(1:255)
BRQ001                               TO RUEC66-URLCODRASTLDDEMPNMNT
BRQ001     ELSE
BRQ001         MOVE ALL '*'          TO RUEC66-GP-COR0001-RASTLDDEM
BRQ001     END-IF.
BRQ001
BRQ001* FORMATA PERCENTUAL ENERGIA RENOVAVEL EMPREENDIMENTO
BRQ001     IF  PENERG-RENOV-EMPTO    OF RUFIB002
BRQ001                               GREATER ZEROS
BRQ001         MOVE PENERG-RENOV-EMPTO OF RUFIB002
BRQ001                               TO WRK-PPRESV-CS
BRQ001         MOVE WRK-PPRESV-SS    TO RUEC66-PERCENERRENOVVLEMPNMNT
BRQ001     ELSE
BRQ001         MOVE ALL '*'          TO RUEC66-COR0001-20
BRQ001     END-IF.
BRQ001
BRQ001* CONCATENA GRUPO COR0001-17
BRQ001     IF WRK-CONTINUA  EQUAL  ZEROS
BRQ001        MOVE RUEC66-COR0001-17 TO
BRQ001             WRK-B09A-SIS(WRK-POSICAO:
BRQ001                          LENGTH OF RUEC66-COR0001-17)
BRQ001        ADD LENGTH OF RUEC66-COR0001-17 TO WRK-TAM
BRQ001        ADD LENGTH OF RUEC66-COR0001-17 TO WRK-POSICAO
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-CONTINUA  GREATER  ZEROS
BRQ001        MOVE RUEC66-COR0001-17 TO
BRQ001             WRK-B09A-SIS2(WRK-POSICAO2:
BRQ001                           LENGTH OF RUEC66-COR0001-17)
BRQ001        ADD LENGTH OF RUEC66-COR0001-17 TO WRK-TAM2
BRQ001        ADD LENGTH OF RUEC66-COR0001-17 TO WRK-POSICAO2
BRQ001     END-IF.
BRQ001*
BRQ001* CONCATENA GRUPO COR0001-18
BRQ001     IF WRK-CONTINUA  EQUAL  ZEROS
BRQ001        MOVE RUEC66-COR0001-18 TO
BRQ001             WRK-B09A-SIS(WRK-POSICAO:
BRQ001                          LENGTH OF RUEC66-COR0001-18)
BRQ001        ADD LENGTH OF RUEC66-COR0001-18 TO WRK-TAM
BRQ001        ADD LENGTH OF RUEC66-COR0001-18 TO WRK-POSICAO
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-CONTINUA  GREATER  ZEROS
BRQ001        MOVE RUEC66-COR0001-18 TO
BRQ001             WRK-B09A-SIS2(WRK-POSICAO2:
BRQ001                           LENGTH OF RUEC66-COR0001-18)
BRQ001        ADD LENGTH OF RUEC66-COR0001-18 TO WRK-TAM2
BRQ001        ADD LENGTH OF RUEC66-COR0001-18 TO WRK-POSICAO2
BRQ001     END-IF.
BRQ001*
BRQ001* CONCATENA GRUPO COR0001-19
BRQ001     IF WRK-CONTINUA  EQUAL  ZEROS
BRQ001        MOVE RUEC66-COR0001-19 TO
BRQ001             WRK-B09A-SIS(WRK-POSICAO:
BRQ001                          LENGTH OF RUEC66-COR0001-19)
BRQ001        ADD LENGTH OF RUEC66-COR0001-19 TO WRK-TAM
BRQ001        ADD LENGTH OF RUEC66-COR0001-19 TO WRK-POSICAO
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-CONTINUA  GREATER  ZEROS
BRQ001        MOVE RUEC66-COR0001-19 TO
BRQ001             WRK-B09A-SIS2(WRK-POSICAO2:
BRQ001                           LENGTH OF RUEC66-COR0001-19)
BRQ001        ADD LENGTH OF RUEC66-COR0001-19 TO WRK-TAM2
BRQ001        ADD LENGTH OF RUEC66-COR0001-19 TO WRK-POSICAO2
BRQ001     END-IF.
BRQ001*
BRQ001* CONCATENA GRUPO COR0001-20
BRQ001     IF WRK-CONTINUA  EQUAL  ZEROS
BRQ001        MOVE RUEC66-COR0001-20 TO
BRQ001             WRK-B09A-SIS(WRK-POSICAO:
BRQ001                          LENGTH OF RUEC66-COR0001-20)
BRQ001        ADD LENGTH OF RUEC66-COR0001-20 TO WRK-TAM
BRQ001        ADD LENGTH OF RUEC66-COR0001-20 TO WRK-POSICAO
BRQ001     END-IF.
BRQ001
BRQ001     IF WRK-CONTINUA  GREATER  ZEROS
BRQ001        MOVE RUEC66-COR0001-20 TO
BRQ001             WRK-B09A-SIS2(WRK-POSICAO2:
BRQ001                           LENGTH OF RUEC66-COR0001-20)
BRQ001        ADD LENGTH OF RUEC66-COR0001-20 TO WRK-TAM2
BRQ001        ADD LENGTH OF RUEC66-COR0001-20 TO WRK-POSICAO2
BRQ001     END-IF.
BRQ001*
WP1021     IF (WRK-CTPO-SOLO-AGCLT-NULL  LESS ZEROS)  OR
WP1021        (WRK-CCICLO-CTVDA-NULL     LESS ZEROS)  OR
WP1021        (WRK-CMANUT-SOLO-NULL      LESS ZEROS)
WP1021         MOVE  ALL '*'       TO  RUEC66-COR0001-21
WP1021     ELSE
WP1021         MOVE  CTPO-SOLO-AGCLT OF RUFIB002
WP1021                             TO   WRK-CTPO-SOLO-AGCLT
WP1021         MOVE  CCICLO-CTVDA    OF RUFIB002
WP1021                             TO   WRK-CCICLO-CTVDA
WP1021         MOVE  CMANUT-SOLO     OF RUFIB002
WP1021                             TO   WRK-CMANUT-SOLO
WP1021         MOVE  WRK-CTPO-SOLO-AGCLT-X  TO RUEC66-TIPO-SOLO
WP1021         MOVE  WRK-CCICLO-CTVDA-X     TO RUEC66-CICLO-CULTURA
WP1021         MOVE  WRK-CMANUT-SOLO-X      TO RUEC66-TIPO-MANEJO
WP1021     END-IF.
WP1021
WP1021* CONCATENA GRUPO COR0001-21
WP1021     IF WRK-CONTINUA  EQUAL  ZEROS
WP1021        MOVE RUEC66-COR0001-21 TO
WP1021             WRK-B09A-SIS(WRK-POSICAO:
WP1021                          LENGTH OF RUEC66-COR0001-21)
WP1021        ADD LENGTH OF RUEC66-COR0001-21 TO WRK-TAM
WP1021        ADD LENGTH OF RUEC66-COR0001-21 TO WRK-POSICAO
WP1021     END-IF.
WP1021
WP1021     IF WRK-CONTINUA  GREATER  ZEROS
WP1021        MOVE RUEC66-COR0001-21 TO
WP1021             WRK-B09A-SIS2(WRK-POSICAO2:
WP1021                           LENGTH OF RUEC66-COR0001-21)
WP1021        ADD LENGTH OF RUEC66-COR0001-21 TO WRK-TAM2
WP1021        ADD LENGTH OF RUEC66-COR0001-21 TO WRK-POSICAO2
WP1021     END-IF.
WP1021*
STE001***  PercJurosEncargoFinanc
STE001
.          IF PREDC-JURO-AMBTL   OF RUFIB002
.                                     GREATER ZEROS
.             MOVE  PREDC-JURO-AMBTL   OF RUFIB002
.                                     TO  WRK-VLR5
.             MOVE  WRK-VLR5-CHAR     TO  WRK-VLR5-CHAR-2
.             MOVE  WRK-VLR5-CHAR-1   TO  RUEC66-PERC-BONUS-CAR
.          ELSE
.             MOVE  ALL '*'           TO  RUEC66-PERC-BONUS-CAR
.          END-IF.
.
050825**   CAMPOS DE TIPO DE CONFORMIDADE SERA TRATADOS EM OUTRO PONTO
.     *   -IF CINDCD-CONFM-FLORS OF RUFIB002
.     *                          -NOT LESS ZEROS
.     *      -MOVE  CINDCD-CONFM-FLORS OF RUFIB002
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 3105-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO  RUEC66-IND-RCONFDD-IBAMA
.     *   -ELSE
.     *      -MOVE  ALL '*'           TO  RUEC66-IND-RCONFDD-IBAMA
.     *   -END-IF.
.     *   -IF CINDCD-CONFM-IBAMA OF RUFIB002
.     *                          -NOT LESS ZEROS
.     *      -MOVE  CINDCD-CONFM-IBAMA OF RUFIB002
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 3105-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO  RUEC66-IND-RCONFDD-UC
.     *   -ELSE
.     *      -MOVE  ALL '*'           TO  RUEC66-IND-RCONFDD-UC
.     *   -END-IF.
.
.
.     *   -IF CINDCD-CONFM-CSVAO OF RUFIB002
.     *                          -NOT LESS ZEROS
.     *      -MOVE  CINDCD-CONFM-CSVAO OF RUFIB002
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 3105-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO  RUEC66-IND-RCONFDD-FLORPUBL
.     *   -ELSE
.     *      -MOVE  ALL '*'           TO  RUEC66-IND-RCONFDD-FLORPUBL
.     *   -END-IF.
.
STEFA1*   -IF CINDCD-CONFM-RURAL OF RUFIB002
.     *                          -NOT LESS ZEROS
.     *      -MOVE  CINDCD-CONFM-RURAL OF RUFIB002
.     *        -TO WRK-X-1-B
.     *      -MOVE WRK-9-1-B
.     *        -TO  WRK-INDICADOR-TELA
.     *      -PERFORM 3105-TRATAR-DE-PARA
.     *      -MOVE  WRK-INDICADOR-TELA-X
.     *                              -TO  RUEC66-IND-RCONFDD-IM-RURAL
.     *   -ELSE
.     *      -MOVE  ALL '*'           TO  RUEC66-IND-RCONFDD-IM-RURAL
STEFA1*   -END-IF.
.
STEFA1*   -IF  WRK-COUTRO-NULL   EQUAL ZEROS
.     *
.     *       -PERFORM 3104-SELECT-OUTROS
.     *
.     *       -IF CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.     *                         -NOT NUMERIC
.     *          -MOVE ALL '*'   TO  RUEC66-TP-CONFDD-OTR
.     *       -ELSE
.     *          -MOVE CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.     *                          TO  WRK-S9-2A
.     *          -MOVE WRK-9-2A  TO  RUEC66-TP-CONFDD-OTR
.     *       -END-IF
.     *
.     *   -ELSE
.     *
.     *       -MOVE  ALL '*'     TO  RUEC66-TP-CONFDD-OTR
.     *
STEFA1*   -END-IF.
.
STEFA1     MOVE ALL '*'          TO  RUEC66-CNPJ-BASE-EXIGLDD
.
050825     MOVE ALL '*'          TO  RUEC66-PERC-BONUS-SUS

STE001     MOVE RUEC66-COR0001-21A TO
STE001                             WRK-B09A-SIS (WRK-POSICAO:
STE001                             LENGTH OF RUEC66-COR0001-21A)
STE001     ADD LENGTH OF RUEC66-COR0001-21A TO WRK-TAM
STE001     ADD LENGTH OF RUEC66-COR0001-21A TO WRK-POSICAO

050825* FORMATAR TIPO DE CONFIRMIDADE
.          PERFORM 3160-FORMATAR-TPCONFDD.
.          IF WRK-TEM-TPCONFDD      EQUAL 'N'
.             MOVE ALL '*'             TO RUEC66-GP-COR0001-TPCONFDD
.             MOVE RUEC66-COR0001-21B  TO
.                  WRK-B09A-SIS (WRK-POSICAO:
.                               LENGTH OF RUEC66-COR0001-21B)
.             ADD LENGTH OF RUEC66-COR0001-21B TO WRK-TAM
.             ADD LENGTH OF RUEC66-COR0001-21B TO WRK-POSICAO
.          END-IF
.
.     * FORMATAR GRUPO LIBERACAO
.     *   -MOVE 1 TO WRK-IND2.
.          PERFORM 3165-FORMATA-GRUPO-LIBRC
.     *           -UNTIL WRK-IND2 GREATER  WRK-30.
.
.     * FORMATAR GRUPO COOPERADO
.          MOVE ALL '*'             TO RUEC66-GP-COR0001-COOPERD.
.          MOVE RUEC66-COR0001-21D  TO
.               WRK-B09A-SIS (WRK-POSICAO:
.                            LENGTH OF RUEC66-COR0001-21D).
.          ADD LENGTH OF RUEC66-COR0001-21D TO WRK-TAM.
050825     ADD LENGTH OF RUEC66-COR0001-21D TO WRK-POSICAO.

      * FORMATA DATA MOVIMENTO
           PERFORM 3150-FORMATA-DTMOVTO

RLOS       IF RUEC66-IDENTC-GLEBA(1:1) NOT EQUAL '*'
.             MOVE '******'            TO WRK-B09A-SIS(WRK-NEW-POS:6)
.             MOVE '******'            TO WRK-B09A-SIS2(WRK-NEW-POS2:6)
ONEDA ***  **---------------------------------------
ONEDA ***  -> NAO ENVIA AREA QDO EXIGE COORDENADAS GEODESICAS
ONEDA         MOVE  '***********'  TO RUEC66-AREA-X
ONEDA ***  **---------------------------------------
RLOS       END-IF
           .

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

STEFA1*---------------------------------------------------------------*
STEFA1*3104-SELECT-OUTROS              SECTION.
STEFA1*---------------------------------------------------------------*
STEFA1*
STEFA1*   -MOVE SPACES                 TO  CRURWPGI-E-ROUTRO-CONFM-BACEN
STEFA1*                                    CRURWPGI-E-CSIT-REG-RURAL
STEFA1*   -MOVE 1                      TO  CRURWPGI-E-MAX-OCORR
STEFA1*   -MOVE 'I'                    TO  CRURWPGI-INDICADOR-PAGINACAO
STEFA1*   -MOVE COUTRO-TPO-CONFM       OF  RUFIB002
STEFA1*                                TO  WRK-S9-5A
STEFA1*   -MOVE WRK-9-5A               TO  CRURWPGI-E-COUTRO-TPO-CONFM
STEFA1*
STEFA1*   -CALL  WRK-CRUR240G          USING WRK-AREA-CRUR240G.
STEFA1*
STEFA1*---------------------------------------------------------------*
STEFA1*3104-99-FIM.                    EXIT.
STEFA1*---------------------------------------------------------------*

STE001*----------------------------------------------------------------*
.     *3105-TRATAR-DE-PARA             SECTION.
.     *----------------------------------------------------------------*
.     *   -EVALUATE WRK-INDICADOR-TELA
.     *       -WHEN 1
.     *          -MOVE 'S'             TO WRK-INDICADOR-TELA-X
.     *       -WHEN 2
.     *          -MOVE 'N'             TO WRK-INDICADOR-TELA-X
.     *       -WHEN OTHER
.     *          -MOVE '*'             TO WRK-INDICADOR-TELA-X
.     *   -END-EVALUATE.
.     *----------------------------------------------------------------*
.     *3105-99-FIM.  EXIT.
STE001*----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3110-FORMATA-GRUPO-EMIT  SECTION.
      *---------------------------------------------------------------*

           MOVE 'E'             TO CTPO-RESP-PSSOA.
           PERFORM 3011-OPEN-CSR01-RUECB09E.
           PERFORM VARYING  WRK-IND-B09E     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09E  EQUAL 'S'
                   PERFORM 3012-SELECT-RUECB09E
                   PERFORM 3111-FORMATAR-ITEMB09E
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09E.
           MOVE ZEROS TO WRK-IND-B09E.
           ADD  999   TO WRK-IND1.
           PERFORM 3014-CLOSE-CSR01-RUECB09E.

AC0513     MOVE ALL '*'           TO RUEC66-GP-COR0001-EMIT.
AC0513     MOVE RUEC66-COR0001-2  TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                        LENGTH OF RUEC66-COR0001-2).
AC0513     ADD  LENGTH OF RUEC66-COR0001-2 TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-2 TO WRK-POSICAO2.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3111-FORMATAR-ITEMB09E    SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 3111-99-FIM
           END-IF.

      * GRUPO EMIT TAB B09E
           IF  CTPO-PSSOA  EQUAL  1
               MOVE  'F'           TO  RUEC66-TP-PESSOA-EMIT
050825         MOVE SPACES             TO  RUEC66-CNPJ-CPF-EMIT
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
050825         MOVE WRK-COD9-NUM       TO  WRK-AUX-9-N
.              MOVE WRK-AUX-9-X        TO  RUEC66-CNPJ-CPF-EMIT(1:9)
.              MOVE WRK-COD3-NUM2      TO  WRK-AUX-2-N
.              MOVE WRK-AUX-2-X        TO  RUEC66-CNPJ-CPF-EMIT(10:2)
050825         MOVE '   '              TO  RUEC66-CNPJ-CPF-EMIT(12:3)
           ELSE
               MOVE  'J'           TO  RUEC66-TP-PESSOA-EMIT
               MOVE CCNPJ-CPF          TO  WRK-COD9-NUM
               MOVE CFLIAL-CNPJ        TO  WRK-COD5-NUM
               MOVE CCTRL-CNPJ-CPF     TO  WRK-COD3-NUM
               MOVE WRK-COD9-NUM8      TO  WRK-EDIT-NCNPJ
               MOVE WRK-COD5-NUM4      TO  WRK-EDIT-FCNPJ
               MOVE WRK-COD3-NUM2      TO  WRK-EDIT-CCNPJ
050825         MOVE WRK-EDIT-CNPJ      TO  RUEC66-CNPJ-CPF-EMIT
           END-IF

EG1216     IF  CDECLR-AGCLT-FAMLR      OF RUFIB002
EG1216                                 EQUAL SPACES
050825         MOVE ALL '*'            TO RUEC66-CAF-BENF
EG1216     ELSE
EG1216         MOVE CDECLR-AGCLT-FAMLR OF RUFIB002
050825                                 TO RUEC66-CAF-BENF
EG1216     END-IF.
EG1216
201609     IF  CTPO-BNEFC-RURAL        OF RUFIB002
201609                                 EQUAL ZEROS
201609         MOVE  ALL '*'           TO RUEC66-TPO-BENF
201609     ELSE
201609         PERFORM 3540-OBTER-BNEFC-RURAL
201609     END-IF.
201609
           MOVE RUEC66-COR0001-2  TO
                WRK-B09A-SIS(WRK-POSICAO: LENGTH OF RUEC66-COR0001-2).
           ADD  LENGTH OF RUEC66-COR0001-2 TO WRK-TAM.
           ADD  LENGTH OF RUEC66-COR0001-2 TO WRK-POSICAO.

           ADD  1     TO WRK-IND1.

      *----------------------------------------------------------------*
       3111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3120-FORMATA-GRUPO-DESTCFIN    SECTION.
      *---------------------------------------------------------------*

      *****************************************************************
211025***** O GRUPO DESTCFIN PODE SER FORMATADO DE 1 ATE 60 VEZES
      ***** NO LEGADO O GRUPO E FORMATADO APENAS 1 VEZ
      *****************************************************************

      *** FORMATAR GRUPO DESTCFIN
           MOVE RUEC66-COR0001-3  TO
                WRK-B09A-SIS(WRK-POSICAO: LENGTH OF RUEC66-COR0001-3).
           ADD  LENGTH OF RUEC66-COR0001-3 TO WRK-TAM.
           ADD  LENGTH OF RUEC66-COR0001-3 TO WRK-POSICAO.

AC0513     MOVE RUEC66-COR0001-3  TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                        LENGTH OF RUEC66-COR0001-3).
AC0513     ADD  LENGTH OF RUEC66-COR0001-3 TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-3 TO WRK-POSICAO2.

      *** FORMATAR GRUPO RENEGC
           MOVE 1 TO WRK-IND2.
           PERFORM 3121-FORMATA-GRUPO-RENEGC
                   UNTIL WRK-IND2 GREATER  WRK-999.

      *** CONTINUA FORMATAR GRUPO DESTCFIN

           IF  CPROG-RURAL         OF RUECB099  EQUAL  SPACES
               MOVE  ALL '*'       TO RUEC66-CODPROG-LINHA-CRED
           ELSE
               MOVE  CPROG-RURAL   OF RUECB099
                                   TO RUEC66-CODPROG-LINHA-CRED
           END-IF.

050825* FORMATA SUBPROGRAMA
.          PERFORM 3155-FORMATA-SUBPROG.
050825

           MOVE  CTPO-FONTE-RECOR  TO RUEC66-TP-FNTE-REC.
           MOVE  CMUN-REFT-BACEN   OF RUECB099
                                   TO RUEC66-COD-MUNIC.

           MOVE RUEC66-COR0001-5  TO
                WRK-B09A-SIS(WRK-POSICAO: LENGTH OF RUEC66-COR0001-5).
           ADD  LENGTH OF RUEC66-COR0001-5 TO WRK-TAM.
           ADD  LENGTH OF RUEC66-COR0001-5 TO WRK-POSICAO.

RLOS       COMPUTE WRK-NEW-POS      = (WRK-POSICAO - 6)

AC0513     MOVE RUEC66-COR0001-5  TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                        LENGTH OF RUEC66-COR0001-5).
AC0513     ADD  LENGTH OF RUEC66-COR0001-5 TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-5 TO WRK-POSICAO2.

RLOS       COMPUTE WRK-NEW-POS2     = (WRK-POSICAO2 - 6)

      *** FORMATAR GRUPO GLEBA
           MOVE 1 TO WRK-IND2.
STEFAB*   -MOVE 1 TO WRK-IND2-E.
EG1215     IF  RUECWGEO-COORD-OBRIG    EQUAL 'N'
BR0119     OR  WRK-BENEF               EQUAL '002' OR '009'
BR0419         MOVE 'S'                TO WRK-FINAL
EG1215         PERFORM 3122-FORMATA-GRUPO-GLEBA-AST
                   UNTIL WRK-IND2   GREATER  WRK-999
211025**      EXCLUSAO DE CAMPO - SICOR 5.11
STEFA1*       -PERFORM 4122-FORMATA-GRUPO-GLEBA-AST
STEFA1*           -UNTIL WRK-IND2-E GREATER  WRK-999
EG1215     ELSE
EG1215         MOVE ZEROS              TO ACU-GLEBA
EG1215         PERFORM 3137-FORMATA-GRUPO-GLEBA
EG1215             UNTIL WRK-IND2 GREATER  WRK-999
STEFAB         MOVE ZEROS              TO ACU-GLEBA
EG1215     END-IF.

      *** FORMATAR CAMPO CONSCD
           MOVE 1 TO WRK-IND2.
           PERFORM 3124-FORMATA-GRUPO-CONSCD
                   UNTIL WRK-IND2 GREATER  WRK-999.

      *** CONTINUA FORMATAR GRUPO DESTCFIN
           MOVE  CEMPTO-BACEN      TO RUEC66-COD-EMPNMNT.

      * FORMATAR CSIST-PROD
WIP001     MOVE  CSIST-PROD        TO RUEC66-COD-SIST-PRODC.
           IF  RUEC66-COD-SIST-PRODC(10:02) EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO RUEC66-COD-SIST-PRODC(10:02)
           END-IF
PRI499*   XMOVE '00'               TO RUEC66-COD-SIST-PRODC(10:02).
STE001     IF  RUEC66-COD-SIST-PRODC(12:02) EQUAL SPACES OR LOW-VALUES
STE001         MOVE '00'           TO RUEC66-COD-SIST-PRODC(12:02)
STE001     END-IF
      * FORMATAR VALOR
           MOVE  VPCELA-LIBRC  OF RUECB099
                                   TO WRK-VLR15.
           MOVE  WRK-VLR15-CHAR    TO WRK-VLR15-PARC.
           MOVE  WRK-VLR1          TO RUEC66-VLR-PARCL-CRED.
EG0615
EG0615     IF  VPCELA-REC-PPRIO OF RUECB099 EQUAL ZEROS
EG0615         MOVE  ALL '*'       TO RUEC66-VLR-PARCL-REC-PROP
EG0615     ELSE
EG0615         MOVE  VPCELA-REC-PPRIO  OF RUECB099
EG0615                                 TO WRK-VLR15
EG0615         MOVE  WRK-VLR15-CHAR    TO WRK-VLR15-PARC
EG0615         MOVE  WRK-VLR1          TO RUEC66-VLR-PARCL-REC-PROP
EG0615     END-IF.
EG0615
BS0000     MOVE  ALL '*'           TO RUEC66-VLR-PARCL-SERVICOS.

           MOVE  ALL '*'           TO RUEC66-ALQ-PROAGRO.

           MOVE  PJURO-ENCAR-FINCR TO WRK-JUROS.
           MOVE  WRK-JUR5-NUM      TO RUEC66-PERC-JUROS-ENCARFIN.

           IF  CTPO-ENCAR-COMPL  OF RUECB099  EQUAL  ZEROS
WMM   *       XMOVE '*'             TO RUEC66-TP-ENCARG-FIN-COMPL-X
WMM            MOVE 9               TO RUEC66-TP-ENCARG-FIN-COMPL
           ELSE
               MOVE  CTPO-ENCAR-COMPL  OF RUECB099
                                    TO RUEC66-TP-ENCARG-FIN-COMPL
           END-IF.
           MOVE  ALL '*'            TO RUEC66-PERC-RSC-STN.
           MOVE  ALL '*'            TO RUEC66-PERC-RSC-FUND-CONSCL.
4.13       MOVE  ALL '*'            TO RUEC66-PERC-CST-EFT-TOTAL.

           IF  QAREA-RECOR  EQUAL  ZEROS
               MOVE  ALL '*'        TO RUEC66-AREA-X
           ELSE
               MOVE QAREA-RECOR     TO WRK-QAREA-COMS
               MOVE WRK-QAREA-SEMS   TO RUEC66-AREA-N
           END-IF.

STE002     IF  (QUND-FINCD-RURAL OF RUECB099  EQUAL  ZEROS AND
STE002          QUND-ANMAL       OF RUFIB002  EQUAL  ZEROS)
               MOVE ALL '*'         TO RUEC66-QTD-ITEM-FINCD-X
           ELSE
STEFA4         PERFORM 3129-FORMATAR-CAMPO-QTD
           END-IF.

BR1218     PERFORM 4500-CHECAR-EXIGE-AREA
BR1218
BR1218     IF  QUND-FINCD-RURAL OF RUECB099 NOT EQUAL ZEROS
BR1218     AND WRK-EXIGE-AREA          EQUAL 'S'
BR1218     AND VEGF OF RUECB007        NOT EQUAL ZEROS
BR1218         MOVE  VEGF OF RUECB007  TO WRK-QAREA-COMS
BR1218         MOVE  WRK-QAREA-SEMS    TO RUEC66-AREA-N
BR1218         PERFORM 4600-ZERA-EGF-RUFIB002
BR1218     END-IF.
ONEDA ***  **---------------------------------------
ONEDA ***  -> NAO ENVIA AREA QDO EXIGE COORDENADAS GEODESICAS
ONEDA ***  **---------------------------------------
ONEDA      IF RUEC66-IDENTC-GLEBA(1:1) NOT EQUAL '*'
ONEDA         MOVE  '***********'  TO RUEC66-AREA-X
ONEDA      END-IF.
BR1218
AC0913     IF  QUND-PROD-PROVV   OF RUECB099  EQUAL  ZEROS
AC0913         MOVE ALL '*'         TO RUEC66-QTD-PRV-PRODC-X
AC0913     ELSE
BR4.12        IF (RUEC66-COD-EMPNMNT (02:01) EQUAL '2')  AND
BR4.12           (WRK-BENEF                  NOT EQUAL
BR4.12                                          '002' AND '009') AND
BR4.12           (RUEC66-COD-EMPNMNT (03:02) EQUAL '01' OR '26'
BR4.12                                          OR '31' OR '35' OR '37'
BR4.12                                          OR '38' OR '39' OR '55'
BR4.12                                          OR '60' OR '63' OR '65'
BR4.12                                          OR '67' OR '73' OR '75'
BR4.12                                          OR '77')
BR4.12
BR4.12            MOVE  QUND-PROD-PROVV OF RUECB099  TO WRK-QTD14
BR4.12            MOVE  WRK-QTD14-NUM   TO RUEC66-QTD-PRV-PRODC
BR4.12        ELSE
BR4.12           MOVE ALL '*'          TO RUEC66-QTD-PRV-PRODC-X
BR4.12        END-IF
AC0913     END-IF.

           MOVE RUEC66-COR0001-9  TO
                WRK-B09A-SIS(WRK-POSICAO: LENGTH OF RUEC66-COR0001-9).
           ADD  LENGTH OF RUEC66-COR0001-9  TO WRK-TAM.
           ADD  LENGTH OF RUEC66-COR0001-9  TO WRK-POSICAO.

AC0513     MOVE RUEC66-COR0001-9  TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                        LENGTH OF RUEC66-COR0001-9).
AC0513     ADD  LENGTH OF RUEC66-COR0001-9  TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-9  TO WRK-POSICAO2.

      *** FORMATA GRUPO PROP
BR1217     INITIALIZE WRK-TABELA.

           MOVE 'N'              TO  WRK-CODE100-B09E.
           MOVE 1 TO WRK-IND2.
           PERFORM 3125-FORMATA-GRUPO-PROP
AC0813*            UNTIL WRK-IND2 GREATER  WRK-10.

      *** CONTINUA FORMATAR GRUPO DESTCFIN

           MOVE  CTPO-GARNT-RECOR  TO WRK-AUX3-NUM.
           MOVE  WRK-AUX3-NUM1     TO RUEC66-TP-GAR-EMPNMNT.

           MOVE  ALL '*'             TO RUEC66-CNPJBASE-INST-MUTNTE.
           MOVE  ALL '*'             TO RUEC66-NUMREF-BCCOR-INVESTMT.
           MOVE  ALL '*'             TO RUEC66-VLR-PARCL-INVESTMT.

           IF  VRECTA-BRUTA-RECOR  OF RUECB099  EQUAL ZEROS
               MOVE  ALL '*'         TO RUEC66-VLR-RECBRUTA-ESPRDEMP
           ELSE
BR4.12       IF (RUEC66-COD-EMPNMNT (02:01) EQUAL '2')  AND
BR4.12          (WRK-BENEF                  NOT EQUAL
BR4.12                                      '002' AND '009') AND
BR4.12          (RUEC66-COD-EMPNMNT (03:02) EQUAL '01' OR '26'
BR4.12                                         OR '31' OR '35' OR '37'
BR4.12                                         OR '38' OR '39' OR '55'
BR4.12                                         OR '60' OR '63' OR '65'
BR4.12                                         OR '67' OR '73' OR '75'
BR4.12                                         OR '77')
BR4.12           MOVE  VRECTA-BRUTA-RECOR  OF RUECB099
BR4.12                                     TO WRK-VLR17
BR4.12           MOVE  WRK-VLR17-CHAR      TO WRK-VLR17-PARC
BR4.12           MOVE  WRK-VLR2            TO
BR4.12                                RUEC66-VLR-RECBRUTA-ESPRDEMP
BR4.12       ELSE
BR4.12           MOVE  ALL '*'    TO RUEC66-VLR-RECBRUTA-ESPRDEMP
             END-IF
           END-IF.

           MOVE RUEC66-COR0001-11 TO
                WRK-B09A-SIS(WRK-POSICAO: LENGTH OF RUEC66-COR0001-11).
           ADD  LENGTH OF RUEC66-COR0001-11 TO WRK-TAM.
           ADD  LENGTH OF RUEC66-COR0001-11 TO WRK-POSICAO.

AC0513     MOVE RUEC66-COR0001-11 TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                        LENGTH OF RUEC66-COR0001-11).
AC0513     ADD  LENGTH OF RUEC66-COR0001-11 TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-11 TO WRK-POSICAO2.

      *** FORMATA GRUPO PARCL
           MOVE 'N'              TO  WRK-CODE100-B09D.
           MOVE 1 TO WRK-IND2.
           PERFORM 3127-FORMATA-GRUPO-PARCL
                   UNTIL WRK-IND2 GREATER  WRK-999.

211025*   -ADD 51  TO WRK-IND1.
211025     ADD 61  TO WRK-IND1.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3121-FORMATA-GRUPO-RENEGC SECTION.
      *---------------------------------------------------------------*

           IF WRK-IND2  EQUAL  1
              MOVE  ALL '*'          TO RUEC66-NUMREF-BCCOR-RENEGC
              MOVE  ALL '*'          TO RUEC66-VLR-RENEGC
EG0617        MOVE  ALL '*'          TO RUEC66-BASE-LEGAL-RENEGC
              MOVE RUEC66-COR0001-4  TO
                   WRK-B09A-SIS(WRK-POSICAO:
                                          LENGTH OF RUEC66-COR0001-4)
              ADD LENGTH OF RUEC66-COR0001-4 TO WRK-TAM
              ADD LENGTH OF RUEC66-COR0001-4 TO WRK-POSICAO
              ADD 999 TO WRK-IND2
AC0513        MOVE RUEC66-COR0001-4  TO
AC0513             WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                           LENGTH OF RUEC66-COR0001-4)
AC0513        ADD LENGTH OF RUEC66-COR0001-4 TO WRK-TAM2
AC0513        ADD LENGTH OF RUEC66-COR0001-4 TO WRK-POSICAO2
           ELSE
              ADD 999 TO WRK-IND2
           END-IF.

      *----------------------------------------------------------------*
       3121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
EG1215 3122-FORMATA-GRUPO-GLEBA-AST  SECTION.
      *---------------------------------------------------------------*

           IF WRK-IND2  EQUAL  1
              MOVE  ALL '*'          TO RUEC66-IDENTC-GLEBA
211025        MOVE  ALL '*'          TO RUEC66-AREA-NCULTVD
              MOVE RUEC66-COR0001-6  TO
                   WRK-B09A-SIS(WRK-POSICAO:
                                        LENGTH OF RUEC66-COR0001-6)
              ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM
              ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO
              MOVE 1 TO WRK-IND3
AC0513        MOVE RUEC66-COR0001-6  TO
AC0513             WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                                 LENGTH OF RUEC66-COR0001-6)
AC0513        ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM2
AC0513        ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO2
EG1215        PERFORM 3123-FORMATA-GRUPO-PTPOLG-AST
                   UNTIL WRK-IND3 GREATER  WRK-999
              ADD 999 TO WRK-IND2
           ELSE
              ADD 999 TO WRK-IND2
           END-IF.

      *----------------------------------------------------------------*
       3122-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
EG1215 3123-FORMATA-GRUPO-PTPOLG-AST  SECTION.
      *---------------------------------------------------------------*

           IF WRK-IND3  EQUAL  1
211025**      EXCLUSAO DE CAMPO - SICOR 5.11
      *      -MOVE  ALL '*'          TO RUEC66-LAT-PONTO
AC0813*      -MOVE  ALL '*'          TO RUEC66-LONG-PONTO
AC0813*      -MOVE  ALL '*'          TO RUEC66-ALTT-PONTO
      *      -MOVE RUEC66-COR0001-7  TO
      *           -WRK-B09A-SIS(WRK-POSICAO:
      *                                -LENGTH OF RUEC66-COR0001-7)
      *      -ADD LENGTH OF RUEC66-COR0001-7 TO WRK-TAM
      *      -ADD LENGTH OF RUEC66-COR0001-7 TO WRK-POSICAO
              ADD 999 TO WRK-IND3
AC0513*      -MOVE RUEC66-COR0001-7  TO
AC0513*            WRK-B09A-SIS2(WRK-POSICAO2:
AC0513*                                -LENGTH OF RUEC66-COR0001-7)
AC0513*      -ADD LENGTH OF RUEC66-COR0001-7 TO WRK-TAM2
AC0513*      -ADD LENGTH OF RUEC66-COR0001-7 TO WRK-POSICAO2

BR4.11*      -MOVE  ALL '*'          TO RUEC66-COD-MUN-GLEB
BS0216
211025*      -PERFORM 3139-FORMATA-GRUPO-AREA-N-CULT
BS0216
211025**      NOVO CAMPO - SICOR 5.11
211025        MOVE  ALL '*'          TO RUEC66-WKT-PART
211025
211025        MOVE RUEC66-COR0001-7  TO WRK-B09A-SIS(WRK-POSICAO:
211025                                  LENGTH OF RUEC66-COR0001-7)
211025        ADD LENGTH OF RUEC66-COR0001-7  TO WRK-TAM
211025        ADD LENGTH OF RUEC66-COR0001-7  TO WRK-POSICAO
211025
211025        MOVE RUEC66-COR0001-7  TO WRK-B09A-SIS2(WRK-POSICAO2:
211025                                  LENGTH OF RUEC66-COR0001-7)
211025        ADD LENGTH OF RUEC66-COR0001-7  TO WRK-TAM2
211025        ADD LENGTH OF RUEC66-COR0001-7  TO WRK-POSICAO2
           ELSE
              ADD 999 TO WRK-IND3
           END-IF.

      *----------------------------------------------------------------*
       3123-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3124-FORMATA-GRUPO-CONSCD        SECTION.
      *---------------------------------------------------------------*

           PERFORM 3028-OPEN-CSR01-RUECB09F.
           PERFORM VARYING  WRK-IND-B09F     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09F  EQUAL 'S'
                   PERFORM 3029-SELECT-RUECB09F
                   PERFORM 3130-FORMATAR-ITEMB09F
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09F.
           MOVE ZEROS TO WRK-IND-B09F.
           ADD  999   TO WRK-IND2.
           PERFORM 3031-CLOSE-CSR01-RUECB09F.

AC0513     MOVE ALL '*' TO RUEC66-COD-PRODT-CONSCD.
AC0513     MOVE RUEC66-COR0001-8  TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                                LENGTH OF RUEC66-COR0001-8).
AC0513     ADD LENGTH OF RUEC66-COR0001-8  TO WRK-TAM2.
AC0513     ADD LENGTH OF RUEC66-COR0001-8  TO WRK-POSICAO2.

      *----------------------------------------------------------------*
       3124-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3125-FORMATA-GRUPO-PROP          SECTION.
      *---------------------------------------------------------------*

           MOVE 'P'             TO CTPO-RESP-PSSOA.
           PERFORM 3011-OPEN-CSR01-RUECB09E.
BR1217     MOVE ZEROS      TO   WRK-IND-TAB
EG0717
EG0717     PERFORM 3012-SELECT-RUECB09E.
EG0717
EG0717     IF  WRK-CODE100-B09E         EQUAL 'S'
EG0717***      SE NAO HOUVER PROPRIETARIOS - GRAVA REGISTRO UNICO COM
EG0717***      EMITENTE
EG0717         PERFORM 3014-CLOSE-CSR01-RUECB09E
EG0717         MOVE 'E'                 TO CTPO-RESP-PSSOA
EG0717         PERFORM 3011-OPEN-CSR01-RUECB09E
EG0717         PERFORM 3012-SELECT-RUECB09E
BR1217         PERFORM 3140-FORMATAR-PROP-TAB
BR1217         MOVE 1                      TO WRK-IND-B09E
BR1217         PERFORM 3126-FORMATAR-ITEMB09E
BR1217         MOVE  ZEROS                 TO WRK-IND-TAB
EG0717     END-IF.
EG0717
BR1217     PERFORM VARYING  WRK-IND-B09E     FROM 1 BY 1
BR1217             UNTIL WRK-CODE100-B09E  EQUAL 'S'
BR1217             PERFORM 3140-FORMATAR-PROP-TAB
BR1217             PERFORM 3012-SELECT-RUECB09E
BR1217     END-PERFORM.

           PERFORM VARYING  WRK-IND-B09E  FROM    1 BY 1
                   UNTIL WRK-IND-B09E     GREATER WRK-IND-TAB OR
                   WRK-IND-B09E           GREATER 100
                   PERFORM 3126-FORMATAR-ITEMB09E
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09E.
           MOVE ZEROS TO WRK-IND-B09E.
BR1218     ADD  101   TO WRK-IND2.
           PERFORM 3014-CLOSE-CSR01-RUECB09E.

AC0513     MOVE ALL '*'      TO  RUEC66-GP-COR0001-PROP-F.
AC0513     MOVE RUEC66-COR0001-10-F TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                       LENGTH OF RUEC66-COR0001-10-F).
AC0513     ADD  LENGTH OF RUEC66-COR0001-10-F TO WRK-TAM2.
AC0513     ADD  LENGTH OF RUEC66-COR0001-10-F TO WRK-POSICAO2.

      *----------------------------------------------------------------*
       3125-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3126-FORMATAR-ITEMB09E    SECTION.
      *---------------------------------------------------------------*

      * GRUPO PROP TAB B09E
BR1218     IF WRK-IND-B09E  LESS 100
              IF  WRK-CTPO-PSSOA(WRK-IND-B09E)  EQUAL  1
                  MOVE  'F'           TO  RUEC66-TP-PESSOA-PROPT-F
                  MOVE WRK-CCNPJ-CPF(WRK-IND-B09E)
                                      TO  WRK-COD9-CPF
                  MOVE WRK-CCTRL-CNPJ-CPF(WRK-IND-B09E)
                                      TO  WRK-COD2-NUM-S
                  MOVE WRK-COD9-CPF-9 TO  WRK-EDIT-NCPF
                  MOVE WRK-COD2-NUM   TO  WRK-EDIT-CCPF
                  MOVE WRK-EDIT-CPF   TO  RUEC66-CPFPROPT
EG0617
EG0617            IF WRK-CNIRF-IMOV(WRK-IND-B09E) EQUAL SPACES
050825               MOVE ALL '*'      TO RUEC66-CIB-F
EG0617            ELSE
EG0617               MOVE WRK-CNIRF-IMOV(WRK-IND-B09E)(2:8)
050825                                 TO RUEC66-CIB-F
EG0617            END-IF
EG0617
EG0617            IF WRK-CCCIR(WRK-IND-B09E)      EQUAL SPACES
EG0617               MOVE ALL '*'      TO RUEC66-CCIR-F
EG0617            ELSE
EG0617               MOVE WRK-CCCIR(WRK-IND-B09E)
EG0617                                 TO RUEC66-CCIR-F
EG0617            END-IF
EG0617
EG0617            IF WRK-CREG-AMBTL(WRK-IND-B09E) EQUAL SPACES
EG0617               MOVE ALL '*'      TO RUEC66-NRO-CAR-F
EG0617            ELSE
EG0617               MOVE WRK-CREG-AMBTL(WRK-IND-B09E)
EG0617                                 TO RUEC66-NRO-CAR-F
EG0617            END-IF
EG0617
BRQ001            IF WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E) EQUAL SPACES
BRQ001               MOVE ALL '*'      TO RUEC66-NRO-OUT-F
BRQ001            ELSE
BRQ001               MOVE WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E)
BRQ001                                 TO RUEC66-NRO-OUT-F
BRQ001            END-IF
BRQ001
BRQ001            IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E) EQUAL ZEROS
BRQ001               MOVE ALL '*'      TO RUEC66-PERC-PRESC-F
BRQ001            ELSE
BRQ001               MOVE WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E)
BRQ001                                 TO RUEC66-PERC-PRESC-F-N
BRQ001            END-IF
BRQ001
                  MOVE RUEC66-COR0001-10-F TO
                       WRK-B09A-SIS(WRK-POSICAO:
                                        LENGTH OF RUEC66-COR0001-10-F)
                  ADD  LENGTH OF RUEC66-COR0001-10-F TO WRK-TAM
                  ADD  LENGTH OF RUEC66-COR0001-10-F TO WRK-POSICAO
                  ADD  1     TO WRK-IND2
              ELSE
                  MOVE  'J'           TO  RUEC66-TP-PESSOA-PROPT-J
                  MOVE WRK-CCNPJ-CPF(WRK-IND-B09E)
                                        TO  WRK-COD9-NUM-S
                  MOVE WRK-COD9-NUM8-XS TO RUEC66-CNPJBASE-PROPT
EG0617
EG0617            IF WRK-CNIRF-IMOV(WRK-IND-B09E) EQUAL SPACES
050825               MOVE ALL '*'      TO RUEC66-CIB-J
EG0617            ELSE
EG0617               MOVE WRK-CNIRF-IMOV(WRK-IND-B09E)(2:8)
050825                                 TO RUEC66-CIB-J
EG0617            END-IF
EG0617
EG0617            IF WRK-CCCIR(WRK-IND-B09E)      EQUAL SPACES
EG0617               MOVE ALL '*'      TO RUEC66-CCIR-J
EG0617            ELSE
EG0617               MOVE WRK-CCCIR(WRK-IND-B09E)
EG0617                                 TO RUEC66-CCIR-J
EG0617            END-IF
EG0617
EG0617            IF WRK-CREG-AMBTL(WRK-IND-B09E) EQUAL SPACES
EG0617               MOVE ALL '*'      TO RUEC66-NRO-CAR-J
EG0617            ELSE
EG0617            MOVE WRK-CREG-AMBTL(WRK-IND-B09E)
EG0617                                 TO RUEC66-NRO-CAR-J
EG0617            END-IF
EG0617
BRQ001            IF WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E) EQUAL SPACES
BRQ001               MOVE ALL '*'      TO RUEC66-NRO-OUT-J
BRQ001            ELSE
BRQ001               MOVE WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E)
BRQ001                                 TO RUEC66-NRO-OUT-J
BRQ001            END-IF
BRQ001
BRQ001            IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E) EQUAL ZEROS
BRQ001               MOVE ALL '*'      TO RUEC66-PERC-PRESC-J
BRQ001            ELSE
BRQ001               MOVE WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E)
BRQ001                                 TO RUEC66-PERC-PRESC-J-N
BRQ001            END-IF
BRQ001
                  MOVE RUEC66-COR0001-10-J TO
                       WRK-B09A-SIS(WRK-POSICAO:
                                        LENGTH OF RUEC66-COR0001-10-J)
                  ADD  LENGTH OF RUEC66-COR0001-10-J TO WRK-TAM
                  ADD  LENGTH OF RUEC66-COR0001-10-J TO WRK-POSICAO
                  ADD  1     TO WRK-IND2
              END-IF
           ELSE
BR1218        ADD  101   TO WRK-IND2
           END-IF.

      *----------------------------------------------------------------*
       3126-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3127-FORMATA-GRUPO-PARCL         SECTION.
      *---------------------------------------------------------------*

           PERFORM 3021-OPEN-CSR01-RUECB09D.
           PERFORM VARYING  WRK-IND-B09D     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09D  EQUAL 'S'
                   PERFORM 3022-SELECT-RUECB09D
                   PERFORM 3128-FORMATAR-ITEMB09D
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09D.
           MOVE ZEROS TO WRK-IND-B09D.
           ADD  999   TO WRK-IND2.
           PERFORM 3024-CLOSE-CSR01-RUECB09D.

AC0513     MOVE ALL'*'            TO RUEC66-GP-COR0001-PARCL.
AC0513     MOVE RUEC66-COR0001-12 TO
AC0513          WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                       LENGTH OF RUEC66-COR0001-12).
AC0513     ADD LENGTH OF RUEC66-COR0001-12  TO WRK-TAM2.
AC0513     ADD LENGTH OF RUEC66-COR0001-12  TO WRK-POSICAO2.

      *----------------------------------------------------------------*
       3127-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3128-FORMATAR-ITEMB09D    SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              GO TO 3128-99-FIM
           END-IF.

      *** GRUPO PROP TAB B09D
           IF WRK-IND2 NOT GREATER WRK-999
              MOVE  NPCELA-CONTR       TO  RUEC66-NUM-PARCL

      * FORMATAR DATA AAAAMMDD
              MOVE  DVCTO-PCELA-CONTR  TO  WRK-DATA
              MOVE  WRK-DD             TO  WRK-EDIT-DD
              MOVE  WRK-MM             TO  WRK-EDIT-MM
              MOVE  WRK-AAAA           TO  WRK-EDIT-AAAA
              MOVE  WRK-EDIT-DATA-R    TO  RUEC66-DT-PRVPGTO


      * FORMATAR VALOR
              MOVE  VPCELA-RURAL       TO  WRK-VLR17
              MOVE  WRK-VLR17-CHAR     TO  WRK-VLR17-PARC
              MOVE  WRK-VLR2           TO  RUEC66-VLR-PRINC-PARCL

              MOVE RUEC66-COR0001-12 TO
                   WRK-B09A-SIS(WRK-POSICAO:
                                        LENGTH OF RUEC66-COR0001-12)
              ADD LENGTH OF RUEC66-COR0001-12  TO WRK-TAM
              ADD LENGTH OF RUEC66-COR0001-12  TO WRK-POSICAO
              ADD 1   TO WRK-IND2
           ELSE
              ADD 999 TO WRK-IND2
           END-IF.

      *----------------------------------------------------------------*
       3128-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR4.11*---------------------------------------------------------------*
BR4.11*3129-FORMATA-GRUPO-LIBRC  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*
BR4.11****-MOVE ALL '*'       TO RUEC66-DTLIBRC-RECUR
BR4.11****-MOVE ALL '*'       TO RUEC66-VLR-LIBRC-RECUR
BR4.11****-
BR4.11****-MOVE RUEC66-COR0001-14 TO
BR4.11****-      WRK-B09A-SIS(WRK-POSICAO:
BR4.11****-                           LENGTH OF RUEC66-COR0001-14)
BR4.11****-ADD LENGTH OF RUEC66-COR0001-14 TO WRK-TAM
BR4.11****-ADD LENGTH OF RUEC66-COR0001-14 TO WRK-POSICAO
BR4.11****-
BR4.11****-MOVE RUEC66-COR0001-14 TO
BR4.11****-      WRK-B09A-SIS2(WRK-POSICAO:
BR4.11****-                           LENGTH OF RUEC66-COR0001-14)
BR4.11****-ADD LENGTH OF RUEC66-COR0001-14 TO WRK-TAM2
BR4.11****-ADD LENGTH OF RUEC66-COR0001-14 TO WRK-POSICAO2
BR4.11****-
BR4.11****-ADD  31            TO WRK-IND2.
BR4.11****-------------------------------------------------------------*
BR4.11****--99-FIM.                    EXIT.
BR4.11****-------------------------------------------------------------*

STEFA4*---------------------------------------------------------------*
.      3129-FORMATAR-CAMPO-QTD   SECTION.
.     *---------------------------------------------------------------*
.
.          IF (QUND-FINCD-RURAL  OF RUECB099  GREATER  ZEROS AND
.               QUND-ANMAL       OF RUFIB002  GREATER  ZEROS)
.              MOVE  QUND-ANMAL  OF RUFIB002  TO WRK-QTD14
.              MOVE  WRK-QTD14-NUM  TO RUEC66-QTD-ITEM-FINCD-N
.              GO                   TO 3129-99-FIM
.          END-IF.
.
.          IF  (QUND-FINCD-RURAL OF RUECB099  GREATER ZEROS AND
.               QUND-ANMAL       OF RUFIB002  EQUAL  ZEROS)
.              MOVE  QUND-FINCD-RURAL  OF RUECB099  TO WRK-QTD14
.              MOVE  WRK-QTD14-NUM  TO RUEC66-QTD-ITEM-FINCD-N
.          ELSE
.           IF (QUND-FINCD-RURAL OF RUECB099  EQUAL    ZEROS AND
.               QUND-ANMAL       OF RUFIB002  GREATER  ZEROS)
.              MOVE  QUND-ANMAL  OF RUFIB002  TO WRK-QTD14
.              MOVE  WRK-QTD14-NUM  TO RUEC66-QTD-ITEM-FINCD-N
.           END-IF
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3129-99-FIM.                    EXIT.
STEFA4*----------------------------------------------------------------*


      *---------------------------------------------------------------*
       3130-FORMATAR-ITEMB09F    SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE EQUAL +100
              IF WRK-ACHOU-B09F EQUAL 'S'
                 MOVE 'N' TO WRK-ACHOU-B09F
                 ADD 999 TO WRK-IND2
                 GO TO 3130-99-FIM
              ELSE
                 MOVE ALL '*' TO RUEC66-COD-PRODT-CONSCD
                 MOVE RUEC66-COR0001-8  TO
                      WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-8)
                 ADD LENGTH OF RUEC66-COR0001-8  TO WRK-TAM
                 ADD LENGTH OF RUEC66-COR0001-8  TO WRK-POSICAO
                 ADD 999 TO WRK-IND2
                 GO TO 3130-99-FIM
              END-IF
           END-IF.

AC0513     MOVE 'S' TO WRK-ACHOU-B09F.
AC0513     PERFORM 3032-SELECT-B006.
AC0513     MOVE CPRODT-BACEN     TO WRK-COD5-CHAR.
AC0513     MOVE WRK-COD5-CHAR4   TO RUEC66-COD-PRODT-CONSCD.
AC0513     MOVE RUEC66-COR0001-8 TO
AC0513          WRK-B09A-SIS(WRK-POSICAO:
AC0513                       LENGTH OF RUEC66-COR0001-8).
AC0513     ADD LENGTH OF RUEC66-COR0001-8  TO WRK-TAM.
AC0513     ADD LENGTH OF RUEC66-COR0001-8  TO WRK-POSICAO.
AC0513     ADD 1   TO WRK-IND2.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BS0216*---------------------------------------------------------------*
BS0216 3131-ACESSA-RUECV025      SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216* FORMATA VALOR PRODUTIVIDADE
BR4.11****-MOVE ALL '*'       TO RUEC66-VLR-PROD-OBTD
BS0216
BS0216     PERFORM 3410-CHECAR-FINALIDADE-BACEN
BS0216
BS0216* NAO INFORMA DATAS DE PLANTIO E COLHEITA SE
EG0416* ** -- FINALIDADE BRADESCO =  4
EG0416* ** -- ATIVIDADE  BRADESCO <> 1 E 5
EG0416* ** -- FINALIDADE BACEN    <> 00002 - CUSTEIO
BS0216     IF   CFNALD-RURAL  OF RUFIB002 EQUAL 4 OR
BS0216         (CATVDD-RURAL  OF RUFIB002 NOT EQUAL 1 AND 5)
EG0416     OR  (CFNALD-RECOR  OF RURCB003 NOT EQUAL '00002' AND '00003')
EG0617        MOVE ALL '*'                       TO RUEC66-DT-INI-PLANT
EG0617        MOVE ALL '*'                       TO RUEC66-DT-FIM-PLANT
EG0617        MOVE ALL '*'                       TO RUEC66-DT-INI-COLHT
EG0617        MOVE ALL '*'                       TO RUEC66-DT-FIM-COLHT
BS0216        GO 3131-99-FIM
BS0216     END-IF
BS0216
MJ1223     IF  (CFNALD-RECOR  OF RURCB003 EQUAL '00002')
MJ1223     OR ((CFNALD-RECOR  OF RURCB003 EQUAL '00003') AND
MJ1223         (CFNALD-RURAL  OF RUFIB002 EQUAL 11))
                CONTINUE
           ELSE
MJ1223        MOVE ALL '*'                       TO RUEC66-DT-INI-PLANT
MJ1223        MOVE ALL '*'                       TO RUEC66-DT-FIM-PLANT
MJ1223        MOVE ALL '*'                       TO RUEC66-DT-INI-COLHT
MJ1223        MOVE ALL '*'                       TO RUEC66-DT-FIM-COLHT
MJ1223        GO 3131-99-FIM
MJ1223     END-IF
BS0216
BS0216* SE FOR CULTURA
BS0216
BS0216     MOVE 'N'                      TO  WRK-FIM-V25
BS0216     MOVE 'N'                      TO  WRK-ACHOU-V25-COLHT
BS0216     MOVE 'N'                      TO  WRK-ACHOU-V25-PLANT
BS0216     MOVE CJUNC-DEPDC  OF RUFIB002 TO CJUNC-DEPDC OF RUECV025.
BS0216     MOVE CCDULA       OF RUFIB002 TO CCDULA      OF RUECV025.
BS0216
BS0216     PERFORM 3300-OPEN-CSR01-RUECV025.
BS0216
BS0216     PERFORM 3310-FETCH-CSR01-RUECV025.
BS0216
EG0617     MOVE ALL '*'                       TO RUEC66-DT-INI-PLANT
EG0617     MOVE ALL '*'                       TO RUEC66-DT-FIM-PLANT
EG0617     MOVE ALL '*'                       TO RUEC66-DT-INI-COLHT
EG0617     MOVE ALL '*'                       TO RUEC66-DT-FIM-COLHT
BS0216
BS0216     IF  WRK-FIM-V25             EQUAL  'S'
BS0216        PERFORM 3320-CLOSE-CSR01-RUECV025
BS0216        GO 3131-99-FIM
BS0216     END-IF.
BS0216
BS0216     PERFORM UNTIL WRK-FIM-V25   EQUAL  'S'
BS0216* FORMATA DATA COLHEITA
BS0216      IF WRK-ACHOU-V25-COLHT EQUAL 'N'
BS0216       IF RTPO-OPER-ORCAM OF RUECV025 (01:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (02:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (03:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (04:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (05:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (06:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (07:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (08:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (09:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (10:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (11:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (12:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (13:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (14:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (15:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (16:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (17:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (18:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (19:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (20:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (21:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (22:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (23:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (24:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (25:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (26:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (27:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (28:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (29:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (30:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (31:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (32:08)  EQUAL 'COLHEITA' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (33:08)  EQUAL 'COLHEITA'
EG0617          PERFORM 4400-FORMATAR-DTINI-DTFIM
EG0617          MOVE WRK-66-DT-INI     TO RUEC66-DT-INI-COLHT
EG0617          MOVE WRK-66-DT-FIM     TO RUEC66-DT-FIM-COLHT
BS0216          MOVE 'S' TO WRK-ACHOU-V25-COLHT
BS0216       END-IF
BS0216      END-IF
BS0216
BS0216* FORMATA DATA PLANTIO
BS0216
BS0216      IF WRK-ACHOU-V25-PLANT EQUAL 'N'
BS0216       IF RTPO-OPER-ORCAM OF RUECV025 (01:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (02:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (03:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (04:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (05:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (06:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (07:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (08:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (09:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (10:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (11:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (12:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (13:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (14:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (15:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (16:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (17:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (18:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (19:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (20:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (21:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (22:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (23:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (24:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (25:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (26:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (27:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (28:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (29:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (30:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (31:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (32:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (33:07)  EQUAL 'PLANTIO' OR
BS0216          RTPO-OPER-ORCAM OF RUECV025 (34:07)  EQUAL 'PLANTIO'
EG0617          PERFORM 4400-FORMATAR-DTINI-DTFIM
EG0617          MOVE WRK-66-DT-INI     TO RUEC66-DT-INI-PLANT
EG0617          MOVE WRK-66-DT-FIM     TO RUEC66-DT-FIM-PLANT
BS0216          MOVE 'S' TO WRK-ACHOU-V25-PLANT
BS0216       END-IF
BS0216      END-IF
BS0216
BS0216* VERIFICA SE ACHOU PLANTIO E COLHEITA
BS0216
BS0216       IF WRK-ACHOU-V25-COLHT EQUAL 'S' AND
BS0216          WRK-ACHOU-V25-PLANT EQUAL 'S'
BS0216          MOVE 'S'   TO WRK-FIM-V25
BS0216       END-IF
BS0216       IF WRK-FIM-V25 NOT EQUAL 'S'
BS0216             PERFORM 3310-FETCH-CSR01-RUECV025
BS0216       END-IF
BS0216     END-PERFORM.
BS0216
BS0216     PERFORM 3320-CLOSE-CSR01-RUECV025.
BS0216
BS0216* VERIFICA SE EH PRODUTO PERENE (0 - NAO PERENE / 1 - PERENE)
BS0216
BS0216     PERFORM 3390-CHECAR-PRODUTO-PERENE.
BS0216
BS0216***  CULTURA NAO PERENE(0)       - OBRIGATORIO  PLANTIO E COLHEITA
BS0216***  CULTURA PERENE(1) - INVEST. (00003) - OBRIGATORIO   PLANTIO,
BS0216***                                        PERMITIDO     COLHEITA
BS0216***  CULTURA PERENE(1) - CUSTEIO (00002) - NAO PERMITIDO PLANTIO,
BS0216***                                        OBRIGATORIO   COLHEITA
MJ0824******F CTEMPO-VIDA-CULTR     OF RURCB006 EQUAL 1 AND
MJ0824******  CFNALD-RECOR          OF RURCB003 EQUAL '00002'
MJ0824*********OVE ALL '*'                       TO RUEC66-DT-INI-PLANT
MJ0824*********OVE ALL '*'                       TO RUEC66-DT-FIM-PLANT
MJ1223*********OVE ALL '*'                       TO RUEC66-DT-INI-COLHT
MJ1223*********OVE ALL '*'                       TO RUEC66-DT-FIM-COLHT
MJ0824*********O 3131-99-FIM
MJ0824******ND-IF.
BS0216
BS0216     IF  ( CFNALD-RECOR      OF RURCB003 EQUAL '00002' OR
BS0216           CFNALD-RECOR      OF RURCB003 EQUAL '00003' )
BS0216       IF CTEMPO-VIDA-CULTR     OF RURCB006 EQUAL 0
BS0216          IF WRK-ACHOU-V25-COLHT EQUAL 'N' OR
BS0216             WRK-ACHOU-V25-PLANT EQUAL 'N'
BS0216             MOVE 11                   TO RUEC65-COD-RET
BS0216             MOVE 'DATAS PLANTIO E COLHEITA SAO OBRIGATORIOS PARA
BS0216-                 'CULTURAS NAO PERENES'
BS0216                                       TO RUEC65-MENSAGEM
BS0216             MOVE SPACES               TO RUEC65-SQLCA
BS0216             MOVE SPACES               TO RUEC65-ERRO-AREA
BS0216             PERFORM 3420-PERMITE-ALTERACAO
BS0216             PERFORM 9000-ROTINA-FIM
BS0216          END-IF
BS0216       ELSE
BS0216          IF  CFNALD-RECOR      OF RURCB003 EQUAL '00002'
BS0216             IF WRK-ACHOU-V25-PLANT EQUAL 'N'
BS0216               MOVE 12                   TO RUEC65-COD-RET
MJ0824               MOVE 'DATA DE PLANTIO OBRIGATORIA PARA CUSTEIO DE C
MJ0824-                 'ULTURAS PERENES'
BS0216                                         TO RUEC65-MENSAGEM
BS0216               MOVE SPACES               TO RUEC65-SQLCA
BS0216               MOVE SPACES               TO RUEC65-ERRO-AREA
BS0216               PERFORM 9000-ROTINA-FIM
BS0216             END-IF
BS0216             IF WRK-ACHOU-V25-COLHT EQUAL 'N'
BS0216               MOVE 13                   TO RUEC65-COD-RET
BS0216               MOVE 'DATA DE COLHEITA OBRIGATORIO PARA CUSTEIO DE
BS0216-                 'CULTURAS PERENES'
BS0216                                         TO RUEC65-MENSAGEM
BS0216               MOVE SPACES               TO RUEC65-SQLCA
BS0216               MOVE SPACES               TO RUEC65-ERRO-AREA
BS0216               PERFORM 3420-PERMITE-ALTERACAO
BS0216               PERFORM 9000-ROTINA-FIM
BS0216             END-IF
BS0216          ELSE
BS0216             IF WRK-ACHOU-V25-PLANT EQUAL 'N'
BS0216               MOVE 14                   TO RUEC65-COD-RET
BS0216               MOVE 'DATA DE PLANTIO OBRIGATORIO PARA INVESTIMENTO
BS0216-                 ' EM CULTURAS PERENES'
BS0216                                         TO RUEC65-MENSAGEM
BS0216               MOVE SPACES               TO RUEC65-SQLCA
BS0216               MOVE SPACES               TO RUEC65-ERRO-AREA
BS0216               PERFORM 3420-PERMITE-ALTERACAO
BS0216               PERFORM 9000-ROTINA-FIM
BS0216             END-IF
BS0216          END-IF
BS0216       END-IF
BS0216     END-IF.
BS0216*----------------------------------------------------------------*
BS0216 3131-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*

BS0216*---------------------------------------------------------------*
BS0216 3132-ACESSA-FUMAGEIRA     SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216* FORMATA VALOR PRODUTIVIDADE
BR4.11****-MOVE ALL '*'       TO RUEC66-VLR-PROD-OBTD
BS0216
BS0216* FORMATA VALOR PRODUTIVIDADE
BS0216     PERFORM 3380-SELECT-RURCV014
BS0216
BS0216* FORMATA DATA CULTIVO
BS0216
BS0216     PERFORM 3330-SEL-RUIAV007.
BS0216
BS0216     IF   SQLCODE   EQUAL  100 OR  -305
EG0617        MOVE ALL '*'                       TO RUEC66-DT-INI-COLHT
EG0617        MOVE ALL '*'                       TO RUEC66-DT-FIM-COLHT
BS0216     ELSE
BS0216        PERFORM 3340-SEL-RUIAV007-2
BS0216        IF   SQLCODE   EQUAL  100 OR  -305
EG0617           MOVE ALL '*'                    TO RUEC66-DT-INI-COLHT
EG0617           MOVE ALL '*'                    TO RUEC66-DT-FIM-COLHT
BS0216        ELSE
BS0216           MOVE 'N'                        TO  WRK-FIM-RUIA
BS0216           MOVE 'N'                        TO  WRK-ACHOU-RUIA
BS0216           PERFORM 3350-OPEN-CSR01-RUIAV008-JN
BS0216           PERFORM 3360-LER-CSR01-RUIAV008-JN
BS0216           IF  WRK-FIM-RUIA            EQUAL  'S'
BS0716           OR  SQLCODE                 EQUAL  +100
BS0216               MOVE ALL '*'                TO RUEC66-DT-INI-COLHT
BS0216               MOVE ALL '*'                TO RUEC66-DT-FIM-COLHT
BS0216           END-IF
BS0216           PERFORM UNTIL WRK-FIM-RUIA  EQUAL  'S'
BS0716                      OR SQLCODE       EQUAL  +100
BS0216             IF RTPO-OPER-ORCAM OF RUIAV005 (01:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (02:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (03:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (04:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (05:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (06:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (07:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (08:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (09:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (10:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (11:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (12:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (13:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (14:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (15:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (16:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (17:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (18:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (19:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (20:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (21:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (22:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (23:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (24:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (25:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (26:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (27:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (28:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (29:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (30:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (31:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (32:08)
BS0216                                               EQUAL 'COLHEITA' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (33:08)
BS0216                                               EQUAL 'COLHEITA'
BS0216                MOVE DINIC-VGCIA-PCELA OF RUIAV008
BS0216                                           TO WRK-DATA
EG0617                MOVE WRK-DD                TO WRK-EDIT-DD
EG0617                MOVE WRK-MM                TO WRK-EDIT-MM
EG0617                MOVE WRK-AAAA              TO WRK-EDIT-AAAA
EG0617                MOVE WRK-EDIT-DATA         TO RUEC66-DT-INI-COLHT
EG0617                MOVE DFIM-VGCIA            OF RUIAV008
EG0617                                           TO WRK-DATA
EG0617                MOVE WRK-DD                TO WRK-EDIT-DD
EG0617                MOVE WRK-MM                TO WRK-EDIT-MM
EG0617                MOVE WRK-AAAA              TO WRK-EDIT-AAAA
EG0617                MOVE WRK-EDIT-DATA         TO RUEC66-DT-FIM-COLHT
BS0216                MOVE 'S'                   TO WRK-FIM-RUIA
BS0216                MOVE 'S'                   TO WRK-ACHOU-RUIA
BS0216             END-IF
BS0216             IF WRK-FIM-RUIA NOT EQUAL 'S'
BS0216                PERFORM 3360-LER-CSR01-RUIAV008-JN
BS0216             END-IF
BS0216           END-PERFORM
BS0216
BS0216           IF  WRK-ACHOU-RUIA           EQUAL  'N'
EG0617              MOVE ALL '*'             TO RUEC66-DT-INI-COLHT
EG0617              MOVE ALL '*'             TO RUEC66-DT-FIM-COLHT
BS0216           END-IF
BS0216           PERFORM 3370-CLOSE-CSR01-RUIAV008-JN
BS0216        END-IF
BS0216     END-IF
BS0216
BS0216
BS0216* FORMATA DATA PLANTIO
BS0216
BS0216     MOVE 'N'                              TO  WRK-FIM-RUIA
BS0216     MOVE 'N'                              TO  WRK-ACHOU-RUIA
BS0216
BS0216     PERFORM 3330-SEL-RUIAV007.
BS0216
BS0216     IF   SQLCODE   EQUAL  100 OR  -305
EG0617        MOVE ALL '*'                       TO RUEC66-DT-INI-PLANT
EG0617        MOVE ALL '*'                       TO RUEC66-DT-FIM-PLANT
BS0216     ELSE
BS0216        PERFORM 3340-SEL-RUIAV007-2
BS0216        IF   SQLCODE   EQUAL  100 OR  -305
EG0617           MOVE ALL '*'                    TO RUEC66-DT-INI-PLANT
EG0617           MOVE ALL '*'                    TO RUEC66-DT-FIM-PLANT
BS0216        ELSE
BS0216           MOVE 'N'                        TO  WRK-FIM-RUIA
BS0216           MOVE 'N'                        TO  WRK-ACHOU-RUIA
BS0216           PERFORM 3350-OPEN-CSR01-RUIAV008-JN
BS0216           PERFORM 3360-LER-CSR01-RUIAV008-JN
BS0216           IF  WRK-FIM-RUIA            EQUAL  'S'
BS0716           OR  SQLCODE                 EQUAL  +100
BS0216               MOVE ALL '*'                TO RUEC66-DT-INI-PLANT
BS0216               MOVE ALL '*'                TO RUEC66-DT-FIM-PLANT
BS0216           END-IF
BS0216           PERFORM UNTIL WRK-FIM-RUIA  EQUAL  'S'
BS0716                      OR SQLCODE       EQUAL  +100
BS0216             IF RTPO-OPER-ORCAM OF RUIAV005 (01:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (02:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (03:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (04:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (05:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (06:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (07:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (08:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (09:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (10:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (11:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (12:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (13:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (14:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (15:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (16:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (17:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (18:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (19:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (20:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (21:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (22:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (23:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (24:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (25:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (26:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (27:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (28:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (29:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (30:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (31:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (32:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (33:07)
BS0216                                               EQUAL 'PLANTIO' OR
BS0216                RTPO-OPER-ORCAM OF RUIAV005 (34:07)
BS0216                                               EQUAL 'PLANTIO'
BS0216                MOVE DINIC-VGCIA-PCELA OF RUIAV008
BS0216                                           TO WRK-DATA
EG0617                MOVE WRK-DD                TO WRK-EDIT-DD
EG0617                MOVE WRK-MM                TO WRK-EDIT-MM
EG0617                MOVE WRK-AAAA              TO WRK-EDIT-AAAA
EG0617                MOVE WRK-EDIT-DATA         TO RUEC66-DT-INI-PLANT
EG0617                MOVE DFIM-VGCIA            OF RUIAV008
EG0617                                           TO WRK-DATA
EG0617                MOVE WRK-DD                TO WRK-EDIT-DD
EG0617                MOVE WRK-MM                TO WRK-EDIT-MM
EG0617                MOVE WRK-AAAA              TO WRK-EDIT-AAAA
EG0617                MOVE WRK-EDIT-DATA         TO RUEC66-DT-FIM-PLANT
BS0216                MOVE 'S'                   TO WRK-FIM-RUIA
BS0216                MOVE 'S'                   TO WRK-ACHOU-RUIA
BS0216             END-IF
BS0216             IF WRK-FIM-RUIA NOT EQUAL 'S'
BS0216                PERFORM 3360-LER-CSR01-RUIAV008-JN
BS0216             END-IF
BS0216           END-PERFORM
BS0216
BS0216           IF  WRK-ACHOU-RUIA           EQUAL  'N'
EG0617              MOVE ALL '*'             TO RUEC66-DT-INI-PLANT
EG0617              MOVE ALL '*'             TO RUEC66-DT-FIM-PLANT
BS0216           END-IF
BS0216           PERFORM 3370-CLOSE-CSR01-RUIAV008-JN
BS0216        END-IF
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3132-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*

PA1215*---------------------------------------------------------------*
PA1215 3137-FORMATA-GRUPO-GLEBA  SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     MOVE CCONTR OF RUFIB002     TO CCONTR      OF RUECB040.
PA1215
BR1811     IF  WRK-PRIMEIRO-IMOV       NOT EQUAL 'N'
EG0517         PERFORM 3201-OPEN-CSR01-RUECB040-JN
EG0517         PERFORM 3202-FETCH-CSR01-RUECB040-JN
BR1811     END-IF.
PA1215
PA1215     IF  WRK-FIM-B40             EQUAL  'S'
PA1215         MOVE ALL '*'            TO RUEC66-GP-COR0001-GLEBA
211025**      EXCLUSAO DE CAMPO - SICOR 5.11
PA1215**      -MOVE ALL '*'            TO RUEC66-GP-COR0001-PTPOLG
BR4.11*       -MOVE ALL '*'            TO RUEC66-COD-MUN-GLEB
BS0216         MOVE ALL '*'            TO RUEC66-AREA-NCULTVD
BR0119         PERFORM 3122-FORMATA-GRUPO-GLEBA-AST
BR0119             UNTIL WRK-IND2   GREATER  WRK-999
211025**      EXCLUSAO DE CAMPO - SICOR 5.11
STEFA1**      -PERFORM 4122-FORMATA-GRUPO-GLEBA-AST
STEFA1**          -UNTIL WRK-IND2-E GREATER  WRK-999
PA1215     END-IF.
PA1215
BR1811     MOVE ZEROS                    TO WRK-QTD-COORD
211025     MOVE ZEROS                    TO WRK-NUM-POLIGN
PA1215     PERFORM UNTIL WRK-FIM-B40     EQUAL  'S'
BR1811*              -OR WRK-QTD-COORD   EQUAL  90
211025                OR WRK-QTD-COORD   EQUAL  350
211025                OR WRK-NUM-POLIGN  GREATER 1

PA1215             PERFORM 3204-FORMATAR-CSR01-RUECB040

STEFAB             PERFORM 3244-FORMATAR-CSR02-RUECB040

BR1811             IF  WRK-FIM-B9N     EQUAL  'S'
211025                 PERFORM 3220-MONTAR-POLIGONO
EG0517                 PERFORM 3202-FETCH-CSR01-RUECB040-JN
PA1215                 ADD 1           TO WRK-IND2
BR1811             END-IF
PA1215     END-PERFORM.
PA1215
PA1215     ADD 999                     TO WRK-IND2.
PA1215
BR1811     IF  WRK-FIM-B40         NOT EQUAL  'S'
BR1811         MOVE 'N'                TO WRK-PRIMEIRO-IMOV
BR1811         MOVE 'N'                TO WRK-FINAL
BR1811     ELSE
BR1811         MOVE 'S'                TO WRK-FINAL
EG0517         PERFORM 3203-CLOSE-CSR01-RUECB040-JN
BR1811     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3137-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
211025**      AJUSTE DE CAMPO - SICOR 5.11
PA1215*---------------------------------------------------------------*
PA1215 3138-FORMATA-GRUPO-PTPOLG   SECTION.
PA1215*---------------------------------------------------------------*
PA1215*
PA1215     MOVE RUEC66-COR0001-7       TO WRK-B09A-SIS(WRK-POSICAO:
PA1215                                    LENGTH OF RUEC66-COR0001-7).
PA1215     ADD LENGTH OF RUEC66-COR0001-7 TO WRK-TAM.
PA1215     ADD LENGTH OF RUEC66-COR0001-7 TO WRK-POSICAO.
PA1215
PA1215     MOVE RUEC66-COR0001-7       TO WRK-B09A-SIS2(WRK-POSICAO2:
PA1215                                    LENGTH OF RUEC66-COR0001-7).
PA1215     ADD LENGTH OF RUEC66-COR0001-7 TO WRK-TAM2.
PA1215     ADD LENGTH OF RUEC66-COR0001-7 TO WRK-POSICAO2.
PA1215*
PA1215*----------------------------------------------------------------*
PA1215 3138-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
BS0216*---------------------------------------------------------------*
BS0216*3139-FORMATA-GRUPO-AREA-N-CULT SECTION.
BS0216*---------------------------------------------------------------*
BS0216*
BS0216*   -MOVE RUEC66-COR0001-6       TO WRK-B09A-SIS(WRK-POSICAO:
BS0216*                                  -LENGTH OF RUEC66-COR0001-6).
BS0216*   -ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM.
BS0216*   -ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO.
BS0216*
BS0216*   -MOVE RUEC66-COR0001-6        TO WRK-B09A-SIS2(WRK-POSICAO2:
BS0216*                                  -LENGTH OF RUEC66-COR0001-6).
BS0216*   -ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM2.
BS0216*   -ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO2.
BS0216*
BS0216*----------------------------------------------------------------*
BS0216*3139-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
PA1215
      *---------------------------------------------------------------*
       3140-FORMATAR-PROP-TAB    SECTION.
      *---------------------------------------------------------------*
           IF SQLCODE EQUAL +100
              GO TO 3140-99-FIM
           END-IF.

           IF  WRK-IND-TAB        EQUAL ZERO                   OR
               CCNPJ-CPF      NOT EQUAL WRK-CCNPJ-CPF-ANT      OR
               CFLIAL-CNPJ    NOT EQUAL WRK-CFLIAL-CNPJ-ANT    OR
               CCTRL-CNPJ-CPF NOT EQUAL WRK-CCTRL-CNPJ-CPF-ANT
              ADD  1                 TO WRK-IND-TAB
              MOVE WRK-IND-TAB       TO WRK-IND-INI
              MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
              MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
              MOVE CFLIAL-CNPJ       TO WRK-CFLIAL-CNPJ(WRK-IND-TAB)
              MOVE CCTRL-CNPJ-CPF    TO WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
              MOVE CNIRF-IMOV-RURAL  TO WRK-CNIRF-IMOV(WRK-IND-TAB)
              MOVE CCCIR             TO WRK-CCCIR(WRK-IND-TAB)
              MOVE CREG-AMBTL-RURAL  TO WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001        MOVE PPRESV-AMBTL-PPRIE OF RUECB040
BRQ001                       TO WRK-PPRESV-CS
BRQ001        MOVE WRK-PPRESV-SS
BRQ001                       TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001        MOVE COUTGA-AGUA-IMOV   OF RUECB040
BRQ001                       TO WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001        MOVE DVALDD-OUTGA-AGUA  OF RUECB040
BRQ001                       TO WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
           ELSE
              PERFORM  3141-TRATAR-PROP-TAB
           END-IF.

           MOVE CCNPJ-CPF         TO  WRK-CCNPJ-CPF-ANT.
           MOVE CFLIAL-CNPJ       TO  WRK-CFLIAL-CNPJ-ANT.
           MOVE CCTRL-CNPJ-CPF    TO  WRK-CCTRL-CNPJ-CPF-ANT.
      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3141-TRATAR-PROP-TAB    SECTION.
      *---------------------------------------------------------------*
      *   TRATAR CNIRF
           IF CNIRF-IMOV-RURAL NOT EQUAL SPACES
                 MOVE 'N'       TO WRK-ATUALIZA
                 PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
                         UNTIL    WRK-IND4     GREATER WRK-IND-TAB OR
                         WRK-ATUALIZA            EQUAL 'S'
                   IF WRK-CNIRF-IMOV(WRK-IND4) EQUAL CNIRF-IMOV-RURAL
                       MOVE 'S'       TO WRK-ATUALIZA
                   ELSE
                    IF WRK-CNIRF-IMOV(WRK-IND4) EQUAL SPACES
                      MOVE CNIRF-IMOV-RURAL  TO
                                              WRK-CNIRF-IMOV(WRK-IND4)
                      MOVE 'S'       TO WRK-ATUALIZA
                    END-IF
                   END-IF
                 END-PERFORM

                 IF WRK-ATUALIZA NOT EQUAL 'S'
                   ADD  1                 TO WRK-IND-TAB
                   MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
                   MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
                   MOVE CFLIAL-CNPJ       TO
                                         WRK-CFLIAL-CNPJ(WRK-IND-TAB)
                   MOVE CCTRL-CNPJ-CPF    TO
                                         WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
                   MOVE CNIRF-IMOV-RURAL  TO WRK-CNIRF-IMOV(WRK-IND-TAB)
                   MOVE SPACES            TO
                                         WRK-CCCIR(WRK-IND-TAB)
                   MOVE SPACES            TO
                                         WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE ZEROS             TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
                 END-IF
           END-IF

      *   TRATAR CCCIR
           IF CCCIR  NOT EQUAL SPACES
                 MOVE 'N'       TO WRK-ATUALIZA
                 PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
                         UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
                         WRK-ATUALIZA            EQUAL 'S'
                   IF WRK-CCCIR(WRK-IND4) EQUAL CCCIR
                     MOVE 'S'       TO WRK-ATUALIZA
                   ELSE
                    IF WRK-CCCIR(WRK-IND4) EQUAL SPACES
                      MOVE CCCIR     TO WRK-CCCIR(WRK-IND4)
      *IN3543238 - I
                      MOVE 'S'       TO WRK-ATUALIZA
      *IN3543238 - F
                    END-IF
                   END-IF
                 END-PERFORM

                 IF WRK-ATUALIZA NOT EQUAL 'S'
                   ADD  1                 TO WRK-IND-TAB
                   MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
                   MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
                   MOVE CFLIAL-CNPJ       TO
                                         WRK-CFLIAL-CNPJ(WRK-IND-TAB)
                   MOVE CCTRL-CNPJ-CPF    TO
                                         WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
                   MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
                   MOVE CCCIR             TO
                                         WRK-CCCIR(WRK-IND-TAB)
                   MOVE SPACES            TO
                                         WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE ZEROS             TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
                 END-IF
           END-IF

      *   TRATAR CAR
           IF CREG-AMBTL-RURAL NOT EQUAL SPACES
                 MOVE 'N'       TO WRK-ATUALIZA
                 PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
                         UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
                         WRK-ATUALIZA            EQUAL 'S'
                   IF WRK-CREG-AMBTL(WRK-IND4) EQUAL CREG-AMBTL-RURAL
                     MOVE 'S'       TO WRK-ATUALIZA
                   ELSE
                    IF WRK-CREG-AMBTL(WRK-IND4) EQUAL SPACES
                      MOVE CREG-AMBTL-RURAL  TO
                                              WRK-CREG-AMBTL(WRK-IND4)
                      MOVE 'S'       TO WRK-ATUALIZA
                    END-IF
                   END-IF
                 END-PERFORM

                 IF WRK-ATUALIZA NOT EQUAL 'S'
                   ADD  1                 TO WRK-IND-TAB
                   MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
                   MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
                   MOVE CFLIAL-CNPJ       TO
                                         WRK-CFLIAL-CNPJ(WRK-IND-TAB)
                   MOVE CCTRL-CNPJ-CPF    TO
                                         WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
                   MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
                   MOVE SPACES            TO
                                         WRK-CCCIR(WRK-IND-TAB)
                   MOVE CREG-AMBTL-RURAL  TO
                                         WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE ZEROS             TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
                 END-IF
           END-IF.

BRQ001*   TRATAR PPRESV-AMBTL-PPRIE
BRQ001     IF PPRESV-AMBTL-PPRIE OF RUECB040 NOT EQUAL ZEROS
BRQ001           MOVE 'N'       TO WRK-ATUALIZA
BRQ001           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BRQ001                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BRQ001                   WRK-ATUALIZA            EQUAL 'S'
BRQ001             MOVE PPRESV-AMBTL-PPRIE OF RUECB040
BRQ001                              TO WRK-PPRESV-CS
BRQ001             IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND4)
BRQ001                              EQUAL WRK-PPRESV-SS
BRQ001               MOVE 'S'       TO WRK-ATUALIZA
BRQ001             ELSE
BRQ001              IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND4) EQUAL ZEROS
BRQ001                MOVE WRK-PPRESV-SS
BRQ001                               TO WRK-PPRESV-AMBTL-PPRIE(WRK-IND4)
BRQ001                MOVE 'S'       TO WRK-ATUALIZA
BRQ001              END-IF
BRQ001             END-IF
BRQ001           END-PERFORM
BRQ001
BRQ001           IF WRK-ATUALIZA NOT EQUAL 'S'
BRQ001             ADD  1                 TO WRK-IND-TAB
BRQ001             MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BRQ001             MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE CFLIAL-CNPJ       TO
BRQ001                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BRQ001             MOVE CCTRL-CNPJ-CPF    TO
BRQ001                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CCCIR(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE PPRESV-AMBTL-PPRIE OF RUECB040
BRQ001                                    TO WRK-PPRESV-CS
BRQ001             MOVE WRK-PPRESV-SS     TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
BRQ001           END-IF
BRQ001     END-IF.

BRQ001*   TRATAR COUTGA-AGUA-IMOV
BRQ001     IF COUTGA-AGUA-IMOV   OF RUECB040 NOT EQUAL ZEROS
BRQ001           MOVE 'N'       TO WRK-ATUALIZA
BRQ001           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BRQ001                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BRQ001                   WRK-ATUALIZA            EQUAL 'S'
BRQ001             IF WRK-COUTGA-AGUA-IMOV(WRK-IND4)
BRQ001                              EQUAL COUTGA-AGUA-IMOV OF RUECB040
BRQ001               MOVE 'S'       TO WRK-ATUALIZA
BRQ001             ELSE
BRQ001              IF WRK-COUTGA-AGUA-IMOV(WRK-IND4) EQUAL SPACES
BRQ001                MOVE COUTGA-AGUA-IMOV OF RUECB040
BRQ001                               TO WRK-COUTGA-AGUA-IMOV(WRK-IND4)
BRQ001                MOVE 'S'       TO WRK-ATUALIZA
BRQ001              END-IF
BRQ001             END-IF
BRQ001           END-PERFORM
BRQ001
BRQ001           IF WRK-ATUALIZA NOT EQUAL 'S'
BRQ001             ADD  1                 TO WRK-IND-TAB
BRQ001             MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BRQ001             MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE CFLIAL-CNPJ       TO
BRQ001                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BRQ001             MOVE CCTRL-CNPJ-CPF    TO
BRQ001                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CCCIR(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE ZEROS             TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE COUTGA-AGUA-IMOV OF RUECB040 TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
BRQ001           END-IF
BRQ001     END-IF.

BRQ001*   TRATAR DVALDD-OUTGA-AGUA
BRQ001     IF DVALDD-OUTGA-AGUA  OF RUECB040 NOT EQUAL ZEROS
BRQ001           MOVE 'N'       TO WRK-ATUALIZA
BRQ001           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BRQ001                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BRQ001                   WRK-ATUALIZA            EQUAL 'S'
BRQ001             IF WRK-DVALDD-OUTGA-AGUA(WRK-IND4)
BRQ001                              EQUAL DVALDD-OUTGA-AGUA OF RUECB040
BRQ001               MOVE 'S'       TO WRK-ATUALIZA
BRQ001             ELSE
BRQ001              IF WRK-DVALDD-OUTGA-AGUA(WRK-IND4) EQUAL SPACES
BRQ001                MOVE DVALDD-OUTGA-AGUA OF RUECB040
BRQ001                               TO WRK-DVALDD-OUTGA-AGUA(WRK-IND4)
BRQ001                MOVE 'S'       TO WRK-ATUALIZA
BRQ001              END-IF
BRQ001             END-IF
BRQ001           END-PERFORM
BRQ001
BRQ001           IF WRK-ATUALIZA NOT EQUAL 'S'
BRQ001             ADD  1                 TO WRK-IND-TAB
BRQ001             MOVE CTPO-PSSOA        TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BRQ001             MOVE CCNPJ-CPF         TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE CFLIAL-CNPJ       TO
BRQ001                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BRQ001             MOVE CCTRL-CNPJ-CPF    TO
BRQ001                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CCCIR(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BRQ001             MOVE ZEROS             TO
BRQ001                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BRQ001             MOVE SPACES            TO
BRQ001                                WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BRQ001             MOVE DVALDD-OUTGA-AGUA OF RUECB040 TO
BRQ001                               WRK-DVALDD-OUTGA-AGUA(WRK-IND-TAB)
BRQ001           END-IF
BRQ001     END-IF.

      *----------------------------------------------------------------*
       3141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
PA1215
      *---------------------------------------------------------------*
       3150-FORMATA-DTMOVTO      SECTION.
      *---------------------------------------------------------------*

      * FORMATAR DATA AAAAMMDD
           MOVE DMOVTO-RECOR     TO  WRK-DATA.
           MOVE WRK-DD           TO  WRK-EDIT-DD.
           MOVE WRK-MM           TO  WRK-EDIT-MM.
           MOVE WRK-AAAA         TO  WRK-EDIT-AAAA.
           MOVE WRK-EDIT-DATA-R  TO  RUEC66-DTMOVTO.

AC0513     IF WRK-CONTINUA  EQUAL  ZEROS
BRQ001        MOVE RUEC66-COR0001-22 TO
                   WRK-B09A-SIS(WRK-POSICAO:
                                LENGTH OF RUEC66-COR0001-22)
              ADD LENGTH OF RUEC66-COR0001-22 TO WRK-TAM
              ADD LENGTH OF RUEC66-COR0001-22 TO WRK-POSICAO
AC0513        GO TO 3150-99-FIM
AC0513     END-IF.

           IF WRK-CONTINUA  GREATER  ZEROS
AC0513        MOVE RUEC66-COR0001-22 TO
AC0513             WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                           LENGTH OF RUEC66-COR0001-22)
AC0513        ADD LENGTH OF RUEC66-COR0001-22 TO WRK-TAM2
AC0513        ADD LENGTH OF RUEC66-COR0001-22 TO WRK-POSICAO2
AC0513        GO TO 3150-99-FIM
AC0513     END-IF.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

EG0615*---------------------------------------------------------------*
EG0615 3155-FORMATA-SUBPROG            SECTION.
EG0615*---------------------------------------------------------------*
EG0615
EG0615     INITIALIZE RUECW824-REGISTRO.
EG0615
EG0615     MOVE CORIGE-REC             OF  RUFIB002
EG0615                                 TO  RUECW824-E-CORIGE-REC
EG0615     MOVE CFNALD-RURAL           OF  RUFIB002
EG0615                                 TO  RUECW824-E-CFNALD-RURAL
EG0615     MOVE CPRODT-RURAL           OF  RUFIB002
EG0615                                 TO  RUECW824-E-CPRODT-RURAL
EG0615     MOVE SPACES                 TO  RUECW824-E-RESERVA
EG0615
EG0615     MOVE 'RUEC8824'             TO  WRK-MODULO.
EG0615
EG0615     CALL  WRK-MODULO        USING   RUECW824-COPYBOOK
EG0615                                     ERRO-AREA
EG0615                                     SQLCA.
EG0615
EG0615     IF  RUECW824-COD-RETORNO    NOT EQUAL ZEROS
EG0615         MOVE 99                 TO RUEC65-COD-RET
EG0615         PERFORM 9000-ROTINA-FIM
EG0615     END-IF.
EG0615
EG0615     MOVE RUECW824-S-SUB-PROG-RURAL
EG0615                                 TO  RUEC66-CODSUB-PROG.
EG0615
EG0615*   -IF  WRK-CONTINUA            EQUAL  ZEROS
EG0615*       -MOVE RUEC66-COR0001-13  TO
EG0615*            -WRK-B09A-SIS(WRK-POSICAO:
EG0615*                                 -LENGTH OF RUEC66-COR0001-13)
EG0615*       -ADD LENGTH OF RUEC66-COR0001-13  TO WRK-TAM
EG0615*       -ADD LENGTH OF RUEC66-COR0001-13  TO WRK-POSICAO
EG0615*       -GO TO 3155-99-FIM
EG0615*   -END-IF.
EG0615*
EG0615*   -IF  WRK-CONTINUA            GREATER  ZEROS
EG0615*       -MOVE RUEC66-COR0001-13 TO
BSI999*            -WRK-B09A-SIS2(WRK-POSICAO2:
EG0615*                                 -LENGTH OF RUEC66-COR0001-13)
EG0615*       -ADD LENGTH OF RUEC66-COR0001-13  TO WRK-TAM2
EG0615*       -ADD LENGTH OF RUEC66-COR0001-13  TO WRK-POSICAO2
EG0615*       -GO TO 3155-99-FIM
EG0615*   -END-IF.
EG0615
EG0615*----------------------------------------------------------------*
EG0615 3155-99-FIM.                    EXIT.
EG0615*----------------------------------------------------------------*

050825*---------------------------------------------------------------*
.      3160-FORMATAR-TPCONFDD    SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE 'N'               TO WRK-TEM-TPCONFDD.
.
.          PERFORM 3161-OPEN-CSR01-RUFIB094.
.
.          PERFORM 3162-SELECT-RUFIB094.
.
.          PERFORM VARYING WRK-IND-B094     FROM  01 BY 01
.                    UNTIL WRK-CODE100-B094 EQUAL       'S'
.                       OR WRK-IND-B094     GREATER     99
.                  PERFORM 3164-BUSCA-TP-CONF
.                  PERFORM 3162-SELECT-RUFIB094
.          END-PERFORM.
.
.          PERFORM 3163-CLOSE-CSR01-RUFIB094.
.
.     *----------------------------------------------------------------*
.      3160-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3161-OPEN-CSR01-RUFIB094    SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE RUEC65-CCONTR         TO CCONTR
.                                        OF RUFIB094.
.
.          EXEC SQL
.             OPEN      CSR01-RUFIB094
.          END-EXEC.
.
.          IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
.             ( SQLWARN0       EQUAL  'W'   )
.              MOVE 99                   TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'TCONTR_RURAL_OUTRO' TO ERR-DBD-TAB
.              MOVE 'OPEN    '           TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '3015'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM   9000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3161-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3162-SELECT-RUFIB094 SECTION.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.             FETCH CSR01-RUFIB094    INTO
.                :RUFIB094.COUTRO-TPO-CONFM
.          END-EXEC.
.
.          IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0      EQUAL 'W')
.              MOVE 99                   TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'TCONTR_RURAL_OUTRO' TO ERR-DBD-TAB
.              MOVE 'SELECT'             TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '3016'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.          IF SQLCODE EQUAL +100
.             MOVE 'S' TO WRK-CODE100-B094
.             GO TO 3162-99-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3162-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3163-CLOSE-CSR01-RUFIB094    SECTION.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.             CLOSE     CSR01-RUFIB094
.          END-EXEC.
.
.          IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
.             ( SQLWARN0       EQUAL    'W'   )
.              MOVE 99                   TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
.              MOVE 'DB2'                TO ERR-TIPO-ACESSO
.              MOVE 'TCONTR_RURAL_OUTRO' TO ERR-DBD-TAB
.              MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
.              MOVE SQLCODE              TO ERR-SQL-CODE
.              MOVE '3017'               TO ERR-LOCAL
.              MOVE SPACES               TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3163-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3164-BUSCA-TP-CONF              SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE SPACES                 TO  CRURWPGI-E-ROUTRO-CONFM-BACEN
.                                          CRURWPGI-E-CSIT-REG-RURAL
.          MOVE 1                      TO  CRURWPGI-E-MAX-OCORR
.          MOVE 'I'                    TO  CRURWPGI-INDICADOR-PAGINACAO
.          MOVE COUTRO-TPO-CONFM       OF  RUFIB094
.                                      TO  WRK-S9-5A
.          MOVE WRK-9-5A               TO  CRURWPGI-E-COUTRO-TPO-CONFM
.
.          CALL  WRK-CRUR240G          USING WRK-AREA-CRUR240G.
.
.          IF CRURWPGI-E-COUTRO-TPO-CONFM EQUAL
.             CRURWPGI-S-COUTRO-TPO-CONFM(1)
.             IF CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.                                         NUMERIC
.                MOVE 'S'         TO WRK-TEM-TPCONFDD
.                MOVE CRURWPGI-S-COUTRO-CONFM-BACEN(1)
.                                 TO WRK-S9-2A
.                MOVE WRK-9-2A
.                                TO RUEC66-TPCONFDD-X
.                MOVE RUEC66-COR0001-21B  TO
.                         WRK-B09A-SIS (WRK-POSICAO:
.                                       LENGTH OF RUEC66-COR0001-21B)
.                ADD LENGTH OF RUEC66-COR0001-21B TO WRK-TAM
.                ADD LENGTH OF RUEC66-COR0001-21B TO WRK-POSICAO
.             END-IF
.          END-IF.
.
.     *---------------------------------------------------------------*
.      3164-99-FIM.                    EXIT.
.     *---------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3165-FORMATA-GRUPO-LIBRC  SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE ALL '*'       TO RUEC66-DTLIBRC-RECUR
.          MOVE ALL '*'       TO RUEC66-VLR-LIBRC-RECUR
.
.          MOVE RUEC66-COR0001-21C    TO
.                WRK-B09A-SIS(WRK-POSICAO:
.                                     LENGTH OF RUEC66-COR0001-21C)
.          ADD LENGTH OF RUEC66-COR0001-21C  TO WRK-TAM
.          ADD LENGTH OF RUEC66-COR0001-21C  TO WRK-POSICAO
.
.          MOVE RUEC66-COR0001-21C    TO
.                WRK-B09A-SIS2(WRK-POSICAO:
.                                     LENGTH OF RUEC66-COR0001-21C)
.          ADD LENGTH OF RUEC66-COR0001-21C  TO WRK-TAM2
.          ADD LENGTH OF RUEC66-COR0001-21C  TO WRK-POSICAO2.
.
.     *   -ADD  31            TO WRK-IND2.
.
.     *----------------------------------------------------------------*
.      3165-99-FIM.                    EXIT.
050825*----------------------------------------------------------------*

EG0615
PA1215*---------------------------------------------------------------*
EG0517 3201-OPEN-CSR01-RUECB040-JN     SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
EG0517          OPEN   CSR01-RUECB040-JN
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
PA1215         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3201'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215     MOVE 'N'                    TO WRK-FIM-B40.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3201-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
EG0517 3202-FETCH-CSR01-RUECB040-JN    SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
EG0517          FETCH  CSR01-RUECB040-JN
PA1215           INTO :RUECB040.CCONTR
PA1215               ,:RUECB040.CSEQ-MATR-RURAL
EG0517               ,:RUECB09N.NSEQ-GLEBA-IMOV
CAT408               ,:RUECB040.CMUN-RURAL
EG0517               ,:RUECB09N.NCORND-GEOGR-IMOV
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
PA1215         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3202'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215     IF  SQLCODE                 EQUAL +100
PA1215         MOVE 'S'                TO WRK-FIM-B40
PA1215     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3202-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
EG0517 3203-CLOSE-CSR01-RUECB040-JN    SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
EG0517          CLOSE  CSR01-RUECB040-JN
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'MATR_CDULA_RURAL' TO ERR-DBD-TAB
PA1215         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3203'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3203-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
PA1215 3204-FORMATAR-CSR01-RUECB040    SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     MOVE CCONTR                 OF RUECB040
PA1215                                 TO CCONTR          OF RUECB09N.
PA1215     MOVE CSEQ-MATR-RURAL        OF RUECB040
PA1215                                 TO CSEQ-MATR-RURAL OF RUECB09N.
EG0517     MOVE ZEROS                  TO WRK-SEQ-GLEBA-X19
EG0517     MOVE CSEQ-MATR-RURAL        OF RUECB09N
EG0517                                 TO WRK-SEQ-MATR-N03
EG0517     MOVE NSEQ-GLEBA-IMOV        OF RUECB09N
EG0517                                 TO WRK-SEQ-GLEBA-N05
EG0517     MOVE WRK-SEQ-GLEBA-X19      TO RUEC66-IDENTC-GLEBA
EG0517
211025     MOVE ALL '*'                TO RUEC66-AREA-NCULTVD

PA1215     MOVE RUEC66-COR0001-6       TO WRK-B09A-SIS(WRK-POSICAO:
PA1215                                    LENGTH OF RUEC66-COR0001-6)
PA1215     ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM
PA1215     ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO
PA1215
AC0513     MOVE RUEC66-COR0001-6       TO WRK-B09A-SIS2(WRK-POSICAO2:
AC0513                                    LENGTH OF RUEC66-COR0001-6)
AC0513     ADD LENGTH OF RUEC66-COR0001-6  TO WRK-TAM2
AC0513     ADD LENGTH OF RUEC66-COR0001-6  TO WRK-POSICAO2.
PA1215
PA1215
BR1811     IF  WRK-PRIMEIRA-COORD      NOT EQUAL 'N'
PA1215         PERFORM 3205-OPEN-CSR01-RUECB09N
PA1215         PERFORM 3206-FETCH-CSR01-RUECB09N
BR1811     END-IF.
PA1215
PA1215     IF  WRK-FIM-B9N             EQUAL  'S'
211025**      -NOVO CAMPO - SICOR 5.11
211025         MOVE ALL '*'            TO RUEC66-WKT-PART
211025         PERFORM 3138-FORMATA-GRUPO-PTPOLG
PA1215     END-IF.
PA1215
PA1215     MOVE 1 TO WRK-IND3
PA1215     PERFORM UNTIL WRK-FIM-B9N   EQUAL  'S'
BR1811*              -OR WRK-QTD-COORD EQUAL  90
211025                OR WRK-QTD-COORD EQUAL  350
BR1811             ADD 1               TO WRK-QTD-COORD
211025**           AQUI EH FORMATADA AS COORDENAS
PA1215             PERFORM 3208-FORMATAR-CSR01-RUECB09N
PA1215             PERFORM 3206-FETCH-CSR01-RUECB09N
PA1215     END-PERFORM.
PA1215
BR1811     IF  WRK-FIM-B9N         NOT EQUAL  'S'
BR1811         MOVE 'N'                TO WRK-PRIMEIRA-COORD
BR1811     ELSE
BR1811         MOVE 'S'                TO WRK-PRIMEIRA-COORD
PA1215         PERFORM 3207-CLOSE-CSR01-RUECB09N
BR1811     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3204-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
PA1215 3205-OPEN-CSR01-RUECB09N        SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
PA1215          OPEN   CSR01-RUECB09N
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'TCORND_GEOGR_CONTR'
PA1215                                 TO ERR-DBD-TAB
PA1215         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3205'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215     MOVE 'N'                    TO WRK-FIM-B9N.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3205-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
PA1215 3206-FETCH-CSR01-RUECB09N       SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
PA1215          FETCH  CSR01-RUECB09N
EG0517           INTO :RUECB09N.NCORND-GEOGR-IMOV
EG0517              , :RUECB09N.MLOGTD-IMOV-RURAL
PA1215              , :RUECB09N.MLATTD-IMOV-RURAL
PA1215              , :RUECB09N.MALTTD-IMOV-RURAL
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'TCORND_GEOGR_IMOV'
PA1215                                 TO ERR-DBD-TAB
PA1215         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3206'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215     IF  SQLCODE                 EQUAL +100
PA1215         MOVE 'S'                TO WRK-FIM-B9N
PA1215     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3206-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
PA1215 3207-CLOSE-CSR01-RUECB09N       SECTION.
PA1215*---------------------------------------------------------------*
PA1215
PA1215     EXEC SQL
PA1215          CLOSE  CSR01-RUECB09N
PA1215     END-EXEC.
PA1215
PA1215     IF (SQLCODE                 NOT EQUAL ZEROS) OR
PA1215        (SQLWARN0                    EQUAL 'W'  )
PA1215         MOVE 99                 TO RUEC65-COD-RET
PA1215         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
PA1215         MOVE 'DB2'              TO ERR-TIPO-ACESSO
PA1215         MOVE 'TCORND_GEOGR_CONTR'
PA1215                                 TO ERR-DBD-TAB
PA1215         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
PA1215         MOVE SQLCODE            TO ERR-SQL-CODE
PA1215         MOVE '3207'             TO ERR-LOCAL
PA1215         MOVE SPACES             TO ERR-SEGM
PA1215         PERFORM 9000-ROTINA-FIM
PA1215     END-IF.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3207-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
PA1215*---------------------------------------------------------------*
PA1215 3208-FORMATAR-CSR01-RUECB09N    SECTION.
PA1215*---------------------------------------------------------------*
PA1215
211025**      ADEQUACAO DE CAMPO - SICOR 5.11
211025**      REMOVENDO ALTITUDE
211025     ADD 1                       TO  WRK-QTD-CULTVD
211025
211025     MOVE MLOGTD-IMOV-RURAL      OF RUECB09N
211025                                 TO WRK-LOG-PONTO

211025     IF WRK-LOG-PONTO            LESS ZEROS
211025        MOVE WRK-LOG-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
211025                                    (WRK-QTD-CULTVD)
211025        MOVE '-'                 TO  WRK-CULTVD-LON
211025                                    (WRK-QTD-CULTVD)(1:1)
211025     ELSE
211025        MOVE WRK-LOG-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LON
211025                                    (WRK-QTD-CULTVD)
211025     END-IF

211025     INSPECT WRK-CULTVD-LON(WRK-QTD-CULTVD)
211025                                 REPLACING ALL ','
211025                                 BY '.'
      *
PA1215     MOVE MLATTD-IMOV-RURAL      OF RUECB09N
PA1215                                 TO WRK-LAT-PONTO

211025     IF WRK-LAT-PONTO            LESS ZEROS
211025        MOVE WRK-LAT-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
211025                                    (WRK-QTD-CULTVD)
211025        MOVE '-'                 TO  WRK-CULTVD-LAT
211025                                    (WRK-QTD-CULTVD)(1:1)
211025     ELSE
211025        MOVE WRK-LAT-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-CULTVD-LAT
211025                                    (WRK-QTD-CULTVD)
211025     END-IF

211025     INSPECT WRK-CULTVD-LAT(WRK-QTD-CULTVD)
211025                                 REPLACING ALL ','
211025                                 BY '.'

PA1215*   -MOVE WRK-LAT-PONTO-GRUPO    TO WRK-CULTVD-LON
PA1215*   -MOVE WRK-LAT-PONTO-GRUPO    TO RUEC66-LAT-PONTO
PA1215*
PA1215*   -MOVE MALTTD-IMOV-RURAL      OF RUECB09N
PA1215*                               -TO WRK-HEXA-ZDS5V2
BS0216*   -MOVE WRK-HEXA-ZDS4V2        TO WRK-ALT-PONTO.
BS0216*   -MOVE WRK-ALT-PONTO-GRUPO    TO RUEC66-ALTT-PONTO.
BS0216     MOVE ALL '*'                TO RUEC66-AREA-NCULTVD.
BR4.11*   -MOVE CMUN-RURAL             OF RUECB040
BR4.11*                                TO WRK-HEXA-ZDS9
BR4.11*   -MOVE WRK-HEXA-ZD6-X         TO RUEC66-COD-MUN-GLEB
PA1215*   -PERFORM 3138-FORMATA-GRUPO-PTPOLG.
PA1215
PA1215*----------------------------------------------------------------*
PA1215 3208-99-FIM.                    EXIT.
PA1215*----------------------------------------------------------------*
PA1215
211025**      EXCLUSAO DE CAMPO - SICOR 5.11
STEFAB*---------------------------------------------------------------*
.     *3238-FORMATA-GRUPO-PTPOLG   SECTION.
.     *---------------------------------------------------------------*
.     *
.     *   -MOVE RUEC66-COR0001-7BB     TO WRK-B09A-SIS(WRK-POSICAO:
.     *                                  -LENGTH OF RUEC66-COR0001-7BB)
.     *   -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-TAM.
.     *   -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-POSICAO.
.     *
.     *   -MOVE RUEC66-COR0001-7BB     TO WRK-B09A-SIS2(WRK-POSICAO2:
.     *                                  -LENGTH OF RUEC66-COR0001-7BB)
.     *   -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-TAM2.
.     *   -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-POSICAO2.
.     *
.     *----------------------------------------------------------------*
.     *3238-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *
      *----------------------------------------------------------------*
211025 3220-MONTAR-POLIGONO            SECTION.
  .   *----------------------------------------------------------------*
  .        IF WRK-QTD-CULTVD           EQUAL ZEROS
  .           MOVE ALL '*'             TO WRK-POLIGN
  .           GO                       TO 3220-99-FIM
  .        END-IF.
  .
  .        MOVE SPACE                  TO WRK-POLIGN
  .        MOVE 1                      TO WRK-TAM-POLIGN
  .
  .        MOVE 'POLYGON(('            TO WRK-POLIGN(WRK-TAM-POLIGN:9)
  .        ADD 9                       TO WRK-TAM-POLIGN
  .
  .        PERFORM                     VARYING IND-1
  .                                    FROM 1
  .                                    BY 1
  .                                    UNTIL IND-1
  .                                    GREATER WRK-QTD-CULTVD
  .           IF IND-1                 GREATER 1
  .              MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
  .              ADD 1                 TO WRK-TAM-POLIGN
  .           END-IF
  .
  .           MOVE WRK-PONTO-CULTVD(IND-1)
  .                                    TO WRK-POLIGN
  .                                      (WRK-TAM-POLIGN: LENGTH OF
  .                                       WRK-PONTO-CULTVD(IND-1))
  .           ADD LENGTH               OF WRK-PONTO-CULTVD(IND-1)
  .                                    TO WRK-TAM-POLIGN
  .        END-PERFORM.
  .
  .        MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1)
  .        ADD 1                       TO WRK-TAM-POLIGN
  .
  .        IF WRK-QTD-NCULTVD          GREATER ZEROS
  .           MOVE ',('                TO WRK-POLIGN(WRK-TAM-POLIGN:2)
  .           ADD 2                    TO WRK-TAM-POLIGN
  .        END-IF.
  .
  .        PERFORM                     VARYING IND-1
  .                                    FROM 1
  .                                    BY 1
  .                                    UNTIL IND-1
  .                                    GREATER WRK-QTD-NCULTVD
  .           IF IND-1                 GREATER 1
  .              MOVE ','              TO WRK-POLIGN(WRK-TAM-POLIGN:1)
  .              ADD 1                 TO WRK-TAM-POLIGN
  .           END-IF
  .
  .          MOVE WRK-PONTO-NCULTVD(IND-1)
  .                                    TO WRK-POLIGN
  .                                      (WRK-TAM-POLIGN:LENGTH OF
  .                                       WRK-PONTO-NCULTVD(IND-1))
  .
  .           ADD LENGTH               OF WRK-PONTO-NCULTVD(IND-1)
  .                                    TO WRK-TAM-POLIGN
  .        END-PERFORM.
  .
  .        IF WRK-QTD-NCULTVD          GREATER ZEROS
  .           MOVE ')'                 TO WRK-POLIGN(WRK-TAM-POLIGN:1)
  .           ADD 1                    TO WRK-TAM-POLIGN
  .        END-IF.
  .
  .        MOVE ')'                    TO WRK-POLIGN(WRK-TAM-POLIGN:1).
  .        ADD 1                       TO WRK-TAM-POLIGN.
  .
  .        MOVE  WRK-POLIGN            TO  RUEC66-WKT-PART
  .        MOVE  ZEROS                 TO  WRK-QTD-CULTVD
  .                                        WRK-QTD-NCULTVD
  .        ADD   1                     TO  WRK-NUM-POLIGN
  .
  .        PERFORM 3138-FORMATA-GRUPO-PTPOLG.
  .        PERFORM 3225-LIMPAR-POLIGONO.
  .
  .   *----------------------------------------------------------------*
211025 3220-99-FIM.  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
211025 3225-LIMPAR-POLIGONO            SECTION.
   .  *----------------------------------------------------------------*
   .       PERFORM                      VARYING WRK-IND5
   .                                  FROM 1
   .                                   BY 1
   .                                   UNTIL WRK-IND5
   .                                   GREATER WRK-QTD-CULTVD
   .         MOVE SPACES              TO WRK-PONTO-CULTVD(WRK-IND5)
   .       END-PERFORM
   .
   .       PERFORM                     VARYING WRK-IND5
   .                                   FROM 1
   .                                  BY 1
   .                                   UNTIL WRK-IND5
   .                                   GREATER WRK-QTD-NCULTVD
   .         MOVE SPACES              TO WRK-PONTO-NCULTVD(WRK-IND5)
   .       END-PERFORM
   .
   .       MOVE ZEROS                  TO WRK-QTD-CULTVD
   .                                      WRK-QTD-NCULTVD.
   .
   .  *----------------------------------------------------------------*
211025 3225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

.     *----------------------------------------------------------------*
.     *
.     *---------------------------------------------------------------*
.      3244-FORMATAR-CSR02-RUECB040    SECTION.
.     *---------------------------------------------------------------*
.
.          MOVE CCONTR                 OF RUECB040
.                                      TO CCONTR          OF RUECB09N.
.          MOVE CSEQ-MATR-RURAL        OF RUECB040
.                                      TO CSEQ-MATR-RURAL OF RUECB09N.
.
.          IF  WRK-PRIMEIRA-COORD-E    NOT EQUAL 'N'
.              PERFORM 3245-OPEN-CSR02-RUECB09N
.              PERFORM 3246-FETCH-CSR02-RUECB09N
.          END-IF.
.
.     *   -MOVE 1 TO WRK-IND3-E
.          PERFORM UNTIL WRK-FIM-B9N-E EQUAL  'S'
.     *              -OR WRK-QTD-COORD EQUAL  90
211025                OR WRK-QTD-COORD EQUAL  350
.                  ADD 1               TO WRK-QTD-COORD
211025**           AQUI EH FORMATADA AS COORDENAS "BURACOS"
211025**           NOVA ROTINA DEVE ESTAR POR AQUI
211025**           COMO SERA FEITA A CONTAGEM DO TAMANHO DO CAMPO 10K
.                  PERFORM 3248-FORMATAR-CSR02-RUECB09N
.                  PERFORM 3246-FETCH-CSR02-RUECB09N
.          END-PERFORM.
.
.          IF  WRK-FIM-B9N-E       NOT EQUAL  'S'
.              MOVE 'N'                TO WRK-PRIMEIRA-COORD-E
.          ELSE
.              MOVE 'S'                TO WRK-PRIMEIRA-COORD-E
.              PERFORM 3247-CLOSE-CSR02-RUECB09N
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3244-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3245-OPEN-CSR02-RUECB09N        SECTION.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.               OPEN   CSR02-RUECB09N
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS) OR
.             (SQLWARN0                    EQUAL 'W'  )
.              MOVE 99                 TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
.              MOVE 'DB2'              TO ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_CONTR'
.                                  TO ERR-DBD-TAB
.              MOVE 'OPEN    '         TO ERR-FUN-COMANDO
.              MOVE SQLCODE            TO ERR-SQL-CODE
.              MOVE '3205'             TO ERR-LOCAL
.              MOVE SPACES             TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.          MOVE 'N'                    TO WRK-FIM-B9N-E.
.
.     *----------------------------------------------------------------*
.      3245-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3246-FETCH-CSR02-RUECB09N       SECTION.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.               FETCH  CSR02-RUECB09N
.                INTO :RUECB09N.NCORND-GEOGR-IMOV
.                   , :RUECB09N.MLOGTD-IMOV-RURAL
.                   , :RUECB09N.MLATTD-IMOV-RURAL
.                   , :RUECB09N.MALTTD-IMOV-RURAL
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                    EQUAL 'W'  )
.              MOVE 99                 TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
.              MOVE 'DB2'              TO ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_IMOV'
.                                      TO ERR-DBD-TAB
.              MOVE 'FETCH   '         TO ERR-FUN-COMANDO
.              MOVE SQLCODE            TO ERR-SQL-CODE
.              MOVE '3206'             TO ERR-LOCAL
.              MOVE SPACES             TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.          IF  SQLCODE                 EQUAL +100
.              MOVE 'S'                TO WRK-FIM-B9N-E
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3246-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
.     *---------------------------------------------------------------*
.      3247-CLOSE-CSR02-RUECB09N       SECTION.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.               CLOSE  CSR02-RUECB09N
.          END-EXEC.
.
.          IF (SQLCODE                 NOT EQUAL ZEROS) OR
.             (SQLWARN0                    EQUAL 'W'  )
.              MOVE 99                 TO RUEC65-COD-RET
.              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
.              MOVE 'DB2'              TO ERR-TIPO-ACESSO
.              MOVE 'TCORND_GEOGR_CONTR'
.                                      TO ERR-DBD-TAB
.              MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
.              MOVE SQLCODE            TO ERR-SQL-CODE
.              MOVE '3207'             TO ERR-LOCAL
.              MOVE SPACES             TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3247-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.
211025**      MONTA VALORES PARA AREA NAO CULTIVADA
.     *---------------------------------------------------------------*
.      3248-FORMATAR-CSR02-RUECB09N    SECTION.
.     *---------------------------------------------------------------*
.     *
211025     ADD 1                       TO  WRK-QTD-NCULTVD
211025
211025     MOVE MLOGTD-IMOV-RURAL      OF RUECB09N
211025                                 TO WRK-LOG-PONTO.
      *
211025     IF WRK-LOG-PONTO            LESS ZEROS
211025        MOVE WRK-LOG-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO WRK-NCULTVD-LON
211025                                    (WRK-QTD-NCULTVD)
211025        MOVE '-'                 TO WRK-NCULTVD-LON
211025                                    (WRK-QTD-NCULTVD)(1:1)
211025     ELSE
211025        MOVE WRK-LOG-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-NCULTVD-LON
211025                                    (WRK-QTD-NCULTVD)
211025     END-IF

211025     INSPECT WRK-NCULTVD-LON(WRK-QTD-NCULTVD)
211025                                 REPLACING ALL ','
211025                                 BY '.'
      *
PA1215     MOVE MLATTD-IMOV-RURAL      OF RUECB09N
PA1215                                 TO WRK-LAT-PONTO

211025     IF WRK-LAT-PONTO            LESS ZEROS
211025        MOVE WRK-LAT-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-NCULTVD-LAT
211025                                    (WRK-QTD-NCULTVD)
211025        MOVE '-'                 TO  WRK-NCULTVD-LAT
211025                                    (WRK-QTD-NCULTVD)(1:1)
211025     ELSE
211025        MOVE WRK-LAT-PONTO-9V7   TO WRK-MASK-COORD
211025        MOVE WRK-MASK-COORD-X    TO  WRK-NCULTVD-LAT
211025                                    (WRK-QTD-NCULTVD)
211025     END-IF

211025     INSPECT WRK-NCULTVD-LAT(WRK-QTD-NCULTVD)
211025                                 REPLACING ALL ','
211025                                 BY '.'
.     *   -MOVE WRK-HEXA-ZDS2V11       TO WRK-LAT-PONTO
.     *   -MOVE WRK-LAT-PONTO-GRUPO    TO RUEC66-LAT-PONTO-EXCL
.     *
.     *   -MOVE MALTTD-IMOV-RURAL      OF RUECB09N
.     *                               -TO WRK-HEXA-ZDS5V2
.     *   -MOVE WRK-HEXA-ZDS4V2        TO WRK-ALT-PONTO.
.     *   -MOVE WRK-ALT-PONTO-GRUPO    TO RUEC66-ALTT-PONTO-EXCL
.     *   -PERFORM 3238-FORMATA-GRUPO-PTPOLG.
.          .
.     *
.     *----------------------------------------------------------------*
.      3248-99-FIM.                    EXIT.
STEFAB*----------------------------------------------------------------*
PA1215
BS0216*---------------------------------------------------------------*
BS0216 3300-OPEN-CSR01-RUECV025        SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216          OPEN   CSR01-RUECV025
BS0216     END-EXEC.
BS0216
BS0216     IF (SQLCODE                 NOT EQUAL ZEROS) OR
BS0216        (SQLWARN0                    EQUAL 'W'  )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01ORCAM_CDULA  ' TO ERR-DBD-TAB
BS0216         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3300'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     MOVE 'N'                    TO WRK-FIM-V25.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3300-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BS0216*---------------------------------------------------------------*
BS0216 3310-FETCH-CSR01-RUECV025       SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216          FETCH  CSR01-RUECV025
BS0216           INTO :RUECV025.DLIBRC-OPER-ORCAM
BS0216                :WRK-DLIBRC-OP-ORCAM-NULL,
BS0216                :RUECV025.RTPO-OPER-ORCAM
RE0517           ,    :RUECV025.DFIM-EVNTO-CDULA
RE0517                :WRK-DFIM-EVNTO-CDULA-NULL
BS0216     END-EXEC.
BS0216
BS0216     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BS0216        (SQLWARN0                    EQUAL 'W'  )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01ORCAM_CDULA  ' TO ERR-DBD-TAB
BS0216         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3310'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE                 EQUAL +100
BS0216         MOVE 'S'                TO WRK-FIM-V25
EG0617         GO TO 3310-99-FIM
BS0216     END-IF.
BS0216
BS0216     IF  WRK-DLIBRC-OP-ORCAM-NULL LESS ZEROS
BS0216         MOVE SPACES             TO DLIBRC-OPER-ORCAM OF RUECV025
BS0216     END-IF.
BS0216
BS0216     IF  WRK-DFIM-EVNTO-CDULA-NULL  LESS ZEROS
BS0216         MOVE SPACES             TO DFIM-EVNTO-CDULA  OF RUECV025
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3310-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BS0216*---------------------------------------------------------------*
BS0216 3320-CLOSE-CSR01-RUECV025       SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216          CLOSE  CSR01-RUECV025
BS0216     END-EXEC.
BS0216
BS0216     IF (SQLCODE                 NOT EQUAL ZEROS) OR
BS0216        (SQLWARN0                    EQUAL 'W'  )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01ORCAM_CDULA  ' TO ERR-DBD-TAB
BS0216         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3320'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3320-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216*----------------------------------------------------------------*
BS0216 3330-SEL-RUIAV007 SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     MOVE CPRODT-RURAL    OF RUFIB002
BS0216                             TO CPRODT-RURAL        OF RUIAV007.
BS0216     MOVE CPROJ-RURAL     OF RURCV014
BS0216                             TO CPROJ-RURAL         OF RUIAV007.
BS0216     MOVE CUND-MEDD-PROVV OF RUFIB002
BS0216                             TO CUND-MEDD           OF RUIAV007.
BS0216
BS0216     COMPUTE QMIN-FAIXA-PROTV  OF RUIAV007
BS0216       EQUAL QUND-PROD-PROVV   OF RUFIB002 * 100
BS0216
BS0216     COMPUTE QMAX-FAIXA-PROTV  OF RUIAV007
BS0216       EQUAL QUND-PROD-PROVV   OF RUFIB002 * 100
BS0216
BS0216     EXEC SQL
BS0216        SELECT
BS0216             MAX (DINIC_VGCIA)
BS0216        INTO
BS0216            :RUIAV007.DINIC-VGCIA
BS0216        FROM DB2PRD.V01ORCAMENTO_RURAL
BS0216        WHERE
BS0216             CPRODT_RURAL       = :RUIAV007.CPRODT-RURAL     AND
BS0216             CPROJ_RURAL        = :RUIAV007.CPROJ-RURAL      AND
BS0216             QMIN_FAIXA_PROTV  <= :RUIAV007.QMIN-FAIXA-PROTV AND
BS0216             QMAX_FAIXA_PROTV  >= :RUIAV007.QMAX-FAIXA-PROTV AND
BS0216             CUND_MEDD          = :RUIAV007.CUND-MEDD        AND
BS0216             DINIC_VGCIA       <=  CURRENT DATE
BS0216
BS0216     END-EXEC.
BS0216
BS0216     IF    ( SQLCODE   NOT  =    ZEROS AND +100 AND -305)  OR
BS0216           ( SQLWARN0       =    'W'   )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01ORCAMENTO_RURAL' TO ERR-DBD-TAB
BS0216         MOVE 'SELECT  '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3330'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.

BS0216     IF    SQLCODE  EQUAL +100
              CONTINUE
BS0216     END-IF.

BS0216*----------------------------------------------------------------*
BS0216 3330-99-SEL-RUIAV007. EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3340-SEL-RUIAV007-2 SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     MOVE CPRODT-RURAL    OF RUFIB002
BS0216                             TO CPRODT-RURAL        OF RUIAV007.
BS0216     MOVE CPROJ-RURAL     OF RURCV014
BS0216                             TO CPROJ-RURAL         OF RUIAV007.
BS0216     MOVE CUND-MEDD-PROVV OF RUFIB002
BS0216                             TO CUND-MEDD           OF RUIAV007.
BS0216
BS0216     COMPUTE QMIN-FAIXA-PROTV  OF RUIAV007
BS0216       EQUAL QUND-PROD-PROVV   OF RUFIB002 * 100
BS0216
BS0216     COMPUTE QMAX-FAIXA-PROTV  OF RUIAV007
BS0216       EQUAL QUND-PROD-PROVV   OF RUFIB002 * 100
BS0216
BS0216     EXEC SQL
BS0216        SELECT
BS0216             QMIN_FAIXA_PROTV,
BS0216             QMAX_FAIXA_PROTV
BS0216        INTO
BS0216            :RUIAV007.QMIN-FAIXA-PROTV,
BS0216            :RUIAV007.QMAX-FAIXA-PROTV
BS0216        FROM DB2PRD.V01ORCAMENTO_RURAL
BS0216        WHERE
BS0216             CPRODT_RURAL       = :RUIAV007.CPRODT-RURAL     AND
BS0216             CPROJ_RURAL        = :RUIAV007.CPROJ-RURAL      AND
BS0216             QMIN_FAIXA_PROTV  <= :RUIAV007.QMIN-FAIXA-PROTV AND
BS0216             QMAX_FAIXA_PROTV  >= :RUIAV007.QMAX-FAIXA-PROTV AND
BS0216             CUND_MEDD          = :RUIAV007.CUND-MEDD        AND
BS0216             DINIC_VGCIA        = :RUIAV007.DINIC-VGCIA
BS0216
BS0216     END-EXEC.
BS0216
BS0216     IF    ( SQLCODE   NOT  =    ZEROS AND +100 )  OR
BS0216           ( SQLWARN0       =    'W'   )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01ORCAMENTO_RURAL' TO ERR-DBD-TAB
BS0216         MOVE 'SELECT  '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3330'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE EQUAL +100
BS0216         GO TO 3340-99-SEL-RUIAV007-2
BS0216     END-IF.
BS0216*----------------------------------------------------------------*
BS0216 3340-99-SEL-RUIAV007-2. EXIT.
BS0216*----------------------------------------------------------------*
BS0216*----------------------------------------------------------------*
BS0216 3350-OPEN-CSR01-RUIAV008-JN SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     MOVE CPROJ-RURAL     OF RURCV014 TO CPROJ-RURAL  OF RUIAV008.
BS0216     MOVE CPRODT-RURAL    OF RUFIB002 TO CPRODT-RURAL OF RUIAV008.
BS0216     MOVE QMIN-FAIXA-PROTV OF RUIAV007   TO
BS0216                                   QMIN-FAIXA-PROTV   OF RUIAV008.
BS0216     MOVE DINIC-VGCIA OF RUIAV007     TO DINIC-VGCIA  OF RUIAV008.
BS0216
BS0216
BS0216     EXEC SQL
BS0216              OPEN CSR01-RUIAV008-JN
BS0216     END-EXEC.
BS0216
BS0216     IF    ( SQLCODE   NOT  =    ZEROS )   OR
BS0216           ( SQLWARN0       =    'W'   )
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01PCELA_ORCAM    ' TO ERR-DBD-TAB
BS0216         MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3350'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3350-99-FIM. EXIT.
BS0216*----------------------------------------------------------------*
BS0216*----------------------------------------------------------------*
BS0216 3360-LER-CSR01-RUIAV008-JN SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216        FETCH  CSR01-RUIAV008-JN
BS0216        INTO   :RUIAV008.CTPO-OPER-ORCAM,
BS0216               :RUIAV008.DINIC-VGCIA-PCELA,
BS0216               :RUIAV008.DFIM-VGCIA,
BS0216               :RUIAV008.VPCELA-LIBRC,
BS0216               :RUIAV005.RTPO-OPER-ORCAM
BS0216     END-EXEC.
BS0216
BS0216     IF (SQLCODE NOT EQUAL ZEROS AND +100)  OR
BS0216        (SQLWARN0    EQUAL 'W')
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01PCELA_ORCAM    ' TO ERR-DBD-TAB
BS0216         MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3360'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE                 EQUAL +100
BS0216         MOVE 'S'                TO WRK-FIM-RUIA
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3360-99-FIM. EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3370-CLOSE-CSR01-RUIAV008-JN SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     EXEC SQL
BS0216          CLOSE CSR01-RUIAV008-JN
BS0216     END-EXEC.
BS0216
BS0216     IF (SQLCODE                 NOT EQUAL   0) OR
BS0216        (SQLWARN0                EQUAL      'W')
BS0216         MOVE 99                 TO RUEC65-COD-RET
BS0216         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216         MOVE 'V01PCELA_ORCAM    ' TO ERR-DBD-TAB
BS0216         MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BS0216         MOVE SQLCODE            TO ERR-SQL-CODE
BS0216         MOVE '3370'             TO ERR-LOCAL
BS0216         MOVE SPACES             TO ERR-SEGM
BS0216         PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE                 EQUAL +100
BS0216         MOVE 'S'                TO WRK-FIM-RUIA
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3370-99-FIM. EXIT.
BS0216*----------------------------------------------------------------*
BS0216*---------------------------------------------------------------*
BS0216 3380-SELECT-RURCV014      SECTION.
BS0216*---------------------------------------------------------------*
BS0216
BS0216     MOVE CMUN-RURAL OF RUFIB002   TO CMUN-RURAL OF RURCV014
BS0216
BS0216     EXEC SQL
BS0216        SELECT
BS0216             CPROJ_RURAL
BS0216        INTO
BS0216            :RURCV014.CPROJ-RURAL
BS0216        FROM DB2PRD.V01MUNICIPIO_RURAL
BS0216        WHERE
BS0216             CMUN_RURAL         = :RURCV014.CMUN-RURAL
BS0216     END-EXEC.
BS0216
BS0216     IF    ( SQLCODE   NOT  =    ZEROS AND +100)   OR
BS0216           ( SQLWARN0       =    'W'   )
BS0216             MOVE 99                 TO RUEC65-COD-RET
BS0216             MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216             MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216             MOVE 'V01MUNICIPIO_RURAL' TO ERR-DBD-TAB
BS0216             MOVE 'SELECT  '         TO ERR-FUN-COMANDO
BS0216             MOVE SQLCODE            TO ERR-SQL-CODE
BS0216             MOVE '3380'             TO ERR-LOCAL
BS0216             MOVE SPACES             TO ERR-SEGM
BS0216             PERFORM 9000-ROTINA-FIM
BS0216     END-IF.

           IF    SQLCODE  EQUAL +100
              CONTINUE
           END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3380-99-FIM. EXIT.
BS0216*----------------------------------------------------------------*
BS0216*----------------------------------------------------------------*
BS0216 3390-CHECAR-PRODUTO-PERENE      SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216**** CTEMPO-VIDA-CULTR  = 0 - NAO PERENE
BS0216**** CTEMPO-VIDA-CULTR  = 1 - PERENE
BS0216
BS0216     MOVE CPRODT-RURAL OF RUFIB002 TO CPRODT-RURAL OF RURCB006
BS0216
BS0216     EXEC SQL
BS0216       SELECT
BS0216         CTEMPO_VIDA_CULTR
BS0216       INTO
BS0216         :RURCB006.CTEMPO-VIDA-CULTR :WRK-N-CTEMPO-VIDA
BS0216       FROM  DB2PRD.PRODUTO_RURAL
BS0216       WHERE CPRODT_RURAL        = :RURCB006.CPRODT-RURAL
BS0216     END-EXEC.
BS0216
BS0216     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100 )
BS0216     OR  SQLWARN0                EQUAL 'W'
BS0216             MOVE 99                 TO RUEC65-COD-RET
BS0216             MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216             MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216             MOVE 'PRODUTO_RURAL'    TO ERR-DBD-TAB
BS0216             MOVE 'SELECT'           TO ERR-FUN-COMANDO
BS0216             MOVE SQLCODE            TO ERR-SQL-CODE
BS0216             MOVE '3390'             TO ERR-LOCAL
BS0216             MOVE SPACES             TO ERR-SEGM
BS0216             PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE                 EQUAL +100
BS0216         MOVE  ZEROS         TO  CTEMPO-VIDA-CULTR    OF RURCB006
BS0216     END-IF.
BS0216
BS0216     IF  WRK-N-CTEMPO-VIDA       NOT EQUAL ZEROS
BS0216         MOVE  ZEROS         TO  CTEMPO-VIDA-CULTR    OF RURCB006
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3390-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3410-CHECAR-FINALIDADE-BACEN    SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     MOVE CFNALD-RURAL OF RUFIB002 TO CFNALD-RURAL OF RURCB003
BS0216
BS0216     EXEC SQL
BS0216       SELECT
BS0216         CFNALD_RECOR
BS0216       INTO
BS0216         :RURCB003.CFNALD-RECOR
BS0216       FROM  DB2PRD.FNALD_RURAL
BS0216       WHERE CFNALD_RURAL        = :RURCB003.CFNALD-RURAL
BS0216     END-EXEC.
BS0216
BS0216     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100 )
BS0216     OR  SQLWARN0                EQUAL 'W'
BS0216             MOVE 99                 TO RUEC65-COD-RET
BS0216             MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216             MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216             MOVE 'FNALD_RURAL'      TO ERR-DBD-TAB
BS0216             MOVE 'SELECT'           TO ERR-FUN-COMANDO
BS0216             MOVE SQLCODE            TO ERR-SQL-CODE
BS0216             MOVE '3410'             TO ERR-LOCAL
BS0216             MOVE SPACES             TO ERR-SEGM
BS0216             PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216     IF  SQLCODE                 EQUAL +100
BS0216         MOVE SPACES             TO CFNALD-RECOR OF RURCB003
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3410-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216*----------------------------------------------------------------*
BS0216 3420-PERMITE-ALTERACAO          SECTION.
BS0216*----------------------------------------------------------------*
BS0216
BS0216     MOVE CCDULA      OF RUFIB002   TO CCDULA          OF RUECB007
BS0216     MOVE CJUNC-DEPDC OF RUFIB002   TO CJUNC-DEPDC     OF RUECB007
BS0216     MOVE 0                         TO CIMPRE-CDULA-TEMPR
BS0216                                                       OF RUECB007
BS0216
BS0216     EXEC SQL
BS0216         UPDATE DB2PRD.CEDULA_TEMPR
BS0216            SET CIMPRE_CDULA_TEMPR = :RUECB007.CIMPRE-CDULA-TEMPR
BS0216         WHERE  CJUNC_DEPDC        = :RUECB007.CJUNC-DEPDC
BS0216           AND  CCDULA             = :RUECB007.CCDULA
BS0216     END-EXEC.
BS0216
BS0216     IF  ( SQLCODE               NOT EQUAL ZEROS) OR
BS0216         SQLWARN0                EQUAL 'W'
BS0216             MOVE 99                 TO RUEC65-COD-RET
BS0216             MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BS0216             MOVE 'DB2'              TO ERR-TIPO-ACESSO
BS0216             MOVE 'CEDULA_TEMPR    ' TO ERR-DBD-TAB
BS0216             MOVE 'UPDATE'           TO ERR-FUN-COMANDO
BS0216             MOVE SQLCODE            TO ERR-SQL-CODE
BS0216             MOVE '3420'             TO ERR-LOCAL
BS0216             MOVE SPACES             TO ERR-SEGM
BS0216             PERFORM 9000-ROTINA-FIM
BS0216     END-IF.
BS0216
BS0216*----------------------------------------------------------------*
BS0216 3420-99-FIM.                    EXIT.
BS0216*----------------------------------------------------------------*
BS0216
BR4.11*---------------------------------------------------------------*
BR4.11*3500-FORMATA-GRUPO-DCLAS  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*
BR4.11*   -PERFORM 3510-OPEN-CSR01-RUFIB044.
BR4.11*
BR4.11*   -PERFORM 3520-FETCH-CSR01-RUFIB044.
BR4.11*
BR4.11*   -IF  WRK-FIM-B97             EQUAL  'S'
BR4.11*       -MOVE ALL '*'            TO RUEC66-DT-DESCLASS
BR4.11*       -MOVE ALL '*'            TO RUEC66-VLR-DESCLASS
BR4.11*       -MOVE ALL '*'            TO RUEC66-MTV-DESCLASS
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -PERFORM UNTIL WRK-FIM-B97   EQUAL  'S'
BR4.11*           -PERFORM 3525-OBTER-MTV-DESCLASS
BR4.11*           -PERFORM 3520-FETCH-CSR01-RUFIB044
BR4.11*           -ADD 1               TO WRK-IND2
BR4.11*   -END-PERFORM.
BR4.11*
BR4.11*   -IF  WRK-FIM-B97             EQUAL  'S'
BR4.11*       -ADD 999                 TO WRK-IND2
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -PERFORM 3530-CLOSE-CSR01-RUFIB044.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3500-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*

BR4.11*----------------------------------------------------------------*
BR4.11*3510-OPEN-CSR01-RUFIB044    SECTION.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*   -MOVE CCONTR OF RUFIB002     TO CCONTR        OF RUFIB044.
BR4.11*   -MOVE ZEROS                  TO NMOTVO-DCLAS-CONTR
BR4.11*                                                 OF RUFIB044.
BR4.11*
BR4.11*   -EXEC SQL
BR4.11*            -OPEN CSR01-RUFIB044
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF    ( SQLCODE   NOT  =    ZEROS )   OR
BR4.11*          ( SQLWARN0       =    'W'   )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TMOTVO_DCLAS_CONTR' TO ERR-DBD-TAB
BR4.11*       -MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3510'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3510-99-FIM. EXIT.
BR4.11*----------------------------------------------------------------*

BR4.11*----------------------------------------------------------------*
BR4.11*3520-FETCH-CSR01-RUFIB044  SECTION.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*   -EXEC SQL
BR4.11*      -FETCH  CSR01-RUFIB044
BR4.11*      -INTO   :RUFIB044.CMOTVO-DCLAS-OPER,
BR4.11*              :RUFIB044.DDCLAS-OPER-CREDT,
BR4.11*              :RUFIB044.VPRINC-DCLAS-CONTR
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE NOT EQUAL ZEROS AND +100)  OR
BR4.11*      -(SQLWARN0    EQUAL 'W')
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TMOTVO_DCLAS_CONTR' TO ERR-DBD-TAB
BR4.11*       -MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3520'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -IF  SQLCODE                 EQUAL +100
BR4.11*       -MOVE 'S'                TO WRK-FIM-B97
BR4.11*   -END-IF.
BR4.11*
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3520-99-FIM. EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3525-OBTER-MTV-DESCLASS     SECTION.
BR4.11*---------------------------------------------------------------*

BR4.11*   -MOVE CMOTVO-DCLAS-OPER OF RUFIB044
BR4.11*                               TO CMOTVO-DCLAS-OPER OF RURCB097
BR4.11*
BR4.11*   -EXEC SQL
BR4.11*   -SELECT CMOTVO_DCLAS_BACEN
BR4.11*   -INTO   :RURCB097.CMOTVO-DCLAS-BACEN
BR4.11*   -FROM  DB2PRD.TMOTVO_DCLAS_OPER
BR4.11*   -WHERE CMOTVO_DCLAS_OPER= :RURCB097.CMOTVO-DCLAS-OPER
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
BR4.11*      -(SQLWARN0      EQUAL 'W')
BR4.11*       -MOVE 99                   TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TMOTVO_DCLAS_OPER'  TO ERR-DBD-TAB
BR4.11*       -MOVE 'SELECT'             TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE              TO ERR-SQL-CODE
BR4.11*       -MOVE '3525'               TO ERR-LOCAL
BR4.11*       -MOVE SPACES               TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -IF (SQLCODE       EQUAL  +100)
BR4.11*       -MOVE 06                   TO RUEC65-COD-RET
BR4.11*       -MOVE 'MTVO DE DESCLASSIFICACAO NAO ENCONTRADO B098'
BR4.11*                                  TO RUEC65-MENSAGEM
BR4.11*       -MOVE SPACES               TO RUEC65-SQLCA
BR4.11*       -MOVE SPACES               TO RUEC65-ERRO-AREA
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -MOVE CMOTVO-DCLAS-BACEN       OF RURCB097
BR4.11*                                  TO RUEC66-MTV-DESCLASS
BR4.11*   -MOVE VPRINC-DCLAS-CONTR       OF RUFIB044
BR4.11*                                  TO WRK-VLR15
BR4.11*   -MOVE WRK-VLR15-CHAR           TO WRK-VLR15-PARC.
BR4.11*   -MOVE WRK-VLR1                 TO RUEC66-VLR-DESCLASS
BR4.11*   -MOVE DDCLAS-OPER-CREDT        OF RUFIB044
BR4.11*                                  TO WRK-DATA
BR4.11*   -MOVE WRK-DD                   TO WRK-EDIT-DD
BR4.11*   -MOVE WRK-MM                   TO WRK-EDIT-MM
BR4.11*   -MOVE WRK-AAAA                 TO WRK-EDIT-AAAA
BR4.11*   -MOVE WRK-EDIT-DATA            TO RUEC66-DT-DESCLASS.
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3525-99-FIM.                EXIT.
BR4.11*---------------------------------------------------------------*

BR4.11*----------------------------------------------------------------*
BR4.11*3530-CLOSE-CSR01-RUFIB044    SECTION.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*   -EXEC SQL
BR4.11*        -CLOSE CSR01-RUFIB044
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL   0) OR
BR4.11*      -(SQLWARN0                EQUAL      'W')
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TMOTVO_DCLAS_CONTR' TO ERR-DBD-TAB
BR4.11*       -MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3530'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3530-99-FIM. EXIT.
BR4.11*----------------------------------------------------------------*
CAT408
CAT408*----------------------------------------------------------------*
CAT408 3540-OBTER-BNEFC-RURAL          SECTION.
CAT408*----------------------------------------------------------------*
CAT408
CAT408     INITIALIZE CMRUW09I-REGISTRO
CAT408                CMRUW09I-S-ERRO-AREA
CAT408                CMRUW09I-S-SQLCA
CAT408
CAT408     MOVE CTPO-BNEFC-RURAL       OF RUFIB002
CAT408                                 TO CMRUW09I-E-CTPO-BNEFC-RURAL.
CAT408     MOVE 'CMRU307C'             TO WRK-MODULO.
CAT408
CAT408     CALL WRK-MODULO-CMRU307C    USING WRK-AREA-CMRU307C.
CAT408
CAT408     EVALUATE CMRUW09I-COD-RETORNO
CAT408         WHEN ZEROS
CAT408              MOVE CMRUW09I-S-CTPO-BNEFC-BACEN
CAT408                                        TO RUEC66-TPO-BENF
BR4.12                                           WRK-BENEF
CAT408         WHEN 04
CAT408         WHEN 08
CAT408              MOVE ALL '*'              TO RUEC66-TPO-BENF
CAT408
CAT408         WHEN OTHER
CAT408              MOVE 99                   TO RUEC65-COD-RET
CAT408              MOVE CMRUW09I-S-SQLCA     TO RUEC65-SQLCA
CAT408              MOVE CMRUW09I-S-ERRO-AREA TO RUEC65-ERRO-AREA
CAT408              PERFORM 9000-ROTINA-FIM
CAT408
CAT408     END-EVALUATE.
CAT408
CAT408*----------------------------------------------------------------*
CAT408 3540-99-FIM.                    EXIT.
CAT408*----------------------------------------------------------------*
CAT408
      *---------------------------------------------------------------*
       4000-INSERT-B09A          SECTION.
      *---------------------------------------------------------------*

           IF WRK-CONTINUA  GREATER  ZEROS
              ADD  1                   TO WRK-MAX-NREG
              MOVE WRK-MAX-NREG        TO NREG-MSGEM-RECOR
                                       OF RUECB09A
7c2511*      -MOVE WRK-TAM2            TO WRK-TAMANHO-SISMSG
7c2511        MOVE WRK-TAM             TO WRK-TAMANHO-SISMSG
AC0513        MOVE WRK-TAMANHO-SISMSG  TO WCONTD-MSGEM-RURAL-LEN
AC0513                                 OF RUECB09A
AC0513        MOVE WRK-B09A-SIS2       TO WCONTD-MSGEM-RURAL-TEXT
AC0513                                 OF RUECB09A
              GO TO 4000-01-CONTINUA
           END-IF.

           PERFORM 4010-SELMAX-B09A.

           IF SQLCODE     EQUAL  -305
              MOVE ZEROS              TO  WRK-MAX-NREG
              ADD  1                  TO  WRK-MAX-NREG
           ELSE
              IF SQLCODE  EQUAL  ZEROS
                 MOVE NREG-MSGEM-RECOR   TO  WRK-MAX-NREG
                 ADD  1                  TO  WRK-MAX-NREG
              END-IF
           END-IF.

           EXEC SQL
           SET :WRK-TIMESTAMP-B09A = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP-B09A    TO HINCL
                                         OF RUECB09A.

7C0120     MOVE WRK-PRIM-NUMCTRLIF    TO WRK-B09A-NUMCTRLIF

           MOVE WRK-MAX-NREG          TO NREG-MSGEM-RECOR
                                         OF RUECB09A.

           MOVE 1                     TO CSIT-MSGEM
                                         OF RUECB09A.

           MOVE WRK-TAM               TO WRK-TAMANHO-SISMSG.

           MOVE WRK-TAMANHO-SISMSG    TO WCONTD-MSGEM-RURAL-LEN
                                         OF RUECB09A.

           MOVE WRK-B09A-SIS          TO WCONTD-MSGEM-RURAL-TEXT
                                         OF RUECB09A.

AC0513*----------------------------------------------------------------*
AC0513* INSERIR CONTINUACAO                                            *
AC0513*----------------------------------------------------------------*
AC0513 4000-01-CONTINUA.

           EXEC SQL
           INSERT INTO DB2PRD.TENVIO_RECOR_TEMPR
                  (CCONTR_CREDT_RURAL,
                   NENVIO_RECOR,
                   NREG_MSGEM_RECOR,
                   CSIT_MSGEM,
                   HINCL,
                   WCONTD_MSGEM_RURAL)
           VALUES
                  (:RUECB09A.CCONTR-CREDT-RURAL,
                   :RUECB09A.NENVIO-RECOR,
                   :RUECB09A.NREG-MSGEM-RECOR,
                   :RUECB09A.CSIT-MSGEM,
                   :RUECB09A.HINCL,
                   :RUECB09A.WCONTD-MSGEM-RURAL)
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'INSERT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4000'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4010-SELMAX-B09A               SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.

           EXEC SQL
           SELECT MAX(NREG_MSGEM_RECOR)
           INTO :RUECB09A.NREG-MSGEM-RECOR
           FROM DB2PRD.TENVIO_RECOR_TEMPR
           WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
           AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
           AND    NREG_MSGEM_RECOR > 0
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS  AND
               SQLCODE   NOT EQUAL -305   AND
               SQLCODE   NOT EQUAL +100)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4010'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF    SQLCODE  EQUAL +100
              CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       4010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-ENVIO                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 4140-COUNT-RUECB09A.

           PERFORM 4110-OPEN-CSR01-RUECB09A
           PERFORM VARYING  WRK-IND-B09A     FROM 1 BY 1
                   UNTIL WRK-CODE100-B09A  EQUAL 'S'
                   PERFORM 4120-SELECT-RUECB09A
                   PERFORM 4105-ENVIA-RCOR5005
           END-PERFORM.

           MOVE 'N'   TO WRK-CODE100-B09A.
           MOVE 'S'   TO WRK-PRIM-B09B.
           PERFORM 4130-CLOSE-CSR01-RUECB09A.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4105-ENVIA-RCOR5005         SECTION.
      *---------------------------------------------------------------*

           IF SQLCODE  EQUAL  +100
              GO TO 4105-99-FIM
           END-IF.

           INITIALIZE  RCOR10-REGISTRO.

BR1811     ADD 1                   TO WRK-CONTA-ENVIO

BR1811     IF WRK-FINAL     EQUAL  'S'
              MOVE 'N'      TO  RCOR10-IND-CONTINUA
           ELSE
              MOVE 'S'      TO  RCOR10-IND-CONTINUA
           END-IF.

           IF WRK-PRIM-ENVIO  EQUAL  'N'
              MOVE WRK-PRIM-NUMCTRLIF  TO  RCOR10-NRO-CTRLIF
           END-IF.

           MOVE  'RUEC'                  TO RCOR10-CCUSTO.

           MOVE ZEROS                    TO RCOR10-TAM-SISMSG.
           MOVE SPACES                   TO RCOR10-SISMSG.
           MOVE ZEROS                    TO WRK-ENVIO-LEN.
           MOVE SPACES                   TO WRK-ENVIO-SIS.

           MOVE  WCONTD-MSGEM-RURAL-LEN  TO RCOR10-TAM-SISMSG.
           MOVE  WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF RCOR10-SISMSG)
                                         TO RCOR10-SISMSG.
           MOVE  WCONTD-MSGEM-RURAL-LEN  TO WRK-ENVIO-LEN.
           MOVE  WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF WRK-ENVIO-SIS)
                                         TO WRK-ENVIO-SIS.

BR1811     MOVE WRK-CONTA-ENVIO TO  WRK-SEQ-CSINAL.
           MOVE WRK-SEQ-SSINAL  TO  WRK-SEQ-NUM5.
           MOVE WRK-SEQ-NUM3    TO  RCOR10-SEQ-MSG.

           CALL WRK-MODULO-RCOR5005  USING  RCOR10-REGISTRO
                                            ERRO-AREA
                                            WRK-SQLCA.

           IF RCOR10-COD-RETORNO EQUAL ZEROS
              MOVE RCOR10-NRO-CTRL-IF TO WRK-ENVIO-NUMCTRLIF
              IF WRK-PRIM-ENVIO  EQUAL 'S'
                 MOVE  'N'  TO WRK-PRIM-ENVIO
                 MOVE RCOR10-NRO-CTRL-IF TO WRK-PRIM-NUMCTRLIF
              END-IF
              PERFORM 4200-UPDATE-B09A
              PERFORM 4300-UPDATE-B09B
           ELSE
              MOVE 99                   TO RUEC65-COD-RET
              MOVE 'ERRO RCOR5005'      TO RUEC65-MENSAGEM
              MOVE ERRO-AREA            TO RUEC65-ERRO-AREA
              MOVE WRK-SQLCA(1:LENGTH OF RUEC65-SQLCA)
                                        TO RUEC65-SQLCA
              PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4110-OPEN-CSR01-RUECB09A    SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.
           MOVE WRK-TIMESTAMP-B09A    TO HINCL
                                         OF RUECB09A.

           EXEC SQL
              OPEN      CSR01-RUECB09A
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
              ( SQLWARN0       EQUAL  'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO   ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO   ERR-DBD-TAB
               MOVE 'OPEN    '           TO   ERR-FUN-COMANDO
               MOVE SQLCODE              TO   ERR-SQL-CODE
               MOVE '4110'               TO   ERR-LOCAL
               MOVE SPACES               TO   ERR-SEGM
               PERFORM   9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4120-SELECT-RUECB09A SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES TO WCONTD-MSGEM-RURAL-TEXT OF RUECB09A.

           EXEC SQL
           FETCH CSR01-RUECB09A
           INTO  :RUECB09A.CCONTR-CREDT-RURAL,
                 :RUECB09A.NENVIO-RECOR,
                 :RUECB09A.NREG-MSGEM-RECOR,
                 :RUECB09A.CSIT-MSGEM
                      :WRK-B09A-CSIT-NULL,
                 :RUECB09A.HINCL
                      :WRK-B09A-HINCL-NULL,
                 :RUECB09A.WCONTD-MSGEM-RURAL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO  ERR-DBD-TAB
               MOVE 'SELECT'             TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '4120'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-CODE100-B09A
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 4121-VER-NULOS-B09A
           END-IF.

      *----------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4121-VER-NULOS-B09A       SECTION.
      *---------------------------------------------------------------*

           IF WRK-B09A-CSIT-NULL    LESS ZEROS
              MOVE ZEROS  TO CSIT-MSGEM          OF RUECB09A
           END-IF.

           IF WRK-B09A-HINCL-NULL   LESS ZEROS
              MOVE SPACES TO  HINCL              OF RUECB09A
           END-IF.

      *----------------------------------------------------------------*
       4121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

211025**      EXCLUSAO DE CAMPO - SICOR 5.11
STEFAB*---------------------------------------------------------------*
.     *4122-FORMATA-GRUPO-GLEBA-AST  SECTION.
.     *---------------------------------------------------------------*
.     *
.     *   -IF WRK-IND2-E             EQUAL  1
.     *      -MOVE RUEC66-COR0001-7B TO
.     *           -WRK-B09A-SIS(WRK-POSICAO:
.     *                                -LENGTH OF RUEC66-COR0001-7B)
.     *      -ADD LENGTH OF RUEC66-COR0001-7B TO WRK-TAM
.     *      -ADD LENGTH OF RUEC66-COR0001-7B TO WRK-POSICAO
.     *      -MOVE 1 TO WRK-IND3-E
.     *      -MOVE RUEC66-COR0001-7B TO
.     *           -WRK-B09A-SIS2(WRK-POSICAO2:
.     *                               -LENGTH OF RUEC66-COR0001-7B)
.     *      -ADD LENGTH OF RUEC66-COR0001-7B TO WRK-TAM2
.     *      -ADD LENGTH OF RUEC66-COR0001-7B TO WRK-POSICAO2
.     *      -PERFORM 4123-FORMATA-GRUPO-PTPOLG-AST
.     *           -UNTIL WRK-IND3-E GREATER  WRK-999
.     *      -ADD 999 TO WRK-IND2-E
.     *   -ELSE
.     *      -ADD 999 TO WRK-IND2-E
.     *   -END-IF.
.     *
.     *----------------------------------------------------------------*
.     *4122-99-FIM.                    EXIT.
.     *----------------------------------------------------------------*
.     *
.     *---------------------------------------------------------------*
.     *4123-FORMATA-GRUPO-PTPOLG-AST  SECTION.
.     *---------------------------------------------------------------*
.     *
.     *   -IF WRK-IND3-E  EQUAL  1
.     *      -MOVE  ALL '*'          TO RUEC66-LAT-PONTO-EXCL
.     *      -MOVE  ALL '*'          TO RUEC66-LONG-PONTO-EXCL
.     *      -MOVE  ALL '*'          TO RUEC66-ALTT-PONTO-EXCL
.     *      -MOVE RUEC66-COR0001-7BB
.     *                             -TO
.     *           -WRK-B09A-SIS(WRK-POSICAO:
.     *                                -LENGTH OF RUEC66-COR0001-7BB)
.     *      -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-TAM
.     *      -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-POSICAO
.     *      -ADD 999 TO WRK-IND3-E
.     *      -MOVE RUEC66-COR0001-7BB  TO
.     *           -WRK-B09A-SIS2(WRK-POSICAO2:
.     *                                -LENGTH OF RUEC66-COR0001-7BB)
.     *      -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-TAM2
.     *      -ADD LENGTH OF RUEC66-COR0001-7BB TO WRK-POSICAO2
.     *   -ELSE
.     *      -ADD 999 TO WRK-IND3-E
.     *   -END-IF.
.     *
.     *----------------------------------------------------------------*
.     *4123-99-FIM.                    EXIT.
STEFAB*----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4130-CLOSE-CSR01-RUECB09A    SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              CLOSE     CSR01-RUECB09A
           END-EXEC.

           IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
              ( SQLWARN0       EQUAL    'W'   )
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4130'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4140-COUNT-RUECB09A  SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09A.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09A.
           MOVE WRK-TIMESTAMP-B09A    TO HINCL
                                         OF RUECB09A.

           EXEC SQL
           SELECT  COUNT(*)
           INTO  :WRK-QTDREG-B09A
           FROM  DB2PRD.TENVIO_RECOR_TEMPR
           WHERE CCONTR_CREDT_RURAL   = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR         = :RUECB09A.NENVIO-RECOR
           AND   HINCL                = :RUECB09A.HINCL
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO  ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO  ERR-DBD-TAB
               MOVE 'COUNT'              TO  ERR-FUN-COMANDO
               MOVE SQLCODE              TO  ERR-SQL-CODE
               MOVE '4140'               TO  ERR-LOCAL
               MOVE SPACES               TO  ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF WRK-QTDREG-B09A EQUAL ZEROS
              CONTINUE
           END-IF.

           IF    SQLCODE  EQUAL +100
              CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       4140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-UPDATE-B09A            SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-ENVIO-LEN          TO WCONTD-MSGEM-RURAL-LEN
                                          OF RUECB09A.

           MOVE WRK-ENVIO-SIS          TO WCONTD-MSGEM-RURAL-TEXT
                                          OF RUECB09A.

           MOVE 2                      TO CSIT-MSGEM
                                          OF RUECB09A.

           EXEC SQL
           UPDATE DB2PRD.TENVIO_RECOR_TEMPR
           SET CSIT_MSGEM           = :RUECB09A.CSIT-MSGEM,
               WCONTD_MSGEM_RURAL   = :RUECB09A.WCONTD-MSGEM-RURAL
           WHERE CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
           AND   NREG_MSGEM_RECOR   = :RUECB09A.NREG-MSGEM-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR' TO ERR-DBD-TAB
               MOVE 'UPDATE'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4200'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4300-UPDATE-B09B            SECTION.
      *---------------------------------------------------------------*

           IF WRK-PRIM-B09B  EQUAL 'S'
              MOVE 'N'  TO WRK-PRIM-B09B
           ELSE
              GO TO 4300-99-FIM
           END-IF.

           PERFORM 4310-SELMAX-B09B.

           MOVE RUEC65-CCONTR           TO CCONTR-CREDT-RURAL
                                           OF RUECB09B.

           MOVE RUEC65-NENVIO-RECOR     TO NENVIO-RECOR
                                           OF RUECB09B.

           MOVE RCOR10-NRO-CTRL-IF      TO CCTRL-RQUIS-FINCR
                                           OF RUECB09B.

BS0250     IF WRK-QTDREG-B097          GREATER 100
BS0250        MOVE 6                   TO CSIT-MSGEM OF RUECB09B
BS0250     ELSE
              MOVE 2                   TO CSIT-MSGEM OF RUECB09B
BS0250     END-IF.

           EXEC SQL
           SET :WRK-TIMESTAMP = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP         TO HINCL  OF RUECB09B.

           MOVE '999999999'           TO CUSUAR-RESP OF RUECB09B.

           EXEC SQL
           UPDATE DB2PRD.THIST_ENVIO_RECOR
           SET CCTRL_RQUIS_FINCR  = :RUECB09B.CCTRL-RQUIS-FINCR,
               CSIT_MSGEM         = :RUECB09B.CSIT-MSGEM,
               HINCL              = :RUECB09B.HINCL,
               CUSUAR_RESP        = :RUECB09B.CUSUAR-RESP
           WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           AND   HULT_ATULZ         = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'UPDATE'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4210'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4310-SELMAX-B09B               SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                         OF RUECB09B.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                         OF RUECB09B.

           EXEC SQL
           SELECT MAX(HULT_ATULZ)
           INTO :RUECB09B.HULT-ATULZ
           FROM DB2PRD.THIST_ENVIO_RECOR
           WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           AND    NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS  AND
               SQLCODE   NOT EQUAL -305   AND
               SQLCODE   NOT EQUAL +100)  OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4310'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF SQLCODE  EQUAL  -305
              MOVE 99                   TO RUEC65-COD-RET
              MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
              MOVE 'DB2'                TO ERR-TIPO-ACESSO
              MOVE 'THIST_ENVIO_RECOR'  TO ERR-DBD-TAB
              MOVE 'SELECT'             TO ERR-FUN-COMANDO
              MOVE SQLCODE              TO ERR-SQL-CODE
              MOVE '431A'               TO ERR-LOCAL
              MOVE SPACES               TO ERR-SEGM
              PERFORM 9000-ROTINA-FIM
           END-IF.

           IF    SQLCODE  EQUAL +100
              CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4320-SELECT-B09B            SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
                                      OF RUECB09B.
           MOVE RUEC65-NENVIO-RECOR   TO NENVIO-RECOR
                                      OF RUECB09B.

           EXEC SQL
           SELECT CCONTR_CREDT_RURAL,
                  NENVIO_RECOR,
                  HULT_ATULZ,
                  CMSGEM_RECOR,
                  HINCL,
                  CCTRL_RQUIS_FINCR,
                  CUNIC_MSGEM,
                  CREFT_RECOR,
                  CSIT_MSGEM,
                  CUSUAR_RESP
           INTO   :RUECB09B.CCONTR-CREDT-RURAL,
                  :RUECB09B.NENVIO-RECOR,
                  :RUECB09B.HULT-ATULZ,
                  :RUECB09B.CMSGEM-RECOR
                       :WRK-B09B-CMSGEM-NULL,
                  :RUECB09B.HINCL
                       :WRK-B09B-HINCL-NULL,
                  :RUECB09B.CCTRL-RQUIS-FINCR
                       :WRK-B09B-CCTRL-NULL,
                  :RUECB09B.CUNIC-MSGEM
                       :WRK-B09B-CUNIC-NULL,
                  :RUECB09B.CREFT-RECOR
                       :WRK-B09B-CREFT-NULL,
                  :RUECB09B.CSIT-MSGEM
                       :WRK-B09B-CSIT-NULL,
                  :RUECB09B.CUSUAR-RESP
                       :WRK-B09B-CUSUAR-NULL
           FROM  DB2PRD.THIST_ENVIO_RECOR
           WHERE CCONTR_CREDT_RURAL  = :RUECB09B.CCONTR-CREDT-RURAL
           AND   NENVIO_RECOR        = :RUECB09B.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0      EQUAL 'W')
               MOVE 99                   TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
               MOVE 'DB2'                TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO ERR-DBD-TAB
               MOVE 'SELECT'             TO ERR-FUN-COMANDO
               MOVE SQLCODE              TO ERR-SQL-CODE
               MOVE '4320'               TO ERR-LOCAL
               MOVE SPACES               TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE   EQUAL ZEROS
               PERFORM 4321-VER-NULOS-B09B
           END-IF.

           IF    SQLCODE  EQUAL +100
              CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       4320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4321-VER-NULOS-B09B            SECTION.
      *----------------------------------------------------------------*

           IF WRK-B09B-CMSGEM-NULL       LESS ZEROS
              MOVE ZEROS   TO CMSGEM-RECOR       OF RUECB09B
           END-IF.

           IF WRK-B09B-HINCL-NULL        LESS ZEROS
              MOVE SPACES TO  HINCL              OF RUECB09B
           END-IF.

           IF WRK-B09B-CCTRL-NULL        LESS ZEROS
              MOVE SPACES TO  CCTRL-RQUIS-FINCR  OF RUECB09B
           END-IF.

           IF WRK-B09B-CUNIC-NULL        LESS ZEROS
              MOVE SPACES TO  CUNIC-MSGEM        OF RUECB09B
           END-IF.

           IF WRK-B09B-CREFT-NULL        LESS ZEROS
              MOVE SPACES TO  CREFT-RECOR        OF RUECB09B
           END-IF.

           IF WRK-B09B-CSIT-NULL         LESS ZEROS
              MOVE ZEROS  TO  CSIT-MSGEM   OF RUECB09B
           END-IF.

           IF WRK-B09B-CUSUAR-NULL       LESS ZEROS
              MOVE SPACES TO  CUSUAR-RESP        OF RUECB09B
           END-IF.

      *----------------------------------------------------------------*
       4321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

EG0617*---------------------------------------------------------------*
EG0617 4400-FORMATAR-DTINI-DTFIM       SECTION.
EG0617*---------------------------------------------------------------*
EG0617
EG0617     IF  DLIBRC-OPER-ORCAM       OF RUECV025 EQUAL SPACES
EG0617         MOVE ALL '*'            TO WRK-66-DT-INI
EG0617         MOVE ALL '*'            TO WRK-66-DT-FIM
EG0617         GO TO 4400-99-FIM
EG0617     END-IF.
EG0617
BS0216     MOVE DLIBRC-OPER-ORCAM      OF RUECV025
BS0216                                 TO WRK-DATA
EG0617     MOVE WRK-DD                 TO WRK-EDIT-DD
EG0617     MOVE WRK-MM                 TO WRK-EDIT-MM
EG0617     MOVE WRK-AAAA               TO WRK-EDIT-AAAA
EG0617     MOVE WRK-EDIT-DATA          TO WRK-66-DT-INI
EG0617
EG0617     IF  DFIM-EVNTO-CDULA        OF RUECV025 EQUAL SPACES
EG0617         MOVE ALL '*'            TO WRK-66-DT-FIM
EG0617     ELSE
EG0617         MOVE DFIM-EVNTO-CDULA   OF RUECV025
EG0617                                 TO WRK-DATA
EG0617         MOVE WRK-DD             TO WRK-EDIT-DD
EG0617         MOVE WRK-MM             TO WRK-EDIT-MM
EG0617         MOVE WRK-AAAA           TO WRK-EDIT-AAAA
EG0617         MOVE WRK-EDIT-DATA      TO WRK-66-DT-FIM
EG0617     END-IF.
BS0216
EG0617*----------------------------------------------------------------*
EG0617 4400-99-FIM.                    EXIT.
EG0617*----------------------------------------------------------------*
EG0617
BR1218*----------------------------------------------------------------*
BR1218 4500-CHECAR-EXIGE-AREA          SECTION.
BR1218*----------------------------------------------------------------*
BR1218*
BR1218     MOVE 'N'                    TO WRK-EXIGE-AREA
WI0421     IF  CEMPTO-BACEN            EQUAL '23271300193002'
WI0421         MOVE 'S'                TO WRK-EXIGE-AREA
WI0421         GO TO 4500-99-FIM
WI0421     END-IF.
WI0421
BR1218     IF  WRK-BENEF               NOT NUMERIC
BR1218     OR  WRK-BENEF               EQUAL ZEROS
BR1218         MOVE 'N'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR0119     IF  CEMPTO-BACEN            EQUAL '23133180110002' OR
BR0119                                       '23136760282002'
BR0119         MOVE 'N'                TO WRK-EXIGE-AREA
BR0119         GO TO 4500-99-FIM
BR0119     END-IF.
BR1218
BR1218     IF  WRK-BENEF               EQUAL '002' OR '009'
BR1218         MOVE 'N'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR1218***  CUSTEIO AGRICOLA
BR1218     IF  CEMPTO-BACEN   (02:01)  EQUAL '2'
BR1218     AND CEMPTO-BACEN   (01:01)  EQUAL '1'
BR1218     AND CEMPTO-BACEN   (03:02)  EQUAL '01'
BR1218         MOVE 'S'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR1218***  CUSTEIO PECUARIO
BR1218     IF  CEMPTO-BACEN   (02:01)  EQUAL '2'
BR1218     AND CEMPTO-BACEN   (01:01)  EQUAL '2'
BR1218     AND ( CEMPTO-BACEN   (3:2)  EQUAL '10'
BR1218                                    OR '26'
BR1218                                    OR '31'
BR1218                                    OR '35'
BR1218                                    OR '37'
BR1218                                    OR '38'
BR1218                                    OR '39'
BR1218                                    OR '63' )
BR1218         MOVE 'S'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR1218***  INVESTIMENTO AGRICOLA
BR1218     IF  CEMPTO-BACEN   (02:01)  EQUAL '3'
BR1218     AND CEMPTO-BACEN   (01:01)  EQUAL '1'
BR1218     AND ( CEMPTO-BACEN   (3:2)  EQUAL '11'
BR1218                                    OR '12' )
BR1218         MOVE 'S'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR1218***  INVESTIMENTO PECUARIO
BR1218     IF  CEMPTO-BACEN   (02:01)  EQUAL '3'
BR1218     AND CEMPTO-BACEN   (01:01)  EQUAL '2'
BR1218     AND ( CEMPTO-BACEN   (3:2)  EQUAL '10'
BR1218                                    OR '13' )
BR1218         MOVE 'S'                TO WRK-EXIGE-AREA
BR1218         GO TO 4500-99-FIM
BR1218     END-IF.
BR1218
BR1218*---------------------------------------------------------------*
BR1218 4500-99-FIM.                    EXIT.
BR1218*---------------------------------------------------------------*
BR1218
BR1218*----------------------------------------------------------------*
BR1218 4600-ZERA-EGF-RUFIB002          SECTION.
BR1218*----------------------------------------------------------------*
BR1218*
BR1218     MOVE -1                     TO WRK-B02-VEGF-N.
BR1218
BR1218     EXEC SQL
BR1218       UPDATE DB2PRD.CONTRATO_RURAL
BR1218       SET    VEGF               = :RUFIB002.VEGF
BR1218                                   :WRK-B02-VEGF-N
BR1218       WHERE  CCONTR             = :RUFIB002.CCONTR
BR1218     END-EXEC.
BR1218
BR1218     IF (SQLCODE   NOT EQUAL ZEROS) OR
BR1218        (SQLWARN0      EQUAL 'W')
BR1218         MOVE 99                   TO RUEC65-COD-RET
BR1218         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR1218         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR1218         MOVE 'CONTRATO_RURAL'     TO ERR-DBD-TAB
BR1218         MOVE 'UPDATE'             TO ERR-FUN-COMANDO
BR1218         MOVE SQLCODE              TO ERR-SQL-CODE
BR1218         MOVE '4600'               TO ERR-LOCAL
BR1218         MOVE SPACES               TO ERR-SEGM
BR1218         PERFORM 9000-ROTINA-FIM
BR1218     END-IF.
BR1218
BR1218*---------------------------------------------------------------*
BR1218 4600-99-FIM.                    EXIT.
BR1218*---------------------------------------------------------------*
BR1218
BR1811*----------------------------------------------------------------*
BR1811 5000-TRATAR-CONTINUACAO         SECTION.
BR1811*----------------------------------------------------------------*
BR1811
BR1811     MOVE 1                      TO WRK-POSICAO
BR1811                                    WRK-POSICAO2
BR1811     MOVE ZEROS                  TO WRK-TAM
BR1811                                    WRK-TAM2
BR1811     MOVE SPACES                 TO WRK-B09A-SIS.
BR1811***
7c2511     MOVE 'COR0001'              TO RUEC66-CODMSG.
7c2511     MOVE WRK-NUMCTRLIF          TO RUEC66-NUMCTRLIF.
BR1811     MOVE RUEC66-COR0001-1       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-1).
BR1811     ADD LENGTH OF RUEC66-COR0001-1     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE ALL '*'                TO RUEC66-GP-COR0001-EMIT
BR1811     MOVE RUEC66-COR0001-2       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-2).
BR1811     ADD LENGTH OF RUEC66-COR0001-2     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE RUEC66-COR0001-3       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-3).
BR1811     ADD LENGTH OF RUEC66-COR0001-3     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE ALL '*'                TO RUEC66-GP-COR0001-RENEGC
BR1811     MOVE RUEC66-COR0001-4       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-4).
BR1811     ADD LENGTH OF RUEC66-COR0001-4     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE RUEC66-COR0001-5       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-5).
BR1811     ADD LENGTH OF RUEC66-COR0001-5     TO WRK-POSICAO WRK-TAM.

211025     COMPUTE WRK-NEW-POS      = (WRK-POSICAO - 6)
BR1811***
BR1811***  CONTINUA FORMATACAO DAS COORDENADAS GEODESICAS
BR1811     PERFORM 3137-FORMATA-GRUPO-GLEBA.

STEFAB     MOVE 'N'                    TO WRK-FIM-B40

RLOS       IF RUEC66-IDENTC-GLEBA(1:1) NOT EQUAL '*'
.             MOVE '******'            TO WRK-B09A-SIS(WRK-NEW-POS:6)
.             MOVE '******'            TO WRK-B09A-SIS2(WRK-NEW-POS2:6)
RLOS       END-IF
BR1811***
BR1811     MOVE RUEC66-COR0001-8       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-8).
BR1811     ADD LENGTH OF RUEC66-COR0001-8     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE RUEC66-COR0001-9       TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-9).
BR1811     ADD LENGTH OF RUEC66-COR0001-9     TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE ALL '*'                TO RUEC66-GP-COR0001-PROP-F
BR1811     MOVE RUEC66-COR0001-10-F    TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-10-F).
BR1811     ADD LENGTH OF RUEC66-COR0001-10-F  TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE RUEC66-COR0001-11      TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-11).
BR1811     ADD LENGTH OF RUEC66-COR0001-11    TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE ALL '*'                TO RUEC66-GP-COR0001-PARCL
BR1811     MOVE RUEC66-COR0001-12      TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-12).
BR1811     ADD LENGTH OF RUEC66-COR0001-12    TO WRK-POSICAO WRK-TAM.
BR1811***
050825***  INIBIR CAMPOS POIS COD SUB PGM FOI MUDADO DE LUGAR
BR1811*** -MOVE RUEC66-COR0001-13      TO WRK-B09A-SIS(WRK-POSICAO:
BR1811***                             -LENGTH OF RUEC66-COR0001-13).
BR1811*** -ADD LENGTH OF RUEC66-COR0001-13    TO WRK-POSICAO WRK-TAM.
BR1811***
BR1811     MOVE RUEC66-COR0001-15      TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-15).
BR1811     ADD LENGTH OF RUEC66-COR0001-15    TO WRK-POSICAO WRK-TAM.

7c2512     MOVE ALL '*'                TO RUEC66-GP-COR0001-DESCLASS
7C2512     MOVE RUEC66-COR0001-16      TO WRK-B09A-SIS(WRK-POSICAO:
7C2512                                 LENGTH OF RUEC66-COR0001-16).
7C2512     ADD LENGTH OF RUEC66-COR0001-16    TO WRK-POSICAO WRK-TAM.

BR1811***
BR1811     MOVE RUEC66-COR0001-17      TO WRK-B09A-SIS(WRK-POSICAO:
BR1811                                 LENGTH OF RUEC66-COR0001-17).
BR1811     ADD LENGTH OF RUEC66-COR0001-17    TO WRK-POSICAO WRK-TAM.
201025***  **------MOVIMENTA CAMPOS PARA CONTINUACAO ----------
           MOVE RUEC66-COR0001-18      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-18).
           ADD LENGTH OF RUEC66-COR0001-18    TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-19      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-19).
           ADD LENGTH OF RUEC66-COR0001-19    TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-20      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-20).
           ADD LENGTH OF RUEC66-COR0001-20    TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-21      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-21).
           ADD LENGTH OF RUEC66-COR0001-21    TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-21A     TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-21A).
           ADD LENGTH OF RUEC66-COR0001-21A   TO WRK-POSICAO WRK-TAM.

7c2512     MOVE ALL '*'                TO RUEC66-GP-COR0001-TPCONFDD
           MOVE RUEC66-COR0001-21B     TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-21B).
           ADD LENGTH OF RUEC66-COR0001-21B   TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-21C     TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-21C).
           ADD LENGTH OF RUEC66-COR0001-21C   TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-21D     TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-21D).
           ADD LENGTH OF RUEC66-COR0001-21D   TO WRK-POSICAO WRK-TAM.

           MOVE RUEC66-COR0001-22      TO WRK-B09A-SIS(WRK-POSICAO:
                                       LENGTH OF RUEC66-COR0001-22).
           ADD LENGTH OF RUEC66-COR0001-22    TO WRK-POSICAO WRK-TAM.
201025
ONEDA ***  **---------------------------------------
ONEDA ***  -> NAO ENVIA AREA QDO EXIGE COORDENADAS GEODESICAS
ONEDA ***  **---------------------------------------
ONEDA      IF RUEC66-IDENTC-GLEBA(1:1) NOT EQUAL '*'
ONEDA         MOVE  '***********'  TO RUEC66-AREA-X
ONEDA      END-IF.
BR1811***
BR1811     EXEC SQL
BR1811          SET :WRK-TIMESTAMP-1S  = CURRENT_TIMESTAMP + 1 SECOND
BR1811     END-EXEC.
BR1811
BR1811     PERFORM UNTIL WRK-TIMES-1S  GREATER WRK-TIMESTAMP-1S
BR1811         CALL 'POOL7600'         USING WRK-AREA-POOL7600
BR1811
BR1811         STRING WRK-PO-TIMESTAMP(01:04) '-'
BR1811                WRK-PO-TIMESTAMP(05:02) '-'
BR1811                WRK-PO-TIMESTAMP(07:02) '-'
BR1811                WRK-PO-TIMESTAMP(09:02) '.'
BR1811                WRK-PO-TIMESTAMP(11:02) '.'
BR1811                WRK-PO-TIMESTAMP(13:02) '.'
BR1811                WRK-PO-TIMESTAMP(15:06)
BR1811           DELIMITED BY SIZE   INTO  WRK-TIMES-1S
BR1811     END-PERFORM.
BR1811
BR1811*----------------------------------------------------------------*
BR1811 5000-99-FIM.                    EXIT.
BR1811*----------------------------------------------------------------*
BR1811
      *---------------------------------------------------------------*
       9000-ROTINA-FIM             SECTION.
      *---------------------------------------------------------------*

           IF  RUEC65-COD-RET      EQUAL 99
               MOVE 'RUEC8810'     TO ERR-MODULO
               MOVE ERRO-AREA      TO RUEC65-ERRO-AREA
               MOVE SQLCA(1:LENGTH OF RUEC65-SQLCA)
                                   TO RUEC65-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9000-99-FIM.                EXIT.
      *---------------------------------------------------------------**
