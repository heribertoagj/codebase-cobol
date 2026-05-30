      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC8831.
       AUTHOR.     BSI TECNOLOGIA.
      *================================================================*
      *                    B S I  T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   RUEC8831                                    *
      *    ANALISTA....:   BSI TECNOLOGIA                              *
      *    DATA........:   08/2012                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR SISMSG COR0005 P/ ENVIO SISTEMA RCOR  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#RUEC65 - AREA DE RETORNO DO MODULO RUEC8831               *
      *    I#RCOR10 - AREA DE RETORNO DO MODULO RCOR5005               *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERRO DB2.                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - VERIFICAR DISPONIBILIDADE                        *
      *----------------------------------------------------------------*
WMM   *                    ALTERACAO - PRIME                           *
WMM   *----------------------------------------------------------------*
WMM   *  ANALISTA PRIME:   WALTER MESSAS                               *
WMM   *  DATA..........:   26/12/2019                                  *
WMM   *----------------------------------------------------------------*
WMM   *  OBJETIVO......:   ENCARGOS FINANCEIROS COMPLEMENTARES         *
WMM   *                    ALTERACAO REGRA DE ENVIO SICOR              *
BR0420*----------------------------------------------------------------*
BR0420*                    ALTERACAO - BRQ                             *
BR0420*----------------------------------------------------------------*
BR0420*  ANALISTA PRIME:   EVANDRO GUIMARAES                           *
BR0420*  DATA..........:   27/04/2020                                  *
BR0420*----------------------------------------------------------------*
BR0420*  OBJETIVO......:   TRATAR IN3993163                            *
BR0420*                    ENVIAR ORIGEM DE RECURSOS ANTERIOR QUANDO   *
BR0420*                    OPERACAO FOR DESCLASSIFICADA                *
BR0420*================================================================*
QT1120*                    ALTERACAO - QINTESS                         *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA QINTESS: QINTESS                                     *
QT1120*  DATA............: 11/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO........: ADEQUAR PARA SICOR 5.01                     *
QT1120*    ALTERACOES NA COR0005:                                      *
QT1120*                    - RETIRADA DO CAMPO "PERCENTUAL DESCLASSIFI *
QT1120*                                         CACAO"                 *
QT1120*----------------------------------------------------------------*
RO1404*        ANALISTA : DS - BRADESCO RURAL - ROSANA L OLIVEIRA     *
RO1404*        DATA     : ABRIL/2021                                  *
RO1404*        OBJETIVO : IN5627181 - TRATAMENTO QUANDO A ORIGEM DE   *
RO1404*                   RECURSOS NAO TEM ASSOCIADO O CODIGO DE O    *
RO1404*                   PLANO REFIN, GERA * NO CAMPO TpFnteRec DA   *
RO1404*                   COR0005, POIS É UM CAMPO NAO OBRIGATORIO    *
      *================================================================*
BR0521*                    ALTERACAO - BRQ DIGITAL SOLUTIONS           *
BR0521*----------------------------------------------------------------*
BR0521*  ANALISTA BRQ...:  SIMONE                                      *
BR0521*  DATA...........:  13/06/2021                                  *
BR0521*----------------------------------------------------------------*
BR0521*  OBJETIVO......:   ADEQUACOES SICOR 5.02                       *
BR0521*                    EXCLUIR FORMATACAO Grupo_COR0005_Desclass   *
BR0521*                    INCLUIR FORMATACAO                          *
BR0521*                    IndrConfcAlertSustldd                       *
BR0521*                    Grupo_COR0005_Propt                         *
BR0521*                    Grupo_COR0005_CertificOrgnco                *
BR0521*                    Grupo_COR0005_RastlddEmpnmnt                *
BR0521*                    PercEnerRenovvlEmpnmnt                      *
BR0521*================================================================*
BR0621*                    ALTERACAO - BRQ DIGITAL SOLUTIONS           *
BR0621*----------------------------------------------------------------*
BR0621*  ANALISTA BRQ...:  SIMONE                                      *
BR0621*  DATA...........:  AGOSTO/2021                                 *
BR0621*----------------------------------------------------------------*
BR0621*  OBJETIVO......:   ADEQUACOES SICOR 5.02                       *
BR0621*                    FORMATACAO DOS CAMPOS INCLUIDOS             *
BR0621*================================================================*
WP0522*================================================================*
WP0522*                    ALTERACAO - WIPRO                           *
WP0522*----------------------------------------------------------------*
WP0522*  ANALISTA.......:  WIPRO                                       *
WP0522*  DATA...........:  MAIO/2022                                   *
WP0522*----------------------------------------------------------------*
WP0522*  OBJETIVO......:   ADEQUACOES SICOR 5.03                       *
WP0522*                    FORMATACAO DOS CAMPOS INCLUIDOS             *
WP0522*================================================================*
WIP001*                    ALTERACAO - WIPRO                           *
-     *----------------------------------------------------------------*
-     *  ANALISTA WIPRO.:  WIPRO                                       *
-     *  DATA...........:  31/10/2022                                  *
-     *----------------------------------------------------------------*
-     *  OBJETIVO......:   ADEQUACOES SICOR 5.05                       *
WIP001*                    INCLUIR FORMATACAO Grupo_COR0005_Desclass   *
NAV506*================================================================*
NAV506*  ANALISTA......:   NAVA                                        *
NAV506*  DATA..........:   MAIO/2023                                   *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUACOES SICOR 5.06                       *
NAV506*                    FORMATACAO DOS CAMPOS                       *
      *================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
WIP002*                    ALTERACAO - WIPRO                           *
.     *----------------------------------------------------------------*
.     *  ANALISTA WIPRO.:  CATIA                                       *
.     *  DATA...........:  03/2024                                     *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   ATENDER INC0219344                          *
.     *                    TRATAR CAMPOS  RUECC5-COD-PROGRAMA /        *
.     *                                   RUECC5-SUB-PROGRAMA          *
WIP002*================================================================*
STE001*                    ALTERACAO - STEFANINI                       *
.     *----------------------------------------------------------------*
.     *  ANALISTA WIPRO.:  STEFANINI                                   *
.     *  DATA...........:  05/2024                                     *
.     *----------------------------------------------------------------*
.     *  OBJETIVO......:   SICOR 5.08                                  *
STE001*================================================================*
MJ0724*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  MILTON JANUARIO                              *
.     *    DATA........:  JULHO/ 2024                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.08 - NAO ENVIAR AS COORDENADAS NO    *
.     *                   COR0005                                      *
MJ0724*================================================================*
STEFAB*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  STEFANINI                                    *
.     *    DATA........:  OUTUBRO/2024                                 *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.09                                   *
.     *    PESQUISA....:  STEFAB                                       *
STEFAB*================================================================*
MJ1224*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  MILTON JANUARIO                              *
.     *    DATA........:  DEZEMBRO / 2024                              *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  ENVIAR ASTERISCO NO GRP DE PROPRIETARIOS     *
MJ1224*================================================================*
050825*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  7COMM - SQUAD FLASH                          *
.     *    DATA........:  AGOSTO/2025                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  ADEQUACOES DE LAYOUT PARA SICOR 5.10         *
.     *                   TRATAMENTO PARA CNPJ ALFANUMERICO            *
050825*    PESQUISA....:  050825                                       *
      *================================================================*
211025*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  7COMM - SQUAD FLASH - RT4 S2 - AGRO7-2700    *
.     *    DATA........:  OUTUBRO/2025                                 *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  ADEQUACOES DE LAYOUT PARA SICOR 5.11         *
.     *                   EXCLUSAO DOS CAMPOS DE ALTITUDE, LATITUDE E  *
.     *                   LONGITUDE (EXCLUSAO)                         *
.     *                   INCLUSAO DO CAMPO WKT PARTE                  *
211025*    PESQUISA....:  211025                                       *
      *================================================================*
      *================================================================*
CAP326*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  CAPGEMINI - SQUAD FLASH                      *
.     *    DATA........:  OUTUBRO/2025                                 *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  NO ENVIO DA 'COROO05' NA LIBERACAO DE        *
.     *                   RECURSOS DO PROCESSO BATCH(AUTOMATICO)       *
.     *                   O CAMPO 'TpFnteRec' NAO DEVE SER ENVIADO     *
CAP326*    PESQUISA....:  CAP326                                       *
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
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*** AUXILIARES PARA STRING   ***'.
      *---------------------------------------------------------------*

       01  FILLER.
           03 WRK-SQLCA                PIC X(136) VALUE SPACES.
BR4.11     03 WRK-FIM-B40              PIC X(001) VALUE SPACES.
BR4.11     03 WRK-FIM-B9N              PIC X(001) VALUE SPACES.
BR4.11     03 WRK-IND2                 PIC S9(05) COMP-3 VALUE ZEROS.
BR4.11     03 WRK-IND3                 PIC S9(05) COMP-3 VALUE ZEROS.
BR4.11     03  WRK-999                 PIC S9(05) COMP-3 VALUE 999.
BR0219     03 WRK-COUNT-RUECB09N       PIC S9(09) COMP   VALUE ZEROS.
BR0420     03 WRK-ORIGEM-ANT-DESCL     PIC S9(03) COMP-3 VALUE ZEROS.
BR0621     03 WRK-IND4                 PIC S9(05) COMP-3 VALUE ZEROS.
BR0621     03 WRK-IND-TAB              PIC S9(05) COMP-3 VALUE ZEROS.
BR0621     03 WRK-IND-INI              PIC S9(05) COMP-3 VALUE ZEROS.
BR0621     03 WRK-ATUALIZA             PIC  X(01) VALUE SPACES.

       01  WRK-AUXILIARES-STRING.
           03 WRK-TAM                  PIC  9(05) COMP-3 VALUE ZEROS.
           03 WRK-POSICAO              PIC  9(05) COMP-3 VALUE ZEROS.

       01  WRK-AUXILIARES-ENVIO.
           03 WRK-QTDREG-B09A          PIC S9(05) COMP-3 VALUE ZEROS.
           03 WRK-PRIM-ENVIO           PIC  X(01) VALUE 'S'.
           03 WRK-PRIM-NUMCTRLIF       PIC  X(20) VALUE SPACES.
           03 WRK-PRIM-B09B            PIC  X(01) VALUE 'S'.

           03 WRK-IND-B09A             PIC S9(05) COMP-3 VALUE ZEROS.
           03 WRK-CODE100-B09A         PIC  X(01) VALUE 'N'.
BR0621     03 WRK-IND-B09E             PIC S9(05) COMP-3 VALUE ZEROS.
BR0621     03 WRK-CODE100-B09E         PIC  X(01) VALUE 'N'.
           03 WRK-MODULO-RCOR5005      PIC  X(08) VALUE 'RCOR5005'.
7COMMP     03 WRK-MODULO               PIC  X(08) VALUE SPACES.
           03 WRK-TAMANHO-SISMSG       PIC S9(04) COMP VALUE ZEROS.
           03 WRK-MAX-NREG             PIC S9(05) COMP-3 VALUE ZEROS.
           03 WRK-ENVIO-LEN            PIC S9(04) COMP VALUE ZEROS.

       01  WRK-NUMERO-04-INI           PIC  S9(04) COMP-3 VALUE ZEROS.
       01  WRK-NUMERO-04-FIM           PIC  S9(04) COMP-3 VALUE ZEROS.

BR4.11 01  WRK-LOG-PONTO-GRUPO.
BR4.11     05 WRK-LOG-PONTO            PIC  S9(03)V9(11)  VALUE ZEROS.
BR4.11
BR4.11 01  WRK-LAT-PONTO-GRUPO.
BR4.11     05 WRK-LAT-PONTO            PIC  S9(02)V9(11)  VALUE ZEROS.
BR4.11
BR4.11 01  WRK-ALT-PONTO-GRUPO.
BR4.11     05 WRK-ALT-PONTO            PIC  S9(04)V9(02)  VALUE ZEROS.
BR4.11
BR4.11 01  WRK-HEXA-ZDS3V11        PIC  S9(03)V9(11)       VALUE ZEROS.
BR4.11 01  FILLER REDEFINES WRK-HEXA-ZDS3V11.
BR4.11     03  FILLER              PIC   X(01).
BR4.11     03  WRK-HEXA-ZDS2V11    PIC  S9(02)V9(11).
BR4.11
BR4.11 01  WRK-HEXA-ZDS5V2         PIC  S9(05)V9(02)       VALUE ZEROS.
BR4.11 01  FILLER REDEFINES WRK-HEXA-ZDS5V2.
BR4.11     03  FILLER              PIC   X(01).
BR4.11     03  WRK-HEXA-ZDS4V2     PIC  S9(04)V9(02).

BR0621 01  WRK-CNPJ-CPF-ANT.
BR0621     05  WRK-CCNPJ-CPF-ANT       PIC S9(09)    COMP-3 VALUE ZEROS.
BR0621     05  WRK-CFLIAL-CNPJ-ANT     PIC S9(05)    COMP-3 VALUE ZEROS.
BR0621     05  WRK-CCTRL-CNPJ-CPF-ANT  PIC S9(02)    COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '**** CONTROLE DE CONTINUACAO ***'.
      *----------------------------------------------------------------*

       01  WRK-CTRLES-CONTINUACAO.
           03 WRK-OPEN-RUECB09A        PIC  X(01)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '**** GRUPO LIBERACAO         ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03 WRK-DLIBRC-ANT           PIC  X(10)         VALUE SPACES.
           03 WRK-VLIBRC-COMP3         PIC S9(17)V99      COMP-3
                                                          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '**** AUXILIARES GERAIS       ***'.
      *----------------------------------------------------------------*

       01  WRK-AUX-GERAIS.
           03 WRK-FIM-CSR01            PIC  X(01)         VALUE SPACES.
           03 WRK-FIM-CSR02            PIC  X(01)         VALUE SPACES.
           03 WRK-FIM-CSR04            PIC  X(01)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '**** AUXILIARES REFORMATACAO ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03 WRK-SZD17V2              PIC +9(17)V99 VALUE ZEROS.
           03 FILLER                   REDEFINES WRK-SZD17V2.
              05  FILLER               PIC  X(01).
              05  WRK-CH19             PIC  X(19).
           03 FILLER                   REDEFINES WRK-SZD17V2.
              05  FILLER               PIC  X(15).
              05  WRK-CH5              PIC  X(05).


       01  WRK-COD9-NUM                PIC  9(9)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD9-NUM.
           05 FILLER                   PIC  9(1).
           05 WRK-COD9-NUM8            PIC  9(8).

       01  WRK-COD5-NUM                PIC  9(5)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD5-NUM.
           05 FILLER                   PIC  9(1).
           05 WRK-COD5-NUM4            PIC  9(4).

       01  WRK-COD3-NUM                PIC  9(3)        VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-COD3-NUM.
           05 FILLER                   PIC  9(1).
           05 WRK-COD3-NUM2            PIC  9(2).

       01  WRK-EDIT-CNPJ.
           05 WRK-EDIT-NCNPJ           PIC  9(08) VALUE ZEROS.
           05 WRK-EDIT-FCNPJ           PIC  9(04) VALUE ZEROS.
           05 WRK-EDIT-CCNPJ           PIC  9(02) VALUE ZEROS.
       01  WRK-EDIT-CNPJ-R  REDEFINES  WRK-EDIT-CNPJ  PIC 9(14).

       01  WRK-SEQ-CSINAL              PIC  S9(05) VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SEQ-CSINAL.
           05 WRK-SEQ-SSINAL           PIC   9(05).

       01  WRK-SEQ-NUM5                PIC   9(05) VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-SEQ-NUM5.
           05 FILLER                   PIC   9(02).
           05 WRK-SEQ-NUM3             PIC   9(03).

       01  WRK-ENVIO-SIS               PIC X(32000)    VALUE SPACES.
       01  WRK-ENVIO-SIS-R             REDEFINES WRK-ENVIO-SIS.
           05 FILLER                   PIC X(00009).
           05 WRK-ENVIO-NUMCTRLIF      PIC X(00020).
           05 FILLER                   PIC X(31971).

       01  WRK-B09A-SIS                PIC X(32000)     VALUE SPACES.
       01  WRK-B09A-SIS-R              REDEFINES WRK-B09A-SIS.
           05 FILLER                   PIC X(00009).
           05 WRK-B09A-NUMCTRLIF       PIC X(00020).
           05 FILLER                   PIC X(31971).

       01  WRK-NUMCTRLIF.
           05 WRK-NUMCTRLIF-FIXO1      PIC X(008)  VALUE '0237RUEC'.
           05 WRK-NUMCTRLIF-ANO        PIC 9(004)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-MES        PIC 9(002)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-DIA        PIC 9(002)  VALUE ZEROS.
           05 WRK-NUMCTRLIF-SEQ        PIC 9(004)  VALUE 0001.

       01  WRK-NUMCTRLIF-AUX           PIC 9(004)  COMP-3 VALUE 0001.

       01  WRK-TIMESTAMP-B09A          PIC X(26) VALUE SPACES.

       01  WRK-TIMESTAMP               PIC X(26) VALUE SPACES.
       01  WRK-FORMA-DATA              REDEFINES WRK-TIMESTAMP.
           05 WRK-FORMA-AAAA           PIC  9(04).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MM             PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-DD             PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-HORA           PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MIN            PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-SEG            PIC  9(02).
           05 FILLER                   PIC  X(01).
           05 WRK-FORMA-MILE           PIC  9(06).

       01  WRK-DATA-DB2-GRUPO.
           05 WRK-DIA-DB2              PIC  9(02) VALUE ZEROS.
           05 FILLER                   PIC  X(01) VALUE '.'.
           05 WRK-MES-DB2              PIC  9(02) VALUE ZEROS.
           05 FILLER                   PIC  X(01) VALUE '.'.
           05 WRK-ANO-DB2              PIC  9(04) VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-DB2-GRUPO.
           05 WRK-DATA-DB2             PIC  X(10).

       01  WRK-DATA-ISO-GRUPO.
           05 WRK-ANO-ISO              PIC  9(04) VALUE ZEROS.
           05 WRK-MES-ISO              PIC  9(02) VALUE ZEROS.
           05 WRK-DIA-ISO              PIC  9(02) VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-ISO-GRUPO.
           05 WRK-DATA-ISO-N8          PIC  9(08).

       01  WRK-QTDREG-B097             PIC S9(05) COMP-3 VALUE ZEROS.

BR0621 01  WRK-COD9-CPF            PIC +9(09) VALUE ZEROS.
BR0621 01  FILLER                  REDEFINES WRK-COD9-CPF.
BR0621     05 FILLER               PIC  9(01).
BR0621     05 WRK-COD9-CPF-9       PIC  9(09).

BR0621 01  WRK-COD2-NUM-S          PIC +9(02) VALUE ZEROS.
BR0621 01  FILLER                  REDEFINES WRK-COD2-NUM-S.
BR0621     05 FILLER               PIC  X(01).
BR0621     05 WRK-COD2-NUM         PIC  9(02).

BR0621 01  WRK-EDIT-CPF.
BR0621     05 WRK-EDIT-NCPF        PIC  9(09) VALUE ZEROS.
BR0621     05 WRK-EDIT-CCPF        PIC  9(02) VALUE ZEROS.

BR0621 01  WRK-COD9-NUM-S          PIC +9(09) VALUE ZEROS.
BR0621 01  FILLER                  REDEFINES WRK-COD9-NUM-S.
BR0621     05 FILLER               PIC  X(01).
BR0621     05 FILLER               PIC  9(01).
BR0621     05 WRK-COD9-NUM8-S      PIC  9(08).
BR0621     05 WRK-COD9-NUM8-XS     REDEFINES  WRK-COD9-NUM8-S
BR0621                             PIC  X(08).

BR0621 01  WRK-EDIT2-CNPJ.
BR0621     05 WRK-EDIT2-NCNPJ      PIC  X(08) VALUE SPACES.
BR0621     05 FILLER               PIC  X(03) VALUE SPACES.

BR0621 01  WRK-NUM5-S              PIC +9(03)V99     VALUE ZEROS.
BR0621 01  FILLER                  REDEFINES WRK-NUM5-S.
BR0621     05 FILLER               PIC  X(01).
BR0621     05 WRK-NUM5             PIC  9(03)V99.

BR0621*---------------------------------------------------------------*
BR0621 01  FILLER                      PIC  X(32)         VALUE
BR0621     '* TAB. INTERNA PARA PROPRIETARIO'.
BR0621*---------------------------------------------------------------*
BR0621
BR0621 01 WRK-TABELA.
BR0621    05 WRK-TAB-PROPRIETARIO OCCURS 100 TIMES.
BR0621       10  WRK-CTPO-PSSOA         PIC S9(01)  COMP-3 VALUE ZEROS.
BR0621       10  WRK-CCNPJ-CPF          PIC S9(09)  COMP-3 VALUE ZEROS.
BR0621       10  WRK-CFLIAL-CNPJ        PIC S9(05)  COMP-3 VALUE ZEROS.
BR0621       10  WRK-CCTRL-CNPJ-CPF     PIC S9(02)  COMP-3 VALUE ZEROS.
BR0621       10  WRK-CNIRF-IMOV         PIC  X(09)         VALUE SPACES.
BR0621       10  WRK-CCCIR              PIC  X(13)         VALUE SPACES.
BR0621       10  WRK-CREG-AMBTL         PIC  X(41)         VALUE SPACES.
BR0621       10  WRK-PPRESV-AMBTL-PPRIE PIC S9(03)V99      VALUE ZEROS.
BR0621       10  WRK-COUTGA-AGUA-IMOV   PIC  X(30)         VALUE SPACES.

7COMMP*---------------------------------------------------------------*
7COMMP 01  FILLER                      PIC X(032)          VALUE
7COMMP     '*** AREA DO MODULO RUEC8824  ***'.
7COMMP*---------------------------------------------------------------*
7COMMP
7COMMP COPY 'RUECW824'.
7COMMP
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '**** COPYBOOK I#RUECC5       ***'.
      *----------------------------------------------------------------*

           COPY 'I#RUECC5'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** COPYBOOK I#RCOR10       ***'.
      *---------------------------------------------------------------*

           COPY 'I#RCOR10'.

BR4.11*----------------------------------------------------------------*
BR4.11 01  FILLER                      PIC  X(032)         VALUE
BR4.11     '*** AREA DO MODULO RUECC941  ***'.
BR4.11*----------------------------------------------------------------*
BR4.11
BR4.11     COPY 'RUECWGEO'.
BR4.11
BR4.11 01  FILLER.
BR4.11     03 WRK-RUECC941             PIC X(008)    VALUE 'RUECC941'.
BR4.11     03 WRK-SEQ-GLEBA-X19.
BR4.11        05 FILLER                PIC 9(011)    VALUE ZEROS.
BR4.11        05 WRK-SEQ-MATR-N03      PIC 9(003)    VALUE ZEROS.
BR4.11        05 WRK-SEQ-GLEBA-N05     PIC 9(005)    VALUE ZEROS.
BR4.11     03 ACU-GLEBA                PIC 9(009)    COMP-3 VALUE ZEROS.
BR4.11
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** AREA BRAD7600           ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3  VALUE ZEROS.
           03  WRK-TI-TIMESTAMP        PIC X(20)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** COPYBOOK I#BRAD7C       ***'.
      *---------------------------------------------------------------*

           COPY 'I#BRAD7C'.

BR0921*---------------------------------------------------------------*
BR0921 01  FILLER                      PIC  X(32)          VALUE
BR0921     '**** ACESSO CMRU8835         ***'.
BR0921*---------------------------------------------------------------*
BR0921
BR0921     COPY 'CMRUWPPR'.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *                         B R Q                                 *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#CMRUPP - PARA MODULO RCOR5005(COR0006)*
      * GERACAO.............: SETEMBRO/2021                           *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO COM SERVICO CMRU8835        *
      *---------------------------------------------------------------*


BR0921
BR0921 01  FILLER.
BR0921     03 WRK-CMRU8835             PIC  X(08) VALUE 'CMRU8835'.
BR0921     03 WRK-TAM-PPRIE-PD         PIC  9(05) COMP-3   VALUE ZEROS.
BR0921
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE SQLCA           ***'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUCAB004        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUCAB004
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUECB099        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB099
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUECB09A        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB09A
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUECB09B        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB09B
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUECV012        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV012
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUFIB002        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** INCLUDE RUFIB044        ***'.
      *---------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUFIB044
           END-EXEC.

4.13  *---------------------------------------------------------------*
4.13   01  FILLER                      PIC  X(32)          VALUE
4.13       '**** INCLUDE RURCB000        ***'.
4.13  *---------------------------------------------------------------*
4.13
4.13       EXEC SQL
4.13           INCLUDE RURCB000
4.13       END-EXEC.
4.13
4.13  *---------------------------------------------------------------*
4.13   01  FILLER                      PIC  X(32)          VALUE
4.13       '**** INCLUDE RURCB028        ***'.
4.13  *---------------------------------------------------------------*
4.13
4.13       EXEC SQL
4.13           INCLUDE RURCB028
4.13       END-EXEC.
4.13

BR0518*---------------------------------------------------------------*
BR0518 01  FILLER                      PIC  X(32)          VALUE
BR0518     '**** INCLUDE RUFIB092        ***'.
BR0518*---------------------------------------------------------------*
BR0518
BR0518     EXEC SQL
BR0518         INCLUDE RUFIB092
BR0518     END-EXEC.
BR0518
BR4.11*---------------------------------------------------------------*
BR4.11 01  FILLER                      PIC  X(32)          VALUE
BR4.11     '**** INCLUDE RUECB09N        ***'.
BR4.11*---------------------------------------------------------------*
BR4.11
BR4.11     EXEC SQL
BR4.11         INCLUDE RUECB09N
BR4.11     END-EXEC.
BR4.11
BR4.11*---------------------------------------------------------------*
BR4.11 01  FILLER                      PIC  X(32)          VALUE
BR4.11     '**** INCLUDE RUECB040        ***'.
BR4.11*---------------------------------------------------------------*
BR4.11
BR4.11     EXEC SQL
BR4.11         INCLUDE RUECB040
BR4.11     END-EXEC.
BR4.11
BR0420*---------------------------------------------------------------*
BR0420 01  FILLER                      PIC  X(32)          VALUE
BR0420     '**** INCLUDE RUCAB003        ***'.
BR0420*---------------------------------------------------------------*
BR0420
BR0420     EXEC SQL
BR0420         INCLUDE RUCAB003
BR0420     END-EXEC.
BR0420
BR0621*---------------------------------------------------------------*
BR0621 01  FILLER                      PIC  X(32)          VALUE
BR0621     '**** INCLUDE RUECB09E        ***'.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     EXEC SQL
BR0621         INCLUDE RUECB09E
BR0621     END-EXEC.
BR0621

BR0921*---------------------------------------------------------------*
BR0921 01  FILLER                      PIC  X(32)          VALUE
BR0921     '**** INCLUDE RURCB00P        ***'.
BR0921*---------------------------------------------------------------*
BR0921
BR0921     EXEC SQL
BR0921         INCLUDE RURCB00P
BR0921     END-EXEC.
BR0921
WIP001*---------------------------------------------------------------*
-      01  FILLER                      PIC  X(32)          VALUE
-          '**** INCLUDE RURCB00P        ***'.
-     *---------------------------------------------------------------*
-
-          EXEC SQL
-              INCLUDE RURCB097
-          END-EXEC.
WIP001
7COMMP*---------------------------------------------------------------*
.      01  FILLER                      PIC  X(32)          VALUE
.          '**** INCLUDE RURCB094        ***'.
.     *---------------------------------------------------------------*
.
.          EXEC SQL
.              INCLUDE RURCB094
7COMMP     END-EXEC.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** AREAS DE NULIDADE       ***'.
      *----------------------------------------------------------------*

       01  WRK-B002-NULL.
           05 WRK-B002-QPROTV-NULL PIC S9(04) COMP VALUE +0.
           05 WRK-B002-CORIGE-NULL PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B002-CCETFC-NULL     PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B002-EURL-CETFC-NULL PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B002-CRSTRB-NULL     PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B002-EURL-RSTRB-NULL PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B002-PENERG-NULL     PIC S9(04) COMP VALUE +0.

       01  WRK-B09A-NULL.
           05 WRK-B09A-CSIT-NULL   PIC S9(04) COMP VALUE +0.
           05 WRK-B09A-HINCL-NULL  PIC S9(04) COMP VALUE +0.

       01  WRK-B09B-NULL.
           05 WRK-B09B-CCTRL-NULL  PIC S9(04) COMP VALUE +0.

BR0518 01  WRK-B092-NULL.
BR0518     05 WRK-B092-CPROG-RURAL PIC S9(04) COMP VALUE +0.

BR0118 01  WRK-NULL-RUECB099.
BR0118     03  WRK-NULL-DCDULA             PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-DVCTO-CONTR        PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-VFINAN             PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-CCATEG-PROTR-RECOR PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-CPROG-RURAL        PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-CMUN-REFT-BACEN    PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-VPCELA-LIBRC       PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-VPCELA-REC-PPRIO   PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-QUND-FINCD-RURAL   PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-QUND-PROD-PROVV    PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-DINIC-SAFRA        PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-DFIM-SAFRA         PIC S9(04) COMP VALUE ZEROS.
BR0118     03  WRK-NULL-CTPO-GARNT-RECOR   PIC S9(04) COMP VALUE ZEROS.

BR0621 01  WRK-B09E-NULL.
BR0621     05 WRK-B09E-RESP-NULL        PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B09E-PSSOA-NULL       PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B09E-CCNPJ-NULL       PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B09E-CFLIAL-NULL      PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B09E-CCTRL-NULL       PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B040-CNIRF-IMOV-NULL  PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B040-CCCIR-NULL       PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B040-CREG-AMBTL-NULL  PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B040-PPRESV-NULL      PIC S9(04) COMP VALUE +0.
BR0621     05 WRK-B040-COUTGA-NULL      PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** DECLARACAO DE CURSORES  ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-RUCAB004 CURSOR WITH HOLD FOR
             SELECT B004_CH_CONTR
             ,      VALUE ( B004_DTEVENTO , '01.01.0001' )
             ,      VALUE ( B004_DEBCRED  , '01.01.0001' )
             ,      B004_CODLANC
             ,      B004_VLPRICOR
             FROM   DB2PRD.RUCA_CAD_DEP
             WHERE  B004_CH_CONTR      = :RUCAB004.B004-CH-CONTR
             AND    B004_DTCONTAB      > '01.01.0001'
             AND    B004_VLPRICOR      > 0
             AND    B004_DEBCRED       = '2'
             ORDER BY B004_DTEVENTO
           END-EXEC.
WIP001     EXEC SQL
-            DECLARE CSR02-RUFIB044-JN CURSOR WITH HOLD FOR
-            SELECT A.CMOTVO_DCLAS_OPER
-            ,      A.DDCLAS_OPER_CREDT
-            ,      A.VPRINC_DCLAS_CONTR
-            ,      B.CMOTVO_DCLAS_BACEN
-            FROM   DB2PRD.TMOTVO_DCLAS_CONTR A
-            ,      DB2PRD.TMOTVO_DCLAS_OPER  B
-            WHERE  CCONTR              = :RUFIB044.CCONTR
-            AND    A.CMOTVO_DCLAS_OPER = B.CMOTVO_DCLAS_OPER
WIP001     END-EXEC.

           EXEC SQL
             DECLARE CSR03-RUECB09A CURSOR WITH HOLD FOR
             SELECT CCONTR_CREDT_RURAL
             ,      NENVIO_RECOR
             ,      NREG_MSGEM_RECOR
             ,      CSIT_MSGEM
             ,      HINCL
             ,      WCONTD_MSGEM_RURAL
             FROM   DB2PRD.TENVIO_RECOR_TEMPR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
             AND    HINCL              = :RUECB09A.HINCL
             ORDER  BY CCONTR_CREDT_RURAL
             ,         NENVIO_RECOR
             ,         NREG_MSGEM_RECOR
           END-EXEC.

BR0518     EXEC SQL
BR0518       DECLARE CSR04-RUFIB092 CURSOR WITH HOLD FOR
BR0518       SELECT CTPO_PSSOA
BR0518       ,      CCNPJ_CPF_COOP
BR0518       ,      CFLIAL_CNPJ_COOP
BR0518       ,      CCTRL_CPF_COOP
BR0518       ,      VPCELA_COOP_RURAL
BR0518       ,      CPROG_RURAL
BR0518       FROM   DB2PRD.TINFO_COOP_CONTR
BR0518       WHERE  CCONTR             = :RUFIB092.CCONTR
BR0518       AND    NID_COOP_RURAL    >= :RUFIB092.NID-COOP-RURAL
BR0518     END-EXEC.

BR4.11     EXEC SQL
BR4.11         DECLARE  CSR05-RUECB040-JN CURSOR WITH HOLD FOR
BR4.11            SELECT  A.CCONTR
BR4.11                  , A.CSEQ_MATR_RURAL
BR4.11                  , VALUE ( B.NSEQ_GLEBA_IMOV , 0 )
BR4.11                  , A.CMUN_RURAL
BR4.11                  , VALUE ( MIN (B.NCORND_GEOGR_IMOV) , 0 )
BR4.11              FROM  DB2PRD.MATR_CONTR_RURAL   A
BR4.11              LEFT  OUTER JOIN
BR4.11                    DB2PRD.TCORND_GEOGR_CONTR B
BR4.11                ON  A.CCONTR          = B.CCONTR
BR4.11               AND  A.CSEQ_MATR_RURAL = B.CSEQ_MATR_RURAL
BR4.11             WHERE  A.CCONTR          = :RUECB040.CCONTR
BR4.11          GROUP BY  A.CCONTR
BR4.11                  , A.CSEQ_MATR_RURAL
BR4.11                  , B.NSEQ_GLEBA_IMOV
BR4.11                  , A.CMUN_RURAL
BR4.11          ORDER BY  A.CCONTR
BR4.11                  , A.CSEQ_MATR_RURAL
BR4.11                  , B.NSEQ_GLEBA_IMOV
BR4.11                  , A.CMUN_RURAL
BR4.11     END-EXEC.
BR4.11
BR4.11
BR4.11     EXEC SQL
BR4.11         DECLARE  CSR06-RUECB09N  CURSOR WITH HOLD FOR
BR4.11          SELECT  NCORND_GEOGR_IMOV
BR4.11                , MLOGTD_IMOV_RURAL
BR4.11                , MLATTD_IMOV_RURAL
BR4.11                , MALTTD_IMOV_RURAL
BR4.11            FROM  DB2PRD.TCORND_GEOGR_CONTR
BR4.11           WHERE  CCONTR          = :RUECB09N.CCONTR
BR4.11             AND  CSEQ_MATR_RURAL = :RUECB09N.CSEQ-MATR-RURAL
BR4.11             AND  NSEQ_GLEBA_IMOV = :RUECB09N.NSEQ-GLEBA-IMOV
BR4.11        ORDER BY  CCONTR
BR4.11                , CSEQ_MATR_RURAL
BR4.11                , NSEQ_GLEBA_IMOV
BR4.11                , NCORND_GEOGR_IMOV
BR4.11     END-EXEC.
BR4.11
BR0621
BR0621     EXEC SQL
BR0621     DECLARE CSR07-RUECB09E-JN CURSOR  WITH HOLD  FOR
BR0621     SELECT  DISTINCT
BR0621             A.CCONTR_CREDT_RURAL,
BR0621             A.NENVIO_RECOR,
BR0621             A.CTPO_RESP_PSSOA,
BR0621             A.CTPO_PSSOA,
BR0621             A.CCNPJ_CPF,
BR0621             A.CFLIAL_CNPJ,
BR0621             A.CCTRL_CNPJ_CPF,
BR0621             B.CNIRF_IMOV_RURAL,
BR0621             B.CCCIR,
BR0621             B.CREG_AMBTL_RURAL,
BR0621             B.PPRESV_AMBTL_PPRIE,
BR0621             B.COUTGA_AGUA_IMOV
BR0621     FROM  DB2PRD.TPSSOA_ENVIO_RECOR          A
BR0621     LEFT  OUTER JOIN DB2PRD.MATR_CONTR_RURAL B
BR0621     ON    B.CCONTR             = A.CCONTR
BR0621     AND   B.CSEQ_MATR_RURAL    = A.CSEQ_MATR_RURAL
BR0621     WHERE A.CCONTR_CREDT_RURAL = :RUECB09E.CCONTR-CREDT-RURAL
BR0621     AND   A.NENVIO_RECOR       = :RUECB09E.NENVIO-RECOR
BR0621     AND   A.CTPO_RESP_PSSOA    = :RUECB09E.CTPO-RESP-PSSOA
BR0621     ORDER BY A.CTPO_PSSOA,
BR0621              A.CCNPJ_CPF,
BR0621              A.CFLIAL_CNPJ,
BR0621              A.CCTRL_CNPJ_CPF
BR0621     END-EXEC.
BR0621
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '**** FIM DA WORKING RUEC8831 ***'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       LINKAGE                         SECTION.
      *---------------------------------------------------------------*

           COPY 'I#RUEC65'.

      *================================================================*
       PROCEDURE DIVISION          USING RUEC65-AREA.
      *================================================================*

      *---------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *---------------------------------------------------------------*

           CALL 'CKRS1000'.

           PERFORM  0001-INICIALIZAR-WORK.

           MOVE ZEROS              TO RUEC65-COD-RET.
           MOVE ZEROS              TO WRK-TAM.

           MOVE SPACES             TO WRK-B09A-SIS.

           PERFORM  1000-PROCESSAR.

           PERFORM  9000-ROTINA-FIM.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       0001-INICIALIZAR-WORK           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE   WRK-SQLCA
                        WRK-TAM
                        WRK-POSICAO
                        WRK-QTDREG-B09A
                        WRK-PRIM-NUMCTRLIF
                        WRK-IND-B09A
                        WRK-TAMANHO-SISMSG
                        WRK-MAX-NREG
                        WRK-ENVIO-LEN
                        WRK-NUMERO-04-INI
                        WRK-NUMERO-04-FIM
                        WRK-OPEN-RUECB09A
                        WRK-DLIBRC-ANT
                        WRK-VLIBRC-COMP3
                        WRK-FIM-CSR01
                        WRK-FIM-CSR02
                        WRK-FIM-CSR04
                        WRK-SZD17V2
                        WRK-COD9-NUM
                        WRK-COD5-NUM
                        WRK-COD3-NUM
                        WRK-EDIT-NCNPJ
                        WRK-EDIT-FCNPJ
                        WRK-EDIT-CCNPJ
                        WRK-SEQ-CSINAL
                        WRK-SEQ-NUM5
                        WRK-ENVIO-SIS
                        WRK-B09A-SIS
                        WRK-NUMCTRLIF-ANO
                        WRK-NUMCTRLIF-MES
                        WRK-NUMCTRLIF-DIA
                        WRK-NUMCTRLIF-SEQ
                        WRK-TIMESTAMP-B09A
                        WRK-TIMESTAMP
                        WRK-DIA-DB2
                        WRK-MES-DB2
                        WRK-ANO-DB2
                        WRK-ANO-ISO
                        WRK-MES-ISO
                        WRK-DIA-ISO
                        WRK-QTDREG-B097
                        WRK-DT-JULIANA
                        WRK-DT-AAMMDD
                        WRK-DT-AAAAMMDD
                        WRK-TI-HHMMSS
                        WRK-TI-HHMMSSMMMMMM
                        WRK-TI-TIMESTAMP
                        WRK-B002-QPROTV-NULL
                        WRK-B002-CORIGE-NULL
BR0621                  WRK-B002-CCETFC-NULL
BR0621                  WRK-B002-EURL-CETFC-NULL
BR0621                  WRK-B002-CRSTRB-NULL
BR0621                  WRK-B002-EURL-RSTRB-NULL
BR0621                  WRK-B002-PENERG-NULL
                        WRK-B09A-CSIT-NULL
                        WRK-B09A-HINCL-NULL
                        WRK-B09B-CCTRL-NULL.

      *---------------------------------------------------------------*
       0001-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-PROCESSAR                  SECTION.
      *---------------------------------------------------------------*

           CALL 'POOL7600'             USING  WRK-DATA-HORA.

           PERFORM 2000-VALIDAR-ARG-ENTRADA.

BR4.11     PERFORM 2100-COORDENADA-OBRIGATORIA

           PERFORM 3100-FORMATA-SIS1.

           PERFORM 4000-INSERT-B09A.

           PERFORM 4100-ENVIO.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VALIDAR-ARG-ENTRADA        SECTION.
      *---------------------------------------------------------------*

           IF  RUEC65-PGM-CHAMADOR     EQUAL SPACES
               MOVE 01                 TO RUEC65-COD-RET
               MOVE 'PGM CHAMADOR INVALIDO'
                                       TO RUEC65-MENSAGEM
               GO TO 2000-99-FIM
           END-IF.

           IF  (RUEC65-CCONTR          NOT NUMERIC)
           OR  (RUEC65-CCONTR          EQUAL ZEROS)
               MOVE 01                 TO RUEC65-COD-RET
               MOVE 'CONTRATO INVALIDO'
                                       TO RUEC65-MENSAGEM
               GO  TO  2000-99-FIM
           END-IF.

           IF  (RUEC65-NENVIO-RECOR    NOT NUMERIC)
               MOVE 01                 TO RUEC65-COD-RET
               MOVE 'NENVIO-RECOR INVALIDO'
                                       TO RUEC65-MENSAGEM
               GO TO 2000-99-FIM
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

BR4.11*------------------------------------------------------------*
BR4.11 2100-COORDENADA-OBRIGATORIA     SECTION.
BR4.11*------------------------------------------------------------*
BR4.11
BR4.11     INITIALIZE RUECWGEO-REGISTRO.
BR4.11
BR4.11     MOVE RUEC65-CCONTR          TO RUECWGEO-E-CCONTR.
BR4.11
BR4.11     MOVE 'RUECC941'             TO WRK-RUECC941
BR4.11     CALL WRK-RUECC941           USING RUECWGEO-COPYBOOK
BR4.11                                       ERRO-AREA
BR4.11                                       SQLCA.
BR4.11
BR4.11     IF  RUECWGEO-COD-RETORNO    NOT EQUAL ZEROS
BR4.11         MOVE 'N'                TO RUECWGEO-COORD-OBRIG
BR4.11     END-IF.
BR4.11
BR4.11*------------------------------------------------------------*
BR4.11 2100-99-FIM.                    EXIT.
BR4.11*------------------------------------------------------------*
      *---------------------------------------------------------------*
       3100-FORMATA-SIS1               SECTION.
      *---------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-TAM.
           MOVE 1                      TO WRK-POSICAO.

      ***  CodMsg
           MOVE 'COR0005'              TO RUECC5-CODMSG.

      ***  NumRefBCCOROr
           MOVE RUEC65-CCONTR          TO CCONTR OF RUFIB002.
           PERFORM 3130-OBTER-DADOS-CONTRATO.

BR0420     PERFORM 3131-OBTER-ORIGEM-ANTERIOR.
BR0420
           MOVE CREFT-RECOR            OF RUFIB002
                                       TO RUECC5-NUMREF-BCCOROR.

      ***  NumCtrlIF
           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB09B.
           PERFORM 3110-OBTER-NRO-CONTROLE-IF.
           MOVE CCTRL-RQUIS-FINCR      OF RUECB09B
                                       TO RUECC5-NUMCTRLIF.

      ***  CNPJEntRespons
           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB099.
           MOVE NENVIO-RECOR           OF RUECB09B
                                       TO NENVIO-RECOR OF RUECB099
           PERFORM 3120-OBTER-CNPJ-AGENCIA.

           MOVE CCNPJ-ENTID-RESP       TO WRK-COD9-NUM.
           MOVE CFLIAL-CNPJ-ENTID      TO WRK-COD5-NUM.
           MOVE CCTRL-CNPJ-ENTID       TO WRK-COD3-NUM.
           MOVE WRK-COD9-NUM8          TO WRK-EDIT-NCNPJ.
           MOVE WRK-COD5-NUM4          TO WRK-EDIT-FCNPJ.
           MOVE WRK-COD3-NUM2          TO WRK-EDIT-CCNPJ.
050825     MOVE WRK-EDIT-CNPJ          TO RUECC5-CNPJ-ENT-RESPONS.

      ***  DtVenc
           MOVE RUEC65-CCONTR          TO CCONTR         OF RUECV012.
           PERFORM 3140-OBTER-DATA-VENCIMENTO.

           MOVE DEVNTO OF RUECV012     TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
           MOVE WRK-MES-DB2            TO WRK-MES-ISO.
           MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
           MOVE WRK-DATA-ISO-N8        TO RUECC5-DTVENC.

BR1118     IF DEVNTO OF RUECV012       EQUAL '01.01.0001'
BR1118        MOVE DVCTO-CONTR OF RUFIB002  TO WRK-DATA-DB2
BR1118        MOVE WRK-DIA-DB2              TO WRK-DIA-ISO
BR1118        MOVE WRK-MES-DB2              TO WRK-MES-ISO
BR1118        MOVE WRK-ANO-DB2              TO WRK-ANO-ISO
BR1118        MOVE WRK-DATA-ISO-N8          TO RUECC5-DTVENC
BR1118     END-IF.

BR1117     MOVE ALL '*'                TO RUECC5-CBASE-LEGAL-PRO

BR4.12     MOVE ALL '*'                TO RUECC5-TP-INSTNTO-CRED

BR0521     MOVE 'N'                    TO RUECC5-IND-CONF-ALRT-SUST

BR0921     EXEC SQL
BR0921       SELECT CALERT_SUSTBD_PER
BR0921       INTO  :RURCB00P.CALERT-SUSTBD-PER
BR0921       FROM  DB2PRD.TPER_ALERT_SUSTBD
BR0921       WHERE NPER_ALERT_SUSTBD   =  1
BR0921       AND   DINIC_VGCIA_ALERT   <= CURRENT_DATE
BR0921       AND   DFIM_VGCIA_ALERT    >= CURRENT_DATE
BR0921     END-EXEC.
BR0921
BR0921     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BR0921        (SQLWARN0                EQUAL 'W')
BR0921         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0921         MOVE 'TPER_ALERT_SUSTBD' TO ERR-DBD-TAB
BR0921         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BR0921         MOVE SQLCODE            TO ERR-SQL-CODE
BR0921         MOVE '3100'             TO ERR-LOCAL
BR0921         MOVE SPACES             TO ERR-SEGM
BR0921         PERFORM 9000-ROTINA-FIM
BR0921     END-IF.
BR0921
BR0921     IF  SQLCODE                 EQUAL +100
BR0921         MOVE 'N'                TO CALERT-SUSTBD-PER OF RURCB00P
BR0921     END-IF.
BR0921
BR0921     MOVE CALERT-SUSTBD-PER      OF RURCB00P
BR0921                                 TO RUECC5-IND-CONF-ALRT-SUST
BR0921
           MOVE RUECC5-COR0005-1       TO WRK-B09A-SIS.
           ADD  LENGTH                 OF RUECC5-COR0005-1
                                       TO WRK-TAM
                                          WRK-POSICAO.

      ***  Grupo_COR0005_DestcFincmnt
           MOVE RUECC5-COR0005-2       TO WRK-B09A-SIS (WRK-POSICAO:
                                          LENGTH OF RUECC5-COR0005-2 )
           ADD  LENGTH                 OF RUECC5-COR0005-2
                                       TO WRK-TAM
                                          WRK-POSICAO.

      ***  NumOrdemDestcOr
           MOVE '001'                  TO RUECC5-NRO-ORDEM-DEST.

      ***  VlrProdddObtd
           IF  WRK-B002-QPROTV-NULL    NOT EQUAL ZEROS
               MOVE ALL '*'            TO RUECC5-VLR-PROD-OBTD
           ELSE
               MOVE QPROTV-ADQUI       OF RUFIB002
                                       TO WRK-SZD17V2
               MOVE WRK-CH19           TO RUECC5-VLR-PROD-OBTD
           END-IF.

           MOVE RUECC5-COR0005-3       TO WRK-B09A-SIS (WRK-POSICAO:
                                          LENGTH OF RUECC5-COR0005-3 )
           ADD  LENGTH                 OF RUECC5-COR0005-3
                                       TO WRK-TAM
                                          WRK-POSICAO.

      ***  Grupo_COR0005_Librc
           PERFORM 3150-TRATAR-GRUPO-LIBERACAO.

      ***  Grupo_COR0005_Cooperd
           PERFORM 3170-TRATAR-GRUPO-COOPERADO.

4.13  ***  ORIGEM DE RECURSO
BR0420     IF  WRK-ORIGEM-ANT-DESCL    EQUAL ZEROS
BR0420         MOVE CORIGE-REC OF RUFIB002  TO CORIGE-REC OF RURCB000
BR0420     ELSE
BR0420         MOVE WRK-ORIGEM-ANT-DESCL    TO CORIGE-REC OF RURCB000
BR0420     END-IF.
4.13       PERFORM 4500-SELECT-RURCB000
RO1404     IF CPLANO-REFIN-RECOR OF RURCB028 EQUAL SPACES
RO1404         MOVE  ALL '*'            TO RUECC5-TP-FNTE-REC
RO1404     ELSE
4.13           MOVE CPLANO-REFIN-RECOR  TO RUECC5-TP-FNTE-REC
RO1404     END-IF

CAP326     IF  RUEC65-PGM-CHAMADOR      EQUAL 'RUEC0895'
CAP326         MOVE  ALL '*'            TO RUECC5-TP-FNTE-REC
CAP326     END-IF.

4.13       MOVE RUECC5-COR0005-6A       TO WRK-B09A-SIS (WRK-POSICAO:
4.13                                    LENGTH OF RUECC5-COR0005-6A)
4.13       ADD  LENGTH                  OF RUECC5-COR0005-6A
4.13                                    TO WRK-TAM
4.13                                       WRK-POSICAO

MJ0724***  SICOR 5.08 - NAO ENVIAR AS COORDENADAS NO COR0005
MJ0724***  MOVENDO 'N' PARA RUECWGEO-COORD-OBRIG. CASO PRECISE
MJ0724***  VOLTAR A ENVIAR AS COORDENADAS RETIRAR A MOVIMENTACAO
MJ0724     MOVE 'N'                    TO RUECWGEO-COORD-OBRIG
BR0219     IF RUECWGEO-COORD-OBRIG     EQUAL 'S'
BR0219        PERFORM 4400-COORDENADAS
BR0219        IF WRK-COUNT-RUECB09N    EQUAL ZEROS
BR0219           MOVE 'N'              TO    RUECWGEO-COORD-OBRIG
BR0219        END-IF
BR0219     END-IF.

      *** Grupo_COR0005_Gleba
           MOVE 1 TO WRK-IND2.
BR4.11     IF  RUECWGEO-COORD-OBRIG    EQUAL 'N'
BR4.11         PERFORM 3180-FORMATA-GRUPO-GLEBA-AST
BR4.11             UNTIL WRK-IND2 GREATER  WRK-999
211025**   CAMPOS EXCLUIDOS - SICOR 5.11
BR4.11*   -ELSE
BR4.11*       -MOVE ZEROS              TO ACU-GLEBA
BR4.11*       -PERFORM 3190-FORMATA-GRUPO-GLEBA
BR4.11*           -UNTIL WRK-IND2 GREATER  WRK-999
BR4.11     END-IF.

BR4.11***  PercJurosEncargoFinancPosfix
BR4.11     MOVE ALL '*'                TO RUECC5-PERC-POSFIX
BR4.11***  TpEncargoFinancComplta
WMM        MOVE '09'                   TO RUECC5-TPENCARGO-COMPL
RL4.14***  Area
RL4.14     MOVE ALL '*'                TO RUECC5-AREA-X
RL4.14***  QtdItemFincd
RL4.14     MOVE ALL '*'                TO RUECC5-QTD-ITEM-FINCD-X
RL4.14***  PercCstEftTot
RL4.14     MOVE ALL '*'                TO RUECC5-PERC-CST-EFT-TOTAL
BR4.11
211025**   CAMPOS EXCLUIDOS - SICOR 5.11
STE001***  Grupo_COR0005_GlebaExcl
STE001*   -MOVE ALL '*'                TO RUECC5-LAT-PONTO-EXCL
STE001*   -MOVE ALL '*'                TO RUECC5-LONG-PONTO-EXCL
STE001*   -MOVE ALL '*'                TO RUECC5-ALTT-PONTO-EXCL
STE001***  PercJurosEncargoFinanc
STE001     MOVE ALL '*'            TO RUECC5-PERCJUROSENCARGOFINANC
STE001***  PercBonusCAR
STE001     MOVE ALL '*'                TO RUECC5-PERCBONUSCAR
BR4.11     MOVE RUECC5-COR0005-9       TO WRK-B09A-SIS (WRK-POSICAO:
BR4.11                                    LENGTH OF RUECC5-COR0005-9)
BR4.11     ADD  LENGTH                 OF RUECC5-COR0005-9
BR4.11                                 TO WRK-TAM
BR4.11                                    WRK-POSICAO.

BR0521*** Grupo_COR0005_Propt
.
BR0621     INITIALIZE WRK-TABELA.
BR0621     MOVE 'N'      TO WRK-CODE100-B09E.
BR0921
BR0921     PERFORM 4900-RECUPERA-PROP-COR0001.
BR0921
BR0921*   -IF  CMRUPP-COD-RETORNO      EQUAL ZEROS
BR0921**********OVE CMRUPP-S-TAM-GRUPO TO WRK-TAM-PPRIE-PD
BR0921**********OVE CMRUPP-S-GRUPO-PROP(1:WRK-TAM-PPRIE-PD  )
BR0921**********                       TO WRK-B09A-SIS(WRK-POSICAO:
BR0921**********                          WRK-TAM-PPRIE-PD  )
BR0921**********DD WRK-TAM-PPRIE-PD    TO WRK-TAM
BR0921**********DD WRK-TAM-PPRIE-PD    TO WRK-POSICAO
MJ1124***  DEZ/2024 - ENVIAR O GRP DE PROPRIETARIOS VAZIO
MJ1224**      -MOVE ALL '*'            TO RUECC5-GP-COR0005-PROPT
MJ1224**      -MOVE RUECC5-COR0005-10  TO WRK-B09A-SIS(WRK-POSICAO:
MJ1224**                             -LENGTH OF RUECC5-COR0005-10)
MJ1224**      -ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-TAM
MJ1224**      -ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-POSICAO
BR0921**  -ELSE
BR0621         PERFORM 4600-FORMATA-GRUPO-PROP
MJ1124***  DEZ/2024 - ENVIAR O GRP DE PROPRIETARIOS VAZIO
MJ1224         MOVE ALL '*'            TO RUECC5-GP-COR0005-PROPT
MJ1224         MOVE RUECC5-COR0005-10  TO WRK-B09A-SIS(WRK-POSICAO:
MJ1224                                LENGTH OF RUECC5-COR0005-10)
MJ1224         ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-TAM
MJ1224         ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-POSICAO
BR0921**  -END-IF.
.
.     *** Grupo_COR0005_CertificOrgnco
.
BR0621     IF CCETFC-ORGNC-EMPTO     OF RUFIB002 NOT EQUAL SPACES
BR0621        MOVE CCETFC-ORGNC-EMPTO
BR0621                               OF RUFIB002
BR0621                               TO RUECC5-CCETFC-ORGNC
BR0621        MOVE EURL-CETFC-ORGNC-TEXT
BR0621                               OF RUFIB002(1:EURL-CETFC-ORGNC-LEN)
BR0621                               TO RUECC5-EURL-CETFC-ORGNC
BR0621     ELSE
BR0621        MOVE ALL '*'           TO RUECC5-GP-COR0005-CORGN
BR0621     END-IF.
.
.          MOVE RUECC5-COR0005-11      TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF RUECC5-COR0005-11)
.          ADD  LENGTH                 OF RUECC5-COR0005-11
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
.     *** Grupo_COR0005_RastlddEmpnmnt
.
BR0621     IF CRSTRB-EMPTO-RURAL     OF RUFIB002 NOT EQUAL SPACES
BR0621        MOVE CRSTRB-EMPTO-RURAL
BR0621                               OF RUFIB002
BR0621                               TO RUECC5-CRSTRB-EMPTO
BR0621
BR0621        MOVE EURL-RSTRB-EMPTO-TEXT
BR0621                               OF RUFIB002(1:EURL-RSTRB-EMPTO-LEN)
BR0621                               TO RUECC5-EURL-RSTRB
BR0621     ELSE
BR0621        MOVE ALL '*'           TO RUECC5-GP-COR0005-RASTB
BR0621     END-IF.
.
.          MOVE RUECC5-COR0005-12      TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF RUECC5-COR0005-12)
.          ADD  LENGTH                 OF RUECC5-COR0005-12
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
.     *** PercEnerRenovvlEmpnmnt
.
BR0621     IF PENERG-RENOV-EMPTO       OF RUFIB002 GREATER ZEROS
BR0621        MOVE  PENERG-RENOV-EMPTO OF RUFIB002
BR0621                                 TO WRK-NUM5-S
BR0621        MOVE  WRK-NUM5           TO RUECC5-PENERG-RENOV-N
BR0621     ELSE
BR0621        MOVE ALL '*'             TO RUECC5-PENERG-RENOV
BR0621     END-IF.
.
.          MOVE RUECC5-COR0005-13      TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF RUECC5-COR0005-13)
.          ADD  LENGTH                 OF RUECC5-COR0005-13
.                                      TO WRK-TAM
BR0521                                    WRK-POSICAO.
WP0522
.     *** CodSTNEqulzc
.
.          MOVE ALL '*'                TO RUECC5-COD-STN-EQULZC
.
.          MOVE RUECC5-COR0005-14      TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF RUECC5-COR0005-14)
.          ADD  LENGTH                 OF RUECC5-COR0005-14
.                                      TO WRK-TAM
WP0522                                    WRK-POSICAO.

WIP001***  Grupo_COR0005_Desclass
WIP001     PERFORM 3160-TRATAR-GRUPO-DESCLASS.

NAV506***  Grupo_COR0005_Desclass_Parcial
NAV506     PERFORM 3165-TRATAR-GR-DESCLASS-PAR.

7COMMP*** MONTAR GRUPO SICOR 5.07 - INICIO
.
.          IF  RUEC65-PGM-CHAMADOR     EQUAL 'RUCA6704' OR
.              RUEC65-PGM-CHAMADOR     EQUAL 'RUMO0270' OR
.              RUEC65-PGM-CHAMADOR     EQUAL 'RUCA6910' OR
.              RUEC65-PGM-CHAMADOR     EQUAL 'RUCA6651' OR
.              RUEC65-PGM-CHAMADOR     EQUAL 'RUCA6652' OR
.              RUEC65-PGM-CHAMADOR     EQUAL 'RUCA6653'
.              MOVE '****'             TO RUECC5-COD-PROGRAMA
.                                         RUECC5-SUB-PROGRAMA
.          ELSE
.              IF  RUEC65-PGM-CHAMADOR EQUAL 'RUCA6709'
.                  PERFORM 5200-FORMATA-SUBPROG-RUCA
.                  PERFORM 5300-FORMATA-CODPROG-RUCA
.              ELSE
.                  PERFORM 5000-FORMATA-SUBPROG
.                  PERFORM 5100-FORMATA-CODPROG
.              END-IF
.          END-IF
.
WIP002******F  RUECC5-COD-PROGRAMA     EQUAL SPACES AND
.    *******   RUECC5-SUB-PROGRAMA     EQUAL SPACES
.    *******   ZOVE '****'             TO RUECC5-COD-PROGRAMA
.    *******                              RUECC5-SUB-PROGRAMA
.    *******ND-IF
.
.          IF  RUECC5-COD-PROGRAMA      EQUAL SPACES
.              MOVE '****'              TO RUECC5-COD-PROGRAMA
.          ELSE
.              MOVE RUECC5-COD-PROGRAMA TO RUECC5-COD-PROGRAMA
.          END-IF
.
.          IF  RUECC5-SUB-PROGRAMA      EQUAL SPACES
.              MOVE '****'              TO RUECC5-SUB-PROGRAMA
.          ELSE
.              MOVE RUECC5-SUB-PROGRAMA TO RUECC5-SUB-PROGRAMA
WIP002     END-IF
.
.
.          MOVE RUECC5-COR0005-DEST    TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF
.                                         RUECC5-COR0005-DEST)
.          ADD  LENGTH                 OF RUECC5-COR0005-DEST
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
STE001     MOVE RUECC5-COR0005-JUROS   TO WRK-B09A-SIS (WRK-POSICAO:
STE001                                    LENGTH OF
STE001                                    RUECC5-COR0005-JUROS)
STE001     ADD  LENGTH                 OF RUECC5-COR0005-JUROS
STE001                                 TO WRK-TAM
STE001                                    WRK-POSICAO.
.
7COMMP*** MONTAR GRUPO SICOR 5.07 - FIM

STEFAB*** MONTAR GRUPO SICOR 5.09 - INICIO
.
.     ***  NumParcl
.          MOVE ALL '*'                TO RUECC5-NUM-PARCL-X
.     ***  DtPrvPgto
.          MOVE ALL '*'                TO RUECC5-DT-PRV-PAGTO
.     ***  VlrPrincipalParcL
.          MOVE ALL '*'                TO RUECC5-VLR-PRINC-X
.          MOVE RUECC5-COR0005-16A     TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF
.                                         RUECC5-COR0005-16A)
.          ADD  LENGTH                 OF RUECC5-COR0005-16A
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
.     ***  CNPJBaseInstExigldd
.          MOVE ALL '*'                TO RUECC5-CNPJ-BASE-INST-EXIGIB
.          MOVE RUECC5-COR0005-CNPJ    TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF
.                                         RUECC5-COR0005-CNPJ)
.          ADD  LENGTH                 OF RUECC5-COR0005-CNPJ
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
STEFAB*** MONTAR GRUPO SICOR 5.09 - FIM

050825***  PercBonusSustvl
.          MOVE ALL '*'                TO RUECC5-COR0005-BONUS-SUST
.          MOVE RUECC5-COR0005-BONUS-SUST
.                                      TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF
.                                         RUECC5-COR0005-BONUS-SUST)
.          ADD  LENGTH                 OF RUECC5-COR0005-BONUS-SUST
.                                      TO WRK-TAM
.                                         WRK-POSICAO.
.
.     ***  TpConfdd
.          MOVE ALL '*'                TO RUECC5-GP-COR0005-CONFDD
.          MOVE RUECC5-COR0005-16B     TO WRK-B09A-SIS (WRK-POSICAO:
.                                         LENGTH OF
.                                         RUECC5-COR0005-16B)
.          ADD  LENGTH                 OF RUECC5-COR0005-16B
.                                      TO WRK-TAM
050825                                    WRK-POSICAO.

      ***  DtMovto
           EXEC SQL
             SET :WRK-DATA-DB2         = CURRENT DATE
           END-EXEC.

           MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
           MOVE WRK-MES-DB2            TO WRK-MES-ISO.
           MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
           MOVE WRK-DATA-ISO-N8        TO RUECC5-DTMOVTO.

NAV506     MOVE RUECC5-COR0005-17      TO WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-17)
NAV506     ADD  LENGTH                 OF RUECC5-COR0005-17
                                       TO WRK-TAM
                                          WRK-POSICAO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR1118*---------------------------------------------------------------*
.      3105-INSERE-HIST                SECTION.
.     *---------------------------------------------------------------*
.          MOVE 1                  TO NENVIO-RECOR       OF RUECB09B.
.          MOVE 'COR0005'          TO CMSGEM-RECOR       OF RUECB09B.
.          MOVE SPACES             TO CCTRL-RQUIS-FINCR  OF RUECB09B.
.          MOVE SPACES             TO CUNIC-MSGEM        OF RUECB09B.
.          MOVE CREFT-RECOR OF RUFIB002
.                                  TO CREFT-RECOR        OF RUECB09B.
.          MOVE 2                  TO CSIT-MSGEM         OF RUECB09B.
.          MOVE '999999999'        TO CUSUAR-RESP        OF RUECB09B.
.
.          EXEC SQL
.            INSERT INTO DB2PRD.THIST_ENVIO_RECOR
.                  (CCONTR_CREDT_RURAL
.                 , NENVIO_RECOR
.                 , HULT_ATULZ
.                 , CMSGEM_RECOR
.                 , HINCL
.                 , CCTRL_RQUIS_FINCR
.                 , CUNIC_MSGEM
.                 , CREFT_RECOR
.                 , CSIT_MSGEM
.                 , CUSUAR_RESP)
.            VALUES
.                  (:RUECB09B.CCONTR-CREDT-RURAL
.                 , :RUECB09B.NENVIO-RECOR
.                 , CURRENT TIMESTAMP
.                 , :RUECB09B.CMSGEM-RECOR
.                 , CURRENT TIMESTAMP
.                 , :RUECB09B.CCTRL-RQUIS-FINCR
.                 , :RUECB09B.CUNIC-MSGEM
.                 , :RUECB09B.CREFT-RECOR
.                 , :RUECB09B.CSIT-MSGEM
.                 , :RUECB09B.CUSUAR-RESP)
.          END-EXEC.
.
.          IF  SQLCODE                  EQUAL +100
.              MOVE 'DB2'               TO ERR-TIPO-ACESSO
.              MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
.              MOVE 'INSERT'            TO ERR-FUN-COMANDO
.              MOVE SQLCODE             TO ERR-SQL-CODE
.              MOVE '3105'              TO ERR-LOCAL
.              MOVE SPACES              TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.          IF (SQLCODE                  NOT EQUAL ZEROS) OR
.             (SQLWARN0                 EQUAL 'W')
.              MOVE 'DB2'               TO ERR-TIPO-ACESSO
.              MOVE 'THIST_ENVIO_RECOR ' TO ERR-DBD-TAB
.              MOVE 'INSERT'            TO ERR-FUN-COMANDO
.              MOVE SQLCODE             TO ERR-SQL-CODE
.              MOVE '3105'              TO ERR-LOCAL
.              MOVE SPACES              TO ERR-SEGM
.              PERFORM 9000-ROTINA-FIM
.          END-IF.
.
.     *----------------------------------------------------------------*
.      3105-99-FIM.                    EXIT.
BR1118*----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3110-OBTER-NRO-CONTROLE-IF      SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT VALUE ( MAX ( NENVIO_RECOR ) , 0 )
             INTO   :RUECB09B.NENVIO-RECOR
             FROM   DB2PRD.THIST_ENVIO_RECOR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '311A'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100  OR
               NENVIO-RECOR            OF RUECB09B EQUAL ZEROS
BR1118         PERFORM 3105-INSERE-HIST
BR1118     ELSE

             EXEC SQL
              SELECT VALUE ( MAX ( HULT_ATULZ ) ,
                           '0001-01-01-00.00.00.000000' )
              INTO   :RUECB09B.HULT-ATULZ
              FROM   DB2PRD.THIST_ENVIO_RECOR
              WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
              AND    NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
             END-EXEC

             IF   SQLCODE                EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '311B'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
             END-IF


             IF  (SQLCODE                NOT EQUAL ZEROS )
             OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '311B'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
             END-IF

             EXEC SQL
              SELECT CCTRL_RQUIS_FINCR
              INTO   :RUECB09B.CCTRL-RQUIS-FINCR :WRK-B09B-CCTRL-NULL
              FROM   DB2PRD.THIST_ENVIO_RECOR
              WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
              AND    NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
              AND    HULT_ATULZ         = :RUECB09B.HULT-ATULZ
             END-EXEC

             IF   SQLCODE                EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '311C'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
             END-IF

             IF  (SQLCODE                NOT EQUAL ZEROS )
             OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '311C'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
             END-IF

             IF  WRK-B09B-CCTRL-NULL     NOT EQUAL ZEROS
               MOVE SPACES             TO CCTRL-RQUIS-FINCR OF RUECB09B
             END-IF

             MOVE CCTRL-RQUIS-FINCR      OF RUECB09B
                                         TO WRK-NUMCTRLIF

           END-IF.
           EXEC SQL
             SET :WRK-TIMESTAMP        = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE '0237RUEC'             TO WRK-NUMCTRLIF-FIXO1.
           MOVE WRK-FORMA-AAAA         TO WRK-NUMCTRLIF-ANO.
           MOVE WRK-FORMA-MM           TO WRK-NUMCTRLIF-MES.
           MOVE WRK-FORMA-DD           TO WRK-NUMCTRLIF-DIA.

           MOVE WRK-NUMCTRLIF-SEQ      TO WRK-NUMCTRLIF-AUX

           IF   WRK-NUMCTRLIF-SEQ      NUMERIC
                ADD 1                  TO WRK-NUMCTRLIF-AUX
           ELSE
                MOVE 1                 TO WRK-NUMCTRLIF-AUX
           END-IF.

           MOVE WRK-NUMCTRLIF-AUX      TO WRK-NUMCTRLIF-SEQ

           MOVE WRK-NUMCTRLIF          TO CCTRL-RQUIS-FINCR OF RUECB09B.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3120-OBTER-CNPJ-AGENCIA         SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CCNPJ_ENTID_RESP
             ,      CFLIAL_CNPJ_ENTID
             ,      CCTRL_CNPJ_ENTID
             INTO   :RUECB099.CCNPJ-ENTID-RESP
             ,      :RUECB099.CFLIAL-CNPJ-ENTID
             ,      :RUECB099.CCTRL-CNPJ-ENTID
             FROM   DB2PRD.TCONTR_ENVIO_RECOR
             WHERE  CCONTR_CREDT_RURAL = :RUECB099.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB099.NENVIO-RECOR
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3120'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
BR0118********-MOVE 04                 TO RUEC65-COD-RET
BR0118         MOVE 60746948           TO CCNPJ-ENTID-RESP  OF RUECB099
BR0118         MOVE 0001               TO CFLIAL-CNPJ-ENTID OF RUECB099
BR0118         MOVE 63                 TO CCTRL-CNPJ-ENTID  OF RUECB099
BR0118********-MOVE 'CONTRATO NAO FOI ENVIADO AO BACEN'
BR0118********-                        TO RUEC65-MENSAGEM
BR0118********-PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3130-OBTER-DADOS-CONTRATO       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             SELECT CREFT_RECOR
             ,      VFINAN
             ,      QPROTV_ADQUI
             ,      DVCTO_CONTR
4.13         ,      CORIGE_REC
BR0621       ,      CCETFC_ORGNC_EMPTO
BR0621       ,      EURL_CETFC_ORGNC
BR0621       ,      CRSTRB_EMPTO_RURAL
BR0621       ,      EURL_RSTRB_EMPTO
BR0621       ,      PENERG_RENOV_EMPTO
7COMMP       ,      VALUE(CFNALD_RURAL, 0)
7COMMP       ,      VALUE(CPRODT_RURAL, 0)
CAP326       ,      CCTRO_CUSTO
             INTO   :RUFIB002.CREFT-RECOR
             ,      :RUFIB002.VFINAN
             ,      :RUFIB002.QPROTV-ADQUI :WRK-B002-QPROTV-NULL
             ,      :RUFIB002.DVCTO-CONTR
4.13         ,      :RUFIB002.CORIGE-REC   :WRK-B002-CORIGE-NULL
BR0621       ,      :RUFIB002.CCETFC-ORGNC-EMPTO
BR0621                                     :WRK-B002-CCETFC-NULL
BR0621       ,      :RUFIB002.EURL-CETFC-ORGNC
BR0621                                     :WRK-B002-EURL-CETFC-NULL
BR0621       ,      :RUFIB002.CRSTRB-EMPTO-RURAL
BR0621                                     :WRK-B002-CRSTRB-NULL
BR0621       ,      :RUFIB002.EURL-RSTRB-EMPTO
BR0621                                     :WRK-B002-EURL-RSTRB-NULL
BR0621       ,      :RUFIB002.PENERG-RENOV-EMPTO
BR0621                                     :WRK-B002-PENERG-NULL
7COMMP       ,      :RUFIB002.CFNALD-RURAL
7COMMP       ,      :RUFIB002.CPRODT-RURAL
CAP326       ,      :RUFIB002.CCTRO-CUSTO
             FROM   DB2PRD.CONTRATO_RURAL
             WHERE  CCONTR             = :RUFIB002.CCONTR
           END-EXEC.

           IF   SQLCODE                EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3130'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'CONTRATO_RURAL'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3130'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.
           IF  WRK-B002-QPROTV-NULL    NOT EQUAL ZEROS
               MOVE ZEROS              TO QPROTV-ADQUI OF RUFIB002
           END-IF.

           IF  WRK-B002-CORIGE-NULL    NOT EQUAL ZEROS
               MOVE ZEROS              TO  CORIGE-REC  OF RUFIB002
           END-IF.

BR0621     IF  WRK-B002-CCETFC-NULL     LESS ZEROS
.              MOVE SPACES              TO CCETFC-ORGNC-EMPTO
.                                       OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-EURL-CETFC-NULL LESS ZEROS
.              MOVE SPACES              TO EURL-CETFC-ORGNC-TEXT
.                                       OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-CRSTRB-NULL     LESS ZEROS
.              MOVE SPACES              TO CRSTRB-EMPTO-RURAL
.                                       OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-EURL-RSTRB-NULL LESS ZEROS
.              MOVE SPACES              TO EURL-RSTRB-EMPTO-TEXT
.                                       OF RUFIB002
.          END-IF.
.
.          IF  WRK-B002-PENERG-NULL     LESS ZEROS
.              MOVE ZEROS               TO PENERG-RENOV-EMPTO
.                                       OF RUFIB002
BR0621     END-IF.

      *----------------------------------------------------------------*
       3130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR0420*---------------------------------------------------------------*
BR0420 3131-OBTER-ORIGEM-ANTERIOR      SECTION.
BR0420*---------------------------------------------------------------*
BR0420
BR0420     MOVE RUEC65-CCONTR          TO CCONTR OF RUFIB044.
BR0420
BR0420     EXEC SQL
BR0420       SELECT
BR0420         CCONTR
BR0420       , NMOTVO_DCLAS_CONTR
BR0420       , CMOTVO_DCLAS_OPER
BR0420       , DDCLAS_OPER_CREDT
BR0420       INTO
BR0420         :RUFIB044.CCONTR
BR0420       , :RUFIB044.NMOTVO-DCLAS-CONTR
BR0420       , :RUFIB044.CMOTVO-DCLAS-OPER
BR0420       , :RUFIB044.DDCLAS-OPER-CREDT
BR0420       FROM  DB2PRD.TMOTVO_DCLAS_CONTR
BR0420       WHERE CCONTR                = :RUFIB044.CCONTR
BR0420       ORDER BY
BR0420             CMOTVO_DCLAS_OPER DESC
BR0420       FETCH FIRST 1 ROW ONLY
BR0420     END-EXEC.
BR0420
BR0420     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
BR0420     OR  (SQLWARN0               EQUAL 'W')
BR0420         MOVE 99                 TO RUEC65-COD-RET
BR0420         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0420         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0420         MOVE 'TMOTVO_DCLAS_CONTR'  TO ERR-DBD-TAB
BR0420         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BR0420         MOVE SQLCODE            TO ERR-SQL-CODE
BR0420         MOVE '313A'             TO ERR-LOCAL
BR0420         MOVE SPACES             TO ERR-SEGM
BR0420         PERFORM 9000-ROTINA-FIM
BR0420     END-IF.
BR0420
BR0420     IF  SQLCODE                 EQUAL +100
BR0420         MOVE ZEROS              TO WRK-ORIGEM-ANT-DESCL
BR0420         GO TO 3131-99-FIM
BR0420     END-IF.
BR0420
BR0420     MOVE RUEC65-CCONTR          TO B003-CH-CONTR.
BR0420
BR0420     EXEC SQL
BR0420       SELECT
BR0420         B003_ORIGREC1
BR0420       , B003_ORIGREC2
BR0420       , B003_ORIGREC3
BR0420       , B003_ORIGREC4
BR0420       INTO
BR0420         :B003-ORIGREC1
BR0420       , :B003-ORIGREC2
BR0420       , :B003-ORIGREC3
BR0420       , :B003-ORIGREC4
BR0420       FROM  DB2PRD.RUCA_CAD_ROOT
BR0420       WHERE B003_CH_CONTR       = :B003-CH-CONTR
BR0420     END-EXEC.
BR0420
BR0420     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
BR0420     OR  (SQLWARN0               EQUAL 'W')
BR0420         MOVE 99                 TO RUEC65-COD-RET
BR0420         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0420         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0420         MOVE 'RUCA_CAD_ROOT'    TO ERR-DBD-TAB
BR0420         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BR0420         MOVE SQLCODE            TO ERR-SQL-CODE
BR0420         MOVE '313B'             TO ERR-LOCAL
BR0420         MOVE SPACES             TO ERR-SEGM
BR0420         PERFORM 9000-ROTINA-FIM
BR0420     END-IF.
BR0420
BR0420     IF  SQLCODE                 EQUAL +100
BR0420         MOVE ZEROS              TO WRK-ORIGEM-ANT-DESCL
BR0420         GO TO 3131-99-FIM
BR0420     END-IF.
BR0420
BR0420     IF  B003-ORIGREC1           NOT EQUAL ZEROS
BR0420         MOVE B003-ORIGREC1      TO WRK-ORIGEM-ANT-DESCL
BR0420     END-IF.
BR0420
BR0420     IF  B003-ORIGREC2           NOT EQUAL ZEROS
BR0420         MOVE B003-ORIGREC2      TO WRK-ORIGEM-ANT-DESCL
BR0420     END-IF.
BR0420
BR0420     IF  B003-ORIGREC3           NOT EQUAL ZEROS
BR0420         MOVE B003-ORIGREC3      TO WRK-ORIGEM-ANT-DESCL
BR0420     END-IF.
BR0420
BR0420     IF  B003-ORIGREC4           NOT EQUAL ZEROS
BR0420         MOVE B003-ORIGREC4      TO WRK-ORIGEM-ANT-DESCL
BR0420     END-IF.
BR0420
BR0420*----------------------------------------------------------------*
BR0420 3131-99-FIM.                    EXIT.
BR0420*----------------------------------------------------------------*
BR0420
      *---------------------------------------------------------------*
       3140-OBTER-DATA-VENCIMENTO      SECTION.
      *---------------------------------------------------------------*

           MOVE 6000                   TO WRK-NUMERO-04-INI
           MOVE 6999                   TO WRK-NUMERO-04-FIM

           EXEC SQL
             SELECT VALUE ( MAX (DEVNTO) , '01.01.0001' )
             INTO   :RUECV012.DEVNTO
             FROM   DB2PRD.V01EVNTO_RURAL
             WHERE  CCONTR             = :RUECV012.CCONTR
             AND    CTPO_EVNTO_RURAL  >= :WRK-NUMERO-04-INI
             AND    CTPO_EVNTO_RURAL  <= :WRK-NUMERO-04-FIM
           END-EXEC.

           IF   SQLCODE                EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3140'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'V01EVNTO_RURAL'   TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3140'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.
      *----------------------------------------------------------------*
       3140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3150-TRATAR-GRUPO-LIBERACAO     SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR          TO B004-CH-CONTR.

           PERFORM 3151-OPEN-CSR01-RUCAB004.

           PERFORM 3152-FETCH-CSR01-RUCAB004.

           IF  WRK-FIM-CSR01           EQUAL 'S'
      ***      DtLibrcRecEmpnmnt
               MOVE ALL '*'            TO RUECC5-DTLIBRC-RECUR
      ***      VlrRecLibrc
               MOVE ALL '*'            TO RUECC5-VLR-LIBRC-RECUR
               PERFORM 3153-CLOSE-CSR01-RUCAB004

               MOVE RUECC5-COR0005-4   TO WRK-B09A-SIS (WRK-POSICAO:
                                          LENGTH OF RUECC5-COR0005-4 )
               ADD  LENGTH             OF RUECC5-COR0005-4
                                       TO WRK-TAM
                                          WRK-POSICAO
               GO TO 3150-99-FIM
           END-IF.

           MOVE B004-DTEVENTO          TO WRK-DLIBRC-ANT.

           PERFORM 3154-MONTAR-LIBERACOES
             UNTIL WRK-FIM-CSR01       EQUAL 'S'

      ***  DtLibrcRecEmpnmnt
           MOVE WRK-DLIBRC-ANT         TO WRK-DATA-DB2.
           MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
           MOVE WRK-MES-DB2            TO WRK-MES-ISO.
           MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
           MOVE WRK-DATA-ISO-GRUPO     TO RUECC5-DTLIBRC-RECUR.
      ***  VlrRecLibrc
           MOVE WRK-VLIBRC-COMP3       TO WRK-SZD17V2
           MOVE WRK-CH19               TO RUECC5-VLR-LIBRC-RECUR.

           MOVE RUECC5-COR0005-4       TO WRK-B09A-SIS (WRK-POSICAO:
                                          LENGTH OF RUECC5-COR0005-4 )
           ADD  LENGTH                 OF RUECC5-COR0005-4
                                       TO WRK-TAM
                                          WRK-POSICAO

           PERFORM 3153-CLOSE-CSR01-RUCAB004.

      *----------------------------------------------------------------*
       3150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3151-OPEN-CSR01-RUCAB004        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR01-RUCAB004
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO ERR-DBD-TAB
               MOVE 'OPEN'             TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3151'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR01.

      *----------------------------------------------------------------*
       3151-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3152-FETCH-CSR01-RUCAB004       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-RUCAB004
             INTO  :RUCAB004.B004-CH-CONTR
             ,     :RUCAB004.B004-DTEVENTO
             ,     :RUCAB004.B004-DEBCRED
             ,     :RUCAB004.B004-CODLANC
             ,     :RUCAB004.B004-VLPRICOR
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3152'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR01
           END-IF.

      *----------------------------------------------------------------*
       3152-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3153-CLOSE-CSR01-RUCAB004       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-RUCAB004
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUCA_CAD_DEP'     TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '3153'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       3153-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       3154-MONTAR-LIBERACOES          SECTION.
      *---------------------------------------------------------------*

           IF  B004-DTEVENTO           NOT EQUAL WRK-DLIBRC-ANT
      ***      DtLibrcRecEmpnmnt
               MOVE WRK-DLIBRC-ANT     TO WRK-DATA-DB2
               MOVE WRK-DIA-DB2        TO WRK-DIA-ISO
               MOVE WRK-MES-DB2        TO WRK-MES-ISO
               MOVE WRK-ANO-DB2        TO WRK-ANO-ISO
               MOVE WRK-DATA-ISO-GRUPO TO RUECC5-DTLIBRC-RECUR
      ***      VlrRecLibrc
               MOVE WRK-VLIBRC-COMP3   TO WRK-SZD17V2
               MOVE WRK-CH19           TO RUECC5-VLR-LIBRC-RECUR
               MOVE ZEROS              TO WRK-VLIBRC-COMP3

               MOVE RUECC5-COR0005-4   TO WRK-B09A-SIS (WRK-POSICAO:
                                          LENGTH OF RUECC5-COR0005-4 )
               ADD  LENGTH             OF RUECC5-COR0005-4
                                       TO WRK-TAM
                                          WRK-POSICAO
               MOVE ZEROS              TO WRK-VLIBRC-COMP3

           END-IF.

           IF  B004-CODLANC            LESS 60
           AND B004-CODLANC            NOT EQUAL 58
           AND B004-CODLANC            NOT EQUAL 59
      ***  *** LIBERACAO
               ADD B004-VLPRICOR       TO WRK-VLIBRC-COMP3
           ELSE
      ***  *** ESTORNO
               SUBTRACT B004-VLPRICOR  FROM WRK-VLIBRC-COMP3
           END-IF.

           MOVE B004-DTEVENTO          TO WRK-DLIBRC-ANT.
           PERFORM 3152-FETCH-CSR01-RUCAB004.

      *----------------------------------------------------------------*
       3154-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
WIP001*---------------------------------------------------------------*
-      3160-TRATAR-GRUPO-DESCLASS      SECTION.
-     *---------------------------------------------------------------*
NAV506
NAV506     IF  RUEC65-TP-DESCLASS      EQUAL 'P'
NAV506     OR  RUEC65-TP-DESCLASS      EQUAL '*'
NAV506***      DtDesclassEmpnmnt
NAV506         MOVE ALL '*'            TO RUECC5-DT-DESCLASS
NAV506***      MotvDesclassCOR
NAV506         MOVE ALL '*'            TO RUECC5-MTV-DESCLASS
NAV506         MOVE RUECC5-COR0005-15  TO WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-15)
NAV506         ADD  LENGTH             OF RUECC5-COR0005-15
NAV506                                 TO WRK-TAM
NAV506                                    WRK-POSICAO
NAV506     ELSE
NAV506
NAV506         MOVE RUEC65-CCONTR          TO CCONTR OF RUFIB044
NAV506
NAV506         PERFORM 3161-OPEN-CSR02-RUFIB044-JN
NAV506
NAV506         PERFORM 3162-FETCH-CSR02-RUFIB044-JN
NAV506
NAV506         IF  WRK-FIM-CSR02           EQUAL 'S'
NAV506***          DtDesclassEmpnmnt
NAV506             MOVE ALL '*'            TO RUECC5-DT-DESCLASS
NAV506***          MotvDesclassCOR
NAV506             MOVE ALL '*'            TO RUECC5-MTV-DESCLASS
NAV506             MOVE RUECC5-COR0005-15  TO
NAV506                                    WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-15)
NAV506             ADD  LENGTH             OF RUECC5-COR0005-15
NAV506                                     TO WRK-TAM
NAV506                                        WRK-POSICAO
NAV506         END-IF
NAV506
NAV506         PERFORM 3164-MONTAR-DESCLASSIFICACOES
NAV506           UNTIL WRK-FIM-CSR02       EQUAL 'S'
NAV506
NAV506         PERFORM 3163-CLOSE-CSR02-RUFIB044-JN
NAV506     END-IF.
-
-     *----------------------------------------------------------------*
-      3160-99-FIM.                    EXIT.
-     *----------------------------------------------------------------*
-     *---------------------------------------------------------------*
-      3161-OPEN-CSR02-RUFIB044-JN     SECTION.
-     *---------------------------------------------------------------*
-
-          EXEC SQL
-            OPEN CSR02-RUFIB044-JN
-          END-EXEC.
-
-          IF  (SQLCODE                NOT EQUAL ZEROS )
-          OR  (SQLWARN0               EQUAL 'W')
-              MOVE 99                 TO RUEC65-COD-RET
-              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
-              MOVE 'DB2'              TO ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO ERR-DBD-TAB
-              MOVE 'OPEN'             TO ERR-FUN-COMANDO
-              MOVE SQLCODE            TO ERR-SQL-CODE
-              MOVE '3161'             TO ERR-LOCAL
-              MOVE SPACES             TO ERR-SEGM
-              PERFORM 9000-ROTINA-FIM
-          END-IF.
-
-          MOVE 'N'                    TO WRK-FIM-CSR02.
-
-     *----------------------------------------------------------------*
-      3161-99-FIM.                    EXIT.
-     *----------------------------------------------------------------*
-     *---------------------------------------------------------------*
-      3162-FETCH-CSR02-RUFIB044-JN    SECTION.
-     *---------------------------------------------------------------*
-
-          EXEC SQL
-            FETCH CSR02-RUFIB044-JN
-            INTO  :RUFIB044.CMOTVO-DCLAS-OPER
-            ,     :RUFIB044.DDCLAS-OPER-CREDT
-            ,     :RUFIB044.VPRINC-DCLAS-CONTR
-            ,     :RURCB097.CMOTVO-DCLAS-BACEN
-          END-EXEC.
-
-          IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
-          OR  (SQLWARN0               EQUAL 'W')
-              MOVE 99                 TO RUEC65-COD-RET
-              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
-              MOVE 'DB2'              TO ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO ERR-DBD-TAB
-              MOVE 'FETCH'            TO ERR-FUN-COMANDO
-              MOVE SQLCODE            TO ERR-SQL-CODE
-              MOVE '3162'             TO ERR-LOCAL
-              MOVE SPACES             TO ERR-SEGM
-              PERFORM 9000-ROTINA-FIM
-          END-IF.
-
-          IF  SQLCODE                 EQUAL +100
-              MOVE 'S'                TO WRK-FIM-CSR02
-          END-IF.
-
-     *----------------------------------------------------------------*
-      3162-99-FIM.                    EXIT.
-     *----------------------------------------------------------------*
-     *---------------------------------------------------------------*
-      3163-CLOSE-CSR02-RUFIB044-JN   SECTION.
-     *---------------------------------------------------------------*
-
-          EXEC SQL
-            CLOSE CSR02-RUFIB044-JN
-          END-EXEC.
-
-          IF  (SQLCODE                NOT EQUAL ZEROS)
-          OR  (SQLWARN0               EQUAL 'W')
-              MOVE 99                 TO RUEC65-COD-RET
-              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
-              MOVE 'DB2'              TO ERR-TIPO-ACESSO
-              MOVE 'TMOTVO_DCLAS_CONTR'
-                                      TO ERR-DBD-TAB
-              MOVE 'CLOSE'            TO ERR-FUN-COMANDO
-              MOVE SQLCODE            TO ERR-SQL-CODE
-              MOVE '3163'             TO ERR-LOCAL
-              MOVE SPACES             TO ERR-SEGM
-              PERFORM 9000-ROTINA-FIM
-          END-IF.
-
-     *----------------------------------------------------------------*
-      3163-99-FIM.                    EXIT.
-     *----------------------------------------------------------------*
-     *---------------------------------------------------------------*
-      3164-MONTAR-DESCLASSIFICACOES   SECTION.
-     *---------------------------------------------------------------*
-
-     ***  DtDesclassEmpnmnt
-          MOVE DDCLAS-OPER-CREDT      OF RUFIB044
-                                      TO WRK-DATA-DB2.
-          MOVE WRK-DATA-DB2(1:2)      TO RUECC5-DT-DESCLASS(7:2)
-          MOVE WRK-DATA-DB2(4:2)      TO RUECC5-DT-DESCLASS(5:2)
-          MOVE WRK-DATA-DB2(7:4)      TO RUECC5-DT-DESCLASS(1:4)
-
-     ***  MotvDesclassCOR
-          MOVE CMOTVO-DCLAS-BACEN     OF RURCB097
-                                      TO RUECC5-MTV-DESCLASS.
-
-
-          MOVE RUECC5-COR0005-15      TO WRK-B09A-SIS (WRK-POSICAO:
-                                         LENGTH OF RUECC5-COR0005-15)
-          ADD  LENGTH                 OF RUECC5-COR0005-15
-                                      TO WRK-TAM
-                                         WRK-POSICAO.
-
-          PERFORM 3162-FETCH-CSR02-RUFIB044-JN.
-
-     *----------------------------------------------------------------*
-      3164-99-FIM.                    EXIT.
WIP001*----------------------------------------------------------------*
NAV506*---------------------------------------------------------------*
NAV506 3165-TRATAR-GR-DESCLASS-PAR     SECTION.
NAV506*---------------------------------------------------------------*
NAV506
NAV506     IF  RUEC65-TP-DESCLASS      EQUAL 'T'
NAV506     OR  RUEC65-TP-DESCLASS      EQUAL '*'
NAV506***      DESCLASSIFICACAO PARCIAL
NAV506*
NAV506***      DtDesclassParclEmpnmnt
NAV506         MOVE ALL '*'            TO RUECC5-DT-DESCLASS-PAR
NAV506***      VlrDesclascd
NAV506         MOVE ALL '*'            TO RUECC5-VLR-DESCLASS-PAR
NAV506***      MotvDesclassCOR
NAV506         MOVE ALL '*'            TO RUECC5-MTV-DESCLASS-PAR
NAV506         MOVE RUECC5-COR0005-16  TO WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-16)
NAV506         ADD  LENGTH             OF RUECC5-COR0005-16
NAV506                                 TO WRK-TAM
NAV506                                    WRK-POSICAO
NAV506     ELSE
NAV506
NAV506         MOVE RUEC65-CCONTR          TO CCONTR OF RUFIB044
NAV506
NAV506         PERFORM 3161-OPEN-CSR02-RUFIB044-JN
NAV506
NAV506         PERFORM 3162-FETCH-CSR02-RUFIB044-JN
NAV506
NAV506         IF  WRK-FIM-CSR02           EQUAL 'S'
NAV506***          DESCLASSIFICACAO PARCIAL
NAV506*
NAV506***          DtDesclassParclEmpnmnt
NAV506             MOVE ALL '*'            TO RUECC5-DT-DESCLASS-PAR
NAV506***          VlrDesclascd
NAV506             MOVE ALL '*'            TO RUECC5-VLR-DESCLASS-PAR
NAV506***          MotvDesclassCOR
NAV506             MOVE ALL '*'            TO RUECC5-MTV-DESCLASS-PAR
NAV506             MOVE RUECC5-COR0005-16  TO
NAV506                                    WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-16)
NAV506             ADD  LENGTH             OF RUECC5-COR0005-16
NAV506                                     TO WRK-TAM
NAV506                                        WRK-POSICAO
NAV506         END-IF
NAV506
NAV506         PERFORM 3166-MONTAR-DESCLASS-PARCIAL
NAV506           UNTIL WRK-FIM-CSR02       EQUAL 'S'
NAV506
NAV506         PERFORM 3163-CLOSE-CSR02-RUFIB044-JN
NAV506     END-IF.
NAV506
NAV506*----------------------------------------------------------------*
NAV506 3165-99-FIM.                    EXIT.
NAV506*----------------------------------------------------------------*
NAV506*---------------------------------------------------------------*
NAV506 3166-MONTAR-DESCLASS-PARCIAL    SECTION.
NAV506*---------------------------------------------------------------*
NAV506
NAV506     MOVE DDCLAS-OPER-CREDT      OF RUFIB044
NAV506                                 TO WRK-DATA-DB2.
NAV506***  DESCLASSIFICACAO PARCIAL
NAV506*
NAV506***  DtDesclassParclEmpnmnt
NAV506     MOVE WRK-DATA-DB2(1:2)      TO RUECC5-DT-DESCLASS-PAR(7:2)
NAV506     MOVE WRK-DATA-DB2(4:2)      TO RUECC5-DT-DESCLASS-PAR(5:2)
NAV506     MOVE WRK-DATA-DB2(7:4)      TO RUECC5-DT-DESCLASS-PAR(1:4)
NAV506***  VlrDesclascd
NAV506     MOVE VPRINC-DCLAS-CONTR     OF RUFIB044
NAV506                                 TO WRK-SZD17V2
NAV506     MOVE WRK-CH19               TO RUECC5-VLR-DESCLASS-PAR
NAV506***  MotvDesclassCOR
NAV506     MOVE CMOTVO-DCLAS-BACEN     OF RURCB097
NAV506                                 TO RUECC5-MTV-DESCLASS-PAR
NAV506     MOVE RUECC5-COR0005-16      TO WRK-B09A-SIS (WRK-POSICAO:
NAV506                                    LENGTH OF RUECC5-COR0005-16)
NAV506     ADD  LENGTH                 OF RUECC5-COR0005-16
NAV506                                 TO WRK-TAM
NAV506                                    WRK-POSICAO
NAV506*
NAV506     PERFORM 3162-FETCH-CSR02-RUFIB044-JN.
NAV506
NAV506*----------------------------------------------------------------*
NAV506 3166-99-FIM.                    EXIT.
NAV506*----------------------------------------------------------------*
      *---------------------------------------------------------------*
       3170-TRATAR-GRUPO-COOPERADO     SECTION.
      *---------------------------------------------------------------*

BR0518     MOVE ZEROS                  TO NID-COOP-RURAL OF RUFIB092.
BR0518     MOVE RUEC65-CCONTR          TO CCONTR         OF RUFIB092.
BR0518
BR0518     PERFORM 3171-OPEN-CSR04-RUFIB092.
BR0518
BR0518     PERFORM 3172-FETCH-CSR04-RUFIB092.
BR0518

BR0518     IF  WRK-FIM-CSR04           EQUAL 'S'
      ***      TpPessoaCooperd
               MOVE ALL '*'            TO RUECC5-TPSSOA-COOP

      ***      CNPJ_CPFCooperd
               MOVE ALL '*'            TO RUECC5-CNPJ-CPF-COOP-X

      ***      VlrParclCooperd
               MOVE ALL '*'            TO RUECC5-VLR-PCELA-COOP

      ***      CodProg_LinhaCredCooperd
               MOVE ALL '*'            TO RUECC5-CODPROG-LINCRED

               MOVE RUECC5-COR0005-6   TO WRK-B09A-SIS (WRK-POSICAO:
                                       LENGTH OF RUECC5-COR0005-6 )
               ADD  LENGTH             OF RUECC5-COR0005-6
                                       TO WRK-TAM
                                       WRK-POSICAO
           END-IF.


BR0518     PERFORM 3174-MONTAR-COOPERADOS
BR0518       UNTIL WRK-FIM-CSR04       EQUAL 'S'.
BR0518
BR0518     PERFORM 3173-CLOSE-CSR04-RUFIB092.
BR0518
      *----------------------------------------------------------------*
       3170-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR0518*---------------------------------------------------------------*
BR0518 3171-OPEN-CSR04-RUFIB092        SECTION.
BR0518*---------------------------------------------------------------*
BR0518
BR0518     EXEC SQL
BR0518       OPEN CSR04-RUFIB092
BR0518     END-EXEC.
BR0518
BR0518     IF  (SQLCODE                NOT EQUAL ZEROS )
BR0518     OR  (SQLWARN0               EQUAL 'W')
BR0518         MOVE 99                 TO RUEC65-COD-RET
BR0518         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0518         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0518         MOVE 'TINFO_COOP_CONTR' TO ERR-DBD-TAB
BR0518         MOVE 'OPEN'             TO ERR-FUN-COMANDO
BR0518         MOVE SQLCODE            TO ERR-SQL-CODE
BR0518         MOVE '3171'             TO ERR-LOCAL
BR0518         MOVE SPACES             TO ERR-SEGM
BR0518         PERFORM 9000-ROTINA-FIM
BR0518     END-IF.
BR0518
BR0518     MOVE 'N'                    TO WRK-FIM-CSR04.
BR0518
BR0518*----------------------------------------------------------------*
BR0518 3171-99-FIM.                    EXIT.
BR0518*----------------------------------------------------------------*
BR0518
BR0518*---------------------------------------------------------------*
BR0518 3172-FETCH-CSR04-RUFIB092       SECTION.
BR0518*---------------------------------------------------------------*
BR0518
BR0518     EXEC SQL
BR0518       FETCH CSR04-RUFIB092
BR0518       INTO  :RUFIB092.CTPO-PSSOA
BR0518       ,     :RUFIB092.CCNPJ-CPF-COOP
BR0518       ,     :RUFIB092.CFLIAL-CNPJ-COOP
BR0518       ,     :RUFIB092.CCTRL-CPF-COOP
BR0518       ,     :RUFIB092.VPCELA-COOP-RURAL
BR0518       ,     :RUFIB092.CPROG-RURAL
BR0518                  :WRK-B092-CPROG-RURAL
BR0518     END-EXEC.
BR0518
BR0518     IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
BR0518     OR  (SQLWARN0               EQUAL 'W')
BR0518         MOVE 99                 TO RUEC65-COD-RET
BR0518         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0518         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0518         MOVE 'TINFO_COOP_CONTR' TO ERR-DBD-TAB
BR0518         MOVE 'FETCH'            TO ERR-FUN-COMANDO
BR0518         MOVE SQLCODE            TO ERR-SQL-CODE
BR0518         MOVE '3172'             TO ERR-LOCAL
BR0518         MOVE SPACES             TO ERR-SEGM
BR0518         PERFORM 9000-ROTINA-FIM
BR0518     END-IF.
BR0518
BR0518     IF  SQLCODE                 EQUAL +100
BR0518         MOVE 'S'                TO WRK-FIM-CSR04
BR0518     END-IF.
BR0518
BR0518     IF WRK-B092-CPROG-RURAL     NOT EQUAL ZEROS
BR0518        MOVE ZEROS               TO  CPROG-RURAL OF RUFIB092
BR0518     END-IF.
BR0518
BR0518*----------------------------------------------------------------*
BR0518 3172-99-FIM.                    EXIT.
BR0518*----------------------------------------------------------------*
BR0518
BR0518*---------------------------------------------------------------*
BR0518 3173-CLOSE-CSR04-RUFIB092      SECTION.
BR0518*---------------------------------------------------------------*
BR0518
BR0518     EXEC SQL
BR0518       CLOSE CSR04-RUFIB092
BR0518     END-EXEC.
BR0518
BR0518     IF  (SQLCODE                NOT EQUAL ZEROS)
BR0518     OR  (SQLWARN0               EQUAL 'W')
BR0518         MOVE 99                 TO RUEC65-COD-RET
BR0518         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0518         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0518         MOVE 'TINFO_COOP_CONTR' TO ERR-DBD-TAB
BR0518         MOVE 'CLOSE'            TO ERR-FUN-COMANDO
BR0518         MOVE SQLCODE            TO ERR-SQL-CODE
BR0518         MOVE '3173'             TO ERR-LOCAL
BR0518         MOVE SPACES             TO ERR-SEGM
BR0518         PERFORM 9000-ROTINA-FIM
BR0518     END-IF.
BR0518
BR0518*----------------------------------------------------------------*
BR0518 3173-99-FIM.                    EXIT.
BR0518*----------------------------------------------------------------*
BR0518
BR0518*---------------------------------------------------------------*
BR0518 3174-MONTAR-COOPERADOS          SECTION.
BR0518*---------------------------------------------------------------*
BR0518
BR0518***  TpPessoaCooperd
BR0518     IF CTPO-PSSOA OF RUFIB092   EQUAL 1
BR0518        MOVE 'F'                 TO RUECC5-TPSSOA-COOP
BR0518     ELSE
BR0518        MOVE 'J'                 TO RUECC5-TPSSOA-COOP
BR0518     END-IF.
BR0518
BR0518***  CNPJ_CPFCooperd
BR0518     MOVE CCNPJ-CPF-COOP         OF RUFIB092
BR0518                                 TO WRK-COD9-NUM
BR0518     MOVE CFLIAL-CNPJ-COOP       OF RUFIB092
BR0518                                 TO WRK-COD5-NUM
BR0518     MOVE CCTRL-CPF-COOP         OF RUFIB092
BR0518                                 TO WRK-COD3-NUM
BR0518
BR0518     IF RUECC5-TPSSOA-COOP       EQUAL 'J'
BR0518        MOVE WRK-COD9-NUM8       TO WRK-EDIT-NCNPJ
BR0518        MOVE WRK-COD5-NUM4       TO WRK-EDIT-FCNPJ
BR0518        MOVE WRK-COD3-NUM2       TO WRK-EDIT-CCNPJ
050825        MOVE WRK-EDIT-CNPJ       TO RUECC5-CNPJ-CPF-COOP-X
BR0518     ELSE
050825        MOVE WRK-COD9-NUM        TO RUECC5-CNPJ-CPF-COOP(1:9)
.             MOVE WRK-COD3-NUM2       TO RUECC5-CNPJ-CPF-COOP(10:2)
050825        MOVE '   '               TO RUECC5-CNPJ-CPF-COOP-X(12:3)
BR0518     END-IF.
BR0518
BR0518***  VlrParclCooperd
BR0518     MOVE VPCELA-COOP-RURAL      OF RUFIB092
BR0518                                 TO WRK-SZD17V2.
BR0518     MOVE WRK-CH19               TO RUECC5-VLR-PCELA-COOP
BR0518
BR0518***  CodProg_LinhaCredCooperd
BR0518     IF CPROG-RURAL OF RUFIB092  EQUAL ZEROS
BR0518        MOVE '0999'              TO RUECC5-CODPROG-LINCRED
BR0518     ELSE
BR0518        MOVE CPROG-RURAL OF RUFIB092
BR0518                                 TO RUECC5-CODPROG-LINCRED
BR0518     END-IF.
BR0518
BR0518     MOVE RUECC5-COR0005-6       TO WRK-B09A-SIS (WRK-POSICAO:
BR0518                                    LENGTH OF RUECC5-COR0005-6 )
BR0518     ADD  LENGTH                 OF RUECC5-COR0005-6
BR0518                                 TO WRK-TAM
BR0518                                    WRK-POSICAO.
BR0518
BR0518     PERFORM 3172-FETCH-CSR04-RUFIB092.
BR0518
BR0518*----------------------------------------------------------------*
BR0518 3174-99-FIM.                    EXIT.
BR0518*----------------------------------------------------------------*

BR4.11*---------------------------------------------------------------*
BR4.11 3180-FORMATA-GRUPO-GLEBA-AST  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11
BR4.11     IF WRK-IND2  EQUAL  1
BR4.11        MOVE  ALL '*'          TO RUECC5-IDENTC-GLEBA
BR4.11        MOVE RUECC5-COR0005-7  TO
BR4.11             WRK-B09A-SIS(WRK-POSICAO:
BR4.11                                  LENGTH OF RUECC5-COR0005-7)
BR4.11        ADD LENGTH OF RUECC5-COR0005-7  TO WRK-TAM
BR4.11                                           WRK-POSICAO
BR4.11        MOVE 1 TO WRK-IND3

BR4.11        PERFORM 3181-FORMATA-GRUPO-PTPOLG-AST
BR4.11             UNTIL WRK-IND3 GREATER  WRK-999
BR4.11        ADD 999 TO WRK-IND2
211025**      SICOR 5.11 - CAMPO <WKTPart>
211025        MOVE  ALL '*'          TO RUECC5-WKT-PART
211025        MOVE RUECC5-COR0005-8C TO WRK-B09A-SIS(WRK-POSICAO:
211025                                  LENGTH OF RUECC5-COR0005-8C)
211025        ADD LENGTH OF RUECC5-COR0005-8C TO WRK-TAM
211025                                           WRK-POSICAO
BR4.11     ELSE
BR4.11        ADD 999 TO WRK-IND2
BR4.11     END-IF.
BR4.11
BR4.11*----------------------------------------------------------------*
BR4.11 3180-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11
BR4.11*---------------------------------------------------------------*
BR4.11 3181-FORMATA-GRUPO-PTPOLG-AST  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11
BR4.11     IF WRK-IND3  EQUAL  1
211025**      CAMPOS EXCLUIDOS - SICOR 5.11
BR4.11*      -MOVE  ALL '*'          TO RUECC5-LAT-PONTO
BR4.11*      -MOVE  ALL '*'          TO RUECC5-LONG-PONTO
BR4.11*      -MOVE  ALL '*'          TO RUECC5-ALTT-PONTO
BR4.11*      -MOVE RUECC5-COR0005-8  TO
BR4.11*           -WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                                -LENGTH OF RUECC5-COR0005-8)
BR4.11*      -ADD LENGTH OF RUECC5-COR0005-8 TO WRK-TAM
BR4.11*                                        -WRK-POSICAO
BR4.11        ADD 999 TO WRK-IND3
BR4.11
BR4.11        MOVE  ALL '*'          TO RUECC5-AREA-NAO-CULT
BR4.11
BR4.11        PERFORM 3182-FORMATA-GRUPO-AREA-N-CULT
BR4.11
BR4.11     ELSE
BR4.11        ADD 999 TO WRK-IND3
BR4.11     END-IF.
BR4.11
BR4.11*----------------------------------------------------------------*
BR4.11 3181-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*

BR4.11*---------------------------------------------------------------*
BR4.11 3182-FORMATA-GRUPO-AREA-N-CULT SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11
BR4.11     MOVE RUECC5-COR0005-8A      TO WRK-B09A-SIS(WRK-POSICAO:
BR4.11                                    LENGTH OF RUECC5-COR0005-8A).
BR4.11     ADD LENGTH OF RUECC5-COR0005-8A TO WRK-TAM
BR4.11                                        WRK-POSICAO.
STE001
211025**   CAMPOS EXCLUIDOS - SICOR 5.11
STE001***  TRATAMENTO GLEBA EXCLUIDA
STE001*   -PERFORM 3183-TRATAR-GLEBA-EXCLUIDA
           .
BR4.11
BR4.11*----------------------------------------------------------------*
BR4.11 3182-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*

211025**   CAMPOS EXCLUIDOS - SICOR 5.11
STE001*---------------------------------------------------------------*
.     *3183-TRATAR-GLEBA-EXCLUIDA    SECTION.
.     *---------------------------------------------------------------*
.     *
STE001*   -MOVE RUECC5-COR0005-8B TO WRK-B09A-SIS(WRK-POSICAO:
STE001*                                -LENGTH OF RUECC5-COR0005-8B)
STE001*   -ADD LENGTH OF RUECC5-COR0005-8B TO WRK-TAM
STE001*                                      -WRK-POSICAO
STE001*
STE001*   -MOVE ALL '*'           TO RUECC5-LAT-PONTO-EXCL
STE001*   -MOVE ALL '*'           TO RUECC5-LONG-PONTO-EXCL
STE001*   -MOVE ALL '*'           TO RUECC5-ALTT-PONTO-EXCL
STE001*   -MOVE RUECC5-COR0005-8BB TO WRK-B09A-SIS(WRK-POSICAO:
STE001*                                -LENGTH OF RUECC5-COR0005-8BB)
STE001*   -ADD LENGTH OF RUECC5-COR0005-8BB TO WRK-TAM
STE001*                                      -WRK-POSICAO.
.     *
.     *----------------------------------------------------------------*
.     *3183-99-FIM.                    EXIT.
STE001*----------------------------------------------------------------*
      *
BR4.11*---------------------------------------------------------------*
BR4.11*3190-FORMATA-GRUPO-GLEBA  SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -MOVE RUEC65-CCONTR          TO CCONTR      OF RUECB040.
BR4.11*
BR4.11*   -PERFORM 3191-OPEN-CSR05-RUECB040-JN.
BR4.11*
BR4.11*   -PERFORM 3192-FETCH-CSR05-RUECB040-JN.
BR4.11*
BR4.11*   -IF  WRK-FIM-B40             EQUAL  'S'
BR4.11*       -MOVE ALL '*'            TO RUECC5-GP-COR0005-GLEBA
BR4.11*       -MOVE ALL '*'            TO RUECC5-GP-COR0005-PTPOLG
BR4.11*       -MOVE ALL '*'            TO RUECC5-AREA-NAO-CULT
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -PERFORM UNTIL WRK-FIM-B40   EQUAL  'S'
BR4.11*           -PERFORM 3194-FORMATAR-CSR01-RUECB040
BR4.11*           -PERFORM 3192-FETCH-CSR05-RUECB040-JN
BR4.11*           -ADD 1               TO WRK-IND2
BR4.11*   -END-PERFORM.
BR4.11*
BR4.11*   -IF  WRK-FIM-B40             EQUAL  'S'
BR4.11*       -ADD 999                 TO WRK-IND2
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -PERFORM 3193-CLOSE-CSR05-RUECB040-JN.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3190-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
      *
BR4.11*---------------------------------------------------------------*
BR4.11*3191-OPEN-CSR05-RUECB040-JN     SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -OPEN   CSR05-RUECB040-JN
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
BR4.11*       -MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3191'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -MOVE 'N'                    TO WRK-FIM-B40.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3191-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3192-FETCH-CSR05-RUECB040-JN    SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -FETCH  CSR05-RUECB040-JN
BR4.11*         -INTO :RUECB040.CCONTR
BR4.11*             -,:RUECB040.CSEQ-MATR-RURAL
BR4.11*             -,:RUECB09N.NSEQ-GLEBA-IMOV
BR4.11*             -,:RUECB040.CMUN-RURAL
BR4.11*             -,:RUECB09N.NCORND-GEOGR-IMOV
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'MATR_CONTR_RURAL' TO ERR-DBD-TAB
BR4.11*       -MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3192'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -IF  SQLCODE                 EQUAL +100
BR4.11*       -MOVE 'S'                TO WRK-FIM-B40
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3192-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3193-CLOSE-CSR05-RUECB040-JN    SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -CLOSE  CSR05-RUECB040-JN
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'MATR_CDULA_RURAL' TO ERR-DBD-TAB
BR4.11*       -MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3193'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3193-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
      *
BR4.11*---------------------------------------------------------------*
BR4.11*3194-FORMATAR-CSR01-RUECB040    SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -MOVE CCONTR                 OF RUECB040
BR4.11*                               -TO CCONTR          OF RUECB09N.
BR4.11*   -MOVE CSEQ-MATR-RURAL        OF RUECB040
BR4.11*                               -TO CSEQ-MATR-RURAL OF RUECB09N.
BR4.11*   -MOVE ZEROS                  TO WRK-SEQ-GLEBA-X19
BR4.11*   -MOVE CSEQ-MATR-RURAL        OF RUECB09N
BR4.11*                               -TO WRK-SEQ-MATR-N03
BR4.11*   -MOVE NSEQ-GLEBA-IMOV        OF RUECB09N
BR4.11*                               -TO WRK-SEQ-GLEBA-N05
BR4.11*   -MOVE WRK-SEQ-GLEBA-X19      TO RUECC5-IDENTC-GLEBA
BR4.11*
BR4.11*   -MOVE RUECC5-COR0005-7       TO WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                                  -LENGTH OF RUECC5-COR0005-7)
BR4.11*   -ADD LENGTH OF RUECC5-COR0005-7  TO WRK-TAM
BR4.11*                                      -WRK-POSICAO
BR4.11*
BR4.11*   -PERFORM 3195-OPEN-CSR06-RUECB09N.
BR4.11*   -PERFORM 3196-FETCH-CSR06-RUECB09N.
BR4.11*
BR4.11*   -IF  WRK-FIM-B9N             EQUAL  'S'
BR4.11*       -MOVE  ALL '*'           TO RUECC5-LAT-PONTO
BR4.11*                                  -RUECC5-LONG-PONTO
BR4.11*                                  -RUECC5-ALTT-PONTO
BR4.11*       -PERFORM 3198-FORMATA-GRUPO-PTPOLG
BR4.11*       -MOVE ALL '*'            TO RUECC5-AREA-NAO-CULT
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -MOVE 1 TO WRK-IND3
BR4.11*   -PERFORM UNTIL WRK-FIM-B9N   EQUAL  'S'
MJ0724*           -MOVE ALL '*'        TO RUECC5-AREA-NAO-CULT
BR4.11*           -PERFORM 3199-FORMATAR-CSR06-RUECB09N
BR4.11*           -PERFORM 3196-FETCH-CSR06-RUECB09N
BR4.11*   -END-PERFORM.
BR4.11*
BR4.11*   -PERFORM 3182-FORMATA-GRUPO-AREA-N-CULT
BR4.11*
BR4.11*   -PERFORM 3197-CLOSE-CSR06-RUECB09N.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3194-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3195-OPEN-CSR06-RUECB09N        SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -OPEN   CSR06-RUECB09N
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TCORND_GEOGR_CONTR'
BR4.11*                               -TO ERR-DBD-TAB
BR4.11*       -MOVE 'OPEN    '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3195'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -MOVE 'N'                    TO WRK-FIM-B9N.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3195-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3196-FETCH-CSR06-RUECB09N       SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -FETCH  CSR06-RUECB09N
BR4.11*         -INTO :RUECB09N.NCORND-GEOGR-IMOV
BR4.11*            -, :RUECB09N.MLOGTD-IMOV-RURAL
BR4.11*            -, :RUECB09N.MLATTD-IMOV-RURAL
BR4.11*            -, :RUECB09N.MALTTD-IMOV-RURAL
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TCORND_GEOGR_IMOV'
BR4.11*                               -TO ERR-DBD-TAB
BR4.11*       -MOVE 'FETCH   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3196'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*   -IF  SQLCODE                 EQUAL +100
BR4.11*       -MOVE 'S'                TO WRK-FIM-B9N
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3196-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3197-CLOSE-CSR06-RUECB09N       SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -EXEC SQL
BR4.11*        -CLOSE  CSR06-RUECB09N
BR4.11*   -END-EXEC.
BR4.11*
BR4.11*   -IF (SQLCODE                 NOT EQUAL ZEROS) OR
BR4.11*      -(SQLWARN0                    EQUAL 'W'  )
BR4.11*       -MOVE 99                 TO RUEC65-COD-RET
BR4.11*       -MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR4.11*       -MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR4.11*       -MOVE 'TCORND_GEOGR_CONTR'
BR4.11*                               -TO ERR-DBD-TAB
BR4.11*       -MOVE 'CLOSE   '         TO ERR-FUN-COMANDO
BR4.11*       -MOVE SQLCODE            TO ERR-SQL-CODE
BR4.11*       -MOVE '3197'             TO ERR-LOCAL
BR4.11*       -MOVE SPACES             TO ERR-SEGM
BR4.11*       -PERFORM 9000-ROTINA-FIM
BR4.11*   -END-IF.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3197-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
      *
BR4.11*---------------------------------------------------------------*
BR4.11*3198-FORMATA-GRUPO-PTPOLG   SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -MOVE RUECC5-COR0005-8       TO WRK-B09A-SIS(WRK-POSICAO:
BR4.11*                                  -LENGTH OF RUECC5-COR0005-8).
BR4.11*   -ADD LENGTH OF RUECC5-COR0005-8 TO WRK-TAM
BR4.11*                                     -WRK-POSICAO.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3198-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
BR4.11*
BR4.11*---------------------------------------------------------------*
BR4.11*3199-FORMATAR-CSR06-RUECB09N    SECTION.
BR4.11*---------------------------------------------------------------*
BR4.11*   -MOVE MLOGTD-IMOV-RURAL      OF RUECB09N
BR4.11*                               -TO WRK-LOG-PONTO.
BR4.11*   -MOVE WRK-LOG-PONTO-GRUPO    TO RUECC5-LONG-PONTO
BR4.11*
BR4.11*   -MOVE MLATTD-IMOV-RURAL      OF RUECB09N
BR4.11*                               -TO WRK-HEXA-ZDS3V11
BR4.11*   -MOVE WRK-HEXA-ZDS2V11       TO WRK-LAT-PONTO
BR4.11*   -MOVE WRK-LAT-PONTO-GRUPO    TO RUECC5-LAT-PONTO
BR4.11*
BR4.11*   -MOVE MALTTD-IMOV-RURAL      OF RUECB09N
BR4.11*                               -TO WRK-HEXA-ZDS5V2
BR4.11*   -MOVE WRK-HEXA-ZDS4V2        TO WRK-ALT-PONTO.
BR4.11*   -MOVE WRK-ALT-PONTO-GRUPO    TO RUECC5-ALTT-PONTO.
MJ0724*   -MOVE ALL '*'                TO RUECC5-AREA-NAO-CULT
BR4.11*   -PERFORM 3198-FORMATA-GRUPO-PTPOLG.
BR4.11*
BR4.11*----------------------------------------------------------------*
BR4.11*3199-99-FIM.                    EXIT.
BR4.11*----------------------------------------------------------------*
      *---------------------------------------------------------------*
       4000-INSERT-B09A                SECTION.
      *---------------------------------------------------------------*

           PERFORM 4005-SELMAX-B099.

           PERFORM 4010-SELMAX-B09A.
           MOVE NREG-MSGEM-RECOR       TO  WRK-MAX-NREG.

           EXEC SQL
             SET :WRK-TIMESTAMP-B09A   = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP-B09A     TO HINCL            OF RUECB09A.
           MOVE WRK-MAX-NREG           TO NREG-MSGEM-RECOR OF RUECB09A.
      *    4DD  1                      T0 NREG-MSGEM-RECOR 0F RUECB09A.
           MOVE 1                      TO CSIT-MSGEM       OF RUECB09A.
           MOVE WRK-TAM                TO WRK-TAMANHO-SISMSG.
           MOVE WRK-TAMANHO-SISMSG     TO WCONTD-MSGEM-RURAL-LEN
                                       OF RUECB09A.
           MOVE WRK-B09A-SIS           TO WCONTD-MSGEM-RURAL-TEXT
                                       OF RUECB09A.

           EXEC SQL
             INSERT INTO DB2PRD.TENVIO_RECOR_TEMPR
             ( CCONTR_CREDT_RURAL
             , NENVIO_RECOR
             , NREG_MSGEM_RECOR
             , CSIT_MSGEM
             , HINCL
             , WCONTD_MSGEM_RURAL)
             VALUES
             ( :RUECB09A.CCONTR-CREDT-RURAL
             , :RUECB09A.NENVIO-RECOR
             , :RUECB09A.NREG-MSGEM-RECOR
             , :RUECB09A.CSIT-MSGEM
             , :RUECB09A.HINCL
             , :RUECB09A.WCONTD-MSGEM-RURAL)
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4000'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR0118*----------------------------------------------------------------*
-      4005-SELMAX-B099                SECTION.
-     *----------------------------------------------------------------*
-
-          MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
-                                      OF RUECB099.
-
-          MOVE ZEROS                  TO NENVIO-RECOR
-                                      OF RUECB099.
-
-          EXEC SQL
-            SELECT VALUE ( MAX(NENVIO_RECOR) , 0 )
-            INTO   :RUECB099.NENVIO-RECOR
-            FROM   DB2PRD.TCONTR_ENVIO_RECOR
-            WHERE  CCONTR_CREDT_RURAL = :RUECB099.CCONTR-CREDT-RURAL
-            AND    NENVIO_RECOR       > :RUECB099.NENVIO-RECOR
-          END-EXEC.
-
-          IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
-          OR  (SQLWARN0               EQUAL 'W')
-              MOVE 99                 TO RUEC65-COD-RET
-              MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
-              MOVE 'DB2'              TO ERR-TIPO-ACESSO
-              MOVE 'TCONTR_ENVIO_RECOR'
-                                      TO ERR-DBD-TAB
-              MOVE 'SELECT'           TO ERR-FUN-COMANDO
-              MOVE SQLCODE            TO ERR-SQL-CODE
-              MOVE '4005'             TO ERR-LOCAL
-              MOVE SPACES             TO ERR-SEGM
-              PERFORM 9000-ROTINA-FIM
-          END-IF.
-
-          IF SQLCODE                  EQUAL +100  OR
-             NENVIO-RECOR OF RUECB099 EQUAL ZEROS
-             PERFORM 4020-INSERT-B099
-          END-IF.
-
-     *----------------------------------------------------------------*
-      4005-99-FIM.                    EXIT.
BR0118*----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4010-SELMAX-B09A                SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB09A.
           MOVE NENVIO-RECOR           OF RUECB099
                                       TO NENVIO-RECOR OF RUECB09A.

           EXEC SQL
             SELECT VALUE ( MAX(NREG_MSGEM_RECOR) , 0 ) +1
             INTO   :RUECB09A.NREG-MSGEM-RECOR
             FROM   DB2PRD.TENVIO_RECOR_TEMPR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
             AND    NREG_MSGEM_RECOR   > 0
           END-EXEC.

           IF   SQLCODE                EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR0118*---------------------------------------------------------------*
BR0118 4020-INSERT-B099                SECTION.
BR0118*---------------------------------------------------------------*
BR0118
BR0118     INITIALIZE                  RUECB099
BR0118
BR0118     MOVE -1                     TO WRK-NULL-DCDULA
BR0118
BR0118     MOVE -1                     TO WRK-NULL-DVCTO-CONTR
BR0118
BR0118     MOVE -1                     TO WRK-NULL-VFINAN
BR0118
BR0118     MOVE -1                     TO WRK-NULL-CCATEG-PROTR-RECOR
BR0118
BR0118     MOVE -1                     TO WRK-NULL-CPROG-RURAL
BR0118
BR0118     MOVE -1                     TO WRK-NULL-CMUN-REFT-BACEN
BR0118
BR0118     MOVE -1                     TO WRK-NULL-VPCELA-LIBRC
BR0118
BR0118     MOVE -1                     TO WRK-NULL-VPCELA-REC-PPRIO
BR0118
BR0118     MOVE -1                     TO WRK-NULL-QUND-FINCD-RURAL
BR0118
BR0118     MOVE -1                     TO WRK-NULL-QUND-PROD-PROVV
BR0118
BR0118     MOVE -1                     TO WRK-NULL-DINIC-SAFRA
BR0118
BR0118     MOVE -1                     TO WRK-NULL-DFIM-SAFRA
BR0118
BR0118     MOVE -1                     TO WRK-NULL-CTPO-GARNT-RECOR
BR0118
BR0118     MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
BR0118                                                      OF RUECB099
BR0118     MOVE 1                      TO NENVIO-RECOR      OF RUECB099
BR0118
BR0118     MOVE 60746948               TO CCNPJ-ENTID-RESP  OF RUECB099
BR0118     MOVE 0001                   TO CFLIAL-CNPJ-ENTID OF RUECB099
BR0118     MOVE 63                     TO CCTRL-CNPJ-ENTID  OF RUECB099
BR0118
BR0118     EXEC SQL
BR0118          INSERT INTO DB2PRD.TCONTR_ENVIO_RECOR
BR0118                 (CCONTR_CREDT_RURAL,
BR0118                  NENVIO_RECOR,
BR0118                  CCNPJ_ENTID_RESP,
BR0118                  CFLIAL_CNPJ_ENTID,
BR0118                  CCTRL_CNPJ_ENTID,
BR0118                  CCNPJ_PARTC,
BR0118                  CFLIAL_CNPJ_PARTC,
BR0118                  CCTRL_CNPJ_PARTC,
BR0118                  CSIT_OPER_RURAL,
BR0118                  DCDULA,
BR0118                  DVCTO_CONTR,
BR0118                  CCDULA_RECOR,
BR0118                  CTPO_INSTT_CREDT,
BR0118                  VFINAN,
BR0118                  CCATEG_PROTR_RECOR,
BR0118                  CPROG_RURAL,
BR0118                  CTPO_FONTE_RECOR,
BR0118                  CMUN_REFT_BACEN,
BR0118                  CEMPTO_BACEN,
BR0118                  CSIST_PROD,
BR0118                  VPCELA_LIBRC,
BR0118                  VPCELA_REC_PPRIO,
BR0118                  PALIQT_PROAGRO,
BR0118                  PJURO_ENCAR_FINCR,
BR0118                  CTPO_ENCAR_COMPL,
BR0118                  CTSORO_NACIO,
BR0118                  PRISCO_TSORO_NACIO,
BR0118                  PRISCO_FUNDO_CNSTT,
BR0118                  QAREA_RECOR,
BR0118                  QUND_FINCD_RURAL,
BR0118                  CUND_MEDD_FINCD,
BR0118                  QUND_PROD_PROVV,
BR0118                  CUND_MEDD_PROVV,
BR0118                  DINIC_SAFRA,
BR0118                  DFIM_SAFRA,
BR0118                  CTPO_GARNT_RECOR,
BR0118                  VRECTA_BRUTA_RECOR,
BR0118                  DMOVTO_RECOR)
BR0118          VALUES (:RUECB099.CCONTR-CREDT-RURAL,
BR0118                  :RUECB099.NENVIO-RECOR,
BR0118                  :RUECB099.CCNPJ-ENTID-RESP,
BR0118                  :RUECB099.CFLIAL-CNPJ-ENTID,
BR0118                  :RUECB099.CCTRL-CNPJ-ENTID,
BR0118                  :RUECB099.CCNPJ-PARTC,
BR0118                  :RUECB099.CFLIAL-CNPJ-PARTC,
BR0118                  :RUECB099.CCTRL-CNPJ-PARTC,
BR0118                  :RUECB099.CSIT-OPER-RURAL,
BR0118                  :RUECB099.DCDULA
BR0118                    :WRK-NULL-DCDULA,
BR0118                  :RUECB099.DVCTO-CONTR
BR0118                    :WRK-NULL-DVCTO-CONTR,
BR0118                  :RUECB099.CCDULA-RECOR,
BR0118                  :RUECB099.CTPO-INSTT-CREDT,
BR0118                  :RUECB099.VFINAN
BR0118                    :WRK-NULL-VFINAN,
BR0118                  :RUECB099.CCATEG-PROTR-RECOR
BR0118                    :WRK-NULL-CCATEG-PROTR-RECOR,
BR0118                  :RUECB099.CPROG-RURAL
BR0118                    :WRK-NULL-CPROG-RURAL,
BR0118                  :RUECB099.CTPO-FONTE-RECOR,
BR0118                  :RUECB099.CMUN-REFT-BACEN
BR0118                    :WRK-NULL-CMUN-REFT-BACEN,
BR0118                  :RUECB099.CEMPTO-BACEN,
BR0118                  :RUECB099.CSIST-PROD,
BR0118                  :RUECB099.VPCELA-LIBRC
BR0118                    :WRK-NULL-VPCELA-LIBRC,
BR0118                  :RUECB099.VPCELA-REC-PPRIO
BR0118                    :WRK-NULL-VPCELA-REC-PPRIO,
BR0118                  :RUECB099.PALIQT-PROAGRO,
BR0118                  :RUECB099.PJURO-ENCAR-FINCR,
BR0118                  :RUECB099.CTPO-ENCAR-COMPL,
BR0118                  :RUECB099.CTSORO-NACIO,
BR0118                  :RUECB099.PRISCO-TSORO-NACIO,
BR0118                  :RUECB099.PRISCO-FUNDO-CNSTT,
BR0118                  :RUECB099.QAREA-RECOR,
BR0118                  :RUECB099.QUND-FINCD-RURAL
BR0118                    :WRK-NULL-QUND-FINCD-RURAL,
BR0118                  :RUECB099.CUND-MEDD-FINCD,
BR0118                  :RUECB099.QUND-PROD-PROVV
BR0118                    :WRK-NULL-QUND-PROD-PROVV,
BR0118                  :RUECB099.CUND-MEDD-PROVV,
BR0118                  :RUECB099.DINIC-SAFRA
BR0118                    :WRK-NULL-DINIC-SAFRA,
BR0118                  :RUECB099.DFIM-SAFRA
BR0118                    :WRK-NULL-DFIM-SAFRA,
BR0118                  :RUECB099.CTPO-GARNT-RECOR
BR0118                    :WRK-NULL-CTPO-GARNT-RECOR,
BR0118                  :RUECB099.VRECTA-BRUTA-RECOR,
BR0118                  CURRENT DATE)
BR0118     END-EXEC.
BR0118
BR0118     IF (SQLCODE                  NOT EQUAL ZEROS)
BR0118     OR (SQLWARN0                 EQUAL 'W')
BR0118        MOVE 99                   TO RUEC65-COD-RET
BR0118        MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR0118        MOVE 'TCONTR_ENVIO_RECO' TO ERR-DBD-TAB
BR0118        MOVE 'INSERT'             TO ERR-FUN-COMANDO
BR0118        MOVE SQLCODE              TO ERR-SQL-CODE
BR0118        MOVE '4020'               TO ERR-LOCAL
BR0118        MOVE SPACES               TO ERR-SEGM
BR0118        PERFORM 9000-ROTINA-FIM
BR0118     END-IF.
BR0118
BR0118*---------------------------------------------------------------*
BR0118 4020-99-FIM.                    EXIT.
BR0118*---------------------------------------------------------------*
      *---------------------------------------------------------------*
       4100-ENVIO                      SECTION.
      *---------------------------------------------------------------*

           PERFORM 4140-COUNT-RUECB09A.

           PERFORM 4110-OPEN-CSR03-RUECB09A.

           PERFORM 4120-FETCH-CSR03-RUECB09A.

           MOVE 'S'                    TO WRK-PRIM-ENVIO.

           PERFORM VARYING WRK-IND-B09A   FROM 1 BY 1
             UNTIL WRK-CODE100-B09A       EQUAL 'S'
                   PERFORM 4105-ENVIA-RCOR5005
                   PERFORM 4120-FETCH-CSR03-RUECB09A
           END-PERFORM.

           MOVE 'N'                    TO WRK-CODE100-B09A.
           MOVE 'S'                    TO WRK-PRIM-B09B.
           MOVE ZEROS                  TO WRK-QTDREG-B09A.
           MOVE SPACES                 TO WRK-PRIM-NUMCTRLIF.
           MOVE ZEROS                  TO WRK-IND-B09A.

           PERFORM 4130-CLOSE-CSR03-RUECB09A.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4105-ENVIA-RCOR5005             SECTION.
      *---------------------------------------------------------------*

           IF  SQLCODE                 EQUAL +100
               GO TO 4105-99-FIM
           END-IF.

           INITIALIZE  RCOR10-REGISTRO.

           IF  WRK-IND-B09A            EQUAL  WRK-QTDREG-B09A
               MOVE 'N'                TO  RCOR10-IND-CONTINUA
           ELSE
               MOVE 'S'                TO  RCOR10-IND-CONTINUA
           END-IF.

           IF  WRK-PRIM-ENVIO          EQUAL 'S'
               MOVE SPACES             TO RCOR10-NRO-CTRLIF
           ELSE
               MOVE WRK-PRIM-NUMCTRLIF TO RCOR10-NRO-CTRLIF
           END-IF

           MOVE 'RUEC'                 TO RCOR10-CCUSTO.

           MOVE ZEROS                  TO RCOR10-TAM-SISMSG.
           MOVE SPACES                 TO RCOR10-SISMSG.
           MOVE ZEROS                  TO WRK-ENVIO-LEN.
           MOVE SPACES                 TO WRK-ENVIO-SIS.

           MOVE WCONTD-MSGEM-RURAL-LEN TO RCOR10-TAM-SISMSG.
           MOVE WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF RCOR10-SISMSG)
                                       TO RCOR10-SISMSG.

jeff       INSPECT RCOR10-SISMSG REPLACING ALL '{2400}' BY '{1400}'

           MOVE WCONTD-MSGEM-RURAL-LEN TO WRK-ENVIO-LEN.
           MOVE WCONTD-MSGEM-RURAL-TEXT(1: LENGTH OF WRK-ENVIO-SIS)
                                       TO WRK-ENVIO-SIS.

           MOVE WRK-IND-B09A           TO WRK-SEQ-CSINAL.
           MOVE WRK-SEQ-SSINAL         TO WRK-SEQ-NUM5.
           MOVE WRK-SEQ-NUM3           TO RCOR10-SEQ-MSG.

           CALL WRK-MODULO-RCOR5005    USING RCOR10-REGISTRO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  RCOR10-COD-RETORNO      EQUAL ZEROS
               MOVE RCOR10-NRO-CTRL-IF TO WRK-ENVIO-NUMCTRLIF
               IF  WRK-PRIM-ENVIO      EQUAL 'S'
                   MOVE 'N'            TO WRK-PRIM-ENVIO
                   MOVE RCOR10-NRO-CTRL-IF
                                       TO WRK-PRIM-NUMCTRLIF
               END-IF
               PERFORM 4200-UPDATE-B09A
               PERFORM 4300-UPDATE-B09B
           ELSE
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO RCOR5005'    TO RUEC65-MENSAGEM
               MOVE ERRO-AREA          TO RUEC65-ERRO-AREA
               MOVE WRK-SQLCA(1:LENGTH OF RUEC65-SQLCA)
                                       TO RUEC65-SQLCA
              PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4110-OPEN-CSR03-RUECB09A        SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB09A.
           MOVE NENVIO-RECOR           OF RUECB099
                                       TO NENVIO-RECOR OF RUECB09A.
           MOVE WRK-TIMESTAMP-B09A     TO HINCL        OF RUECB09A.

           EXEC SQL
              OPEN CSR03-RUECB09A
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4110'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           MOVE 'S'                    TO WRK-OPEN-RUECB09A.

      *----------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4120-FETCH-CSR03-RUECB09A       SECTION.
      *---------------------------------------------------------------*

           MOVE SPACES                 TO WCONTD-MSGEM-RURAL-TEXT
                                       OF RUECB09A.

           EXEC SQL
             FETCH CSR03-RUECB09A
             INTO  :RUECB09A.CCONTR-CREDT-RURAL
             ,     :RUECB09A.NENVIO-RECOR
             ,     :RUECB09A.NREG-MSGEM-RECOR
             ,     :RUECB09A.CSIT-MSGEM         :WRK-B09A-CSIT-NULL
             ,     :RUECB09A.HINCL              :WRK-B09A-HINCL-NULL
             ,     :RUECB09A.WCONTD-MSGEM-RURAL
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'FETCH'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4120'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-CODE100-B09A
               GO TO 4120-99-FIM
           END-IF.

           IF  WRK-B09A-CSIT-NULL      LESS ZEROS
               MOVE ZEROS              TO CSIT-MSGEM        OF RUECB09A
           END-IF.

           IF  WRK-B09A-HINCL-NULL     LESS ZEROS
               MOVE SPACES             TO HINCL             OF RUECB09A
           END-IF.

      *----------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4130-CLOSE-CSR03-RUECB09A       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR03-RUECB09A
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'CLOSE'            TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4130'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           MOVE 'N'                    TO WRK-OPEN-RUECB09A.

      *----------------------------------------------------------------*
       4130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4140-COUNT-RUECB09A             SECTION.
      *---------------------------------------------------------------*

           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB09A.
           MOVE NENVIO-RECOR           OF RUECB099
                                       TO NENVIO-RECOR OF RUECB09A.
           MOVE WRK-TIMESTAMP-B09A     TO HINCL        OF RUECB09A.

           EXEC SQL
             SELECT COUNT(*)
             INTO   :WRK-QTDREG-B09A
             FROM   DB2PRD.TENVIO_RECOR_TEMPR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
             AND    HINCL              = :RUECB09A.HINCL
           END-EXEC.

           IF  SQLCODE                 EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4140'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4140'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-UPDATE-B09A                SECTION.
      *---------------------------------------------------------------*

           MOVE WRK-ENVIO-LEN          TO WCONTD-MSGEM-RURAL-LEN
                                       OF RUECB09A.
           MOVE WRK-ENVIO-SIS          TO WCONTD-MSGEM-RURAL-TEXT
                                       OF RUECB09A.
           MOVE 2                      TO CSIT-MSGEM OF RUECB09A.

           EXEC SQL
             UPDATE DB2PRD.TENVIO_RECOR_TEMPR
             SET    CSIT_MSGEM         = :RUECB09A.CSIT-MSGEM
             ,      WCONTD_MSGEM_RURAL = :RUECB09A.WCONTD-MSGEM-RURAL
             WHERE  CCONTR_CREDT_RURAL = :RUECB09A.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB09A.NENVIO-RECOR
             AND    NREG_MSGEM_RECOR   = :RUECB09A.NREG-MSGEM-RECOR
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'TENVIO_RECOR_TEMPR'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4300-UPDATE-B09B                SECTION.
      *---------------------------------------------------------------*

           IF  WRK-PRIM-B09B           EQUAL 'S'
               MOVE 'N'                TO WRK-PRIM-B09B
           ELSE
               GO TO 4300-99-FIM
           END-IF.

           PERFORM 4310-SELMAX-B09B.

           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL OF RUECB09B
           MOVE RCOR10-NRO-CTRL-IF     TO CCTRL-RQUIS-FINCR  OF RUECB09B

           IF WRK-QTDREG-B097          GREATER 100
              MOVE 6                   TO CSIT-MSGEM OF RUECB09B
           ELSE
              MOVE 2                   TO CSIT-MSGEM OF RUECB09B
           END-IF.

           EXEC SQL
             SET :WRK-TIMESTAMP        = CURRENT_TIMESTAMP
           END-EXEC.

           MOVE WRK-TIMESTAMP          TO HINCL       OF RUECB09B.
           MOVE '999999999'            TO CUSUAR-RESP OF RUECB09B.

           EXEC SQL
             UPDATE DB2PRD.THIST_ENVIO_RECOR
             SET   CCTRL_RQUIS_FINCR  = :RUECB09B.CCTRL-RQUIS-FINCR
             ,     CSIT_MSGEM         = :RUECB09B.CSIT-MSGEM
             ,     HINCL              = :RUECB09B.HINCL
             ,     CUSUAR_RESP        = :RUECB09B.CUSUAR-RESP
             WHERE CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
             AND   NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
             AND   HULT_ATULZ         = :RUECB09B.HULT-ATULZ
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4300'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4310-SELMAX-B09B                SECTION.
      *----------------------------------------------------------------*

           MOVE RUEC65-CCONTR          TO CCONTR-CREDT-RURAL
                                       OF RUECB09B.

           EXEC SQL
             SELECT VALUE ( MAX(HULT_ATULZ)
                          , '0001-01-01-00.00.00.000000' )
             INTO   :RUECB09B.HULT-ATULZ
             FROM   DB2PRD.THIST_ENVIO_RECOR
             WHERE  CCONTR_CREDT_RURAL = :RUECB09B.CCONTR-CREDT-RURAL
             AND    NENVIO_RECOR       = :RUECB09B.NENVIO-RECOR
           END-EXEC.

           IF  SQLCODE                 EQUAL +100
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4310'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

jeff       IF (SQLCODE             NOT EQUAL ZEROS)
           OR  (SQLWARN0               EQUAL 'W')
               MOVE 99                 TO RUEC65-COD-RET
               MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'THIST_ENVIO_RECOR'
                                       TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4310'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9000-ROTINA-FIM
           END-IF.

      *----------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BR0219*----------------------------------------------------------------*
BR0219 4400-COORDENADAS                SECTION.
BR0219*----------------------------------------------------------------*
BR0219
BR0219     MOVE ZEROS                  TO WRK-COUNT-RUECB09N
BR0219     MOVE RUEC65-CCONTR          TO CCONTR
BR0219                                 OF RUECB09N.
BR0219
BR0219     EXEC SQL
BR0219       SELECT COUNT(*)
BR0219       INTO   :WRK-COUNT-RUECB09N
BR0219       FROM   DB2PRD.TCORND_GEOGR_CONTR
BR0219       WHERE  CCONTR             = :RUECB09N.CCONTR
BR0219     END-EXEC.
BR0219
BR0219     IF  SQLCODE                 EQUAL +100
BR0219         MOVE 99                 TO RUEC65-COD-RET
BR0219         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0219         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0219         MOVE 'TCORND_GEOGR_CONTR'
BR0219                                 TO ERR-DBD-TAB
BR0219         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BR0219         MOVE SQLCODE            TO ERR-SQL-CODE
BR0219         MOVE '4400'             TO ERR-LOCAL
BR0219         MOVE SPACES             TO ERR-SEGM
BR0219         PERFORM 9000-ROTINA-FIM
BR0219     END-IF.
BR0219
BR0219     IF  (SQLCODE                NOT EQUAL ZEROS )
BR0219     OR  (SQLWARN0               EQUAL 'W')
BR0219         MOVE 99                 TO RUEC65-COD-RET
BR0219         MOVE 'ERRO ACESSO DB2'  TO RUEC65-MENSAGEM
BR0219         MOVE 'DB2'              TO ERR-TIPO-ACESSO
BR0219         MOVE 'TCORND_GEOGR_CONTR'
BR0219                                 TO ERR-DBD-TAB
BR0219         MOVE 'SELECT'           TO ERR-FUN-COMANDO
BR0219         MOVE SQLCODE            TO ERR-SQL-CODE
BR0219         MOVE '4400'             TO ERR-LOCAL
BR0219         MOVE SPACES             TO ERR-SEGM
BR0219         PERFORM 9000-ROTINA-FIM
BR0219     END-IF.
BR0219
BR0219*----------------------------------------------------------------*
BR0219 4400-99-FIM.                    EXIT.
BR0219*----------------------------------------------------------------*
4.13  *---------------------------------------------------------------*
4.13   4500-SELECT-RURCB000        SECTION.
4.13  *---------------------------------------------------------------*
4.13
4.13       EXEC SQL
4.13         SELECT CPLANO_REFIN_RURAL
4.13              , CPROG_RURAL
4.13              INTO    :RURCB000.CPLANO-REFIN-RURAL
4.13                    , :RURCB000.CPROG-RURAL
4.13         FROM   DB2PRD.ORIGEM_RECURSO
4.13         WHERE CORIGE_REC         = :RURCB000.CORIGE-REC
4.13       END-EXEC.
4.13
4.13       IF  SQLCODE                  EQUAL +100
4.13           MOVE 99                  TO RUEC65-COD-RET
4.13           MOVE 'ERRO ACESSO DB2'   TO RUEC65-MENSAGEM
4.13           MOVE 'DB2'               TO ERR-TIPO-ACESSO
4.13           MOVE 'ORIGEM_RECURSO   ' TO ERR-DBD-TAB
4.13           MOVE 'SELECT'            TO ERR-FUN-COMANDO
4.13           MOVE SQLCODE             TO ERR-SQL-CODE
4.13           MOVE '4500'              TO ERR-LOCAL
4.13           MOVE SPACES              TO ERR-SEGM
4.13           PERFORM 9000-ROTINA-FIM
4.13       END-IF.
4.13
4.13       IF (SQLCODE                  NOT EQUAL ZEROS) OR
4.13          (SQLWARN0                 EQUAL 'W')
4.13           MOVE 99                  TO RUEC65-COD-RET
4.13           MOVE 'ERRO ACESSO DB2'   TO RUEC65-MENSAGEM
4.13           MOVE 'DB2'               TO ERR-TIPO-ACESSO
4.13           MOVE 'ORIGEM_RECURSO   ' TO ERR-DBD-TAB
4.13           MOVE 'SELECT'            TO ERR-FUN-COMANDO
4.13           MOVE SQLCODE             TO ERR-SQL-CODE
4.13           MOVE '4500'              TO ERR-LOCAL
4.13           MOVE SPACES              TO ERR-SEGM
4.13           PERFORM 9000-ROTINA-FIM
4.13       END-IF.
4.13
4.13       MOVE CPLANO-REFIN-RURAL      TO CPLANO-REFIN
4.13       PERFORM 4510-SELECT-RURCB028.
4.13
4.13  *---------------------------------------------------------------*
4.13   4500-99-FIM.                EXIT.
4.13  *---------------------------------------------------------------*
4.13
4.13  *---------------------------------------------------------------*
4.13   4510-SELECT-RURCB028        SECTION.
4.13  *---------------------------------------------------------------*
4.13
4.13       EXEC SQL
4.13         SELECT CPLANO_REFIN_RECOR
4.13              INTO    :RURCB028.CPLANO-REFIN-RECOR
4.13         FROM   DB2PRD.PLANO_REFIN
4.13         WHERE CPLANO_REFIN       = :RURCB028.CPLANO-REFIN
4.13       END-EXEC.
4.13
RO1404     IF (SQLCODE                  NOT EQUAL ZEROS AND +100) OR
4.13          (SQLWARN0                 EQUAL 'W')
4.13           MOVE 99                  TO RUEC65-COD-RET
4.13           MOVE 'ERRO ACESSO DB2'   TO RUEC65-MENSAGEM
4.13           MOVE 'DB2'               TO ERR-TIPO-ACESSO
4.13           MOVE 'PLANO_REFIN      ' TO ERR-DBD-TAB
4.13           MOVE 'SELECT'            TO ERR-FUN-COMANDO
4.13           MOVE SQLCODE             TO ERR-SQL-CODE
4.13           MOVE '4510'              TO ERR-LOCAL
4.13           MOVE SPACES              TO ERR-SEGM
4.13           PERFORM 9000-ROTINA-FIM
4.13       END-IF
4.13
RO1404     IF SQLCODE  EQUAL +100
RO1404        MOVE SPACES TO CPLANO-REFIN-RECOR OF RURCB028
RO1404     END-IF.

4.13  *---------------------------------------------------------------*
4.13   4510-99-FIM.                EXIT.
4.13  *---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4600-FORMATA-GRUPO-PROP         SECTION.
BR0621*---------------------------------------------------------------*
BR0621
MJ1124***  DEZ/2024 - NAO ENVIAR O GRP DE PROPRIETARIOS PARA A COR5
MJ1224     GO TO 4600-99-FIM.
BR0621     MOVE 'P'                   TO CTPO-RESP-PSSOA
BR0621                                OF RUECB09E
BR0621     PERFORM 4610-OPEN-CSR07-RUECB09E.
BR0621     MOVE ZEROS           TO   WRK-IND-TAB
BR0621
BR0621     PERFORM 4620-SELECT-RUECB09E.
BR0621
BR0621     IF  WRK-CODE100-B09E         EQUAL 'S'
BR0621         PERFORM 4640-CLOSE-CSR07-RUECB09E
BR0621         MOVE 'E'                 TO CTPO-RESP-PSSOA
BR0621                                  OF RUECB09E
BR0621         PERFORM 4610-OPEN-CSR07-RUECB09E
BR0621         PERFORM 4620-SELECT-RUECB09E
BR0621         PERFORM 4700-FORMATAR-PROP-TAB
BR0621         MOVE 1                   TO WRK-IND-B09E
BR0621         PERFORM 4800-FORMATAR-ITEMB09E
BR0621         MOVE  ZEROS              TO WRK-IND-TAB
BR0621***    M.MOVE ALL '*'             TO RUECC5-GP-COR0005-PROPT
BR0621***    M.MOVE RUECC5-COR0005-10   TO WRK-B09A-SIS (WRK-POSICAO:
BR0621***    M.                         LENGTH OF RUECC5-COR0005-10)
BR0621***    A.ADD  LENGTH              OF RUECC5-COR0005-10
BR0621***    A.                         TO WRK-TAM
BR0621***    A.                            WRK-POSICAO
BR0621     END-IF.
BR0621
BR0621     PERFORM VARYING  WRK-IND-B09E      FROM 1 BY 1
BR0621               UNTIL  WRK-CODE100-B09E  EQUAL 'S'
BR0621             PERFORM  4700-FORMATAR-PROP-TAB
BR0621             PERFORM  4620-SELECT-RUECB09E
BR0621     END-PERFORM.
BR0621
BR0621     PERFORM VARYING  WRK-IND-B09E      FROM    1 BY 1
BR0621               UNTIL  WRK-IND-B09E      GREATER WRK-IND-TAB
BR0621                  OR  WRK-IND-B09E      GREATER 100
BR0621             PERFORM 4800-FORMATAR-ITEMB09E
BR0621     END-PERFORM.
BR0621
BR0621     MOVE 'N'   TO WRK-CODE100-B09E.
BR0621     MOVE ZEROS TO WRK-IND-B09E.
BR0621     PERFORM 4640-CLOSE-CSR07-RUECB09E.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4600-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4610-OPEN-CSR07-RUECB09E        SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     MOVE 'P'                   TO CTPO-RESP-PSSOA
BR0621                                OF RUECB09E.
BR0621     MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
BR0621                                OF RUECB09E.
BR0621     PERFORM 4611-MAX-NENVIO.
BR0621
BR0621     EXEC SQL
BR0621        OPEN      CSR07-RUECB09E-JN
BR0621     END-EXEC.
BR0621
BR0621     IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
BR0621        ( SQLWARN0       EQUAL  'W'   )
BR0621         MOVE 99                   TO RUEC65-COD-RET
BR0621         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR0621         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR0621         MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
BR0621         MOVE 'OPEN    '           TO ERR-FUN-COMANDO
BR0621         MOVE SQLCODE              TO ERR-SQL-CODE
BR0621         MOVE '4610'               TO ERR-LOCAL
BR0621         MOVE SPACES               TO ERR-SEGM
BR0621         PERFORM   9000-ROTINA-FIM
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4610-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*
br0621
BR0621*---------------------------------------------------------------*
BR0621 4611-MAX-NENVIO                 SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     MOVE RUEC65-CCONTR         TO CCONTR-CREDT-RURAL
BR0621                                OF RUECB09E.
BR0621
BR0621     EXEC SQL
BR0621        SELECT VALUE ( MAX ( NENVIO_RECOR ) , 0 )
BR0621        INTO  :RUECB09E.NENVIO-RECOR
BR0621        FROM   DB2PRD.TPSSOA_ENVIO_RECOR
BR0621        WHERE  CCONTR            = :RUECB09E.CCONTR-CREDT-RURAL
BR0621     END-EXEC.
BR0621
BR0621     IF   SQLCODE   EQUAL    +100
BR0621         MOVE 99                   TO RUEC65-COD-RET
BR0621         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR0621         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR0621         MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
BR0621         MOVE 'SELECT  '           TO ERR-FUN-COMANDO
BR0621         MOVE SQLCODE              TO ERR-SQL-CODE
BR0621         MOVE '4611'               TO ERR-LOCAL
BR0621         MOVE SPACES               TO ERR-SEGM
BR0621         PERFORM   9000-ROTINA-FIM
BR0621     END-IF.

BR0621     IF ( SQLCODE   NOT  EQUAL  ZEROS )   OR
BR0621        ( SQLWARN0       EQUAL  'W'   )
BR0621         MOVE 99                   TO RUEC65-COD-RET
BR0621         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR0621         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR0621         MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
BR0621         MOVE 'SELECT  '           TO ERR-FUN-COMANDO
BR0621         MOVE SQLCODE              TO ERR-SQL-CODE
BR0621         MOVE '4611'               TO ERR-LOCAL
BR0621         MOVE SPACES               TO ERR-SEGM
BR0621         PERFORM   9000-ROTINA-FIM
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4611-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4620-SELECT-RUECB09E            SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     EXEC SQL
BR0621        FETCH CSR07-RUECB09E-JN INTO
BR0621           :RUECB09E.CCONTR-CREDT-RURAL
BR0621        ,  :RUECB09E.NENVIO-RECOR
BR0621        ,  :RUECB09E.CTPO-RESP-PSSOA
BR0621                :WRK-B09E-RESP-NULL
BR0621        ,  :RUECB09E.CTPO-PSSOA
BR0621                :WRK-B09E-PSSOA-NULL
BR0621        ,  :RUECB09E.CCNPJ-CPF
BR0621                :WRK-B09E-CCNPJ-NULL
BR0621        ,  :RUECB09E.CFLIAL-CNPJ
BR0621                :WRK-B09E-CFLIAL-NULL
BR0621        ,  :RUECB09E.CCTRL-CNPJ-CPF
BR0621                :WRK-B09E-CCTRL-NULL
BR0621        ,  :RUECB040.CNIRF-IMOV-RURAL
BR0621                :WRK-B040-CNIRF-IMOV-NULL
BR0621        ,  :RUECB040.CCCIR
BR0621                :WRK-B040-CCCIR-NULL
BR0621        ,  :RUECB040.CREG-AMBTL-RURAL
BR0621                :WRK-B040-CREG-AMBTL-NULL
BR0621        ,  :RUECB040.PPRESV-AMBTL-PPRIE
BR0621                :WRK-B040-PPRESV-NULL
BR0621        ,  :RUECB040.COUTGA-AGUA-IMOV
BR0621                :WRK-B040-COUTGA-NULL
BR0621     END-EXEC.
BR0621
BR0621     IF (SQLCODE   NOT EQUAL ZEROS AND +100) OR
BR0621        (SQLWARN0      EQUAL 'W')
BR0621         MOVE 99                   TO  RUEC65-COD-RET
BR0621         MOVE 'ERRO ACESSO DB2'    TO  RUEC65-MENSAGEM
BR0621         MOVE 'DB2'                TO  ERR-TIPO-ACESSO
BR0621         MOVE 'TPSSOA_ENVIO_RECOR' TO  ERR-DBD-TAB
BR0621         MOVE 'SELECT'             TO  ERR-FUN-COMANDO
BR0621         MOVE SQLCODE              TO  ERR-SQL-CODE
BR0621         MOVE '4620'               TO  ERR-LOCAL
BR0621         MOVE SPACES               TO  ERR-SEGM
BR0621         PERFORM 9000-ROTINA-FIM
BR0621     END-IF.
BR0621
BR0621     IF SQLCODE EQUAL +100
BR0621        MOVE 'S' TO WRK-CODE100-B09E
BR0621        GO TO 4620-99-FIM
BR0621     END-IF.
BR0621
BR0621     IF SQLCODE EQUAL ZEROS
BR0621        PERFORM 4630-VER-NULOS-B09E
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4620-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4630-VER-NULOS-B09E             SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     IF WRK-B09E-RESP-NULL       LESS ZEROS
BR0621        MOVE SPACES              TO CTPO-RESP-PSSOA    OF RUECB09E
BR0621     END-IF.
BR0621
BR0621     IF WRK-B09E-PSSOA-NULL      LESS ZEROS
BR0621        MOVE ZEROS               TO CTPO-PSSOA         OF RUECB09E
BR0621     END-IF.
BR0621
BR0621     IF WRK-B09E-CCNPJ-NULL      LESS ZEROS
BR0621        MOVE ZEROS               TO CCNPJ-CPF          OF RUECB09E
BR0621     END-IF.
BR0621
BR0621     IF WRK-B09E-CFLIAL-NULL     LESS ZEROS
BR0621        MOVE ZEROS               TO CFLIAL-CNPJ        OF RUECB09E
BR0621     END-IF.
BR0621
BR0621     IF WRK-B09E-CCTRL-NULL      LESS ZEROS
BR0621        MOVE ZEROS               TO CCTRL-CNPJ-CPF     OF RUECB09E
BR0621     END-IF.
BR0621
BR0621     IF WRK-B040-CNIRF-IMOV-NULL LESS ZEROS
BR0621        MOVE SPACES              TO CNIRF-IMOV-RURAL   OF RUECB040
BR0621     END-IF.
BR0621
BR0621     IF WRK-B040-CCCIR-NULL      LESS ZEROS
BR0621        MOVE SPACES              TO CCCIR              OF RUECB040
BR0621     END-IF.
BR0621
BR0621     IF WRK-B040-CREG-AMBTL-NULL LESS ZEROS
BR0621        MOVE SPACES              TO CREG-AMBTL-RURAL   OF RUECB040
BR0621     END-IF.
BR0621
BR0621     IF WRK-B040-PPRESV-NULL     LESS ZEROS
BR0621        MOVE ZEROS               TO PPRESV-AMBTL-PPRIE OF RUECB040
BR0621     END-IF.
BR0621
BR0621     IF WRK-B040-COUTGA-NULL     LESS ZEROS
BR0621        MOVE SPACES              TO COUTGA-AGUA-IMOV   OF RUECB040
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4630-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4640-CLOSE-CSR07-RUECB09E       SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     EXEC SQL
BR0621        CLOSE     CSR07-RUECB09E-JN
BR0621     END-EXEC.
BR0621
BR0621     IF ( SQLCODE   NOT  EQUAL    ZEROS )   OR
BR0621        ( SQLWARN0       EQUAL    'W'   )
BR0621         MOVE 99                   TO RUEC65-COD-RET
BR0621         MOVE 'ERRO ACESSO DB2'    TO RUEC65-MENSAGEM
BR0621         MOVE 'DB2'                TO ERR-TIPO-ACESSO
BR0621         MOVE 'TPSSOA_ENVIO_RECOR' TO ERR-DBD-TAB
BR0621         MOVE 'CLOSE   '           TO ERR-FUN-COMANDO
BR0621         MOVE SQLCODE              TO ERR-SQL-CODE
BR0621         MOVE '4610'               TO ERR-LOCAL
BR0621         MOVE SPACES               TO ERR-SEGM
BR0621         PERFORM 9000-ROTINA-FIM
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4640-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4700-FORMATAR-PROP-TAB          SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621     IF SQLCODE EQUAL +100
BR0621        GO TO 4700-99-FIM
BR0621     END-IF.
BR0621
BR0621     IF  WRK-IND-TAB        EQUAL ZEROS                  OR
BR0621         CCNPJ-CPF      NOT EQUAL WRK-CCNPJ-CPF-ANT      OR
BR0621         CFLIAL-CNPJ    NOT EQUAL WRK-CFLIAL-CNPJ-ANT    OR
BR0621         CCTRL-CNPJ-CPF NOT EQUAL WRK-CCTRL-CNPJ-CPF-ANT
BR0621         ADD  1                 TO WRK-IND-TAB
BR0621         MOVE WRK-IND-TAB       TO WRK-IND-INI
BR0621         MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621         MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621         MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                TO WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621         MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                TO WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621         MOVE CNIRF-IMOV-RURAL  OF RUECB040
BR0621                                TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621         MOVE CCCIR             OF RUECB040
BR0621                                TO WRK-CCCIR(WRK-IND-TAB)
BR0621         MOVE CREG-AMBTL-RURAL  OF RUECB040
BR0621                                TO WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621         MOVE PPRESV-AMBTL-PPRIE
BR0621                                OF RUECB040
BR0621                                TO
BR0621                               WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621         MOVE COUTGA-AGUA-IMOV  OF RUECB040
BR0621                                TO
BR0621                               WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621     ELSE
BR0621        PERFORM  4710-TRATAR-PROP-TAB
BR0621     END-IF.
BR0621
BR0621     MOVE CCNPJ-CPF         OF RUECB09E
BR0621                            TO  WRK-CCNPJ-CPF-ANT.
BR0621     MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                            TO  WRK-CFLIAL-CNPJ-ANT.
BR0621     MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                            TO  WRK-CCTRL-CNPJ-CPF-ANT.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4700-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4710-TRATAR-PROP-TAB            SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621*    TRATAR CNIRF
BR0621     IF CNIRF-IMOV-RURAL NOT EQUAL SPACES
BR0621           MOVE 'N'       TO WRK-ATUALIZA
BR0621           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BR0621                   UNTIL    WRK-IND4     GREATER WRK-IND-TAB OR
BR0621                   WRK-ATUALIZA            EQUAL 'S'
BR0621             IF WRK-CNIRF-IMOV(WRK-IND4) EQUAL CNIRF-IMOV-RURAL
BR0621                 MOVE 'S'       TO WRK-ATUALIZA
BR0621             ELSE
BR0621              IF WRK-CNIRF-IMOV(WRK-IND4) EQUAL SPACES
BR0621                MOVE CNIRF-IMOV-RURAL  TO
BR0621                                        WRK-CNIRF-IMOV(WRK-IND4)
BR0621                MOVE 'S'       TO WRK-ATUALIZA
BR0621              END-IF
BR0621             END-IF
BR0621           END-PERFORM
BR0621
BR0621           IF WRK-ATUALIZA NOT EQUAL 'S'
BR0621             ADD  1                 TO WRK-IND-TAB
BR0621             MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                    TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621             MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                    TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621             MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CNIRF-IMOV-RURAL  OF RUECB040
BR0621                                    TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CCCIR(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621             MOVE ZEROS             TO
BR0621                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                             WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621           END-IF
BR0621     END-IF
BR0621
BR0621*    TRATAR CCCIR
BR0621     IF CCCIR  NOT EQUAL SPACES
BR0621           MOVE 'N'       TO WRK-ATUALIZA
BR0621           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BR0621                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BR0621                   WRK-ATUALIZA            EQUAL 'S'
BR0621             IF WRK-CCCIR(WRK-IND4) EQUAL CCCIR
BR0621               MOVE 'S'       TO WRK-ATUALIZA
BR0621             ELSE
BR0621              IF WRK-CCCIR(WRK-IND4) EQUAL SPACES
BR0621                MOVE CCCIR     TO WRK-CCCIR(WRK-IND4)
BR0621                MOVE 'S'       TO WRK-ATUALIZA
BR0621              END-IF
BR0621             END-IF
BR0621           END-PERFORM
BR0621
BR0621           IF WRK-ATUALIZA NOT EQUAL 'S'
BR0621             ADD  1                 TO WRK-IND-TAB
BR0621             MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                    TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621             MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                    TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621             MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621             MOVE CCCIR             OF RUECB040
BR0621                                    TO
BR0621                                   WRK-CCCIR(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621             MOVE ZEROS             TO
BR0621                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                             WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621           END-IF
BR0621     END-IF
BR0621
BR0621*    TRATAR CAR
BR0621     IF CREG-AMBTL-RURAL NOT EQUAL SPACES
BR0621           MOVE 'N'       TO WRK-ATUALIZA
BR0621           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BR0621                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BR0621                   WRK-ATUALIZA            EQUAL 'S'
BR0621             IF WRK-CREG-AMBTL(WRK-IND4) EQUAL CREG-AMBTL-RURAL
BR0621               MOVE 'S'       TO WRK-ATUALIZA
BR0621             ELSE
BR0621              IF WRK-CREG-AMBTL(WRK-IND4) EQUAL SPACES
BR0621                MOVE CREG-AMBTL-RURAL  TO
BR0621                                        WRK-CREG-AMBTL(WRK-IND4)
BR0621                MOVE 'S'       TO WRK-ATUALIZA
BR0621              END-IF
BR0621             END-IF
BR0621           END-PERFORM
BR0621
BR0621           IF WRK-ATUALIZA NOT EQUAL 'S'
BR0621             ADD  1                 TO WRK-IND-TAB
BR0621             MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                    TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621             MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                    TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621             MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CCCIR(WRK-IND-TAB)
BR0621             MOVE CREG-AMBTL-RURAL  OF RUECB040
BR0621                                    TO
BR0621                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621             MOVE ZEROS             TO
BR0621                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                             WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621           END-IF
BR0621     END-IF.
BR0621
BR0621*    TRATAR PPRESV-AMBTL-PPRIE
BR0621
BR0621     IF PPRESV-AMBTL-PPRIE GREATER ZEROS
BR0621           MOVE 'N'       TO WRK-ATUALIZA
BR0621           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BR0621                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BR0621                   WRK-ATUALIZA            EQUAL 'S'
BR0621             IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND4) EQUAL
BR0621                                               PPRESV-AMBTL-PPRIE
BR0621               MOVE 'S'       TO WRK-ATUALIZA
BR0621             ELSE
BR0621              IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND4) EQUAL ZEROS
BR0621                MOVE PPRESV-AMBTL-PPRIE  TO
BR0621                                WRK-PPRESV-AMBTL-PPRIE(WRK-IND4)
BR0621                MOVE 'S'       TO WRK-ATUALIZA
BR0621              END-IF
BR0621             END-IF
BR0621           END-PERFORM
BR0621
BR0621           IF WRK-ATUALIZA NOT EQUAL 'S'
BR0621             ADD  1                 TO WRK-IND-TAB
BR0621             MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                    TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621             MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                    TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621             MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CCCIR(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621             MOVE PPRESV-AMBTL-PPRIE
BR0621                                    OF RUECB040
BR0621                                    TO
BR0621                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                             WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621           END-IF
BR0621     END-IF.
BR0621
BR0621*    TRATAR COUTGA-AGUA-IMOV
BR0621
BR0621     IF COUTGA-AGUA-IMOV NOT EQUAL SPACES
BR0621           MOVE 'N'       TO WRK-ATUALIZA
BR0621           PERFORM VARYING  WRK-IND4       FROM  WRK-IND-INI BY 1
BR0621                   UNTIL    WRK-IND4    GREATER  WRK-IND-TAB OR
BR0621                   WRK-ATUALIZA            EQUAL 'S'
BR0621             IF WRK-COUTGA-AGUA-IMOV(WRK-IND4) EQUAL
BR0621                                               COUTGA-AGUA-IMOV
BR0621               MOVE 'S'       TO WRK-ATUALIZA
BR0621             ELSE
BR0621              IF WRK-COUTGA-AGUA-IMOV(WRK-IND4) EQUAL SPACES
BR0621                MOVE COUTGA-AGUA-IMOV  TO
BR0621                                WRK-COUTGA-AGUA-IMOV(WRK-IND4)
BR0621                MOVE 'S'       TO WRK-ATUALIZA
BR0621              END-IF
BR0621             END-IF
BR0621           END-PERFORM
BR0621
BR0621           IF WRK-ATUALIZA NOT EQUAL 'S'
BR0621             ADD  1                 TO WRK-IND-TAB
BR0621             MOVE CTPO-PSSOA        OF RUECB09E
BR0621                                    TO WRK-CTPO-PSSOA(WRK-IND-TAB)
BR0621             MOVE CCNPJ-CPF         OF RUECB09E
BR0621                                    TO WRK-CCNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE CFLIAL-CNPJ       OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CFLIAL-CNPJ(WRK-IND-TAB)
BR0621             MOVE CCTRL-CNPJ-CPF    OF RUECB09E
BR0621                                    TO
BR0621                                   WRK-CCTRL-CNPJ-CPF(WRK-IND-TAB)
BR0621             MOVE SPACES            TO WRK-CNIRF-IMOV(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CCCIR(WRK-IND-TAB)
BR0621             MOVE SPACES            TO
BR0621                                   WRK-CREG-AMBTL(WRK-IND-TAB)
BR0621             MOVE ZEROS             TO
BR0621                             WRK-PPRESV-AMBTL-PPRIE(WRK-IND-TAB)
BR0621             MOVE COUTGA-AGUA-IMOV  OF RUECB040
BR0621                                    TO
BR0621                             WRK-COUTGA-AGUA-IMOV(WRK-IND-TAB)
BR0621           END-IF
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4710-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0621*---------------------------------------------------------------*
BR0621 4800-FORMATAR-ITEMB09E          SECTION.
BR0621*---------------------------------------------------------------*
BR0621
BR0621* GRUPO PROP TAB B09E
BR0621     IF WRK-IND-B09E  LESS 100
BR0621        IF  WRK-CTPO-PSSOA(WRK-IND-B09E)  EQUAL  1
BR0621            MOVE  'F'             TO  RUECC5-TPSSOA-PROPT
BR0621            MOVE WRK-CCNPJ-CPF(WRK-IND-B09E)
BR0621                                  TO  WRK-COD9-CPF
BR0621            MOVE WRK-CCTRL-CNPJ-CPF(WRK-IND-B09E)
BR0621                                  TO  WRK-COD2-NUM-S
BR0621            MOVE WRK-COD9-CPF-9   TO  WRK-EDIT-NCPF
BR0621            MOVE WRK-COD2-NUM     TO  WRK-EDIT-CCPF
BR0621            MOVE WRK-EDIT-CPF     TO  RUECC5-CNPJ-CPF-PROPT
BR0621        ELSE
BR0621            MOVE  'J'             TO  RUECC5-TPSSOA-PROPT
BR0621            MOVE WRK-CCNPJ-CPF(WRK-IND-B09E)
BR0621                                  TO  WRK-COD9-NUM-S
BR0621            MOVE WRK-COD9-NUM8-XS TO  WRK-EDIT2-NCNPJ
BR0621            MOVE WRK-EDIT2-CNPJ   TO  RUECC5-CNPJ-CPF-PROPT
BR0621        END-IF
BR0621
BR0621        IF WRK-CNIRF-IMOV(WRK-IND-B09E)         EQUAL SPACES
050825           MOVE ALL '*'      TO RUECC5-CIB
BR0621        ELSE
BR0621           MOVE WRK-CNIRF-IMOV(WRK-IND-B09E)(2:8)
050825                             TO RUECC5-CIB
BR0621        END-IF
BR0621
BR0621        IF WRK-CCCIR(WRK-IND-B09E)              EQUAL SPACES
BR0621           MOVE ALL '*'      TO RUECC5-COD-SNCR
BR0621        ELSE
BR0621           MOVE WRK-CCCIR(WRK-IND-B09E)
BR0621                             TO RUECC5-COD-SNCR
BR0621        END-IF
BR0621
BR0621        IF WRK-CREG-AMBTL(WRK-IND-B09E)         EQUAL SPACES
BR0621           MOVE ALL '*'      TO RUECC5-NUM-REG-CAR
BR0621        ELSE
BR0621           MOVE WRK-CREG-AMBTL(WRK-IND-B09E)
BR0621                             TO RUECC5-NUM-REG-CAR
BR0621        END-IF
BR0621
BR0621        IF WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E)   EQUAL SPACES
BR0621           MOVE ALL '*'      TO RUECC5-COUTGA-AGUA
BR0621        ELSE
BR0621           MOVE WRK-COUTGA-AGUA-IMOV(WRK-IND-B09E)
BR0621                             TO RUECC5-COUTGA-AGUA
BR0621        END-IF
BR0621
BR0621        IF WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E) EQUAL ZEROS
BR0621           MOVE ALL '*'      TO RUECC5-PPRESV-AMBTL
BR0621        ELSE
BR0621           MOVE WRK-PPRESV-AMBTL-PPRIE(WRK-IND-B09E)
BR0621                             TO WRK-NUM5-S
BR0621           MOVE WRK-NUM5     TO RUECC5-PPRESV-AMBTL-N
BR0621        END-IF
BR0621
BR0621        MOVE RUECC5-COR0005-10  TO WRK-B09A-SIS(WRK-POSICAO:
BR0621                                LENGTH OF RUECC5-COR0005-10)
BR0621        ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-TAM
BR0621        ADD  LENGTH OF RUECC5-COR0005-10 TO WRK-POSICAO
BR0621     END-IF.
BR0621
BR0621*---------------------------------------------------------------*
BR0621 4800-99-FIM.                EXIT.
BR0621*---------------------------------------------------------------*

BR0921*---------------------------------------------------------------*
BR0921 4900-RECUPERA-PROP-COR0001      SECTION.
BR0921*---------------------------------------------------------------*
BR0921
211025**   NAO ACESSA CMRU PARA BUSCAR GRUPO PROPRIETARIO
211025     GO TO 4900-99-FIM.
BR0921     MOVE RUEC65-CCONTR          TO CMRUPP-E-CCONTR
BR0921     MOVE CREFT-RECOR            OF RUFIB002
BR0921                                 TO CMRUPP-E-CREFT-RECOR
BR0921
BR0921     CALL WRK-CMRU8835           USING CMRUPP-AREA.
BR0921
BR0921*---------------------------------------------------------------*
BR0921 4900-99-FIM.                    EXIT.
BR0921*---------------------------------------------------------------*
BR0921
7COMMP*---------------------------------------------------------------*
7COMMP 5000-FORMATA-SUBPROG            SECTION.
7COMMP*---------------------------------------------------------------*
7COMMP
7COMMP     INITIALIZE RUECW824-REGISTRO.
7COMMP
7COMMP     MOVE CORIGE-REC             OF  RUFIB002
7COMMP                                 TO  RUECW824-E-CORIGE-REC
7COMMP     MOVE CFNALD-RURAL           OF  RUFIB002
7COMMP                                 TO  RUECW824-E-CFNALD-RURAL
7COMMP     MOVE CPRODT-RURAL           OF  RUFIB002
7COMMP                                 TO  RUECW824-E-CPRODT-RURAL
7COMMP     MOVE SPACES                 TO  RUECW824-E-RESERVA
7COMMP
7COMMP     MOVE 'RUEC8824'             TO  WRK-MODULO.
7COMMP
7COMMP     CALL  WRK-MODULO        USING   RUECW824-COPYBOOK
7COMMP                                     ERRO-AREA
7COMMP                                     SQLCA.
7COMMP
7COMMP     IF  RUECW824-COD-RETORNO    NOT EQUAL ZEROS
7COMMP         MOVE 99                 TO RUEC65-COD-RET
7COMMP         MOVE 'ERRO RUEC8824'    TO RUEC65-MENSAGEM
7COMMP         MOVE ERRO-AREA          TO RUEC65-ERRO-AREA
7COMMP         MOVE WRK-SQLCA(1:LENGTH OF RUEC65-SQLCA)
7COMMP                                 TO RUEC65-SQLCA
7COMMP         PERFORM 9000-ROTINA-FIM
7COMMP     END-IF.
7COMMP
7COMMP     MOVE RUECW824-S-SUB-PROG-RURAL
7COMMP                                 TO RUECC5-SUB-PROGRAMA.
7COMMP
7COMMP*----------------------------------------------------------------*
7COMMP 5000-99-FIM.                    EXIT.
7COMMP*----------------------------------------------------------------*
7COMMP
7COMMP*----------------------------------------------------------------*
7COMMP 5100-FORMATA-CODPROG            SECTION.
7COMMP*----------------------------------------------------------------*
7COMMP
7COMMP     MOVE CCONTR                 OF RUFIB002
7COMMP                                 TO CCONTR-CREDT-RURAL
7COMMP                                 OF RUECB099.
7COMMP     MOVE NENVIO-RECOR           OF RUECB09B
7COMMP                                 TO NENVIO-RECOR
7COMMP                                 OF RUECB099.
7COMMP
7COMMP     EXEC SQL
7COMMP          SELECT VALUE(CPROG_RURAL, '')
7COMMP            INTO :RUECB099.CPROG-RURAL
7COMMP            FROM DB2PRD.TCONTR_ENVIO_RECOR
7COMMP           WHERE CCONTR_CREDT_RURAL = :RUECB099.CCONTR-CREDT-RURAL
7COMMP             AND NENVIO_RECOR       = :RUECB099.NENVIO-RECOR
7COMMP     END-EXEC.
7COMMP
7COMMP     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
7COMMP        (SQLWARN0                EQUAL 'W')
7COMMP         MOVE 'DB2'              TO ERR-TIPO-ACESSO
7COMMP         MOVE 'TCONTR_ENVIO_RECOR'
7COMMP                                 TO ERR-DBD-TAB
7COMMP         MOVE 'SELECT'           TO ERR-FUN-COMANDO
7COMMP         MOVE SQLCODE            TO ERR-SQL-CODE
7COMMP         MOVE '4630'             TO ERR-LOCAL
7COMMP         MOVE SPACES             TO ERR-SEGM
7COMMP         PERFORM 9000-ROTINA-FIM
7COMMP     END-IF.
7COMMP
7COMMP     IF (SQLCODE                 EQUAL +100)
7COMMP         MOVE '****'             TO RUECC5-COD-PROGRAMA
7COMMP         GO                      TO 5100-99-FIM
7COMMP     END-IF.
7COMMP
7COMMP     MOVE CPROG-RURAL            OF RUECB099
7COMMP                                 TO RUECC5-COD-PROGRAMA.
7COMMP
7COMMP*---------------------------------------------------------------*
7COMMP 5100-99-FIM.                    EXIT.
7COMMP*---------------------------------------------------------------*
7COMMP
7COMMP*---------------------------------------------------------------*
7COMMP 5200-FORMATA-SUBPROG-RUCA       SECTION.
7COMMP*---------------------------------------------------------------*
7COMMP
7COMMP     MOVE CORIGE-REC             OF RUFIB002
7COMMP                                 TO CORIGE-REC
7COMMP                                 OF RURCB094
7COMMP     MOVE CFNALD-RURAL           OF RUFIB002
7COMMP                                 TO CFNALD-RURAL
7COMMP                                 OF RURCB094
7COMMP     MOVE CPRODT-RURAL           OF RUFIB002
7COMMP                                 TO CPRODT-RURAL
7COMMP                                 OF RURCB094
7COMMP
7COMMP     EXEC SQL
7COMMP          SELECT CSUB_PROG_RURAL
7COMMP            INTO :RURCB094.CSUB-PROG-RURAL
7COMMP            FROM DB2PRD.TPARMZ_SUB_PROG
7COMMP           WHERE CORIGE_REC      = :RURCB000.CORIGE-REC
7COMMP             AND CFNALD_RURAL    = :RURCB094.CFNALD-RURAL
7COMMP             AND CPRODT_RURAL    = :RURCB094.CPRODT-RURAL
7COMMP     END-EXEC.
7COMMP
7COMMP     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
7COMMP        (SQLWARN0                EQUAL 'W')
7COMMP         MOVE 'DB2'              TO ERR-TIPO-ACESSO
7COMMP         MOVE 'TPARMZ_SUB_PROG'  TO ERR-DBD-TAB
7COMMP         MOVE 'SELECT'           TO ERR-FUN-COMANDO
7COMMP         MOVE SQLCODE            TO ERR-SQL-CODE
7COMMP         MOVE '4631'             TO ERR-LOCAL
7COMMP         MOVE SPACES             TO ERR-SEGM
7COMMP         PERFORM 9000-ROTINA-FIM
7COMMP     END-IF.
7COMMP
7COMMP     IF (SQLCODE                 EQUAL +100)
7COMMP         MOVE '****'             TO RUECC5-SUB-PROGRAMA
7COMMP         GO                      TO 5200-99-FIM
7COMMP     END-IF.
7COMMP
7COMMP     MOVE CSUB-PROG-RURAL        OF RURCB094
7COMMP                                 TO RUECC5-SUB-PROGRAMA.
7COMMP
7COMMP*----------------------------------------------------------------*
7COMMP 5200-99-FIM.                    EXIT.
7COMMP*----------------------------------------------------------------*
7COMMP
7COMMP*----------------------------------------------------------------*
7COMMP 5300-FORMATA-CODPROG-RUCA       SECTION.
7COMMP*----------------------------------------------------------------*
7COMMP
7COMMP     MOVE CORIGE-REC             OF RUFIB002
7COMMP                                 TO CORIGE-REC
7COMMP                                 OF RURCB000
7COMMP
7COMMP     EXEC SQL
7COMMP          SELECT CPROG_RURAL
7COMMP            INTO :RURCB000.CPROG-RURAL
7COMMP            FROM DB2PRD.ORIGEM_RECURSO
7COMMP           WHERE CORIGE_REC      = :RURCB000.CORIGE-REC
7COMMP     END-EXEC.
7COMMP
7COMMP     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
7COMMP        (SQLWARN0                EQUAL 'W')
7COMMP         MOVE 'DB2'              TO ERR-TIPO-ACESSO
7COMMP         MOVE 'ORIGEM_RECURSO'   TO ERR-DBD-TAB
7COMMP         MOVE 'SELECT'           TO ERR-FUN-COMANDO
7COMMP         MOVE SQLCODE            TO ERR-SQL-CODE
7COMMP         MOVE '4632'             TO ERR-LOCAL
7COMMP         MOVE SPACES             TO ERR-SEGM
7COMMP         PERFORM 9000-ROTINA-FIM
7COMMP     END-IF.
7COMMP
7COMMP     IF (SQLCODE                 EQUAL +100)
7COMMP         MOVE '****'             TO RUECC5-COD-PROGRAMA
7COMMP         GO                      TO 5300-99-FIM
7COMMP     END-IF.
7COMMP
7COMMP     MOVE CPROG-RURAL            OF RURCB000
7COMMP                                 TO RUECC5-COD-PROGRAMA.
7COMMP
7COMMP*---------------------------------------------------------------*
7COMMP 5300-99-FIM.                    EXIT.
7COMMP*---------------------------------------------------------------*
7COMMP
      *---------------------------------------------------------------*
       9000-ROTINA-FIM                 SECTION.
      *---------------------------------------------------------------*

           IF  RUEC65-COD-RET          EQUAL 99
               MOVE 'RUEC8831'         TO ERR-MODULO
               MOVE ERRO-AREA          TO RUEC65-ERRO-AREA
               MOVE SQLCA(1:LENGTH     OF RUEC65-SQLCA)
                                       TO RUEC65-SQLCA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *---------------------------------------------------------------**
