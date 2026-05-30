      *================================================================*
      * VERSAO 4.0 - IMPLANTACAO PREVISTA OUTUBRO/2013                 *
      *================================================================*
      *---------------------------------------------------------------*
      *              S O N D A      P R O C W O R K                   *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#RUEC66 - PARA MODULO RCOR5005(COR0001)*
      * GERACAO.............: AGOSTO/2012                             *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO PARA FORMATACAO/REFORMATACAO*
      *               PARA ENVIO DOS DADOS PARA RECOR                 *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      * TAMANHO TOTAL SEM RETICAO =  714                              *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
QT1120* ALTERACAO...........: INCLUSAO DOS CAMPOS                     *
QT1120*                             V3-NUM-REF-BCCO-DESCL             *
QT1120*                             V3-VLR-DESCLASCD                  *
QT1120* GERACAO.............: 13/11/2020                              *
      *---------------------------------------------------------------*
BRQ001*----------------------------------------------------------------*
BRQ001*               U L T I M A  A L T E R A C A O                   *
BRQ001*----------------------------------------------------------------*
BRQ001*                                                                *
BRQ001*      ANALISTA     : RODOLFO VIEIRA        - BRQ                *
BRQ001*      DATA         : MAIO/2021                                  *
BRQ001*                                                                *
BRQ001*      OBJETIVOS    : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
BRQ001*                     V5.02                                      *
BRQ001*                                                                *
BRQ001*      REFERENCIA   : BRQ001                                     *
BRQ001*                                                                *
BRQ001*================================================================*
WP1021*---------------------------------------------------------------*
WP1021* ALTERACAO...........: INCLUSAO DOS CAMPOS SICOR 5.03          *
.     *                       TIPO DE SOLO                            *
.     *                       CICLO DE CULTIVO                        *
.     *                       TIPO DE MANEJO                          *
WP1021* GERACAO.............: 10/2021                                 *
      *---------------------------------------------------------------*
WIP001* ALTERACAO...........: ALTERACAO AMPO SICOR 5.05               *
-     *                       COD-SIST-PRODC                          *
WIP001* GERACAO.............: 10/2022                                 *
      *---------------------------------------------------------------*
STE001* ALTERACAO...........: ALTERACAO      SICOR 5.08               *
-     *                       COD-SIST-PRODC PARA 13                  *
STE001* GERACAO.............: MAIO/2024                               *
      *---------------------------------------------------------------*
050825* ALTERACAO...........: SQUAD FLASH - HISTORIA AGRO7-1014       *
.     *                       ADEQUACOES DE LAYOUT PARA SICOR 5.10    *
.     *                       TRATAMENTO PARA CNPJ ALFANUMERICO       *
050825* GERACAO.............: AGOSTO/2025                             *
      *---------------------------------------------------------------*
211025* ALTERACAO...........: SQUAD FLASH - HISTORIA AGRO7-2978       *
.     *                       ADEQUACOES DE LAYOUT PARA SICOR 5.11    *
211025* GERACAO.............: OUTUBRO/2025                            *
      *---------------------------------------------------------------*
      *
      **** TAMANHO = 140
BS0116**** CAMPO RUEC66-TP-CATG-EMIT SIGNIFICA COD PORTE BENEFICIARIO
      *
       01  RUEC66-COR0001-1.
           05 RUEC66-CODMSG                PIC  X(009) VALUE SPACES.
           05 RUEC66-NUMCTRLIF             PIC  X(020) VALUE SPACES.
050825     05 RUEC66-CNPJ-ENT-RESPONS      PIC  X(014) VALUE SPACES.
           05 RUEC66-CNPJ-PART             PIC  X(014) VALUE SPACES.
           05 RUEC66-NUMREF-BCCOROR        PIC  X(011) VALUE SPACES.
BSI4.8*    05 RUEC66-TP-CEDLCOR            PIC  X(001) VALUE SPACES.
           05 RUEC66-DTEMS                 PIC  9(008) VALUE ZEROS.
           05 RUEC66-DTVENC                PIC  9(008) VALUE ZEROS.
BR4.11     05 RUEC66-CBASE-LEGAL-PRO       PIC  X(003) VALUE SPACES.
           05 RUEC66-NUMCEDL-CRED-RURALIF  PIC  X(017) VALUE SPACES.
           05 RUEC66-TP-INSTNTO-CRED       PIC  X(002) VALUE SPACES.
BSI4.8*    05 RUEC66-CNPJ-BASEIF-SUBEMPRT  PIC  X(008) VALUE SPACES.
BSI4.8*    05 RUEC66-NUMREF-BCCOR-SUBEMPRT PIC  X(011) VALUE SPACES.
           05 RUEC66-VLR-TOTOP             PIC  X(019) VALUE SPACES.
           05 RUEC66-TP-CATG-EMIT          PIC  X(004) VALUE SPACES.

      *
      **** TAMANHO = 49
BS0116**** FOI ALTERADO O NOME DE EMITENTE PARA BENEFICIARIO, MAS NAO
BS0116**** FOI ALTERADO OS NOMES DOS CAMPOS
      *
       01  RUEC66-COR0001-2.
           05 RUEC66-GRUPO-COR0001-EMIT.
              10 FILLER                 PIC  X(006) VALUE '{1000}'.
              10 RUEC66-GP-COR0001-EMIT.
                 15 RUEC66-TP-PESSOA-EMIT PIC  X(001) VALUE SPACES.
050825           15 RUEC66-CNPJ-CPF-EMIT  PIC  X(014) VALUE SPACES.
050825           15 RUEC66-CAF-BENF       PIC  X(025) VALUE SPACES.
BSI4.8           15 RUEC66-TPO-BENF       PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 6
      *
       01  RUEC66-COR0001-3.
           05 RUEC66-GRUPO-COR0001-DESTCFIN.
              10 FILLER                 PIC  X(006) VALUE '{2000}'.

      *
      **** TAMANHO = 36
      *
       01  RUEC66-COR0001-4.
           05 RUEC66-GRUPO-COR0001-RENEGC.
              10 FILLER                PIC  X(006) VALUE '{2100}'.
              10 RUEC66-GP-COR0001-RENEGC.
                 15 RUEC66-NUMREF-BCCOR-RENEGC
                                       PIC  X(011) VALUE SPACES.
                 15 RUEC66-VLR-RENEGC  PIC  X(019) VALUE SPACES.
EG0617***        CODBASELEGALRENEGC
EG0617           15 RUEC66-BASE-LEGAL-RENEGC
EG0617                                 PIC  X(003) VALUE SPACES.

      *
050825**** TAMANHO = 18
      *
       01  RUEC66-COR0001-5.
           05 RUEC66-CODPROG-LINHA-CRED  PIC  X(004) VALUE SPACES.
050825     05 RUEC66-CODSUB-PROG         PIC  X(004) VALUE SPACES.
           05 RUEC66-TP-FNTE-REC         PIC  X(004) VALUE SPACES.
           05 RUEC66-COD-MUNIC           PIC  9(006) VALUE ZEROS.

      *
211025**** TAMANHO = 36
  .   *
  .    01  RUEC66-COR0001-6.
  .        05 RUEC66-GRUPO-COR0001-GLEBA.
  .           10 FILLER                  PIC  X(006) VALUE '{2200}'.
  .           10 RUEC66-GP-COR0001-GLEBA.
  .              15 RUEC66-IDENTC-GLEBA  PIC  X(019) VALUE SPACES.
  .              15 RUEC66-AREA-NCULTVD  PIC  X(011) VALUE SPACES.
  .
  .   *
211025**** TAMANHO = 10006
  .   *
  .    01  RUEC66-COR0001-7.
  .           10 FILLER               PIC  X(006) VALUE '{2210}'.
211025        10 RUEC66-WKT-PART      PIC  X(10000)  VALUE SPACES.
      *

      **** TAMANHO = 10
       01  RUEC66-COR0001-8.
           05 RUEC66-GRUPO-COR0001-CONSCD.
              10 FILLER                     PIC X(006) VALUE '{2300}'.
              10 RUEC66-GP-COR0001-CONSCD.
                 15 RUEC66-COD-PRODT-CONSCD PIC X(004) VALUE SPACES.

      *
WIP001**** TAMANHO = 148
      *
       01  RUEC66-COR0001-9.
           05 RUEC66-COD-EMPNMNT           PIC  X(014) VALUE SPACES.
STE001     05 RUEC66-COD-SIST-PRODC        PIC  X(013) VALUE SPACES.
           05 RUEC66-VLR-PARCL-CRED        PIC  X(019) VALUE SPACES.
           05 RUEC66-VLR-PARCL-REC-PROP    PIC  X(019) VALUE SPACES.
4.12 ******* VLR-PARCL-SERVICOS PASSA A SER VALOR GARANTIA RENDA MIN.
4.12       05 RUEC66-VLR-PARCL-SERVICOS    PIC  X(019) VALUE SPACES.
           05 RUEC66-ALQ-PROAGRO           PIC  X(005) VALUE SPACES.
POS********05 RUEC66-PERC-JUROS-ENCARFIN   PIC  9(003)V99 VALUE ZEROS.
POS        05 RUEC66-PERC-JUROS-ENCARFIN   PIC S9(003)V99 VALUE ZEROS.
           05 RUEC66-TP-ENCARG-FIN-COMPL   PIC  9(002) VALUE ZEROS.
           05 RUEC66-TP-ENCARG-FIN-COMPL-X REDEFINES
              RUEC66-TP-ENCARG-FIN-COMPL   PIC  X(002).
           05 RUEC66-PERC-RSC-STN          PIC  X(005) VALUE SPACES.
POS        05 RUEC66-PERC-RSC-STN-S5       REDEFINES
POS           RUEC66-PERC-RSC-STN          PIC S9(005).
           05 RUEC66-PERC-RSC-FUND-CONSCL  PIC  X(005) VALUE SPACES.
POS        05 RUEC66-PERC-RSC-FUND-CONSCL-S5 REDEFINES
POS           RUEC66-PERC-RSC-FUND-CONSCL  PIC S9(005).
4.13       05 RUEC66-PERC-CST-EFT-TOTAL    PIC  X(005) VALUE SPACES.
4.13       05 RUEC66-PERC-CST-EFT-TOTAL-N  REDEFINES
4.13          RUEC66-PERC-CST-EFT-TOTAL    PIC  9(003)V99.
           05 RUEC66-AREA-X                PIC  X(011) VALUE SPACES.
           05 RUEC66-AREA-N REDEFINES RUEC66-AREA-X
                                           PIC  9(09)V99.
           05 RUEC66-QTD-ITEM-FINCD-N      PIC  9(012)V99 VALUE ZEROS.
           05 RUEC66-QTD-ITEM-FINCD-X REDEFINES RUEC66-QTD-ITEM-FINCD-N
                                           PIC  X(14).
           05 RUEC66-QTD-PRV-PRODC         PIC  9(012)V99 VALUE ZEROS.
           05 RUEC66-QTD-PRV-PRODC-X REDEFINES RUEC66-QTD-PRV-PRODC
                                           PIC  X(14).

      *
      **** TAMANHO = 18
      *
       01  RUEC66-COR0001-10-F.
           05 RUEC66-GRUPO-COR0001-PROP-F.
              10 FILLER              PIC  X(006) VALUE '{2400}'.
              10 RUEC66-GP-COR0001-PROP-F.
                 15 RUEC66-TP-PESSOA-PROPT-F
                                     PIC  X(001) VALUE SPACES.
                 15 RUEC66-CPFPROPT  PIC  X(011) VALUE SPACES.
050825***        CIB
050825           15 RUEC66-CIB-F     PIC  X(008) VALUE SPACES.
EG0617***        CODSNCR
EG0617           15 RUEC66-CCIR-F    PIC  X(013) VALUE SPACES.
EG0617***        NUMREGCAR
EG0617           15 RUEC66-NRO-CAR-F PIC  X(041) VALUE SPACES.
BRQ001***        NUMOUTGAGUAPROPR
BRQ001           15 RUEC66-NRO-OUT-F PIC  X(030) VALUE SPACES.
BRQ001***        PERCPRESCPROPR
BRQ001           15 RUEC66-PERC-PRESC-F  PIC X(005) VALUE SPACES.
BRQ001           15 RUEC66-PERC-PRESC-F-N REDEFINES
BRQ001              RUEC66-PERC-PRESC-F  PIC 9(003)V99.

      *
      **** TAMANHO = 18
      *
       01  RUEC66-COR0001-10-J.
           05 RUEC66-GRUPO-COR0001-PROP-J.
              10 FILLER            PIC  X(006) VALUE '{2400}'.
              10 RUEC66-GP-COR0001-PROP-J.
                 15 RUEC66-TP-PESSOA-PROPT-J
                                   PIC  X(001) VALUE SPACES.
                 15 RUEC66-CNPJBASE-PROPT
                                   PIC  X(008) VALUE SPACES.
                 15 FILLER         PIC  X(003) VALUE SPACES.
050825***        CIB
050825           15 RUEC66-CIB-J     PIC  X(008) VALUE SPACES.
EG0617***        CODSNCR
EG0617           15 RUEC66-CCIR-J    PIC  X(013) VALUE SPACES.
EG0617***        NUMREGCAR
EG0617           15 RUEC66-NRO-CAR-J PIC  X(041) VALUE SPACES.
BRQ001***        NUMOUTGAGUAPROPR
BRQ001           15 RUEC66-NRO-OUT-J PIC  X(030) VALUE SPACES.
BRQ001***        PERCPRESCPROPR
BRQ001           15 RUEC66-PERC-PRESC-J  PIC X(005) VALUE SPACES.
BRQ001           15 RUEC66-PERC-PRESC-J-N REDEFINES
BRQ001              RUEC66-PERC-PRESC-J  PIC 9(003)V99.

      *
      **** TAMANHO = 58
BS0116**** O CAMPO RUEC66-TP-GAR-EMPNMNT PASSA A SER CODIGO MODALIDADE
BS0116**** SEGURO
      *
       01  RUEC66-COR0001-11.
           05 RUEC66-TP-GAR-EMPNMNT         PIC  9(001) VALUE ZEROS.
           05 RUEC66-CNPJBASE-INST-MUTNTE   PIC  X(008) VALUE SPACES.
           05 RUEC66-NUMREF-BCCOR-INVESTMT  PIC  X(011) VALUE SPACES.
           05 RUEC66-VLR-PARCL-INVESTMT     PIC  X(019) VALUE SPACES.
           05 RUEC66-VLR-RECBRUTA-ESPRDEMP  PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 36
      *
       01  RUEC66-COR0001-12.
           05 RUEC66-GRUPO-COR0001-PARCL.
              10 FILLER                  PIC  X(006) VALUE '{2500}'.
              10 RUEC66-GP-COR0001-PARCL.
                 15 RUEC66-NUM-PARCL       PIC  9(003) VALUE ZEROS.
                 15 RUEC66-DT-PRVPGTO      PIC  9(008) VALUE ZEROS.
                 15 RUEC66-VLR-PRINC-PARCL PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 04
      *
BS0116*01  RUEC66-COR0001-13.
050825*    05 RUEC66-CODSUB-PROG         PIC  X(004) VALUE SPACES.
      *
      **** TAMANHO = 33
      *
BR4.11*01  RUEC66-COR0001-14.
BR4.11**** 05 RUEC66-GRUPO-COR0001-LIBRC.
BR4.11****    10 FILLER                  PIC  X(006) VALUE '{2600}'.
BR4.11****    10 RUEC66-GP-COR0001-LIBRC.
BR4.11****       15 RUEC66-DTLIBRC-RECUR    PIC X(008) VALUE SPACES.
BR4.11****       15 RUEC66-VLR-LIBRC-RECUR  PIC X(019) VALUE SPACES.

      *
      **** TAMANHO = 32
      *
       01  RUEC66-COR0001-15.
BR4.11**** 05 RUEC66-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.
EG0617***  05 RUEC66-ANO-MES-PLANT          PIC  X(007) VALUE SPACES.
EG0617***  DTINIPLANT
EG0617     05 RUEC66-DT-INI-PLANT           PIC  X(008) VALUE SPACES.
EG0617     05 RUEC66-DT-FIM-PLANT           PIC  X(008) VALUE SPACES.
EG0617***  05 RUEC66-ANO-MES-COLHT          PIC  X(007) VALUE SPACES.
EG0617***  DTINICOLHT
EG0617     05 RUEC66-DT-INI-COLHT           PIC  X(008) VALUE SPACES.
EG0617     05 RUEC66-DT-FIM-COLHT           PIC  X(008) VALUE SPACES.

      **** TAMANHO = 36
      *
BR4.11*01  RUEC66-COR0001-16.
BR4.11**** 05 RUEC66-GRUPO-COR0001-DESCLASS.
BR4.11****    10 FILLER                     PIC  X(006) VALUE '{2700}'.
BR4.11****    10 RUEC66-GP-COR0001-DESCLASS.
BR4.11****       15 RUEC66-DT-DESCLASS      PIC  X(008) VALUE SPACES.
BR4.11****       15 RUEC66-VLR-DESCLASS     PIC  X(019) VALUE SPACES.
BR4.11****       15 RUEC66-MTV-DESCLASS     PIC  X(003) VALUE SPACES.

QT1120* INCLUSAO DOS CAMPOS RUEC66-NUM-REF-BCCO-DESCL E
QT1120*                     RUEC66-VLR-DESCLASCD
QT1120**** TAMANHO = 33
QT1120*
QT1120 01 RUEC66-COR0001-16.
QT1120    05 RUEC66-GRUPO-COR0001-DESCLASS.
QT1120       10 FILLER                      PIC  X(006) VALUE '{2600}'.
QT1120       10 RUEC66-GP-COR0001-DESCLASS.
QT1120          15 RUEC66-NUM-REF-BCCO-DESCL
QT1120                                      PIC  X(011) VALUE SPACES.
QT1120          15 RUEC66-VLR-DESCLASCD     PIC S9(017)V9(002)
QT1120                                                  VALUE ZEROS.
BRQ001*         MOTVDESCLASSCOR
BRQ001          15 RUEC66-MOTV-DESCLASSCOR  PIC  9(003) VALUE ZEROS.

BRQ001**** TAMANHO = 13
BRQ001*
BRQ001 01  RUEC66-COR0001-17.
BRQ001     05 RUEC66-COD-STN-EQULZC      PIC  X(013) VALUE SPACES.

BRQ001**** TAMANHO = 351
BRQ001*
BRQ001 01 RUEC66-COR0001-18.
BRQ001    05 RUEC66-GRUPO-COR0001-CERTIFCOR.
BRQ001       10 RUEC66-GP-COR0001-CERTIFCOR.
BRQ001          15 RUEC66-CODCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(080) VALUE SPACES.
BRQ001          15 RUEC66-URLCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(255) VALUE SPACES.


BRQ001**** TAMANHO = 351
BRQ001*
BRQ001 01 RUEC66-COR0001-19.
BRQ001    05 RUEC66-GRUPO-COR0001-RASTLDDEM.
BRQ001       10 RUEC66-GP-COR0001-RASTLDDEM.
BRQ001          15 RUEC66-CODRASTLDDEMPNMNT
BRQ001                                      PIC  X(080) VALUE SPACES.
BRQ001          15 RUEC66-URLCODRASTLDDEMPNMNT
BRQ001                                      PIC  X(255) VALUE SPACES.

BRQ001**** TAMANHO = 13
BRQ001*
BRQ001 01  RUEC66-COR0001-20.
BRQ001     05 RUEC66-PERCENERRENOVVLEMPNMNT PIC  9(03)V99 VALUE ZEROS.

WP1021 01  RUEC66-COR0001-21.
WP1021     05 RUEC66-CICLO-CULTURA          PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUEC66-TIPO-SOLO              PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUEC66-TIPO-MANEJO            PIC  X(002)
WP1021                                      VALUE ZEROS.
STE001 01  RUEC66-COR0001-21A.
STE001     05 RUEC66-PERC-BONUS-CAR         PIC  X(005)
STE001                                      VALUE SPACES.
STEFA1     05 RUEC66-CNPJ-BASE-EXIGLDD      PIC  X(008)
STEFA1                                      VALUE SPACES.
050825     05 RUEC66-PERC-BONUS-SUS         PIC  X(005)
.                                           VALUE SPACES.
.     *
.     **** TAMANHO = 8
.     *
.      01  RUEC66-COR0001-21B.
.          05 RUEC66-GRUPO-COR0001-TPCONFDD.
.             10 FILLER                     PIC  X(006) VALUE '{2700}'.
.             10 RUEC66-GP-COR0001-TPCONFDD.
.                15 RUEC66-TPCONFDD-N       PIC  9(002) VALUE ZEROS.
.                15 FILLER REDEFINES RUEC66-TPCONFDD-N.
.                   20 RUEC66-TPCONFDD-X    PIC  X(002).
.     *
.     **** TAMANHO = 33
.     *
.      01  RUEC66-COR0001-21C.
.          05 RUEC66-GRUPO-COR0001-LIBRC.
.             10 FILLER                     PIC  X(006) VALUE '{2800}'.
.             10 RUEC66-GP-COR0001-LIBRC.
.                15 RUEC66-DTLIBRC-RECUR    PIC  X(008) VALUE SPACES.
.                15 RUEC66-VLR-LIBRC-RECUR  PIC  X(019) VALUE SPACES.
.     *
.     **** TAMANHO = 44
.     *
.      01  RUEC66-COR0001-21D.
.          05 RUEC66-GRUPO-COR0001-COOPERD.
.             10 FILLER                     PIC  X(006) VALUE '{2900}'.
.             10 RUEC66-GP-COR0001-COOPERD.
.                15 RUEC66-TP-PESSOA-COOPERD
.                                           PIC  X(001) VALUE SPACES.
.                15 RUEC66-CNPJ-CPF-COOPERD
.                                           PIC  X(014) VALUE SPACES.
.                15 RUEC66-VLR-PCL-COOPERD  PIC  X(019) VALUE SPACES.
.                15 RUEC66-CODPROG-LIN-COOPERD
050825                                      PIC  X(004) VALUE SPACES.
BSI4.8 01  RUEC66-COR0001-22.
           05 RUEC66-DTMOVTO                PIC  9(008) VALUE ZEROS.
