      *-- I#RUCA66 ----------------------------------------------------*
      *                                                                *
      * DESC: AREA PARA FORMATACAO DA MENSAGERIA COR0001 - SICOR WEB   *
      *       SICOR V 5.01                                             *
      * DATA: 11/2020                                                  *
      *----------------------------------------------------------------*
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
WP1021*---------------------------------------------------------------*
WP0311* ALTERACAO...........: ALTERACAO CAMPOS SICOR 5.05             *
.     *                       COD SISTEMA PRODUCAO DE 9 PARA 11 BYTES *
.     * RESPONSAVEL.........: WIPRO - CLAUDIO PIAZZA                  *
WP0311* GERACAO.............: 11/2022                                 *
      *---------------------------------------------------------------*
STE001* ALTERACAO...........: ALTERACAO CAMPOS SICOR 5.08             *
.     *                       COD SISTEMA PRODUCAO DE 11 P/ 13 BYTES  *
.     * RESPONSAVEL.........: STEFANINI                               *
STE001* GERACAO.............: MAIO / 2024                             *
      *---------------------------------------------------------------*
7C2510* ALTERACAO...........: ALTERACAO CAMPOS SICOR 5.11             *
.     *                       ALTERACAO TRATAMENTO DE GLEBA           *
.     * RESPONSAVEL.........: 7COMM                                   *
7C2510* GERACAO.............: NOV  / 2025                             *
      *---------------------------------------------------------------*
      *
      **** TAMANHO = 140
BS0116**** CAMPO RUCA66-TP-CATG-EMIT SIGNIFICA COD PORTE BENEFICIARIO
      *
       01  RUCA66-COR0001-1.
           05 RUCA66-CODMSG                PIC  X(009) VALUE SPACES.
           05 RUCA66-NUMCTRLIF             PIC  X(020) VALUE SPACES.
7C5.10     05 RUCA66-CNPJ-ENT-RESPONS      PIC  X(014) VALUE SPACES.
           05 RUCA66-CNPJ-PART             PIC  X(014) VALUE SPACES.
           05 RUCA66-NUMREF-BCCOROR        PIC  X(011) VALUE SPACES.
BSI4.8*    05 RUCA66-TP-CEDLCOR            PIC  X(001) VALUE SPACES.
           05 RUCA66-DTEMS                 PIC  9(008) VALUE ZEROS.
           05 RUCA66-DTVENC                PIC  9(008) VALUE ZEROS.
BR4.11     05 RUCA66-CBASE-LEGAL-PRO       PIC  X(003) VALUE SPACES.
           05 RUCA66-NUMCEDL-CRED-RURALIF  PIC  X(017) VALUE SPACES.
           05 RUCA66-TP-INSTNTO-CRED       PIC  X(002) VALUE SPACES.
BSI4.8*    05 RUCA66-CNPJ-BASEIF-SUBEMPRT  PIC  X(008) VALUE SPACES.
BSI4.8*    05 RUCA66-NUMREF-BCCOR-SUBEMPRT PIC  X(011) VALUE SPACES.
           05 RUCA66-VLR-TOTOP             PIC  X(019) VALUE SPACES.
           05 RUCA66-TP-CATG-EMIT          PIC  X(004) VALUE SPACES.

      *
      **** TAMANHO = 49
BS0116**** FOI ALTERADO O NOME DE EMITENTE PARA BENEFICIARIO, MAS NAO
BS0116**** FOI ALTERADO OS NOMES DOS CAMPOS
      *
       01  RUCA66-COR0001-2.
           05 RUCA66-GRUPO-COR0001-EMIT.
              10 FILLER                 PIC  X(006) VALUE '{1000}'.
              10 RUCA66-GP-COR0001-EMIT.
                 15 RUCA66-TP-PESSOA-EMIT PIC  X(001) VALUE SPACES.
7C5.10           15 RUCA66-CNPJ-CPF-EMIT  PIC  X(014) VALUE SPACES.
7C5.10           15 RUCA66-CAF-BENF       PIC  X(025) VALUE SPACES.
BSI4.8           15 RUCA66-TPO-BENF       PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 6
      *
       01  RUCA66-COR0001-3.
           05 RUCA66-GRUPO-COR0001-DESTCFIN.
              10 FILLER                 PIC  X(006) VALUE '{2000}'.

      *
      **** TAMANHO = 36
      *
       01  RUCA66-COR0001-4.
           05 RUCA66-GRUPO-COR0001-RENEGC.
              10 FILLER                PIC  X(006) VALUE '{2100}'.
              10 RUCA66-GP-COR0001-RENEGC.
                 15 RUCA66-NUMREF-BCCOR-RENEGC
                                       PIC  X(011) VALUE SPACES.
                 15 RUCA66-VLR-RENEGC  PIC  X(019) VALUE SPACES.
EG0617***        CODBASELEGALRENEGC
EG0617           15 RUCA66-BASE-LEGAL-RENEGC
EG0617                                 PIC  X(003) VALUE SPACES.

      *
      **** TAMANHO = 18
      *
       01  RUCA66-COR0001-5.
           05 RUCA66-CODPROG-LINHA-CRED  PIC  X(004) VALUE SPACES.
7C5.10     05 RUCA66-CODSUB-PROG         PIC  X(004) VALUE SPACES.
           05 RUCA66-TP-FNTE-REC         PIC  X(004) VALUE SPACES.
           05 RUCA66-COD-MUNIC           PIC  9(006) VALUE ZEROS.

      *
      **** TAMANHO = 25
      *
       01  RUCA66-COR0001-6.
           05 RUCA66-GRUPO-COR0001-GLEBA.
              10 FILLER                  PIC  X(006) VALUE '{2200}'.
              10 RUCA66-GP-COR0001-GLEBA.
                 15 RUCA66-IDENTC-GLEBA  PIC  X(019) VALUE SPACES.
                 15 RUCA66-AREA-NCULTVD  PIC  X(011) VALUE SPACES.

7C2510 01  RUCA66-COR0001-7.
7C2510        10 FILLER               PIC  X(006) VALUE '{2210}'.
7C2510        10 RUCA66-WKTPART       PIC  X(10000)  VALUE SPACES.
      *
      **** TAMANHO = 39
      *
7C2510*01  RUCA66-COR0001-7.
7C2510*    05 RUCA66-GRUPO-COR0001-PTPOLG.
7C2510*       10 FILLER               PIC  X(006) VALUE '{2210}'.
7C2510*       10 RUCA66-GP-COR0001-PTPOLG.
7C2510*          15 RUCA66-LAT-PONTO  PIC  X(013) VALUE SPACES.
7C2510*          15 RUCA66-LONG-PONTO PIC  X(014) VALUE SPACES.
7C2510*          15 RUCA66-ALTT-PONTO PIC  X(006) VALUE SPACES.
      *
      **** TAMANHO = 11
      *
7C2510*01  RUCA66-COR0001-7A.
7C2510*    05 RUCA66-AREA-NAO-CULT PIC  X(011) VALUE SPACES.
7C2510**** 05 RUCA66-COD-MUN-GLEB  PIC  X(006) VALUE SPACES.
      *
7C2510*
7C2510**** TAMANHO = 29
7C2510*01  RUCA66-COR0001-7B.
7C2510*    05 RUCA66-GRUPO-COR0001-GLEBA-E.
7C2510*       10 FILLER                  PIC  X(006) VALUE '{2220}'.
STE001
7C2510*01  RUCA66-COR0001-7BB.
7C2510*    05 RUCA66-GLEBA-EXCLUIDA.
7C2510*       10 FILLER                  PIC  X(006) VALUE '{2221}'.
7C2510*       10 RUCA66-COR0001-PTPLGEXC.
7C2510*          15 RUCA66-LAT-PONTO-EXCL
7C2510*                                  PIC  X(013) VALUE SPACES.
7C2510*          15 RUCA66-LONG-PONTO-EXCL
7C2510*                                  PIC  X(014) VALUE SPACES.
7C2510*          15 RUCA66-ALTT-PONTO-EXCL
7C2510*                                  PIC  X(006) VALUE SPACES.
      **** TAMANHO = 10
      *
       01  RUCA66-COR0001-8.
           05 RUCA66-GRUPO-COR0001-CONSCD.
              10 FILLER                     PIC X(006) VALUE '{2300}'.
              10 RUCA66-GP-COR0001-CONSCD.
                 15 RUCA66-COD-PRODT-CONSCD PIC X(004) VALUE SPACES.

      *
      **** TAMANHO = 140
      *
       01  RUCA66-COR0001-9.
           05 RUCA66-COD-EMPNMNT           PIC  X(014) VALUE SPACES.
WP0311*****05 RUCA66-COD-SIST-PRODC        PIC  X(009) VALUE SPACES.
WP0311*****05 RUCA66-COD-SIST-PRODC        PIC  X(011) VALUE SPACES.
STE001     05 RUCA66-COD-SIST-PRODC        PIC  X(013) VALUE SPACES.
           05 RUCA66-VLR-PARCL-CRED        PIC  X(019) VALUE SPACES.
           05 RUCA66-VLR-PARCL-REC-PROP    PIC  X(019) VALUE SPACES.
4.12 ******* VLR-PARCL-SERVICOS PASSA A SER VALOR GARANTIA RENDA MIN.
4.12       05 RUCA66-VLR-PARCL-SERVICOS    PIC  X(019) VALUE SPACES.
           05 RUCA66-ALQ-PROAGRO           PIC  X(005) VALUE SPACES.
POS********05 RUCA66-PERC-JUROS-ENCARFIN   PIC  9(003)V99 VALUE ZEROS.
POS        05 RUCA66-PERC-JUROS-ENCARFIN   PIC S9(003)V99 VALUE ZEROS.
           05 RUCA66-TP-ENCARG-FIN-COMPL   PIC  9(002) VALUE ZEROS.
           05 RUCA66-TP-ENCARG-FIN-COMPL-X REDEFINES
              RUCA66-TP-ENCARG-FIN-COMPL   PIC  X(002).
           05 RUCA66-PERC-RSC-STN          PIC  X(005) VALUE SPACES.
POS        05 RUCA66-PERC-RSC-STN-S5       REDEFINES
POS           RUCA66-PERC-RSC-STN          PIC S9(005).
           05 RUCA66-PERC-RSC-FUND-CONSCL  PIC  X(005) VALUE SPACES.
POS        05 RUCA66-PERC-RSC-FUND-CONSCL-S5 REDEFINES
POS           RUCA66-PERC-RSC-FUND-CONSCL  PIC S9(005).
4.13       05 RUCA66-PERC-CST-EFT-TOTAL    PIC  X(005) VALUE SPACES.
4.13       05 RUCA66-PERC-CST-EFT-TOTAL-N  REDEFINES
4.13          RUCA66-PERC-CST-EFT-TOTAL    PIC  9(003)V99.
           05 RUCA66-AREA-X                PIC  X(011) VALUE SPACES.
           05 RUCA66-AREA-N REDEFINES RUCA66-AREA-X
                                           PIC  9(09)V99.
           05 RUCA66-QTD-ITEM-FINCD-N      PIC  9(012)V99 VALUE ZEROS.
           05 RUCA66-QTD-ITEM-FINCD-X REDEFINES RUCA66-QTD-ITEM-FINCD-N
                                           PIC  X(14).
           05 RUCA66-QTD-PRV-PRODC         PIC  9(012)V99 VALUE ZEROS.
           05 RUCA66-QTD-PRV-PRODC-X REDEFINES RUCA66-QTD-PRV-PRODC
                                           PIC  X(14).

      *
      **** TAMANHO = 18
      *
       01  RUCA66-COR0001-10-F.
           05 RUCA66-GRUPO-COR0001-PROP-F.
              10 FILLER              PIC  X(006) VALUE '{2400}'.
              10 RUCA66-GP-COR0001-PROP-F.
                 15 RUCA66-TP-PESSOA-PROPT-F
                                     PIC  X(001) VALUE SPACES.
                 15 RUCA66-CPFPROPT  PIC  X(011) VALUE SPACES.
7C5.10***        CIB
7C5.10           15 RUCA66-CIB-F     PIC  X(008) VALUE SPACES.
EG0617***        CODSNCR
EG0617           15 RUCA66-CCIR-F    PIC  X(013) VALUE SPACES.
EG0617***        NUMREGCAR
EG0617           15 RUCA66-NRO-CAR-F PIC  X(041) VALUE SPACES.
BRQ001***        NUMOUTGAGUAPROPR
BRQ001           15 RUCA66-NRO-OUT-F PIC  X(030) VALUE SPACES.
BRQ001***        PERCPRESCPROPR
BRQ001           15 RUCA66-PERC-PRESC-F  PIC X(005) VALUE SPACES.
BRQ001           15 RUCA66-PERC-PRESC-F-N REDEFINES
BRQ001              RUCA66-PERC-PRESC-F  PIC 9(003)V99.

      *
      **** TAMANHO = 18
      *
       01  RUCA66-COR0001-10-J.
           05 RUCA66-GRUPO-COR0001-PROP-J.
              10 FILLER            PIC  X(006) VALUE '{2400}'.
              10 RUCA66-GP-COR0001-PROP-J.
                 15 RUCA66-TP-PESSOA-PROPT-J
                                   PIC  X(001) VALUE SPACES.
                 15 RUCA66-CNPJBASE-PROPT
                                   PIC  X(008) VALUE SPACES.
                 15 FILLER         PIC  X(003) VALUE SPACES.
7C5.10***        CIB
7C5.10           15 RUCA66-CIB-J     PIC  X(008) VALUE SPACES.
EG0617***        CODSNCR
EG0617           15 RUCA66-CCIR-J    PIC  X(013) VALUE SPACES.
EG0617***        NUMREGCAR
EG0617           15 RUCA66-NRO-CAR-J PIC  X(041) VALUE SPACES.
BRQ001***        NUMOUTGAGUAPROPR
BRQ001           15 RUCA66-NRO-OUT-J PIC  X(030) VALUE SPACES.
BRQ001***        PERCPRESCPROPR
BRQ001           15 RUCA66-PERC-PRESC-J  PIC X(005) VALUE SPACES.
BRQ001           15 RUCA66-PERC-PRESC-J-N REDEFINES
BRQ001              RUCA66-PERC-PRESC-J  PIC 9(003)V99.

      *
      **** TAMANHO = 58
BS0116**** O CAMPO RUCA66-TP-GAR-EMPNMNT PASSA A SER CODIGO MODALIDADE
BS0116**** SEGURO
      *
       01  RUCA66-COR0001-11.
           05 RUCA66-TP-GAR-EMPNMNT         PIC  9(001) VALUE ZEROS.
           05 RUCA66-CNPJBASE-INST-MUTNTE   PIC  X(008) VALUE SPACES.
           05 RUCA66-NUMREF-BCCOR-INVESTMT  PIC  X(011) VALUE SPACES.
           05 RUCA66-VLR-PARCL-INVESTMT     PIC  X(019) VALUE SPACES.
           05 RUCA66-VLR-RECBRUTA-ESPRDEMP  PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 36
      *
       01  RUCA66-COR0001-12.
           05 RUCA66-GRUPO-COR0001-PARCL.
              10 FILLER                  PIC  X(006) VALUE '{2500}'.
              10 RUCA66-GP-COR0001-PARCL.
                 15 RUCA66-NUM-PARCL       PIC  9(003) VALUE ZEROS.
                 15 RUCA66-DT-PRVPGTO      PIC  9(008) VALUE ZEROS.
                 15 RUCA66-VLR-PRINC-PARCL PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 04
      *
      *
      **** TAMANHO = 33
      *
BR4.11*01  RUCA66-COR0001-14.
BR4.11**** 05 RUCA66-GRUPO-COR0001-LIBRC.
BR4.11****    10 FILLER                  PIC  X(006) VALUE '{2600}'.
BR4.11****    10 RUCA66-GP-COR0001-LIBRC.
BR4.11****       15 RUCA66-DTLIBRC-RECUR    PIC X(008) VALUE SPACES.
BR4.11****       15 RUCA66-VLR-LIBRC-RECUR  PIC X(019) VALUE SPACES.

      *
      **** TAMANHO = 32
      *
       01  RUCA66-COR0001-15.
BR4.11**** 05 RUCA66-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.
EG0617***  05 RUCA66-ANO-MES-PLANT          PIC  X(007) VALUE SPACES.
EG0617***  DTINIPLANT
EG0617     05 RUCA66-DT-INI-PLANT           PIC  X(008) VALUE SPACES.
EG0617     05 RUCA66-DT-FIM-PLANT           PIC  X(008) VALUE SPACES.
EG0617***  05 RUCA66-ANO-MES-COLHT          PIC  X(007) VALUE SPACES.
EG0617***  DTINICOLHT
EG0617     05 RUCA66-DT-INI-COLHT           PIC  X(008) VALUE SPACES.
EG0617     05 RUCA66-DT-FIM-COLHT           PIC  X(008) VALUE SPACES.

      **** TAMANHO = 36
      *
BR4.11*01  RUCA66-COR0001-16.
BR4.11**** 05 RUCA66-GRUPO-COR0001-DESCLASS.
BR4.11****    10 FILLER                     PIC  X(006) VALUE '{2700}'.
BR4.11****    10 RUCA66-GP-COR0001-DESCLASS.
BR4.11****       15 RUCA66-DT-DESCLASS      PIC  X(008) VALUE SPACES.
BR4.11****       15 RUCA66-VLR-DESCLASS     PIC  X(019) VALUE SPACES.
BR4.11****       15 RUCA66-MTV-DESCLASS     PIC  X(003) VALUE SPACES.

      * INCLUSAO DOS CAMPOS RUCA66-NUM-REF-BCCO-DESCL E
      *                     RUCA66-VLR-DESCLASCD
      **** TAMANHO = 36
      *
V5.01  01 RUCA66-COR0001-16.
V5.01     05 RUCA66-GP-COR0001-DESCLASS.
V5.01        10 FILLER                      PIC  X(006) VALUE '{2600}'.
BRQ001       10 RUCA66-GP-COR0001-DESCLASS.
V5.01           15 RUCA66-NUM-REF-BCCO-DESCL PIC  X(011) VALUE SPACES.
V5.01           15 RUCA66-VLR-DESCLASCD      PIC  X(019) VALUE SPACES.
V5.01           15 RUCA66-VLR-DESCLASCD-N    REDEFINES
V5.01              RUCA66-VLR-DESCLASCD      PIC S9(017)V9(02).
BRQ001*         MOTVDESCLASSCOR
BRQ001          15 RUCA66-MOTV-DESCLASSCOR.
BRQ001             17 RUCA66-MOTV-DESCLASSCOR-N PIC  9(003) VALUE ZEROS.

BRQ001**** TAMANHO = 13
BRQ001*
BRQ001 01  RUCA66-COR0001-17.
BRQ001     05 RUCA66-COD-STN-EQULZC      PIC  X(013) VALUE SPACES.

BRQ001**** TAMANHO = 351
BRQ001*
BRQ001 01 RUCA66-COR0001-18.
BRQ001    05 RUCA66-GRUPO-COR0001-CERTIFCOR.
BRQ001       10 RUCA66-GP-COR0001-CERTIFCOR.
BRQ001          15 RUCA66-CODCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(080) VALUE SPACES.
BRQ001          15 RUCA66-URLCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(255) VALUE SPACES.


BRQ001**** TAMANHO = 351
BRQ001*
BRQ001 01 RUCA66-COR0001-19.
BRQ001    05 RUCA66-GRUPO-COR0001-RASTLDDEM.
BRQ001       10 RUCA66-GP-COR0001-RASTLDDEM.
BRQ001          15 RUCA66-CODRASTLDDEMPNMNT
BRQ001                                      PIC  X(080) VALUE SPACES.
BRQ001          15 RUCA66-URLCODRASTLDDEMPNMNT
BRQ001                                      PIC  X(255) VALUE SPACES.

BRQ001**** TAMANHO = 13
BRQ001*
BRQ001 01  RUCA66-COR0001-20.
BRQ001     05 RUCA66-PERCENERRENOVVLEMPNMNT PIC  9(03)V99 VALUE ZEROS.

WP1021 01  RUCA66-COR0001-21.
WP1021     05 RUCA66-CICLO-CULTURA          PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUCA66-TIPO-SOLO              PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUCA66-TIPO-MANEJO            PIC  X(002)
WP1021                                      VALUE ZEROS.
STE001 01  RUCA66-COR0001-21A.
STE001     05 RUCA66-PERC-BONUS-CAR         PIC  X(005) VALUE SPACES.
STEFA1     05 RUCA66-CNPJ-BASE-EXIGLDD      PIC  X(008) VALUE SPACES.
7C5.10     05 RUCA66-PERC-BONUS-SUST-X.
7C5.10        10 RUCA66-PERC-BONUS-SUST     PIC  9(003)V99 VALUE ZEROS.

       01  RUCA66-COR0001-21B.
           05 RUCA66-GRUPO-COR0001-CONFDD.
              10 FILLER                     PIC X(006) VALUE '{2700}'.
              10 RUCA66-GP-COR0001-CONFDD.
                 15 RUCA66-TP-CONFDD-X      PIC X(002) VALUE SPACES.
                 15 RUCA66-TP-CONFDD REDEFINES RUCA66-TP-CONFDD-X
                                            PIC 9(002).
7C5.10 01  RUCA66-COR0001-21C.
.          05 RUCA66-GRUPO-COR0001-LIBRC.
.             10 FILLER                  PIC  X(006) VALUE '{2800}'.
.             10 RUCA66-GP-COR0001-LIBRC.
.                15 RUCA66-DTLIBRC          PIC X(008) VALUE SPACES.
7C5.10           15 RUCA66-VLR-LIBRC        PIC X(019) VALUE SPACES.

7C5.10 01  RUCA66-COR0001-21D.
.          05 RUCA66-GRUPO-COR0001-COOP.
.             10 FILLER                  PIC  X(006) VALUE '{2900}'.
.             10 RUCA66-GP-COR0001-COOP.
.                15 RUCA66-TP-PESSOA-COOP      PIC  X(001) VALUE SPACES.
.                15 RUCA66-CNPJ-CPF-COOP       PIC  X(014) VALUE SPACES.
.                15 RUCA66-VLR-PARC-COOP       PIC  X(019) VALUE SPACES.
7C5.10           15 RUCA66-CODPROG-LINHA-COOP  PIC  X(004) VALUE SPACES.

WP1021 01  RUCA66-COR0001-22.
           05 RUCA66-DTMOVTO                PIC  9(008) VALUE ZEROS.
