      *---------------------------------------------------------------*
      *                  C A P G E M I N I                            *
      *---------------------------------------------------------------*
      * INCLUDE.............: CRURWAY1 - PARA MODULO RCOR7000(COR0001)*
      * GERACAO.............: MAIO/2014                               *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO PARA FORMATACAO DE ENVIO    *
      *               DOS DADOS PARA RECOR                            *
      *---------------------------------------------------------------*
      * TAMANHO TOTAL SEM RETICAO =  790                              *
      *---------------------------------------------------------------*
      * ALTERACAO...........: INCLUSAO DO ATRIBUTO DE SUBPROGRMA NA   *
      *                       VERSAO 4.06 DO CATALOGO SICOR COR0001   *
      * GERACAO.............: 16/06/2015                              *
      *---------------------------------------------------------------*
      * ALTERACAO...........: AUMENTO NO TAMANHO DOS CAMPOS DE COORDE-*
      *                       NADAS GEODESICAS, VERSAO 4.07           *
      * GERACAO.............: 04/01/2016                              *
      *---------------------------------------------------------------*
      * ALTERACAO...........: INCLUSAO DOS CAMPOS COD BASE LEGAL RENEG*
      *                       PLANTIO E COLHEITA                      *
      * GERACAO.............: 04/01/2016                              *
      *---------------------------------------------------------------*
QT1120* ALTERACAO...........: INCLUSAO DOS CAMPOS                     *
QT1120*                             V3-NUM-REF-BCCO-DESCL             *
QT1120*                             V3-VLR-DESCLASCD                  *
QT1120* GERACAO.............: 13/11/2020                              *
      *---------------------------------------------------------------*
BR0521* ALTERACAO...........: INCLUSAO DOS CAMPOS SICOR 5.02          *
.     *                       NUMERO OUTORGA AGUA PROPRIEDADE         *
.     *                       PERCENTUAL PRESERVACAO PROPRIEDADE      *
.     *                       MOTIVO DESCLASSIFICACAO COR             *
.     *                       CODIGO STN EQUALIZACAO                  *
.     *                       CODIGO CERTIFICACAO ORGANICA EMPR       *
.     *                       URL CODIGO CERTIFICACAO ORGANICA EMPR   *
.     *                       CODIGO RASTREABILIDADE EMPR             *
.     *                       URL CODIGO RASTREABILIDADE EMPR         *
.     *                       PERCENTUAL ENERGIA RENOVAVEL EMPR       *
BR0521* GERACAO.............: 10/2021                                 *
WP1021* ALTERACAO...........: INCLUSAO DOS CAMPOS SICOR 5.03          *
.     *                       TIPO DE SOLO                            *
.     *                       TIPO DE MANEJO                          *
WP1021* GERACAO.............: 10/2021                                 *
      *---------------------------------------------------------------*
WIP001* ALTERACAO...........: ALTERACAO DE CAMPO SICOR 5.05           *
-     *                       COD-SIST-PRODC                         *
WIP001* GERACAO.............: 10/2022                                 *
      *---------------------------------------------------------------*
STE001* ALTERACAO...........: ALTERACAO DE CAMPO SICOR 5.08           *
-     *                       COD-SIST-PRODC                          *
STE001* GERACAO.............: MAIO / 2024                             *
      *---------------------------------------------------------------*
      *
STE003**** TAMANHO = 129
      * O CAMPO CRURY1-TP-CATG-EMIT FOI RENOMEADO PELO BACEN PARA
      * CODIGO DO PORTE DO BENEFICIARIO
      *
STE003* ALTERACAO...........: ALTERACAO DE CAMPO SICOR 5.09           *
-     *                       NIRF-F E NIRF-J DE 9 PARA 8 BYTES       *
STE003* GERACAO.............: NOVEMBRO / 2024                         *
      *---------------------------------------------------------------*
7C5.10* ALTERACAO...........: ALTERACAO DE CAMPO SICOR 5.10           *
7C5.10* GERACAO.............: 08/2025                                 *
      *---------------------------------------------------------------*
211025* ALTERACAO...........: SQUAD FLASH - HISTORIA AGRO7-2978       *
.     *                       ADEQUACOES DE LAYOUT PARA SICOR 5.11    *
211025* GERACAO.............: OUTUBRO/2025                            *
      *---------------------------------------------------------------*
       01  CRURY1-COR0001-1.
           05 CRURY1-CODMSG                PIC  X(009) VALUE SPACES.
           05 CRURY1-NUMCTRLIF             PIC  X(020) VALUE SPACES.
7C5.10     05 CRURY1-CNPJ-ENT-RESPONS      PIC  X(014) VALUE SPACES.
           05 CRURY1-CNPJ-PART             PIC  X(014) VALUE SPACES.
           05 CRURY1-NUMREF-BCCOROR        PIC  X(011) VALUE SPACES.
CAT408***  CAMPO RETIRADO
CAT408***  05 CRURY1-TP-CEDLCOR            PIC  X(001) VALUE SPACES.
CAT408***
           05 CRURY1-DTEMS                 PIC  9(008) VALUE ZEROS.
           05 CRURY1-DTVENC                PIC  9(008) VALUE ZEROS.
BR4.11     05 CRURY1-COD-BASE-PRORROG      PIC  X(003) VALUE SPACES.
           05 CRURY1-NUMCEDL-CRED-RURALIF  PIC  X(017) VALUE SPACES.
           05 CRURY1-TP-INSTNTO-CRED       PIC  X(002) VALUE SPACES.
CAT408***  CAMPOS RETIRADOS
CAT408***  05 CRURY1-NUMREF-BCCOR-SUBEMPRT PIC  X(011) VALUE SPACES.
CAT408***
           05 CRURY1-VLR-TOTOP             PIC  X(019) VALUE SPACES.
           05 CRURY1-TP-CATG-EMIT          PIC  X(004) VALUE SPACES.

      *
CAT408**** TAMANHO = 49
      * O NOME DO EMITENTE DOI ALTERADO PARA BENEFICIARIO NO BACEN
      *
       01  CRURY1-COR0001-2.
           05 CRURY1-GRUPO-COR0001-EMIT.
              10 FILLER                 PIC  X(006) VALUE '{1000}'.
              10 CRURY1-GP-COR0001-EMIT.
                 15 CRURY1-TP-PESSOA-EMIT PIC  X(001) VALUE SPACES.
7C5.10           15 CRURY1-CNPJ-CPF-EMIT  PIC  X(014) VALUE SPACES.
7C5.10           15 CRURY1-CAF-BENF       PIC  X(025) VALUE SPACES.
CAT408           15 CRURY1-TPO-BENF       PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 6
      *
       01  CRURY1-COR0001-3.
              10 FILLER                 PIC  X(006) VALUE '{2000}'.

      *
      **** TAMANHO = 39
      *
       01  CRURY1-COR0001-4.
           05 CRURY1-GRUPO-COR0001-RENEGC.
              10 FILLER                PIC  X(006) VALUE '{2100}'.
              10 CRURY1-GP-COR0001-RENEGC.
                 15 CRURY1-NUMREF-BCCOR-RENEGC
                                       PIC  X(011) VALUE SPACES.
                 15 CRURY1-VLR-RENEGC  PIC  X(019) VALUE SPACES.
201706           15 CRURY1-COD-BASE-RENEG
201706                                 PIC  X(003) VALUE SPACES.

      *
      **** TAMANHO = 14
      *
       01  CRURY1-COR0001-5.
           05 CRURY1-CODPROG-LINHA-CRED  PIC  X(004) VALUE SPACES.
7C5.10     05 CRURY1-CODSUB-PROG         PIC  X(004) VALUE SPACES.
           05 CRURY1-COD-MUNIC           PIC  9(006) VALUE ZEROS.

      *
      **** TAMANHO = 25
      *
       01  CRURY1-COR0001-6.
           05 CRURY1-GRUPO-COR0001-GLEBA.
              10 FILLER                  PIC  X(006) VALUE '{2200}'.
              10 CRURY1-GP-COR0001-GLEBA.
                 15 CRURY1-IDENTC-GLEBA  PIC  X(019) VALUE SPACES.
7C2510           10 CRURY1-AREA-NAO-CULT PIC  X(011) VALUE SPACES.

7C2510 01  CRURY1-COR0001-7.
7C2510**** 26 <WKTPART>                     [1..N]
7C2510     05 FILLER                     PIC  X(006) VALUE '{2210}'.
7C2510     05 CRURY1-WKT-PART            PIC  X(10000) VALUE SPACES.

211025*    ==> INICIO DOS CAMPOS EXCLUIDOS DA SICOR 5.11
      *
CAT408**** TAMANHO = 39
      *
      *    05 CRURY1-GRUPO-COR0001-PTPOLG.
      *       10 FILLER               PIC  X(006) VALUE '{2210}'.
      *       10 CRURY1-GP-COR0001-PTPOLG.
CAT408*          15 CRURY1-LAT-PONTO    PIC  X(013)   VALUE SPACES.
CAT408*          15 CRURY1-LONG-PONTO   PIC  X(014)   VALUE SPACES.
      *          15 CRURY1-ALTT-PONTO   PIC  X(006)   VALUE SPACES.
211025*    ==> FIM DOS CAMPOS EXCLUIDOS DA SICOR 5.11
      *
CAT408**** TAMANHO = 17
      *
7C2510*01  CRURY1-COR0001-7A.
7C2510*    05 CRURY1-AREA-NAO-CULT      PIC  X(011) VALUE SPACES.
BR4.11*****05 CRURY1-COD-MUN-GLEB       PIC  X(006) VALUE SPACES.
      *
211025*    ==> INICIO DOS CAMPOS EXCLUIDOS DA SICOR 5.11
STE001*
STE001**** TAMANHO = 29
STE001*
STE001*01  CRURY1-COR0001-7B.
STE001*    05 CRURY1-GRUPO-COR0001-GLEBA-E.
STE001*       10 FILLER                     PIC  X(006) VALUE '{2220}'.
STE001*01  CRURY1-COR0001-7BB.
STE001*    05 CRURY1-GLEBA-EXCLUIDA.
STE001*       10 CRURY1-COR0001-PTPLGEXC.
STE001*          15 FILLER               PIC  X(006) VALUE '{2221}'.
STE001*          15 CRURY1-LAT-PONTO-EXCL
STE001*                                  PIC  X(013) VALUE SPACES.
STE001*          15 CRURY1-LONG-PONTO-EXCL
STE001*                                  PIC  X(014) VALUE SPACES.
STE001*          15 CRURY1-ALTT-PONTO-EXCL
STE001*                                  PIC  X(006) VALUE SPACES.
211025*    ==> FIM DOS CAMPOS EXCLUIDOS DA SICOR 5.11
211025*
211025**** TAMANHO = 10006
211025*

      **** TAMANHO = 10
      *
       01  CRURY1-COR0001-8.
           05 CRURY1-GRUPO-COR0001-CONSCD.
              10 FILLER                     PIC X(006) VALUE '{2300}'.
              10 CRURY1-GP-COR0001-CONSCD.

      *
WIP001**** TAMANHO = 148
      *
       01  CRURY1-COR0001-9.
           05 CRURY1-COD-EMPNMNT           PIC  X(014) VALUE SPACES.
STE001     05 CRURY1-COD-SIST-PRODC        PIC  X(013) VALUE SPACES.
           05 CRURY1-VLR-PARCL-CRED        PIC  X(019) VALUE SPACES.
           05 CRURY1-VLR-PARCL-REC-PROP    PIC  X(019) VALUE SPACES.
4.12 ******* VLR-PARCL-SERVICOS PASSA A SER VALOR GARANTIA RENDA MIN.
BS1213     05 CRURY1-VLR-PARCL-SERVICOS    PIC  X(019) VALUE SPACES.
           05 CRURY1-ALQ-PROAGRO           PIC  X(005) VALUE SPACES.
           05 CRURY1-PERC-JUROS-ENCARFIN   PIC S9(003)V99 VALUE ZEROS.
           05 CRURY1-TP-ENCARG-FIN-COMPL   PIC  9(002) VALUE ZEROS.
           05 CRURY1-TP-ENCARG-FIN-COMPL-X REDEFINES
              CRURY1-TP-ENCARG-FIN-COMPL   PIC  X(002).
           05 CRURY1-PERC-RSC-STN          PIC  X(005) VALUE SPACES.
           05 CRURY1-PERC-RSC-FUND-CONSCL  PIC  X(005) VALUE SPACES.
4.13       05 CRURY1-PERC-CST-EFT-TOT-X    PIC  X(005) VALUE SPACES.
4.13       05 CRURY1-PERC-CST-EFT-TOT-N    REDEFINES
4.13          CRURY1-PERC-CST-EFT-TOT-X    PIC  9(003)V99.
           05 CRURY1-AREA-N REDEFINES CRURY1-AREA-X
                                           PIC  9(09)V99.
           05 CRURY1-QTD-ITEM-FINCD-N      PIC  9(012)V99 VALUE ZEROS.
           05 CRURY1-QTD-ITEM-FINCD-X REDEFINES CRURY1-QTD-ITEM-FINCD-N
                                           PIC  X(14).
           05 CRURY1-QTD-PRV-PRODC         PIC  9(012)V99 VALUE ZEROS.
           05 CRURY1-QTD-PRV-PRODC-X REDEFINES CRURY1-QTD-PRV-PRODC
                                           PIC  X(14).
      *
      **** TAMANHO = 116
      *
       01  CRURY1-COR0001-10-F.
           05 CRURY1-GRUPO-COR0001-PROP-F.
              10 FILLER              PIC  X(006) VALUE '{2400}'.
              10 CRURY1-GP-COR0001-PROP-F.
                 15 CRURY1-TP-PESSOA-PROPT-F
                                     PIC  X(001) VALUE SPACES.
                 15 CRURY1-CPFPROPT  PIC  X(011) VALUE SPACES.
STE003*          15 CRURY1-NIRF-F    PIC  X(009) VALUE SPACES.
7C5.10           15 CRURY1-CIB-F     PIC  X(008) VALUE SPACES.
201706           15 CRURY1-COD-SNCR-F
201706                               PIC  X(013) VALUE SPACES.
201706           15 CRURY1-CREG-AMBTL-RURAL-F
201706                               PIC  X(041) VALUE SPACES.
BR0521           15 CRURY1-COUTGA-AGUA-IMOV-F
BR0521                               PIC  X(030) VALUE SPACES.
BR0521           15 CRURY1-PPRESV-AMBTL-PPRIE-F
BR0521                               PIC  9(003)V9(02)
BR0521                                           VALUE ZEROS.
BR0521           15 CRURY1-PPRESV-AMBTL-PPRIE-F-X
BR0521              REDEFINES CRURY1-PPRESV-AMBTL-PPRIE-F
BR0521                               PIC  X(005).

      *
      **** TAMANHO = 116
      *
       01  CRURY1-COR0001-10-J.
           05 CRURY1-GRUPO-COR0001-PROP-J.
              10 FILLER            PIC  X(006) VALUE '{2400}'.
              10 CRURY1-GP-COR0001-PROP-J.
                 15 CRURY1-TP-PESSOA-PROPT-J
                                   PIC  X(001) VALUE SPACES.
                 15 CRURY1-CNPJBASE-PROPT
                                   PIC  X(008) VALUE SPACES.
                 15 FILLER         PIC  X(003) VALUE SPACES.
STE003*          15 CRURY1-NIRF-J  PIC  X(009) VALUE SPACES.
7C5.10           15 CRURY1-CIB-J  PIC   X(008) VALUE SPACES.
201706           15 CRURY1-COD-SNCR-J
201706                             PIC  X(013) VALUE SPACES.
201706           15 CRURY1-CREG-AMBTL-RURAL-J
201706                             PIC  X(041) VALUE SPACES.
BR0521           15 CRURY1-COUTGA-AGUA-IMOV-J
BR0521                             PIC  X(030) VALUE SPACES.
BR0521           15 CRURY1-PPRESV-AMBTL-PPRIE-J
BR0521                             PIC  9(003)V9(02)
BR0521                                         VALUE ZEROS.
BR0521           15 CRURY1-PPRESV-AMBTL-PPRIE-J-X
BR0521              REDEFINES CRURY1-PPRESV-AMBTL-PPRIE-J
BR0521                             PIC  X(005).

      *
      **** TAMANHO = 58
      * O CAMPO CRURY1-TP-GAR-EMPTNMNT FOI RENOMEADO PELO BACEN PARA
      * CODIGO DA MODALIDADE DE SEGURO
      *
           05 CRURY1-TP-GAR-EMPNMNT         PIC  9(001) VALUE ZEROS.
           05 CRURY1-CNPJBASE-INST-MUTNTE   PIC  X(008) VALUE SPACES.
           05 CRURY1-NUMREF-BCCOR-INVESTMT  PIC  X(011) VALUE SPACES.
           05 CRURY1-VLR-PARCL-INVESTMT     PIC  X(019) VALUE SPACES.
           05 CRURY1-VLR-RECBRUTA-ESPRDEMP  PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 36
      *
       01  CRURY1-COR0001-12.
           05 CRURY1-GRUPO-COR0001-PARCL.
              10 FILLER                  PIC  X(006) VALUE '{2500}'.
              10 CRURY1-GP-COR0001-PARCL.
                 15 CRURY1-NUM-PARCL       PIC  9(003) VALUE ZEROS.
                 15 CRURY1-DT-PRVPGTO      PIC  9(008) VALUE ZEROS.
                 15 CRURY1-VLR-PRINC-PARCL PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 33
      *
BR4.11*01  CRURY1-COR0001-14.
BR4.11*       10 FILLER                  PIC  X(006) VALUE '{2600}'.
BR4.11*       10 CRURY1-GP-COR0001-LIBRC.
BR4.11*          15 CRURY1-DLIBRC-RECUR     PIC X(008) VALUE SPACES.
BR4.11*          15 CRURY1-VLR-LIBRC-REC    PIC X(019) VALUE SPACES.

      *
CAT408**** TAMANHO = 51
      *
       01  CRURY1-COR0001-15.
BR4.11*    05 CRURY1-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.
201706     05 CRURY1-DTINI-PLANT            PIC  X(008) VALUE SPACES.
201706     05 CRURY1-DTFIM-PLANT            PIC  X(008) VALUE SPACES.
201706     05 CRURY1-DTINI-COLHT            PIC  X(008) VALUE SPACES.
201706     05 CRURY1-DTFIM-COLHT            PIC  X(008) VALUE SPACES.

      **** TAMANHO = 36
      *
BR4.11*01  CRURY1-COR0001-16.
BR4.11*    05 CRURY1-GRUPO-COR0001-DESCLASS.
BR4.11*       10 FILLER                     PIC  X(006) VALUE '{2700}'.
BR4.11*       10 CRURY1-GP-COR0001-DESCLASS.
BR4.11*          15 CRURY1-VLR-DESCLASS     PIC  X(019) VALUE SPACES.
BR4.11*          15 CRURY1-MTV-DESCLASS     PIC  X(003) VALUE SPACES.


QT1120* INCLUSAO DOS CAMPOS V3-NUM-REF-BCCO-DESCL E V3-VLR-DESCLASCD
QT1120**** TAMANHO = 39
QT1120*
QT1120 01  CRURY1-COR0001-16.
QT1120     05 CRURY1-GRP-COR0001-DESCLASSIF.
QT1120        10 FILLER                     PIC  X(006) VALUE '{2600}'.
QT1120        10 CRURY1-GP-COR0001-DESCLASS.
QT1120           15 CRURY1-NUM-REF-BCCO-DESCL
QT1120                                      PIC  X(011)    VALUE SPACES.
QT1120           15 CRURY1-VLR-DESCLASCD
QT1120                                      PIC S9(017)V9(002)
QT1120                                                      VALUE ZEROS.
BR0521           15 CRURY1-MOTVO-DESCLASS
BR0521                                      PIC  X(003)    VALUE SPACES.

BR0521 01  CRURY1-COR0001-17.
BR0521     05 CRURY1-COD-STN-EQUALIZ        PIC  X(013) VALUE SPACES.
BR0521**** TAMANHO = 335
BR0521*
BR0521 01  CRURY1-COR0001-18.
BR0521     05 CRURY1-GP-COR0001-CERTF-ORGNCA.
BR0521        10 CRURY1-CCETFC-ORGNC-EMPTO
BR0521                                      PIC  X(080)    VALUE SPACES.
BR0521        10 CRURY1-EURL-CETFC-ORGNC
BR0521                                      PIC  X(255)    VALUE SPACES.

BR0521**** TAMANHO = 335
BR0521*
BR0521 01  CRURY1-COR0001-19.
BR0521     05 CRURY1-GP-COR0001-RASTLDD-EMP.
BR0521        10 CRURY1-CRSTRB-EMPTO
BR0521                                      PIC  X(080)    VALUE SPACES.
BR0521        10 CRURY1-EURL-RSTRB
BR0521                                      PIC  X(255)    VALUE SPACES.

BR0521 01  CRURY1-COR0001-20.
BR0521     05 CRURY1-PENERG-RENOV           PIC  9(003)V9(02)
BR0521                                      VALUE ZEROS.
BR0521     05 CRURY1-PENERG-RENOV-X   REDEFINES CRURY1-PENERG-RENOV
BR0521                                      PIC  X(005).           
BR0521                                      PIC  X(005).

WP1021 01  CRURY1-COR0001-21.
WP1021     05 CRURY1-CICLO-CULTURA          PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 CRURY1-TIPO-SOLO              PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 CRURY1-TIPO-MANEJO            PIC  X(002)
WP1021                                      VALUE ZEROS.

STE001 01  CRURY1-COR0001-21A.
STE001     05 CRURY1-PERC-BONUS-CAR         PIC  X(05) VALUE SPACES.
STE003     05 CRURY1-IND-CPF-CNPJ           PIC  X(08) VALUE SPACES.
7C5.10     05 CRURY1-PERC-BONUS-SUST-X.
7C5.10        10 CRURY1-PERC-BONUS-SUST     PIC  9(03)V99 VALUE ZEROS.

7C5.10 01  CRURY1-COR0001-21B.
.          05 CRURY1-GRUPO-COR0001-CONFDD.
.             10 FILLER                     PIC  X(06) VALUE '{2700}'.
.             10 CRURY1-GP-COR0001-CONFDD.
.                15 CRURY1-TP-CONFDD-X      PIC  X(02) VALUE SPACES.
7C5.10                                      PIC  9(02).

7C5.10 01  CRURY1-COR0001-21C.
.          05 CRURY1-GRUPO-COR0001-LIBRC.
.             10 FILLER                     PIC  X(06) VALUE '{2800}'.
.             10 CRURY1-GP-COR0001-LIBRC.
.                15 CRURY1-DTLIBRC          PIC  X(08) VALUE SPACES.
7C5.10           15 CRURY1-VLR-LIBRC        PIC  X(19) VALUE SPACES.

7C5.10 01  CRURY1-COR0001-21D.
.          05 CRURY1-GRUPO-COR0001-COOP.
.             10 FILLER                     PIC  X(06) VALUE '{2900}'.
.             10 CRURY1-GP-COR0001-COOP.
.                15 CRURY1-TP-PESSOA-COOP      PIC  X(01) VALUE SPACES.
.                15 CRURY1-CNPJ-CPF-COOP       PIC  X(14) VALUE SPACES.
.                15 CRURY1-VLR-PARC-COOP       PIC  X(19) VALUE SPACES.
7C5.10           15 CRURY1-CODPROG-LINHA-COOP  PIC  X(04) VALUE SPACES.

WP1021* DTMOVTO REALOCADO.
WP1021 01  CRURY1-COR0001-22.
           05 CRURY1-DTMOVTO                PIC  9(008) VALUE ZEROS.
