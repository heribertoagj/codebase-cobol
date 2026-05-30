      *================================================================*
      * VERSAO 5.12- IMPLANTACAO PREVISTA MAIO/2026                    *
      *================================================================*
      *================================================================*
      * VERSAO 4.06- IMPLANTACAO PREVISTA JULHO/2015                   *
      *================================================================*
      *================================================================*
      * VERSAO 4.0 - IMPLANTACAO PREVISTA OUTUBRO/2013                 *
      *================================================================*
      *---------------------------------------------------------------*
      *              S O N D A      P R O C W O R K                   *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#RUEC68 - COR0001E                     *
      * GERACAO.............: AGOSTO/2012                             *
      *---------------------------------------------------------------*
      * OBJETIVO....: DETALHAMENTO DA SISMSG PARA COR0001E DO RECOR   *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
EG0615*              B S I   T E C N O L O G I A                      *
EG0615*---------------------------------------------------------------*
EG0615* INCLUDE.............: I#RUEC68 - COR0001E                     *
EG0615* GERACAO.............: JULHO/2015                              *
EG0615*---------------------------------------------------------------*
EG0615* OBJETIVO....: INCLUSAO DO CAMPO CODIGO DO SUBPROGRAMA         *
EG0615*---------------------------------------------------------------*
      *---------------------------------------------------------------*
EG0216*              B S I   T E C N O L O G I A                      *
EG0216*---------------------------------------------------------------*
EG0216* INCLUDE.............: I#RUEC68 - COR0001E                     *
EG0216* GERACAO.............: FEVEREIRO/2016                          *
EG0216*---------------------------------------------------------------*
EG0216* OBJETIVO....: ADAPTACAO A VERSAO 4.07 DO CATALOGO SICOR       *
EG0216*---------------------------------------------------------------*
EG0216**** CAMPO RUEC68-TP-CATG-EMIT SIGNIFICA COD PORTE BENEFICIARIO
BSI4.8*---------------------------------------------------------------*
BSI4.8*             B S I   T E C N O L O G I A                      *
BSI4.8*---------------------------------------------------------------*
BSI4.8* INCLUDE.............: I#RUEC68 - COR0001E                    *
BSI4.8* GERACAO.............: SETEMBRO/2016                          *
BSI4.8*---------------------------------------------------------------*
BSI4.8* OBJETIVO....: ADAPTACAO A VERSAO 4.08 DO CATALOGO SICOR      *
      *---------------------------------------------------------------*
QT1120* ALTERACAO...........: INCLUSAO DOS CAMPOS                     *
QT1120*                             V3-NUM-REF-BCCO-DESCL             *
QT1120*                             V3-VLR-DESCLASCD                  *
QT1120* GERACAO.............: 13/11/2020                              *
      *---------------------------------------------------------------*
STE001* ALTERACAO...........: ALTERACAO      SICOR 5.08               *
STE001*                       COD-SIST-PRODC PARA 13                  *
STE001*                       CRIACAO DA AREA DE GLEBA EXCLUIDA       *
STE001* GERACAO.............: MAIO/2024                               *
      *---------------------------------------------------------------*
AG1243*                  B R A D E S C O   B B I                      *
AG1243*---------------------------------------------------------------*
AG1243* INCLUDE.............: I#RUEC68 - COR0001E                     *
AG1243* GERACAO.............: MAIO/2026                               *
AG1243*---------------------------------------------------------------*
AG1243* OBJETIVO....: ADEQUACOES DE LAYOUT PARA SICOR 5.12            *
AG1243*               - ALINHAMENTO COMPLETO COM I#RUEC66 SICOR 5.11  *
AG1243*               - CAMPO {2210} SUBSTITUIDO POR WKT-PART X(10000)*
AG1243*               - CAMPOS SICOR 5.10 (050825): CNPJ ALFANUM.,    *
AG1243*                 CAF-BENF, CODSUB-PROG, NOVOS BLOCOS           *
AG1243*               - CAMPOS SICOR 5.11 (211025): AREA-NCULTVD,     *
AG1243*                 COR0001-21B/21C/21D                           *
AG1243*               - REFERENCIA: AGROLEG-1243                      *
AG1243*---------------------------------------------------------------*
      *
      **** TAMANHO BLOCO 1 = 140
      *
       01  RUEC68-COR0001-1.
           05 RUEC68-CODMSG                PIC  X(009) VALUE SPACES.
           05 RUEC68-CODMSG-E              PIC  X(008) VALUE SPACES.
           05 RUEC68-NUMCTRLIF             PIC  X(020) VALUE SPACES.
           05 RUEC68-NUMCTRLIF-E           PIC  X(008) VALUE SPACES.
AG1243*   PIC CORRIGIDO DE 9(014) PARA X(014) - SICOR 5.10
050825     05 RUEC68-CNPJ-ENT-RESPONS      PIC  X(014) VALUE SPACES.
           05 RUEC68-CNPJ-ENT-RESPONS-E    PIC  X(008) VALUE SPACES.
           05 RUEC68-CNPJ-PART             PIC  X(014) VALUE SPACES.
           05 RUEC68-CNPJ-PART-E           PIC  X(008) VALUE SPACES.
           05 RUEC68-NUMREF-BCCOROR        PIC  X(011) VALUE SPACES.
           05 RUEC68-NUMREF-BCCOROR-E      PIC  X(008) VALUE SPACES.
           05 RUEC68-DTEMS                 PIC  9(008) VALUE ZEROS.
           05 RUEC68-DTEMS-E               PIC  X(008) VALUE SPACES.
           05 RUEC68-DTVENC                PIC  9(008) VALUE ZEROS.
           05 RUEC68-DTVENC-E              PIC  X(008) VALUE SPACES.
BR4.11     05 RUEC68-CBASE-LEGAL-PRO       PIC  X(003) VALUE SPACES.
BR4.11     05 RUEC68-CBASE-LEGAL-PRO-E     PIC  X(008) VALUE SPACES.
           05 RUEC68-NUMCEDL-CRED-RURALIF  PIC  X(017) VALUE SPACES.
           05 RUEC68-NUMCEDL-CRED-RURAL-E  PIC  X(008) VALUE SPACES.
           05 RUEC68-TP-INSTNTO-CRED       PIC  X(002) VALUE SPACES.
           05 RUEC68-TP-INSTNTO-CRED-E     PIC  X(008) VALUE SPACES.
           05 RUEC68-VLR-TOTOP             PIC  X(019) VALUE SPACES.
           05 RUEC68-VLR-TOTOP-E           PIC  X(008) VALUE SPACES.
           05 RUEC68-TP-CATG-EMIT          PIC  X(004) VALUE SPACES.
           05 RUEC68-TP-CATG-EMIT-E        PIC  X(008) VALUE SPACES.
 
      * 
      **** TAMANHO BLOCO 2 = 49
EG0216**** FOI ALTERADO O NOME DE EMITENTE PARA BENEFICIARIO, MAS NAO
EG0216**** FOI ALTERADO OS NOMES DOS CAMPOS
      *
       01  RUEC68-COR0001-2.
           05 RUEC68-REPET-COR0001-EMIT.
              10 RUEC68-GRUPO-COR0001-EMIT.
                15 FILLER                  PIC  X(006) VALUE '{1000}'.
                15 RUEC68-TP-PESSOA-EMIT   PIC  X(001) VALUE SPACES.
                15 RUEC68-TP-PESSOA-EMIT-E PIC  X(008) VALUE SPACES.
AG1243*         PIC CORRIGIDO DE 9(014) PARA X(014) - SICOR 5.10
050825          15 RUEC68-CNPJ-CPF-EMIT    PIC  X(014) VALUE SPACES.
                15 RUEC68-CNPJ-CPF-EMIT-E  PIC  X(008) VALUE SPACES.
AG1243*         CAMPO RENOMEADO DE DAP-EMIT PARA CAF-BENF - SICOR 5.10
050825          15 RUEC68-CAF-BENF         PIC  X(025) VALUE SPACES.
                15 RUEC68-CAF-BENF-E       PIC  X(008) VALUE SPACES.
BSI4.8          15 RUEC68-TPO-BENF         PIC  X(003) VALUE SPACES.
BSI4.8          15 RUEC68-TPO-BENF-E       PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 3 = 6
      *
       01  RUEC68-COR0001-3.
           05 RUEC68-REPET-COR0001-DESTCFIN.
             10 RUEC68-GRUPO-COR0001-DESTCFIN.
                15 FILLER                 PIC  X(006) VALUE '{2000}'.

      *
      **** TAMANHO BLOCO 4 = 36
      *
       01  RUEC68-COR0001-4.
           05 RUEC68-REPET-COR0001-RENEGC.
             10 RUEC68-GRUPO-COR0001-RENEGC.
                15 FILLER                PIC  X(006) VALUE '{2100}'.
                15 RUEC68-NUMREF-BCCOR-RENEGC
                                         PIC  X(011) VALUE SPACES.
                15 RUEC68-NUMREF-BCCOR-RENE-E
                                         PIC  X(008) VALUE SPACES.
                15 RUEC68-VLR-RENEGC     PIC  X(019) VALUE SPACES.
                15 RUEC68-VLR-RENEGC-E   PIC  X(008) VALUE SPACES.
EG0617          15 RUEC68-BASE-LEGAL-RENEGC
                                         PIC  X(003) VALUE SPACES.
EG0617          15 RUEC68-BASE-LEGAL-RENEGC-E
                                         PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 5 = 18
      *
       01  RUEC68-COR0001-5.
           05 RUEC68-CODPROG-LINHA-CRED  PIC  X(004) VALUE SPACES.
           05 RUEC68-CODPROG-LINHA-CR-E  PIC  X(008) VALUE SPACES.
AG1243*   CAMPO CODSUB-PROG INCLUIDO - SICOR 5.10
050825     05 RUEC68-CODSUB-PROG         PIC  X(004) VALUE SPACES.
050825     05 RUEC68-CODSUB-PROG-E       PIC  X(008) VALUE SPACES.
           05 RUEC68-TP-FNTE-REC         PIC  X(004) VALUE SPACES.
           05 RUEC68-TP-FNTE-REC-E       PIC  X(008) VALUE SPACES.
           05 RUEC68-COD-MUNIC           PIC  9(006) VALUE ZEROS.
           05 RUEC68-COD-MUNIC-E         PIC  X(008) VALUE SPACES.
      *
AG1243**** TAMANHO BLOCO 6 = 36 (SICOR 5.11 - INCLUIDO AREA-NCULTVD)
      *
       01  RUEC68-COR0001-6.
           05 RUEC68-REPET-COR0001-GLEBA.
             10 RUEC68-GRUPO-COR0001-GLEBA.
                15 FILLER                PIC  X(006) VALUE '{2200}'.
                15 RUEC68-IDENTC-GLEBA   PIC  X(019) VALUE SPACES.
                15 RUEC68-IDENTC-GLEBA-E PIC  X(008) VALUE SPACES.
AG1243*         CAMPO INCLUIDO - SICOR 5.11
211025          15 RUEC68-AREA-NCULTVD   PIC  X(011) VALUE SPACES.
211025          15 RUEC68-AREA-NCULTVD-E PIC  X(008) VALUE SPACES.

     *
AG1243**** TAMANHO BLOCO 7 = 10006
AG1243**** SICOR 5.11 - SUBSTITUICAO TOTAL DE COORDENADAS POR WKT-PART
AG1243**** CAMPO {2210} PASSA A RECEBER STRING WKT (WELL-KNOWN TEXT)
     *
       01  RUEC68-COR0001-7.
          05 RUEC68-REPET-COR0001-PTPOLG.
             10 RUEC68-GRUPO-COR0001-PTPOLG.
                15 FILLER               PIC  X(006) VALUE '{2210}'.
211025          15 RUEC68-WKT-PART      PIC  X(10000) VALUE SPACES.
211025          15 RUEC68-WKT-PART-E    PIC  X(008)   VALUE SPACES.

      *
      **** TAMANHO BLOCO 7B = 29
STE001*   GLEBA EXCLUIDA - TAG {2220}
      *
       01  RUEC68-COR0001-7B.
STE001     05 RUEC68-GLEBA-EXCLUIDA.
STE001       10 RUEC68-COR0001-PTPLGEXC.
STE001          15 FILLER               PIC  X(006) VALUE '{2220}'.
STE001          15 RUEC68-LAT-PONTO-EXCL
STE001                                  PIC  X(008) VALUE SPACES.
STE001          15 RUEC68-LAT-PONTO-EXCL-E
STE001                                  PIC  X(008) VALUE SPACES.
STE001          15 RUEC68-LONG-PONTO-EXCL
STE001                                  PIC  X(009) VALUE SPACES.
STE001          15 RUEC68-LONG-PONTO-EXCL-E
STE001                                  PIC  X(008) VALUE SPACES.
STE001          15 RUEC68-ALTT-PONTO-EXCL
STE001                                  PIC  X(006) VALUE SPACES.
STE001          15 RUEC68-ALTT-PONTO-EXCL-E
STE001                                  PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 8 = 10
      *
       01  RUEC68-COR0001-8.
           05 RUEC68-REPET-COR0001-CONSCD.
             10 FILLER                  PIC  X(006) VALUE '{2300}'.
             10 RUEC68-COD-PRODT-CONSCD PIC  X(004) VALUE SPACES.
             10 RUEC68-COD-PRODT-CONS-E PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 9 = 148
      *
       01  RUEC68-COR0001-9.
           05 RUEC68-COD-EMPNMNT           PIC  X(014) VALUE SPACES.
           05 RUEC68-COD-EMPNMNT-E         PIC  X(008) VALUE SPACES.
STE001     05 RUEC68-COD-SIST-PRODC        PIC  X(013) VALUE SPACES.
           05 RUEC68-COD-SIST-PRODC-E      PIC  X(008) VALUE SPACES.
           05 RUEC68-VLR-PARCL-CRED        PIC  X(019) VALUE SPACES.
           05 RUEC68-VLR-PARCL-CRED-E      PIC  X(008) VALUE SPACES.
           05 RUEC68-VLR-PARCL-REC-PROP    PIC  X(019) VALUE SPACES.
           05 RUEC68-VLR-PARCL-REC-PR-E    PIC  X(008) VALUE SPACES.
4.12       05 RUEC68-VLR-PARCL-SERVICOS    PIC  X(019) VALUE SPACES.
4.12       05 RUEC68-VLR-PARCL-SERVICOS-E  PIC  X(008) VALUE SPACES.
           05 RUEC68-ALQ-PROAGRO           PIC  X(005) VALUE SPACES.
           05 RUEC68-ALQ-PROAGRO-E         PIC  X(008) VALUE SPACES.
POS        05 RUEC68-PERC-JUROS-ENCARFIN   PIC  S9(003)V99 VALUE ZEROS.
           05 RUEC68-PERC-JUROS-ENCARF-E   PIC  X(008) VALUE SPACES.
           05 RUEC68-TP-ENCARG-FIN-COMPL   PIC  9(002) VALUE ZEROS.
           05 RUEC68-TP-ENCARG-FIN-COM-X REDEFINES
              RUEC68-TP-ENCARG-FIN-COMPL   PIC  X(002).
           05 RUEC68-TP-ENCARG-FIN-COM-E   PIC  X(008) VALUE SPACES.
           05 RUEC68-PERC-RSC-STN          PIC  X(005) VALUE SPACES.
POS        05 RUEC68-PERC-RSC-STN-S5 REDEFINES
POS           RUEC68-PERC-RSC-STN          PIC  S9(005).
           05 RUEC68-PERC-RSC-STN-E        PIC  X(008) VALUE SPACES.
           05 RUEC68-PERC-RSC-FUND-CONSCL  PIC  X(005) VALUE SPACES.
POS        05 RUEC68-PERC-RSC-FUND-CONS-S5 REDEFINES
POS           RUEC68-PERC-RSC-FUND-CONSCL  PIC  S9(005).
           05 RUEC68-PERC-RSC-FUND-CONS-E  PIC  X(008) VALUE SPACES.
4.13       05 RUEC68-PERC-CST-EFT-TOTAL    PIC  X(005) VALUE SPACES.
4.13       05 RUEC68-PERC-CST-EFT-TOTAL-N REDEFINES
4.13          RUEC68-PERC-CST-EFT-TOTAL    PIC  9(003)V99.
4.13       05 RUEC68-PERC-CST-EFT-TOT-E    PIC  X(008) VALUE SPACES.
           05 RUEC68-AREA                  PIC  X(011) VALUE SPACES.
           05 RUEC68-AREA-N REDEFINES
              RUEC68-AREA                  PIC  9(09)V99.
           05 RUEC68-AREA-E                PIC  X(008) VALUE SPACES.
           05 RUEC68-QTD-ITEM-FINCD-N      PIC  9(012)V99 VALUE ZEROS.
           05 RUEC68-QTD-ITEM-FINCD-X REDEFINES
              RUEC68-QTD-ITEM-FINCD-N      PIC  X(014).
           05 RUEC68-QTD-ITEM-FINCD-E      PIC  X(008) VALUE SPACES.
           05 RUEC68-QTD-PRV-PRODC         PIC  9(012)V99 VALUE ZEROS.
           05 RUEC68-QTD-PRV-PRODC-X REDEFINES
              RUEC68-QTD-PRV-PRODC         PIC  X(014).
           05 RUEC68-QTD-PRV-PRODC-E       PIC  X(008) VALUE SPACES.
 
      *
      **** TAMANHO BLOCO 10-F = 118
      *
       01  RUEC68-COR0001-10-F.
           05 RUEC68-GRUPO-COR0001-PROP-F.
              10 FILLER              PIC  X(006) VALUE '{2400}'.
              10 RUEC68-GP-COR0001-PROP-F.
                15 RUEC68-TP-PESSOA-PROPT-F
                                    PIC  X(001) VALUE SPACES.
                15 RUEC68-TP-PESSOA-PROPT-F-E
                                    PIC  X(008) VALUE SPACES.
                15 RUEC68-CPFPROPT  PIC  X(011) VALUE SPACES.
                15 RUEC68-CPFPROPT-E
                                    PIC  X(008) VALUE SPACES.
050825          15 RUEC68-CIB-F     PIC  X(008) VALUE SPACES.
050825          15 RUEC68-CIB-F-E   PIC  X(008) VALUE SPACES.
EG0617          15 RUEC68-CCIR-F    PIC  X(013) VALUE SPACES.
EG0617          15 RUEC68-CCIR-F-E  PIC  X(008) VALUE SPACES.
EG0617          15 RUEC68-NRO-CAR-F PIC  X(041) VALUE SPACES.
EG0617          15 RUEC68-NRO-CAR-F-E
                                    PIC  X(008) VALUE SPACES.
BRQ001          15 RUEC68-NRO-OUT-F PIC  X(030) VALUE SPACES.
BRQ001          15 RUEC68-NRO-OUT-F-E
                                    PIC  X(008) VALUE SPACES.
BRQ001          15 RUEC68-PERC-PRESC-F
                                    PIC  X(005) VALUE SPACES.
BRQ001          15 RUEC68-PERC-PRESC-F-N REDEFINES
BRQ001             RUEC68-PERC-PRESC-F   PIC  9(003)V99.
BRQ001          15 RUEC68-PERC-PRESC-F-E PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 10-J = 121
      *
       01  RUEC68-COR0001-10-J.
           05 RUEC68-GRUPO-COR0001-PROP-J.
              10 FILLER            PIC  X(006) VALUE '{2400}'.
              10 RUEC68-GP-COR0001-PROP-J.
                 15 RUEC68-TP-PESSOA-PROPT-J
                                  PIC  X(001) VALUE SPACES.
                 15 RUEC68-TP-PESSOA-PROPT-J-E
                                  PIC  X(008) VALUE SPACES.
                 15 RUEC68-CNPJBASE-PROPT
                                  PIC  X(008) VALUE SPACES.
                 15 RUEC68-CNPJBASE-PROPT-E
                                  PIC  X(008) VALUE SPACES.
                 15 FILLER         PIC  X(003) VALUE SPACES.
050825           15 RUEC68-CIB-J   PIC  X(008) VALUE SPACES.
050825           15 RUEC68-CIB-J-E PIC  X(008) VALUE SPACES.
EG0617           15 RUEC68-CCIR-J  PIC  X(013) VALUE SPACES.
EG0617           15 RUEC68-CCIR-J-E
                                  PIC  X(008) VALUE SPACES.
EG0617           15 RUEC68-NRO-CAR-J
                                  PIC  X(041) VALUE SPACES.
EG0617           15 RUEC68-NRO-CAR-J-E
                                  PIC  X(008) VALUE SPACES.
BRQ001           15 RUEC68-NRO-OUT-J
                                  PIC  X(030) VALUE SPACES.
BRQ001           15 RUEC68-NRO-OUT-J-E
                                  PIC  X(008) VALUE SPACES.
BRQ001           15 RUEC68-PERC-PRESC-J
                                  PIC  X(005) VALUE SPACES.
BRQ001           15 RUEC68-PERC-PRESC-J-N REDEFINES
BRQ001             RUEC68-PERC-PRESC-J   PIC  9(003)V99.
BRQ001           15 RUEC68-PERC-PRESC-J-E PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 11 = 58
EG0216**** O CAMPO RUEC68-TP-GAR-EMPNMNT PASSA A SER CODIGO MODALIDADE
EG0216**** SEGURO
      *
       01  RUEC68-COR0001-11.
           05 RUEC68-TP-GAR-EMPNMNT         PIC  9(001) VALUE ZEROS.
           05 RUEC68-TP-GAR-EMPNMNT-E       PIC  X(008) VALUE SPACES.
           05 RUEC68-CNPJBASE-INST-MUTNTE   PIC  X(008) VALUE SPACES.
           05 RUEC68-CNPJBASE-INST-MUTN-E   PIC  X(008) VALUE SPACES.
           05 RUEC68-NUMREF-BCCOR-INVESTMT  PIC  X(011) VALUE SPACES.
           05 RUEC68-NUMREF-BCCOR-INVEST-E  PIC  X(008) VALUE SPACES.
           05 RUEC68-VLR-PARCL-INVESTMT     PIC  X(019) VALUE SPACES.
           05 RUEC68-VLR-PARCL-INVESTMT-E   PIC  X(008) VALUE SPACES.
           05 RUEC68-VLR-RECBRUTA-ESPRDEMP  PIC  X(019) VALUE SPACES.
           05 RUEC68-VLR-RECBRUTA-ESPRDE-E  PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 12 = 36
      *
       01  RUEC68-COR0001-12.
           05 RUEC68-REPET-COR0001-PARCL.
              10 RUEC68-GRUPO-COR0001-PARCL.
                15 FILLER                  PIC  X(006) VALUE '{2500}'.
                15 RUEC68-NUM-PARCL        PIC  9(003) VALUE ZEROS.
                15 RUEC68-NUM-PARCL-E      PIC  X(008) VALUE SPACES.
                15 RUEC68-DT-PRVPGTO       PIC  9(008) VALUE ZEROS.
                15 RUEC68-DT-PRVPGTO-E     PIC  X(008) VALUE SPACES.
                15 RUEC68-VLR-PRINC-PARCL  PIC  X(019) VALUE SPACES.
                15 RUEC68-VLR-PRINC-PAR-E  PIC  X(008) VALUE SPACES.
      *
      **** TAMANHO BLOCO 13 = 4
      *
       01  RUEC68-COR0001-13.
EG0216     05 RUEC68-SUBPROG                PIC  X(004) VALUE SPACES.
EG0615     05 RUEC68-SUBPROG-E              PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 14 = 33
      *
EG0216 01 RUEC68-COR0001-14.
EG0216     05 RUEC68-GRUPO-COR0001-LIBRC.
EG0216        10 FILLER                  PIC  X(006) VALUE '{2600}'.
EG0216        10 RUEC68-GP-COR0001-LIBRC.
EG0216           15 RUEC68-DTLIBRC-RECUR    PIC  X(008) VALUE SPACES.
EG0216           15 RUEC68-DTLIBRC-RECUR-E  PIC  X(008) VALUE SPACES.
EG0216           15 RUEC68-VLR-LIBRC-RECUR  PIC  X(019) VALUE SPACES.
EG0216           15 RUEC68-VLR-LIBRC-RECUR-E
EG0216                                      PIC  X(008) VALUE SPACES.

      *
      **** TAMANHO BLOCO 15 = 32
      *
       01  RUEC68-COR0001-15.
EG0216     05 RUEC68-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.
EG0216     05 RUEC68-VLR-PROD-OBTD-E        PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-INI-PLANT           PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-INI-PLANT-E         PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-FIM-PLANT           PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-FIM-PLANT-E         PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-INI-COLHT           PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-INI-COLHT-E         PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-FIM-COLHT           PIC  X(008) VALUE SPACES.
EG0216     05 RUEC68-DT-FIM-COLHT-E         PIC  X(008) VALUE SPACES.

      *
QT1120**** TAMANHO BLOCO 16 = 33
      *
QT1120 01 RUEC68-COR0001-16.
QT1120     05 RUEC68-GP-COR0001-DESCLASS.
QT1120       10 FILLER                      PIC  X(006) VALUE '{2600}'.
QT1120       10 RUEC68-NUM-REF-BCCO-DESCL   PIC  X(011) VALUE SPACES.
QT1120       10 RUEC68-NUM-REF-BCCO-DESCL-E PIC  X(008) VALUE SPACES.
QT1120       10 RUEC68-VLR-DESCLASCD        PIC  S9(017)V9(002)
QT1120                                                   VALUE ZEROS.
QT1120       10 RUEC68-VLR-DESCLASCD-E      PIC  X(008) VALUE SPACES.
BRQ001       10 RUEC68-MOTV-DESCLASSCOR     PIC  9(003) VALUE ZEROS.
BRQ001       10 RUEC68-MOTV-DESCLASSCOR-E   PIC  X(008) VALUE SPACES.

      *
BRQ001**** TAMANHO BLOCO 17 = 13
      *
BRQ001 01  RUEC68-COR0001-17.
BRQ001     05 RUEC68-COD-STN-EQULZC      PIC  X(013) VALUE SPACES.
BRQ001     05 RUEC68-COD-STN-EQULZC-E    PIC  X(008) VALUE SPACES.

      *
BRQ001**** TAMANHO BLOCO 18 = 351
      *
BRQ001 01 RUEC68-COR0001-18.
BRQ001    05 RUEC68-GRUPO-COR0001-CERTFCOR.
BRQ001       10 RUEC68-GP-COR0001-CERTFCOR.
BRQ001          15 RUEC68-CODCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(080) VALUE SPACES.
BRQ001          15 RUEC68-CODCERTIFCORGNCOEMPN-E
BRQ001                                      PIC  X(008) VALUE SPACES.
BRQ001          15 RUEC68-URLCERTIFCORGNCOEMPNMNT
BRQ001                                      PIC  X(255) VALUE SPACES.
BRQ001          15 RUEC68-URLCERTIFCORGNCOEMPN-E
BRQ001                                      PIC  X(008) VALUE SPACES.

      *
BRQ001**** TAMANHO BLOCO 19 = 351
      *
BRQ001 01 RUEC68-COR0001-19.
BRQ001    05 RUEC68-GRUPO-COR0001-RASTLDDEM.
BRQ001       10 RUEC68-GP-COR0001-RASTLDDEM.
BRQ001          15 RUEC68-CODRASTLDDEMPNMNT  PIC  X(080) VALUE SPACES.
BRQ001          15 RUEC68-CODRASTLDDEMPNMNT-E
BRQ001                                       PIC  X(008) VALUE SPACES.
BRQ001          15 RUEC68-URLCODRASTLDDEMPNMNT
BRQ001                                       PIC  X(255) VALUE SPACES.
BRQ001          15 RUEC68-URLCODRASTLDDEMPNMT-E
BRQ001                                       PIC  X(008) VALUE SPACES.

      *
BRQ001**** TAMANHO BLOCO 20 = 5
      *
BRQ001 01  RUEC68-COR0001-20.
BRQ001     05 RUEC68-PERCENERRENOVVLEMPNMNT  PIC  9(03)V99
BRQ001                                                   VALUE ZEROS.
BRQ001     05 RUEC68-PERCENERRENOVVLEMPNMT-E PIC  X(008) VALUE SPACES.

      *
WP1021**** TAMANHO BLOCO 21 = 6
      *
WP1021 01  RUEC68-COR0001-21.
WP1021     05 RUEC68-CICLO-CULTURA          PIC  X(002) VALUE SPACES.
WP1021     05 RUEC68-CICLO-CULTURA-E        PIC  X(008) VALUE SPACES.
WP1021     05 RUEC68-TIPO-SOLO              PIC  X(002) VALUE SPACES.
WP1021     05 RUEC68-TIPO-SOLO-E            PIC  X(008) VALUE SPACES.
WP1021     05 RUEC68-TIPO-MANEJO            PIC  X(002) VALUE SPACES.
WP1021     05 RUEC68-TIPO-MANEJO-E          PIC  X(008) VALUE SPACES.

      *
STE001**** TAMANHO BLOCO 21A = 18
      *
STE001 01  RUEC68-COR0001-21A.
STE001     05 RUEC68-PERC-BONUS-CAR         PIC  X(005) VALUE SPACES.
STE001     05 RUEC68-PERC-BONUS-CAR-E       PIC  X(008) VALUE SPACES.
STEFA1     05 RUEC68-CNPJ-BASE-EXIGLDD      PIC  X(008) VALUE SPACES.
STEFA1     05 RUEC68-CNPJ-BASE-EXIGLDD-E    PIC  X(008) VALUE SPACES.
050825     05 RUEC68-PERC-BONUS-SUS         PIC  X(005) VALUE SPACES.
050825     05 RUEC68-PERC-BONUS-SUS-E       PIC  X(008) VALUE SPACES.

      *
050825**** TAMANHO BLOCO 21B = 8
      *
050825 01  RUEC68-COR0001-21B.
050825     05 RUEC68-GRUPO-COR0001-TPCONFDD.
050825        10 FILLER                     PIC  X(006) VALUE '{2700}'.
050825        10 RUEC68-GP-COR0001-TPCONFDD.
050825           15 RUEC68-TPCONFDD-N       PIC  9(002) VALUE ZEROS.
050825           15 RUEC68-TPCONFDD-X REDEFINES
050825              RUEC68-TPCONFDD-N       PIC  X(002).
050825           15 RUEC68-TPCONFDD-E       PIC  X(008) VALUE SPACES.

      *
050825**** TAMANHO BLOCO 21C = 33
      *
050825 01  RUEC68-COR0001-21C.
050825     05 RUEC68-GRUPO-COR0001-LIBRC.
050825        10 FILLER                     PIC  X(006) VALUE '{2800}'.
050825        10 RUEC68-GP-COR0001-LIBRC.
050825           15 RUEC68-DTLIBRC-RECUR    PIC  X(008) VALUE SPACES.
050825           15 RUEC68-DTLIBRC-RECUR-E  PIC  X(008) VALUE SPACES.
050825           15 RUEC68-VLR-LIBRC-RECUR  PIC  X(019) VALUE SPACES.
050825           15 RUEC68-VLR-LIBRC-RECUR-E
050825                                      PIC  X(008) VALUE SPACES.
      *
050825**** TAMANHO BLOCO 21D = 44
      *
050825 01  RUEC68-COR0001-21D.
050825     05 RUEC68-GRUPO-COR0001-COOPERD.
050825        10 FILLER                     PIC  X(006) VALUE '{2900}'.
050825        10 RUEC68-GP-COR0001-COOPERD.
050825           15 RUEC68-TP-PESSOA-COOPERD
050825                                      PIC  X(001) VALUE SPACES.
050825           15 RUEC68-TP-PESSOA-COOPERD-E
050825                                      PIC  X(008) VALUE SPACES.
050825           15 RUEC68-CNPJ-CPF-COOPERD  PIC  X(014) VALUE SPACES.
050825           15 RUEC68-CNPJ-CPF-COOPERD-E
050825                                       PIC  X(008) VALUE SPACES.
050825           15 RUEC68-VLR-PCL-COOPERD   PIC  X(019) VALUE SPACES.
050825           15 RUEC68-VLR-PCL-COOPERD-E PIC  X(008) VALUE SPACES.
050825           15 RUEC68-CODPROG-LIN-COOPERD
050825                                       PIC  X(004) VALUE SPACES.
050825           15 RUEC68-CODPROG-LIN-COOPERD-E
050825                                       PIC  X(008) VALUE SPACES.
      *
BSI4.8**** TAMANHO BLOCO 22 = 8
      *
BSI4.8 01  RUEC68-COR0001-22.
BSI4.8     05 RUEC68-DTMOVTO                PIC  9(008) VALUE ZEROS.
BSI4.8     05 RUEC68-DTMOVTO-E              PIC  X(008) VALUE SPACES.
