      *---------------------------------------------------------------*
      *              S O N D A      P R O C W O R K                   *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#RUEC90 - RETORNO COR0003R1            *
      * GERACAO.............: JULHO/2021                              *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE TRATAMENTO DA COR0003R1                 *
      *---------------------------------------------------------------*
      * TAMANHO TOTAL SEM RETICAO =  714                              *
      *---------------------------------------------------------------*
STE001* ALTERACAO...........: ALTERACAO      SICOR 5.08               *
-     *                       COD-SIST-PRODC PARA 13                  *
-     *                       CRIACAO DA AREA DE GLEBA EXCLUIDA       *
STE001* GERACAO.............: MAIO/2024                               *
      *---------------------------------------------------------------*
      *
      **** TAMANHO = 52 - PARTE FIXA
      *
       01  RUEC90-COR0003-1.
           05 RUEC90-CODMSG                PIC  X(009) VALUE SPACES.
           05 RUEC90-NUMCTRLIF             PIC  X(020) VALUE SPACES.
           05 RUEC90-CNPJ-ENT-RESPONS      PIC  9(014) VALUE ZEROS.
           05 RUEC90-QTDCED                PIC  9(009) VALUE ZEROS.
      *
      **** TAMANHO = 106 - GRUPO_COR0003R1_CEDL
      *
       01  RUEC90-COR0003-2.
           05 RUEC90-GRUPO-COR0003-CEDL.
              10 FILLER                      PIC  X(006) VALUE '{1000}'.
              10 RUEC90-GP-COR0003-CEDL.
                 15 RUEC90-CNPJ-RESP-CEDL    PIC  9(014) VALUE ZEROS.
                 15 RUEC90-CNPJ-PART         PIC  X(014) VALUE SPACES.
                 15 RUEC90-NUMREF-BCCOROR    PIC  X(011) VALUE SPACES.
                 15 RUEC90-DTEMS             PIC  9(008) VALUE ZEROS.
                 15 RUEC90-DTVENC            PIC  9(008) VALUE ZEROS.
                 15 RUEC90-CBASE-LEGAL-PRO   PIC  X(003) VALUE SPACES.
                 15 RUEC90-NUMCEDL-CRED-RURALIF
                                             PIC  X(017) VALUE SPACES.
                 15 RUEC90-TP-INSTNTO-CRED   PIC  X(002) VALUE SPACES.
                 15 RUEC90-VLR-TOTOP         PIC  X(019) VALUE SPACES.
                 15 RUEC90-CODPORTEBENFCRIO  PIC  X(004) VALUE SPACES.
      *
      **** TAMANHO = 49 - GRUPO_COR0003R1_BENFCRIO
      *
       01  RUEC90-COR0003-3.
           05 RUEC90-GRUPO-COR0003-BENF.
              10 FILLER                 PIC  X(006) VALUE '{1100}'.
              10 RUEC90-GP-COR0003-BENF.
                 15 RUEC90-TP-PESSOA-BENF PIC  X(001) VALUE SPACES.
                 15 RUEC90-CNPJ-CPF-BENF  PIC  9(014) VALUE ZEROS.
                 15 RUEC90-DAP-BENF       PIC  X(025) VALUE SPACES.
                 15 RUEC90-TPO-BENF       PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 6 - GRUPO_COR0003R1_DESTCFINCMNT
      *
       01  RUEC90-COR0003-4.
           05 RUEC90-GRUPO-COR0003-DESTCFIN.
              10 FILLER                 PIC  X(006) VALUE '{1200}'.
      *
      **** TAMANHO = 39 - GRUPO_COR0003R1_RENEGC
      *
       01  RUEC90-COR0003-5.
           05 RUEC90-GRUPO-COR0003-RENEGC.
              10 FILLER                PIC  X(006) VALUE '{1210}'.
              10 RUEC90-GP-COR0003-RENEGC.
                 15 RUEC90-NUMREF-BCCOR-RENEGC
                                       PIC  X(011) VALUE SPACES.
                 15 RUEC90-VLR-RENEGC  PIC  X(019) VALUE SPACES.
      ***        CODBASELEGALRENEGC
                 15 RUEC90-BASE-LEGAL-RENEGC
                                       PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 14
      *
       01  RUEC90-COR0003-6.
           05 RUEC90-CODPROG-LINHA-CRED  PIC  X(004) VALUE SPACES.
           05 RUEC90-TP-FNTE-REC         PIC  X(004) VALUE SPACES.
           05 RUEC90-COD-MUNIC           PIC  9(006) VALUE ZEROS.

      *
      **** TAMANHO = 25 - GRUPO_COR0003R1_GLEBA
      *
       01  RUEC90-COR0003-7.
           05 RUEC90-GRUPO-COR0003-GLEBA.
              10 FILLER                  PIC  X(006) VALUE '{1220}'.
              10 RUEC90-GP-COR0003-GLEBA.
                 15 RUEC90-IDENTC-GLEBA  PIC  X(019) VALUE SPACES.

      *
      **** TAMANHO = 39 - GRUPO_COR0003R1_PONTOPOLG
      *
       01  RUEC90-COR0003-8.
           05 RUEC90-GRUPO-COR0003-PTPOLG.
              10 FILLER               PIC  X(006) VALUE '{1221}'.
              10 RUEC90-GP-COR0003-PTPOLG.
                 15 RUEC90-LAT-PONTO  PIC  S9(02)V9(11) VALUE ZEROS.
                 15 RUEC90-LONG-PONTO PIC  S9(03)V9(11) VALUE ZEROS.
                 15 RUEC90-ALTT-PONTO PIC  S9(04)V9(02) VALUE ZEROS.
STE001*
STE001**** TAMANHO = 29
STE001 01  RUEC90-COR0003-8X.
STE001     05 RUEC90-GRUPO-COR0001-GLEBA-E.
STE001        10 FILLER               PIC  X(006) VALUE '{2220}'.
STE001*
STE001 01  RUEC90-COR0003-8XX.
STE001     05 RUEC90-GLEBA-EXCLUIDA.
STE001        10 RUEC90-COR0001-PTPLGEXC.
STE001           15 FILLER               PIC  X(006) VALUE '{2221}'.
STE001           15 RUEC90-LAT-PONTO-EXCL
STE001                                   PIC  X(013) VALUE SPACES.
STE001           15 RUEC90-LONG-PONTO-EXCL
STE001                                   PIC  X(014) VALUE SPACES.
STE001           15 RUEC90-ALTT-PONTO-EXCL
STE001                                   PIC  X(006) VALUE SPACES.

      *
      **** TAMANHO = 11
      *
       01  RUEC90-COR0003-8A.
           05 RUEC90-AREA-NAO-CULT PIC S9(09)V9(02) VALUE ZEROS.
      *
      **** TAMANHO = 10 - GRUPO_COR0003R1_CONSCD
      *
       01  RUEC90-COR0003-9.
           05 RUEC90-GRUPO-COR0003-CONSCD.
              10 FILLER                     PIC X(006) VALUE '{1230}'.
              10 RUEC90-GP-COR0003-CONSCD.
                 15 RUEC90-COD-PRODT-CONSCD PIC X(004) VALUE SPACES.
      *
      **** TAMANHO = 146
      *
       01  RUEC90-COR0003-9A.
           05 RUEC90-COD-EMPNMNT      PIC  X(014) VALUE SPACES.
STE001     05 RUEC90-COD-SIST-PRODC   PIC  X(013) VALUE SPACES.
           05 RUEC90-VLR-PARCL-CRED   PIC  X(019) VALUE SPACES.
           05 RUEC90-VLR-PARCL-REC-PROP PIC  X(019) VALUE SPACES.
           05 RUEC90-VLR-PARCL-SERVICOS PIC  X(019) VALUE SPACES.
           05 RUEC90-ALQ-PROAGRO      PIC  X(005) VALUE SPACES.
           05 RUEC90-PERC-JUROS-ENCARFIN     PIC S9(003)V99 VALUE ZEROS.
           05 RUEC90-TP-ENCARG-FIN-COMPL PIC  9(002) VALUE ZEROS.
           05 RUEC90-TP-ENCARG-FIN-COMPL-X REDEFINES
              RUEC90-TP-ENCARG-FIN-COMPL PIC  X(002).
           05 RUEC90-PERC-RSC-STN        PIC  X(005) VALUE SPACES.
           05 RUEC90-PERC-RSC-STN-S5     REDEFINES
              RUEC90-PERC-RSC-STN        PIC S9(005).
           05 RUEC90-PERC-RSC-FUND-CONSCL PIC X(005) VALUE SPACES.
           05 RUEC90-PERC-RSC-FUND-CONSCL-S5 REDEFINES
              RUEC90-PERC-RSC-FUND-CONSCL  PIC S9(005).
           05 RUEC90-PERC-CST-EFT-TOTAL PIC  X(005) VALUE SPACES.
           05 RUEC90-PERC-CST-EFT-TOTAL-N  REDEFINES
              RUEC90-PERC-CST-EFT-TOTAL    PIC  9(003)V99.
           05 RUEC90-AREA-X             PIC  X(011) VALUE SPACES.
           05 RUEC90-AREA-N REDEFINES RUEC90-AREA-X
                                       PIC  9(09)V99.
           05 RUEC90-QTD-ITEM-FINCD-N  PIC  9(012)V99 VALUE ZEROS.
           05 RUEC90-QTD-ITEM-FINCD-X REDEFINES
              RUEC90-QTD-ITEM-FINCD-N PIC  X(14).
           05 RUEC90-QTD-PRV-PRODC    PIC  9(012)V99 VALUE ZEROS.
           05 RUEC90-QTD-PRV-PRODC-X REDEFINES
              RUEC90-QTD-PRV-PRODC   PIC  X(14).
      *
      **** TAMANHO = 116 - <GRUPO_COR0003R1_PROPT>
      *
       01  RUEC90-COR0003-10-F.
           05 RUEC90-GRUPO-COR0003-PROP-F.
              10 FILLER              PIC  X(006) VALUE '{1240}'.
              10 RUEC90-GP-COR0003-PROP-F.
                 15 RUEC90-TP-PESSOA-PROPT-F
                                     PIC  X(001) VALUE SPACES.
                 15 RUEC90-CPFPROPT  PIC  X(011) VALUE SPACES.
      ***        NIRF
STEFA1           15 RUEC90-NIRF-F    PIC  X(008) VALUE SPACES.
      ***        CODSNCR
                 15 RUEC90-CCIR-F    PIC  X(013) VALUE SPACES.
      ***        NUMREGCAR
                 15 RUEC90-NRO-CAR-F PIC  X(041) VALUE SPACES.
      ***        NUMOUTGAGUAPROPR
                 15 RUEC90-NRO-OUT-F PIC  X(030) VALUE SPACES.
      ***        PERCPRESCPROPR
                 15 RUEC90-PERC-PRESC-F  PIC X(005) VALUE SPACES.
                 15 RUEC90-PERC-PRESC-F-N REDEFINES
                    RUEC90-PERC-PRESC-F  PIC 9(003)V99.
      *
      **** TAMANHO = 116 - <GRUPO_COR0003R1_PROPT>
      *
       01  RUEC90-COR0003-10-J.
           05 RUEC90-GRUPO-COR0003-PROP-J.
              10 FILLER            PIC  X(006) VALUE '{1240}'.
              10 RUEC90-GP-COR0003-PROP-J.
                 15 RUEC90-TP-PESSOA-PROPT-J
                                   PIC  X(001) VALUE SPACES.
                 15 RUEC90-CNPJBASE-PROPT
                                   PIC  X(008) VALUE SPACES.
                 15 FILLER         PIC  X(003) VALUE SPACES.
      ***        NIRF
STEFA1           15 RUEC90-NIRF-J    PIC  X(008) VALUE SPACES.
      ***        CODSNCR
                 15 RUEC90-CCIR-J    PIC  X(013) VALUE SPACES.
      ***        NUMREGCAR
                 15 RUEC90-NRO-CAR-J PIC  X(041) VALUE SPACES.
      ***        NUMOUTGAGUAPROPR
                 15 RUEC90-NRO-OUT-J PIC  X(030) VALUE SPACES.
      ***        PERCPRESCPROPR
                 15 RUEC90-PERC-PRESC-J  PIC X(005) VALUE SPACES.
                 15 RUEC90-PERC-PRESC-J-N REDEFINES
                    RUEC90-PERC-PRESC-J  PIC 9(003)V99.
      *
      **** TAMANHO = 58 - <GRUPO_COR0003R1_PROAGROMAIS>
      *
       01  RUEC90-COR0003-11.
           05 RUEC90-CODMODSEGR             PIC  9(001) VALUE ZEROS.
           05 RUEC90-CNPJBASE-INST-MUTNTE   PIC  X(008) VALUE SPACES.
           05 RUEC90-NUMREF-BCCOR-INVESTMT  PIC  X(011) VALUE SPACES.
           05 RUEC90-VLR-PARCL-INVESTMT     PIC  X(019) VALUE SPACES.
           05 RUEC90-VLR-RECBRUTA-ESPRDEMP  PIC  X(019) VALUE SPACES.
      *
      **** TAMANHO = 36  - <GRUPO_COR0003R1_PARCL>
      *
       01  RUEC90-COR0003-12.
           05 RUEC90-GRUPO-COR0003-PARCL.
              10 FILLER                  PIC  X(006) VALUE '{1250}'.
              10 RUEC90-GP-COR0003-PARCL.
                 15 RUEC90-NUM-PARCL       PIC  9(003) VALUE ZEROS.
                 15 RUEC90-DT-PRVPGTO      PIC  9(008) VALUE ZEROS.
                 15 RUEC90-VLR-PRINC-PARCL PIC  X(019) VALUE SPACES.
      *
      **** TAMANHO = 04
      *
       01  RUEC90-COR0003-13.
           05 RUEC90-CODSUB-PROG         PIC  X(004) VALUE SPACES.
      *
      **** TAMANHO = 33  - <GRUPO_COR0003R1_LIBRC>
      *
       01  RUEC90-COR0003-14.
           05 RUEC90-GRUPO-COR0003-LIBRC.
              10 FILLER                  PIC  X(006) VALUE '{1260}'.
              10 RUEC90-GP-COR0003-LIBRC.
                 15 RUEC90-DT-LIBRC      PIC  9(008) VALUE ZEROS.
                 15 RUEC90-VLR-REC-LIBRC PIC  X(019) VALUE SPACES.
      *
      **** TAMANHO = 51
      *
       01  RUEC90-COR0003-15.
           05 RUEC90-VLRPRODDDOBTD          PIC  X(019) VALUE SPACES.
           05 RUEC90-DT-INI-PLANT           PIC  X(008) VALUE SPACES.
           05 RUEC90-DT-FIM-PLANT           PIC  X(008) VALUE SPACES.
           05 RUEC90-DT-INI-COLHT           PIC  X(008) VALUE SPACES.
           05 RUEC90-DT-FIM-COLHT           PIC  X(008) VALUE SPACES.
      *
      **** TAMANHO = 36 - <GRUPO_COR0003R1_DESCLASS>
      *
       01 RUEC90-COR0003-16.
          05 RUEC90-GRUPO-COR0003-DESCLASS.
             10 FILLER                      PIC  X(006) VALUE '{1270}'.
             10 RUEC90-GP-COR0003-DESCLASS.
                15 RUEC90-DTDESCLASSEMPNMNT PIC  9(008) VALUE ZEROS.
                15 RUEC90-VLR-DESCLASCD     PIC S9(017)V9(002)
                                                        VALUE ZEROS.
                15 RUEC90-MOTV-DESCLASSCOR  PIC  9(003) VALUE ZEROS.
      *
      **** TAMANHO = 44 - <GRUPO_COR0003R1_COOPERD>
      *
       01 RUEC90-COR0003-17.
          05 RUEC90-GRUPO-COR0003-COOPERD.
             10 FILLER                      PIC  X(006) VALUE '{1280}'.
             10 RUEC90-GP-COR0003-COOPERD.
                15 RUEC90-TPPESSOA-COOPERD  PIC  X(001) VALUE SPACES.
                15 RUEC90-CNPJ-CPF-COOPERD  PIC  9(014) VALUE ZEROS.
                15 RUEC90-VLRPARCL-COOPERD  PIC S9(017)V9(002)
                                                        VALUE ZEROS.
                15 RUEC90-CODPROG-COOPERD   PIC  9(004) VALUE ZEROS.
      *
      **** TAMANHO = 36 - <GRUPO_COR0003R1_REFDESCLASCDPARCLMNT>
      *
       01 RUEC90-COR0003-18.
          05 RUEC90-GRUPO-COR0003-DESCPAR.
             10 FILLER                      PIC  X(006) VALUE '{1290}'.
             10 RUEC90-GP-COR0003-DESCPAR.
                15 RUEC90-NUMREF-DESCPAR    PIC  X(011) VALUE SPACES.
                15 RUEC90-VLR-DESCPAR       PIC S9(017)V9(002)
                                                        VALUE ZEROS.
      *
      **** TAMANHO = 13
      *
       01  RUEC90-COR0003-19.
           05 RUEC90-COD-STN-EQULZC      PIC  X(013) VALUE SPACES.
      *
      **** TAMANHO = 335
      *
       01 RUEC90-COR0003-20.
          05 RUEC90-GRUPO-COR0003-CERTIFCOR.
             10 RUEC90-GP-COR0003-CERTIFCOR.
                15 RUEC90-CODCERTIFCORGNCOEMPNMNT
                                            PIC  X(080) VALUE SPACES.
                15 RUEC90-URLCERTIFCORGNCOEMPNMNT
                                            PIC  X(255) VALUE SPACES.
      *
      **** TAMANHO = 335
      *
       01 RUEC90-COR0003-21.
          05 RUEC90-GRUPO-COR0003-RASTLDDEM.
             10 RUEC90-GP-COR0003-RASTLDDEM.
                15 RUEC90-CODRASTLDDEMPNMNT
                                            PIC  X(080) VALUE SPACES.
                15 RUEC90-URLCODRASTLDDEMPNMNT
                                            PIC  X(255) VALUE SPACES.
      *
      **** TAMANHO = 06
      *
       01  RUEC90-COR0003-22.
           05 RUEC90-PERCENERRENOVVLEMPNMNT PIC  9(03)V99 VALUE ZEROS.
           05 RUEC90-INDRSUSTLDD            PIC  X(01)    VALUE SPACES.
      *
WP1021 01  RUEC90-COR0003-23.
WP1021     05 RUEC90-CICLO-CULTURA          PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUEC90-TIPO-SOLO              PIC  X(002)
WP1021                                      VALUE ZEROS.
WP1021     05 RUEC90-TIPO-MANEJO            PIC  X(002)
WP1021                                      VALUE ZEROS.
      *
      **** TAMANHO = 11
STE001 01  RUEC90-COR0003-23A.
.          05 RUEC90-PC-BONUSCAR           PIC  9(008) VALUE ZEROS.
.          05 RUEC90-INDR-CONFDD-IBAMA     PIC  X(001) VALUE SPACES.
.          05 RUEC90-INDR-CONFDD-UC        PIC  X(001) VALUE SPACES.
STE001     05 RUEC90-INDR-CONFDD-FL-PUB    PIC  X(001) VALUE SPACES.
      *
      **** TAMANHO = 14
      *
WP1021 01  RUEC90-COR0003-24.
           05 RUEC90-DTHRBC                 PIC  9(014) VALUE ZEROS.
      *
      **** TAMANHO = 09
WIP003 01  RUEC90-COR0003-25.
           05 RUEC90-DT-DESCLASS            PIC  X(008) VALUE SPACES.
           05 RUEC90-MTV-DESCLASS           PIC  X(003) VALUE SPACES.
      *
      **** TAMANHO = 08
STE001 01  RUEC90-COR0003-21A.
STE001     05 RUEC90-PERC-BONUS-CAR         PIC S9(003)V99
STE001                                      VALUE ZEROS.
STE001     05 RUEC90-IND-RCONFDD-IBAMA      PIC  X(001)
STE001                                      VALUE SPACES.
STE001     05 RUEC90-IND-RCONFDD-UC         PIC  X(001)
STE001                                      VALUE SPACES.
STE001     05 RUEC90-IND-RCONFDD-FLORPUBL   PIC  X(001)
STE001                                      VALUE SPACES.
STEFA1     05 RUEC90-IND-RCONFDD-IM-RURAL   PIC  X(001)
STEFA1                                      VALUE SPACES.
STEFA1     05 RUEC90-TP-CONFDD-OTR          PIC  X(002)
STEFA1                                      VALUE SPACES.
STEFA1     05 RUEC90-CNPJ-BASE-EXIGLDD      PIC  X(008)
STEFA1                                      VALUE SPACES.
      **** TAMANHO = 08
      *
WIP003 01  RUEC90-COR0003-26.
           05 RUEC90-DTMOVTO                PIC  9(008) VALUE ZEROS.
