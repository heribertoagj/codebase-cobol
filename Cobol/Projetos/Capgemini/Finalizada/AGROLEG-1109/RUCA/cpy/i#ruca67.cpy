      *-- I#RUCA67 ----------------------------------------------------*
      *                                                                *
      * DESC: AREA PARA FORMATACAO DA MENSAGERIA COR0005 - SICOR WEB   *
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
WP0522*----------------------------------------------------------------*
WP0522*               U L T I M A  A L T E R A C A O                   *
WP0522*----------------------------------------------------------------*
WP0522*                                                                *
WP0522*      ANALISTA     : WIPRO                                      *
WP0522*      DATA         : MAIO/2022                                  *
WP0522*                                                                *
WP0522*      OBJETIVOS    : INCLUIR NOVOS CAMPOS DO CATALOGO SICOR     *
WP0522*                     V5.04                                      *
WP0522*                                                                *
WP0522*      REFERENCIA   : WP0522                                     *
WP0522*                                                                *
WP0522*================================================================*
WP1122* ALTERACAO...........: ALTERACAO CAMPOS SICOR 5.05              *
.     *                       INCLUSAO DO GRUPO DE DESCLASSIFICACAO    *
.     * RESPONSAVEL.........: WIPRO - CLAUDIO PIAZZA                   *
WP1122* GERACAO.............: 11/2022                                  *
WP1122*================================================================*
NAV506* ALTERACAO...........: ALTERACAO CAMPOS SICOR 5.06              *
NAV506*                       INCLUSAO GRUPO DESCLASSIFICACAO PARCIAL  *
NAV506* RESPONSAVEL.........: NAVA                                     *
NAV506* GERACAO.............: 05/2023                                  *
NAV506*================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
7C2510*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.11                                   *
.     *    PESQUISA....:  7COMM                                        *
7C2510*================================================================*
       01  RUCA67-COR0005-1.
      **** 01 <CODMSG>                      [1..1]
           05 RUCA67-CODMSG                 PIC  X(009) VALUE SPACES.
      **** 02 <NUMCTRLIF>                   [1..1]
           05 RUCA67-NUMCTRLIF              PIC  X(020) VALUE SPACES.
      **** 03 <CNPJENTRESPONS>              [1..1]
7C5.10     05 RUCA67-CNPJ-ENT-RESPONS       PIC  X(014) VALUE SPACES.
      **** 04 <NUMREFBCCOROR>               [1..1]
           05 RUCA67-NUMREF-BCCOROR         PIC  X(011) VALUE SPACES.
      **** 05 <DTVENC>                      [0..1]
           05 RUCA67-DTVENC                 PIC  9(008) VALUE ZEROS.
           05 RUCA67-DTVENC-X               REDEFINES
              RUCA67-DTVENC                 PIC  X(008).
RE1117**** 06 <CODBASELEGALPRORROGC>        [0..1]
RE1117     05 RUCA67-CBASE-LEGAL-PRO        PIC  X(003) VALUE SPACES.
BR4.12**** 07 <TPINSTNTOCRED>               [0..1]
BR4.12     05 RUCA67-TP-INSTNTO-CRED        PIC  X(002) VALUE SPACES.
BRQ001**** 07 <INDRCONFCALERTSUSTLDD>       [0..1]
BRQ001     05 RUCA67-IND-CONF-ALRT-SUST     PIC  X(001) VALUE SPACES.

       01  RUCA67-COR0005-2.
      **** 08 <GRUPO_COR0005_DESTCFINCMNT>  [0..50]
           05 RUCA67-GRUPO-COR0005-DESTCFIN.
              10 FILLER                     PIC  X(006) VALUE '{1000}'.

       01  RUCA67-COR0005-3.
      **** 09 <NUMORDEMDESTCOR>             [1..1]
           05 RUCA67-NRO-ORDEM-DEST         PIC  X(003) VALUE SPACES.
      **** 10 <VLRPRODDDOBTD>               [0..1]
           05 RUCA67-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.

       01  RUCA67-COR0005-4.
      **** 11 <GRUPO_COR0005_LIBRC>         [0..30]
           05 RUCA67-GRUPO-COR0005-LIBRC.
              10 FILLER                     PIC  X(006) VALUE '{1100}'.
              10 RUCA67-GP-COR0005-LIBRC.
      **** 12    <DTLIBRCRECEMPNMNT>        [1..1]
                 15 RUCA67-DTLIBRC-RECUR    PIC  X(008) VALUE SPACES.
      **** 13    <VLRRECLIBRC>              [1..1]
                 15 RUCA67-VLR-LIBRC-RECUR  PIC  X(019) VALUE SPACES.

BRQ001*01  RUCA67-COR0005-5.
BRQ001**** 14 <GRUPO_COR0005_DESCLASS>      [0..10]
BRQ001*    05 RUCA67-GRUPO-COR0005-DESCLASS.
BRQ001*       10 FILLER                     PIC  X(006) VALUE '{1200}'.
BRQ001*       10 RUCA67-GP-COR0005-DESCLASS.
BRQ001**** 15    <DTDESCLASSEMPNMNT>        [1..1]
BRQ001*          15 RUCA67-DT-DESCLASS      PIC  X(008) VALUE SPACES.
BRQ001**** 16    <VLRDESCLASS>              [1..1]
BRQ001*          15 RUCA67-VLR-DESCLASS     PIC  X(019) VALUE SPACES.
BRQ001**** 17    <MOTVDESCLASSCOR>          [1..1]
BRQ001*          15 RUCA67-MTV-DESCLASS     PIC  X(003) VALUE SPACES.
V5.01 **** 18    <PERCDESCLASS>             [1..1]
V5.01 *          15 RUCA67-PER-DESCLASS     PIC  X(005) VALUE SPACES.
V5.01 *          15 RUCA67-PER-DESCLASS-S5  REDEFINES
V5.01 *             RUCA67-PER-DESCLASS     PIC S9(005).

       01  RUCA67-COR0005-6.
      **** 19 <GRUPO_COR0005_COOPERD>       [0..N]
           05 RUCA67-GRUPO-COR0005-COOP.
              10 FILLER                     PIC  X(006) VALUE '{1200}'.
              10 RUCA67-GP-COR0005-COOP.
      **** 20    <TPPESSOACOOPERD>          [1..1]
                 15 RUCA67-TPSSOA-COOP      PIC  X(001) VALUE SPACES.
      **** 21    <CNPJ_CPFCOOPERD>          [1..1]
7C5.10           15 RUCA67-CNPJ-CPF-COOP    PIC  X(014) VALUE SPACES.
      **** 22    <VLRPARCLCOOPERD>          [1..1]
                 15 RUCA67-VLR-PCELA-COOP   PIC  X(019) VALUE SPACES.
      **** 23    <CODPROG_LINHACREDCOOPERD> [1..1]
                 15 RUCA67-CODPROG-LINCRED  PIC  X(004) VALUE SPACES.

4.13   01  RUCA67-COR0005-6A.
4.13  ***  24    <TPFNTEREC>                [0..1]
4.13       05 RUCA67-TP-FNTE-REC            PIC  X(004) VALUE SPACES.

BR4.11 01  RUCA67-COR0005-7.
BR4.11**** 25 <GRUPO_COR0005_GLEBA>         [0..N]
BR4.11     05 RUCA67-GRUPO-COR0005-GLEBA.
BR4.11        10 FILLER                     PIC  X(006) VALUE '{1300}'.
BR4.11        10 RUCA67-GP-COR0005-GLEBA.
BR4.11**** 26 <IDENTCGLEBA>                 [1..1]
BR4.11           15 RUCA67-IDENTC-GLEBA     PIC  X(019) VALUE SPACES.
7C2510           15 RUCA67-AREA-NAO-CULT    PIC  X(011) VALUE SPACES.
BR4.11
7C2510 01  RUCA67-COR0005-8.
7C2510     05 FILLER                        PIC  X(006) VALUE '{1310}'.
7C2510     05 RUCA67-WKTPART                PIC  X(10000) VALUE SPACES.
      *
      **** TAMANHO = 39
      *
7C2510*
7C2510*01  RUCA67-COR0005-8.
7C2510**** 27 <GRUPO_COR0005_PONTOPOLG>     [1..N]
7C2510*    05 RUCA67-GRUPO-COR0005-PTPOLG.
7C2510*       10 FILLER                     PIC  X(006) VALUE '{1310}'.
7C2510*       10 RUCA67-GP-COR0005-PTPOLG.
7C2510**** 28 <LATPONTO>                    [1..1]
7C2510*          15 RUCA67-LAT-PONTO        PIC  X(013) VALUE SPACES.
7C2510**** 29 <LONGPONTO>                   [1..1]
7C2510*          15 RUCA67-LONG-PONTO       PIC  X(014) VALUE SPACES.
7C2510**** 30 <ALTTPONTO>                   [1..1]
7C2510*          15 RUCA67-ALTT-PONTO       PIC  X(006) VALUE SPACES.
      *
      **** TAMANHO = 11
      *
7C2510*01  RUCA67-COR0005-8A.
7C2510**** 31 <AREANCULTD>                  [0..1]
7C2510*    05 RUCA67-AREA-NAO-CULT          PIC  X(011) VALUE SPACES.
BR4.11**** 05 RUCA67-COD-MUN-GLEB  PIC  X(006) VALUE SPACES.
      *
7C2510*
7C2510*01  RUCA67-COR0005-8B.
7C2510*    05 RUCA67-GRUPO-COR0005-GLEBA-E.
7C2510*       10 FILLER                     PIC  X(006) VALUE '{1320}'.
7C2510*
7C2510*01  RUCA67-COR0005-8BB.
7C2510*    05 RUCA67-GLEBA-EXCLUIDA.
7C2510*       10 RUCA67-COR0001-PTPLGEXC.
7C2510*          15 FILLER               PIC  X(006) VALUE '{1321}'.
7C2510*          15 RUCA67-LAT-PONTO-EXCL
7C2510*                                  PIC  X(013) VALUE SPACES.
7C2510*          15 RUCA67-LONG-PONTO-EXCL
7C2510*                                  PIC  X(014) VALUE SPACES.
7C2510*          15 RUCA67-ALTT-PONTO-EXCL
7C2510*                                  PIC  X(006) VALUE SPACES.

BR4.11 01  RUCA67-COR0005-9.
BR4.11**** 32 <PERCJUROSENCARGOFINANCPOSFIX>[0..1]
BR4.11     05 RUCA67-PERC-POSFIX            PIC  X(009) VALUE SPACES.
POS        05 RUCA67-PERC-POSFIX-S9         REDEFINES
POS           RUCA67-PERC-POSFIX            PIC S9(009).
BR4.11**** 33 <TPENCARGOFINANCCOMPLTAR>     [1..1]
BR4.11     05 RUCA67-TPENCARGO-COMPL        PIC  X(002) VALUE SPACES.
RL4.14**** 34-<AREA>-                      [0..1]
RL4.14     05 RUCA67-AREA-X                 PIC  X(011) VALUE SPACES.
RL4.14     05 RUCA67-AREA-N REDEFINES RUCA67-AREA-X
RL4.14                                      PIC  9(09)V99.
RL4.14**** 35 <QTDITEMFINCD>-              [0..1]-
RL4.14     05 RUCA67-QTD-ITEM-FINCD-N       PIC  9(012)V99 VALUE ZEROS.
RL4.14     05 RUCA67-QTD-ITEM-FINCD-X REDEFINES RUCA67-QTD-ITEM-FINCD-N
RL4.14                                      PIC  X(14).
RL4.14**** 36 <PERCCSTEFTTOT>--            [0..1]-
RL4.14     05 RUCA67-PERC-CST-EFT-TOTAL     PIC  X(005) VALUE SPACES.
RL4.14     05 RUCA67-PERC-CST-EFT-TOTAL-N   REDEFINES
RL4.14        RUCA67-PERC-CST-EFT-TOTAL     PIC  9(003)V99.
      *
BRQ001 01  RUCA67-COR0005-10.
.     **** 37 <Grupo_COR0005_Propt>         [0..n]
.          05 RUCA67-GRUPO-COR0005-PROPT.
.             10 FILLER                     PIC  X(006) VALUE '{1400}'.
.             10 RUCA67-GP-COR0005-PROPT.
.     **** 38    <TpPessoaPropt>            [1..1]
.                15 RUCA67-TPSSOA-PROPT     PIC  X(001) VALUE SPACES.
.     **** 39    <CNPJBase_CPFPropt>        [1..1]
.                15 RUCA67-CNPJ-CPF-PROPT   PIC  X(011) VALUE SPACES.
.     **** 40    <CIB>                      [0..1]
7C5.10           15 RUCA67-CIB              PIC  X(008) VALUE SPACES.
.     **** 41    <CodSNCR>                  [0..1]
.                15 RUCA67-COD-SNCR         PIC  X(013) VALUE SPACES.
.     **** 42    <NumRegCAR>                [0..1]
.                15 RUCA67-NUM-REG-CAR      PIC  X(041) VALUE SPACES.
.     **** 43    <NumOutgAguaPropr>         [0..1]
.                15 RUCA67-COUTGA-AGUA      PIC  X(030) VALUE SPACES.
.     **** 44    <PercPrescPropr>           [0..1]
.                15 RUCA67-PPRESV-AMBTL     PIC  X(005) VALUE SPACES.
.                15 RUCA67-PPRESV-AMBTL-N   REDEFINES
.                   RUCA67-PPRESV-AMBTL     PIC  9(003)V99.
.     *
.      01  RUCA67-COR0005-11.
.     **** 46 <Grupo_COR0005_CertificOrgnco>  [0..1]
.          05 RUCA67-GRUPO-COR0005-CORGN.
.             10 RUCA67-GP-COR0005-CORGN.
.     **** 47    <CodCertifcOrgncoEmpnmnt>    [1..1]
.                15 RUCA67-CCETFC-ORGNC     PIC  X(080) VALUE SPACES.
.     **** 48    <URLCodCertifcOrgncoEmpnmnt> [1..1]
.                15 RUCA67-EURL-CETFC-ORGNC PIC  X(255) VALUE SPACES.
.     *
.      01  RUCA67-COR0005-12.
.     **** 50 <Grupo_COR0005_RastlddEmpnmnt>  [0..1]
.          05 RUCA67-GRUPO-COR0005-RASTB.
.             10 RUCA67-GP-COR0005-RASTB.
.     **** 51    <CodRastlddEmpnmnt>          [1..1]
.                15 RUCA67-CRSTRB-EMPTO     PIC  X(080) VALUE SPACES.
.     **** 52    <URLCodRastlddEmpnmnt>       [1..1]
.                15 RUCA67-EURL-RSTRB       PIC  X(255) VALUE SPACES.
.     *
.      01  RUCA67-COR0005-13.
.     **** 54 <PercEnerRenovvlEmpnmnt>        [0..1]
.          05 RUCA67-PENERG-RENOV           PIC  X(005) VALUE SPACES.
.          05 RUCA67-PENERG-RENOV-N         REDEFINES
BRQ001        RUCA67-PENERG-RENOV           PIC  9(003)V99.
WP0522
.      01  RUCA67-COR0005-14.
.     **** 55 <CodSTNEqulzc>                  [0..1]
.          05 RUCA67-COD-STN-EQULZC         PIC  X(013) VALUE SPACES.
WP1122
-      01  RUCA67-COR0005-15.
-     **** 56 <Grupo_COR0005_Desclass>      [0..10]
-          05 RUCA67-GRUPO-COR0005-DESCLASS.
-             10 RUCA67-GP-COR0005-DESCLASS.
-     **** 57    <DtDesclassEmpnmnt>        [1..1]
-                15 RUCA67-DT-DESCLASS      PIC  X(008) VALUE SPACES.
-     **** 58    <MotvDesclassCOR>          [1..1]
-                15 RUCA67-MTV-DESCLASS     PIC  X(003) VALUE SPACES.
WP1122
NAV506 01  RUCA67-COR0005-16.
-     **** 60 <Grupo_COR0005_DesclassParcl> [0..n]
-          05 RUCA67-GRUPO-COR0005-DESC-PARC.
-             10 FILLER                     PIC  X(006) VALUE '{1500}'.
-             10 RUCA67-GP-COR0005-DESCL-PARC.
-     **** 61    <DtDesclassParclEmpnmnt>   [1..1]
-                15 RUCA67-DT-DESCLASS-PAR  PIC  X(008) VALUE SPACES.
-     **** 62    <VlrDesclascd>             [1..1]
-                15 RUCA67-VLR-DESCLASS-PAR PIC  X(019) VALUE SPACES.
-     **** 63    <MotvDesclassCOR>          [1..1]
-                15 RUCA67-MTV-DESCLASS-PAR PIC  X(003) VALUE SPACES.
-
7COMMP 01  RUCA67-COR0005-DEST.
7COMMP     05 RUCA67-COD-PROGRAMA           PIC  X(004) VALUE SPACES.
7COMMP     05 RUCA67-SUB-PROGRAMA           PIC  X(004) VALUE SPACES.

STE001 01  RUCA67-COR0005-JUROS.
STE001     05 RUCA67-PERCJUROSENCARGOFINANC PIC  X(005) VALUE SPACES.
STE001     05 RUCA67-PERCBONUSCAR           PIC  X(005) VALUE SPACES.
-     *
STEFAB 01  RUCAC5-COR0005-16A.
-     **** 76 <Grupo_COR0005_Parcl> [0..n]
-          05 RUCAC5-GRUPO-COR0005-PARCL.
-             10 FILLER                     PIC  X(006) VALUE '{1600}'.
-             10 RUCAC5-GP-COR0005-PARCL.
-     **** 77    <NumParcl>                 [1..1]
-                15 RUCAC5-NUM-PARCL-X      PIC  X(003) VALUE SPACES.
-                15 RUCAC5-NUM-PARCL-N      REDEFINES RUCAC5-NUM-PARCL-X
-                                           PIC  9(003).
-     **** 78    <DtPrvPgto>                [1..1]
-                15 RUCAC5-DT-PRV-PAGTO     PIC  X(008) VALUE SPACES.
-     **** 79    <VlrPrincipalParcL>        [1..1]
-                15 RUCAC5-VLR-PRINC-X      PIC  X(019) VALUE SPACES.
-                15 RUCAC5-VLR-PRINC-N      REDEFINES RUCAC5-VLR-PRINC-X
-                                           PIC  9(017)V99.
-     *
-      01  RUCAC5-COR0005-CNPJ.
STEFAB     05 RUCAC5-CNPJ-BASE-INST-EXIGIB  PIC  X(008) VALUE SPACES.
7C5.10 01  RUCA67-COR0005-BONUS-SUST.
.          05 RUCA67-PERC-BONUS-SUST        PIC  9(003)V99 VALUE ZEROS.
.
.      01  RUCA67-COR0005-16B.
.          05 RUCA67-GRUPO-COR0005-CONFDD.
.             10 FILLER                     PIC X(006) VALUE '{1700}'.
.             10 RUCA67-GP-COR0005-CONFDD.
.                15 RUCA67-TP-CONFDD-X      PIC X(002) VALUE SPACES.
.                15 RUCA67-TP-CONFDD REDEFINES RUCA67-TP-CONFDD-X
7C5.10                                      PIC 9(002).
-     *
NAV506 01  RUCA67-COR0005-17.
           05 RUCA67-DTMOVTO                PIC  9(008) VALUE ZEROS.
