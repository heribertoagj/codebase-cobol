      *---------------------------------------------------------------*
      *              B S I   T E C N O L O G I A                      *
      *---------------------------------------------------------------*
      * INCLUDE.............: I#RUECC5 - PARA MODULO RCOR5005(COR0001)*
      * GERACAO.............: JUNHO/2017                              *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO PARA FORMATACAO/REFORMATACAO*
      *               PARA ENVIO DOS DADOS PARA RECOR                 *
      *               **** MENSAGEM COR0005 ***                       *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
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
BR0521*                    ALTERACAO - BRQ DIGITAL SOLUTIONS           *
BR0521*----------------------------------------------------------------*
BR0521*  ANALISTA BRQ...:  SIMONE                                      *
BR0521*  DATA...........:  13/06/2021                                  *
BR0521*----------------------------------------------------------------*
BR0521*  OBJETIVO......:   ADEQUAR PARA SICOR 5.02                     *
BR0521*================================================================*
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
NAV506*                    ALTERACAO - NAVA                            *
NAV506*----------------------------------------------------------------*
NAV506*  ANALISTA ......:  NAVA                                        *
NAV506*  DATA...........:  11/05/2023                                  *
NAV506*----------------------------------------------------------------*
NAV506*  OBJETIVO......:   ADEQUAR PARA SICOR 5.06                     *
NAV506*================================================================*
7COMMP*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  PRISCILA PEREIRA                             *
.     *    DATA........:  SETEMBRO/2023                                *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.07                                   *
.     *    PESQUISA....:  7COMMP                                       *
7COMMP*================================================================*
STE001*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  STEFANINI                                    *
.     *    DATA........:  MAIO / 2024                                  *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.08                                   *
.     *    PESQUISA....:  STE001                                       *
STE001*================================================================*
STEFAB*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  STEFANINI                                    *
.     *    DATA........:  OUTUBRO/2024                                 *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  SICOR 5.09                                   *
.     *    PESQUISA....:  STEFAB                                       *
STEFAB*================================================================*
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
211025*    PESQUISA....:  211025                                       *
      *================================================================*
CAPG05*================================================================*
CAPG05*                   A L T E R A C A O                            *
.     *----------------------------------------------------------------*
.     *    ANALISTA....:  7COMM - SQUAD FLASH                          *
.     *    DATA........:  MAIO/2026                                    *
.     *----------------------------------------------------------------*
.     *    OBJETIVO....:  AGROLEG-1239 - SICOR 5.12                   *
.     *                   EXCLUIR CAMPOS DESCONTINUADOS DA COR0005    *
CAPG05*    PESQUISA....:  CAPG05                                       *
      *================================================================*

       01  RUECC5-COR0005-1.
      **** 01 <CodMsg>                      [1..1]
           05 RUECC5-CODMSG                 PIC  X(009) VALUE SPACES.
      **** 02 <NumCtrlIF>                   [1..1]
           05 RUECC5-NUMCTRLIF              PIC  X(020) VALUE SPACES.
      **** 03 <CNPJEntRespons>              [1..1]
050825     05 RUECC5-CNPJ-ENT-RESPONS       PIC  X(014) VALUE SPACES.
      **** 04 <NumRefBCCOROr>               [1..1]
           05 RUECC5-NUMREF-BCCOROR         PIC  X(011) VALUE SPACES.
      **** 05 <DtVenc>                      [0..1]
           05 RUECC5-DTVENC                 PIC  9(008) VALUE ZEROS.
RE1117**** 06 <CodBaseLegalProrrogc>        [0..1]
RE1117     05 RUECC5-CBASE-LEGAL-PRO        PIC  X(003) VALUE SPACES.
BR4.12**** 07 <TpInstntoCred>               [0..1]
BR4.12     05 RUECC5-TP-INSTNTO-CRED        PIC  X(002) VALUE SPACES.
CAPG05**   CAMPOS EXCLUIDOS - SICOR 5.12
CAPG05**** 08 <IndrConfcAlertSustldd>       [0..1]
CAPG05*    -05 RUECC5-IND-CONF-ALRT-SUST     PIC  X(001) VALUE SPACES.

       01  RUECC5-COR0005-2.
211025**** 09 <Grupo_COR0005_DestcFincmnt>  [0..60]
           05 RUECC5-GRUPO-COR0005-DESTCFIN.
              10 FILLER                     PIC  X(006) VALUE '{1000}'.

       01  RUECC5-COR0005-3.
      **** 10 <NumOrdemDestcOr>             [1..1]
           05 RUECC5-NRO-ORDEM-DEST         PIC  X(003) VALUE SPACES.
      **** 11 <VlrProdddObtd>               [0..1]
           05 RUECC5-VLR-PROD-OBTD          PIC  X(019) VALUE SPACES.

       01  RUECC5-COR0005-4.
211025**** 12 <Grupo_COR0005_Librc>         [0..55]
           05 RUECC5-GRUPO-COR0005-LIBRC.
              10 FILLER                     PIC  X(006) VALUE '{1100}'.
              10 RUECC5-GP-COR0005-LIBRC.
      **** 13    <DtLibrcRecEmpnmnt>        [1..1]
                 15 RUECC5-DTLIBRC-RECUR    PIC  X(008) VALUE SPACES.
      **** 14    <VlrRecLibrc>              [1..1]
                 15 RUECC5-VLR-LIBRC-RECUR  PIC  X(019) VALUE SPACES.

BR0521*01  RUECC5-COR0005-5.
BR0521**** 14 <Grupo_COR0005_Desclass>      [0..10]
BR0521*    05 RUECC5-GRUPO-COR0005-DESCLASS.
BR0521*       10 FILLER                     PIC  X(006) VALUE '{1200}'.
BR0521*       10 RUECC5-GP-COR0005-DESCLASS.
BR0521**** 15    <DtDesclassEmpnmnt>        [1..1]
BR0521*          15 RUECC5-DT-DESCLASS      PIC  X(008) VALUE SPACES.
BR0521**** 16    <VlrDesclass>              [1..1]
BR0521*          15 RUECC5-VLR-DESCLASS     PIC  X(019) VALUE SPACES.
BR0521**** 17    <MotvDesclassCOR>          [1..1]
BR0521*          15 RUECC5-MTV-DESCLASS     PIC  X(003) VALUE SPACES.
QT1120**** 18    <PERCDESCLASS>             [1..1]
QT1120*          15 RUECC5-PER-DESCLASS     PIC  X(005) VALUE SPACES.
QT1120*          15 RUECC5-PER-DESCLASS-S5  REDEFINES
QT1120*             RUECC5-PER-DESCLASS     PIC S9(005).

       01  RUECC5-COR0005-6.
      **** 16 <Grupo_COR0005_Cooperd>       [0..n]
           05 RUECC5-GRUPO-COR0005-COOP.
              10 FILLER                     PIC  X(006) VALUE '{1200}'.
              10 RUECC5-GP-COR0005-COOP.
      **** 17    <TpPessoaCooperd>          [1..1]
                 15 RUECC5-TPSSOA-COOP      PIC  X(001) VALUE SPACES.
      **** 18    <CNPJ_CPFCooperd>          [1..1]
BR0518           15 RUECC5-CNPJ-CPF-COOP    PIC  9(014) VALUE ZEROS.
BR0518           15 RUECC5-CNPJ-CPF-COOP-X  REDEFINES
BR0518              RUECC5-CNPJ-CPF-COOP    PIC  X(014).
      **** 19    <VlrParclCooperd>          [1..1]
                 15 RUECC5-VLR-PCELA-COOP   PIC  X(019) VALUE SPACES.
      **** 20    <CodProg_LinhaCredCooperd> [1..1]
                 15 RUECC5-CODPROG-LINCRED  PIC  X(004) VALUE SPACES.

4.13   01  RUECC5-COR0005-6A.
4.13  ***  22    <TpFnteRec>                [0..1]
4.13       05 RUECC5-TP-FNTE-REC            PIC  X(004) VALUE SPACES.

BR4.11 01  RUECC5-COR0005-7.
BR4.11**** 23 <Grupo_COR0005_Gleba>         [0..n]
BR4.11     05 RUECC5-GRUPO-COR0005-GLEBA.
BR4.11        10 FILLER                     PIC  X(006) VALUE '{1300}'.
BR4.11        10 RUECC5-GP-COR0005-GLEBA.
BR4.11**** 24 <IdentcGleba>                 [1..1]
BR4.11           15 RUECC5-IDENTC-GLEBA     PIC  X(019) VALUE SPACES.
BR4.11
      *
      **** TAMANHO = 39
      *
211025*    ==> INICIO DOS CAMPOS EXCLUIDOS DA SICOR 5.11
BR4.11*01  RUECC5-COR0005-8.
BR4.11**** 27 <Grupo_COR0005_PontoPolg>     [1..N]
BR4.11*    05 RUECC5-GRUPO-COR0005-PTPOLG.
BR4.11*       10 FILLER                     PIC  X(006) VALUE '{1310}'.
BR4.11*       10 RUECC5-GP-COR0005-PTPOLG.
BR4.11**** 28 <LatPonto>                    [1..1]
BR4.11*          15 RUECC5-LAT-PONTO        PIC  X(013) VALUE SPACES.
BR4.11**** 29 <LongPonto>                   [1..1]
BR4.11*          15 RUECC5-LONG-PONTO       PIC  X(014) VALUE SPACES.
BR4.11**** 30 <AlttPonto>                   [1..1]
BR4.11*          15 RUECC5-ALTT-PONTO       PIC  X(006) VALUE SPACES.
211025*    ==> FIM DOS CAMPOS EXCLUIDOS DA SICOR 5.11
      *
      **** TAMANHO = 11
      *
BR4.11 01  RUECC5-COR0005-8A.
BR4.11**** 25 <AreaNCultd>                  [0..1]
BR4.11     05 RUECC5-AREA-NAO-CULT          PIC  X(011) VALUE SPACES.
BR4.11**** 05 RUECC5-COD-MUN-GLEB  PIC  X(006) VALUE SPACES.
      *
      **** TAMANHO = 29
STE001*
211025*    ==> INICIO DOS CAMPOS EXCLUIDOS DA SICOR 5.11
STE001*01  RUECC5-COR0005-8B.
STE001*    05 RUECC5-GRUPO-COR0005-GLEBA-E.
STE001*       10 FILLER                     PIC  X(006) VALUE '{1320}'.
STE001*
STE001*01  RUECC5-COR0005-8BB.
STE001*    05 RUECC5-GLEBA-EXCLUIDA.
STE001*       10 RUECC5-COR0001-PTPLGEXC.
STE001*          15 FILLER               PIC  X(006) VALUE '{1321}'.
STE001*          15 RUECC5-LAT-PONTO-EXCL
STE001*                                  PIC  X(013) VALUE SPACES.
STE001*          15 RUECC5-LONG-PONTO-EXCL
STE001*                                  PIC  X(014) VALUE SPACES.
STE001*          15 RUECC5-ALTT-PONTO-EXCL
STE001*                                  PIC  X(006) VALUE SPACES.
211025*    ==> FIM DOS CAMPOS EXCLUIDOS DA SICOR 5.11
211025*
211025**** TAMANHO = 10000
211025*
211025 01  RUECC5-COR0005-8C.
211025**** 26 <WKTPart>                     [1..N]
211025     05 FILLER                        PIC  X(006) VALUE '{1310}'.
211025     05 RUECC5-WKT-PART               PIC  X(10000) VALUE SPACES.
      *
BR4.11 01  RUECC5-COR0005-9.
BR4.11**** 28 <PercJurosEncargoFinancPosfix>[0..1]
BR4.11     05 RUECC5-PERC-POSFIX            PIC  X(009) VALUE SPACES.
POS        05 RUECC5-PERC-POSFIX-S9         REDEFINES
POS           RUECC5-PERC-POSFIX            PIC S9(009).
BR4.11**** 29 <TpEncargoFinancCompltar>     [1..1]
BR4.11     05 RUECC5-TPENCARGO-COMPL        PIC  X(002) VALUE SPACES.
RL4.14**** 30-<Area>-                      [0..1]
RL4.14     05 RUECC5-AREA-X                 PIC  X(011) VALUE SPACES.
RL4.14     05 RUECC5-AREA-N REDEFINES RUECC5-AREA-X
RL4.14                                      PIC  9(09)V99.
RL4.14**** 31 <QtdItemFincd>-              [0..1]-
RL4.14     05 RUECC5-QTD-ITEM-FINCD-N       PIC  9(012)V99 VALUE ZEROS.
RL4.14     05 RUECC5-QTD-ITEM-FINCD-X REDEFINES RUECC5-QTD-ITEM-FINCD-N
RL4.14                                      PIC  X(14).
RL4.14**** 32 <PercCstEftTot>--            [0..1]-
RL4.14     05 RUECC5-PERC-CST-EFT-TOTAL     PIC  X(005) VALUE SPACES.
RL4.14     05 RUECC5-PERC-CST-EFT-TOTAL-N   REDEFINES
RL4.14        RUECC5-PERC-CST-EFT-TOTAL     PIC  9(003)V99.
      *
BR0521 01  RUECC5-COR0005-10.
.     **** 33 <Grupo_COR0005_Propt>         [0..n]
.          05 RUECC5-GRUPO-COR0005-PROPT.
.             10 FILLER                     PIC  X(006) VALUE '{1400}'.
.             10 RUECC5-GP-COR0005-PROPT.
.     **** 34    <TpPessoaPropt>            [1..1]
.                15 RUECC5-TPSSOA-PROPT     PIC  X(001) VALUE SPACES.
.     **** 35    <CNPJBase_CPFPropt>        [1..1]
.                15 RUECC5-CNPJ-CPF-PROPT   PIC  X(011) VALUE SPACES.
.     **** 36    <CIB>                      [0..1]
050825           15 RUECC5-CIB              PIC  X(008) VALUE SPACES.
.     **** 37    <CodSNCR>                  [0..1]
.                15 RUECC5-COD-SNCR         PIC  X(013) VALUE SPACES.
.     **** 38    <NumRegCAR>                [0..1]
.                15 RUECC5-NUM-REG-CAR      PIC  X(041) VALUE SPACES.
.     **** 39    <NumOutgAguaPropr>         [0..1]
.                15 RUECC5-COUTGA-AGUA      PIC  X(030) VALUE SPACES.
.     **** 40    <PercPrescPropr>           [0..1]
.                15 RUECC5-PPRESV-AMBTL     PIC  X(005) VALUE SPACES.
.                15 RUECC5-PPRESV-AMBTL-N   REDEFINES
.                   RUECC5-PPRESV-AMBTL     PIC  9(003)V99.
.     *
CAPG05**   CAMPOS EXCLUIDOS - SICOR 5.12
CAPG05*    -01  RUECC5-COR0005-11.
CAPG05**** 42 <Grupo_COR0005_CertificOrgnco>  [0..1]
CAPG05*        -05 RUECC5-GRUPO-COR0005-CORGN.
CAPG05*           -10 RUECC5-GP-COR0005-CORGN.
CAPG05**** 43    <CodCertifcOrgncoEmpnmnt>    [1..1]
CAPG05*               -15 RUECC5-CCETFC-ORGNC     PIC  X(080) VALUE SPACES.
CAPG05**** 44    <URLCodCertifcOrgncoEmpnmnt> [1..1]
CAPG05*               -15 RUECC5-EURL-CETFC-ORGNC PIC  X(255) VALUE SPACES.
CAPG05*
CAPG05*    -01  RUECC5-COR0005-12.
CAPG05**** 46 <Grupo_COR0005_RastlddEmpnmnt>  [0..1]
CAPG05*        -05 RUECC5-GRUPO-COR0005-RASTB.
CAPG05*           -10 RUECC5-GP-COR0005-RASTB.
CAPG05**** 47    <CodRastlddEmpnmnt>          [1..1]
CAPG05*               -15 RUECC5-CRSTRB-EMPTO     PIC  X(080) VALUE SPACES.
CAPG05**** 48    <URLCodRastlddEmpnmnt>       [1..1]
CAPG05*               -15 RUECC5-EURL-RSTRB       PIC  X(255) VALUE SPACES.
CAPG05*
CAPG05*    -01  RUECC5-COR0005-13.
CAPG05**** 50 <PercEnerRenovvlEmpnmnt>        [0..1]
CAPG05*         -05 RUECC5-PENERG-RENOV           PIC  X(005) VALUE SPACES.
CAPG05*         -05 RUECC5-PENERG-RENOV-N         REDEFINES
CAPG05*            -RUECC5-PENERG-RENOV           PIC  9(003)V99.
WP0522*
.      01  RUECC5-COR0005-14.
.     **** 51 <CodSTNEqulzc>                  [0..1]
.          05 RUECC5-COD-STN-EQULZC         PIC  X(013) VALUE SPACES.
.     *
WIP001 01  RUECC5-COR0005-15.
-     **** 52 <Grupo_COR0005_Desclass>      [0..10]
-          05 RUECC5-GRUPO-COR0005-DESCLASS.
-             10 RUECC5-GP-COR0005-DESCLASS.
-     **** 53    <DtDesclassEmpnmnt>        [1..1]
-                15 RUECC5-DT-DESCLASS      PIC  X(008) VALUE SPACES.
-     **** 54    <MotvDesclassCOR>          [1..1]
-                15 RUECC5-MTV-DESCLASS     PIC  X(003) VALUE SPACES.
.     *
NAV506 01  RUECC5-COR0005-16.
-     **** 56 <Grupo_COR0005_DesclassParcl> [0..n]
-          05 RUECC5-GRUPO-COR0005-DESC-PARC.
-             10 FILLER                     PIC  X(006) VALUE '{1500}'.
-             10 RUECC5-GP-COR0005-DESC-PARC.
-     **** 57    <DtDesclassParclEmpnmnt>   [1..1]
-                15 RUECC5-DT-DESCLASS-PAR  PIC  X(008) VALUE SPACES.
-     **** 58    <VlrDesclascd>             [1..1]
-                15 RUECC5-VLR-DESCLASS-PAR PIC  X(019) VALUE SPACES.
-     **** 59    <MotvDesclassCOR>          [1..1]
-                15 RUECC5-MTV-DESCLASS-PAR PIC  X(003) VALUE SPACES.
-     *
7COMMP 01  RUECC5-COR0005-DEST.
7COMMP     05 RUECC5-COD-PROGRAMA           PIC  X(004) VALUE SPACES.
7COMMP     05 RUECC5-SUB-PROGRAMA           PIC  X(004) VALUE SPACES.
-     *
STE001 01  RUECC5-COR0005-JUROS.
STE001     05 RUECC5-PERCJUROSENCARGOFINANC PIC  X(005) VALUE SPACES.
STE001     05 RUECC5-PERCBONUSCAR           PIC  X(005) VALUE SPACES.
-     *
STEFAB 01  RUECC5-COR0005-16A.
-     **** 65 <Grupo_COR0005_Parcl> [0..n]
-          05 RUECC5-GRUPO-COR0005-PARCL.
-             10 FILLER                     PIC  X(006) VALUE '{1600}'.
-             10 RUECC5-GP-COR0005-PARCL.
-     **** 66    <NumParcl>                 [1..1]
-                15 RUECC5-NUM-PARCL-X      PIC  X(003) VALUE SPACES.
-                15 RUECC5-NUM-PARCL-N      REDEFINES RUECC5-NUM-PARCL-X
-                                           PIC  9(003).
-     **** 67    <DtPrvPgto>                [1..1]
-                15 RUECC5-DT-PRV-PAGTO     PIC  X(008) VALUE SPACES.
-     **** 68    <VlrPrincipalParcL>        [1..1]
-                15 RUECC5-VLR-PRINC-X      PIC  X(019) VALUE SPACES.
-                15 RUECC5-VLR-PRINC-N      REDEFINES RUECC5-VLR-PRINC-X
-                                           PIC  9(017)V99.
-     *
-      01  RUECC5-COR0005-CNPJ.
STEFAB     05 RUECC5-CNPJ-BASE-INST-EXIGIB  PIC  X(008) VALUE SPACES.
      *
050825 01  RUECC5-COR0005-BONUS-SUST.
.          05 RUECC5-PERC-BONUS-SUST        PIC  9(003)V99 VALUE ZEROS.
.
.      01  RUECC5-COR0005-16B.
.          05 RUECC5-GRUPO-COR0005-CONFDD.
.             10 FILLER                     PIC X(006) VALUE '{1700}'.
.             10 RUECC5-GP-COR0005-CONFDD.
.                15 RUECC5-TP-CONFDD-X      PIC X(002) VALUE SPACES.
.                15 RUECC5-TP-CONFDD REDEFINES RUECC5-TP-CONFDD-X
050825                                      PIC 9(002).
      *
NAV506 01  RUECC5-COR0005-17.
.     *
           05 RUECC5-DTMOVTO                PIC  9(008) VALUE ZEROS.
