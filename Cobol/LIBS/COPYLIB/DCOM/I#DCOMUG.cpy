      ******************************************************************
      * SISTEMA : DCOM - I#DCOMUG                                      *
      * TAMANHO : 2158 BYTES                                           *
      * ARQUIVO : DE TITULOS/CHEQUES ANALISADOS MARCADOS               *
      * INCLUDE PARA TABELA: DCOMB0N5 - DB2PRD.TANLSE_RCBVL_DESC       *
      ******************************************************************
       01  UG-N5-REGISTRO.
1    2     10  UG-N5-CBCO               PIC S9(003) COMP-3.
3    3     10  UG-N5-CAG-BCRIA          PIC S9(005) COMP-3.
6    7     10  UG-N5-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
13   2     10  UG-N5-CELMTO-DESC-COML   PIC S9(003) COMP-3.
15  45     10  UG-N5-CCHAVE-ELMTO-DESC  PIC  X(045).
60   2     10  UG-N5-CPTCAO-TBELA       PIC S9(003) COMP-3.
62   2     10  UG-N5-CPRODT             PIC S9(003) COMP-3.
64   2     10  UG-N5-CSPROD-DESC-COML   PIC S9(003) COMP-3.
66   2     10  UG-N5-CRESUL-ANLSE-TITLO PIC S9(002) COMP-3.
68  10     10  UG-N5-DSOLTC-ANLSE-CREDT PIC  X(010).
78   1     10  UG-N5-CIDTFD-TPO-MANUT   PIC  X(001).
79   9     10  UG-N5-CUSUAR-INCL        PIC  X(009).
88  26     10  UG-N5-HINCL-REG          PIC  X(026).
114  9     10  UG-N5-CUSUAR-MANUT       PIC  X(009).
123 26     10  UG-N5-HULT-ATULZ         PIC  X(026).
148  8     10  UG-N5-CTERM              PIC  X(008).
           10  UG-N5-WRESUL-ANLSE-TITLO.
157  2        15  UG-N5-WRESUL-ANLSE-TITLO-LEN  PIC S9(004) COMP.
159           15  UG-N5-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).
  2000

