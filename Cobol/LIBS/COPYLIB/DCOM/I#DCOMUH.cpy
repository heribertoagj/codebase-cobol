      ******************************************************************
      * SISTEMA : DCOM - I#DCOMUH                                      *
      * TAMANHO : 2185 BYTES                                           *
      * ARQUIVO : DE TITULOS/CHEQUES ANALISADOS CONTRATADO             *
      * INCLUDE PARA TABELA: DCOMB0N6 - DB2PRD.THIST_ANLSE_RCBVL       *
      ******************************************************************
       01  UH-N6-REGISTRO.
           10  UH-N6-CBCO                 PIC S9(003) COMP-3.
           10  UH-N6-CAG-BCRIA            PIC S9(005) COMP-3.
           10  UH-N6-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  UH-N6-CELMTO-DESC-COML     PIC S9(003) COMP-3.
           10  UH-N6-CCHAVE-ELMTO-DESC    PIC  X(045).
           10  UH-N6-HINCL-REG-HIST       PIC  X(026).
           10  UH-N6-CPTCAO-TBELA         PIC S9(003) COMP-3.
           10  UH-N6-CPRODT               PIC S9(003) COMP-3.
           10  UH-N6-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  UH-N6-CRESUL-ANLSE-TITLO   PIC S9(002) COMP-3.
           10  UH-N6-DSOLTC-ANLSE-CREDT   PIC  X(010).
           10  UH-N6-CINDCD-TITLO-DESC    PIC  X(001).
           10  UH-N6-CIDTFD-TPO-MANUT     PIC  X(001).
           10  UH-N6-CUSUAR-INCL          PIC  X(009).
           10  UH-N6-HINCL-REG            PIC  X(026).
           10  UH-N6-CUSUAR-MANUT         PIC  X(009).
           10  UH-N6-HULT-ATULZ           PIC  X(026).
           10  UH-N6-CTERM                PIC  X(008).
           10  UH-N6-WRESUL-ANLSE-TITLO.
              15  UH-N6-WRESUL-ANLSE-TITLO-LEN  PIC S9(004) COMP.
              15  UH-N6-WRESUL-ANLSE-TITLO-TEXT PIC  X(2000).
