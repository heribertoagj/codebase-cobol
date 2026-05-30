      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0J7: DB2PRD.TCTRL_NRO_CORP                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4J5S                                       *
      *    -> FETCH   - DCOM4J5F                                       *
      *    -> INSERT  - DCOM4J5I                                       *
      *    -> UPDATE  - DCOM4J5U                                       *
      *    -> DELETE  - DCOM4J5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWJ5E-HEADER.
          10 DCOMWJ5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ5E'.
          10 DCOMWJ5E-TAM-LAYOUT       PIC  9(005) VALUE 00500.
      *
       05 DCOMWJ5E-BLOCO-ENTRADA.
          10 DCOMWJ5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWJ5E-ENTRADA.
             15 DCOMWJ5E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWJ5E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWJ5E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWJ5E-NULT-SEQ-CONTR            PIC S9(018) COMP-3.
             15 DCOMWJ5E-QDIG-SEQ-CONTR            PIC S9(003) COMP-3.
             15 DCOMWJ5E-HULT-ATULZ                PIC  X(026).
      *
          10 DCOMWJ5E-BLOCO-VARIAVEL.
             15 DCOMWJ5E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWJ5E-BLOCO-PAGINACAO.
             15 DCOMWJ5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWJ5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWJ5E-P-INICIAL              VALUE 'I'.
                88 DCOMWJ5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWJ5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWJ5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWJ5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWJ5E-BLOCO-RESTART.
             15 DCOMWJ5E-CHAVE-INI.
                20 DCOMWJ5E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWJ5E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWJ5E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWJ5E-I-FILLER               PIC  X(010).
             15 DCOMWJ5E-CHAVE-FIM.
                20 DCOMWJ5E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWJ5E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWJ5E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWJ5E-F-FILLER               PIC  X(010).
      *
          10 DCOMWJ5E-FILLER                       PIC  X(079).
      *
