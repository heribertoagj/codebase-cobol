      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB061: DB2PRD.TSPROD_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM461S                                       *
      *    -> FETCH   - DCOM461F                                       *
      *    -> INSERT  - DCOM461I                                       *
      *    -> UPDATE  - DCOM461U                                       *
      *    -> DELETE  - DCOM461D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW61E-HEADER.
          10 DCOMW61E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW61E'.
          10 DCOMW61E-TAM-LAYOUT       PIC  9(005) VALUE 00575.
      *
       05 DCOMW61E-BLOCO-ENTRADA.
          10 DCOMW61E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW61E-ENTRADA.
             15 DCOMW61E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMW61E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMW61E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW61E-CSPROD-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMW61E-HULT-ATULZ                PIC  X(026).
             15 DCOMW61E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW61E-CTERM                     PIC  X(008).
             15 DCOMW61E-FILLER                    PIC  X(039).
      *
          10 DCOMW61E-BLOCO-VARIAVEL.
             15 DCOMW61E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW61E-BLOCO-PAGINACAO.
             15 DCOMW61E-MAX-OCORR                 PIC  9(003).
             15 DCOMW61E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW61E-P-INICIAL              VALUE 'I'.
                88 DCOMW61E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW61E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW61E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW61E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW61E-BLOCO-RESTART.
             15 DCOMW61E-CHAVE-INI.
                20 DCOMW61E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW61E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW61E-I-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW61E-I-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW61E-I-FILLER               PIC  X(012).
             15 DCOMW61E-CHAVE-FIM.
                20 DCOMW61E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMW61E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMW61E-F-CPRODT               PIC  S9(003) COMP-3.
                20 DCOMW61E-F-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW61E-F-FILLER               PIC  X(012).
      *
          10 DCOMW61E-FILLER                       PIC  X(113).
      *
