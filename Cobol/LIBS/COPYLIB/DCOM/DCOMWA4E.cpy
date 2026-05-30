      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0A4: DB2PRD.TGARNT_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4A4S                                       *
      *    -> FETCH   - DCOM4A4F                                       *
      *    -> INSERT  - DCOM4A4I                                       *
      *    -> UPDATE  - DCOM4A4U                                       *
      *    -> DELETE  - DCOM4A4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWA4E-HEADER.
          10 DCOMWA4E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWA4E'.
          10 DCOMWA4E-TAM-LAYOUT       PIC  9(005) VALUE 00578.
      *
       05 DCOMWA4E-BLOCO-ENTRADA.
          10 DCOMWA4E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWA4E-ENTRADA.
             15 DCOMWA4E-CCONTR-LIM-DESC           PIC  S9(009) COMP-3.
             15 DCOMWA4E-CVRSAO-CONTR-LIM          PIC  S9(003) COMP-3.
             15 DCOMWA4E-CTPO-GARNT                PIC  S9(003) COMP-3.
             15 DCOMWA4E-CTERM                     PIC  X(008).
             15 DCOMWA4E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWA4E-HULT-ATULZ                PIC  X(026).
             15 DCOMWA4E-FILLER                    PIC  X(042).
      *
          10 DCOMWA4E-BLOCO-VARIAVEL.
             15 DCOMWA4E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWA4E-BLOCO-PAGINACAO.
             15 DCOMWA4E-MAX-OCORR                 PIC  9(003).
             15 DCOMWA4E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWA4E-P-INICIAL              VALUE 'I'.
                88 DCOMWA4E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWA4E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWA4E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWA4E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWA4E-BLOCO-RESTART.
             15 DCOMWA4E-CHAVE-INI.
                20 DCOMWA4E-I-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWA4E-I-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWA4E-I-CTPO-GARNT           PIC  S9(003) COMP-3.
                20 DCOMWA4E-I-FILLER               PIC  X(015).
             15 DCOMWA4E-CHAVE-FIM.
                20 DCOMWA4E-F-CCONTR-LIM-DESC      PIC  S9(009) COMP-3.
                20 DCOMWA4E-F-CVRSAO-CONTR-LIM     PIC  S9(003) COMP-3.
                20 DCOMWA4E-F-CTPO-GARNT           PIC  S9(003) COMP-3.
                20 DCOMWA4E-F-FILLER               PIC  X(015).
      *
          10 DCOMWA4E-FILLER                       PIC  X(113).
      *
