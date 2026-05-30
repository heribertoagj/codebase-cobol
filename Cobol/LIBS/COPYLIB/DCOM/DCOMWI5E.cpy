      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0I5: DB2PRD.TPAREC_FORML_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4I5S                                       *
      *    -> FETCH   - DCOM4I5F                                       *
      *    -> INSERT  - DCOM4I5I                                       *
      *    -> UPDATE  - DCOM4I5U                                       *
      *    -> DELETE  - DCOM4I5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWI5E-HEADER.
          10 DCOMWI5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWI5E'.
          10 DCOMWI5E-TAM-LAYOUT       PIC  9(005) VALUE 00993.
      *
       05 DCOMWI5E-BLOCO-ENTRADA.
          10 DCOMWI5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWI5E-ENTRADA.
             15 DCOMWI5E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWI5E-HPAREC-FORML-DESC         PIC  X(026).
             15 DCOMWI5E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWI5E-RPRCER-FORML-DESC.
                49 DCOMWI5E-RPRCER-FORML-LEN       PIC S9(004) COMP.
                49 DCOMWI5E-RPRCER-FORML-TEXT      PIC  X(240).
             15 DCOMWI5E-HULT-ATULZ                PIC  X(026).
             15 DCOMWI5E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWI5E-CTERM                     PIC  X(008).
             15 DCOMWI5E-FILLER                    PIC  X(021).
      *
          10 DCOMWI5E-BLOCO-VARIAVEL.
             15 DCOMWI5E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWI5E-BLOCO-PAGINACAO.
             15 DCOMWI5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWI5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWI5E-P-INICIAL              VALUE 'I'.
                88 DCOMWI5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWI5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWI5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWI5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWI5E-BLOCO-RESTART.
             15 DCOMWI5E-CHAVE-INI.
                20 DCOMWI5E-I-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMWI5E-I-HPAREC-FORML-DESC    PIC  X(026).
                20 DCOMWI5E-I-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWI5E-I-FILLER               PIC  X(016).
             15 DCOMWI5E-CHAVE-FIM.
                20 DCOMWI5E-F-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMWI5E-F-HPAREC-FORML-DESC    PIC  X(026).
                20 DCOMWI5E-F-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWI5E-F-FILLER               PIC  X(016).
      *
          10 DCOMWI5E-FILLER                       PIC  X(113).
      *
