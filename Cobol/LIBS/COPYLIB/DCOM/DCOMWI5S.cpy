      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWI5S-HEADER.
          10 DCOMWI5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWI5S'.
          10 DCOMWI5S-TAM-LAYOUT       PIC  9(005) VALUE 18776.
      *
          10 DCOMWI5S-BLOCO-SAIDA.
             15 DCOMWI5S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWI5S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWI5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWI5S-CELMTO-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMWI5S-HPAREC-FORML-DESC      PIC  X(026).
                20 DCOMWI5S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMWI5S-RPRCER-FORML-DESC.
                   49 DCOMWI5S-HPAREC-FORML-LEN    PIC S9(004) COMP.
                   49 DCOMWI5S-HPAREC-FORML-TEXT   PIC  X(240).
                20 DCOMWI5S-HULT-ATULZ             PIC  X(026).
                20 DCOMWI5S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWI5S-CTERM                  PIC  X(008).
                20 DCOMWI5S-FILLER                 PIC  X(021).
      *                                                                *
      *================================================================*
