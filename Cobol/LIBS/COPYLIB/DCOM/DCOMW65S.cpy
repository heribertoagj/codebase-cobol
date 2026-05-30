      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB065: DB2PRD.TSPROD_MEIO_LIBRC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM465S                                       *
      *    -> FETCH   - DCOM465F                                       *
      *    -> INSERT  - DCOM465I                                       *
      *    -> UPDATE  - DCOM465U                                       *
      *    -> DELETE  - DCOM465D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW65S-HEADER.
          10 DCOMW65S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW65S'.
          10 DCOMW65S-TAM-LAYOUT       PIC  9(005) VALUE 14876.
      *
          10 DCOMW65S-BLOCO-SAIDA.
             15 DCOMW65S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW65S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW65S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW65S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW65S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW65S-CMEIO-LIBRC-DESC       PIC S9(003) COMP-3.
                20 DCOMW65S-HINIC-MEIO-LIBRC       PIC  X(026).
                20 DCOMW65S-HFIM-MEIO-LIBRC        PIC  X(026).
                20 DCOMW65S-HULT-ATULZ             PIC  X(026).
                20 DCOMW65S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW65S-CTERM                  PIC  X(008).
                20 DCOMW65S-FILLER                 PIC  X(200).
      *                                                                *
