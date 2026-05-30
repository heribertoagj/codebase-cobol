      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB065: DB2PRD.TSPROD_MEIO_LIBRC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM434S                                       *
      *    -> FETCH   - DCOM434F                                       *
      *    -> INSERT  - DCOM434I                                       *
      *    -> UPDATE  - DCOM434U                                       *
      *    -> DELETE  - DCOM434D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW34S-HEADER.
          10 DCOMW34S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW34S'.
          10 DCOMW34S-TAM-LAYOUT       PIC  9(005) VALUE 14126.
      *
          10 DCOMW34S-BLOCO-SAIDA.
             15 DCOMW34S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW34S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW34S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW34S-CMEIO-LIBRC-DESC       PIC S9(003) COMP-3.
                20 DCOMW34S-IMEIO-LIBRC-DESC       PIC  X(025).
                20 DCOMW34S-IRSUMO-MEIO-LIBRC      PIC  X(015).
                20 DCOMW34S-CSGL-MEIO-LIBRC        PIC  X(006).
                20 DCOMW34S-HULT-ATULZ             PIC  X(026).
                20 DCOMW34S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW34S-FILLER                 PIC  X(200).
      *                                                                *
