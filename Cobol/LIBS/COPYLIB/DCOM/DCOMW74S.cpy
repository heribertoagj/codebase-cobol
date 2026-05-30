      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB074: DB2PRD.TTAC_CONTR_LIM                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM474S                                       *
      *    -> FETCH   - DCOM474F                                       *
      *    -> INSERT  - DCOM474I                                       *
      *    -> UPDATE  - DCOM474U                                       *
      *    -> DELETE  - DCOM474D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW74S-HEADER.
          10 DCOMW74S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW74S'.
          10 DCOMW74S-TAM-LAYOUT       PIC  9(005) VALUE 05826.
      *
          10 DCOMW74S-BLOCO-SAIDA.
             15 DCOMW74S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW74S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW74S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW74S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW74S-DINIC-PARMZ-CONTR      PIC  X(010).
                20 DCOMW74S-CTPO-COBR-TAC          PIC  S9(003) COMP-3.
                20 DCOMW74S-CTAC-CONTR-LIM         PIC  S9(005) COMP-3.
                20 DCOMW74S-CTAC-PERC-CONTR        PIC  S9(005) COMP-3.
                20 DCOMW74S-CTAC-MIN-CONTR         PIC  S9(005) COMP-3.
                20 DCOMW74S-CTAC-MAX-CONTR         PIC  S9(005) COMP-3.
                20 DCOMW74S-DFIM-PARMZ-CONTR       PIC  X(010).
                20 DCOMW74S-HULT-ATULZ             PIC  X(026).
                20 DCOMW74S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW74S-CTERM                  PIC  X(008).
                20 DCOMW74S-FILLER                 PIC  X(041).
      *                                                                *
