      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0H0: DB2PRD.TCONTR_CONS_CREDT                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H0S                                       *
      *    -> FETCH   - DCOM4H0F                                       *
      *    -> INSERT  - DCOM4H0I                                       *
      *    -> UPDATE  - DCOM4H0U                                       *
      *    -> DELETE  - DCOM4H0D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH0S-HEADER.
          10 DCOMWH0S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH0S'.
          10 DCOMWH0S-TAM-LAYOUT       PIC  9(005) VALUE 23850.
      *
          10 DCOMWH0S-BLOCO-SAIDA.
             15 DCOMWH0S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWH0S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWH0S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWH0S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMWH0S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMWH0S-NSEQ-CONS-CREDT        PIC  S9(003) COMP-3.
                20 DCOMWH0S-DANO-BASE              PIC  S9(004) COMP-3.
                20 DCOMWH0S-CDEPDC                 PIC  S9(005) COMP-3.
                20 DCOMWH0S-CSEQ-STUDO             PIC  S9(009) COMP-3.
                20 DCOMWH0S-VCONS-NEGOC            PIC S9(015)V9(002)
                                                                 COMP-3.
                20 DCOMWH0S-CCART-LIM-CREDT        PIC  X(005).
                20 DCOMWH0S-HULT-ATULZ             PIC  X(026).
                20 DCOMWH0S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWH0S-CTERM                  PIC  X(008).
          10 DCOMWH0S-FILLER                       PIC  X(500).
      *                                                                *
      *================================================================*
