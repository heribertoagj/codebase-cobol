      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB087: DB2PRD.TWORKF_CONTR_LIM                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM487S                                       *
      *    -> FETCH   - DCOM487F                                       *
      *    -> INSERT  - DCOM487I                                       *
      *    -> UPDATE  - DCOM487U                                       *
      *    -> DELETE  - DCOM487D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW87S-HEADER.
          10 DCOMW87S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW87S'.
          10 DCOMW87S-TAM-LAYOUT       PIC  9(005) VALUE 21926.
      *
          10 DCOMW87S-BLOCO-SAIDA.
             15 DCOMW87S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW87S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW87S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW87S-CCONTR-LIM-DESC        PIC  S9(009) COMP-3.
                20 DCOMW87S-CVRSAO-CONTR-LIM       PIC  S9(003) COMP-3.
                20 DCOMW87S-CEVNTO-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW87S-HWORKF-CONTR-LIM       PIC  X(026).
                20 DCOMW87S-CSIT-INIC-DESC         PIC  S9(003) COMP-3.
                20 DCOMW87S-HSIT-INIC-DESC         PIC  X(026).
                20 DCOMW87S-CSIT-FNAL-DESC         PIC  S9(003) COMP-3.
                20 DCOMW87S-HSIT-FNAL-DESC         PIC  X(026).
                20 DCOMW87S-RWORKF-CONTR-LIM.
                   49 DCOMW87S-RWORKF-CONTR-LIM-LEN  PIC S9(004) COMP.
                   49 DCOMW87S-RWORKF-CONTR-LIM-TEXT PIC  X(240).
                20 DCOMW87S-HULT-ATULZ             PIC  X(026).
                20 DCOMW87S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW87S-CTERM                  PIC  X(008).
                20 DCOMW87S-FILLER                 PIC  X(066).
      *                                                                *
