      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB072: DB2PRD.TSPROD_TX_JURO                          *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM472S                                       *
      *    -> FETCH   - DCOM472F                                       *
      *    -> INSERT  - DCOM472I                                       *
      *    -> UPDATE  - DCOM472U                                       *
      *    -> DELETE  - DCOM472D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW72S-HEADER.
          10 DCOMW72S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW72S'.
          10 DCOMW72S-TAM-LAYOUT       PIC  9(005) VALUE  05500.
      *
          10 DCOMW72S-BLOCO-SAIDA.
             15 DCOMW72S-QTDE-TOTAL                PIC S9(010) COMP-3.
             15 DCOMW72S-QTDE-RETORNADA            PIC S9(003) COMP-3.
             15 DCOMW72S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW72S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMW72S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW72S-HINIC-TX-SPROD         PIC  X(026).
                20 DCOMW72S-CTPO-TX-JURO-DESC      PIC S9(003) COMP-3.
                20 DCOMW72S-HFIM-TX-SPROD          PIC  X(026).
                20 DCOMW72S-HULT-ATULZ             PIC  X(026).
                20 DCOMW72S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW72S-CTERM                  PIC  X(008).
                20 DCOMW72S-QDIA-OPER-ATIVO        PIC S9(004) COMP-3.
                20 DCOMW72S-CINDCD-FREQ-ESTOQ      PIC  X(002).
                20 DCOMW72S-CINDCD-OPER-ATRSO      PIC  X(001).
      *
             15 DCOMW72S-FILLER                    PIC  X(329).
