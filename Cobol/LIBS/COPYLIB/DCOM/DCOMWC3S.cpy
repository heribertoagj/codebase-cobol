      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0C3: DB2PRD.TMOVTC_DESC_COML                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4C3S                                       *
      *    -> FETCH   - DCOM4C3F                                       *
      *    -> INSERT  - DCOM4C3I                                       *
      *    -> UPDATE  - DCOM4C3U                                       *
      *    -> DELETE  - DCOM4C3D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWC3S-HEADER.
          10 DCOMWC3S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC3S'.
          10 DCOMWC3S-TAM-LAYOUT       PIC  9(005) VALUE 25000.
      *
          10 DCOMWC3S-BLOCO-SAIDA.
             15 DCOMWC3S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWC3S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWC3S-OCORRENCIA                OCCURS 015 TIMES.
                20 DCOMWC3S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC3S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC3S-HULT-ATULZ             PIC  X(026).
                20 DCOMWC3S-CINDCD-MANUT-REG       PIC  X(001).
                20 DCOMWC3S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWC3S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMWC3S-WREG-MOVTC-DESC.
                   25 DCOMWC3S-WREG-MOVTC-LEN      PIC S9(004) COMP.
                   25 DCOMWC3S-WREG-MOVTC-TEXT     PIC X(1500).
                20 DCOMWC3S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMWC3S-CINDCD-MODLD-PROCM     PIC  X(001).
                20 DCOMWC3S-CEMPR-INC              PIC S9(005) COMP-3.
                20 DCOMWC3S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMWC3S-DMOVTC-DESC-COML       PIC  X(010).
                20 DCOMWC3S-CCATLG-COMPS-REG       PIC  X(008).
                20 DCOMWC3S-NSEQ-PROCM-EVNTO       PIC S9(003) COMP-3.
                20 DCOMWC3S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWC3S-CTERM                  PIC  X(008).
      *
             15 DCOMWC3S-FILLER                    PIC  X(554).
      *================================================================*
