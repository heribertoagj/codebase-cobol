      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB031: DB2PRD.TLOG_COPLT_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM431S                                       *
      *    -> FETCH   - DCOM431F                                       *
      *    -> INSERT  - DCOM431I                                       *
      *    -> UPDATE  - DCOM431U                                       *
      *    -> DELETE  - DCOM431D                                       *
      *================================================================*
      *
       05 DCOMW31S-HEADER.
          10 DCOMW31S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW31S'.
          10 DCOMW31S-TAM-LAYOUT       PIC  9(005) VALUE 15700.
      *
          10 DCOMW31S-BLOCO-SAIDA.
             15 DCOMW31S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW31S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW31S-OCORRENCIA                OCCURS 005 TIMES.
                20 DCOMW31S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW31S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW31S-HULT-ATULZ             PIC  X(026).
                20 DCOMW31S-CINDCD-MANUT-REG       PIC  X(001).
                20 DCOMW31S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMW31S-WREG-LOG-ANTES.
                   25 DCOMW31S-WREG-LOG-ANTES-LEN  PIC S9(004) COMP.
                   25 DCOMW31S-WREG-LOG-ANTES-TEXT PIC X(1500).
                20 DCOMW31S-WREG-LOG-APOS.
                   25 DCOMW31S-WREG-LOG-APOS-LEN   PIC S9(004) COMP.
                   25 DCOMW31S-WREG-LOG-APOS-TEXT  PIC X(1500).
                20 DCOMW31S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW31S-CINDCD-MODLD-PROCM     PIC  X(001).
                20 DCOMW31S-CEMPR-INC              PIC S9(005) COMP-3.
                20 DCOMW31S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMW31S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW31S-CTERM                  PIC  X(008).
             15 DCOMW31S-FILLER                    PIC  X(134).
      *
      *================================================================*
