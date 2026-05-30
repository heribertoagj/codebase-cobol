      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB032: DB2PRD.TLOG_RSUMO_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM432S                                       *
      *    -> FETCH   - DCOM432F                                       *
      *    -> INSERT  - DCOM432I                                       *
      *    -> UPDATE  - DCOM432U                                       *
      *    -> DELETE  - DCOM432D                                       *
      *================================================================*
      *
       05 DCOMW32S-HEADER.
          10 DCOMW32S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWS32'.
          10 DCOMW32S-TAM-LAYOUT       PIC  9(005) VALUE 005399.
          10 DCOMW32S-BLOCO-SAIDA.
             15 DCOMW32S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW32S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW32S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW32S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW32S-CEVNTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMW32S-HULT-ATULZ             PIC  X(026).
                20 DCOMW32S-CINDCD-MANUT-REG       PIC  X(001).
                20 DCOMW32S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMW32S-CTRANS-PROG-ATULZ      PIC  X(008).
                20 DCOMW32S-CINDCD-MODLD-PROCM     PIC  X(001).
                20 DCOMW32S-CEMPR-INC              PIC S9(005) COMP-3.
                20 DCOMW32S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMW32S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMW32S-CTERM                  PIC  X(008).
             15 DCOMW32S-FILLER                    PIC  X(173).
      *                                                                *
      *================================================================*
