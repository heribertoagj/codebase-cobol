      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB031: DB2PRD.TLOG_COPLT_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM431S                                       *
      *    -> FETCH   - DCOM431F                                       *
      *    -> INSERT  - DCOM431I                                       *
      *    -> UPDATE  - DCOM431U                                       *
      *    -> DELETE  - DCOM431D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW31E-HEADER.
          10 DCOMW31E-COD-LAYOUT      PIC  X(008) VALUE 'DCOMWE31'.
          10 DCOMW31E-TAM-LAYOUT      PIC  9(005) VALUE 03600.
      *
       05 DCOMW31E-BLOCO-ENTRADA.
          10 DCOMW31E-INSTRUCAO                   PIC  X(010).
      *
          10 DCOMW31E-ENTRADA.
             15 DCOMW31E-CELMTO-DESC-COML         PIC S9(003) COMP-3.
             15 DCOMW31E-CEVNTO-DESC-COML         PIC S9(003) COMP-3.
             15 DCOMW31E-HULT-ATULZ               PIC  X(026).
             15 DCOMW31E-CINDCD-MANUT-REG         PIC  X(001).
             15 DCOMW31E-CCHAVE-ELMTO-DESC        PIC  X(045).
             15 DCOMW31E-WREG-LOG-ANTES.
                20 DCOMW31E-WREG-LOG-ANTES-LEN    PIC S9(004) COMP.
                20 DCOMW31E-WREG-LOG-ANTES-TEXT   PIC X(1500).
             15 DCOMW31E-WREG-LOG-APOS.
                20 DCOMW31E-WREG-LOG-APOS-LEN     PIC S9(004) COMP.
                20 DCOMW31E-WREG-LOG-APOS-TEXT    PIC X(1500).
             15 DCOMW31E-CTRANS-PROG-ATULZ        PIC  X(008).
             15 DCOMW31E-CINDCD-MODLD-PROCM       PIC  X(001).
             15 DCOMW31E-CEMPR-INC                PIC S9(005) COMP-3.
             15 DCOMW31E-CDEPDC                   PIC S9(005) COMP-3.
             15 DCOMW31E-CFUNC-BDSCO              PIC S9(009) COMP-3.
             15 DCOMW31E-CTERM                    PIC  X(008).
      *
          10 DCOMW31E-BLOCO-VARIAVEL.
             15 DCOMW31E-V-FILLER                 PIC  X(100).

          10 DCOMW31E-BLOCO-PAGINACAO.
             15 DCOMW31E-MAX-OCORR                PIC  9(003).
             15 DCOMW31E-INDICADOR-PAGINACAO      PIC  X(001).
                88 DCOMW31E-P-INICIAL             VALUE 'I'.
                88 DCOMW31E-P-PRIMEIRA            VALUE 'P'.
                88 DCOMW31E-P-SEGUINTE            VALUE 'S'.
                88 DCOMW31E-P-ANTERIOR            VALUE 'A'.
                88 DCOMW31E-P-ULTIMA              VALUE 'U'.
      *
          10 DCOMW31E-BLOCO-RESTART.
             15 DCOMW31E-CHAVE-INI.
                20 DCOMW31E-I-CELMTO-DESC-COML    PIC S9(003) COMP-3.
                20 DCOMW31E-I-CEVNTO-DESC-COML    PIC S9(003) COMP-3.
                20 DCOMW31E-I-HULT-ATULZ          PIC  X(026).
                20 FILLER                         PIC  X(100).
             15 DCOMW31E-CHAVE-FIM.
                20 DCOMW31E-F-CELMTO-DESC-COML    PIC S9(003) COMP-3.
                20 DCOMW31E-F-CEVNTO-DESC-COML    PIC S9(003) COMP-3.
                20 DCOMW31E-F-HULT-ATULZ          PIC  X(026).
                20 FILLER                         PIC  X(100).
      *
          10 DCOMW31E-FILLER                      PIC  X(105).
      *
