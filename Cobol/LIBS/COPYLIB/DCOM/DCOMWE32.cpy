      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB032: DB2PRD.TLOG_RSUMO_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM432S                                       *
      *    -> FETCH   - DCOM432F                                       *
      *    -> INSERT  - DCOM432I                                       *
      *    -> UPDATE  - DCOM432U                                       *
      *    -> DELETE  - DCOM432D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW32E-HEADER.
          10 DCOMW32E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE32'.
          10 DCOMW32E-TAM-LAYOUT       PIC  9(005) VALUE 01069.
      *
       05 DCOMW32E-BLOCO-ENTRADA.
          10 DCOMW32E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW32E-ENTRADA.
             15 DCOMW32E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW32E-CEVNTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW32E-HULT-ATULZ                PIC  X(026).
             15 DCOMW32E-CINDCD-MANUT-REG          PIC  X(001).
             15 DCOMW32E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMW32E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMW32E-CINDCD-MODLD-PROCM        PIC  X(001).
             15 DCOMW32E-CEMPR-INC                 PIC S9(005) COMP-3.
             15 DCOMW32E-CDEPDC                    PIC S9(005) COMP-3.
             15 DCOMW32E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW32E-CTERM                     PIC  X(008).
      *
          10 DCOMW32E-BLOCO-VARIAVEL.
             15 DCOMW32E-V-FILLER                  PIC  X(289).

          10 DCOMW32E-BLOCO-PAGINACAO.
             15 DCOMW32E-MAX-OCORR                 PIC  9(003).
             15 DCOMW32E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW32E-P-INICIAL              VALUE 'I'.
                88 DCOMW32E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW32E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW32E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW32E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW32E-BLOCO-RESTART.
             15 DCOMW32E-CHAVE-INI.
                20 DCOMW32E-I-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW32E-I-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW32E-I-HULT-ATULZ           PIC  X(026).
                20 FILLER                          PIC  X(200).
             15 DCOMW32E-CHAVE-FIM.
                20 DCOMW32E-F-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW32E-F-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW32E-F-HULT-ATULZ           PIC  X(0026).
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW32E-FILLER                       PIC  X(189).
      *
