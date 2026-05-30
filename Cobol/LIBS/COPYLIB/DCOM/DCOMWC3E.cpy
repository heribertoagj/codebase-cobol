      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWC3E-HEADER.
          10 DCOMWC3E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC3E'.
          10 DCOMWC3E-TAM-LAYOUT       PIC  9(005) VALUE 02000.
      *
       05 DCOMWC3E-BLOCO-ENTRADA.
          10 DCOMWC3E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWC3E-ENTRADA.
             15 DCOMWC3E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWC3E-CEVNTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWC3E-HULT-ATULZ                PIC  X(026).
             15 DCOMWC3E-CINDCD-MANUT-REG          PIC  X(001).
             15 DCOMWC3E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWC3E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWC3E-WREG-MOVTC-DESC.
                20 DCOMWC3E-WREG-MOVTC-LEN         PIC S9(004) COMP.
                20 DCOMWC3E-WREG-MOVTC-TEXT        PIC X(1500).
             15 DCOMWC3E-CTRANS-PROG-ATULZ         PIC  X(008).
             15 DCOMWC3E-CINDCD-MODLD-PROCM        PIC  X(001).
             15 DCOMWC3E-CEMPR-INC                 PIC S9(005) COMP-3.
             15 DCOMWC3E-CDEPDC                    PIC S9(005) COMP-3.
             15 DCOMWC3E-DMOVTC-DESC-COML          PIC  X(010).
             15 DCOMWC3E-CCATLG-COMPS-REG          PIC  X(008).
             15 DCOMWC3E-NSEQ-PROCM-EVNTO          PIC S9(003) COMP-3.
             15 DCOMWC3E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWC3E-CTERM                     PIC  X(008).
      *
          10 DCOMWC3E-BLOCO-VARIAVEL.
             15 DCOMWC3E-V-FILLER                  PIC  X(100).
      *
          10 DCOMWC3E-BLOCO-PAGINACAO.
             15 DCOMWC3E-MAX-OCORR                 PIC  9(003).
             15 DCOMWC3E-TOTALIZAR                 PIC  X(001).
             15 DCOMWC3E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWC3E-P-INICIAL              VALUE 'I'.
                88 DCOMWC3E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWC3E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWC3E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWC3E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWC3E-BLOCO-RESTART.
             15 DCOMWC3E-CHAVE-INI.
                20 DCOMWC3E-I-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC3E-I-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC3E-I-HULT-ATULZ           PIC  X(026).
             15 DCOMWC3E-CHAVE-FIM.
                20 DCOMWC3E-F-CELMTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC3E-F-CEVNTO-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC3E-F-HULT-ATULZ           PIC  X(026).
      *
          10 DCOMWC3E-FILLER                       PIC  X(184).
