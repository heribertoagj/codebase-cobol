      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0D6: DB2PRD.TRELAC_PSSOA_DESC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4D6S                                       *
      *    -> FETCH   - DCOM4D6F                                       *
      *    -> INSERT  - DCOM4D6I                                       *
      *    -> UPDATE  - DCOM4D6U                                       *
      *    -> DELETE  - DCOM4D6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWD6E-HEADER.
          10 DCOMWD6E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWD6E'.
          10 DCOMWD6E-TAM-LAYOUT       PIC  9(005) VALUE 01200.
      *
       05 DCOMWD6E-BLOCO-ENTRADA.
          10 DCOMWD6E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWD6E-ENTRADA.
             15 DCOMWD6E-NPSSOA-DESC-COML          PIC  S9(009) COMP-3.
             15 DCOMWD6E-NRELAC-PSSOA-DESC         PIC  S9(009) COMP-3.
             15 DCOMWD6E-CINDCD-PSSOA-DESC         PIC  X(002).
             15 DCOMWD6E-CELMTO-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMWD6E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWD6E-CBCO                      PIC  S9(003) COMP-3.
             15 DCOMWD6E-CAG-BCRIA                 PIC  S9(005) COMP-3.
             15 DCOMWD6E-CCTA-BCRIA-CLI            PIC  S9(013) COMP-3.
             15 DCOMWD6E-CPOSTO-SERVC              PIC  S9(003) COMP-3.
             15 DCOMWD6E-IPSSOA-CONJG              PIC  X(040).
             15 DCOMWD6E-CCPF-CONJG                PIC  S9(009) COMP-3.
             15 DCOMWD6E-CCTRL-CPF-CONJG           PIC  S9(002) COMP-3.
             15 DCOMWD6E-CDOCTO-ID-CONJG           PIC  X(015).
             15 DCOMWD6E-CINDCD-CONJG-AUTRZ        PIC  X(001).
             15 DCOMWD6E-CCEP                      PIC  S9(005) COMP-3.
             15 DCOMWD6E-CCEP-COMPL                PIC  S9(003) COMP-3.
             15 DCOMWD6E-ELOGDR                    PIC  X(040).
             15 DCOMWD6E-CSGL-UF                   PIC  X(002).
             15 DCOMWD6E-DINCL-REG                 PIC  X(010).
             15 DCOMWD6E-HULT-ATULZ                PIC  X(026).
             15 DCOMWD6E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWD6E-CTERM                     PIC  X(008).
             15 DCOMWD6E-FILLER                    PIC  X(100).
      *
          10 DCOMWD6E-BLOCO-VARIAVEL.
             15 DCOMWD6E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWD6E-BLOCO-PAGINACAO.
             15 DCOMWD6E-MAX-OCORR                 PIC  9(003).
             15 DCOMWD6E-TOTALIZAR                 PIC  X(001).
             15 DCOMWD6E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWD6E-P-INICIAL              VALUE 'I'.
                88 DCOMWD6E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWD6E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWD6E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWD6E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWD6E-BLOCO-RESTART.
             15 DCOMWD6E-CHAVE-INI.
                20 DCOMWD6E-I-NPSSOA-DESC-COML     PIC  S9(009) COMP-3.
                20 DCOMWD6E-I-NRELAC-PSSOA-DESC    PIC  S9(009) COMP-3.
                20 DCOMWD6E-I-CINDCD-PSSOA-DESC    PIC  X(002).
                20 DCOMWD6E-I-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMWD6E-I-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWD6E-I-CBCO                 PIC  S9(003) COMP-3.
                20 DCOMWD6E-I-CAG-BCRIA            PIC  S9(005) COMP-3.
                20 DCOMWD6E-I-CCTA-BCRIA-CLI       PIC  S9(013) COMP-3.
                20 DCOMWD6E-I-FILLER               PIC  X(100).
             15 DCOMWD6E-CHAVE-FIM.
                20 DCOMWD6E-F-NPSSOA-DESC-COML     PIC  S9(009) COMP-3.
                20 DCOMWD6E-F-NRELAC-PSSOA-DESC    PIC  S9(009) COMP-3.
                20 DCOMWD6E-F-CINDCD-PSSOA-DESC    PIC  X(002).
                20 DCOMWD6E-F-CELMTO-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMWD6E-F-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWD6E-F-CBCO                 PIC  S9(003) COMP-3.
                20 DCOMWD6E-F-CAG-BCRIA            PIC  S9(005) COMP-3.
                20 DCOMWD6E-F-CCTA-BCRIA-CLI       PIC  S9(013) COMP-3.
                20 DCOMWD6E-F-FILLER               PIC  X(100).
      *
          10 DCOMWD6E-FILLER                       PIC  X(232).
      *
