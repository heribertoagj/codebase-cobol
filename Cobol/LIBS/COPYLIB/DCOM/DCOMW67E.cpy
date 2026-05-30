      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB067: DB2PRD.TSPROD_REGRA_CONTR                      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM467S                                       *
      *    -> FETCH   - DCOM467F                                       *
      *    -> INSERT  - DCOM467I                                       *
      *    -> UPDATE  - DCOM467U                                       *
      *    -> DELETE  - DCOM467D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW67E-HEADER.
          10 DCOMW67E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW67E'.
          10 DCOMW67E-TAM-LAYOUT       PIC  9(005) VALUE 01054.
      *
       05 DCOMW67E-BLOCO-ENTRADA.
          10 DCOMW67E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMW67E-ENTRADA.
             15 DCOMW67E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMW67E-CSPROD-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMW67E-HINIC-REGRA-CONTR         PIC  X(026).
             15 DCOMW67E-HFIM-REGRA-CONTR          PIC  X(026).
             15 DCOMW67E-CINDCD-TOMAD-OPER         PIC  X(001).
             15 DCOMW67E-CINDCD-PSSOA-TOMAD        PIC  X(001).
             15 DCOMW67E-CINDCD-CONTR-LIM          PIC  X(001).
             15 DCOMW67E-CINDCD-CONVE-CLI          PIC  X(001).
             15 DCOMW67E-CINDCD-CONVE-GRAL         PIC  X(001).
             15 DCOMW67E-CINDCD-BAIXA-PCIAL        PIC  X(001).
             15 DCOMW67E-CINDCD-BAIXA-RTEIO        PIC  X(001).
             15 DCOMW67E-CINDCD-BAIXA-ANTCP        PIC  X(001).
             15 DCOMW67E-CINDCD-BAIXA-GSTOR        PIC  X(001).
             15 DCOMW67E-CINDCD-ADTTO-PCELA        PIC  X(001).
             15 DCOMW67E-CINDCD-PRROG-PCELA        PIC  X(001).
             15 DCOMW67E-VTARIF-PRROG-PCELA        PIC  S9(017) COMP-3.
             15 DCOMW67E-CTARIF-PRROG-PCELA        PIC  S9(005) COMP-3.
             15 DCOMW67E-CRESP-LIM-CREDT           PIC  X(001).
             15 DCOMW67E-CINDCD-CONS-CREDT         PIC  X(001).
             15 DCOMW67E-CINDCD-ANLSE-CREDT        PIC  X(001).
             15 DCOMW67E-HULT-ATULZ                PIC  X(026).
             15 DCOMW67E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMW67E-CTERM                     PIC  X(008).
             15 DCOMW67E-CINDCD-DISPN-AG           PIC  X(001).
             15 DCOMW67E-CINDCD-DISPN-NE           PIC  X(001).
             15 DCOMW67E-CINDCD-DISPN-TLBCO        PIC  X(001).
             15 DCOMW67E-CINDCD-DISPN-IB           PIC  X(001).
             15 DCOMW67E-CINDCD-ADTTO-TX           PIC  X(001).
             15 DCOMW67E-QVCTO-PCELA-ADTTO         PIC  S9(003) COMP-3.
             15 DCOMW67E-CINDCD-ADTTO-OPCAO        PIC  X(001).
             15 DCOMW67E-PTX-OPCAO-ACRES           PIC  S9(009) COMP-3.
             15 DCOMW67E-PTX-OPCAO-REDC            PIC  S9(009) COMP-3.
      *
          10 DCOMW67E-BLOCO-VARIAVEL.
             15 DCOMW67E-CPRODT-SERVC-OPER         PIC  S9(008) COMP-3.
             15 DCOMW67E-V-FILLER                  PIC  X(292).
      *
          10 DCOMW67E-BLOCO-PAGINACAO.
             15 DCOMW67E-MAX-OCORR                 PIC  9(003).
             15 DCOMW67E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW67E-P-INICIAL              VALUE 'I'.
                88 DCOMW67E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW67E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW67E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW67E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW67E-BLOCO-RESTART.
             15 DCOMW67E-CHAVE-INI.
                20 DCOMW67E-I-CPRODT               PIC  S9(009) COMP-3.
                20 DCOMW67E-I-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW67E-I-HINIC-REGRA-CONT     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMW67E-CHAVE-FIM.
                20 DCOMW67E-F-CPRODT               PIC  S9(009) COMP-3.
                20 DCOMW67E-F-CSPROD-DESC-COML     PIC  S9(003) COMP-3.
                20 DCOMW67E-F-HINIC-REGRA-CONT     PIC  S9(003) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMW67E-FILLER                       PIC  X(173).
      *
