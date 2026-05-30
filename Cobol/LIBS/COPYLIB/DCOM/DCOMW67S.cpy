      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW67S-HEADER.
          10 DCOMW67S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW67S'.
          10 DCOMW67S-TAM-LAYOUT       PIC  9(005) VALUE 06976.
      *
          10 DCOMW67S-BLOCO-SAIDA.
             15 DCOMW67S-QTDE-TOTAL                PIC  9(010).
             15 DCOMW67S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMW67S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMW67S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMW67S-CSPROD-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMW67S-HINIC-REGRA-CONTR      PIC  X(026).
                20 DCOMW67S-HFIM-REGRA-CONTR       PIC  X(026).
                20 DCOMW67S-CINDCD-TOMAD-OPER      PIC  X(001).
                20 DCOMW67S-CINDCD-PSSOA-TOMAD     PIC  X(001).
                20 DCOMW67S-CINDCD-CONTR-LIM       PIC  X(001).
                20 DCOMW67S-CINDCD-CONVE-CLI       PIC  X(001).
                20 DCOMW67S-CINDCD-CONVE-GRAL      PIC  X(001).
                20 DCOMW67S-CINDCD-BAIXA-PCIAL     PIC  X(001).
                20 DCOMW67S-CINDCD-BAIXA-RTEIO     PIC  X(001).
                20 DCOMW67S-CINDCD-BAIXA-ANTCP     PIC  X(001).
                20 DCOMW67S-CINDCD-BAIXA-GSTOR     PIC  X(001).
                20 DCOMW67S-CINDCD-ADTTO-PCELA     PIC  X(001).
                20 DCOMW67S-CINDCD-PRROG-PCELA     PIC  X(001).
                20 DCOMW67S-VTARIF-PRROG-PCELA     PIC  S9(017) COMP-3.
                20 DCOMW67S-CTARIF-PRROG-PCELA     PIC  S9(005) COMP-3.
                20 DCOMW67S-CRESP-LIM-CREDT        PIC  X(001).
                20 DCOMW67S-CINDCD-CONS-CREDT      PIC  X(001).
                20 DCOMW67S-CINDCD-ANLSE-CREDT     PIC  X(001).
                20 DCOMW67S-HULT-ATULZ             PIC  X(026).
                20 DCOMW67S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMW67S-CTERM                  PIC  X(008).
                20 DCOMW67S-CINDCD-DISPN-AG        PIC  X(001).
                20 DCOMW67S-CINDCD-DISPN-NE        PIC  X(001).
                20 DCOMW67S-CINDCD-DISPN-TLBCO     PIC  X(001).
                20 DCOMW67S-CINDCD-DISPN-IB        PIC  X(001).
                20 DCOMW67S-CINDCD-ADTTO-TX        PIC  X(001).
                20 DCOMW67S-QVCTO-PCELA-ADTTO      PIC  S9(003) COMP-3.
                20 DCOMW67S-CINDCD-ADTTO-OPCAO     PIC  X(001).
                20 DCOMW67S-PTX-OPCAO-ACRES        PIC  S9(009) COMP-3.
                20 DCOMW67S-PTX-OPCAO-REDC         PIC  S9(009) COMP-3.
