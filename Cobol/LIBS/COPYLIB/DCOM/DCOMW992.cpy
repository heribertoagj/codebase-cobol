      ******************************************************************
      *  NOME BOOK.: DCOMW992                                          *
      *  DESCRICAO.: LAYOUT BAIXA DAS TABELAS                          *
      *  DATA......: 11/2020                                           *
      *  AUTOR     : ANSELMO DIAS DUARTE - CD BRADESCO                 *
      *  SISTEMA   : DCOM -                                            *
      *  TAMANHO...: 068 BYTES                                         *
      ******************************************************************
      *
       01  W992-REGISTRO.
           10 W992-CPRODT               PIC S9(3)V       COMP-3.
           10 W992-CSPROD-DESC-COML     PIC S9(3)V       COMP-3.
           10 W992-CMEIO-ENTRD-DESC     PIC S9(3)V       COMP-3.
           10 W992-CINDCD-PSSOA-TOMAD   PIC X(1).
           10 W992-CSGMTO-CLI           PIC S9(3)V       COMP-3.
           10 W992-DINIC-TX-JURO        PIC X(10).
           10 W992-DFIM-TX-JURO         PIC X(10).
           10 W992-VMIN-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W992-VMAX-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W992-PMIN-TX-REDE-DESC    PIC S9(3)V9(3)   COMP-3.
           10 W992-PMAX-TX-REDE-DESC    PIC S9(3)V9(3)   COMP-3.
           10 W992-CFUNC-BDSCO          PIC S9(9)V       COMP-3.
           10 W992-CTERM                PIC X(8).
