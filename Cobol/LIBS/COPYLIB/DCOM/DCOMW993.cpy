      ******************************************************************
      *  NOME BOOK.: DCOMW993                                          *
      *  DESCRICAO.: LAYOUT BAIXA DAS TABELAS                          *
      *  DATA......: 11/2020                                           *
      *  AUTOR     : ANSELMO DIAS DUARTE - CD BRADESCO                 *
      *  SISTEMA   : DCOM -                                            *
      *  TAMANHO...: 095 BYTES                                         *
      ******************************************************************
      *
       01  W993-REGISTRO.
           10 W993-CPRODT               PIC S9(3)V       COMP-3.
           10 W993-CSPROD-DESC-COML     PIC S9(3)V       COMP-3.
           10 W993-CMEIO-ENTRD-DESC     PIC S9(3)V       COMP-3.
           10 W993-CINDCD-PSSOA-TOMAD   PIC X(1).
           10 W993-CSGMTO-CLI           PIC S9(3)V       COMP-3.
           10 W993-HINIC-TX-OPER-DESC   PIC X(26).
           10 W993-HFIM-TX-OPER-DESC    PIC X(26).
           10 W993-VMIN-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W993-VMAX-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W993-PTX-MAX-JURO-DESC    PIC S9(3)V9(2)   COMP-3.
           10 W993-CFUNC-BDSCO          PIC S9(9)V       COMP-3.
           10 W993-CTERM                PIC X(8).
