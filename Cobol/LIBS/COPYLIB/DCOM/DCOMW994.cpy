      ******************************************************************
      *  NOME BOOK.: DCOMW994                                          *
      *  DESCRICAO.: LAYOUT BAIXA DAS TABELAS                          *
      *  DATA......: 11/2020                                           *
      *  AUTOR     : ANSELMO DIAS DUARTE - CD BRADESCO                 *
      *  SISTEMA   : DCOM -                                            *
      *  TAMANHO...: 095 BYTES                                         *
      ******************************************************************
      *
       01  W994-REGISTRO.
           10 W994-CPRODT               PIC S9(3)V       COMP-3.
           10 W994-CSPROD-DESC-COML     PIC S9(3)V       COMP-3.
           10 W994-CMEIO-ENTRD-DESC     PIC S9(3)V       COMP-3.
           10 W994-CINDCD-PSSOA-TOMAD   PIC X(1).
           10 W994-CSGMTO-CLI           PIC S9(3)V       COMP-3.
           10 W994-HINIC-TX-OPER-DESC   PIC X(26).
           10 W994-HFIM-TX-OPER-DESC    PIC X(26).
           10 W994-VMIN-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W994-VMAX-FAIXA-OPER      PIC S9(15)V9(2)  COMP-3.
           10 W994-PTX-MAX-JURO-DESC    PIC S9(3)V9(2)   COMP-3.
           10 W994-CFUNC-BDSCO          PIC S9(9)V       COMP-3.
           10 W994-CTERM                PIC X(8).
