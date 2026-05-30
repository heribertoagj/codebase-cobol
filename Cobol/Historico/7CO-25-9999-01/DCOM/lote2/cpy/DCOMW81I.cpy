      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW81I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW81I-BLOCO-SAIDA.
             05 DCOMW81I-S-CAG-BCRIA          PIC  9(05).
             05 DCOMW81I-S-DIGITO-AGENCIA     PIC  X(01).
             05 DCOMW81I-S-CCTA-BCRIA-CLI     PIC  9(13).
             05 DCOMW81I-S-DIGITO-CONTA       PIC  X(01).
4S2511*      05 DCOMW81I-S-CCNPJ-CPF          PIC  9(09).
4S2511       05 DCOMW81I-S-CCNPJ-CPF          PIC  X(09).
4S2511*      05 DCOMW81I-S-CFLIAL-CNPJ        PIC  9(04).
4S2511       05 DCOMW81I-S-CFLIAL-CNPJ        PIC  X(04).
             05 DCOMW81I-S-CCTRL-CNPJ-CPF     PIC  9(02).
             05 DCOMW81I-S-DINIC-OPER-DESC    PIC  X(10).
             05 DCOMW81I-S-VOPER-DESC-COML    PIC  9(15)V99.
             05 DCOMW81I-S-IPSSOA-DESC-COML   PIC  X(60).
             05 DCOMW81I-S-DATA-PRIM-VCTO     PIC  X(10).
             05 DCOMW81I-S-DVCTO-FNAL-OPER    PIC  X(10).
             05 DCOMW81I-S-PTX-JURO-MES       PIC  9(06)V9(07).
             05 DCOMW81I-S-PTX-JURO-ANO       PIC  9(06)V9(07).
             05 DCOMW81I-S-QTITLO-CHEQ-PCELA  PIC  9(09).
             05 DCOMW81I-S-VTRIBUTOS          PIC  9(15)V99.
             05 DCOMW81I-S-PTRIBUTOS          PIC  9(03)V99.
             05 DCOMW81I-S-VSEGUROS           PIC  9(15)V99.
             05 DCOMW81I-S-VTARIFA-CET        PIC  9(15)V99.
             05 DCOMW81I-S-PTARIFA-CET        PIC  9(03)V99.
             05 DCOMW81I-S-VPGTOSERV          PIC  9(15)V99.
             05 DCOMW81I-S-PPGTOSERV          PIC  9(03)V99.
             05 DCOMW81I-S-VREGISTROS         PIC  9(15)V99.
             05 DCOMW81I-S-PREGISTROS         PIC  9(03)V99.
             05 DCOMW81I-S-VCET-TOTAL         PIC  9(15)V99.
             05 DCOMW81I-S-PCET-TOTAL         PIC  9(03)V99.
             05 DCOMW81I-S-VCET-MES           PIC  9(08)V9(07).
             05 DCOMW81I-S-VCET-ANO           PIC  9(08)V9(07).
             05 DCOMW81I-S-VTOT-LIQ           PIC  9(15)V99.
             05 DCOMW81I-S-VTOT-LIB           PIC  9(15)V99.
             05 DCOMW81I-S-TOT-LIB            PIC  9(03)V99.
             05 DCOMW81I-S-NOCOR              PIC  9(02).