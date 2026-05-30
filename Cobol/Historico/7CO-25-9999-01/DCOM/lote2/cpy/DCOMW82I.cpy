      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW82I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW82I-BLOCO-SAIDA.
             05 DCOMW82I-S-CAG-BCRIA          PIC  9(05).
             05 DCOMW82I-S-DIGITO-AGENCIA     PIC  X(01).
             05 DCOMW82I-S-CCTA-BCRIA-CLI     PIC  9(13).
             05 DCOMW82I-S-DIGITO-CONTA       PIC  X(01).
4S2511*      05 DCOMW82I-S-CCNPJ-CPF          PIC  9(09).
4S2511       05 DCOMW82I-S-CCNPJ-CPF          PIC  X(09).
4S2511*      05 DCOMW82I-S-CFLIAL-CNPJ        PIC  9(04).
4S2511       05 DCOMW82I-S-CFLIAL-CNPJ        PIC  X(04).
             05 DCOMW82I-S-CCTRL-CNPJ-CPF     PIC  9(02).
             05 DCOMW82I-S-DINIC-OPER-DESC    PIC  X(10).
             05 DCOMW82I-S-VOPER-DESC-COML    PIC  9(15)V99.
             05 DCOMW82I-S-IPSSOA-DESC-COML   PIC  X(60).
             05 DCOMW82I-S-DATA-PRIM-VCTO     PIC  X(10).
             05 DCOMW82I-S-DVCTO-FNAL-OPER    PIC  X(10).
             05 DCOMW82I-S-PTX-JURO-MES       PIC  9(06)V9(07).
             05 DCOMW82I-S-PTX-JURO-ANO       PIC  9(06)V9(07).
             05 DCOMW82I-S-QTITLO-CHEQ-PCELA  PIC  9(09).
             05 DCOMW82I-S-CCART-LIM          PIC  9(03).
             05 DCOMW82I-S-VTRIBUTOS          PIC  9(15)V99.
             05 DCOMW82I-S-PTRIBUTOS          PIC  9(03)V99.
             05 DCOMW82I-S-VSEGUROS           PIC  9(15)V99.
             05 DCOMW82I-S-VIOF-OPER          PIC  9(15)V99.
             05 DCOMW82I-S-VTAC-OPER          PIC  9(15)V99.
             05 DCOMW82I-S-VTARIFA-CET        PIC  9(15)V99.
             05 DCOMW82I-S-PTARIFA-CET        PIC  9(03)V99.
             05 DCOMW82I-S-VPGTOSERV          PIC  9(15)V99.
             05 DCOMW82I-S-PPGTOSERV          PIC  9(03)V99.
             05 DCOMW82I-S-VREGISTROS         PIC  9(15)V99.
             05 DCOMW82I-S-PREGISTROS         PIC  9(03)V99.
             05 DCOMW82I-S-VCET-TOTAL         PIC  9(15)V99.
             05 DCOMW82I-S-PCET-TOTAL         PIC  9(03)V99.
             05 DCOMW82I-S-VCET-MES           PIC  9(08)V9(07).
             05 DCOMW82I-S-VCET-ANO           PIC  9(08)V9(07).
             05 DCOMW82I-S-VTOT-LIQ           PIC  9(15)V99.
             05 DCOMW82I-S-VTOT-LIB           PIC  9(15)V99.
             05 DCOMW82I-S-TOT-LIB            PIC  9(03)V99.
             05 DCOMW82I-S-NOCOR              PIC  9(02).

             05 DCOMW82I-S-OCORR OCCURS 050 TIMES INDEXED BY IND-82I.
                 10 DCOMW82I-S-NOME-AVAL       PIC  X(40).
4S2511*            10 DCOMW82I-S-CNPJ-AVAL       PIC  9(09).
4S2511             10 DCOMW82I-S-CNPJ-AVAL       PIC  X(09).
4S2511*            10 DCOMW82I-S-FILIAL-AVAL     PIC  9(04).
4S2511             10 DCOMW82I-S-FILIAL-AVAL     PIC  X(04).
                    10 DCOMW82I-S-CTRL-AVAL      PIC  9(02).
                 10 DCOMW82I-S-NOME-CONJUGE    PIC  X(40).
4S2511*            10 DCOMW82I-S-CNPJ-CNJG       PIC  9(09).
4S2511             10 DCOMW82I-S-CNPJ-CNJG       PIC  X(09).
4S2511*            10 DCOMW82I-S-FILIAL-CNJG     PIC  9(04).
4S2511             10 DCOMW82I-S-FILIAL-CNJG     PIC  X(04).
                    10 DCOMW82I-S-CTRL-CNJG      PIC  9(02).
                 10 DCOMW82I-S-NOME-TEST       PIC  X(40).
4S2511*            10 DCOMW82I-S-CNPJ-TEST       PIC  9(09).
4S2511             10 DCOMW82I-S-CNPJ-TEST       PIC  X(09).
4S2511*            10 DCOMW82I-S-FILIAL-TEST     PIC  9(04).
4S2511             10 DCOMW82I-S-FILIAL-TEST     PIC  X(04).
                    10 DCOMW82I-S-CTRL-TEST      PIC  9(02).