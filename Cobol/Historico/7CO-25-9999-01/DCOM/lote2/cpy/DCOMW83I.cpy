      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW83I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW83I-BLOCO-SAIDA.
             05 DCOMW83I-S-CAG-BCRIA           PIC  9(05).
             05 DCOMW83I-S-DIGITO-AGENCIA      PIC  X(01).
             05 DCOMW83I-S-CCTA-BCRIA-CLI      PIC  9(13).
             05 DCOMW83I-S-DIGITO-CONTA        PIC  X(01).
4S2511*      05 DCOMW83I-S-CCNPJ-CPF           PIC  9(09).
4S2511       05 DCOMW83I-S-CCNPJ-CPF           PIC  X(09).
4S2511*      05 DCOMW83I-S-CFLIAL-CNPJ         PIC  9(04).
4S2511       05 DCOMW83I-S-CFLIAL-CNPJ         PIC  X(04).
             05 DCOMW83I-S-CCTRL-CNPJ-CPF      PIC  9(02).
             05 DCOMW83I-S-DINIC-OPER-DESC     PIC  X(10).
             05 DCOMW83I-S-VOPER-DESC-COML     PIC  9(15)V99.
             05 DCOMW83I-S-NOME-AGEN           PIC  X(40).
             05 DCOMW83I-S-IPSSOA-DESC-COML    PIC  X(60).
             05 DCOMW83I-S-VLIQ-OPER           PIC  9(15)V99.
             05 DCOMW83I-S-PTX-JURO-MES        PIC  9(06)V9(07).
             05 DCOMW83I-S-PTX-JURO-ANO        PIC  9(06)V9(07).
             05 DCOMW83I-S-CCART-LIM           PIC  9(03).
             05 DCOMW83I-S-DATA-PRIM-VCTO      PIC  X(10).
             05 DCOMW83I-S-DVCTO-FNAL-OPER     PIC  X(10).
             05 DCOMW83I-S-VNOTA-PROMIS        PIC  9(15)V99.
             05 DCOMW83I-S-VIOF-OPER           PIC  9(15)V99.
             05 DCOMW83I-S-VTAC-OPER           PIC  9(15)V99.
             05 DCOMW83I-S-VTARIF-REG          PIC  9(15)V99.
             05 DCOMW83I-S-VTRIBUTOS           PIC  9(15)V99.
             05 DCOMW83I-S-PTRIBUTOS           PIC  9(03)V99.
             05 DCOMW83I-S-VSEGUROS            PIC  9(15)V99.
             05 DCOMW83I-S-PSEGUROS            PIC  9(03)V99.
             05 DCOMW83I-S-VTARIFA-CET         PIC  9(15)V99.
             05 DCOMW83I-S-PTARIFA-CET         PIC  9(03)V99.
             05 DCOMW83I-S-VPGTOSERV           PIC  9(15)V99.
             05 DCOMW83I-S-PPGTOSERV           PIC  9(03)V99.
             05 DCOMW83I-S-VREGISTROS          PIC  9(15)V99.
             05 DCOMW83I-S-PREGISTROS          PIC  9(03)V99.
             05 DCOMW83I-S-VCET-TOTAL          PIC  9(15)V99.
             05 DCOMW83I-S-PCET-TOTAL          PIC  9(03)V99.
             05 DCOMW83I-S-VCET-MES            PIC  9(08)V9(07).
             05 DCOMW83I-S-VCET-ANO            PIC  9(08)V9(07).
             05 DCOMW83I-S-VTOT-LIB            PIC  9(15)V99.
             05 DCOMW83I-S-TOT-LIB             PIC  9(03)V99.
             05 DCOMW83I-S-NOCOR               PIC  9(02).

             05 DCOMW83I-S-OCORR OCCURS 050 TIMES INDEXED BY IND-83I.
                 10 DCOMW83I-S-NOME-AVALISTA    PIC  X(40).
4S2511*            10 DCOMW83I-S-CNPJ-AVALISTA    PIC  9(09).
4S2511             10 DCOMW83I-S-CNPJ-AVALISTA    PIC  X(09).
4S2511*            10 DCOMW83I-S-FILIAL-AVALISTA  PIC  9(04).
4S2511             10 DCOMW83I-S-FILIAL-AVALISTA  PIC  X(04).
                    10 DCOMW83I-S-CTRL-AVALISTA   PIC  9(02).
                 10 DCOMW83I-S-NOME-CONJUGE     PIC  X(40).
4S2511*            10 DCOMW83I-S-CNPJ-CNJG        PIC  9(09).
4S2511             10 DCOMW83I-S-CNPJ-CNJG        PIC  X(09).
4S2511*            10 DCOMW83I-S-FILIAL-CNJG      PIC  9(04).
4S2511             10 DCOMW83I-S-FILIAL-CNJG      PIC  X(04).
                    10 DCOMW83I-S-CTRL-CNJG       PIC  9(02).
                 10 DCOMW83I-S-NOME-TESTEMUNHA  PIC  X(40).
4S2511*            10 DCOMW83I-S-CNPJ-TESTEMUNHA  PIC  9(09).
4S2511             10 DCOMW83I-S-CNPJ-TESTEMUNHA  PIC  X(09).
4S2511*            10 DCOMW83I-S-FILIAL-TESTEMUNHA PIC  9(04).
4S2511             10 DCOMW83I-S-FILIAL-TESTEMUNHA PIC  X(04).
                    10 DCOMW83I-S-CTRL-TESTEMUNHA PIC  9(02).