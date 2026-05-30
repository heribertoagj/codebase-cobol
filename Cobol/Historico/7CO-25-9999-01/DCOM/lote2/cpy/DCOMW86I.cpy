      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW86I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM3Z4O e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW86I-BLOCO-SAIDA.
             05 DCOMW86I-S-IPSSOA-DESC-COML    PIC  X(60).

4S2511*      05 DCOMW86I-S-CCNPJ-CPF           PIC  9(09).
4S2511       05 DCOMW86I-S-CCNPJ-CPF           PIC  X(09).
4S2511*      05 DCOMW86I-S-CFLIAL-CNPJ         PIC  9(04).
4S2511       05 DCOMW86I-S-CFLIAL-CNPJ         PIC  X(04).
             05 DCOMW86I-S-CCTRL-CNPJ-CPF      PIC  9(02).

             05 DCOMW86I-S-NOCOR               PIC  9(02).

             05 DCOMW86I-S-CAG-BCRIA           PIC  9(05).
             05 DCOMW86I-S-DIGITO-AGENCIA      PIC  X(01).
             05 DCOMW86I-S-CCTA-BCRIA-CLI      PIC  9(13).
             05 DCOMW86I-S-DIGITO-CONTA        PIC  X(01).
             05 DCOMW86I-S-DINIC-OPER-DESC     PIC  X(10).
             05 DCOMW86I-S-OPERACAO            PIC  X(20).
             05 DCOMW86I-S-VOPER-DESC-COML     PIC  9(15)V99.
             05 DCOMW86I-S-NOME-AGEN           PIC  X(40).

             05 DCOMW86I-S-CHEQUES OCCURS 050 TIMES INDEXED BY IND-86I.
                10 DCOMW86I-S-CBCO-CHQ         PIC  9(03).
                10 DCOMW86I-S-AGEN-CHQ         PIC  9(05).
                10 DCOMW86I-S-CTA-CHQ          PIC  9(13).
4S2511*           10 DCOMW86I-S-CCPFCNPJ-CHQ    PIC  9(09).
4S2511            10 DCOMW86I-S-CCPFCNPJ-CHQ    PIC  X(09).
4S2511*           10 DCOMW86I-S-CFILIAL-CHQ     PIC  9(04).
4S2511            10 DCOMW86I-S-CFILIAL-CHQ     PIC  X(04).
                10 DCOMW86I-S-CCONT-CHQ        PIC  9(02).
                10 DCOMW86I-S-NCHQ             PIC  9(06).
                10 DCOMW86I-S-DT-DEPOS         PIC  X(10).
                10 DCOMW86I-S-VIOF-CHQ         PIC  9(15)V99.
                10 DCOMW86I-S-VJUR-CHQ         PIC  9(15)V99.
                10 DCOMW86I-S-VLIQ-CHQ         PIC  9(15)V99.
                10 DCOMW86I-S-VTOT-CHQ         PIC  9(15)V99.

             05 DCOMW86I-S-VTOT-IOF-CHQ        PIC  9(15)V99.
             05 DCOMW86I-S-VTOT-JUR-CHQ        PIC  9(15)V99.
             05 DCOMW86I-S-VTOT-LIQ-CHQ        PIC  9(15)V99.
             05 DCOMW86I-S-VSOMATOT-CHQ        PIC  9(15)V99.