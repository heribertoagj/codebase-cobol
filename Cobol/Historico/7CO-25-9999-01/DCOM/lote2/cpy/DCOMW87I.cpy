      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW87I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW87I-BLOCO-SAIDA.
             05 DCOMW87I-S-CAG-BCRIA          PIC  9(05).
             05 DCOMW87I-S-DIGITO-AGEN        PIC  X(01).
             05 DCOMW87I-S-NOME-AGEN          PIC  X(40).
             05 DCOMW87I-S-CCTA-BCRIA         PIC  9(13).
             05 DCOMW87I-S-DIGITO-CONTA       PIC  X(01).
4S2511*      05 DCOMW87I-S-CCNPJ-CPF          PIC  9(09).
4S2511       05 DCOMW87I-S-CCNPJ-CPF          PIC  X(09).
4S2511*      05 DCOMW87I-S-CFLIAL-CNPJ        PIC  9(04).
4S2511       05 DCOMW87I-S-CFLIAL-CNPJ        PIC  X(04).
             05 DCOMW87I-S-CCTRL-CNPJ-CPF     PIC  9(02).
             05 DCOMW87I-S-DINIC-OPER         PIC  X(10).
             05 DCOMW87I-S-VOPER-DESC         PIC  9(15)V99.
             05 DCOMW87I-S-IPSSOA-DESC        PIC  X(60).
             05 DCOMW87I-S-ENDER              PIC  X(60).
             05 DCOMW87I-S-ICIDADE            PIC  X(40).
             05 DCOMW87I-S-CEP                PIC  9(08).
             05 DCOMW87I-S-SGLUF              PIC  X(02).
             05 DCOMW87I-S-NOCOR              PIC  9(02).

             05 DCOMW87I-S-TEST OCCURS 050 TIMES INDEXED BY IND-87I.
                 10 DCOMW87I-S-ITESTEMUNHA     PIC  X(40).
                 10 DCOMW87I-S-CRGTEST         PIC  X(20).
4S2511*            10 DCOMW87I-S-CCPFCNPJ-TEST   PIC  9(09).
4S2511             10 DCOMW87I-S-CCPFCNPJ-TEST   PIC  X(09).
4S2511*            10 DCOMW87I-S-CFILIAL-TEST     PIC  9(04).
4S2511             10 DCOMW87I-S-CFILIAL-TEST     PIC  X(04).
                    10 DCOMW87I-S-CCONT-TEST      PIC  9(02).