      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWANI                                          *
      *----------------------------------------------------------------*
      *    Comarea do modulo ANIs                                      *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumerico          *
      ******************************************************************

       01 DCOMWANI-BLOCO-ENTRADA.
           05 DCOMWANI-E-CPPSTA-PRODT       PIC  9(03).
           05 DCOMWANI-E-NSMULA-OPER-DESC   PIC  9(09).
           05 DCOMWANI-E-CAG-BCRIA          PIC  9(05).
           05 DCOMWANI-E-CCTA-BCRIA-CLI     PIC  9(13).
4S2511*      05 DCOMWANI-E-CCNPJ-CPF        PIC  9(09).
4S2511       05 DCOMWANI-E-CCNPJ-CPF        PIC  X(09).
4S2511*      05 DCOMWANI-E-CFLIAL-CNPJ      PIC  9(04).
4S2511       05 DCOMWANI-E-CFLIAL-CNPJ      PIC  X(04).
           05 DCOMWANI-E-CCTRL-CNPJ-CPF     PIC  9(02).

       01 DCOMWANI-BLOCO-SAIDA.
           05 DCOMWANI-S-CPSSOA-JURID-CONTR PIC  9(09).
           05 DCOMWANI-S-CTPO-CONTR-NEGOC   PIC  9(02).
           05 DCOMWANI-S-NSEQ-CONTR-NEGOC   PIC  9(10).