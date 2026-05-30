      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW88I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW88I-BLOCO-SAIDA.
             05 DCOMW88I-S-NNOTA-PROMIS      PIC  9(10).
             05 DCOMW88I-S-DVCTO-FNAL-OPER   PIC  X(10).
             05 DCOMW88I-S-VALOR-NOTA-PROMIS PIC  9(15)V99.
             05 DCOMW88I-S-NOME-BANCO        PIC  X(40).
             05 DCOMW88I-S-NOME-AGENCIA      PIC  X(40).
             05 DCOMW88I-S-VAL-OPER-EXT      PIC  9(15)V99.
             05 DCOMW88I-S-NOME-EMINENTE     PIC  X(40).
             05 DCOMW88I-S-RG-EMITENTE       PIC  X(20).
4S2511*      05 DCOMW88I-S-CNPJ-EMINENTE     PIC  9(09).
4S2511       05 DCOMW88I-S-CNPJ-EMINENTE     PIC  X(09).
4S2511*      05 DCOMW88I-S-FILIAL-EMINENTE   PIC  9(04).
4S2511       05 DCOMW88I-S-FILIAL-EMINENTE   PIC  X(04).
             05 DCOMW88I-S-CTRL-EMINENTE     PIC  9(02).
             05 DCOMW88I-S-END-EMINENTE      PIC  X(60).
             05 DCOMW88I-S-CCEP              PIC  9(08).
             05 DCOMW88I-S-NOCOR             PIC  9(02).

             05 DCOMW88I-S-OCORR OCCURS 050 TIMES INDEXED BY IND-88I.
                 10 DCOMW88I-S-NOME-AVAL       PIC  X(40).
                 10 DCOMW88I-S-CDOCTO-ID-AVAL  PIC  X(15).
4S2511*            10 DCOMW88I-S-CNPJ-AVAL       PIC  9(09).
4S2511             10 DCOMW88I-S-CNPJ-AVAL       PIC  X(09).
4S2511*            10 DCOMW88I-S-FILIAL-AVAL     PIC  9(04).
4S2511             10 DCOMW88I-S-FILIAL-AVAL     PIC  X(04).
                    10 DCOMW88I-S-CTRL-AVAL      PIC  9(02).
                 10 DCOMW88I-S-END-AVAL        PIC  X(60).
                 10 DCOMW88I-S-CEP-AVAL        PIC  9(08).
                 10 DCOMW88I-S-NOME-CONJUGE    PIC  X(40).
4S2511*            10 DCOMW88I-S-CNPJ-CNJG       PIC  9(09).
4S2511             10 DCOMW88I-S-CNPJ-CNJG       PIC  X(09).
4S2511*            10 DCOMW88I-S-FILIAL-CNJG     PIC  9(04).
4S2511             10 DCOMW88I-S-FILIAL-CNJG     PIC  X(04).
                    10 DCOMW88I-S-CTRL-CNJG      PIC  9(02).