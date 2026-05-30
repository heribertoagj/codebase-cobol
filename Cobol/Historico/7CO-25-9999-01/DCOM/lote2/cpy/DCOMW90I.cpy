      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW90I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM3Z2O e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW90I-BLOCO-SAIDA.
4S2511*      05 DCOMW90I-S-CNPJ                   PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ                   PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL                 PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL                 PIC  X(04).
             05 DCOMW90I-S-CTRL                   PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-AVAL-1AT          PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-AVAL-1AT          PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-AVAL-1AT        PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-AVAL-1AT        PIC  X(04).
             05 DCOMW90I-S-CTRL-AVAL-1AT          PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-AVAL-2AT          PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-AVAL-2AT          PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-AVAL-2AT        PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-AVAL-2AT        PIC  X(04).
             05 DCOMW90I-S-CTRL-AVAL-2AT          PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-AVAL-1NV          PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-AVAL-1NV          PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-AVAL-1NV        PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-AVAL-1NV        PIC  X(04).
             05 DCOMW90I-S-CTRL-AVAL-1NV          PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-AVAL-2NV          PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-AVAL-2NV          PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-AVAL-2NV        PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-AVAL-2NV        PIC  X(04).
             05 DCOMW90I-S-CTRL-AVAL-2NV          PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-TESTEMUNHA-1      PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-TESTEMUNHA-1      PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-TESTEMUNHA-1    PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-TESTEMUNHA-1    PIC  X(04).
             05 DCOMW90I-S-CTRL-TESTEMUNHA-1      PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-TESTEMUNHA-2      PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-TESTEMUNHA-2      PIC  X(09).
4S2511*      05 DCOMW90I-S-FIL-TESTEMUNHA-2       PIC  9(04).
4S2511       05 DCOMW90I-S-FIL-TESTEMUNHA-2       PIC  X(04).
             05 DCOMW90I-S-CTRL-TESTEMUNHA-2      PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-CONJUGE-1NV       PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-CONJUGE-1NV       PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-CONJUGE-1NV     PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-CONJUGE-1NV     PIC  X(04).
             05 DCOMW90I-S-CTRL-CONJUGE-1NV       PIC  9(02).

4S2511*      05 DCOMW90I-S-CNPJ-CONJUGE-2NV       PIC  9(09).
4S2511       05 DCOMW90I-S-CNPJ-CONJUGE-2NV       PIC  X(09).
4S2511*      05 DCOMW90I-S-FILIAL-CONJUGE-2NV     PIC  9(04).
4S2511       05 DCOMW90I-S-FILIAL-CONJUGE-2NV     PIC  X(04).
             05 DCOMW90I-S-CTRL-CONJUGE-2NV       PIC  9(02).