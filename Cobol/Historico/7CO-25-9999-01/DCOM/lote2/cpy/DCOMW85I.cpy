      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW85I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM3Z3O e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW85I-BLOCO-SAIDA.
4S2511*      05 DCOMW85I-S-CCNPJ-CPF             PIC  9(09).
4S2511       05 DCOMW85I-S-CCNPJ-CPF             PIC  X(09).
4S2511*      05 DCOMW85I-S-CFLIAL-CNPJ           PIC  9(04).
4S2511       05 DCOMW85I-S-CFLIAL-CNPJ           PIC  X(04).
             05 DCOMW85I-S-CCTRL-CNPJ-CPF        PIC  9(02).

      *    Ocorrências de títulos/fiadores
       05 DCOMW85I-S-OCORR OCCURS 050 TIMES INDEXED BY IND-85I.
4S2511*         10 DCOMW85I-S-CNPJ-SAC          PIC  9(09).
4S2511          10 DCOMW85I-S-CNPJ-SAC          PIC  X(09).
4S2511*         10 DCOMW85I-S-FILIAL-SAC        PIC  9(04).
4S2511          10 DCOMW85I-S-FILIAL-SAC        PIC  X(04).
                10 DCOMW85I-S-CTRL-SAC          PIC  9(02).