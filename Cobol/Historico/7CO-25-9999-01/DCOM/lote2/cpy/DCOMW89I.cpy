      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW89I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM327C e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW89I-BLOCO-SAIDA.
             05 DCOMW89I-S-NOME-FIADOR        PIC  X(40).
4S2511*      05 DCOMW89I-S-CNPJ-FIADOR        PIC  9(09).
4S2511       05 DCOMW89I-S-CNPJ-FIADOR        PIC  X(09).
4S2511*      05 DCOMW89I-S-FILIAL-FIADOR      PIC  9(04).
4S2511       05 DCOMW89I-S-FILIAL-FIADOR      PIC  X(04).
             05 DCOMW89I-S-CTRL-FIADOR        PIC  9(02).
             05 DCOMW89I-S-END-FIADOR         PIC  X(60).
             05 DCOMW89I-S-CEP-FIADOR         PIC  9(08).

             05 DCOMW89I-S-NOME-CONJUGE       PIC  X(40).
4S2511*      05 DCOMW89I-S-CNPJ-CONJUGE       PIC  9(09).
4S2511       05 DCOMW89I-S-CNPJ-CONJUGE       PIC  X(09).
4S2511*      05 DCOMW89I-S-FILIAL-CONJUGE     PIC  9(04).
4S2511       05 DCOMW89I-S-FILIAL-CONJUGE     PIC  X(04).
             05 DCOMW89I-S-CTRL-CONJUGE       PIC  9(02).

             05 DCOMW89I-S-NOME-TESTEMUNHA    PIC  X(40).
4S2511*      05 DCOMW89I-S-CNPJ-TESTEMUNHA    PIC  9(09).
4S2511       05 DCOMW89I-S-CNPJ-TESTEMUNHA    PIC  X(09).
4S2511*      05 DCOMW89I-S-FILIAL-TESTEMUNHA  PIC  9(04).
4S2511       05 DCOMW89I-S-FILIAL-TESTEMUNHA  PIC  X(04).
             05 DCOMW89I-S-CTRL-TESTEMUNHA    PIC  9(02).