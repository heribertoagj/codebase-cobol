      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMW91I                                          *
      *----------------------------------------------------------------*
      *    Comarea de saída utilizada por DCOM3Z5O e correlatos        *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMW91I-BLOCO-SAIDA.
             05 DCOMW91I-S-AGENCIA             PIC  9(05).
             05 DCOMW91I-S-CONTA-CORRENTE      PIC  9(13).
             05 DCOMW91I-S-DIGITO-CONTA        PIC  X(01).
             05 DCOMW91I-S-DATA-OPERACAO       PIC  9(08).
             05 DCOMW91I-S-NR-OPERACAO         PIC  9(10).
             05 DCOMW91I-S-ENDERECO-CLIENTE    PIC  X(60).
             05 DCOMW91I-S-CIDADE-CLIENTE      PIC  X(40).
             05 DCOMW91I-S-ESTADO-CLIENTE      PIC  X(02).
             05 DCOMW91I-S-NOME-AGENCIA        PIC  X(40).
             05 DCOMW91I-S-VLR-TIT-AT          PIC  9(15)V99.

             05 DCOMW91I-S-NOME-CLIENTE        PIC  X(40).
4S2511*      05 DCOMW91I-S-CPF-CLIENTE         PIC  9(09).
4S2511       05 DCOMW91I-S-CPF-CLIENTE         PIC  X(09).
4S2511*      05 DCOMW91I-S-FILIAL-CLIENTE      PIC  9(04).
4S2511       05 DCOMW91I-S-FILIAL-CLIENTE      PIC  X(04).
             05 DCOMW91I-S-CTRL-CLIENTE        PIC  9(02).

             05 DCOMW91I-S-NUM-TITULO          PIC  9(07).
             05 DCOMW91I-S-COD-CONTRATO        PIC  9(10).
             05 DCOMW91I-S-DATA-VENC-ORIG      PIC  9(08).
             05 DCOMW91I-S-DATA-VENC-PRRG      PIC  9(08).
             05 DCOMW91I-S-VLT-TIT-NV          PIC  9(15)V99.
             05 DCOMW91I-S-VLR-TIT-EXT-AT      PIC  X(190).
             05 DCOMW91I-S-VLR-TIT-EXT-NV      PIC  X(190).
             05 DCOMW91I-S-NOME-SAC-AT         PIC  X(40).
             05 DCOMW91I-S-NOME-SACADO         PIC  X(40).
             05 DCOMW91I-S-ENDERC-SAC-AT       PIC  X(60).
             05 DCOMW91I-S-ENDERC-SAC-NV       PIC  X(60).

4S2511*      05 DCOMW91I-S-CNPJ-CPF-SAC-AT     PIC  9(09).
4S2511       05 DCOMW91I-S-CNPJ-CPF-SAC-AT     PIC  X(09).
4S2511*      05 DCOMW91I-S-FILIAL-SAC-AT       PIC  9(04).
4S2511       05 DCOMW91I-S-FILIAL-SAC-AT       PIC  X(04).
             05 DCOMW91I-S-CTRL-SAC-AT         PIC  9(02).

4S2511*      05 DCOMW91I-S-CNPJ-CPF-SAC-NV     PIC  9(09).
4S2511       05 DCOMW91I-S-CNPJ-CPF-SAC-NV     PIC  X(09).
4S2511*      05 DCOMW91I-S-FILIAL-SAC-N        PIC  9(04).
4S2511       05 DCOMW91I-S-FILIAL-SAC-N        PIC  X(04).
             05 DCOMW91I-S-CTRL-SAC-N          PIC  9(02).

             05 DCOMW91I-S-TAX-JUROS-AM-AT     PIC  9(06)V9(07).
             05 DCOMW91I-S-TAX-JUROS-AM-NV     PIC  9(06)V9(07).
             05 DCOMW91I-S-TAX-JUROS-AA-AT     PIC  9(06)V9(07).
             05 DCOMW91I-S-TAX-JUROS-AA-NV     PIC  9(06)V9(07).

             05 DCOMW91I-S-NOME-AVAL-AT        PIC  X(40).
             05 DCOMW91I-S-NOME-AVAL-NV        PIC  X(40).
4S2511*      05 DCOMW91I-S-CNPJ-AVAL-AT        PIC  9(09).
4S2511       05 DCOMW91I-S-CNPJ-AVAL-AT        PIC  X(09).
4S2511*      05 DCOMW91I-S-FILIAL-AVAL-AT      PIC  9(04).
4S2511       05 DCOMW91I-S-FILIAL-AVAL-AT      PIC  X(04).
             05 DCOMW91I-S-CTRL-AVAL-AT        PIC  9(02).

4S2511*      05 DCOMW91I-S-CNPJ-AVAL-NV        PIC  9(09).
4S2511       05 DCOMW91I-S-CNPJ-AVAL-NV        PIC  X(09).
4S2511*      05 DCOMW91I-S-FILIAL-AVAL-NV      PIC  9(04).
4S2511       05 DCOMW91I-S-FILIAL-AVAL-NV      PIC  X(04).
             05 DCOMW91I-S-CTRL-AVAL-NV        PIC  9(02).