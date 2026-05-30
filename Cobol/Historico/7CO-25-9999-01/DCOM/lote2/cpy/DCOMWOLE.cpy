      *----------------------------------------------------------------*
      *    COPYBOOK: DCOMWOLE                                           *
      *----------------------------------------------------------------*
      *    Bloco de entrada para listagem de contratos                 *
      *    Ajuste CNPJ/CPF e FILIAL para formato alfanumérico          *
      ******************************************************************

       01 DCOMWOLE-BLOCO-ENTRADA.
           05 DCOMWOLE-BLOCO-LISTACONTR    PIC  X(30).
           05 DCOMWOLE-E-AGENCIA           PIC  9(05).
           05 DCOMWOLE-E-CONTA             PIC  9(13).
           05 DCOMWOLE-E-CSEQ-TTLAR        PIC  9(07).
4S2511*      05 DCOMWOLE-E-CCPF-CNPJ       PIC  9(09).
4S2511       05 DCOMWOLE-E-CCPF-CNPJ       PIC  X(09).
4S2511*      05 DCOMWOLE-E-CFLIAL-CNPJ     PIC  9(04).
4S2511       05 DCOMWOLE-E-CFLIAL-CNPJ     PIC  X(04).
           05 DCOMWOLE-E-CCTRL-CPF-CNPJ    PIC  9(02).
           05 DCOMWOLE-E-DATA-INICIO       PIC  X(10).
           05 DCOMWOLE-E-DATA-FIM          PIC  X(10).