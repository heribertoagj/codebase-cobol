      ******************************************************************
      * SISTEMA : DCOM - I#DCOMIM                                      *
      * TAMANHO : 064 BYTES       --->> AVISO MOVIMENTACAO EVENTUAL    *
      * ARQUIVO : ARQUIVO COM CNPJ'S E PERIODO DOS CLIENTES A EMITIR   *
      ******************************************************************
       01  IM-REGISTRO.
  1  9     03  IM-CCNPJ-CPF                 PIC 9(09).
 10  5     03  IM-CFLIAL-CNPJ               PIC 9(05).
 15  2     03  IM-CCTRL-CNPJ                PIC 9(02).
 17 10     03  IM-DINIC-PER                 PIC X(10).
 27 10     03  IM-DFNAL-PER                 PIC X(10).
 37 28     03  FILLER                       PIC X(28).
