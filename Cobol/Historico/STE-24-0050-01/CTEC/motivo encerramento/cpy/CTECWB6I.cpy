      ******************************************************************
      * NOME BOOK : CTECWB6I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB6I-HEADER.
              10 CTECWB6I-COD-LAYOUT         PIC X(08) VALUE 'CTECWB6I'.
              10 CTECWB6I-TAM-LAYOUT         PIC 9(05) VALUE 03124.
           05 CTECWJCI-REGISTRO.
              10 CTECWB6I-BLOCO-ENTRADA.
                 15 CTECWB6I-CMOVTO-ENCERR   PIC 9(03).
                 15 CTECWB6I-HINCL-REG       PIC X(26).
                 15 CTECWB6I-CUSUAR-MANUT    PIC X(09).