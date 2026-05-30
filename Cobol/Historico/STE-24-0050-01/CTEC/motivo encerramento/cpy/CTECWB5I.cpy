      ******************************************************************
      * NOME BOOK : CTECWB5I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB5I-HEADER.
              10 CTECWB5I-COD-LAYOUT         PIC X(08) VALUE 'CTECWB5I'.
              10 CTECWB5I-TAM-LAYOUT         PIC 9(05) VALUE 03124.
           05 CTECWB5I-REGISTRO.
              10 CTECWB5I-BLOCO-ENTRADA.
                 15 CTECWB5I-CMOVTO-ENCERR   PIC 9(03).
                 15 CTECWB5I-HINCL-REG       PIC X(26).
                 15 CTECWB5I-RMOTVO-REJEICAO PIC X(255).
                 15 CTECWB5I-CUSUAR-MANUT    PIC X(09).