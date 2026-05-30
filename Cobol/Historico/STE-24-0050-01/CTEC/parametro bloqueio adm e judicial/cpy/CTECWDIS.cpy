      ******************************************************************
      * NOME BOOK : CTECWDIS                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1ATL                                           *
      * TAMANHO   : 03165                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDIS-HEADER.
              10 CTECWDIS-COD-LAYOUT         PIC X(08) VALUE 'CTECWDIS'.
              10 CTECWDIS-TAM-LAYOUT         PIC 9(05) VALUE 03165.
           05 CTECWDIS-REGISTRO.
              10 CTECWDIS-BLOCO-SAIDA.
                 15 CTECWDIS-S-NUM-REG       PIC 9(02).
                 15 CTECWDIS-S-LISTA         OCCURS 0 
                                             TO 50 TIMES
                                             DEPENDING 
                                             ON CTECWDIS-S-NUM-REG.
                    20 CTECWDIS-S-CMOTVO-SIT-CTA          PIC 9(03).
                    20 CTECWDIS-S-RMOTVO-SIT-CTA          PIC X(60).
