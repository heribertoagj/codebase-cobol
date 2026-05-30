      ******************************************************************
      * NOME BOOK : CTECWDHS                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1ASL                                           *
      * TAMANHO   : 00474                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDHS-HEADER.
              10 CTECWDHS-COD-LAYOUT         PIC X(08) VALUE 'CTECWDHS'.
              10 CTECWDHS-TAM-LAYOUT         PIC 9(05) VALUE 00474.
           05 CTECWDHS-REGISTRO.
              10 CTECWDHS-BLOCO-SAIDA.
                 15 CTECWDHS-S-NUM-REG       PIC 9(01).
                 15 CTECWDHS-S-LISTA         OCCURS 0 
                                             TO 10 TIMES
                                             DEPENDING 
                                             ON CTECWDHS-S-NUM-REG.
                    20 CTECWDHS-S-CINDCD-TPO-JUDIC        PIC X(01).
                    20 CTECWDHS-S-IINDCD-TPO-JUDIC        PIC X(45).
