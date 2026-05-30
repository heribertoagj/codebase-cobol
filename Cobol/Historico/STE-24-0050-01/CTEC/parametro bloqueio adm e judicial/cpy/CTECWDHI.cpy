      ******************************************************************
      * NOME BOOK : CTECWDHI - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3ASL                                           *
      * TAMANHO   : 02315 BYTES                                        *
      ******************************************************************
           05 CTECWDHI-HEADER.
              10 CTECWDHI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDHI'.
              10 CTECWDHI-TAM-LAYOUT        PIC 9(005) VALUE 00475.
           05 CTECWDHI-REGISTRO.
              10 CTECWDHI-BLOCO-ENTRADA.
                 15 CTECWDHI-E-CINDCD-TPO-JUDIC   PIC X(01).
              10 CTECWDHI-BLOCO-SAIDA.
                 15 CTECWDHI-S-NUM-REG      PIC 9(01).
                 15 CTECWDHI-S-LISTA        OCCURS 0 
                                            TO 10 TIMES
                                            DEPENDING 
                                            ON CTECWDHI-S-NUM-REG.
                    20 CTECWDHI-S-CINDCD-TPO-JUDIC      PIC X(01).
                    20 CTECWDHI-S-IINDCD-TPO-JUDIC      PIC X(45).
