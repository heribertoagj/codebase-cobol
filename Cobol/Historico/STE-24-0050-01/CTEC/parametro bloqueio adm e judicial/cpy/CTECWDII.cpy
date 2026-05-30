      ******************************************************************
      * NOME BOOK : CTECWDII - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3ATL                                           *
      * TAMANHO   : 11124 BYTES                                        *
      ******************************************************************
           05 CTECWDII-HEADER.
              10 CTECWDII-COD-LAYOUT        PIC X(008) VALUE 'CTECWDII'.
              10 CTECWDII-TAM-LAYOUT        PIC 9(005) VALUE 11124.
           05 CTECWDII-REGISTRO.
              10 CTECWDII-BLOCO-PAGINACAO.
                 15 CTECWDII-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDII-P-INICIAL    VALUE 'I'.
                    88 CTECWDII-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDII-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDII-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDII-P-ULTIMA     VALUE 'U'.
                 15 CTECWDII-PRI-CHAVE.
                    20 CTECWDII-PRI-CMOVTO-CTA   PIC 9(03).
                 15 CTECWDII-ULT-CHAVE.
                    20 CTECWDII-ULT-CMOVTO-CTA   PIC 9(03).
              10 CTECWDII-BLOCO-ENTRADA.
                 15 CTECWDII-E-CMOVTO-SIT-CTA    PIC 9(03).
                 15 CTECWDII-E-QTD-REG           PIC 9(02).
              10 CTECWDII-BLOCO-SAIDA.
                 15 CTECWDII-S-NUM-REG      PIC 9(02).
                 15 CTECWDII-S-LISTA        OCCURS 0 
                                            TO 50 TIMES
                                            DEPENDING 
                                            ON CTECWDII-S-NUM-REG.
                    20 CTECWDII-S-CMOVTO-SIT-CTA        PIC 9(03).
                    20 CTECWDII-S-RMOVTO-SIT-CTA        PIC X(60).
