      ******************************************************************
      * NOME BOOK : GFCTW3PJ - BOOK DE ENTRADA E SAIDA                 *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * TAMANHO   : 00017 BYTES                                        *
      ******************************************************************
           05 GFCTW3PJ-REGISTRO.
              10 GFCTW3PJ-BLOCO-ENTRADA.
                 15 GFCTW3PJ-E-CGC-CNPJ   PIC X(09).
                 15 GFCTW3PJ-E-FILIAL     PIC X(04).
              10 GFCTW3PJ-BLOCO-SAIDA.
                 15 GFCTW3PJ-S-PJ-VALIDO    PIC X(02).
                    88 GFCTW3PJ-S-VALIDO    VALUE 'S'.
                    88 GFCTW3PJ-S-INVALIDO  VALUE 'N'.
                 15 GFCTW3PJ-S-COD-RETORNO  PIC X(02).
