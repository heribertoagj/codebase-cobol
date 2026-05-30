      ******************************************************************
      * NOME BOOK : GFCTW2PJ - BOOK DE ENTRADA E SAIDA                 *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 06/2025                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * TAMANHO   : 00017 BYTES                                        *
      ******************************************************************
           05 GFCTW2PJ-REGISTRO.
              10 GFCTW2PJ-BLOCO-ENTRADA.
                 15 GFCTW2PJ-E-CGC-CNPJ   PIC X(09).
                 15 GFCTW2PJ-E-FILIAL     PIC X(04).
              10 GFCTW2PJ-BLOCO-SAIDA.
                 15 GFCTW2PJ-S-PJ-VALIDO    PIC X(02).
                    88 GFCTW2PJ-S-VALIDO    VALUE 'S'.
                    88 GFCTW2PJ-S-INVALIDO  VALUE 'N'.
                 15 GFCTW2PJ-S-COD-RETORNO  PIC X(02).
