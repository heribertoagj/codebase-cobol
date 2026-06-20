      *****************************************************************
      * NOME DA INC - I#GFCT65                                        *
      * DESCRICAO   - ARQUIVO DE CLIENTES QUE PAGARAM FICHA CADASTRAL *
      *               NOS ULTIMOS 6 MESES                             *
      * TAMANHO     - 0039                                            *
      * DATA        - 06.2008                                         *
      * RESPONSAVEL - WAGNER - PROCWORK                               *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 REG-GFCT65.
ST2507*   03 GFCT65-CPF-NUM                         PIC 9(09).
ST2507    03 GFCT65-CPF-NUM                         PIC X(09).
          03 GFCT65-CPF-DIG                         PIC 9(02).
          03 GFCT65-AGENCIA                         PIC 9(05).
          03 GFCT65-CONTA                           PIC 9(07).
          03 GFCT65-DT-INCL-RENOV                   PIC 9(08).
          03 GFCT65-DT-CONTROLE                     PIC 9(08).
