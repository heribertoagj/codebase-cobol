      *****************************************************************
      * NOME DA INC - I#GFCTU7                                        *
      * DESCRICAO   - ARQUIVO CART. COM CPF                           *        X
      * TAMANHO     - 0030                                            *        X
      * DATA        - 06.2008                                         *        X
      * RESPONSAVEL - WAGNER - PROCWORK                               *        X
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  REG-CRESP1.
           03  GFCTU7-AGENCIA                       PIC  9(05).
           03  GFCTU7-CONTA                         PIC  9(07).
           03  GFCTU7-CARTEIRA                      PIC  X(03).                X
ST2507*    03  GFCTU7-CPF-NUM                       PIC  9(09).                X
ST2507     03  GFCTU7-CPF-NUM                       PIC  X(09).                X
           03  GFCTU7-CPF-CTR                       PIC  9(02).                X
           03  FILLER                               PIC  X(04).
