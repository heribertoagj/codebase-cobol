      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVECR                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE PARAMETRIZACAO DE CARTEIRAS      *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : KAUE SANTOS                                         *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-CART-CH-CARTEIRA        | CARTEIRA                        *
      * PARM-CART-DESCRICAO          | DESCRICAO CARTEIRA              *
      * PARM-CART-DT-INCLUSAO        | DATA INCLUSAO                   *
      * PARM-CART-USUARIO            | CODIGO USUARIO INCLUSAO         *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-CARTEIRA.
           05 PARM-CART-CH.
              10 PARM-CART-CH-CARTEIRA           PIC X(003).
           05 PARM-CART-DESCRICAO                PIC X(025).
           05 PARM-CART-DT-INCLUSAO              PIC 9(008).
           05 PARM-CART-USUARIO                  PIC X(009).
           05 PARM-CART-FILLER                   PIC X(055).
