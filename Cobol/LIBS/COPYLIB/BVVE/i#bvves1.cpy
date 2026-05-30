      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVES1                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            MEIO DE PAGAMENTO                                   *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : KAUE SANTOS                                         *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-CART-CH-CARTEIRA       | CARTEIRA                        *
      * SPARM-CART-DESCRICAO         | DESCRICAO MEIO DE PAGAMENTO     *
      * SPARM-CART-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-CART-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-PGTO-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    C - CONSULTAR                *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-CARTEIRA.
           05 SPARM-CART-CH.
              10 SPARM-CART-CH-CARTEIRA          PIC X(003).
           05 SPARM-CART-DESCRICAO               PIC X(025).
           05 SPARM-CART-DT-INCLUSAO             PIC 9(008).
           05 SPARM-CART-USUARIO                 PIC X(009).
           05 SPARM-CART-ID-MANUT                PIC X(001).
           05 SPARM-CART-FILLER                  PIC X(054).
