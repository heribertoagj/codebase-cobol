      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVES2                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            BIM CARTAO                                          *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : KAUE SANTOS                                         *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-CART-CH-BIM            | CODIGO DO BIM                   *
      * SPARM-CART-DESCRICAO         | DESCRICAO BIM DO CARTAO         *
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
       01  SPARM-BIM-CARTAO.
           05 SPARM-BIM-CH.
              10 SPARM-BIM-CH-BIM                PIC 9(005).
           05 SPARM-BIM-DESCRICAO                PIC X(025).
           05 SPARM-BIM-DT-INCLUSAO              PIC 9(008).
           05 SPARM-BIM-USUARIO                  PIC X(009).
           05 SPARM-BIM-ID-MANUT                 PIC X(001).
           05 SPARM-BIM-FILLER                   PIC X(052).
