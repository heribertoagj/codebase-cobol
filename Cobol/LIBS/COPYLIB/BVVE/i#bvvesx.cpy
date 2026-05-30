      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESX                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            TEXTO PROPOSTA ELETRONICA                           *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 1000 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-TXT-CH                 | CHAVE CODIGO PRODUTO            *
      * SPARM-TXT-DESCRICAO          | DESCRICAO TEXTO                 *
      * SPARM-TXT-DT-INCLUSAO        | DATA INCLUSAO                   *
      * SPARM-TXT-USUARIO            | CODIGO USUARIO INCLUSAO         *
      * SPARM-TXT-ID-MANUT           | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-TEXTO-PROPOSTA.
           05 SPARM-TXT-CH.
              10 SPARM-TXT-CH-CODIGO-PRODT       PIC 9(003).
           05 SPARM-TXT-DESCRICAO                PIC X(936).
           05 SPARM-TXT-DT-INCLUSAO              PIC 9(008).
           05 SPARM-TXT-USUARIO                  PIC X(009).
           05 SPARM-TXT-ID-MANUT                 PIC X(001).
           05 SPARM-TXT-FILLER                   PIC X(043).
