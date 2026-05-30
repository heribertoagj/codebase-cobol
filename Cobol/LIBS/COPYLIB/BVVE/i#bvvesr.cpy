      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESR                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            RESTRICAO                                           *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-REST-CH                | CHAVE CODIGO RESTRICAO          *
      * SPARM-REST-DESCRICAO         | DESCRICAO RESTRICAO             *
      * SPARM-REST-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-REST-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-REST-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-RESTRICAO.
           05 SPARM-REST-CH.
              10 SPARM-REST-CH-CODIGO            PIC 9(002).
           05 SPARM-REST-DESCRICAO               PIC X(030).
           05 SPARM-REST-DT-INCLUSAO             PIC 9(008).
           05 SPARM-REST-USUARIO                 PIC X(009).
           05 SPARM-REST-ID-MANUT                PIC X(001).
           05 SPARM-REST-FILLER                  PIC X(050).
