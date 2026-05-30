      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESP                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            PRODUTOS CONTRATADOS                                *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-PROD-CH                | CHAVE PRODUTO                   *
      * SPARM-PROD-DESCRICAO         | DESCRICAO PRODUTO               *
      * SPARM-PROD-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-PROD-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-PROD-DESTINO           | DESTINO PRODUTO:                *
      *                              |    1 - ALELO                    *
      *                              |    2 - FIDELITY                 *
      * SPARM-PROD-SITUACAO          | SITUACAO PRODUTO:               *
      *                              |    A - ATIVO                    *
      *                              |    I - INATIVO                  *
      * SPARM-PROD-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-PRODUTO.
           05 SPARM-PROD-CH.
              10 SPARM-PROD-CH-CODIGO            PIC 9(003).
           05 SPARM-PROD-DESCRICAO               PIC X(030).
           05 SPARM-PROD-DT-INCLUSAO             PIC 9(008).
           05 SPARM-PROD-USUARIO                 PIC X(009).
           05 SPARM-PROD-DESTINO                 PIC 9(002).
           05 SPARM-PROD-SITUACAO                PIC X(001).
           05 SPARM-PROD-ID-MANUT                PIC X(001).
           05 SPARM-PROD-FILLER                  PIC X(046).
