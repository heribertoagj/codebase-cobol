      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEST                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            TARIFAS                                             *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-TARI-CH                | CHAVE TIPO TARIFA               *
      * SPARM-TARI-DESCRICAO         | DESCRICAO TIPO TARIFA           *
      * SPARM-TARI-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-TARI-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-TARI-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-TARIFA.
           05 SPARM-TARI-CH.
              10 SPARM-TARI-CH-CODIGO            PIC 9(003).
           05 SPARM-TARI-DESCRICAO               PIC X(040).
           05 SPARM-TARI-DT-INCLUSAO             PIC 9(008).
           05 SPARM-TARI-USUARIO                 PIC X(009).
           05 SPARM-TARI-ID-MANUT                PIC X(001).
           05 SPARM-TARI-FILLER                  PIC X(039).
