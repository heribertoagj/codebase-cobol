      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESB                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            BENEFICIO MINIMO X MAXIMO                           *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-BENE-CH                | CHAVE CODIGO BENEFICIO MIN MAX  *
      * SPARM-BENE-VALOR-MIN         | VALOR BENEFICIO MINIMO          *
      * SPARM-BENE-VALOR-MAX         | VALOR BENEFICIO MAXIMO          *
      * SPARM-BENE-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-BENE-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-BENE-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-BENEFICIO-MIN-MAX.
           05 SPARM-BENE-CH.
              10 SPARM-BENE-CH-CODIGO-PROD       PIC 9(003).
           05 SPARM-BENE-VALOR-MIN               PIC 9(005)V99.
           05 SPARM-BENE-VALOR-MAX               PIC 9(005)V99.
           05 SPARM-BENE-DT-INCLUSAO             PIC 9(008).
           05 SPARM-BENE-USUARIO                 PIC X(009).
           05 SPARM-BENE-ID-MANUT                PIC X(001).
           05 SPARM-BENE-FILLER                  PIC X(065).
