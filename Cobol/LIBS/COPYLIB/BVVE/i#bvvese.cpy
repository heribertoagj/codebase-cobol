      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESE                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            TIPOS DE EMPRESA                                    *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 150  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-EMPR-CH                | CHAVE TIPO EMPRESA              *
      * SPARM-EMPR-DESCRICAO         | DESCRICAO TIPO EMPRESA          *
      * SPARM-EMPR-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-EMPR-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-EMPR-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-EMPRESA.
           05 SPARM-EMPR-CH.
              10 SPARM-EMPR-CH-CODIGO            PIC 9(002).
           05 SPARM-EMPR-DESCRICAO               PIC X(070).
           05 SPARM-EMPR-DT-INCLUSAO             PIC 9(008).
           05 SPARM-EMPR-USUARIO                 PIC X(009).
           05 SPARM-EMPR-ID-MANUT                PIC X(001).
           05 SPARM-EMPR-FILLER                  PIC X(060).
