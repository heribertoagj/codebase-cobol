      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESM                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            MEIO DE PAGAMENTO                                   *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-PGTO-CH-FORMA          ! FORMA PAGAMANETO                *
      *                              !    A-ANTECIPADO                 *
      *                              !    P-A PRAZO                    *
      * SPARM-PGTO-CH                | CHAVE CODIGO MEIO DE PAGAMENTO  *
      * SPARM-PGTO-DESCRICAO         | DESCRICAO MEIO DE PAGAMENTO     *
      * SPARM-PGTO-SITUACAO          | SITUACAO MEIO DE PAGAMENTO:     *
      *                              |    A - ATIVO                    *
      *                              |    I - INATIVO                  *
      * SPARM-PGTO-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-PGTO-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-PGTO-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-MEIO-PAGAMENTO.
           05 SPARM-PGTO-CH.
              10 SPARM-PGTO-CH-FORMA             PIC X(001).
              10 SPARM-PGTO-CH-CODIGO            PIC 9(003).
           05 SPARM-PGTO-DESCRICAO               PIC X(030).
           05 SPARM-PGTO-SITUACAO                PIC X(001).
           05 SPARM-PGTO-DT-INCLUSAO             PIC 9(008).
           05 SPARM-PGTO-USUARIO                 PIC X(009).
           05 SPARM-PGTO-ID-MANUT                PIC X(001).
           05 SPARM-PGTO-FILLER                  PIC X(047).
