      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEMP                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE PARAMETRIZACAO DO MEIO DE        *
      *            PAGAMENTO                                           *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-PGTO-CH-FORMA           ! FORMA PAGAMANETO                *
      *                              !    A-ANTECIPADO                 *
      *                              !    P-A PRAZO                    *
      * PARM-PGTO-CH                 | CHAVE CODIGO MEIO DE PAGAMENTO  *
      * PARM-PGTO-DESCRICAO          | DESCRICAO MEIO DE PAGAMENTO     *
      * PARM-PGTO-DT-INCLUSAO        | DATA INCLUSAO                   *
      * PARM-PGTO-USUARIO            | CODIGO USUARIO INCLUSAO         *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-MEIO-PAGAMENTO.
           05 PARM-PGTO-CH.
              10 PARM-PGTO-CH-FORMA             PIC X(001).
              10 PARM-PGTO-CH-CODIGO            PIC 9(003).
           05 PARM-PGTO-DESCRICAO               PIC X(030).
           05 PARM-PGTO-SITUACAO                PIC X(001).
           05 PARM-PGTO-DT-INCLUSAO             PIC 9(008).
           05 PARM-PGTO-USUARIO                 PIC X(009).
           05 PARM-PGTO-FILLER                  PIC X(048).
