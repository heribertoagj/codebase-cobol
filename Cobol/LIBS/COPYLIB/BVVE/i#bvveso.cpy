      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESO                                            *
      * DESCRICAO: BOOK - ARQUIVO SOLICITACAO OUTRAS PARAMETRIZACOES   *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-OP-CH-CODPROD          | CHAVE CODIGO DO PRODUTO         *
      * SPARM-OP-QTDEXP              | QUANTIDADE DE DIAS PARA EXPURGO *
      * SPARM-OP-QTDMAX              | PRAZO MAX. P/ PGMTO PRAZO(DIAS) *
      * SPARM-OP-QTDVAL              | PRAZO MAX. P/ VLDD LIMITE(DIAS) *
      * SPARM-OP-MARGEM              | MARGEM DE SEGURANCA (PERCENTUAL)*
      * SPARM-OP-PRZATE              | VALOR INALTERADO ATE PRAZO XXX  *
      * SPARM-OP-MPDE02              | VALOR X2 DE                     *
      * SPARM-OP-MPAT02              | VALOR X2 ATE                    *
      * SPARM-OP-MPDE03              | VALOR X3 DE                     *
      * SPARM-OP-MPAT03              | VALOR X3 ATE                    *
      * SPARM-OP-MPDE04              | VALOR X4 DE                     *
      * SPARM-OP-MPAT04              | VALOR X4 ATE                    *
      * SPARM-DT-INCLUSAO            | DATA INCLUSAO                   *
      * SPARM-USUARIO                | CODIGO USUARIO INCLUSAO         *
      * SPARM-ID-MANUT               | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-OUTRAS-PARM.
           05 SPARM-OP-CH.
              10 SPARM-OP-CH-CODPROD             PIC 9(003).
           05 SPARM-OP-QTDEXP                    PIC 9(003).
           05 SPARM-OP-QTDMAX                    PIC 9(003).
           05 SPARM-OP-QTDVAL                    PIC 9(003).
           05 SPARM-OP-MARGEM                    PIC 9(003)V9(02).
           05 SPARM-OP-PRZATE                    PIC 9(003).
           05 SPARM-OP-MPDE02                    PIC 9(003).
           05 SPARM-OP-MPAT02                    PIC 9(003).
           05 SPARM-OP-MPDE03                    PIC 9(003).
           05 SPARM-OP-MPAT03                    PIC 9(003).
           05 SPARM-OP-MPDE04                    PIC 9(003).
           05 SPARM-OP-MPAT04                    PIC 9(003).
           05 SPARM-OP-DT-INCLUSAO               PIC 9(008).
           05 SPARM-OP-USUARIO                   PIC X(009).
           05 SPARM-OP-ID-MANUT                  PIC X(001).
           05 SPARM-OP-FILLER                    PIC X(044).
