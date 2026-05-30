      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEOP                                            *
      * DESCRICAO: BOOK - ARQUIVO OUTRAS PARAMETRIZACOES               *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-OP-CH-CODPROD           | CHAVE CODIGO DO PRODUTO         *
      * PARM-OP-QTDEXP               | QUANTIDADE DE DIAS PARA EXPURGO *
      * PARM-OP-QTDMAX               | PRAZO MAX. P/ PGMTO PRAZO(DIAS) *
      * PARM-OP-QTDVAL               | PRAZO MAX. P/ VLDD LIMITE(DIAS) *
      * PARM-OP-MARGEM               | MARGEM DE SEGURANCA (PERCENTUAL)*
      * PARM-OP-PRZATE               | VALOR INALTERADO ATE PRAZO XXX  *
      * PARM-OP-MPDE02               | VALOR X2 DE                     *
      * PARM-OP-MPAT02               | VALOR X2 ATE                    *
      * PARM-OP-MPDE03               | VALOR X3 DE                     *
      * PARM-OP-MPAT03               | VALOR X3 ATE                    *
      * PARM-OP-MPDE04               | VALOR X4 DE                     *
      * PARM-OP-MPAT04               | VALOR X4 ATE                    *
      * PARM-DT-INCLUSAO             | DATA INCLUSAO                   *
      * PARM-USUARIO                 | CODIGO USUARIO INCLUSAO         *
      * PARM-ID-MANUT                | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-OUTRAS-PARM.
           05 PARM-OP-CH.
              10 PARM-OP-CH-CODPROD              PIC 9(003).
           05 PARM-OP-QTDEXP                     PIC 9(003).
           05 PARM-OP-QTDMAX                     PIC 9(003).
           05 PARM-OP-QTDVAL                     PIC 9(003).
           05 PARM-OP-MARGEM                     PIC 9(003)V9(02).
           05 PARM-OP-PRZATE                     PIC 9(003).
           05 PARM-OP-MPDE02                     PIC 9(003).
           05 PARM-OP-MPAT02                     PIC 9(003).
           05 PARM-OP-MPDE03                     PIC 9(003).
           05 PARM-OP-MPAT03                     PIC 9(003).
           05 PARM-OP-MPDE04                     PIC 9(003).
           05 PARM-OP-MPAT04                     PIC 9(003).
           05 PARM-OP-DT-INCLUSAO                PIC 9(008).
           05 PARM-OP-USUARIO                    PIC X(009).
           05 PARM-OP-FILLER                     PIC X(045).
