      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESQ                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            TARIFAS POR QUANTIDADE DE CARTOES                   *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-TFQT-CH-CODIGO-PROD    | CHAVE CODIGO PRODUTO            *
      * SPARM-TFQT-CH-CODIGO-TAR     | CHAVE CODIGO TARIFA             *
      * SPARM-TFQT-CH-FORMA-PAGTO    | CHAVE FORMA DE PAGAMENTO        *
      * SPARM-TFQT-CH-QTD-CARTAO-ATE | CHAVE QUANTIDADE DE CARTAO      *
      * SPARM-TFQT-CH-PRAZO-PAGTO    | CHAVE PRAZO DE PAGAMENTO        *
      * SPARM-TFQT-CH-FATMT-PREVT    | CHAVE FATURAMENTO PREVISTO      *
      * SPARM-TFQT-TARIF-OBRIG       | DOMINIO TARIFA OBRIGATORIA:     *
      *                              |    S - SIM                      *
      *                              |    N - NAO                      *
      * SPARM-TFQT-TARIF-FORMT       | DOMINIO FORMATO TARIFA:         *
      *                              |    P - PERCENTUAL               *
      *                              |    R - REAL                     *
      * SPARM-TFQT-VALOR             | VALOR TARIFA                    *
      * SPARM-TFQT-DT-INCLUSAO       | DATA INCLUSAO                   *
      * SPARM-TFQT-USUARIO           | CODIGO USUARIO INCLUSAO         *
      * SPARM-TFQT-ID-MANUT          | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-TARIFA-QTDE.
           05 SPARM-TFQT-CH.
              10 SPARM-TFQT-CH-CODIGO-PROD       PIC 9(003).
              10 SPARM-TFQT-CH-CODIGO-TARI       PIC 9(003).
              10 SPARM-TFQT-CH-FORMA-PAGTO       PIC X(001).
              10 SPARM-TFQT-CH-QTD-CARTAO-ATE    PIC 9(006).
              10 SPARM-TFQT-CH-PRAZO-PAGTO       PIC 9(003).
              10 SPARM-TFQT-CH-FATMT-PREVT       PIC 9(009)V99.
           05 SPARM-TFQT-TARIF-OBRIG             PIC X(001).
           05 SPARM-TFQT-TARIF-FORMT             PIC X(001).
           05 SPARM-TFQT-VALOR                   PIC 9(003)V99.
           05 SPARM-TFQT-DT-INCLUSAO             PIC 9(008).
           05 SPARM-TFQT-USUARIO                 PIC X(009).
           05 SPARM-TFQT-ID-MANUT                PIC X(001).
           05 SPARM-TFQT-FILLER                  PIC X(048).
