      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESC                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            CNPJ COM RESTRICAO                                  *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-CNPJ-CH                | CHAVE CNPJ                      *
      * SPARM-CNPJ-REST-MOTIVO       | MOTIVO RESTRICAO                *
      * SPARM-CNPJ-REST-DT-INCLUSAO  | DATA INCLUSAO                   *
      * SPARM-CNPJ-REST-USUARIO      | CODIGO USUARIO INCLUSAO         *
      * SPARM-CNPJ-REST-ID-MANUT     | DOMINIO MANUTENCAO:             *
      *                              |    I - INCLUSAO                 *
      *                              |    A - ALTERACAO                *
      *                              |    E - EXCLUIR                  *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-CNPJ-RESTRICAO.
           05 SPARM-CNPJ-REST-CH.
7C2511*       10 SPARM-CNPJ-RESTRICAO-CH         PIC 9(015).
7C2511        10 SPARM-CNPJ-RESTRICAO-CH         PIC X(015).
           05 SPARM-CNPJ-REST-MOTIVO             PIC X(020).
           05 SPARM-CNPJ-REST-DT-INCLUSAO        PIC 9(008).
           05 SPARM-CNPJ-REST-USUARIO            PIC X(009).
           05 SPARM-CNPJ-REST-ID-MANUT           PIC X(001).
           05 SPARM-CNPJ-FILLER                  PIC X(047).
