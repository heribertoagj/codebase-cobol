      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESC                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE PARAMETRIZACAO DE                *
      *            CNPJ COM RESTRICAO                                  *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-CNPJ-CH                 | CHAVE CNPJ                      *
      * PARM-CNPJ-REST-MOTIVO        | MOTIVO RESTRICAO                *
      * PARM-CNPJ-REST-DT-INCLUSAO   | DATA INCLUSAO                   *
      * PARM-CNPJ-REST-USUARIO       | CODIGO USUARIO INCLUSAO         *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-CNPJ-RESTRICAO.
           05 PARM-CNPJ-REST-CH.
              10 PARM-CNPJ-RESTRICAO-CH          PIC 9(015).
           05 PARM-CNPJ-REST-MOTIVO              PIC X(020).
           05 PARM-CNPJ-REST-DT-INCLUSAO         PIC 9(008).
           05 PARM-CNPJ-REST-USUARIO             PIC X(009).
           05 PARM-CNPJ-FILLER                   PIC X(048).
