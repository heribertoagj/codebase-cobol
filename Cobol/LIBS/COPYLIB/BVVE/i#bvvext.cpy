      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEXT                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE PARAMETRIZACAO DE                *
      *            TEXTO PROPOSTA ELETRONICA                           *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : JULIANA TURCO                                       *
      * TAMANHO  : 1000 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-TXT-CH                  | CHAVE CODIGO PRODUTO            *
      * PARM-TXT-DESCRICAO           | DESCRICAO TEXTO                 *
      * PARM-TXT-DT-INCLUSAO         | DATA INCLUSAO                   *
      * PARM-TXT-USUARIO             | CODIGO USUARIO INCLUSAO         *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-TEXTO-PROPOSTA.
           05 PARM-TXT-CH.
              10 PARM-TXT-CH-CODIGO-PRODT        PIC 9(003).
           05 PARM-TXT-DESCRICAO                 PIC X(936).
           05 PARM-TXT-DT-INCLUSAO               PIC 9(008).
           05 PARM-TXT-USUARIO                   PIC X(009).
           05 PARM-TXT-FILLER                    PIC X(044).
