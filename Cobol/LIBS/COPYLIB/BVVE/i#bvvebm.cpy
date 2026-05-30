      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVEBM                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE PARAMETRIZACAO DE BIM - CARTAO   *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : KAUE SANTOS                                         *
      * TAMANHO  : 100  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * PARM-BIM-CH-BIM              | COD. BIM                        *
      * PARM-BIM-DESCRICAO           | DESCRICAO BIM DO CARTAO         *
      * PARM-BIM-DT-INCLUSAO         | DATA INCLUSAO                   *
      * PARM-BIM-USUARIO             | CODIGO USUARIO INCLUSAO         *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  PARM-BIM-CARTAO.
           05 PARM-BIM-CH.
              10 PARM-BIM-CH-BIM                 PIC 9(005).
           05 PARM-BIM-DESCRICAO                 PIC X(025).
           05 PARM-BIM-DT-INCLUSAO               PIC 9(008).
           05 PARM-BIM-USUARIO                   PIC X(009).
           05 PARM-BIM-FILLER                    PIC X(053).
