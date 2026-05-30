      *----------------------------------------------------------------*
      *  I#BVVEDN - MANUTENCAO DE DOMICILIO BANCARIO - CIP             *
      *  SEGMENTO DE CONTROLE BCMSG                                    *
      *----------------------------------------------------------------*
       01  BVVEDN-BCMSG.
           03  BVVEDN-FIXO-1               PIC X(01).
           03  BVVEDN-IDENTD-EMISSOR       PIC X(08).
           03  BVVEDN-FIXO-2               PIC X(01).
           03  BVVEDN-IDENTD-DESTINATARIO  PIC X(08).
      *    03  BVVEDN-IDENTD-CONTG         PIC X(08).
      *    03  BVVEDN-IDENTD-OPERAD        PIC X(32).
      *    03  BVVEDN-IDENTD-OPERAD-CONFC  PIC X(32).
           03  BVVEDN-GRUPO-SEQ.
               05  BVVEDN-NUM-SEQ          PIC 9(09).
               05  BVVEDN-INDR-CONT        PIC X(01).
           03  BVVEDN-DOM-SIST             PIC X(05).
      *    03  BVVEDN-NU-OP                PIC X(23).
           03  BVVEDN-NU-OP                PIC X(18).

