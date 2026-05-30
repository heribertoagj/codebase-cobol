      *----------------------------------------------------------------*
      *    BRQ                                                         *
      *----------------------------------------------------------------*
      *    CENTRO CUSTO:   DCOM - DESCONTO COMERCIAL                   *
      *    PROJETO.....:   REDISTRIBUICAO DE LIMITES NO MOBILE BANKING *
      *    PROGRAMA....:   DCOM9869                                    *
      *    BOOK........:   DCOMWEHP                                    *
      *    PROGRAMADOR.:   LUIS EDUARDO                                *
      *    DATA........:   14/05/2021                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   HEADER DE COMUNICACAO COM OS PERIFERICOS    *
      *----------------------------------------------------------------*
      * OBSERVACOES:                                                   *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      * ALTERACOES:                                                    *
      *                                                                *
      * DATA:    NOME:     DESCRICAO:                                  *
      * XX/XX/XX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                *
      *----------------------------------------------------------------*
           05  DCOMWEHP-REGISTRO.
               10  DCOMWEHP-BLOCOA.
                   15  DCOMWEHP-TRX                    PIC X(4).
                   15  DCOMWEHP-ID                     PIC X(3).
                   15  DCOMWEHP-NUM-PERIF              PIC X(8).
                   15  DCOMWEHP-PERIF-ANT              PIC X.
                   15  DCOMWEHP-LU-RESPOSTA            PIC 9.
                       88  DCOMWEHP-LU-RESPOSTA-MESMA  VALUE 0.
                       88  DCOMWEHP-LU-RESPOSTA-1      VALUE 1.
                       88  DCOMWEHP-LU-RESPOSTA-2      VALUE 2.
                       88  DCOMWEHP-LU-RESPOSTA-3      VALUE 3.
                       88  DCOMWEHP-LU-RESPOSTA-4      VALUE 4.
               10  DCOMWEHP-BLOCOB.
                   15  DCOMWEHP-AG-ORIGEM              PIC 9(4).
                   15  DCOMWEHP-SEQ-PERIF              PIC X(6).
                   15  DCOMWEHP-AG-DESTINO             PIC 9(4).
                   15  DCOMWEHP-ANULACAO               PIC X(20).
                   15  DCOMWEHP-COD-PERIF              PIC 9(3).
                   15  DCOMWEHP-CTRLE-TX               PIC X(2).
                   15  DCOMWEHP-RCODE1                 PIC X(1).
                   15  DCOMWEHP-RESERVA                PIC X(23).
      *----------------------------------------------------------------*
