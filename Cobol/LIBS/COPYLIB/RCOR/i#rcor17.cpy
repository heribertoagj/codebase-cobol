      *----------------------> H P D 2 0 0 0 C <----------------------*
      *           HEADER DE COMUNICACAO COM OS PERIFERICOS            *
      *===============================================================*
      *                                                               *
      * ALTERACOES:                                                   *
      *                                                               *
      *.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*
      * QUANDO ALTERAR ESTA DUMMY,NAO ESQUECER DE ALT.A DUMMY HPD2000 *
      *.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*
      *                                                               *
      * DATA:         DESCRICAO:                                      *
      * XX/XXX/XXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX *
      *                                                               *
      *---------------------------------------------------------------*
           05  HPD2000C.

               10  HPD-BLOCOA.

                   15  HPD-TRX                  PIC X(4).
                   15  HPD-ID                   PIC X(3).
                   15  HPD-NUM-PERIF            PIC X(8).
                   15  HPD-PERIF-ANT            PIC X.
                   15  HPD-LU-RESPOSTA          PIC 9.
                       88  HPD-LU-RESPOSTA-MESMA VALUE 0.
                       88  HPD-LU-RESPOSTA-1     VALUE 1.
                       88  HPD-LU-RESPOSTA-2     VALUE 2.
                       88  HPD-LU-RESPOSTA-3     VALUE 3.
                       88  HPD-LU-RESPOSTA-4     VALUE 4.

               10  HPD-BLOCOB.

                   15  HPD-AG-ORIGEM            PIC 9(4).
                   15  HPD-SEQ-PERIF            PIC X(6).
                   15  HPD-AG-DESTINO           PIC 9(4).
                   15  HPD-ANULACAO             PIC X(20).
                   15  HPD-COD-PERIF            PIC 9(3).
                   15  HPD-CTRLE-TX             PIC X(2).
                   15  HPD-RCODE1               PIC X(1).
                   15  HPD-RESERVA              PIC X(23).
      *-------------------------------------------------------------*
