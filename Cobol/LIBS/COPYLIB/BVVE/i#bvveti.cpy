      *================================================================*
      * I#BVVETI - TRAVA AUTOMATICA PARA PRODUTO X BANDEIRA            *
      *            AREA DO VSAM TRAUPDBD                               *
      *----------------------------------------------------------------*
      * DSN  : CB.BACC.VON.TRAUPDBD                                    *
      * LRECL: 80 / CHAVE: 5 - DESLOCAMENTO DE 3 BYTES                 *
      *================================================================*
       01  REG-BVVETI.
           05 BVVETI-BANDEIRA          PIC  9(003).
           05 BVVETI-CHAVE.
              10 BVVETI-PRODUTO        PIC  9(005).
           05 BVVETI-USUARIO           PIC  X(009).
           05 BVVETI-TIMESTAMP         PIC  X(026).
           05 BVVETI-DESCRICAO         PIC  X(030).
           05 BVVETI-FILLER-01         PIC  X(007).
