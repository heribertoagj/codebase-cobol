      *================================================================*
      * I#BVVETJ - TRAVA AUTOMATICA PARA BANDEIRA X PRODUTO            *
      *            AREA DO VSAM AIXTRAUB                               *
      *----------------------------------------------------------------*
      * DSN  : CB.BACC.VON.TRAUBDPD                                    *
      * LRECL: 80 / CHAVE: 8 - DESLOCAMENTO DE 0 BYTES                 *
      *================================================================*
       01  REG-BVVETJ.
           05 BVVETJ-CHAVE.
              10 BVVETJ-BANDEIRA       PIC  9(003).
              10 BVVETJ-PRODUTO        PIC  9(005).
           05 BVVETJ-USUARIO           PIC  X(009).
           05 BVVETJ-TIMESTAMP         PIC  X(026).
           05 BVVETJ-DESCRICAO         PIC  X(030).
           05 BVVETJ-FILLER-01         PIC  X(007).
