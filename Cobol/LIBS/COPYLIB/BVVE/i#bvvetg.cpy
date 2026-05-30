      *================================================================*
      * I#BVVETG - TRAVA AUTOMATICA                                    *
      *            CONTROLE CICLICO DA TRAVA AUTOMATICA                *
      *----------------------------------------------------------------*
      * LRECL: 100 / CHAVE: 50                                         *
      *================================================================*
       01  BVVETG-TRAUCICL.
           05 BVVETG-CHAVE.
              10 BVVETG-CH-PERIODO-INI   PIC  X(26).
              10 BVVETG-CH-RESERVA       PIC  X(24).
           05 BVVETG-PERIODO-FIM         PIC  X(26).
           05 BVVETG-RESERVA             PIC  X(24).
