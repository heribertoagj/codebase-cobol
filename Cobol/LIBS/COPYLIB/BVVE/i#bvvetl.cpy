      *================================================================*
      * I#BVVETL - TRAVA AUTOMATICA PARA DESTRAVA AUTOMATICA           *
      *            AREA DO VSAM TRAUDVAT                               *
      *----------------------------------------------------------------*
      * DSN  : CB.BACC.VON.TRAUDVAT                                    *
      * LRECL: 70 / CHAVE: 30                                          *
      *================================================================*
       01  REG-BVVETL.
           05 BVVETL-CHAVE.
              10 BVVETL-DATA           PIC  X(008).
              10 BVVETL-HORA           PIC  X(006).
              10 BVVETL-FILLER-01      PIC  X(016).
           05 BVVETL-SIM-NAO           PIC  X(001).
           05 BVVETL-USUARIO           PIC  X(009).
           05 BVVETL-FILLER-02         PIC  X(030).
