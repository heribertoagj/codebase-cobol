      *================================================================*
      * I#BVVETM - TRAVA AUTOMATICA PRAZO EM DIAS PARA TRAVA AUTOMATICA*
      *            AREA DO VSAM TRAUPZTV                               *
      *----------------------------------------------------------------*
      * DSN  : CB.BACC.VON.TRAUPZTV                                    *
      * LRECL: 70 / CHAVE: 30                                          *
      *================================================================*
       01  REG-BVVETM.
           05 BVVETM-CHAVE.
              10 BVVETM-DATA           PIC  X(008).
              10 BVVETM-HORA           PIC  X(006).
              10 BVVETM-FILLER-01      PIC  X(016).
           05 BVVETM-PRAZO-DIAS        PIC  9(005).
           05 BVVETM-USUARIO           PIC  X(009).
           05 BVVETM-FILLER-02         PIC  X(026).
