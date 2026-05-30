      *================================================================*
      *  I#BVVEDV - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  PARAMETRIZACAO DO PARECER DO VAREJO                           *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.PARECPVJ
      *  LRECL ARQUIVO: 080                                            *
      *  LRECL CHAVE  : 040                                            *
      *================================================================*

       01  BVVEDV-PARECPVJ.
           05 BVVEDV-CHAVE.
               10 BVVEDV-CH-TIMESTAMP          PIC  X(020).
               10 BVVEDV-CH-FILLER             PIC  X(020).
           05 BVVEDV-PARECER                   PIC  X(001).
      *       S = SIM
      *       N = NAO
           05 BVVEDV-USUARIO                   PIC  X(009).
           05 BVVEDV-TERMINAL                  PIC  X(008).
           05 FILLER                           PIC  X(022).
