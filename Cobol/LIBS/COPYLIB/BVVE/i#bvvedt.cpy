      *================================================================*
      *  I#BVVEDT - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  PARAMETRO PARA SELECAO DE TRAVAS A VENCER EM DIAS             *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.TRAVPDIA
      *  LRECL ARQUIVO: 120                                            *
      *  LRECL CHAVE  : 030                                            *
      *================================================================*

       01  BVVEDT-TRAVPDIA.
           05 BVVEDT-CHAVE.
               10 BVVEDT-CH-SEQUENCIA          PIC  9(003).
               10 BVVEDT-CH-FILLER             PIC  X(027).
           05 BVVEDT-TIMESTAMP                 PIC  X(020).
           05 BVVEDT-QTDE-DIAS                 PIC  9(004).
           05 BVVEDT-USUARIO                   PIC  X(009).
           05 BVVEDT-TERMINAL                  PIC  X(008).
           05 FILLER                           PIC  X(049).
