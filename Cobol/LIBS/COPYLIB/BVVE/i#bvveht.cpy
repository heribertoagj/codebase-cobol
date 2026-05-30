      *==============================================================*
      *   BOOK -  I#BVVEHT                                           *
      *           CARTEIRA 'D' - HISTORICO PARAMETRIZACAO DE TAXAS   *
      ****************************************************************
      *   TAMANHO        0090  BYTES                                 *
      *   TAMANHO CHAVE  0002  BYTES (POSICAO 01 A 02)               *
      ****************************************************************
       01  HISTTAXA-REG.
           03  HIST-CHAVE.
      *    FORMATO CAMPO DATA=AAAAMMDD
               05  HIST-DATA                      PIC  9(008).
               05  HIST-HORA                      PIC  9(006).
               05  HIST-SEQUENCIA                 PIC  9(006).
           03  HIST-ATE                           PIC  9(017).
           03  HIST-TX-PERSONAL                   PIC  9(003)V99.
           03  HIST-TX-URA                        PIC  9(003)V99.
           03  HIST-TX-PROGRAMADA                 PIC  9(003)V99.
           03  HIST-USUARIO                       PIC  X(009).
           03  HIST-ACAO                          PIC  X(001).
           03  FILLER                             PIC  X(038).
