      *==============================================================*
      *   BOOK -  I#BVVEPT                                           *
      *           CARTEIRA 'D' - PARAMETRIZACAO DE TAXAS             *
      ****************************************************************
      *   TAMANHO        0090  BYTES                                 *
      *   TAMANHO CHAVE  0002  BYTES (POSICAO 01 A 02)               *
      ****************************************************************
       01  PARMTAXAS-REG.
           03  PARM-CHAVE.
               05  PARM-ATE                       PIC  9(0017).
           03  PARM-TX-PERSONAL                   PIC  9(003)V99.
           03  PARM-TX-URA                        PIC  9(003)V99.
           03  PARM-TX-PROGRAMADA                 PIC  9(003)V99.
           03  PARM-HORA                          PIC  9(006).
           03  PARM-DATA                          PIC  9(008).
           03  PARM-USUARIO                       PIC  X(009).
           03  FILLER                             PIC  X(035).
