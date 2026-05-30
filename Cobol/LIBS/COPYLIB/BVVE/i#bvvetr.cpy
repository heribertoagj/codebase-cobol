      *****************************************************************
      *                                                               *
      *  I#BVVETR                                                     *
      *                                                               *
      *  BVVE - ARQUIVO TF - AMEX.                                    *
      *         TIPOS DE REGISTRO 01 (COMPANHIA), 04 (PORTADOR - NAO  *
      *         FINANCEIRAS) E 05 (FINANCEIRAS).                      *
      *  LRECL= 674 BYTES                                             *
      *  CRIACAO: MAIO/2013                                           *
      *                                                               *
      *****************************************************************

       01  REG-TF-01.
           03  TF-TIPO-REGISTRO-01                    PIC  9(002).
           03  FILLER                                 PIC  X(003).
           03  TF-CD-COMPANHIA                        PIC  9(010).
           03  FILLER                                 PIC  X(015).
           03  TF-NM-COMPANHIA                        PIC  X(029).
           03  FILLER                                 PIC  X(615).

       01  REG-TF-04                                  REDEFINES
           REG-TF-01.
           03  TF-TIPO-REGISTRO-04                    PIC  9(002).
           03  FILLER                                 PIC  X(013).
           03  TF-CCUSTO                              PIC  X(013).
           03  TF-NUM-CARTAO-04                       PIC  9(015).
           03  FILLER                                 PIC  X(631).

       01  REG-TF-05                                  REDEFINES
           REG-TF-01.
           03  TF-TIPO-REGISTRO-05                    PIC  9(002).
           03  FILLER                                 PIC  X(026).
           03  TF-NUM-CARTAO-05                       PIC  9(015).
           03  FILLER                                 PIC  X(174).
           03  TF-VALOR                               PIC  9(015)V99.
           03  FILLER                                 PIC  X(440).

