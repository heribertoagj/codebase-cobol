      *****************************************************************
      *                                                               *
      *  I#BVVEEB                                                     *
      *                                                               *
      *  BVVE - ARQUIVO EBTA - AMEX.                                  *
      *         TIPOS DE REGISTRO A (HEADER) E C/F (TRANSACAO)        *
      *                                                               *
      *  LRECL= 600 BYTES                                             *
      *  CRIACAO: MAIO/2013                                           *
      *                                                               *
      *****************************************************************

       01  REG-EBTA-A.
           03  EBTA-NUM-CTA-A                         PIC  9(015).
           03  EBTA-TIPO-REGISTRO-A                   PIC  X(001).
           03  EBTA-COD-EMPR                          PIC  9(006).
           03  EBTA-GRP-EMPR                          PIC  9(004).
           03  EBTA-VERSAO                            PIC  9(009).
           03  FILLER                                 PIC  X(279).
           03  EBTA-VALOR                             PIC  9(013)V99.
           03  EBTA-SINAL                             PIC  X(001).
           03  FILLER                                 PIC  X(270).

       01  REG-EBTA-C-F                               REDEFINES
           REG-EBTA-A.
           03  EBTA-NUM-CTA-C-F                       PIC  9(015).
           03  EBTA-TIPO-REGISTRO-C-F                 PIC  X(001).
           03  FILLER                                 PIC  X(172).
           03  EBTA-CCUSTO                            PIC  X(024).
           03  FILLER                                 PIC  X(388).

