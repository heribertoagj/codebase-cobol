      *================================================================*
      *           AGENDA FINANCEIRA BVVE PARA ROTINA IGCO              *
      *                                                                *
      * COPYBOOK: I#BVVEAX                                             *
      * TAMANHO : 82                                                   *
      *================================================================*
       01  BVVEAX-REGISTRO.
           05 BVVEAX-DT-MOVTO         PIC  9(08)    COMP-3.
           05 BVVEAX-AGENCIA          PIC  9(05)    COMP-3.
           05 BVVEAX-CONTA            PIC  9(07)    COMP-3.
           05 BVVEAX-CRED-CNPJ        PIC  9(14)    COMP-3.
           05 BVVEAX-CRED-RAZAO       PIC  X(20).
           05 BVVEAX-BANDEIRA         PIC  X(12).
           05 BVVEAX-PERIODICIDADE    PIC  X(09).
           05 BVVEAX-DT-ORPAG         PIC  9(08)    COMP-3.
           05 BVVEAX-VALOR            PIC S9(13)V99 COMP-3.
           05 BVVEAX-SALDO            PIC S9(13)V99 COMP-3.
