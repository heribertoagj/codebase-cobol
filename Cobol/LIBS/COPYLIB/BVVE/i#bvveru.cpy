      *----------------------------------------------------------------*
      * BOOK I#BVVERU - CONTROLE TIMESTAMP                             *
      * TAMANHO : 026                                                  *
      *----------------------------------------------------------------*
       01  CTRE-REGISTRO.
           03  CTRE-TIMESTAMP          PIC  X(26).
           03  FILLER  REDEFINES  CTRE-TIMESTAMP.
               05  CTRE-DT-ALT.
                   07  CTRE-AAAA       PIC  9(04).
                   07  FILLER          PIC  X(01).
                   07  CTRE-MM         PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  CTRE-DD         PIC  9(02).
               05  CTRE-HORA.
                   07  FILLER          PIC  X(01).
                   07  CTRE-HH         PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  CTRE-MM         PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  CTRE-SS         PIC  9(02).
                   07  FILLER          PIC  X(01).
                   07  CTRE-N          PIC  9(06).

