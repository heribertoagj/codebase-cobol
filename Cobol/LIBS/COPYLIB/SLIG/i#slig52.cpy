      ***===========================================================***
      *** NOME INC                                     LENGTH=00125 ***
      *** I#SLIG52                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - DEPDC-BANDA-MAGNT             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-DEPDC-BANDA-MAGNT.
           05 SLIG52-EMPR-INC                    PIC S9(005)   COMP-3.
           05 SLIG52-DEPENDENCIA                 PIC S9(005)   COMP-3.
           05 SLIG52-BCO-SACDO                   PIC S9(003)   COMP-3.
           05 SLIG52-AGEN-SACDO                  PIC S9(005)   COMP-3.
           05 SLIG52-CONTA-SACDO                 PIC S9(013)   COMP-3.
           05 SLIG52-CGC-CPF-SACDO               PIC S9(009)   COMP-3.
           05 SLIG52-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG52-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
           05 SLIG52-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG52-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG52-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG52-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG52-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG52-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG52-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG52-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG52-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG52-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG52-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG52-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG52-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG52-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG52-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG52-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG52-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG52-CGC-CPF-SACDO-ST            PIC X(009).
