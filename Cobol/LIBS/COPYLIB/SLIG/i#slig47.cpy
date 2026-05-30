
      ***===========================================================***
      *** NOME INC                                     LENGTH=00113 ***
      *** I#SLIG47                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CLI-SACDO-CHEQ                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-CHEQ.
           05 SLIG47-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG47-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG47-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG47-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG47-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG47-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG47-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG47-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG47-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG47-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG47-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG47-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG47-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG47-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG47-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG47-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG47-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG47-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG47-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG47-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG47-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG47-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG47-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG47-CGC-CPF-SACDO-ST            PIC X(009).
