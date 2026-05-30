      ***===========================================================***
      *** NOME INC                                     LENGTH=00125 ***
      *** I#SLIG53                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CONSL-CLI-BANDA               ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CONSL-CLI-BANDA.
           05 SLIG53-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG53-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG53-BCO-SACDO                   PIC S9(03) COMP-3.
           05 SLIG53-AGEN-SACDO                  PIC S9(05) COMP-3.
           05 SLIG53-CONTA-SACDO                 PIC S9(13) COMP-3.
           05 SLIG53-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG53-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG53-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG53-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG53-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG53-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG53-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG53-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG53-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG53-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG53-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG53-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG53-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG53-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG53-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG53-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG53-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG53-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG53-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG53-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG53-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG53-CGC-CPF-SACDO-ST            PIC X(009).
