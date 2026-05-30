
      ***===========================================================***
      *** NOME INC                                     LENGTH=00154 ***
      *** I#SLIG49                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CLI-CHEQ-CONSL                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-CHEQ-CONSL.
           05 SLIG49-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG49-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG49-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG49-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG49-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG49-COPLT-CLI                   PIC X(040).
           05 SLIG49-COPLT-CLI-NULL              PIC X(001).
           05 SLIG49-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG49-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG49-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG49-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG49-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-APOS-PER-NAO-PG-NU     PIC X(001).
           05 SLIG49-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG49-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG49-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG49-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG49-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG49-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG49-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG49-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG49-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG49-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG49-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG49-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG49-CGC-CPF-CLI-ST              PIC X(009).
