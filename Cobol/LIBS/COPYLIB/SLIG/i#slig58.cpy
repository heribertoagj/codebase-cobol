
      ***===========================================================***
      *** NOME INC                                     LENGTH=00166 ***
      *** I#SLIG58                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - HIST-CLI-CHEQ                 ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-CHEQ.
           05 SLIG58-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG58-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG58-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG58-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG58-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG58-PER-REFT                    PIC X(010).
           05 SLIG58-COPLT-CLI                   PIC X(040).
           05 SLIG58-COPLT-CLI-NULL              PIC X(001).
           05 SLIG58-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG58-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG58-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG58-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG58-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG58-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG58-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG58-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG58-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG58-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG58-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG58-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG58-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG58-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG58-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG58-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.
           05 SLIG58-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG58-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG58-CGC-CPF-CLI-ST              PIC X(009).
