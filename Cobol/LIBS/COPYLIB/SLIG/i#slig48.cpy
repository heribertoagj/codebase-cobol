      ***===========================================================***
      *** NOME INC                                     LENGTH=00119 ***
      *** I#SLIG48                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - SACDO-CHEQ-CONSL              ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SACDO-CHEQ-CONSL.
           05 SLIG48-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG48-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG48-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG48-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG48-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG48-GRP-ECONM                   PIC S9(09)    COMP-3.
           05 SLIG48-GRP-ECONM-NULL              PIC X(001).
           05 SLIG48-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG48-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG48-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG48-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG48-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG48-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG48-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG48-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG48-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG48-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG48-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG48-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG48-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG48-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG48-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG48-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG48-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG48-CGC-CPF-SACDO-ST            PIC X(009).
