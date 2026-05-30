
      ***===========================================================***
      *** NOME INC                                     LENGTH=00135 ***
      *** I#SLIG4Q                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            IMSGEM DA TABELA DB2PRD.TCTA_SACDO_CHEQ        ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-CHEQ.
           05 SLIG4Q-CBCO                        PIC S9(03)    COMP-3.
           05 SLIG4Q-CAG-BCRIA                   PIC S9(05)    COMP-3.
           05 SLIG4Q-CCTA-BCRIA-CLI              PIC S9(13)    COMP-3.
           05 SLIG4Q-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG4Q-CFLIAL-SACDO                PIC S9(05)    COMP-3.
           05 SLIG4Q-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG4Q-CCTRL-SACDO                 PIC S9(02)    COMP-3.
           05 SLIG4Q-CINDCD-PSSOA-SACDO          PIC X(001).
           05 SLIG4Q-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG4Q-CFLIAL-CLI                  PIC S9(05)    COMP-3.
           05 SLIG4Q-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG4Q-CINDCD-PSSOA-CLI            PIC X(001).
           05 SLIG4Q-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4Q-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4Q-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4Q-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4Q-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG4Q-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4Q-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4Q-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG4Q-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG4Q-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG4Q-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG4Q-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG4Q-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG4Q-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG4Q-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG4Q-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG4Q-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4Q-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG4Q-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG4Q-CCTRL-SACDO-ST              PIC S9(02)    COMP-3.
           05 SLIG4Q-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG4Q-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG4Q-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
