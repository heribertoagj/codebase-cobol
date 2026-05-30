
      ***===========================================================***
      *** NOME INC                                     LENGTH=00165 ***
      *** I#SLIG4S                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            IMAGEM DA TABELA DB2PRD.TCTA_CHEQ_CONSL        ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-CHEQ-CONSL.
           05 SLIG4S-CBCO                        PIC S9(03)    COMP-3.
           05 SLIG4S-CAG-BCRIA                   PIC S9(05)    COMP-3.
           05 SLIG4S-CCTA-BCRIA-CLI              PIC S9(13)    COMP-3.
           05 SLIG4S-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG4S-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG4S-CFLIAL-CLI                  PIC S9(05)    COMP-3.
           05 SLIG4S-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG4S-CINDCD-PSSOA-CLI            PIC X(001).
           05 SLIG4S-ICOPLT-CLI                  PIC X(040).
           05 SLIG4S-ICOPLT-CLI-NULL             PIC X(001).
           05 SLIG4S-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4S-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4S-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4S-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4S-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-APOS-PER-NAO-PG-NU     PIC X(001).
           05 SLIG4S-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4S-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4S-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG4S-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG4S-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG4S-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG4S-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG4S-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG4S-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG4S-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG4S-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG4S-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4S-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG4S-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG4S-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
