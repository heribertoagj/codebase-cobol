      ***===========================================================***
      *** NOME INC                                     LENGTH=00177 ***
      *** I#SLIG5Q                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - HIST-CTA-CHEQ                 ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-HIST-CTA-CHEQ.
           05 SLIG5Q-CBCO                        PIC S9(03)    COMP-3.
           05 SLIG5Q-CAG-BCRIA                   PIC S9(05)    COMP-3.
           05 SLIG5Q-CCTA-BCRIA-CLI              PIC S9(13)    COMP-3.
           05 SLIG5Q-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG5Q-PER-REFT                    PIC X(010).
           05 SLIG5Q-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG5Q-CCFLIAL-CLI                 PIC S9(05)    COMP-3.
           05 SLIG5Q-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG5Q-CINDCD-PSSOA-CLI            PIC X(001).
           05 SLIG5Q-COPLT-CLI                   PIC X(040).
           05 SLIG5Q-COPLT-CLI-NULL              PIC X(001).
           05 SLIG5Q-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG5Q-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG5Q-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG5Q-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG5Q-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG5Q-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG5Q-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG5Q-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG5Q-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG5Q-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG5Q-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG5Q-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG5Q-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG5Q-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG5Q-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG5Q-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG5Q-CHEQ-DEVOL-ANTER-NULL       PIC X(001).
           05 SLIG5Q-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG5Q-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG5Q-CCFLIAL-CLI-ST              PIC X(005).
           05 SLIG5Q-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
