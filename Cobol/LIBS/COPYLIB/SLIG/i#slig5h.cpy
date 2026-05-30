      ***===========================================================***
      *** NOME INC                                     LENGTH=00171 ***
      *** I#SLIG5H                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - HIST-CLI-CHEQ                 ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG58                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-CHEQ.
           05 SLIG5H-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG5H-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG5H-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG5H-CCFLIAL-CLI                 PIC S9(05)    COMP-3.
           05 SLIG5H-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG5H-PER-REFT                    PIC X(010).
           05 SLIG5H-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG5H-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG5H-COPLT-CLI                   PIC X(040).
           05 SLIG5H-COPLT-CLI-NULL              PIC X(001).
           05 SLIG5H-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG5H-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG5H-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG5H-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG5H-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG5H-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG5H-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG5H-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG5H-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG5H-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG5H-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG5H-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG5H-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG5H-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG5H-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG5H-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.
           05 SLIG5H-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG5H-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG5H-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG5H-CCFLIAL-CLI-ST              PIC X(005).
           05 SLIG5H-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
