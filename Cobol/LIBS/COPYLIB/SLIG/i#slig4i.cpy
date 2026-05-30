
      ***===========================================================***
      *** NOME INC                                     LENGTH=00159 ***
      *** I#SLIG4I                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CLI-CHEQ-CONSL                ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG49                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-CLI-CHEQ-CONSL.
           05 SLIG4I-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG4I-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG4I-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG4I-CFLIAL-CLI                  PIC S9(05)    COMP-3.
           05 SLIG4I-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG4I-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG4I-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG4I-COPLT-CLI                   PIC X(040).
           05 SLIG4I-COPLT-CLI-NULL              PIC X(001).
           05 SLIG4I-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4I-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4I-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4I-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4I-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-APOS-PER-NAO-PG-NU     PIC X(001).
           05 SLIG4I-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4I-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4I-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG4I-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG4I-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG4I-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG4I-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG4I-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG4I-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG4I-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG4I-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG4I-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4I-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG4I-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG4I-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
