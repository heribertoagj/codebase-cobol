
      ***===========================================================***
      *** NOME INC                                     LENGTH=00123 ***
      *** I#SLIG4G                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CLI-SACDO-CHEQ                ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG47                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-CHEQ.
           05 SLIG4G-CGC-CPF-CLI                 PIC S9(09)    COMP-3.
           05 SLIG4G-CFLIAL-CLI                  PIC S9(05)    COMP-3.
           05 SLIG4G-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG4G-CFLIAL-SACDO                PIC S9(05)    COMP-3.
           05 SLIG4G-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG4G-CCTRL-CLI                   PIC S9(02)    COMP-3.
           05 SLIG4G-CCTRL-SACDO                 PIC S9(02)    COMP-3.
           05 SLIG4G-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG4G-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG4G-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4G-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4G-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4G-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4G-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG4G-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4G-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4G-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG4G-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG4G-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG4G-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG4G-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG4G-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG4G-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG4G-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG4G-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG4G-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4G-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG4G-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG4G-CCTRL-CLI-ST                PIC S9(02)    COMP-3.
           05 SLIG4G-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG4G-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG4G-CCTRL-SACDO-ST              PIC S9(02)    COMP-3.
