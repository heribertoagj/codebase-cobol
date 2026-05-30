      ***===========================================================***
      *** NOME INC                                     LENGTH=00130 ***
      *** I#SLIG5B                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - DEPDC-BANDA-MAGNT             ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG52                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-DEPDC-BANDA-MAGNT.
           05 SLIG5B-EMPR-INC                    PIC S9(005)   COMP-3.
           05 SLIG5B-DEPENDENCIA                 PIC S9(005)   COMP-3.
           05 SLIG5B-BCO-SACDO                   PIC S9(003)   COMP-3.
           05 SLIG5B-AGEN-SACDO                  PIC S9(005)   COMP-3.
           05 SLIG5B-CONTA-SACDO                 PIC S9(013)   COMP-3.
           05 SLIG5B-CGC-CPF-SACDO               PIC S9(009)   COMP-3.
           05 SLIG5B-CFLIAL-SACDO                PIC S9(005)   COMP-3.
           05 SLIG5B-IDTFD-CART-LIQDZ            PIC S9(002)   COMP-3.
           05 SLIG5B-CCTRL-SACDO                 PIC S9(002)   COMP-3.
           05 SLIG5B-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG5B-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG5B-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG5B-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG5B-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG5B-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG5B-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG5B-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG5B-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG5B-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG5B-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG5B-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG5B-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG5B-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG5B-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG5B-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG5B-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG5B-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG5B-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG5B-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG5B-CCTRL-SACDO-ST              PIC S9(002)   COMP-3.
