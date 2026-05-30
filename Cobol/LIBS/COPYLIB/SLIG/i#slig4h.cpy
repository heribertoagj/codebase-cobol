      ***===========================================================***
      *** NOME INC                                     LENGTH=00124 ***
      *** I#SLIG4H                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - SACDO-CHEQ-CONSL              ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG48                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-SACDO-CHEQ-CONSL.
           05 SLIG4H-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG4H-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG4H-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG4H-CFLIAL-SACDO                PIC S9(05)    COMP-3.
           05 SLIG4H-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG4H-CCTRL-SACDO                 PIC S9(02)    COMP-3.
           05 SLIG4H-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG4H-GRP-ECONM                   PIC S9(09)    COMP-3.
           05 SLIG4H-GRP-ECONM-NULL              PIC X(001).
           05 SLIG4H-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4H-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4H-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4H-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4H-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG4H-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4H-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4H-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG4H-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG4H-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG4H-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG4H-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG4H-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG4H-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG4H-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG4H-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG4H-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4H-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG4H-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG4H-CCTRL-SACDO-ST              PIC S9(02)    COMP-3.
