      ***===========================================================***
      *** NOME INC                                     LENGTH=00135 ***
      *** I#SLIG5C                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - CONSL-CLI-BANDA               ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG53                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-CONSL-CLI-BANDA.
           05 SLIG5C-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG5C-CFLIAL-CLI                  PIC S9(05) COMP-3.
           05 SLIG5C-BCO-SACDO                   PIC S9(03) COMP-3.
           05 SLIG5C-AGEN-SACDO                  PIC S9(05) COMP-3.
           05 SLIG5C-CONTA-SACDO                 PIC S9(13) COMP-3.
           05 SLIG5C-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG5C-CFLIAL-SACDO                PIC S9(05) COMP-3.
           05 SLIG5C-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG5C-CCTRL-CLI                   PIC S9(02) COMP-3.
           05 SLIG5C-CCTRL-SACDO                 PIC S9(02) COMP-3.
           05 SLIG5C-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG5C-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG5C-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG5C-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG5C-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG5C-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG5C-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG5C-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG5C-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG5C-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG5C-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG5C-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG5C-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG5C-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG5C-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG5C-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG5C-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG5C-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG5C-CHEQ-DEVOL-ANTER-NULL       PIC X(001).
           05 SLIG5C-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG5C-CFLIAL-CLI-ST               PIC X(005).
           05 SLIG5C-CCTRL-CLI-ST                PIC S9(02) COMP-3.
           05 SLIG5C-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG5C-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG5C-CCTRL-SACDO-ST              PIC S9(02) COMP-3.
