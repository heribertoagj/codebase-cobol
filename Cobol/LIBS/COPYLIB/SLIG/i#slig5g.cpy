      ***===========================================================***
      *** NOME INC                                     LENGTH=00136 ***
      *** I#SLIG5G                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO CHEQUE - HIST-SACDO-CHEQ               ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG57                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  REG-HIST-SACDO-CHEQ.
           05 SLIG5G-EMPR-INC                    PIC S9(05) COMP-3.
           05 SLIG5G-DEPENDENCIA                 PIC S9(05) COMP-3.
           05 SLIG5G-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG5G-CFLIAL-SACDO                PIC S9(05) COMP-3.
           05 SLIG5G-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG5G-PER-REFT                    PIC X(010).
           05 SLIG5G-CCTRL-SACDO                 PIC S9(02) COMP-3.
           05 SLIG5G-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG5G-GRP-ECONM                   PIC S9(09) COMP-3.
           05 SLIG5G-GRP-ECONM-NULL              PIC X(001).
           05 SLIG5G-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG5G-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG5G-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG5G-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG5G-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG5G-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG5G-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG5G-CHEQ-PRIM-DEVLC             PIC S9(13)V99 COMP-3.
           05 SLIG5G-CHEQ-PRIM-DEVLC-NULL        PIC X(001).
           05 SLIG5G-CHEQ-REAP                   PIC S9(13)V99 COMP-3.
           05 SLIG5G-CHEQ-REAP-NULL              PIC X(001).
           05 SLIG5G-CHEQ-REAP-DEVOL             PIC S9(13)V99 COMP-3.
           05 SLIG5G-CHEQ-REAP-DEVOL-NULL        PIC X(001).
           05 SLIG5G-CHEQ-REAP-PG                PIC S9(13)V99 COMP-3.
           05 SLIG5G-CHEQ-REAP-PG-NULL           PIC X(001).
           05 SLIG5G-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.
           05 SLIG5G-CHEQ-DEVOL-ANTER            PIC S9(13)V99 COMP-3.
           05 SLIG5G-CHEQ-DEVOL-ANTER-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG5G-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG5G-CFLIAL-SACDO-ST             PIC X(005).
           05 SLIG5G-CCTRL-SACDO-ST              PIC S9(02) COMP-3.
