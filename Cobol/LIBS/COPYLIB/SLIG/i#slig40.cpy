
      ***===========================================================***
      *** NOME INC                                     LENGTH=00165 ***
      *** I#SLIG40                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - HIST-CLI-VNDOR                ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-VNDOR.
           05 SLIG40-EMPR-INC                    PIC S9(05) COMP-3.
           05 SLIG40-DEPENDENCIA                 PIC S9(05) COMP-3.
           05 SLIG40-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG40-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG40-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG40-PER-REFT                    PIC X(010).
           05 SLIG40-COPLT-CLI                   PIC X(040).
           05 SLIG40-COPLT-CLI-NULL              PIC X(001).
           05 SLIG40-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-ANTES-PER-N-PG-NUL     PIC X(001).
           05 SLIG40-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG40-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG40-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG40-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG40-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-APOS-PER-N-PG-NULL     PIC X(001).
           05 SLIG40-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG40-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG40-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG40-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG40-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG40-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG40-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG40-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG40-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG40-PGTO-ANTCP-SACDO-NULL       PIC X(001).
           05 SLIG40-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG40-CGC-CPF-CLI-ST              PIC X(009).
