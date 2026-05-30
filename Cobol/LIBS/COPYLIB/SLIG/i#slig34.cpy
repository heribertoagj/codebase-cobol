
      ***===========================================================***
      *** NOME INC                                     LENGTH=00113 ***
      *** I#SLIG34                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CLI-SACDO-VNDOR               ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-VNDOR.
           05 SLIG34-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG34-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG34-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG34-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG34-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG34-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-ANTES-PER-NAO-PG-N     PIC X(001).
           05 SLIG34-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG34-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG34-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG34-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG34-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-APOS-PER-NAO-PG-N      PIC X(001).
           05 SLIG34-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG34-VCTO-APOS-PER-BXADO-N       PIC X(001).
           05 SLIG34-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG34-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG34-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG34-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG34-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG34-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG34-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG34-PGTO-ANTCP-SACDO-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG34-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG34-CGC-CPF-SACDO-ST            PIC X(009).
