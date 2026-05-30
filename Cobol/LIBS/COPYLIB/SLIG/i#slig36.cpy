
      ***===========================================================***
      *** NOME INC                                     LENGTH=00154 ***
      *** I#SLIG36                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CLI-VNDOR-CONSL               ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-CLI-VNDOR-CONSL.
           05 SLIG36-EMPR-INC                   PIC S9(05) COMP-3.
           05 SLIG36-DEPENDENCIA                PIC S9(05) COMP-3.
           05 SLIG36-CGC-CPF-CLI                PIC S9(09) COMP-3.
           05 SLIG36-INDCD-PSSOA-CLI            PIC X(001).
           05 SLIG36-IDTFD-CART-LIQDZ           PIC S9(02) COMP-3.
           05 SLIG36-COPLT-CLI                  PIC X(040).
           05 SLIG36-COPLT-CLI-NULL             PIC X(001).
           05 SLIG36-ANTES-PER-NAO-PG           PIC S9(13)V99 COMP-3.
           05 SLIG36-ANTES-PER-NAO-PG-NULL      PIC X(001).
           05 SLIG36-VCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG36-VCTO-PER-PG-NULL           PIC X(001).
           05 SLIG36-VCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG36-VCTO-PER-NAO-PG-NULL       PIC X(001).
           05 SLIG36-VCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG36-VCTO-PER-BXADO-NULL        PIC X(001).
           05 SLIG36-VCTO-APOS-PER-PG           PIC S9(13)V99 COMP-3.
           05 SLIG36-VCTO-APOS-PER-PG-NULL      PIC X(001).
           05 SLIG36-APOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG36-APOS-PER-NAO-PG-NULL       PIC X(001).
           05 SLIG36-APOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG36-APOS-PER-BXADO-NULL        PIC X(001).
           05 SLIG36-PGTO-PER-CLI               PIC S9(13)V99 COMP-3.
           05 SLIG36-PGTO-PER-CLI-NULL          PIC X(001).
           05 SLIG36-PGTO-PER-SACDO             PIC S9(13)V99 COMP-3.
           05 SLIG36-PGTO-PER-SACDO-NULL        PIC X(001).
           05 SLIG36-PGTO-ANTCP-CLI             PIC S9(13)V99 COMP-3.
           05 SLIG36-PGTO-ANTCP-CLI-NULL        PIC X(001).
           05 SLIG36-PGTO-ANTCP-SACDO           PIC S9(13)V99 COMP-3.
           05 SLIG36-PGTO-ANTCP-SACDO-NULL      PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG36-CGC-CPF-CLI-ST             PIC X(009).
