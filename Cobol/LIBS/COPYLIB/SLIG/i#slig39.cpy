      ***===========================================================***
      *** NOME INC                                     LENGTH=00131 ***
      *** I#SLIG39                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - HIST-SACDO-VNDOR              ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-HIST-SACDO-VNDOR.
           05 SLIG39-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG39-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG39-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG39-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG39-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG39-PER-REFT                    PIC X(010).
           05 SLIG39-GRP-ECONM                   PIC S9(09)    COMP-3.
           05 SLIG39-GRP-ECONM-NULL              PIC X(001).
           05 SLIG39-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-ANTES-PER-N-PG-NUL     PIC X(001).
           05 SLIG39-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG39-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG39-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG39-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG39-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG39-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG39-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG39-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG39-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG39-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG39-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG39-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG39-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG39-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG39-PGTO-ANTCP-SACDO-NULL       PIC X(001).
           05 SLIG39-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG39-CGC-CPF-SACDO-ST            PIC X(009).
