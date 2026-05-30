
      ***===========================================================***
      *** NOME INC                                     LENGTH=00119 ***
      *** I#SLIG35                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - SACDO-VNDOR-CONSL             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SACDO-VNDOR-CONSL.
           05 SLIG35-EMPR-INC                    PIC S9(05) COMP-3.
           05 SLIG35-DEPENDENCIA                 PIC S9(05) COMP-3.
           05 SLIG35-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG35-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG35-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG35-GRP-ECONM                   PIC S9(09) COMP-3.
           05 SLIG35-GRP-ECONM-NULL              PIC X(001).
           05 SLIG35-VCTO-ANT-PER-N-PG           PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-ANT-PER-N-PG-NULL      PIC X(001).
           05 SLIG35-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG35-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG35-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG35-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG35-VCTO-APOS-PER-N-PG          PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-APOS-PER-N-PG-NULL     PIC X(001).
           05 SLIG35-VCTO-APOS-PER-BX            PIC S9(13)V99 COMP-3.
           05 SLIG35-VCTO-APOS-PER-BX-NULL       PIC X(001).
           05 SLIG35-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG35-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG35-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG35-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG35-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG35-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG35-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG35-PGTO-ANTCP-SACDO-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG35-CGC-CPF-SACDO-ST            PIC X(009).
