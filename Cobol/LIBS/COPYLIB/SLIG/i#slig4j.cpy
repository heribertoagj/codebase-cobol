
      ***===========================================================***
      *** NOME INC                                     LENGTH=00170 ***
      *** I#SLIG4J                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - HIST-CLI-VNDOR                ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG40)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE                                   ***
      ***===========================================================***
      *
       01  REG-HIST-CLI-VNDOR.
           05 SLIG4J-EMPR-INC                    PIC S9(05) COMP-3.
           05 SLIG4J-DEPENDENCIA                 PIC S9(05) COMP-3.
           05 SLIG4J-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG4J-FILIAL-CLI                  PIC S9(05) COMP-3.
           05 SLIG4J-CONTROLE-CLI                PIC S9(02) COMP-3.
           05 SLIG4J-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG4J-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG4J-PER-REFT                    PIC X(010).
           05 SLIG4J-COPLT-CLI                   PIC X(040).
           05 SLIG4J-COPLT-CLI-NULL              PIC X(001).
           05 SLIG4J-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-ANTES-PER-N-PG-NUL     PIC X(001).
           05 SLIG4J-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG4J-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG4J-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG4J-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG4J-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-APOS-PER-N-PG-NULL     PIC X(001).
           05 SLIG4J-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG4J-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG4J-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG4J-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG4J-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG4J-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG4J-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG4J-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG4J-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG4J-PGTO-ANTCP-SACDO-NULL       PIC X(001).
           05 SLIG4J-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG4J-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG4J-FILIAL-CLI-ST               PIC X(005).
           05 SLIG4J-CONTROLE-CLI-ST             PIC S9(02) COMP-3.
