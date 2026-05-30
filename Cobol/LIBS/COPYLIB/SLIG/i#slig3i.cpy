      ***===========================================================***
      *** NOME INC                                     LENGTH=00136 ***
      *** I#SLIG3I                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - HIST-SACDO-VNDOR              ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG39)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA SACADO                                    ***
      ***===========================================================***
      *
       01  REG-HIST-SACDO-VNDOR.
           05 SLIG3I-EMPR-INC                    PIC S9(05)    COMP-3.
           05 SLIG3I-DEPENDENCIA                 PIC S9(05)    COMP-3.
           05 SLIG3I-CGC-CPF-SACDO               PIC S9(09)    COMP-3.
           05 SLIG3I-FILIAL-SACDO                PIC S9(05)    COMP-3.
           05 SLIG3I-CONTROLE-SACDO              PIC S9(02)    COMP-3.
           05 SLIG3I-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG3I-IDTFD-CART-LIQDZ            PIC S9(02)    COMP-3.
           05 SLIG3I-PER-REFT                    PIC X(010).
           05 SLIG3I-GRP-ECONM                   PIC S9(09)    COMP-3.
           05 SLIG3I-GRP-ECONM-NULL              PIC X(001).
           05 SLIG3I-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-ANTES-PER-N-PG-NUL     PIC X(001).
           05 SLIG3I-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG3I-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG3I-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG3I-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG3I-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-APOS-PER-N-PG-NUL      PIC X(001).
           05 SLIG3I-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG3I-VCTO-APOS-PER-BXADO-NUL     PIC X(001).
           05 SLIG3I-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG3I-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG3I-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG3I-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG3I-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG3I-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG3I-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG3I-PGTO-ANTCP-SACDO-NULL       PIC X(001).
           05 SLIG3I-CNUM-PTCAO-HIST             PIC S9(03)    COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG3I-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG3I-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG3I-CONTROLE-SACDO-ST           PIC S9(02)    COMP-3.
