
      ***===========================================================***
      *** NOME INC                                     LENGTH=00124 ***
      *** I#SLIG3E                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - SACDO-VNDOR-CONSL             ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG35)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA SACADO                                    ***
      ***===========================================================***
      *
       01  REG-SACDO-VNDOR-CONSL.
           05 SLIG3E-EMPR-INC                    PIC S9(05) COMP-3.
           05 SLIG3E-DEPENDENCIA                 PIC S9(05) COMP-3.
           05 SLIG3E-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG3E-FILIAL-SACDO                PIC S9(05) COMP-3.
           05 SLIG3E-CONTROLE-SACDO              PIC S9(02) COMP-3.
           05 SLIG3E-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG3E-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG3E-GRP-ECONM                   PIC S9(09) COMP-3.
           05 SLIG3E-GRP-ECONM-NULL              PIC X(001).
           05 SLIG3E-VCTO-ANT-PER-N-PG           PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-ANT-PER-N-PG-NULL      PIC X(001).
           05 SLIG3E-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG3E-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG3E-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG3E-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG3E-VCTO-APOS-PER-N-PG          PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-APOS-PER-N-PG-NULL     PIC X(001).
           05 SLIG3E-VCTO-APOS-PER-BX            PIC S9(13)V99 COMP-3.
           05 SLIG3E-VCTO-APOS-PER-BX-NULL       PIC X(001).
           05 SLIG3E-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG3E-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG3E-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG3E-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG3E-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG3E-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG3E-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG3E-PGTO-ANTCP-SACDO-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG3E-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG3E-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG3E-CONTROLE-SACDO-ST           PIC S9(02) COMP-3.
