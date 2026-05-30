
      ***===========================================================***
      *** NOME INC                                     LENGTH=00123 ***
      *** I#SLIG3D                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CLI-SACDO-VNDOR               ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG34)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE E SACADO                          ***
      ***===========================================================***
      *
       01  REG-CLI-SACDO-VNDOR.
           05 SLIG3D-CGC-CPF-CLI                 PIC S9(09) COMP-3.
           05 SLIG3D-FILIAL-CLI                  PIC S9(05) COMP-3.
           05 SLIG3D-CONTROLE-CLI                PIC S9(02) COMP-3.
           05 SLIG3D-INDCD-PSSOA-CLI             PIC X(001).
           05 SLIG3D-CGC-CPF-SACDO               PIC S9(09) COMP-3.
           05 SLIG3D-FILIAL-SACDO                PIC S9(05) COMP-3.
           05 SLIG3D-CONTROLE-SACDO              PIC S9(02) COMP-3.
           05 SLIG3D-INDCD-PSSOA-SACDO           PIC X(001).
           05 SLIG3D-IDTFD-CART-LIQDZ            PIC S9(02) COMP-3.
           05 SLIG3D-VCTO-ANTES-PER-NAO-PG       PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-ANTES-PER-NAO-PG-N     PIC X(001).
           05 SLIG3D-VCTO-PER-PG                 PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-PER-PG-NULL            PIC X(001).
           05 SLIG3D-VCTO-PER-NAO-PG             PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-PER-NAO-PG-NULL        PIC X(001).
           05 SLIG3D-VCTO-PER-BXADO              PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-PER-BXADO-NULL         PIC X(001).
           05 SLIG3D-VCTO-APOS-PER-PG            PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-APOS-PER-PG-NULL       PIC X(001).
           05 SLIG3D-VCTO-APOS-PER-NAO-PG        PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-APOS-PER-NAO-PG-N      PIC X(001).
           05 SLIG3D-VCTO-APOS-PER-BXADO         PIC S9(13)V99 COMP-3.
           05 SLIG3D-VCTO-APOS-PER-BXADO-N       PIC X(001).
           05 SLIG3D-PGTO-PER-CLI                PIC S9(13)V99 COMP-3.
           05 SLIG3D-PGTO-PER-CLI-NULL           PIC X(001).
           05 SLIG3D-PGTO-PER-SACDO              PIC S9(13)V99 COMP-3.
           05 SLIG3D-PGTO-PER-SACDO-NULL         PIC X(001).
           05 SLIG3D-PGTO-ANTCP-CLI              PIC S9(13)V99 COMP-3.
           05 SLIG3D-PGTO-ANTCP-CLI-NULL         PIC X(001).
           05 SLIG3D-PGTO-ANTCP-SACDO            PIC S9(13)V99 COMP-3.
           05 SLIG3D-PGTO-ANTCP-SACDO-NULL       PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG3D-CGC-CPF-CLI-ST              PIC X(009).
           05 SLIG3D-FILIAL-CLI-ST               PIC X(005).
           05 SLIG3D-CONTROLE-CLI-ST             PIC S9(02) COMP-3.
           05 SLIG3D-CGC-CPF-SACDO-ST            PIC X(009).
           05 SLIG3D-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG3D-CONTROLE-SACDO-ST           PIC S9(02) COMP-3.
