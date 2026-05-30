
      ***===========================================================***
      *** NOME INC                                     LENGTH=00159 ***
      *** I#SLIG3F                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO VENDOR - CLI-VNDOR-CONSL               ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG36)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE                                   ***
      ***===========================================================***
      *
       01  REG-CLI-VNDOR-CONSL.
           05 SLIG3F-EMPR-INC                   PIC S9(05) COMP-3.
           05 SLIG3F-DEPENDENCIA                PIC S9(05) COMP-3.
           05 SLIG3F-CGC-CPF-CLI                PIC S9(09) COMP-3.
           05 SLIG3F-FILIAL-CLI                 PIC S9(05) COMP-3.
           05 SLIG3F-CONTROLE-CLI               PIC S9(02) COMP-3.
           05 SLIG3F-INDCD-PSSOA-CLI            PIC X(001).
           05 SLIG3F-IDTFD-CART-LIQDZ           PIC S9(02) COMP-3.
           05 SLIG3F-COPLT-CLI                  PIC X(040).
           05 SLIG3F-COPLT-CLI-NULL             PIC X(001).
           05 SLIG3F-ANTES-PER-NAO-PG           PIC S9(13)V99 COMP-3.
           05 SLIG3F-ANTES-PER-NAO-PG-NULL      PIC X(001).
           05 SLIG3F-VCTO-PER-PG                PIC S9(13)V99 COMP-3.
           05 SLIG3F-VCTO-PER-PG-NULL           PIC X(001).
           05 SLIG3F-VCTO-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG3F-VCTO-PER-NAO-PG-NULL       PIC X(001).
           05 SLIG3F-VCTO-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG3F-VCTO-PER-BXADO-NULL        PIC X(001).
           05 SLIG3F-VCTO-APOS-PER-PG           PIC S9(13)V99 COMP-3.
           05 SLIG3F-VCTO-APOS-PER-PG-NULL      PIC X(001).
           05 SLIG3F-APOS-PER-NAO-PG            PIC S9(13)V99 COMP-3.
           05 SLIG3F-APOS-PER-NAO-PG-NULL       PIC X(001).
           05 SLIG3F-APOS-PER-BXADO             PIC S9(13)V99 COMP-3.
           05 SLIG3F-APOS-PER-BXADO-NULL        PIC X(001).
           05 SLIG3F-PGTO-PER-CLI               PIC S9(13)V99 COMP-3.
           05 SLIG3F-PGTO-PER-CLI-NULL          PIC X(001).
           05 SLIG3F-PGTO-PER-SACDO             PIC S9(13)V99 COMP-3.
           05 SLIG3F-PGTO-PER-SACDO-NULL        PIC X(001).
           05 SLIG3F-PGTO-ANTCP-CLI             PIC S9(13)V99 COMP-3.
           05 SLIG3F-PGTO-ANTCP-CLI-NULL        PIC X(001).
           05 SLIG3F-PGTO-ANTCP-SACDO           PIC S9(13)V99 COMP-3.
           05 SLIG3F-PGTO-ANTCP-SACDO-NULL      PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG3F-CGC-CPF-CLI-ST             PIC X(009).
           05 SLIG3F-FILIAL-CLI-ST              PIC X(005).
           05 SLIG3F-CONTROLE-CLI-ST            PIC S9(02) COMP-3.
