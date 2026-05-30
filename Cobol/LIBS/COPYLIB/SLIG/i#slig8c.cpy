      ***===========================================================***
      *** NOME INC                                     LENGTH=00082 ***
      *** I#SLIG8C                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS CHEQUES             ***
      ***                                                           ***
      ***           (COPIA DA I#SLIG83                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG8C-REGISTRO.
      *-------- CHAVE COMPLETA CHEQUES (SEM CARTEIRA)
         03 SLIG8C-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8C-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8C-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8C-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF CLIENTE
           05 SLIG8C-FILIAL-CCGC-CPF-CLI         PIC S9(05)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF CLIENTE
           05 SLIG8C-CONTROLE-CCGC-CPF-CLI       PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8C-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- GRUPO ECONOMICO CLIENTE
           05 SLIG8C-CGRP-ECONM                  PIC S9(09)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8C-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG8C-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG8C-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8C-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- BANCO SACADO
           05 SLIG8C-CBCO-SACDO                  PIC S9(003)   COMP-3.
      *-------- AGENCIA SACADO
           05 SLIG8C-CAGEN-SACDO                 PIC S9(005)   COMP-3.
      *-------- CONTA SACADO
           05 SLIG8C-CCONTA-SACDO                PIC S9(013)   COMP-3.
      *
      *
      *-------- INDICES CALCULADOS
         03 SLIG8C-DADOS.
      *--------
           05 SLIG8C-MAIOR-IC                    PIC S9(03)V99 COMP-3.
           05 SLIG8C-MENOR-ILF                   PIC S9(03)V99 COMP-3.
      *-------- INDICE LIQUIDEZ FINAL
           05 SLIG8C-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8C-ILF-SIMPLES-NULL            PIC  X(01).
           05 SLIG8C-ILF-CAUCIONADA              PIC S9(03)V99 COMP-3.
           05 SLIG8C-ILF-CAUCIONADA-NULL         PIC  X(01).
           05 SLIG8C-ILF-DESCONTO                PIC S9(03)V99 COMP-3.
           05 SLIG8C-ILF-DESCONTO-NULL           PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG8C-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8C-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG8C-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8C-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8C-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8C-IC-DESCONTO-NULL            PIC  X(01).
      *--------
           05 SLIG8C-FILLER                      PIC  X(07).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8C-DADOS-CCGC-ALPHA.
           05 SLIG8C-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8C-FILIAL-CCGC-CPF-CLI-ST      PIC X(005).
           05 SLIG8C-CTRL-CCGC-CPF-CLI-ST        PIC S9(02)    COMP-3.
           05 SLIG8C-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8C-FLIAL-CCGC-CPF-SACDO-ST     PIC X(005).
           05 SLIG8C-CTRL-CCGC-CPF-SACDO-ST      PIC S9(002)   COMP-3.
