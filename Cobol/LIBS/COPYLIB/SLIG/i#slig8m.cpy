      ***===========================================================***
      *** NOME INC                                     LENGTH=00070 ***
      *** I#SLIG8M                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS CHEQUES             ***
      ***                                                           ***
      ***===========================================================***
      *
       01  SLIG8M-REGISTRO.
      *-------- CHAVE COMPLETA CHEQUES (SEM CARTEIRA)
         03 SLIG8M-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8M-CBCO                        PIC S9(003)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG8M-CAG-BCRIA                   PIC S9(005)   COMP-3.
      *-------- CONTA CLIENTE
           05 SLIG8M-CCTA-BCRIA-CLI              PIC S9(013)   COMP-3.
      *
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8M-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF CLIENTE
           05 SLIG8M-FILIAL-CCGC-CPF-CLI         PIC S9(05)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF CLIENTE
           05 SLIG8M-CONTROLE-CCGC-CPF-CLI       PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8M-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8M-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG8M-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG8M-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8M-CINDCD-PSSOA-SACDO          PIC X(001).
      *
      *-------- INDICES CALCULADOS
         03 SLIG8M-DADOS.
      *--------
           05 SLIG8M-MAIOR-IC                    PIC S9(03)V99 COMP-3.
           05 SLIG8M-MENOR-ILF                   PIC S9(03)V99 COMP-3.
      *-------- INDICE LIQUIDEZ FINAL
           05 SLIG8M-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8M-ILF-SIMPLES-NULL            PIC  X(01).
           05 SLIG8M-ILF-CAUCIONADA              PIC S9(03)V99 COMP-3.
           05 SLIG8M-ILF-CAUCIONADA-NULL         PIC  X(01).
           05 SLIG8M-ILF-DESCONTO                PIC S9(03)V99 COMP-3.
           05 SLIG8M-ILF-DESCONTO-NULL           PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG8M-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8M-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG8M-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8M-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8M-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8M-IC-DESCONTO-NULL            PIC  X(01).
      *--------
           05 SLIG8M-FILLER                      PIC  X(06).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8M-DADOS-CCGC-ALPHA.
           05 SLIG8M-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8M-FILIAL-CCGC-CPF-CLI-ST      PIC X(005).
           05 SLIG8M-CTRL-CCGC-CPF-CLI-ST    PIC S9(02)    COMP-3.
           05 SLIG8M-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8M-FLIAL-CCGC-CPF-SACDO-ST    PIC X(005).
           05 SLIG8M-CTRL-CCGC-CPF-SACDO-ST  PIC S9(002)   COMP-3.
