      ***===========================================================***
      *** NOME INC                                     LENGTH=00059 ***
      *** I#SLIG8N                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INDICES CALCULADOS TITULOS             ***
      ***            PELA NEGOCIACAO DA COBRANCA                    ***
      ***===========================================================***
      *
       01  SLIG8N-REGISTRO.
      *-------- CHAVE COMPLETA (SEM CARTEIRA)
         03 SLIG8N-CHAVE-COMPLETA.
      *-------- BANCO CLIENTE
           05 SLIG8N-CBCO-PRODT-COBR             PIC S9(03)    COMP-3.
      *-------- CODIGO DO PRODUTO DA COBRANCA
           05 SLIG8N-CIDTFD-PRODT-COBR           PIC S9(02)    COMP-3.
      *-------- CODIGO DO NEGOCIACAO DA COBRANCA
           05 SLIG8N-CNEGOC-COBR                 PIC S9(18)    COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8N-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8N-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8N-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8N-CINDCD-PSSOA-SACDO          PIC X(001).
      *
      *-------- INDICES CALCULADOS
         03 SLIG8N-DADOS.
      *--------
           05 SLIG8N-MAIOR-IC                    PIC S9(03)V99 COMP-3.
           05 SLIG8N-MENOR-IL                    PIC S9(03)V99 COMP-3.
      *-------- INDICE LIQUIDEZ
           05 SLIG8N-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8N-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG8N-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8N-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8N-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8N-IL-DESCONTO-NULL            PIC  X(01).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG8N-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8N-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG8N-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8N-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8N-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8N-IC-DESCONTO-NULL            PIC  X(01).
      *--------
           05 SLIG8N-FILLER                      PIC  X(03).

      * === CAMPOS DUPLICADOS PARA STRING ===
         03 SLIG8N-DADOS-CCGC-ALPHA.
           05 SLIG8N-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8N-CCGC-CPF-SACDO-ST           PIC X(009).
