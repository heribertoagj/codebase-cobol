      ***===========================================================***
      *** NOME INC                                     LENGTH=00046 ***
      *** I#SLIG8Q                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGE DA TABELA                                ***
      ***            LIQUIDEZ-SACADO-NEGOCIACAO                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  SLIG8Q-REGISTRO.
      *-------- BANCO CLIENTE
           05 SLIG8Q-CBCO-PRODT-COBR             PIC S9(03)    COMP-3.
      *-------- CODIGO DO PRODUTO DA COBRANCA
           05 SLIG8Q-CIDTFD-PRODT-COBR           PIC S9(02)    COMP-3.
      *-------- CODIGO DO NEGOCIACAO DA COBRANCA
           05 SLIG8Q-CNEGOC-COBR                 PIC S9(18)    COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG8Q-CSEQ-CLASF-LIQDZ            PIC S9(15)    COMP-3.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8Q-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8Q-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8Q-CCGC-CPF-SACDO              PIC S9(09)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8Q-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- INDICE LIQUIDEZ
           05 SLIG8Q-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8Q-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG8Q-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8Q-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8Q-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8Q-IL-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG8Q-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8Q-CCGC-CPF-SACDO-ST           PIC X(009).
