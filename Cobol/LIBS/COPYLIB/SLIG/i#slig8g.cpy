      ***===========================================================***
      *** NOME INC                                     LENGTH=00042 ***
      *** I#SLIG8G                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGE DAS TABELAS                              ***
      ***            LIQUIDEZ-CLIENTE-EMITENTE-CHEQUE               ***
      ***            LIQUIDEZ-CLIENTE-SACADO-TITULO                 ***
      ***           (COPIA DA I#SLIG87                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG8G-REGISTRO.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8G-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF CLIENTE
           05 SLIG8G-FILIAL-CCGC-CPF-CLI         PIC S9(05)    COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG8G-CSEQ-CLASF-LIQDZ            PIC S9(15)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF CLIENTE
           05 SLIG8G-CONTROLE-CCGC-CPF-CLI       PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8G-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8G-CCGC-CPF-SACDO              PIC S9(009)   COMP-3.
      *-------- J FILIAL CNPJ / F CPF SACADO
           05 SLIG8G-FILIAL-CCGC-CPF-SACDO       PIC S9(005)   COMP-3.
      *-------- J CONTROLE CNPJ / F CPF SACADO
           05 SLIG8G-CONTROLE-CCGC-CPF-SACDO     PIC S9(002)   COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8G-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- INDICE LIQUIDEZ
           05 SLIG8G-IL-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG8G-IL-SIMPLES-NULL             PIC  X(01).
           05 SLIG8G-IL-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG8G-IL-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG8G-IL-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG8G-IL-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG8G-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8G-FILIAL-CCGC-CPF-CLI-ST      PIC X(005).
           05 SLIG8G-CTRL-CCGC-CPF-CLI-ST        PIC S9(02)    COMP-3.
           05 SLIG8G-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8G-FLIAL-CCGC-CPF-SACDO-ST     PIC X(005).
           05 SLIG8G-CTRL-CCGC-CPF-SACDO-ST      PIC S9(002)   COMP-3.
