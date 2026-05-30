      ***===========================================================***
      *** NOME INC                                     LENGTH=00038 ***
      *** I#SLIG8I                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA                               ***
      ***            LIQUIDEZ-CLIENTE-COMPRADOR-VENDOR              ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG89)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE E SACADO                          ***
      ***===========================================================***
      *
       01  SLIG8I-REGISTRO.
      *-------- J CNPJ / F CPF CLIENTE
           05 SLIG8I-CCGC-CPF-CLI                PIC S9(09)    COMP-3.
      *-------- J FILIAL CLIENTE
           05 SLIG8I-FILIAL-CLI                  PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF CLIENTE
           05 SLIG8I-CONTROLE-CLI                PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA CLIENTE J OU F
           05 SLIG8I-CINDCD-PSSOA-CLI            PIC X(001).
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG8I-CSEQ-CLASF-LIQDZ            PIC S9(15)    COMP-3.
      *-------- J CNPJ / F CPF SACADO
           05 SLIG8I-CCGC-CPF-SACDO              PIC S9(09)    COMP-3.
      *-------- J FILIAL SACADO
           05 SLIG8I-FILIAL-SACDO                PIC S9(05)    COMP-3.
      *-------- CONTROLE CNPJ / CPF SACADO
           05 SLIG8I-CONTROLE-SACDO              PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA SACADO J OU F
           05 SLIG8I-CINDCD-PSSOA-SACDO          PIC X(001).
      *-------- INDICE LIQUIDEZ COMPRADOR
           05 SLIG8I-ILC-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8I-ILC-SIMPLES-NULL            PIC  X(01).
      *-------- INDICE LIQUIDEZ FORNECEDOR
           05 SLIG8I-ILF-SIMPLES                 PIC S9(03)V99 COMP-3.
           05 SLIG8I-ILF-SIMPLES-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG8I-CCGC-CPF-CLI-ST             PIC X(009).
           05 SLIG8I-FILIAL-CLI-ST               PIC X(005).
           05 SLIG8I-CONTROLE-CLI-ST             PIC S9(02)    COMP-3.
           05 SLIG8I-CCGC-CPF-SACDO-ST           PIC X(009).
           05 SLIG8I-FILIAL-SACDO-ST             PIC X(005).
           05 SLIG8I-CONTROLE-SACDO-ST           PIC S9(02)    COMP-3.
