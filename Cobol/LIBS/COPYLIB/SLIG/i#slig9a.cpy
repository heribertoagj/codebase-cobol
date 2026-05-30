      ***===========================================================***
      *** NOME INC                                     LENGTH=00038 ***
      *** I#SLIG9A                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS                             ***
      ***            PARTICIPACAO-AGENCIA-PESSOA-CHEQUE             ***
      ***            PARTICIPACAO-AGENCIA-PESSOA-TITULO             ***
      ***            PARTICIPACAO-AGENCIA-PESSOA-VENDOR *           ***
      ***                         * RETIRAR CAUCIONADA NO SORT      ***
      ***                         * RETIRAR DESCONTO NO SORT TAMBEM ***
      ***           (COPIA DA I#SLIG91                              ***
      ***            INCLUIDO FILIAL E CONTROLE DO CNPJ)            ***
      ***===========================================================***
      *
       01  SLIG9A-REGISTRO.
      *-------- TIPO IDENTIFICADOR RELACIONAMENTO 1 CLIENTE 2 SACADO
           05 SLIG9A-CTPO-IDTFD-RLCTO            PIC S9(001)   COMP-3.
      *-------- BANCO CLIENTE
           05 SLIG9A-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- AGENCIA CLIENTE
           05 SLIG9A-CDEPDC                      PIC S9(005)   COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG9A-CSEQ-CLASF-PRTCP            PIC S9(15)    COMP-3.
      *-------- J CNPJ / F CPF      CLIENTE OU SACADO
           05 SLIG9A-CCGC-CPF                    PIC S9(09)    COMP-3.
      *-------- J FILIAL CNPJ / F CPF      CLIENTE OU SACADO
           05 SLIG9A-FILIAL-CCGC-CPF             PIC S9(05)    COMP-3.
      *-------- J CONTROLE CNPJ / F CPF      CLIENTE OU SACADO
           05 SLIG9A-CONTROLE-CCGC-CPF           PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA J OU F  CLIENTE OU SACADO
           05 SLIG9A-CIDTFD-TPO-PSSOA            PIC X(001).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG9A-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG9A-IC-SIMPLES-NULL             PIC  X(01).
           05 SLIG9A-IC-CAUCIONADA               PIC S9(03)V99 COMP-3.
           05 SLIG9A-IC-CAUCIONADA-NULL          PIC  X(01).
           05 SLIG9A-IC-DESCONTO                 PIC S9(03)V99 COMP-3.
           05 SLIG9A-IC-DESCONTO-NULL            PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG9A-CCGC-CPF-ST                 PIC X(009).
           05 SLIG9A-FILIAL-CCGC-CPF-ST          PIC X(005).
           05 SLIG9A-CONTROLE-CCGC-CPF-ST        PIC S9(02)    COMP-3.
