      ***===========================================================***
      *** NOME INC                                     LENGTH=00027 ***
      *** I#SLIG9D                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA                               ***
      ***            PARTICIPACAO-BANCO-PESSOA-VENDOR               ***
      ***                               CLIENTE SACADO              ***
      ***                                                           ***
BRQ250***           (COPIA DA I#SLIG94)                             ***
BRQ250***            INCLUIDO FILIAL E CONTROLE DO CNPJ             ***
BRQ250***            PARA CLIENTE OU SACADO                         ***
      ***===========================================================***
      *
       01  SLIG9D-REGISTRO.
      *-------- TIPO IDENTIFICADOR RELACIONAMENTO 1 CLIENTE 2 SACADO
           05 SLIG9D-CTPO-IDTFD-RLCTO            PIC S9(001)   COMP-3.
      *-------- BANCO CLIENTE
           05 SLIG9D-CEMPR-INC                   PIC S9(005)   COMP-3.
      *-------- SEQUENCIA DA CLASSIFICACAO
           05 SLIG9D-CSEQ-CLASF-PRTCP            PIC S9(015)   COMP-3.
      *-------- J CNPJ / F CPF      CLIENTE OU SACADO
           05 SLIG9D-CCGC-CPF                    PIC S9(09)    COMP-3.
      *-------- J CNPJ / FILIAL     CLIENTE OU SACADO
           05 SLIG9D-FILIAL                      PIC S9(05)    COMP-3.
      *-------- J CNPJ / F CPF CONTROLE CLIENTE OU SACADO
           05 SLIG9D-CONTROLE                    PIC S9(02)    COMP-3.
      *-------- TIPO PESSOA J OU F  CLIENTE OU SACADO
           05 SLIG9D-CINDCD-PSSOA                PIC X(001).
      *-------- INDICE CONCENTRACAO PARTICIPACAO
           05 SLIG9D-IC-SIMPLES                  PIC S9(03)V99 COMP-3.
           05 SLIG9D-IC-SIMPLES-NULL             PIC  X(01).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG9D-CCGC-CPF-ST                 PIC X(009).
           05 SLIG9D-FILIAL-ST                   PIC X(005).
           05 SLIG9D-CONTROLE-ST                 PIC S9(02)    COMP-3.
