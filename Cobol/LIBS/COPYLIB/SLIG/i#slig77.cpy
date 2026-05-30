      ***===========================================================***
      *** NOME INC                                     LENGTH=00056 ***
      *** I#SLIG77                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DAS TABELAS:                            ***
      ***                               TCOCEN_CLI_CHEQ             ***
      ***                               TCOCEN_CLI_TITLO            ***
      ***                               TCOCEN_CLI_VNDOR            ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-IMAGEM-COCEN-CLI.
           05 SLIG77-CCGC-CPF-CLI            PIC 9(009) COMP-3.
           05 SLIG77-CINDCD-PSSOA-CLI        PIC X(001).
           05 SLIG77-ICOPLT-CLI              PIC X(040).
           05 SLIG77-ICOPLT-CLI-NULL         PIC X(001).
           05 SLIG77-VABERT-CLI              PIC 9(013)V99 COMP-3.
           05 SLIG77-VABERT-CLI-NULL         PIC X(001).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG77-CCGC-CPF-CLI-ST         PIC X(009).
