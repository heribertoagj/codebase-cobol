      ***===========================================================***
      *** NOME INC                                     LENGTH=0200  ***
      *** I#SLIGCI                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO MOVTO, CHEQUES REAPRESENTADOS ANTER.    **
      **             DEVOLVIDOS                                      **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       01 SLIGCI-REG-CHE-CLI.
          03 SLIGCI-CONTROLE.
CNPJ7C       05 SLIGCI-CGC-CPF-CLI             PIC X(009).
             05 SLIGCI-NOME-CLI                PIC X(040).
             05 SLIGCI-PRODUTO                 PIC X(007).
             05 SLIGCI-TIPO-CARTEIRA           PIC 9(001).
BRQ250    03 SLIGCI-CONTROLE-CHEQUE.
BRQ250       05 SLIGCI-BCO-CLI                 PIC 9(003) COMP-3.
BRQ250       05 SLIGCI-AGE-CLI                 PIC 9(005) COMP-3.
BRQ250       05 SLIGCI-CTA-CLI                 PIC 9(017) COMP-3.
          03 SLIGCI-PERIODOS.
             05  SLIGCI-DT-PER-INI             PIC 9(008).
             05  SLIGCI-DT-PER-FIM             PIC 9(008).
          03 SLIGCI-VALORES.
             05 SLIGCI-VL-TOT-CART             PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-VCT-CART             PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-BXA-ANTEC            PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-DEP-CHE-PER          PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-PAGOS            PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-PRI-DEV          PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-REAPRES          PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-REAPR-PAG        PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-REAPR-DEV        PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-ANT-REAP-DEV     PIC 9(015)V99 COMP-3.
             05 SLIGCI-VL-CHE-DEV-PER-ANT      PIC 9(015)V99 COMP-3.
             05 FILLER                         PIC X(028).
